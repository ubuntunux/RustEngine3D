use std::time;

use log;
use nalgebra::Vector2;
use sdl2::event;
use sdl2::Sdl;
use winit::application::ApplicationHandler;
use winit::dpi;
use winit::event::{
    DeviceEvent, DeviceId, ElementState, MouseButton, MouseScrollDelta, StartCause, Touch, TouchPhase, WindowEvent,
};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::monitor::{MonitorHandle, VideoModeHandle};
use winit::platform::run_on_demand::EventLoopExtRunOnDemand;
use winit::raw_window_handle::{HasDisplayHandle, RawDisplayHandle};
use winit::window::{CursorGrabMode, Fullscreen, Window, WindowAttributes, WindowId};

use crate::audio::audio_manager::AudioManager;
use crate::constants;
use crate::constants::{DEVELOPMENT, SHOW_DEBUG_TEXT};
use crate::core::engine_service_locator;
use crate::core::engine_service_locator::{get_renderer_context, get_scene_manager_mut};
use crate::core::input::{self, ButtonState};
use crate::effect::effect_manager::EffectManager;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::RenderOption;
use crate::resource::resource::{CallbackLoadRenderPassCreateInfo, EngineResources};
use crate::scene::debug_line::DebugLineManager;
use crate::scene::font::FontManager;
use crate::scene::scene_manager::SceneManager;
use crate::scene::ui::UIManager;
use crate::utilities::scope_profiler::TIME_PROFILER;
use crate::utilities::system::{ptr_as_mut, ptr_as_ref};

use serde::{Deserialize, Serialize};
use strum_macros::{Display, EnumCount, EnumIter, FromRepr};

const DEFAULT_FALLBACK_WIDTH: u32 = 1920;
const DEFAULT_FALLBACK_HEIGHT: u32 = 1080;

fn is_wayland_window(window: &Window) -> bool {
    window.display_handle().map(|handle| matches!(handle.as_raw(), RawDisplayHandle::Wayland(_))).unwrap_or(false)
}

fn is_wayland_event_loop(event_loop: &ActiveEventLoop) -> bool {
    event_loop.display_handle().map(|handle| matches!(handle.as_raw(), RawDisplayHandle::Wayland(_))).unwrap_or(false)
}

fn dedup_monitors<I: IntoIterator<Item = MonitorHandle>>(iter: I) -> Vec<MonitorHandle> {
    let mut monitors: Vec<MonitorHandle> = Vec::new();
    for monitor in iter {
        let is_duplicate = monitors.iter().any(|existing| {
            existing == &monitor
                || (existing.name().is_some()
                    && existing.name() == monitor.name()
                    && existing.position() == monitor.position()
                    && existing.size() == monitor.size())
        });
        if !is_duplicate {
            monitors.push(monitor);
        }
    }
    monitors
}

fn find_best_video_mode(
    monitor: &MonitorHandle,
    desired_size: Option<dpi::PhysicalSize<u32>>,
) -> Option<VideoModeHandle> {
    let monitor_refresh = monitor.refresh_rate_millihertz();
    if let Some(target_size) = desired_size {
        if let Some(refresh_hz) = monitor_refresh {
            if let Some(exact) =
                monitor.video_modes().find(|vm| vm.size() == target_size && vm.refresh_rate_millihertz() == refresh_hz)
            {
                return Some(exact);
            }
        }
        if let Some(best_for_size) = monitor
            .video_modes()
            .filter(|vm| vm.size() == target_size)
            .max_by_key(|vm| (vm.refresh_rate_millihertz(), vm.bit_depth()))
        {
            return Some(best_for_size);
        }
    }

    let native_size = monitor.size();
    if let Some(refresh_hz) = monitor_refresh {
        if let Some(native_exact) =
            monitor.video_modes().find(|vm| vm.size() == native_size && vm.refresh_rate_millihertz() == refresh_hz)
        {
            return Some(native_exact);
        }
    }

    monitor
        .video_modes()
        .filter(|vm| vm.size() == native_size)
        .max_by_key(|vm| (vm.refresh_rate_millihertz(), vm.bit_depth()))
        .or_else(|| monitor.video_modes().next())
}

fn compute_centered_window_pos(monitor: &MonitorHandle, resolution: Vector2<u32>) -> dpi::PhysicalPosition<i32> {
    let mon_pos = monitor.position();
    let mon_size = monitor.size();
    if mon_size.width == 0 || mon_size.height == 0 {
        return mon_pos;
    }
    let win_w = resolution.x.min(mon_size.width) as i32;
    let win_h = resolution.y.min(mon_size.height) as i32;
    let offset_x = (mon_size.width as i32 - win_w).max(0) / 2;
    let offset_y = (mon_size.height as i32 - win_h).max(0) / 2;
    dpi::PhysicalPosition::new(mon_pos.x + offset_x, mon_pos.y + offset_y)
}

fn apply_window_cursor_grab(window: &Window, is_grab_mode: bool) {
    if is_grab_mode {
        // Reset cached grab state first so X11/Wayland re-grabs reliably after fullscreen/monitor transitions.
        let _ = window.set_cursor_grab(CursorGrabMode::None);
        if window.set_cursor_grab(CursorGrabMode::Locked).is_err() {
            let _ = window.set_cursor_grab(CursorGrabMode::Confined);
        }
        window.set_cursor_visible(false);
    } else {
        let _ = window.set_cursor_grab(CursorGrabMode::None);
        window.set_cursor_visible(true);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Display, EnumCount, EnumIter, FromRepr)]
pub enum WindowMode {
    WindowMode,
    FullScreenExclusiveMode,
    FullScreenBorderlessMode,
}

impl WindowMode {
    pub fn display_name(&self) -> &'static str {
        match self {
            WindowMode::WindowMode => "Windowed",
            WindowMode::FullScreenExclusiveMode => "Full Screen (Exclusive)",
            WindowMode::FullScreenBorderlessMode => "Full Screen (Borderless)",
        }
    }
}

const STANDARD_REFRESH_RATES: [f64; 8] = [30.0, 60.0, 75.0, 90.0, 120.0, 144.0, 165.0, 240.0];
const SNAP_THRESHOLD_FPS: f64 = 2.0;
const DEADBAND_THRESHOLD_FPS: f64 = 0.5;

fn snap_to_standard_refresh_rate(fps: f64) -> f64 {
    for &rate in &STANDARD_REFRESH_RATES {
        if (fps - rate).abs() <= SNAP_THRESHOLD_FPS {
            return rate;
        }
    }
    fps
}

#[derive(Debug, Clone)]
pub struct TimeData {
    pub _time_instance: time::Instant,
    pub _last_time_instance: time::Instant,
    pub _current_time: f64,
    pub _acc_frame_time: f64,
    pub _real_acc_frame_time: f64,
    pub _acc_render_time: f64,
    pub _acc_present_time: f64,
    pub _acc_frame_count: i32,
    pub _average_frame_time: f64,
    pub _average_fps: f64,
    pub _target_fps: f64,
    pub _average_render_time: f64,
    pub _average_present_time: f64,
    pub _elapsed_frame: u64,
    pub _elapsed_time_prev: f64,
    pub _elapsed_time: f64,
    pub _delta_time: f64,
    pub _delta_time_with_scale: f64,
}

pub fn create_time_data() -> TimeData {
    let time_instance = time::Instant::now();
    let current_time = time_instance.elapsed().as_secs_f64();
    TimeData {
        _time_instance: time_instance,
        _last_time_instance: time_instance,
        _current_time: current_time,
        _acc_frame_time: 0.0,
        _real_acc_frame_time: 0.0,
        _acc_render_time: 0.0,
        _acc_present_time: 0.0,
        _acc_frame_count: 0,
        _average_frame_time: 0.0,
        _average_fps: 0.0,
        _target_fps: 60.0,
        _average_render_time: 0.0,
        _average_present_time: 0.0,
        _elapsed_frame: 0,
        _elapsed_time_prev: 0.0,
        _elapsed_time: 0.0,
        _delta_time: 0.0,
        _delta_time_with_scale: 0.0,
    }
}

impl TimeData {
    pub fn reset_time_data(&mut self) {
        let now = time::Instant::now();
        self._last_time_instance = now;
        self._real_acc_frame_time = 0.0;
        self._acc_frame_time = 0.0;
        self._acc_frame_count = 0;
        self._acc_render_time = 0.0;
        self._acc_present_time = 0.0;
    }

    pub fn update_time_data(&mut self, delta_time_scale: f64, monitor_refresh_rate: Option<f64>) {
        let now = time::Instant::now();
        let mut delta_duration = now.saturating_duration_since(self._last_time_instance);
        let mut delta_time = delta_duration.as_secs_f64();

        let explicit_target = unsafe { constants::TARGET_FRAME_RATE };
        let max_target_fps: f64 = if explicit_target > 0 {
            explicit_target as f64
        } else if unsafe { !constants::IGNORE_MONITOR_REFRESH_RATE } && monitor_refresh_rate.is_some() {
            monitor_refresh_rate.unwrap().clamp(30.0, 240.0)
        } else {
            240.0
        };

        let dynamic_target_fps: f64 = if unsafe { constants::ENABLE_DYNAMIC_TARGET_FPS } {
            self._target_fps.min(max_target_fps)
        } else if explicit_target > 0 {
            explicit_target as f64
        } else {
            0.0
        };

        let real_acc_frame_time = self._real_acc_frame_time + delta_time;

        if dynamic_target_fps > 0.0 {
            let target_frame_duration = std::time::Duration::from_secs_f64(1.0 / dynamic_target_fps);
            if delta_duration < target_frame_duration {
                let sleep_duration = target_frame_duration - delta_duration;
                if sleep_duration.as_micros() > 2000 {
                    std::thread::sleep(sleep_duration.saturating_sub(std::time::Duration::from_micros(1500)));
                }
                while self._last_time_instance.elapsed() < target_frame_duration {
                    std::hint::spin_loop();
                }
                let after_sleep = time::Instant::now();
                delta_duration = after_sleep.saturating_duration_since(self._last_time_instance);
                delta_time = delta_duration.as_secs_f64();
            }
        }
        self._elapsed_time_prev = self._elapsed_time;
        let elapsed_time = self._elapsed_time + delta_time;
        let acc_frame_time = self._acc_frame_time + delta_time;
        let acc_frame_count = self._acc_frame_count + 1;
        self._elapsed_frame += 1;

        if 1.0 < acc_frame_time {
            let average_frame_time = acc_frame_time / (acc_frame_count as f64) * 1000.0;
            let average_fps = 1000.0 / average_frame_time;

            let real_average_frame_time = if acc_frame_count > 0 {
                (real_acc_frame_time / (acc_frame_count as f64)) * 1000.0
            } else {
                0.0
            };
            let real_average_fps = if real_average_frame_time > 0.0 {
                1000.0 / real_average_frame_time
            } else {
                max_target_fps
            };

            let average_render_time = self._acc_render_time / (acc_frame_count as f64) * 1000.0;
            let average_present_time = self._acc_present_time / (acc_frame_count as f64) * 1000.0;

            let raw_candidate_fps = if dynamic_target_fps > 0.0 {
                let target_frame_time_ms = 1000.0 / self._target_fps;
                if real_average_frame_time < target_frame_time_ms * 0.85 {
                    real_average_fps.min(max_target_fps)
                } else if real_average_frame_time > target_frame_time_ms * 0.98
                    || average_fps < self._target_fps - SNAP_THRESHOLD_FPS
                {
                    real_average_fps.min(average_fps).clamp(10.0, max_target_fps)
                } else {
                    self._target_fps
                }
            } else {
                average_fps.clamp(10.0, max_target_fps)
            };

            let snapped_fps = snap_to_standard_refresh_rate(raw_candidate_fps).min(max_target_fps);

            if self._elapsed_frame == acc_frame_count as u64 {
                self._target_fps = snapped_fps;
            } else if (snapped_fps - self._target_fps).abs() > DEADBAND_THRESHOLD_FPS {
                let is_standard = (snapped_fps - raw_candidate_fps).abs() > 0.0;
                let alpha = if snapped_fps > self._target_fps {
                    0.6
                } else if is_standard {
                    0.5
                } else {
                    0.3
                };
                let smoothed_fps = self._target_fps * (1.0 - alpha) + snapped_fps * alpha;
                self._target_fps = snap_to_standard_refresh_rate(smoothed_fps).clamp(10.0, max_target_fps);
            }

            self._real_acc_frame_time = 0.0;
            self._acc_frame_time = 0.0;
            self._acc_frame_count = 0;
            self._average_frame_time = average_frame_time;
            self._average_fps = average_fps;
            self._average_render_time = average_render_time;
            self._acc_render_time = 0.0;
            self._average_present_time = average_present_time;
            self._acc_present_time = 0.0;

            // debug text
            // log::info!("{}", format!("{:.2}fps / {:.3}ms", self._average_fps, self._average_frame_time));
        } else {
            self._real_acc_frame_time = real_acc_frame_time;
            self._acc_frame_time = acc_frame_time;
            self._acc_frame_count = acc_frame_count;
        }

        let now_end = time::Instant::now();
        self._last_time_instance = now_end;
        self._current_time = now_end.saturating_duration_since(self._time_instance).as_secs_f64();
        self._elapsed_time = elapsed_time;
        self._delta_time = delta_time;
        // prevent lower fps.
        self._delta_time_with_scale = 0.1f64.min(delta_time) * delta_time_scale;
    }

    pub fn get_current_time(&self) -> f64 {
        self._time_instance.elapsed().as_secs_f64()
    }
}

pub trait ApplicationBase<'a> {
    fn initialize_application(&'a mut self, window_size: &Vector2<i32>);
    fn will_terminate_application(&self) -> bool;
    fn terminate_application(&mut self);
    fn get_render_pass_create_info_callback(&self) -> *const CallbackLoadRenderPassCreateInfo;
    fn is_visible_cursor(&self) -> bool;
    fn focused(&mut self, focused: bool);
    fn update_event(&mut self);
    fn update_application(&mut self, delta_time: f64);
}

pub struct EngineCore<'a> {
    pub _window: *const Window,
    pub _window_size: Vector2<i32>,
    pub _is_grab_mode: bool,
    pub _is_grab_mode_backup: bool,
    pub _time_data: TimeData,
    pub _camera_move_speed: f32,
    pub _is_keyboard_input_mode: bool,
    pub _keyboard_input_data: Box<input::KeyboardInputData>,
    pub _mouse_move_data: Box<input::MouseMoveData>,
    pub _mouse_input_data: Box<input::MouseInputData>,
    pub _joystick_input_data: Box<input::JoystickInputData>,
    pub _audio_manager: Box<AudioManager>,
    pub _effect_manager: Box<EffectManager<'a>>,
    pub _debug_line_manager: Box<DebugLineManager>,
    pub _font_manager: Box<FontManager>,
    pub _renderer_context: Box<RendererContext<'a>>,
    pub _ui_manager: Box<UIManager<'a>>,
    pub _scene_manager: Box<SceneManager<'a>>,
    pub _engine_resources: Box<EngineResources<'a>>,
    pub _application: *const dyn ApplicationBase<'a>,
    pub _window_mode: WindowMode,
    pub _monitor_index: Option<usize>,
    pub _resolution: Vector2<u32>,
    pub _pending_window_size: Option<dpi::PhysicalSize<u32>>,
}

impl<'a> EngineCore<'a> {
    pub fn get_application(&self) -> &dyn ApplicationBase<'a> {
        ptr_as_ref(self._application)
    }
    pub fn get_application_mut(&self) -> &mut dyn ApplicationBase<'a> {
        ptr_as_mut(self._application)
    }
    pub fn get_window(&self) -> &Window {
        ptr_as_ref(self._window)
    }
    pub fn get_window_mode(&self) -> WindowMode {
        self._window_mode
    }
    pub fn get_monitor_index(&self) -> Option<usize> {
        self._monitor_index
    }
    pub fn get_resolution(&self) -> Vector2<u32> {
        self._resolution
    }
    pub fn get_current_monitor_refresh_rate(&self) -> Option<f64> {
        let window = self.get_window();
        let monitor = window.current_monitor().or_else(|| window.primary_monitor())?;
        monitor.refresh_rate_millihertz().map(|mhz| (mhz as f64) / 1000.0)
    }
    pub fn set_window_mode(&mut self, window_mode: WindowMode) {
        self.set_display_options(window_mode, self._monitor_index, self._resolution);
    }
    pub fn set_monitor_index(&mut self, monitor_index: Option<usize>) {
        self.set_display_options(self._window_mode, monitor_index, self._resolution);
    }
    pub fn set_resolution(&mut self, resolution: Vector2<u32>) {
        self.set_display_options(self._window_mode, self._monitor_index, resolution);
    }
    pub fn set_display_options(
        &mut self,
        window_mode: WindowMode,
        monitor_index: Option<usize>,
        resolution: Vector2<u32>,
    ) {
        let mode_changed = self._window_mode != window_mode;
        let monitor_changed = self._monitor_index != monitor_index;
        let resolution_changed = self._resolution != resolution;
        if !mode_changed && !monitor_changed && !resolution_changed {
            return;
        }

        self._window_mode = window_mode;
        self._monitor_index = monitor_index;
        self._resolution = resolution;
        self._time_data.reset_time_data();

        let target_size = dpi::PhysicalSize::new(resolution.x.max(1), resolution.y.max(1));

        match window_mode {
            WindowMode::WindowMode => {
                let was_fullscreen = mode_changed || self.get_window().fullscreen().is_some();
                if was_fullscreen {
                    let window = self.get_window();
                    window.set_fullscreen(None);
                    window.set_decorations(true);
                }

                if monitor_changed || mode_changed {
                    if let Some(monitor) = self.get_target_monitor(monitor_index) {
                        let centered_pos = compute_centered_window_pos(&monitor, resolution);
                        self.get_window().set_outer_position(dpi::Position::Physical(centered_pos));
                    }
                }

                if mode_changed || resolution_changed {
                    let returned_size = self.get_window().request_inner_size(dpi::Size::Physical(target_size));
                    if self.get_window().fullscreen().is_some() {
                        // Wayland defers exiting fullscreen until the compositor configure arrives.
                        self._pending_window_size = Some(target_size);
                    } else if let Some(actual_size) = returned_size {
                        self._pending_window_size = None;
                        if actual_size.width > 0 && actual_size.height > 0 {
                            self.resized_window(actual_size);
                        }
                    } else if was_fullscreen {
                        self._pending_window_size = Some(target_size);
                    } else {
                        self._pending_window_size = None;
                    }
                }
            }
            WindowMode::FullScreenExclusiveMode => {
                self._pending_window_size = None;
                if mode_changed || monitor_changed || self.get_window().fullscreen().is_none() {
                    let target_monitor = self
                        .get_target_monitor(monitor_index)
                        .or_else(|| self.get_window().current_monitor())
                        .or_else(|| self.get_window().primary_monitor());
                    if let Some(mon) = target_monitor {
                        let window = self.get_window();
                        if !is_wayland_window(window) {
                            if let Some(vm) = find_best_video_mode(&mon, None) {
                                window.set_fullscreen(Some(Fullscreen::Exclusive(vm)));
                            } else {
                                window.set_fullscreen(Some(Fullscreen::Borderless(Some(mon))));
                            }
                        } else {
                            // Wayland ignores Fullscreen::Exclusive; fallback to Borderless on the selected monitor.
                            window.set_fullscreen(Some(Fullscreen::Borderless(Some(mon))));
                        }
                    }
                }
            }
            WindowMode::FullScreenBorderlessMode => {
                self._pending_window_size = None;
                if mode_changed || monitor_changed || self.get_window().fullscreen().is_none() {
                    let target_monitor = self
                        .get_target_monitor(monitor_index)
                        .or_else(|| self.get_window().current_monitor())
                        .or_else(|| self.get_window().primary_monitor());
                    self.get_window().set_fullscreen(Some(Fullscreen::Borderless(target_monitor)));
                }
            }
        }

        if self._is_grab_mode {
            apply_window_cursor_grab(self.get_window(), true);
        }
    }
    pub fn center_mouse_pos(&mut self) {
        let center_x = self._window_size.x / 2;
        let center_y = self._window_size.y / 2;
        let window = self.get_window();
        if !self._is_grab_mode || is_wayland_window(window) {
            let position = dpi::PhysicalPosition::new(center_x, center_y);
            let _ = window.set_cursor_position(dpi::Position::Physical(position));
        }
        let window_size = self._window_size.clone();
        self._mouse_move_data.set_mouse_pos(&Vector2::new(center_x, center_y), &window_size);
        self._mouse_move_data._mouse_pos_delta = Vector2::zeros();
    }
    pub fn get_available_monitors(&self) -> Vec<MonitorHandle> {
        dedup_monitors(self.get_window().available_monitors())
    }
    pub fn get_primary_monitor(&self) -> Option<MonitorHandle> {
        let window = self.get_window();
        let available_monitors: Vec<MonitorHandle> = dedup_monitors(window.available_monitors());
        window
            .primary_monitor()
            .or_else(|| {
                available_monitors
                    .iter()
                    .find(|monitor| monitor.position().x == 0 && monitor.position().y == 0)
                    .cloned()
            })
            .or_else(|| available_monitors.into_iter().next())
    }
    pub fn get_primary_monitor_index(&self) -> usize {
        let monitors: Vec<MonitorHandle> = self.get_available_monitors();
        if let Some(primary) = self.get_primary_monitor() {
            monitors.iter().position(|m| m.name() == primary.name() && m.position() == primary.position()).unwrap_or(0)
        } else {
            0
        }
    }
    pub fn get_target_monitor(&self, monitor_index: Option<usize>) -> Option<MonitorHandle> {
        let monitors: Vec<MonitorHandle> = self.get_available_monitors();
        if let Some(index) = monitor_index {
            monitors.get(index).cloned().or_else(|| self.get_primary_monitor())
        } else {
            self.get_primary_monitor()
        }
    }
    pub fn get_monitor_names(&self) -> Vec<String> {
        let monitors: Vec<MonitorHandle> = self.get_available_monitors();
        let primary = self.get_primary_monitor();
        if monitors.is_empty() {
            vec!["Primary Monitor".to_string()]
        } else {
            monitors
                .iter()
                .enumerate()
                .map(|(i, m)| {
                    let size = m.size();
                    let is_primary =
                        primary.as_ref().map(|p| p.name() == m.name() && p.position() == m.position()).unwrap_or(false);
                    let primary_tag = if is_primary { " (Primary)" } else { "" };
                    if let Some(name) = m.name() {
                        format!("{}: {}{} ({}x{})", i, name, primary_tag, size.width, size.height)
                    } else {
                        format!("Monitor {}{} ({}x{})", i, primary_tag, size.width, size.height)
                    }
                })
                .collect()
        }
    }
    pub fn get_available_resolutions(&self, monitor_index: Option<usize>) -> Vec<Vector2<u32>> {
        let mut resolutions = Vec::new();
        let mut monitor_size = None;

        if let Some(monitor) = self.get_target_monitor(monitor_index) {
            let s = monitor.size();
            if s.width > 0 && s.height > 0 {
                monitor_size = Some(s);
                let native = Vector2::new(s.width, s.height);
                if !resolutions.contains(&native) {
                    resolutions.push(native);
                }
            }
            for vm in monitor.video_modes() {
                let vm_size = vm.size();
                let res = Vector2::new(vm_size.width, vm_size.height);
                if !resolutions.contains(&res) {
                    resolutions.push(res);
                }
            }
        }

        let common_resolutions = [
            Vector2::new(3840, 2160),
            Vector2::new(2560, 1440),
            Vector2::new(1920, 1080),
            Vector2::new(1600, 900),
            Vector2::new(1366, 768),
            Vector2::new(1280, 720),
            Vector2::new(1024, 768),
        ];

        for &res in &common_resolutions {
            let fits_monitor = monitor_size.map(|s| res.x <= s.width && res.y <= s.height).unwrap_or(true);
            if fits_monitor && !resolutions.contains(&res) {
                resolutions.push(res);
            }
        }

        if resolutions.is_empty() {
            resolutions.extend_from_slice(&common_resolutions);
        }

        resolutions.sort_by(|a, b| (b.x * b.y).cmp(&(a.x * a.y)));
        resolutions.dedup();
        resolutions
    }
    pub fn create_application(
        app_name: &str,
        app_version: u32,
        window: &Window,
        window_mode: WindowMode,
        initial_monitor_index: Option<usize>,
        initial_window_size: Option<Vector2<u32>>,
        sdl: &Sdl,
        application: *const dyn ApplicationBase<'a>,
    ) -> Box<EngineCore<'a>> {
        let inner_size = window.inner_size();
        let fallback_resolution =
            initial_window_size.unwrap_or_else(|| Vector2::new(DEFAULT_FALLBACK_WIDTH, DEFAULT_FALLBACK_HEIGHT));
        let window_size: Vector2<i32> = if inner_size.width > 0 && inner_size.height > 0 {
            Vector2::new(inner_size.width as i32, inner_size.height as i32)
        } else {
            Vector2::new(fallback_resolution.x.max(1) as i32, fallback_resolution.y.max(1) as i32)
        };
        let initial_resolution =
            initial_window_size.unwrap_or_else(|| Vector2::new(window_size.x as u32, window_size.y as u32));

        // create managers
        let callback_load_render_pass_create_info: *const CallbackLoadRenderPassCreateInfo =
            ptr_as_ref(application).get_render_pass_create_info_callback();
        let engine_resources = EngineResources::create_engine_resources(callback_load_render_pass_create_info);
        let debug_line_manager = DebugLineManager::create_debug_line_manager();
        let font_manager = FontManager::create_font_manager();
        let ui_manager = UIManager::create_ui_manager();
        let renderer_context = RendererContext::create_renderer_context(app_name, app_version, &window_size, window);
        let effect_manager = EffectManager::create_effect_manager();
        let audio_manager = AudioManager::create_audio_manager(&sdl);
        let scene_manager = SceneManager::create_scene_manager();
        let keyboard_input_data = input::create_keyboard_input_data();
        let mouse_move_data = input::create_mouse_move_data(&window_size.x / 2, &window_size.y / 2);
        let mouse_input_data = input::create_mouse_input_data();
        let joystick_input_data = input::JoystickInputData::create_joystick_input_data(sdl);
        let mut engine_core = Box::new(EngineCore {
            _window: window,
            _window_size: window_size.into(),
            _is_grab_mode: false,
            _is_grab_mode_backup: false,
            _time_data: create_time_data(),
            _camera_move_speed: 1.0,
            _is_keyboard_input_mode: true,
            _keyboard_input_data: keyboard_input_data,
            _mouse_move_data: mouse_move_data,
            _mouse_input_data: mouse_input_data,
            _joystick_input_data: joystick_input_data,
            _audio_manager: audio_manager,
            _debug_line_manager: debug_line_manager,
            _font_manager: font_manager,
            _ui_manager: ui_manager,
            _renderer_context: renderer_context,
            _effect_manager: effect_manager,
            _scene_manager: scene_manager,
            _engine_resources: engine_resources,
            _application: application,
            _window_mode: window_mode,
            _monitor_index: initial_monitor_index,
            _resolution: initial_resolution,
            _pending_window_size: None,
        });

        // register engine service locator
        engine_service_locator::set_engine_core(engine_core.as_ref() as *const EngineCore);

        engine_core._renderer_context.initialize_renderer_context(engine_core._effect_manager.as_ref());
        engine_core._engine_resources.load_engine_resources();
        engine_core._debug_line_manager.initialize_debug_line_manager();
        engine_core._font_manager.initialize_font_manager();
        engine_core._ui_manager.initialize_ui_manager();
        engine_core._ui_manager.update_window_size(&window_size);
        engine_core._audio_manager.initialize_audio_manager();
        engine_core._effect_manager.initialize_effect_manager();

        // initialize graphics data
        engine_core._renderer_context.prepare_framebuffer_and_descriptors();

        // center mouse pos on application creation
        engine_core.center_mouse_pos();

        engine_core
    }

    pub fn terminate_application(&mut self) {
        let renderer_context = get_renderer_context();
        renderer_context.device_wait_idle();

        // destroy managers
        self.get_application_mut().terminate_application();
        self._audio_manager.destroy_audio_manager();
        self._effect_manager.destroy_effect_manager();
        self._ui_manager.destroy_ui_manager(renderer_context.get_device());
        self._debug_line_manager.destroy_debug_line_manager(renderer_context.get_device());
        self._font_manager.destroy_font_manager(renderer_context.get_device());
        self._engine_resources.destroy_engine_resources(renderer_context);
    }

    pub fn handle_window_resized(&mut self, size: dpi::PhysicalSize<u32>) {
        if self._window_mode == WindowMode::WindowMode && self.get_window().fullscreen().is_none() {
            if let Some(pending_size) = self._pending_window_size.take() {
                if size != pending_size {
                    if let Some(actual_size) = self.get_window().request_inner_size(dpi::Size::Physical(pending_size)) {
                        if actual_size.width > 0 && actual_size.height > 0 {
                            self.resized_window(actual_size);
                            return;
                        }
                    }
                }
            }
        }
        self.resized_window(size);
    }

    pub fn resized_window(&mut self, size: dpi::PhysicalSize<u32>) {
        if size.width == 0 || size.height == 0 {
            self._window_size.x = size.width as i32;
            self._window_size.y = size.height as i32;
            return;
        }

        let old_window_size = self._window_size;
        let new_window_size = Vector2::new(size.width as i32, size.height as i32);
        self._window_size = new_window_size;

        if old_window_size != new_window_size {
            self._mouse_move_data.rescale_mouse_pos(&old_window_size, &new_window_size);
        }

        get_scene_manager_mut().update_window_size(new_window_size.x, new_window_size.y);
        self._ui_manager.update_window_size(&new_window_size);

        if self._is_grab_mode {
            apply_window_cursor_grab(self.get_window(), true);
        }

        let swapchain_extent = self._renderer_context.get_swap_chain_data()._swapchain_extent;
        let need_recreate_swapchain = swapchain_extent.width != size.width || swapchain_extent.height != size.height;
        log::info!(
            "need_recreate_swapchain: {}, swapchain_extent: {:?}, window_size: {:?}",
            need_recreate_swapchain,
            swapchain_extent,
            new_window_size
        );
        if need_recreate_swapchain {
            self._renderer_context.set_need_recreate_swapchain(true);
        }
    }

    pub fn clear_and_update_input_data_list(&mut self) {
        self._mouse_move_data.clear_mouse_move_delta();
        self._mouse_input_data.clear_mouse_input();
        self._keyboard_input_data.clear_key_pressed();
        self._keyboard_input_data.clear_key_released();
        self._joystick_input_data.update_joystick_button_state();
    }

    pub fn clear_key_hold(&mut self) {
        self._keyboard_input_data.clear_key_hold();
    }

    pub fn is_keyboard_input_mode(&self) -> bool {
        self._is_keyboard_input_mode
    }

    pub fn set_keyboard_input_mode(&mut self, keyboard_input_mode: bool) {
        self._is_keyboard_input_mode = keyboard_input_mode;
    }

    pub fn update_mouse_motion(&mut self, delta: &(f64, f64)) {
        if self._is_grab_mode {
            if self._mouse_move_data._is_touch_active {
                return;
            }
            let update_mouse_pos = self.get_application().is_visible_cursor();
            let window_size = self._window_size.clone();
            self._mouse_move_data.update_mouse_move_f64(delta, &window_size, update_mouse_pos);
        }
    }

    pub fn update_mouse_input(&mut self, button: MouseButton, state: ElementState) {
        let mouse_input_data = &mut self._mouse_input_data;
        let pressed = ElementState::Pressed == state;
        match button {
            MouseButton::Left => mouse_input_data.btn_l_pressed(pressed),
            MouseButton::Middle => mouse_input_data.btn_m_pressed(pressed),
            MouseButton::Right => mouse_input_data.btn_r_pressed(pressed),
            _ => (),
        }
    }

    pub fn update_mouse_wheel(&mut self, scroll_x: f32, scroll_y: f32) {
        self._mouse_move_data.update_scroll_move(&(scroll_x as i32, scroll_y as i32));
    }

    pub fn update_cursor_moved(&mut self, position: dpi::PhysicalPosition<f64>) {
        if false == self._is_grab_mode {
            if self._mouse_move_data._is_touch_active {
                return;
            }
            let update_mouse_pos = true;
            let window_size = self._window_size.clone();
            self._mouse_move_data.update_mouse_pos(&position.into(), &window_size, update_mouse_pos);
        }
    }

    pub fn set_grab_mode(&mut self, is_grab_mode: bool) {
        let was_grabbed = self._is_grab_mode;
        self._is_grab_mode = is_grab_mode;
        let target_pos =
            dpi::PhysicalPosition::new(self._mouse_move_data._mouse_pos.x, self._mouse_move_data._mouse_pos.y);
        let window = self.get_window();

        if was_grabbed && !is_grab_mode && is_wayland_window(window) {
            let _ = window.set_cursor_position(dpi::Position::Physical(target_pos));
        }

        apply_window_cursor_grab(window, is_grab_mode);

        if was_grabbed && !is_grab_mode && !is_wayland_window(window) {
            let _ = window.set_cursor_position(dpi::Position::Physical(target_pos));
        }
    }

    pub fn update_keyboard_input(&mut self, input_physical_key: PhysicalKey, input_key_state: ElementState) {
        match input_physical_key {
            PhysicalKey::Code(key) => {
                if ElementState::Pressed == input_key_state {
                    self._keyboard_input_data.set_key_pressed(key);
                } else {
                    self._keyboard_input_data.set_key_released(key);
                }
            }
            _ => {
                log::info!("Unknown key: {:?}", input_physical_key);
            }
        }
    }

    pub fn update_touch(&mut self, touch: &Touch) {
        let touch_pos: (i32, i32) = touch.location.into();
        let window_size = self._window_size.clone();

        match touch.phase {
            TouchPhase::Started => {
                if self._mouse_move_data._primary_touch_id.is_none() {
                    self._mouse_move_data.begin_touch(touch.id, &touch_pos, &window_size);
                    self._mouse_input_data.btn_l_pressed(true);
                }
            }
            TouchPhase::Moved => {
                if self._mouse_move_data._primary_touch_id == Some(touch.id) {
                    self._mouse_move_data.update_touch_move(&touch_pos, &window_size);
                }
            }
            TouchPhase::Ended | TouchPhase::Cancelled => {
                if self._mouse_move_data._primary_touch_id == Some(touch.id) {
                    self._mouse_move_data.end_touch();
                    self._mouse_input_data.btn_l_pressed(false);
                }
            }
        }
    }

    pub fn update_event_and_render_scene(&mut self) {
        let renderer_context = ptr_as_mut(self._renderer_context.as_ref());
        let ui_manager = ptr_as_mut(self._ui_manager.as_ref());
        let debug_line_manager = ptr_as_mut(self._debug_line_manager.as_ref());
        let font_manager = ptr_as_mut(self._font_manager.as_ref());
        let audio_manager = engine_service_locator::get_audio_manager_mut();
        let effect_manager = ptr_as_mut(self._effect_manager.as_ref());
        let scene_manager = ptr_as_mut(self._scene_manager.as_ref());

        // update timer
        let delta_time_scale: f64 = unsafe {
            if DEVELOPMENT && self._keyboard_input_data.get_key_hold(KeyCode::Tab) {
                10.0
            } else {
                1.0
            }
        };
        let monitor_refresh_rate = self.get_current_monitor_refresh_rate();
        self._time_data.update_time_data(delta_time_scale, monitor_refresh_rate);

        // update application event
        self.get_application_mut().update_event();

        let delta_time = self._time_data._delta_time_with_scale;
        if renderer_context.get_need_recreate_swapchain() {
            #[cfg(not(target_os = "android"))]
            {
                log::info!("<<begin recreate_swapchain>>");

                // destroy
                ui_manager.destroy_ui_graphics_data();
                font_manager.destroy_font_descriptor_sets();
                renderer_context.resize_window(self._window_size.x as u32, self._window_size.y as u32);

                // recreate
                font_manager.create_font_descriptor_sets(renderer_context);
                ui_manager.create_ui_graphics_data();
                renderer_context.set_need_recreate_swapchain(false);

                self._time_data.reset_time_data();

                log::info!("<<end recreate_swapchain>>");
            }
        } else {
            // update & render, If the resized event has not yet occurred, the window size may be 0.
            if 0 < self._window_size.x && 0 < self._window_size.y {
                engine_service_locator::get_engine_resources_mut().update_async_loading();
                self.update_application(delta_time);
                scene_manager.update_scene_manager(delta_time);
                audio_manager.update_audio_manager();
                effect_manager.update_effects(delta_time);
                debug_line_manager.update_debug_line();
                font_manager.update_font_manager();
                engine_service_locator::get_engine_resources_mut().update_garbage_collection(delta_time);
                ui_manager.update_ui_manager(
                    delta_time,
                    self,
                    &self._window_size,
                    &self._time_data,
                    &self._keyboard_input_data,
                    &self._mouse_move_data,
                    &self._mouse_input_data,
                );

                // debug text
                unsafe {
                    if SHOW_DEBUG_TEXT {
                        font_manager.log(format!(
                            "{:.2}fps / {:.3}ms",
                            self._time_data._average_fps, self._time_data._average_frame_time
                        ));
                        font_manager.log(format!(
                            "Game Time: {:.3}ms",
                            self._time_data._average_frame_time - self._time_data._average_render_time
                        ));
                        font_manager.log(format!(
                            "Render Time: {:.3}ms",
                            self._time_data._average_render_time - self._time_data._average_present_time
                        ));
                        font_manager.log(format!("Present Time: {:.3}ms", self._time_data._average_present_time));
                        if constants::ENABLE_DYNAMIC_TARGET_FPS {
                            font_manager.log(format!("Target FPS: {:.2}", self._time_data._target_fps));
                        } else {
                            font_manager.log("Dynamic Target FPS: OFF".to_string());
                        }
                        let camera_position = scene_manager.get_main_camera()._transform_object.get_position();
                        let camera_rotation = scene_manager.get_main_camera()._transform_object.get_rotation();
                        font_manager.log(format!(
                            "Camera Position: [{:.2}, {:.2}, {:.2}], Rotation: [{:.2}, {:.2}, {:.2}]",
                            camera_position.x,
                            camera_position.y,
                            camera_position.z,
                            camera_rotation.x,
                            camera_rotation.y,
                            camera_rotation.z,
                        ));
                        font_manager.log(format!(
                            "StaticObjects: {}, SkeletalObjects: {}, DynamicUpdateObjects: {}, CollisionObjects: {}, InstancingRenderObjects: {}",
                            scene_manager._static_render_object_map.len(),
                            scene_manager._skeletal_render_object_map.len(),
                            scene_manager._dynamic_update_object_map.len(),
                            scene_manager._collision_object_key_map.len(),
                            scene_manager._static_render_object_instancing_map.len()
                        ));
                        font_manager.log(format!(
                            "RenderStaticMesh(Instancing): {}({}), RenderStaticShadow(Instancing): {}({})",
                            scene_manager._static_render_elements.len(),
                            scene_manager
                                ._static_render_elements
                                .iter()
                                .map(|element| element._num_render_instances)
                                .sum::<u32>(),
                            scene_manager._static_shadow_render_elements.len(),
                            scene_manager
                                ._static_shadow_render_elements
                                .iter()
                                .map(|element| element._num_render_instances)
                                .sum::<u32>()
                        ));
                        font_manager.log(format!(
                            "RenderSkeletalMesh(Instancing): {}({}), RenderSkeletalShadow(Instancing): {}({})",
                            scene_manager._skeletal_render_elements.len(),
                            scene_manager
                                ._skeletal_render_elements
                                .iter()
                                .map(|element| element._num_render_instances)
                                .sum::<u32>(),
                            scene_manager._skeletal_shadow_render_elements.len(),
                            scene_manager
                                ._skeletal_shadow_render_elements
                                .iter()
                                .map(|element| element._num_render_instances)
                                .sum::<u32>(),
                        ));
                        font_manager.log(format!(
                            "PointLight: {:?}, Render PointLight: {:?}",
                            scene_manager._point_light_object_map.len(),
                            scene_manager.get_render_point_light_count()
                        ));
                        font_manager.log(format!(
                            "RenderTarget: {:?}",
                            renderer_context._renderer_data._debug_render_target
                        ));
                        font_manager.log(format!(
                            "RenderQualityLevel: {:?}, Upsacle: {:?}, RenderSSR: {:?}, RenderOcean: {:?}, RenderAtmosphere: {:?}, RenderSky: {:?}, RenderShadow: {:?}, RenderSSAO: {:?}",
                             { constants::RENDER_QUALITY_LEVEL },
                             { constants::ENABLE_UPSCALE },
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderSSR),
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderOcean),
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderAtmosphere),
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderAtmosphere) && renderer_context._renderer_data.has_render_option(RenderOption::RenderSky),
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderShadow),
                            renderer_context._renderer_data.has_render_option(RenderOption::RenderShadow) && renderer_context._renderer_data.has_render_option(RenderOption::RenderSSAO),
                        ));

                        if let Some(time_profiler_mutex) = TIME_PROFILER.get() {
                            let mut time_profiler = time_profiler_mutex.lock().unwrap();
                            let mut keys: Vec<&String> = time_profiler.keys().collect();
                            keys.sort();
                            for key in keys {
                                let value = time_profiler.get(key).unwrap();
                                font_manager.log(format!("{}: {:.3}ms", key, value));
                            }
                            time_profiler.clear(); // Clear the HashMap for the next frame
                        }
                    }
                }

                // render scene
                let render_time = self._time_data.get_current_time();
                renderer_context.render_scene(
                    scene_manager,
                    debug_line_manager,
                    font_manager,
                    ui_manager,
                    delta_time,
                    &mut self._time_data,
                );
                self._time_data._acc_render_time += self._time_data.get_current_time() - render_time;
            }
        }
    }

    pub fn update_application(&self, delta_time: f64) {
        self.get_application_mut().update_application(delta_time);
    }
}

impl<'a> Drop for EngineCore<'a> {
    fn drop(&mut self) {
        self._renderer_context.destroy_renderer_context();
    }
}

struct EngineAppHandler<'a> {
    _app_name: String,
    _app_version: u32,
    _initial_window_size: Option<Vector2<u32>>,
    _initial_monitor_index: Option<usize>,
    _window_mode: WindowMode,
    _application: *const dyn ApplicationBase<'a>,
    _sdl: Sdl,
    _window: Option<Window>,
    _maybe_engine_core: Option<Box<EngineCore<'a>>>,
    _initialize_done: bool,
    _run_application: bool,
}

impl<'a> EngineAppHandler<'a> {
    fn select_primary_monitor(event_loop: &ActiveEventLoop) -> MonitorHandle {
        let available_monitors: Vec<MonitorHandle> = dedup_monitors(event_loop.available_monitors());
        event_loop
            .primary_monitor()
            .or_else(|| {
                available_monitors
                    .iter()
                    .find(|monitor| monitor.position().x == 0 && monitor.position().y == 0)
                    .cloned()
            })
            .or_else(|| available_monitors.into_iter().next())
            .expect("No available monitor found")
    }

    fn select_target_monitor(event_loop: &ActiveEventLoop, monitor_index: Option<usize>) -> MonitorHandle {
        if let Some(index) = monitor_index {
            let available_monitors: Vec<MonitorHandle> = dedup_monitors(event_loop.available_monitors());
            if let Some(monitor) = available_monitors.get(index) {
                return monitor.clone();
            }
        }
        Self::select_primary_monitor(event_loop)
    }

    fn init_engine_core(&mut self) {
        if self._maybe_engine_core.is_some() {
            let engine_core = self._maybe_engine_core.as_mut().unwrap().as_mut();
            engine_core.terminate_application();
        }

        let window = self._window.as_ref().unwrap();
        let engine_core = EngineCore::create_application(
            self._app_name.as_str(),
            self._app_version,
            window,
            self._window_mode,
            self._initial_monitor_index,
            self._initial_window_size,
            &self._sdl,
            self._application,
        );
        let window_size = engine_core._window_size;
        let application_mut = ptr_as_mut(engine_core._application);
        application_mut.initialize_application(&window_size);

        self._maybe_engine_core = Some(engine_core);
        self._run_application = true;
        self._initialize_done = true;
    }
}

impl<'a> ApplicationHandler for EngineAppHandler<'a> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        log::info!("Application was resumed");
        if self._window.is_none() {
            // show display infos
            for (monitor_index, monitor) in dedup_monitors(event_loop.available_monitors()).into_iter().enumerate() {
                log::debug!("Monitor[{}]: {:?}", monitor_index, monitor.name());
                for (video_index, video_mode) in monitor.video_modes().enumerate() {
                    log::debug!(
                        "    Video Mode[{}]: {:?}, {:?} bit, {:?} hz",
                        video_index,
                        video_mode.size(),
                        video_mode.bit_depth(),
                        video_mode.refresh_rate_millihertz()
                    );
                }
            }

            let target_monitor = Self::select_target_monitor(event_loop, self._initial_monitor_index);
            log::info!(
                "Target Monitor: {:?}, pos: {:?}, size: {:?}, {:?}hz",
                target_monitor.name(),
                target_monitor.position(),
                target_monitor.size(),
                target_monitor.refresh_rate_millihertz()
            );

            let monitor_size = target_monitor.size();
            let fallback_size = if monitor_size.width > 0 && monitor_size.height > 0 {
                monitor_size
            } else {
                find_best_video_mode(&target_monitor, None)
                    .map(|vm| vm.size())
                    .unwrap_or(dpi::PhysicalSize::new(DEFAULT_FALLBACK_WIDTH, DEFAULT_FALLBACK_HEIGHT))
            };

            let requested_size = self
                ._initial_window_size
                .map(|req_size| dpi::PhysicalSize::new(req_size.x.max(1), req_size.y.max(1)))
                .unwrap_or(fallback_size);

            let (window_size, initial_fullscreen) = match self._window_mode {
                WindowMode::WindowMode => (requested_size, None),
                WindowMode::FullScreenExclusiveMode => {
                    if !is_wayland_event_loop(event_loop) {
                        if let Some(video_mode) = find_best_video_mode(&target_monitor, None) {
                            (video_mode.size(), Some(Fullscreen::Exclusive(video_mode)))
                        } else {
                            (
                                fallback_size,
                                Some(Fullscreen::Borderless(Some(target_monitor.clone()))),
                            )
                        }
                    } else {
                        (
                            fallback_size,
                            Some(Fullscreen::Borderless(Some(target_monitor.clone()))),
                        )
                    }
                }
                WindowMode::FullScreenBorderlessMode => (
                    fallback_size,
                    Some(Fullscreen::Borderless(Some(target_monitor.clone()))),
                ),
            };

            let initial_position = if self._window_mode == WindowMode::WindowMode {
                compute_centered_window_pos(&target_monitor, Vector2::new(window_size.width, window_size.height))
            } else {
                target_monitor.position()
            };

            // create window
            log::info!("Window Mode: {:?}, Initial Size: {:?}", self._window_mode, window_size);
            let window_attributes = WindowAttributes::default()
                .with_title(self._app_name.clone())
                .with_position(initial_position)
                .with_inner_size(dpi::Size::Physical(window_size))
                .with_fullscreen(initial_fullscreen.clone());

            let window: Window = event_loop.create_window(window_attributes).expect("failed to create window");
            window.set_ime_allowed(false);

            match self._window_mode {
                WindowMode::WindowMode => {
                    window.set_decorations(true);
                    if !is_wayland_window(&window) {
                        window.set_outer_position(dpi::Position::Physical(initial_position));
                    }
                    let _ = window.request_inner_size(dpi::Size::Physical(window_size));
                }
                WindowMode::FullScreenExclusiveMode | WindowMode::FullScreenBorderlessMode => {
                    if !is_wayland_window(&window) {
                        window.set_fullscreen(initial_fullscreen);
                    }
                }
            }

            self._window = Some(window);
        }

        if !self._initialize_done {
            self.init_engine_core();
        }
    }

    fn suspended(&mut self, _event_loop: &ActiveEventLoop) {
        log::info!("Application was suspended");
        #[cfg(target_os = "android")]
        if self._initialize_done {
            self._run_application = false;
            self._initialize_done = false;
        }
    }

    fn new_events(&mut self, event_loop: &ActiveEventLoop, _cause: StartCause) {
        event_loop.set_control_flow(ControlFlow::Poll);

        let Some(engine_core) = self._maybe_engine_core.as_mut() else {
            return;
        };

        // reset input states on new frame
        if self._run_application {
            engine_core.clear_and_update_input_data_list();

            // process sdl event
            for event in self._sdl.event_pump().unwrap().poll_iter() {
                match event {
                    event::Event::ControllerAxisMotion { axis, value, .. } => {
                        engine_core._joystick_input_data.update_controller_axis_motion(axis, value);
                        engine_core.set_keyboard_input_mode(false);
                    }
                    event::Event::ControllerButtonDown { button, .. } => {
                        engine_core._joystick_input_data.update_controller_button_state(button, ButtonState::Pressed);
                        engine_core.set_keyboard_input_mode(false);
                    }
                    event::Event::ControllerButtonUp { button, .. } => {
                        engine_core._joystick_input_data.update_controller_button_state(button, ButtonState::Released);
                        engine_core.set_keyboard_input_mode(false);
                    }
                    event::Event::Quit { .. } => break,
                    _ => (),
                }
            }
        }

        if !self._run_application {
            engine_core.terminate_application();
            event_loop.exit();
        }
    }

    fn device_event(&mut self, _event_loop: &ActiveEventLoop, _device_id: DeviceId, event: DeviceEvent) {
        let Some(engine_core) = self._maybe_engine_core.as_mut() else {
            return;
        };
        match event {
            DeviceEvent::MouseMotion { delta } => {
                engine_core.update_mouse_motion(&delta);
            }
            DeviceEvent::Key(_raw_key) => {
                // Handled in WindowEvent::KeyboardInput for Wayland & X11 compatibility
            }
            _ => {}
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _window_id: WindowId, event: WindowEvent) {
        let Some(engine_core) = self._maybe_engine_core.as_mut() else {
            return;
        };
        match event {
            WindowEvent::CloseRequested => {
                self._run_application = false;
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                if self._run_application {
                    engine_core.update_event_and_render_scene();
                    if engine_core.get_application().will_terminate_application() {
                        self._run_application = false;
                        event_loop.exit();
                    }
                }
            }
            WindowEvent::Resized(size) => {
                log::info!(
                    "WindowEvent::Resized: {:?}, initialize_done: {}",
                    size,
                    self._initialize_done
                );
                if self._initialize_done {
                    engine_core.handle_window_resized(size);
                }
            }
            WindowEvent::MouseInput { button, state, .. } => {
                engine_core.update_mouse_input(button, state);
            }
            WindowEvent::CursorMoved { position, .. } => {
                engine_core.update_cursor_moved(position);
            }
            WindowEvent::MouseWheel {
                delta: MouseScrollDelta::LineDelta(scroll_x, scroll_y),
                ..
            } => {
                engine_core.update_mouse_wheel(scroll_x, scroll_y);
                engine_core.set_keyboard_input_mode(true);
            }
            WindowEvent::Focused(focused) => {
                engine_core.get_application_mut().focused(focused);
                if focused == false {
                    engine_core.clear_key_hold();
                }
            }
            WindowEvent::CursorEntered {
                device_id: _device_id, ..
            } => {}
            WindowEvent::CursorLeft {
                device_id: _device_id, ..
            } => {}
            WindowEvent::Ime(_event) => {
                // nothing
            }
            WindowEvent::KeyboardInput {
                event: raw_key,
                is_synthetic: _,
                ..
            } => {
                if self._run_application {
                    if !raw_key.repeat {
                        engine_core.update_keyboard_input(raw_key.physical_key, raw_key.state);
                        engine_core.set_keyboard_input_mode(true);
                    }
                }

                // exit
                if engine_core._is_grab_mode == false {
                    if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit1) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderSSR)
                    } else if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit2) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderOcean)
                    } else if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit3) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderAtmosphere)
                    } else if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit4) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderSky)
                    } else if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit5) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderShadow)
                    } else if engine_core._keyboard_input_data.get_key_pressed(KeyCode::Digit6) {
                        engine_core
                            ._renderer_context
                            .get_renderer_data_mut()
                            .toggle_render_option(RenderOption::RenderSSAO)
                    }
                }
            }
            WindowEvent::Touch(touch) => {
                engine_core.update_touch(&touch);
            }
            _ => (),
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        if self._run_application {
            if let Some(window) = self._window.as_ref() {
                window.request_redraw();
            }
        } else {
            event_loop.exit();
        }
    }

    fn exiting(&mut self, _event_loop: &ActiveEventLoop) {
        log::info!("Application destroyed");
    }
}

pub fn run_application<'a>(
    app_name: String,
    app_version: u32,
    initial_window_size: Option<Vector2<u32>>,
    initial_monitor_index: Option<usize>,
    window_mode: WindowMode,
    application: *const dyn ApplicationBase<'a>,
) {
    log::info!("run_application");

    // sdl2
    sdl2::hint::set("SDL_JOYSTICK_THREAD", "1");
    let sdl = sdl2::init().expect("failed to sdl2::init");

    let mut event_loop = EventLoop::new().unwrap();

    let mut app_handler = EngineAppHandler {
        _app_name: app_name,
        _app_version: app_version,
        _initial_window_size: initial_window_size,
        _initial_monitor_index: initial_monitor_index,
        _window_mode: window_mode,
        _application: application,
        _sdl: sdl,
        _window: None,
        _maybe_engine_core: None,
        _initialize_done: false,
        _run_application: false,
    };

    event_loop.run_app_on_demand(&mut app_handler).expect("failed to run application");
}
