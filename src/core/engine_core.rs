use std::time;

use log::{self, LevelFilter};
use nalgebra::Vector2;
use raw_window_handle::{HasRawDisplayHandle, RawDisplayHandle};
use sdl2::event;
use sdl2::Sdl;
use winit::dpi;
use winit::event::{
    DeviceEvent, ElementState, Event, KeyboardInput, MouseButton, MouseScrollDelta, Touch,
    TouchPhase, VirtualKeyCode, WindowEvent,
};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{CursorGrabMode, Fullscreen, Window, WindowBuilder};

use crate::core::input::{self, ButtonState};
use crate::audio::audio_manager::AudioManager;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::renderer_context::RendererContext;
use crate::resource::resource::{EngineResources, ApplicationResourcesBase};
use crate::scene::debug_line::DebugLineManager;
use crate::scene::font::FontManager;
use crate::scene::scene_manager::{ProjectSceneManagerBase, SceneManager};
use crate::scene::ui::{ApplicationUIManagerBase, UIManager};
use crate::utilities::logger;
use crate::utilities::system::{ptr_as_mut, ptr_as_ref};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WindowMode {
    WindowMode,
    FullScreenExclusiveMode,
    FullScreenBorderlessMode,
}

#[derive(Debug, Clone)]
pub struct TimeData {
    pub _acc_frame_time: f64,
    pub _acc_frame_count: i32,
    pub _elapsed_frame: u64,
    pub _average_frame_time: f64,
    pub _average_fps: f64,
    pub _current_time: f64,
    pub _elapsed_time_prev: f64,
    pub _elapsed_time: f64,
    pub _delta_time: f64,
}

pub fn create_time_data(elapsed_time: f64) -> TimeData {
    TimeData {
        _acc_frame_time: 0.0,
        _acc_frame_count: 0,
        _elapsed_frame: 0,
        _average_frame_time: 0.0,
        _average_fps: 0.0,
        _elapsed_time_prev: 0.0,
        _current_time: elapsed_time,
        _elapsed_time: 0.0,
        _delta_time: 0.0,
    }
}

impl TimeData {
    pub fn update_time_data(&mut self, current_time: f64) {
        let previous_time = self._current_time;
        let delta_time = current_time - previous_time;
        self._elapsed_time_prev = self._elapsed_time;
        let elapsed_time = self._elapsed_time + delta_time;
        let acc_frame_time = self._acc_frame_time + delta_time;
        let acc_frame_count = self._acc_frame_count + 1;
        self._elapsed_frame += 1;
        if 1.0 < acc_frame_time {
            let average_frame_time = acc_frame_time / (acc_frame_count as f64) * 1000.0;
            let average_fps = 1000.0 / average_frame_time;
            self._acc_frame_time = 0.0;
            self._acc_frame_count = 0;
            self._average_frame_time = average_frame_time;
            self._average_fps = average_fps;

            // debug text
            let text_fps = format!(
                "{:.2}fps / {:.3}ms",
                self._average_fps, self._average_frame_time
            );
            log::info!("{}", text_fps);
        } else {
            self._acc_frame_time = acc_frame_time;
            self._acc_frame_count = acc_frame_count;
        }
        self._current_time = current_time;
        self._elapsed_time = elapsed_time;
        self._delta_time = delta_time;
    }
}

pub trait ApplicationBase {
    fn initialize_application(
        &mut self,
        engine_core: &EngineCore,
        window_size: &Vector2<i32>,
    );
    fn terminate_application(&mut self);
    fn update_event(&mut self);
    fn update_application(&mut self, delta_time: f64);
}

pub struct EngineCore {
    pub _display_handle: RawDisplayHandle,
    pub _window: *const Window,
    pub _window_size: Vector2<i32>,
    pub _is_grab_mode: bool,
    pub _is_grab_mode_backup: bool,
    pub _time_data: TimeData,
    pub _camera_move_speed: f32,
    pub _keyboard_input_data: Box<input::KeyboardInputData>,
    pub _mouse_move_data: Box<input::MouseMoveData>,
    pub _mouse_input_data: Box<input::MouseInputData>,
    pub _joystick_input_data: Box<input::JoystickInputData>,
    pub _audio_manager: Box<AudioManager>,
    pub _effect_manager: Box<EffectManager>,
    pub _engine_resources: Box<EngineResources>,
    pub _debug_line_manager: Box<DebugLineManager>,
    pub _font_manager: Box<FontManager>,
    pub _renderer_context: Box<RendererContext>,
    pub _ui_manager: Box<UIManager>,
    pub _scene_manager: Box<SceneManager>,
    pub _application: *const dyn ApplicationBase,
}

impl EngineCore {
    pub fn get_application(&self) -> &dyn ApplicationBase {
        ptr_as_ref(self._application)
    }
    pub fn get_application_mut(&self) -> &mut dyn ApplicationBase {
        ptr_as_mut(self._application)
    }
    pub fn get_scene_manager(&self) -> &SceneManager {
        self._scene_manager.as_ref()
    }
    pub fn get_scene_manager_mut(&self) -> &mut SceneManager {
        ptr_as_mut(self._scene_manager.as_ref())
    }
    pub fn get_window(&self) -> &Window {
        ptr_as_ref(self._window)
    }
    pub fn get_audio_manager(&self) -> &AudioManager {
        self._audio_manager.as_ref()
    }
    pub fn get_audio_manager_mut(&self) -> &mut AudioManager {
        ptr_as_mut(self._audio_manager.as_ref())
    }
    pub fn get_effect_manager(&self) -> &EffectManager {
        self._effect_manager.as_ref()
    }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager {
        ptr_as_mut(self._effect_manager.as_ref())
    }
    pub fn get_engine_resources(&self) -> &EngineResources {
        self._engine_resources.as_ref()
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources {
        ptr_as_mut(self._engine_resources.as_ref())
    }
    pub fn get_debug_line_manager(&self) -> &DebugLineManager {
        self._debug_line_manager.as_ref()
    }
    pub fn get_debug_line_manager_mut(&self) -> &mut DebugLineManager {
        ptr_as_mut(self._debug_line_manager.as_ref())
    }
    pub fn get_font_manager(&self) -> &FontManager {
        self._font_manager.as_ref()
    }
    pub fn get_font_manager_mut(&self) -> &mut FontManager {
        ptr_as_mut(self._font_manager.as_ref())
    }
    pub fn get_renderer_context(&self) -> &RendererContext {
        self._renderer_context.as_ref()
    }
    pub fn get_renderer_context_mut(&self) -> &mut RendererContext {
        ptr_as_mut(self._renderer_context.as_ref())
    }
    pub fn get_ui_manager(&self) -> &UIManager {
        self._ui_manager.as_ref()
    }
    pub fn get_ui_manager_mut(&self) -> &mut UIManager {
        ptr_as_mut(self._ui_manager.as_ref())
    }
    pub fn create_application(
        app_name: &str,
        app_version: u32,
        display_handle: RawDisplayHandle,
        window: &Window,
        sdl: &Sdl,
        application: *const dyn ApplicationBase,
        application_resources: *const dyn ApplicationResourcesBase,
        project_scene_manager: *const dyn ProjectSceneManagerBase,
        application_ui_manager: *const dyn ApplicationUIManagerBase,
        elapsed_time: f64,
    ) -> Box<EngineCore> {
        // create managers
        let window_size: Vector2<i32> = Vector2::new(
            window.inner_size().width as i32,
            window.inner_size().height as i32,
        );
        let engine_resources = EngineResources::create_engine_resources(application_resources);
        let debug_line_manager = DebugLineManager::create_debug_line_manager();
        let font_manager = FontManager::create_font_manager();
        let ui_manager = UIManager::create_ui_manager(application_ui_manager);
        let renderer_context = RendererContext::create_renderer_context(
            app_name,
            app_version,
            display_handle,
            &window_size,
            window,
            engine_resources.as_ref(),
        );
        let effect_manager = EffectManager::create_effect_manager();
        let audio_manager = AudioManager::create_audio_manager(&sdl, engine_resources.as_ref());
        let scene_manager = SceneManager::create_scene_manager(project_scene_manager);
        let keyboard_input_data = input::create_keyboard_input_data();
        let mouse_move_data = input::create_mouse_move_data(&window_size.x / 2, &window_size.y / 2);
        let mouse_input_data = input::create_mouse_input_data();
        let joystick_input_data = input::JoystickInputData::create_joystick_input_data(sdl);
        let engine_core_ptr = Box::new(EngineCore {
            _display_handle: display_handle,
            _window: window,
            _window_size: window_size.into(),
            _is_grab_mode: false,
            _is_grab_mode_backup: false,
            _time_data: create_time_data(elapsed_time),
            _camera_move_speed: 1.0,
            _keyboard_input_data: keyboard_input_data,
            _mouse_move_data: mouse_move_data,
            _mouse_input_data: mouse_input_data,
            _joystick_input_data: joystick_input_data,
            _audio_manager: audio_manager,
            _debug_line_manager: debug_line_manager,
            _font_manager: font_manager,
            _ui_manager: ui_manager,
            _renderer_context: renderer_context,
            _engine_resources: engine_resources,
            _effect_manager: effect_manager,
            _scene_manager: scene_manager,
            _application: application,
        });

        // initialize managers
        let engine_core = engine_core_ptr.as_ref();
        engine_core
            .get_renderer_context_mut()
            .initialize_renderer_context(
                engine_core.get_engine_resources(),
                engine_core.get_effect_manager(),
            );
        engine_core
            .get_engine_resources_mut()
            .initialize_engine_resources(engine_core.get_renderer_context());
        engine_core
            .get_debug_line_manager_mut()
            .initialize_debug_line_manager(engine_core.get_renderer_context());
        engine_core
            .get_font_manager_mut()
            .initialize_font_manager(
                engine_core.get_renderer_context(),
                engine_core.get_engine_resources(),
            );
        engine_core
            .get_ui_manager_mut()
            .initialize_ui_manager(
                engine_core.get_renderer_context(),
                engine_core.get_engine_resources(),
            );
        engine_core
            .get_audio_manager_mut()
            .initialize_audio_manager();
        engine_core
            .get_effect_manager_mut()
            .initialize_effect_manager();

        // initialize graphics data
        engine_core
            .get_renderer_context()
            .prepare_framebuffer_and_descriptors();

        // initialize application
        engine_core
            .get_application_mut()
            .initialize_application(&engine_core, &window_size);
        engine_core_ptr
    }

    pub fn terminate_application(&mut self) {
        let renderer_context = self.get_renderer_context();

        // destroy managers
        self.get_application_mut()
            .terminate_application();
        self.get_audio_manager_mut().destroy_audio_manager();
        self.get_effect_manager_mut().destroy_effect_manager();
        self.get_ui_manager_mut()
            .destroy_ui_manager(renderer_context.get_device());
        self.get_debug_line_manager_mut()
            .destroy_debug_line_manager(renderer_context.get_device());
        self.get_font_manager_mut()
            .destroy_font_manager(renderer_context.get_device());
        self.get_engine_resources_mut()
            .destroy_engine_resources(renderer_context);
        self.get_renderer_context_mut().destroy_renderer_context();
    }

    pub fn resized_window(&mut self, size: dpi::PhysicalSize<u32>) {
        self._window_size.x = size.width as i32;
        self._window_size.y = size.height as i32;
        self.get_scene_manager_mut()
            .resized_window(size.width as i32, size.height as i32);

        let renderer_context = self.get_renderer_context_mut();
        let swapchain_extent = renderer_context.get_swap_chain_data()._swapchain_extent;
        let need_recreate_swapchain =
            swapchain_extent.width != size.width || swapchain_extent.height != size.height;
        log::info!(
            "need_recreate_swapchain: {}, swapchain_extent: {:?}",
            need_recreate_swapchain,
            swapchain_extent
        );
        if need_recreate_swapchain {
            renderer_context.set_need_recreate_swapchain(true);
        }
    }

    pub fn clear_and_update_input_data_list(&mut self) {
        self._mouse_move_data.clear_mouse_move_delta();
        self._mouse_input_data.clear_mouse_input();
        self._keyboard_input_data.clear_key_pressed();
        self._keyboard_input_data.clear_key_released();
        self._joystick_input_data.update_joystick_button_state();
    }

    pub fn update_mouse_motion(&mut self, delta: &(f64, f64)) {
        if self._is_grab_mode {
            let window_size = self._window_size.clone();
            self._mouse_move_data
                .update_mouse_move(&(delta.0 as i32, delta.1 as i32), &window_size);
            // window.set_cursor_position(dpi::PhysicalPosition { x: window_size.x / 2, y: window_size.y / 2 }).expect("failed to set_cursor_position");
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
        self._mouse_move_data
            .update_scroll_move(&(scroll_x as i32, scroll_y as i32));
    }

    pub fn update_cursor_moved(&mut self, position: dpi::PhysicalPosition<f64>) {
        if false == self._is_grab_mode {
            let window_size = self._window_size.clone();
            self._mouse_move_data
                .update_mouse_pos(&position.into(), &window_size);
        }
    }

    pub fn set_grab_mode(&mut self, is_grab_mode: bool) {
        self._is_grab_mode = is_grab_mode;
        let _result = self.get_window().set_cursor_grab(if is_grab_mode {
            CursorGrabMode::Confined
        } else {
            CursorGrabMode::None
        });
        self.get_window().set_cursor_visible(!is_grab_mode);
    }

    pub fn update_cursor_entered(&mut self) {
        self.set_grab_mode(self._is_grab_mode_backup);
    }

    pub fn update_cursor_mode(&mut self) {
        self._is_grab_mode_backup = self._is_grab_mode;
        self.set_grab_mode(false);
    }

    pub fn update_keyboard_input(&mut self, input: &KeyboardInput) {
        match input.virtual_keycode {
            Some(key) => {
                if ElementState::Pressed == input.state {
                    self._keyboard_input_data.set_key_pressed(key);
                } else {
                    self._keyboard_input_data.set_key_released(key);
                }
            }
            None => {}
        }
    }

    pub fn update_touch(&mut self, touch: &Touch) {
        if 0 == touch.id {
            let window_size = self._window_size.clone();
            self._mouse_move_data
                .update_mouse_pos(&touch.location.into(), &window_size);

            if TouchPhase::Started == touch.phase {
                self._mouse_input_data.btn_r_pressed(true);
                self._mouse_move_data.clear_mouse_move_delta();
            } else if TouchPhase::Ended == touch.phase {
                self._mouse_input_data.btn_r_pressed(false);
            }
        } else if 1 == touch.id {
            if TouchPhase::Started == touch.phase {
                self._keyboard_input_data.set_key_pressed(VirtualKeyCode::W);
            } else if TouchPhase::Ended == touch.phase {
                self._keyboard_input_data
                    .set_key_released(VirtualKeyCode::W);
            }
        }
    }

    pub fn update_event(&mut self, current_time: f64) -> bool {
        let renderer_context = ptr_as_mut(self._renderer_context.as_ref());
        let ui_manager = ptr_as_mut(self._ui_manager.as_ref());
        let debug_line_manager = ptr_as_mut(self._debug_line_manager.as_ref());
        let font_manager = ptr_as_mut(self._font_manager.as_ref());
        let audio_manager = ptr_as_mut(self._audio_manager.as_ref());
        let effect_manager = ptr_as_mut(self._effect_manager.as_ref());

        // exit
        if self
            ._keyboard_input_data
            .get_key_pressed(VirtualKeyCode::Escape)
        {
            self.terminate_application();
            return false;
        }

        // update timer
        self._time_data.update_time_data(current_time);

        // update application event
        self.get_application_mut().update_event();

        let elapsed_time = self._time_data._elapsed_time;
        let delta_time = self._time_data._delta_time;
        let elapsed_frame = self._time_data._elapsed_frame;

        if renderer_context.get_need_recreate_swapchain() {
            #[cfg(not(target_os = "android"))]
            {
                log::info!("<<begin recreate_swapchain>>");

                // destroy
                ui_manager.destroy_ui_graphics_data();
                font_manager.destroy_font_descriptor_sets();
                renderer_context.resize_window();

                // recreate
                font_manager.create_font_descriptor_sets(
                    &renderer_context,
                    &renderer_context.get_engine_resources(),
                );
                ui_manager.create_ui_graphics_data(&renderer_context.get_engine_resources());
                renderer_context.set_need_recreate_swapchain(false);

                log::info!("<<end recreate_swapchain>>");
            }
        } else {
            // update & render, If the resized event has not yet occurred, the window size may be 0.
            if 0 < self._window_size.x && 0 < self._window_size.y {
                self.update_application(delta_time);
                audio_manager.update_audio_manager();
                effect_manager.update_effects(delta_time);
                debug_line_manager.update_debug_line();
                font_manager.update_font_manager();
                ui_manager.update_ui_manager(
                    delta_time,
                    &self._window_size,
                    &self._time_data,
                    &self._keyboard_input_data,
                    &self._mouse_move_data,
                    &self._mouse_input_data,
                    &renderer_context.get_engine_resources(),
                );
                let scene_manager = self.get_scene_manager();
                renderer_context.render_scene(
                    scene_manager,
                    debug_line_manager,
                    font_manager,
                    ui_manager,
                    elapsed_time,
                    delta_time,
                    elapsed_frame,
                );
            }
        }

        return true;
    }

    pub fn update_application(&self, delta_time: f64) {
        self.get_application_mut()
            .update_application(delta_time);
    }
}

pub fn run_application(
    app_name: String,
    app_version: u32,
    initial_window_size: Vector2<i32>,
    window_mode: WindowMode,
    log_level: LevelFilter,
    application: *const dyn ApplicationBase,
    application_resources: *const dyn ApplicationResourcesBase,
    project_scene_manager: *const dyn ProjectSceneManagerBase,
    application_ui_manager: *const dyn ApplicationUIManagerBase,
) {
    logger::initialize_logger(log_level);

    log::info!("run_application");

    sdl2::hint::set("SDL_JOYSTICK_THREAD", "1");
    let sdl = sdl2::init().expect("failed to sdl2::init");

    let time_instance = time::Instant::now();
    let event_loop = EventLoop::new();
    let display_handle: RawDisplayHandle = event_loop.raw_display_handle();
    let window: Window = WindowBuilder::new()
        .with_title(app_name.clone())
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize {
            width: initial_window_size.x as u32,
            height: initial_window_size.y as u32,
        }))
        .build(&event_loop)
        .unwrap();

    log::info!("Window Mode: {:?}", window_mode);
    for (monitor_index, monitor) in event_loop.available_monitors().enumerate() {
        log::info!("Monitor[{}]: {:?}", monitor_index, monitor.name());
        for (video_index, video_mode) in monitor.video_modes().enumerate() {
            log::info!(
                "    Video Mode[{}]: {:?}, {:?} bit, {:?} hz",
                video_index,
                video_mode.size(),
                video_mode.bit_depth(),
                video_mode.refresh_rate_millihertz()
            );
        }
    }

    if WindowMode::FullScreenExclusiveMode == window_mode {
        // TODO: Choose monitor, video mode
        let first_monitor = event_loop.available_monitors().nth(0).unwrap();
        let video_mode = first_monitor.video_modes().nth(0).unwrap();
        if WindowMode::FullScreenExclusiveMode == window_mode {
            window.set_fullscreen(Some(Fullscreen::Exclusive(video_mode)));
        } else if WindowMode::FullScreenBorderlessMode == window_mode {
            window.set_fullscreen(Some(Fullscreen::Borderless(Some(first_monitor))));
        } else {
            log::error!("unexpected window mode: {:?}", window_mode);
        }
    }

    let mut maybe_engine_core: Option<Box<EngineCore>> = None;

    // main loop
    #[cfg(target_os = "android")]
    let mut need_initialize: bool = false;
    #[cfg(not(target_os = "android"))]
    let mut need_initialize: bool = true;
    let mut initialize_done: bool = false;
    let mut run_application: bool = false;
    event_loop.run(move |event, __window_target, control_flow| {
        let current_time = time_instance.elapsed().as_secs_f64();
        if need_initialize {
            if maybe_engine_core.is_some() {
                // clear up
                let engine_core = maybe_engine_core.as_mut().unwrap().as_mut();
                engine_core.terminate_application();
            }

            let engine_core = EngineCore::create_application(
                app_name.as_str(),
                app_version,
                display_handle,
                &window,
                &sdl,
                application,
                application_resources,
                project_scene_manager,
                application_ui_manager,
                current_time,
            );

            // set managers
            maybe_engine_core = Some(engine_core);
            run_application = true;
            need_initialize = false;
            initialize_done = true;
        }

        let engine_core = maybe_engine_core.as_mut().unwrap().as_mut();

        // winit event
        match event {
            Event::Resumed => {
                log::info!("Application was resumed");
                #[cfg(target_os = "android")]
                if false == initialize_done {
                    need_initialize = true;
                }
            }
            Event::Suspended => {
                log::info!("Application was suspended");
                #[cfg(target_os = "android")]
                if initialize_done {
                    run_application = false;
                    initialize_done = false;
                }
            }
            Event::NewEvents(_) => {
                // reset input states on new frame
                if run_application {
                    engine_core.clear_and_update_input_data_list();

                    // sdl event
                    for event in sdl.event_pump().unwrap().poll_iter() {
                        match event {
                            event::Event::ControllerAxisMotion { axis, value, .. } => {
                                engine_core
                                    ._joystick_input_data
                                    .update_controller_axis_motion(axis, value);
                            }
                            event::Event::ControllerButtonDown { button, .. } => {
                                engine_core
                                    ._joystick_input_data
                                    .update_controller_button_state(button, ButtonState::Pressed);
                            }
                            event::Event::ControllerButtonUp { button, .. } => {
                                engine_core
                                    ._joystick_input_data
                                    .update_controller_button_state(button, ButtonState::Released);
                            }
                            event::Event::Quit { .. } => break,
                            _ => (),
                        }
                    }
                }
            }
            Event::MainEventsCleared => {
                if run_application {
                    // update main event
                    let result = engine_core.update_event(current_time);
                    if false == result {
                        *control_flow = ControlFlow::Exit;
                        run_application = false;
                    }
                }
            }
            Event::DeviceEvent {
                device_id: _device_id,
                event,
            } => match event {
                DeviceEvent::MouseMotion { delta } => {
                    engine_core.update_mouse_motion(&delta);
                }
                _ => {}
            },
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {}
                WindowEvent::Resized(size) => {
                    log::info!(
                        "WindowEvent::Resized: {:?}, initialize_done: {}",
                        size,
                        initialize_done
                    );
                    if initialize_done {
                        engine_core.resized_window(size);
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
                }
                WindowEvent::CursorEntered {
                    device_id: _device_id,
                    ..
                } => {
                    engine_core.update_cursor_entered();
                }
                WindowEvent::CursorLeft {
                    device_id: _device_id,
                    ..
                } => {
                    engine_core.update_cursor_mode();
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if run_application {
                        engine_core.update_keyboard_input(&input);
                    }
                }
                WindowEvent::Touch(touch) => {
                    engine_core.update_touch(&touch);
                }
                _ => (),
            },
            Event::RedrawEventsCleared => {}
            Event::LoopDestroyed => {
                log::trace!("Application destroyed");
            }
            _ => (),
        }
    });
}