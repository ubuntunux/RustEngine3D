use std::cell::RefMut;
use std::time;
use log::{ self, LevelFilter };

use nalgebra::Vector2;
use winit::event::{
    ElementState,
    Event,
    MouseButton,
    MouseScrollDelta,
    Touch,
    TouchPhase,
    VirtualKeyCode,
    WindowEvent,
    DeviceEvent,
};
use winit::event_loop::{
    ControlFlow,
    EventLoop
};
use winit::dpi;
use winit::window::{Fullscreen, WindowBuilder, Window};
use winit::monitor::{MonitorHandle, VideoMode};

use crate::application::scene_manager::{ SceneManagerData, ProjectSceneManagerBase };
use crate::application::input;
use crate::resource::resource::{Resources, ProjectResourcesBase};
use crate::renderer::effect::{ EffectManagerData, ProjectEffectManagerBase };
use crate::renderer::renderer::{ RendererData, ProjectRendererBase };
use crate::renderer::font::FontManager;
use crate::renderer::ui::{ ProjectUIManagerBase, UIManagerData };
use crate::utilities::system::{ RcRefCell, newRcRefCell };
use crate::utilities::logger;
use std::io::Write;

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
    pub _delta_time: f64
}

// Enumerate monitors and prompt user to choose one
fn prompt_for_monitor(event_loop: &EventLoop<()>) -> MonitorHandle {
    for (num, monitor) in event_loop.available_monitors().enumerate() {
        println!("Monitor #{}: {:?}", num, monitor.name());
    }

    print!("Please write the number of the monitor to use: ");
    std::io::stdout().flush().unwrap();

    let mut num = String::new();
    std::io::stdin().read_line(&mut num).unwrap();
    let num = num.trim().parse().ok().expect("Please enter a number");
    let monitor = event_loop
        .available_monitors()
        .nth(num)
        .expect("Please enter a valid ID");

    println!("Using {:?}", monitor.name());

    monitor
}

fn prompt_for_video_mode(monitor: &MonitorHandle) -> VideoMode {
    for (i, video_mode) in monitor.video_modes().enumerate() {
        println!("Video mode #{}: {}", i, video_mode);
    }

    print!("Please write the number of the video mode to use: ");
    std::io::stdout().flush().unwrap();

    let mut num = String::new();
    std::io::stdin().read_line(&mut num).unwrap();
    let num = num.trim().parse().ok().expect("Please enter a number");
    let video_mode = monitor
        .video_modes()
        .nth(num)
        .expect("Please enter a valid ID");

    println!("Using {}", video_mode);

    video_mode
}

pub fn create_time_data(elapsed_time: f64) -> TimeData {
    TimeData {
        _acc_frame_time: 0.0,
        _acc_frame_count: 0,
        _elapsed_frame: 0,
        _average_frame_time: 0.0,
        _average_fps: 0.0,
        _elapsed_time_prev: elapsed_time,
        _current_time: elapsed_time,
        _elapsed_time: elapsed_time,
        _delta_time: 0.0
    }
}

impl TimeData {
    pub fn update_time_data(&mut self, time_instance: &time::Instant) -> bool {
        let mut debug_text: bool = false;
        let current_time = time_instance.elapsed().as_secs_f64();
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
            debug_text = true;
        } else {
            self._acc_frame_time = acc_frame_time;
            self._acc_frame_count = acc_frame_count;
        }
        self._current_time = current_time;
        self._elapsed_time = elapsed_time;
        self._delta_time = delta_time;
        debug_text
    }
}

pub trait ApplicationBase {
    fn initialize_application(&mut self, application_data: &EngineApplication);
    fn terminate_application(&mut self);
    fn update_event(&mut self);
    fn update_application(&mut self);
}

pub struct EngineApplication {
    pub _window: *const Window,
    pub _is_grab_mode: bool,
    pub _is_grab_mode_backup: bool,
    pub _window_size: Vector2<i32>,
    pub _time_data: TimeData,
    pub _camera_move_speed: f32,
    pub _keyboard_input_data: Box<input::KeyboardInputData>,
    pub _mouse_move_data: Box<input::MouseMoveData>,
    pub _mouse_input_data: Box<input::MouseInputData>,
    pub _scene_manager_data: RcRefCell<SceneManagerData>,
    pub _renderer_data: RcRefCell<RendererData>,
    pub _effect_manager_data: RcRefCell<EffectManagerData>,
    pub _font_manager: RcRefCell<FontManager>,
    pub _ui_manager_data: RcRefCell<UIManagerData>,
    pub _resources: RcRefCell<Resources>,
    pub _application: *const dyn ApplicationBase,
}

impl EngineApplication {
    pub fn get_application(&self) -> &dyn ApplicationBase {
        unsafe { &(*self._application) }
    }

    pub fn get_application_mut(&self) -> &mut dyn ApplicationBase {
        unsafe { &mut *(self._application as *mut dyn ApplicationBase) }
    }

    pub fn get_window(&self) -> &Window {
        unsafe { &*self._window }
    }

    pub fn terminate_application(
        &mut self,
        effect_manager_data: &mut EffectManagerData,
        font_manager: &mut FontManager,
        ui_manager_data: &mut UIManagerData,
        scene_manager_data: &mut SceneManagerData,
        resources: &mut Resources,
        renderer_data: &mut RendererData,
    ) {
        scene_manager_data.close_scene_data(renderer_data.get_device());

        // destroy managers
        self.get_application_mut().terminate_application();
        effect_manager_data.destroy_effect_manager_data();
        scene_manager_data.destroy_scene_manager_data(renderer_data.get_device());
        ui_manager_data.destroy_ui_manager_data(renderer_data.get_device());
        font_manager.destroy_font_manager(renderer_data.get_device());
        resources.destroy_resources(renderer_data);
        renderer_data.destroy_renderer_data();
    }

    pub fn clear_input_events(&mut self) {
        self._mouse_move_data.clear_mouse_move_delta();
        self._mouse_input_data.clear_mouse_input();
        self._keyboard_input_data.clear_key_pressed();
        self._keyboard_input_data.clear_key_released();
    }

    pub fn set_grab_mode(&mut self, is_grab_mode: bool) {
        self._is_grab_mode = is_grab_mode;
        self.get_window().set_cursor_grab(is_grab_mode).expect("failed to set_cursor_grab");
        self.get_window().set_cursor_visible(!is_grab_mode);
    }

    pub fn update_event(&self) {
        self.get_application_mut().update_event();
    }

    pub fn update_application(&self) {
        self.get_application_mut().update_application();
    }
}

pub fn run_application(
    log_level: LevelFilter,
    application: *const dyn ApplicationBase,
    project_resources: *const dyn ProjectResourcesBase,
    project_scene_manager: *const dyn ProjectSceneManagerBase,
    project_effect_manager: *const dyn ProjectEffectManagerBase,
    project_renderer: *const dyn ProjectRendererBase,
    project_ui_manager: *const dyn ProjectUIManagerBase,
) {
    logger::initialize_logger(log_level);

    log::info!("run_application");

    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 1;
    let initial_window_size: Vector2<i32> = Vector2::new(1024, 768);

    let time_instance = time::Instant::now();
    let event_loop = EventLoop::new();
    let window: Window = WindowBuilder::new()
        .with_title(app_name)
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width: initial_window_size.x as u32, height: initial_window_size.y as u32 }))
        //.with_fullscreen(Some(Fullscreen::Exclusive(prompt_for_video_mode(&prompt_for_monitor(&event_loop)))))
        //.with_fullscreen(Some(Fullscreen::Borderless(Some(prompt_for_monitor(&event_loop)))))
        .build(&event_loop)
        .unwrap();
    let window_size: Vector2<i32> = Vector2::new(window.inner_size().width as i32, window.inner_size().height as i32);

    let mut maybe_resources: Option<RcRefCell<Resources>> = None;
    let mut maybe_ui_manager_data: Option<RcRefCell<UIManagerData>> = None;
    let mut maybe_font_manager: Option<RcRefCell<FontManager>> = None;
    let mut maybe_renderer_data: Option<RcRefCell<RendererData>> = None;
    let mut maybe_scene_manager_data: Option<RcRefCell<SceneManagerData>> = None;
    let mut maybe_application_data: Option<RcRefCell<EngineApplication>> = None;
    let mut maybe_effect_manager_data: Option<RcRefCell<EffectManagerData>> = None;

    // main loop
    #[cfg(target_os = "android")]
    let mut need_initialize: bool = false;
    #[cfg(not(target_os = "android"))]
    let mut need_initialize: bool = true;
    let mut initialize_done: bool = false;
    let mut run_application: bool = false;
    event_loop.run(move |event, __window_target, control_flow|{
        if need_initialize {
            if maybe_application_data.is_some() {
                // clear up
                let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                let mut scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                let mut ui_manager_data: RefMut<UIManagerData> = maybe_ui_manager_data.as_ref().unwrap().borrow_mut();
                let mut effect_manager_data: RefMut<EffectManagerData> = maybe_effect_manager_data.as_ref().unwrap().borrow_mut();
                application_data.terminate_application(
                    &mut effect_manager_data,
                    &mut font_manager,
                    &mut ui_manager_data,
                    &mut scene_manager_data,
                    &mut maybe_resources.as_ref().unwrap().borrow_mut(),
                    &mut renderer_data,
                );
            }

            // create managers
            let elapsed_time = time_instance.elapsed().as_secs_f64();
            let resources = newRcRefCell(Resources::create_resources(project_resources));
            let font_manager = newRcRefCell(FontManager::create_font_manager());
            let ui_manager_data = newRcRefCell(UIManagerData::create_ui_manager_data(project_ui_manager));
            let renderer_data = newRcRefCell(RendererData::create_renderer_data(app_name, app_version, &window_size, &window, &resources, project_renderer));
            let scene_manager_data = newRcRefCell(SceneManagerData::create_scene_manager_data(&renderer_data, &resources, project_scene_manager));
            let effect_manager_data = newRcRefCell(EffectManagerData::create_effect_manager_data(&renderer_data, &resources, project_effect_manager));
            let keyboard_input_data = input::create_keyboard_input_data();
            let mouse_move_data = input::create_mouse_move_data(&window_size.x / 2, &window_size.y / 2);
            let mouse_input_data = input::create_mouse_input_data();

            // initialize managers
            renderer_data.borrow_mut().initialize_renderer_data(effect_manager_data.as_ptr());
            resources.borrow_mut().initialize_resources(&mut renderer_data.borrow_mut());
            font_manager.borrow_mut().initialize_font_manager(&renderer_data.borrow(), &resources.borrow());
            ui_manager_data.borrow_mut().initialize_ui_manager_data(&renderer_data.borrow(), &resources.borrow());
            effect_manager_data.borrow_mut().initialize_project_effect_manager();
            scene_manager_data.borrow_mut().initialize_scene_manager_data(
                &window_size,
                &renderer_data.borrow(),
                &resources.borrow(),
                effect_manager_data.as_ptr(),
            );
            let application_data = newRcRefCell(EngineApplication {
                _window: &window,
                _window_size: window_size.into(),
                _is_grab_mode: false,
                _is_grab_mode_backup: false,
                _time_data: create_time_data(elapsed_time),
                _camera_move_speed: 1.0,
                _keyboard_input_data: keyboard_input_data,
                _mouse_move_data: mouse_move_data,
                _mouse_input_data: mouse_input_data,
                _font_manager: font_manager.clone(),
                _ui_manager_data: ui_manager_data.clone(),
                _effect_manager_data: effect_manager_data.clone(),
                _scene_manager_data: scene_manager_data.clone(),
                _renderer_data: renderer_data.clone(),
                _resources: resources.clone(),
                _application: application,
            });

            // initialize graphics data
            renderer_data.borrow_mut().prepare_framebuffer_and_descriptors();
            effect_manager_data.borrow_mut().prepare_framebuffer_and_descriptors(&renderer_data.borrow(), &resources.borrow());
            scene_manager_data.borrow_mut().initialize_scene_graphics_data();

            // initialize application
            application_data.borrow().get_application_mut().initialize_application(&application_data.borrow());

            // set managers
            maybe_resources = Some(resources);
            maybe_font_manager = Some(font_manager);
            maybe_ui_manager_data = Some(ui_manager_data);
            maybe_renderer_data = Some(renderer_data);
            maybe_scene_manager_data = Some(scene_manager_data);
            maybe_effect_manager_data = Some(effect_manager_data);
            maybe_application_data = Some(application_data);
            run_application = true;
            need_initialize = false;
            initialize_done = true;
        }

        match event {
            Event::Resumed => {
                log::info!("Application was resumed");
                #[cfg(target_os = "android")]
                if false == initialize_done {
                    need_initialize = true;
                }
            },
            Event::Suspended => {
                log::info!("Application was suspended");
                #[cfg(target_os = "android")]
                if initialize_done {
                    run_application = false;
                    initialize_done = false;
                }
            },
            Event::NewEvents(_) => {
                // reset input states on new frame
                if run_application {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    application_data.clear_input_events();
                }
            },
            Event::MainEventsCleared => {
                if run_application {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                    let mut scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                    let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                    let mut ui_manager_data: RefMut<UIManagerData> = maybe_ui_manager_data.as_ref().unwrap().borrow_mut();
                    let mut effect_manager_data: RefMut<EffectManagerData> = maybe_effect_manager_data.as_ref().unwrap().borrow_mut();

                    // exit
                    if application_data._keyboard_input_data.get_key_pressed(VirtualKeyCode::Escape) {
                        *control_flow = ControlFlow::Exit;
                        application_data.terminate_application(
                            &mut effect_manager_data,
                            &mut font_manager,
                            &mut ui_manager_data,
                            &mut scene_manager_data,
                            &mut maybe_resources.as_ref().unwrap().borrow_mut(),
                            &mut renderer_data,
                        );
                        run_application = false;
                        return;
                    }

                    // update event
                    application_data.update_event();

                    // update timer
                    if application_data._time_data.update_time_data(&time_instance) {
                        let text_fps = format!("{:.2}fps / {:.3}ms", application_data._time_data._average_fps, application_data._time_data._average_frame_time);
                        log::info!("{}", text_fps);
                    }

                    let elapsed_time = application_data._time_data._elapsed_time;
                    let delta_time = application_data._time_data._delta_time;
                    let elapsed_frame = application_data._time_data._elapsed_frame;

                    if renderer_data.get_need_recreate_swapchain() {
                        #[cfg(target_os = "android")]
                        {
                            // Destroy app on suspend for android target.
                            run_application = false;
                            initialize_done = false;
                        }
                        #[cfg(not(target_os = "android"))]
                        {
                            log::info!("<<begin recreate_swapchain>>");

                            // destroy
                            scene_manager_data.destroy_scene_graphics_data(renderer_data.get_device());
                            effect_manager_data.destroy_framebuffer_and_descriptors(&renderer_data);
                            ui_manager_data.destroy_ui_graphics_data();
                            font_manager.destroy_font_descriptor_sets();
                            renderer_data.resize_window();

                            // recreate
                            font_manager.create_font_descriptor_sets(&renderer_data, &renderer_data._resources.borrow());
                            ui_manager_data.create_ui_graphics_data(&renderer_data, &renderer_data._resources.borrow());
                            effect_manager_data.prepare_framebuffer_and_descriptors(&renderer_data, &renderer_data._resources.borrow());
                            scene_manager_data.initialize_scene_graphics_data();
                            renderer_data.set_need_recreate_swapchain(false);

                            log::info!("<<end recreate_swapchain>>");
                        }
                    } else {
                        // update & render, If the resized event has not yet occurred, the window size may be 0.
                        if 0 < application_data._window_size.x && 0 < application_data._window_size.y {
                            application_data.update_application();
                            renderer_data.update_post_process_datas();
                            scene_manager_data.update_scene_manager_data(&application_data._time_data, &mut font_manager);
                            font_manager.update();
                            ui_manager_data.update(
                                delta_time,
                                &application_data._window_size,
                                &application_data._time_data,
                                &application_data._keyboard_input_data,
                                &application_data._mouse_move_data,
                                &application_data._mouse_input_data,
                                &renderer_data._resources.borrow());
                            renderer_data.render_scene(&scene_manager_data, &mut font_manager, &mut ui_manager_data, elapsed_time, delta_time, elapsed_frame);
                        }
                    }
                }
            },
            Event::DeviceEvent { device_id, event } => match event {
                DeviceEvent::MouseMotion { delta } => {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    if application_data._is_grab_mode {
                        let window_size = application_data._window_size.clone();
                        application_data._mouse_move_data.update_mouse_move(&(delta.0 as i32, delta.1 as i32), &window_size);
                        // window.set_cursor_position(dpi::PhysicalPosition { x: window_size.x / 2, y: window_size.y / 2 }).expect("failed to set_cursor_position");
                    }
                },
                _ => {}
            },
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                },
                WindowEvent::Resized(size) => {
                    log::info!("WindowEvent::Resized: {:?}, initialize_done: {}", size, initialize_done);
                    if initialize_done {
                        let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                        let scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                        let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                        application_data._window_size.x = size.width as i32;
                        application_data._window_size.y = size.height as i32;
                        scene_manager_data.resized_window(size.width as i32, size.height as i32);
                        let swapchain_extent = renderer_data._swapchain_data._swapchain_extent;
                        let need_recreate_swapchain = swapchain_extent.width != size.width || swapchain_extent.height != size.height;
                        log::info!("need_recreate_swapchain: {}, swapchain_extent: {:?}", need_recreate_swapchain, swapchain_extent);
                        if need_recreate_swapchain {
                            renderer_data.set_need_recreate_swapchain(true);
                        }
                    }
                },
                WindowEvent::MouseInput { button, state, .. } => {
                    let application_data = &mut maybe_application_data.as_ref().unwrap().borrow_mut();
                    let mouse_input_data = &mut application_data._mouse_input_data;
                    let pressed = state == ElementState::Pressed;
                    match button {
                        MouseButton::Left => mouse_input_data.btn_l_pressed(pressed),
                        MouseButton::Middle => mouse_input_data.btn_m_pressed(pressed),
                        MouseButton::Right => mouse_input_data.btn_r_pressed(pressed),
                        _ => (),
                    }
                }
                WindowEvent::CursorMoved { position, .. } => {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    if false == application_data._is_grab_mode {
                        application_data._mouse_move_data.update_mouse_pos(&position.into(), &window_size);
                    }
                }
                WindowEvent::MouseWheel { delta: MouseScrollDelta::LineDelta(_, _v_lines), .. } => {
                    // wheel_delta = Some(v_lines);
                }
                WindowEvent::CursorEntered { device_id, .. } => {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    let is_grab_mode_backup = application_data._is_grab_mode_backup;
                    application_data.set_grab_mode(is_grab_mode_backup);
                }
                WindowEvent::CursorLeft { device_id, .. } => {
                    let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    application_data._is_grab_mode_backup = application_data._is_grab_mode;
                    application_data.set_grab_mode(false);
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if run_application {
                        let mut application_data: RefMut<EngineApplication> = maybe_application_data.as_ref().unwrap().borrow_mut();
                        match input.virtual_keycode {
                            Some(key) => {
                                if ElementState::Pressed == input.state {
                                    application_data._keyboard_input_data.set_key_pressed(key);
                                } else {
                                    application_data._keyboard_input_data.set_key_released(key);
                                }
                            }
                            None => {}
                        }
                    }
                }
                WindowEvent::Touch(Touch { device_id: _device_id, phase, location, force: _force, id }) => {
                    let application_data = &mut maybe_application_data.as_ref().unwrap().borrow_mut();

                    if 0 == id {
                        let window_size = application_data._window_size.clone();
                        application_data._mouse_move_data.update_mouse_pos(&location.into(), &window_size);

                        if phase == TouchPhase::Started {
                            application_data._mouse_input_data.btn_r_pressed(true);
                            application_data._mouse_move_data.clear_mouse_move_delta();
                        } else if phase == TouchPhase::Ended {
                            application_data._mouse_input_data.btn_r_pressed(false);
                        }
                    } else if 1 == id {
                        if phase == TouchPhase::Started {
                            application_data._keyboard_input_data.set_key_pressed(VirtualKeyCode::W);
                        } else if phase == TouchPhase::Ended {
                            application_data._keyboard_input_data.set_key_released(VirtualKeyCode::W);
                        }
                    }
                }
                _ => (),
            },
            Event::RedrawEventsCleared => {
            },
            Event::LoopDestroyed => {
                log::trace!("Application destroyed");
            }
            _ => (),
        }
    });
}
