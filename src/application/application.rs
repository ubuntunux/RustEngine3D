use std::cell::RefMut;
use std::io::Write;
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
use winit::window::{
    //Fullscreen,
    WindowBuilder,
    Window
};
use winit::monitor::{MonitorHandle, VideoMode};

use crate::application::audio_manager::AudioManager;
use crate::application::scene_manager::{ SceneManager, ProjectSceneManagerBase };
use crate::application::input;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::font::FontManager;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::ui::{ ProjectUIManagerBase, UIManager };
use crate::resource::resource::{EngineResources, ProjectResourcesBase};
use crate::utilities::system::{ RcRefCell, newRcRefCell };
use crate::utilities::logger;

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
fn _prompt_for_monitor(event_loop: &EventLoop<()>) -> MonitorHandle {
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

fn _prompt_for_video_mode(monitor: &MonitorHandle) -> VideoMode {
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
    fn initialize_application(&mut self, engine_application: &EngineApplication);
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
    pub _audio_manager: RcRefCell<AudioManager>,
    pub _effect_manager: RcRefCell<EffectManager>,
    pub _engine_resources: RcRefCell<EngineResources>,
    pub _font_manager: RcRefCell<FontManager>,
    pub _renderer_context: RcRefCell<RendererContext>,
    pub _scene_manager: RcRefCell<SceneManager>,
    pub _ui_manager: RcRefCell<UIManager>,
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

    pub fn get_audio_manager(&self) -> &AudioManager {
        unsafe { &*self._audio_manager.as_ptr() }
    }

    pub fn get_effect_manager(&self) -> &EffectManager {
        unsafe { &*self._effect_manager.as_ptr() }
    }

    pub fn get_renderer_context(&self) -> &RendererContext {
        unsafe { &*self._renderer_context.as_ptr() }
    }

    pub fn terminate_application(
        &mut self,
        audio_manager: &mut AudioManager,
        effect_manager: &mut EffectManager,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        scene_manager: &mut SceneManager,
        engine_resources: &mut EngineResources,
        renderer_context: &mut RendererContext,
    ) {
        scene_manager.close_scene_data(renderer_context.get_device());

        // destroy managers
        self.get_application_mut().terminate_application();
        scene_manager.destroy_scene_manager(renderer_context.get_device());
        audio_manager.destroy_audio_manager();
        effect_manager.destroy_effect_manager();
        ui_manager.destroy_ui_manager(renderer_context.get_device());
        font_manager.destroy_font_manager(renderer_context.get_device());
        engine_resources.destroy_engine_resources(renderer_context);
        renderer_context.destroy_renderer_context();
    }

    pub fn clear_input_events(&mut self) {
        self._mouse_move_data.clear_mouse_move_delta();
        self._mouse_input_data.clear_mouse_input();
        self._keyboard_input_data.clear_key_pressed();
        self._keyboard_input_data.clear_key_released();
    }

    pub fn set_grab_mode(&mut self, is_grab_mode: bool) {
        self._is_grab_mode = is_grab_mode;
        let _result = self.get_window().set_cursor_grab(is_grab_mode);
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
    project_ui_manager: *const dyn ProjectUIManagerBase,
) {
    logger::initialize_logger(log_level);

    log::info!("run_application");

    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 1;
    let initial_window_size: Vector2<i32> = Vector2::new(1024, 768);

    let sdl = sdl2::init().expect("failed to sdl2::init");
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

    let mut maybe_engine_resources: Option<RcRefCell<EngineResources>> = None;
    let mut maybe_ui_manager: Option<RcRefCell<UIManager>> = None;
    let mut maybe_font_manager: Option<RcRefCell<FontManager>> = None;
    let mut maybe_renderer_context: Option<RcRefCell<RendererContext>> = None;
    let mut maybe_audio_manager: Option<RcRefCell<AudioManager>> = None;
    let mut maybe_effect_manager: Option<RcRefCell<EffectManager>> = None;
    let mut maybe_scene_manager: Option<RcRefCell<SceneManager>> = None;
    let mut maybe_engine_application: Option<RcRefCell<EngineApplication>> = None;

    // main loop
    #[cfg(target_os = "android")]
    let mut need_initialize: bool = false;
    #[cfg(not(target_os = "android"))]
    let mut need_initialize: bool = true;
    let mut initialize_done: bool = false;
    let mut run_application: bool = false;
    event_loop.run(move |event, __window_target, control_flow|{
        if need_initialize {
            if maybe_engine_application.is_some() {
                // clear up
                let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                let mut renderer_context: RefMut<RendererContext> = maybe_renderer_context.as_ref().unwrap().borrow_mut();
                let mut scene_manager: RefMut<SceneManager> = maybe_scene_manager.as_ref().unwrap().borrow_mut();
                let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                let mut ui_manager: RefMut<UIManager> = maybe_ui_manager.as_ref().unwrap().borrow_mut();
                let mut effect_manager: RefMut<EffectManager> = maybe_effect_manager.as_ref().unwrap().borrow_mut();
                let mut audio_manager: RefMut<AudioManager> = maybe_audio_manager.as_ref().unwrap().borrow_mut();

                engine_application.terminate_application(
                    &mut audio_manager,
                    &mut effect_manager,
                    &mut font_manager,
                    &mut ui_manager,
                    &mut scene_manager,
                    &mut maybe_engine_resources.as_ref().unwrap().borrow_mut(),
                    &mut renderer_context,
                );
            }

            // create managers
            let elapsed_time = time_instance.elapsed().as_secs_f64();
            let engine_resources = newRcRefCell(EngineResources::create_engine_resources(project_resources));
            let font_manager = newRcRefCell(FontManager::create_font_manager());
            let ui_manager = newRcRefCell(UIManager::create_ui_manager(project_ui_manager));
            let renderer_context = newRcRefCell(RendererContext::create_renderer_context(app_name, app_version, &window_size, &window, &engine_resources));
            let effect_manager = newRcRefCell(EffectManager::create_effect_manager());
            let audio_manager = newRcRefCell(AudioManager::create_audio_manager(&sdl, &engine_resources));
            let scene_manager = newRcRefCell(SceneManager::create_scene_manager(&renderer_context, &engine_resources, project_scene_manager));
            let keyboard_input_data = input::create_keyboard_input_data();
            let mouse_move_data = input::create_mouse_move_data(&window_size.x / 2, &window_size.y / 2);
            let mouse_input_data = input::create_mouse_input_data();

            // initialize managers
            renderer_context.borrow_mut().initialize_renderer_context(engine_resources.as_ptr(), effect_manager.as_ptr());
            engine_resources.borrow_mut().initialize_engine_resources(&mut renderer_context.borrow_mut());
            font_manager.borrow_mut().initialize_font_manager(&renderer_context.borrow(), &engine_resources.borrow());
            ui_manager.borrow_mut().initialize_ui_manager(&renderer_context.borrow(), &engine_resources.borrow());
            audio_manager.borrow_mut().initialize_audio_manager();
            effect_manager.borrow_mut().initialize_effect_manager();
            scene_manager.borrow_mut().initialize_scene_manager(
                &window_size,
                &renderer_context.borrow(),
                &effect_manager.borrow(),
                &engine_resources.borrow()
            );
            let engine_application = newRcRefCell(EngineApplication {
                _window: &window,
                _window_size: window_size.into(),
                _is_grab_mode: false,
                _is_grab_mode_backup: false,
                _time_data: create_time_data(elapsed_time),
                _camera_move_speed: 1.0,
                _keyboard_input_data: keyboard_input_data,
                _mouse_move_data: mouse_move_data,
                _mouse_input_data: mouse_input_data,
                _audio_manager: audio_manager.clone(),
                _font_manager: font_manager.clone(),
                _ui_manager: ui_manager.clone(),
                _scene_manager: scene_manager.clone(),
                _renderer_context: renderer_context.clone(),
                _engine_resources: engine_resources.clone(),
                _application: application,
                _effect_manager: effect_manager.clone(),
            });

            // initialize graphics data
            renderer_context.borrow_mut().prepare_framebuffer_and_descriptors();
            scene_manager.borrow_mut().initialize_scene_graphics_data();

            // initialize application
            engine_application.borrow().get_application_mut().initialize_application(&engine_application.borrow());

            // set managers
            maybe_engine_resources = Some(engine_resources);
            maybe_audio_manager = Some(audio_manager);
            maybe_effect_manager = Some(effect_manager);
            maybe_font_manager = Some(font_manager);
            maybe_ui_manager = Some(ui_manager);
            maybe_renderer_context = Some(renderer_context);
            maybe_scene_manager = Some(scene_manager);
            maybe_engine_application = Some(engine_application);
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
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    engine_application.clear_input_events();
                }
            },
            Event::MainEventsCleared => {
                if run_application {
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    let mut renderer_context: RefMut<RendererContext> = maybe_renderer_context.as_ref().unwrap().borrow_mut();
                    let mut scene_manager: RefMut<SceneManager> = maybe_scene_manager.as_ref().unwrap().borrow_mut();
                    let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                    let mut ui_manager: RefMut<UIManager> = maybe_ui_manager.as_ref().unwrap().borrow_mut();
                    let mut audio_manager: RefMut<AudioManager> = maybe_audio_manager.as_ref().unwrap().borrow_mut();
                    let mut effect_manager: RefMut<EffectManager> = maybe_effect_manager.as_ref().unwrap().borrow_mut();

                    // exit
                    if engine_application._keyboard_input_data.get_key_pressed(VirtualKeyCode::Escape) {
                        *control_flow = ControlFlow::Exit;
                        engine_application.terminate_application(
                            &mut audio_manager,
                            &mut effect_manager,
                            &mut font_manager,
                            &mut ui_manager,
                            &mut scene_manager,
                            &mut maybe_engine_resources.as_ref().unwrap().borrow_mut(),
                            &mut renderer_context,
                        );
                        run_application = false;
                        return;
                    }

                    // update event
                    engine_application.update_event();

                    // update timer
                    if engine_application._time_data.update_time_data(&time_instance) {
                        let text_fps = format!("{:.2}fps / {:.3}ms", engine_application._time_data._average_fps, engine_application._time_data._average_frame_time);
                        log::info!("{}", text_fps);
                    }

                    let elapsed_time = engine_application._time_data._elapsed_time;
                    let delta_time = engine_application._time_data._delta_time;
                    let elapsed_frame = engine_application._time_data._elapsed_frame;

                    if renderer_context.get_need_recreate_swapchain() {
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
                            scene_manager.destroy_scene_graphics_data(renderer_context.get_device());
                            ui_manager.destroy_ui_graphics_data();
                            font_manager.destroy_font_descriptor_sets();
                            renderer_context.resize_window();

                            // recreate
                            font_manager.create_font_descriptor_sets(&renderer_context, &renderer_context.get_engine_resources());
                            ui_manager.create_ui_graphics_data(&renderer_context.get_engine_resources());
                            scene_manager.initialize_scene_graphics_data();
                            renderer_context.set_need_recreate_swapchain(false);

                            log::info!("<<end recreate_swapchain>>");
                        }
                    } else {
                        // update & render, If the resized event has not yet occurred, the window size may be 0.
                        if 0 < engine_application._window_size.x && 0 < engine_application._window_size.y {
                            engine_application.update_application();
                            scene_manager.update_scene_manager(&engine_application._time_data, &mut font_manager);
                            audio_manager.update_audio_manager();
                            effect_manager.update_effects(delta_time);
                            font_manager.update();
                            ui_manager.update(
                                delta_time,
                                &engine_application._window_size,
                                &engine_application._time_data,
                                &engine_application._keyboard_input_data,
                                &engine_application._mouse_move_data,
                                &engine_application._mouse_input_data,
                                &renderer_context.get_engine_resources());
                            renderer_context.render_scene(&scene_manager, &mut font_manager, &mut ui_manager, elapsed_time, delta_time, elapsed_frame);
                        }
                    }
                }
            },
            Event::DeviceEvent { device_id: _device_id, event } => match event {
                DeviceEvent::MouseMotion { delta } => {
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    if engine_application._is_grab_mode {
                        let window_size = engine_application._window_size.clone();
                        engine_application._mouse_move_data.update_mouse_move(&(delta.0 as i32, delta.1 as i32), &window_size);
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
                        let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                        let scene_manager: RefMut<SceneManager> = maybe_scene_manager.as_ref().unwrap().borrow_mut();
                        let mut renderer_context: RefMut<RendererContext> = maybe_renderer_context.as_ref().unwrap().borrow_mut();
                        engine_application._window_size.x = size.width as i32;
                        engine_application._window_size.y = size.height as i32;
                        scene_manager.resized_window(size.width as i32, size.height as i32);
                        let swapchain_extent = renderer_context._swapchain_data._swapchain_extent;
                        let need_recreate_swapchain = swapchain_extent.width != size.width || swapchain_extent.height != size.height;
                        log::info!("need_recreate_swapchain: {}, swapchain_extent: {:?}", need_recreate_swapchain, swapchain_extent);
                        if need_recreate_swapchain {
                            renderer_context.set_need_recreate_swapchain(true);
                        }
                    }
                },
                WindowEvent::MouseInput { button, state, .. } => {
                    let engine_application = &mut maybe_engine_application.as_ref().unwrap().borrow_mut();
                    let mouse_input_data = &mut engine_application._mouse_input_data;
                    let pressed = state == ElementState::Pressed;
                    match button {
                        MouseButton::Left => mouse_input_data.btn_l_pressed(pressed),
                        MouseButton::Middle => mouse_input_data.btn_m_pressed(pressed),
                        MouseButton::Right => mouse_input_data.btn_r_pressed(pressed),
                        _ => (),
                    }
                }
                WindowEvent::CursorMoved { position, .. } => {
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    if false == engine_application._is_grab_mode {
                        engine_application._mouse_move_data.update_mouse_pos(&position.into(), &window_size);
                    }
                }
                WindowEvent::MouseWheel { delta: MouseScrollDelta::LineDelta(scroll_x, scroll_y), .. } => {let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    engine_application._mouse_move_data.update_scroll_move(&(scroll_x as i32, scroll_y as i32));
                }
                WindowEvent::CursorEntered { device_id: _device_id, .. } => {
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    let is_grab_mode_backup = engine_application._is_grab_mode_backup;
                    engine_application.set_grab_mode(is_grab_mode_backup);
                }
                WindowEvent::CursorLeft { device_id: _device_id, .. } => {
                    let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                    engine_application._is_grab_mode_backup = engine_application._is_grab_mode;
                    engine_application.set_grab_mode(false);
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if run_application {
                        let mut engine_application: RefMut<EngineApplication> = maybe_engine_application.as_ref().unwrap().borrow_mut();
                        match input.virtual_keycode {
                            Some(key) => {
                                if ElementState::Pressed == input.state {
                                    engine_application._keyboard_input_data.set_key_pressed(key);
                                } else {
                                    engine_application._keyboard_input_data.set_key_released(key);
                                }
                            }
                            None => {}
                        }
                    }
                }
                WindowEvent::Touch(Touch { device_id: _device_id, phase, location, force: _force, id }) => {
                    let engine_application = &mut maybe_engine_application.as_ref().unwrap().borrow_mut();

                    if 0 == id {
                        let window_size = engine_application._window_size.clone();
                        engine_application._mouse_move_data.update_mouse_pos(&location.into(), &window_size);

                        if phase == TouchPhase::Started {
                            engine_application._mouse_input_data.btn_r_pressed(true);
                            engine_application._mouse_move_data.clear_mouse_move_delta();
                        } else if phase == TouchPhase::Ended {
                            engine_application._mouse_input_data.btn_r_pressed(false);
                        }
                    } else if 1 == id {
                        if phase == TouchPhase::Started {
                            engine_application._keyboard_input_data.set_key_pressed(VirtualKeyCode::W);
                        } else if phase == TouchPhase::Ended {
                            engine_application._keyboard_input_data.set_key_released(VirtualKeyCode::W);
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
