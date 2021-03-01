use std::cell::RefMut;
use std::time;
use log;

use winit::event::{
    ElementState,
    Event,
    MouseButton,
    MouseScrollDelta,
    Touch,
    TouchPhase,
    VirtualKeyCode,
    WindowEvent
};
use winit::event_loop::{
    ControlFlow,
    EventLoop
};
use winit::dpi;
use winit::window::{ WindowBuilder };

use crate::constants::{ Constants };
use crate::application::scene_manager::{ SceneManagerData, SceneManagerBase };
use crate::application::input;
use crate::resource::resource::Resources;
use crate::renderer::renderer::{ RendererData, RendererBase };
use crate::renderer::font::FontManager;
use crate::renderer::ui::UIManager;
use crate::utilities::system::{self, RcRefCell, newRcRefCell};
use crate::utilities::logger;

#[derive(Debug, Clone)]
pub struct TimeData {
    _acc_frame_time: f64,
    _acc_frame_count: i32,
    _elapsed_frame: u64,
    _average_frame_time: f64,
    _average_fps: f64,
    _current_time: f64,
    _elapsed_time_prev: f64,
    _elapsed_time: f64,
    _delta_time: f64
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
    pub fn update_time_data(&mut self, time_instance: &time::Instant) {
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
    fn update_event(&mut self, scene_manager_data: &SceneManagerData);
    fn terminate_applicateion(&mut self);
}

pub struct ApplicationData {
    _constants: Constants,
    _window_size: (u32, u32),
    _time_data: TimeData,
    _camera_move_speed: f32,
    _keyboard_input_data: Box<input::KeyboardInputData>,
    _mouse_move_data: Box<input::MouseMoveData>,
    _mouse_input_data: Box<input::MouseInputData>,
    _scene_manager_data: RcRefCell<SceneManagerData>,
    _renderer_data: RcRefCell<RendererData>,
    _font_manager: RcRefCell<FontManager>,
    _ui_manager: RcRefCell<UIManager>,
    _resources: RcRefCell<Resources>,
    _application: RcRefCell<dyn ApplicationBase>,
}

impl ApplicationData {
    pub fn terminate_applicateion(
        &mut self,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        scene_manager_data: &mut SceneManagerData,
        resources: &mut Resources,
        renderer_data: &mut RendererData,
    ) {
        self._application.borrow_mut().terminate_applicateion();
        scene_manager_data.close_scene_manager_data(renderer_data.get_device());
        ui_manager.destroy_ui_manager(renderer_data.get_device());
        font_manager.destroy_font_manager(renderer_data.get_device());
        renderer_data.destroy_framebuffer_and_descriptors();
        resources.destroy_resources(renderer_data);
        renderer_data.destroy_renderer_data();
    }

    pub fn clear_input_events(&mut self) {
        self._mouse_move_data.clear_mouse_move_delta();
        self._mouse_input_data.clear_mouse_input();
        self._keyboard_input_data.clear_key_pressed();
        self._keyboard_input_data.clear_key_released();
    }

    pub fn update_event(&self) {
        let scene_manager = self._scene_manager_data.borrow();
        self._application.borrow_mut().update_event(&scene_manager);
    }
}

pub fn run_application(
    constants: Constants,
    application: RcRefCell<dyn ApplicationBase>,
    scene_manager: RcRefCell<dyn SceneManagerBase>,
    renderer: RcRefCell<dyn RendererBase>,
) {
    logger::initialize_logger();

    log::info!("run_application");

    let app_name: &str = "RustEngine3D";
    let app_version: u32 = 1;
    let initial_window_size: (u32, u32) = (1024, 768);

    let time_instance = time::Instant::now();
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title(app_name)
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width: initial_window_size.0, height: initial_window_size.1 }))
        .build(&event_loop)
        .unwrap();
    let window_size: (u32, u32) = (window.inner_size().width, window.inner_size().height);

    let mut maybe_resources: Option<RcRefCell<Resources>> = None;
    let mut maybe_ui_manager: Option<RcRefCell<UIManager>> = None;
    let mut maybe_font_manager: Option<RcRefCell<FontManager>> = None;
    let mut maybe_renderer_data: Option<RcRefCell<RendererData>> = None;
    let mut maybe_scene_manager_data: Option<RcRefCell<SceneManagerData>> = None;
    let mut maybe_application_data: Option<RcRefCell<ApplicationData>> = None;

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
                let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
                let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                let mut scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                let mut ui_manager: RefMut<UIManager> = maybe_ui_manager.as_ref().unwrap().borrow_mut();
                application_data.terminate_applicateion(
                    &mut font_manager,
                    &mut ui_manager,
                    &mut scene_manager_data,
                    &mut maybe_resources.as_ref().unwrap().borrow_mut(),
                    &mut renderer_data,
                );
            }

            let elapsed_time = time_instance.elapsed().as_secs_f64();
            let (width, height) = window_size;
            let mouse_pos = (width / 2, height / 2);
            let resources = newRcRefCell(Resources::create_resources());
            let font_manager = newRcRefCell(FontManager::create_font_manager());
            let ui_manager = newRcRefCell(UIManager::create_ui_manager());
            let renderer_data = newRcRefCell(RendererData::create_renderer_data(app_name, app_version, window_size, &window, &resources, &renderer));
            let scene_manager_data = newRcRefCell(SceneManagerData::create_scene_manager_data(&renderer_data, &resources, &scene_manager));
            let keyboard_input_data = input::create_keyboard_input_data();
            let mouse_move_data = input::create_mouse_move_data(mouse_pos);
            let mouse_input_data = input::create_mouse_input_data();

            // initialize grphics
            scene_manager_data.borrow().regist_scene_graphics_data();
            resources.borrow_mut().initialize_resources(&mut renderer_data.borrow_mut());
            font_manager.borrow_mut().initialize_font_manager(&renderer_data.borrow(), &resources.borrow());
            ui_manager.borrow_mut().initialize_ui_manager(&renderer_data.borrow(), &resources.borrow());
            renderer_data.borrow_mut().prepare_framebuffer_and_descriptors();
            scene_manager_data.borrow_mut().initialize_scene_graphics_data();
            scene_manager_data.borrow_mut().open_scene_manager_data();

            let application_data = system::newRcRefCell(
                ApplicationData {
                    _constants: constants.clone(),
                    _window_size: window_size,
                    _time_data: create_time_data(elapsed_time),
                    _camera_move_speed: 1.0,
                    _keyboard_input_data: keyboard_input_data.clone(),
                    _mouse_move_data: mouse_move_data.clone(),
                    _mouse_input_data: mouse_input_data.clone(),
                    _font_manager: font_manager.clone(),
                    _ui_manager: ui_manager.clone(),
                    _scene_manager_data: scene_manager_data.clone(),
                    _renderer_data: renderer_data.clone(),
                    _resources: resources.clone(),
                    _application: application.clone(),
                }
            );

            maybe_resources = Some(resources);
            maybe_font_manager = Some(font_manager);
            maybe_ui_manager = Some(ui_manager);
            maybe_renderer_data = Some(renderer_data);
            maybe_scene_manager_data = Some(scene_manager_data);
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
                    let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    application_data.clear_input_events();
                }
            },
            Event::MainEventsCleared => {
                if run_application {
                    let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                    let mut scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                    let mut font_manager: RefMut<FontManager> = maybe_font_manager.as_ref().unwrap().borrow_mut();
                    let mut ui_manager: RefMut<UIManager> = maybe_ui_manager.as_ref().unwrap().borrow_mut();

                    // exit
                    if application_data._keyboard_input_data.get_key_pressed(VirtualKeyCode::Escape) {
                        *control_flow = ControlFlow::Exit;
                        application_data.terminate_applicateion(
                            &mut font_manager,
                            &mut ui_manager,
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
                    application_data._time_data.update_time_data(&time_instance);
                    let elapsed_time = application_data._time_data._elapsed_time;
                    let delta_time = application_data._time_data._delta_time;
                    let elapsed_frame = application_data._time_data._elapsed_frame;

                    font_manager.log(format!("{:.2}fps / {:.3}ms\n\tTest", application_data._time_data._average_fps, application_data._time_data._average_frame_time));

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
                            ui_manager.destroy_ui_descriptor_sets();
                            font_manager.destroy_font_descriptor_sets();

                            renderer_data.resize_window();

                            // recreate
                            font_manager.create_font_descriptor_sets(&renderer_data, &renderer_data._resources.borrow());
                            ui_manager.create_ui_descriptor_sets(&renderer_data, &renderer_data._resources.borrow());
                            scene_manager_data.initialize_scene_graphics_data();
                            renderer_data.set_need_recreate_swapchain(false);

                            log::info!("<<end recreate_swapchain>>");
                        }
                    } else {
                        // update && render
                        renderer_data.update_post_process_datas();
                        scene_manager_data.update_scene_manager_data(elapsed_time, delta_time);
                        font_manager.update();
                        ui_manager.update(application_data._window_size, delta_time, &renderer_data._resources.borrow());
                        renderer_data.render_scene(scene_manager_data, &mut font_manager, &mut ui_manager, elapsed_time, delta_time, elapsed_frame);
                    }
                }
            },
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                },
                WindowEvent::Resized(size) => {
                    log::info!("WindowEvent::Resized: {:?}, initialize_done: {}", size, initialize_done);
                    if initialize_done {
                        let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
                        let scene_manager_data: RefMut<SceneManagerData> = maybe_scene_manager_data.as_ref().unwrap().borrow_mut();
                        let mut renderer_data: RefMut<RendererData> = maybe_renderer_data.as_ref().unwrap().borrow_mut();
                        application_data._window_size = (size.width, size.height);
                        scene_manager_data.resized_window(size.width, size.height);
                        let swapchain_extent = renderer_data._swapchain_data._swapchain_extent;
                        let need_recreate_swapchain = swapchain_extent.width != size.width || swapchain_extent.height != size.height;
                        log::info!("need_recreate_swapchain: {}, swapchain_extent: {:?}", need_recreate_swapchain, swapchain_extent);
                        if need_recreate_swapchain {
                            renderer_data.set_need_recreate_swapchain(true);
                        }
                    }
                },
                WindowEvent::MouseInput { button, state, .. } => {
                    let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
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
                    let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
                    application_data._mouse_move_data.update_mouse_move(&position.into());
                }
                WindowEvent::MouseWheel { delta: MouseScrollDelta::LineDelta(_, _v_lines), .. } => {
                    // wheel_delta = Some(v_lines);
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if run_application {
                        let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();
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
                    let mut application_data: RefMut<ApplicationData> = maybe_application_data.as_ref().unwrap().borrow_mut();

                    if 0 == id {
                        application_data._mouse_move_data.update_mouse_move(&location.into());

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
