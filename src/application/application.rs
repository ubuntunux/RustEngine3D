use std::time;
use log;

use winit::event::Event;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;

use crate::resource;
use crate::renderer;
use crate::application::scene_manager;
use crate::application::input;

#[derive(Debug, Clone)]
pub struct TimeData
    { _acc_frame_time: f64
    , _acc_frame_count: i32
    , _average_frame_time: f64
    , _average_fps: f64
    , _current_time: f64
    , _elapsed_time: f64
    , _delta_time: f64
    }

pub fn create_time_data(elapsed_time: f64) -> TimeData {
    TimeData {
        _acc_frame_time: 0.0,
        _acc_frame_count: 0,
        _average_frame_time: 0.0,
        _average_fps: 0.0,
        _current_time: elapsed_time,
        _elapsed_time: elapsed_time,
        _delta_time: 0.0
    }
}

impl TimeData {
    pub fn updateTimeData(&mut self, time_instance: &time::Instant) {
        let current_time = time_instance.elapsed().as_secs_f64();
        let previous_time = self._current_time;
        let delta_time = current_time - previous_time;
        let elapsed_time = self._elapsed_time + delta_time;
        let acc_frame_time = self._acc_frame_time + delta_time;
        let acc_frame_count = self._acc_frame_count + 1;
        if 1.0 < acc_frame_time {
            let average_frame_time = acc_frame_time / (acc_frame_count as f64) * 1000.0;
            let average_fps = 1000.0 / average_frame_time;
            log::info!("{:.2}fps / {:.3}ms", average_fps, average_frame_time);
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

pub struct ApplicationData
    { _window: bool
    , _window_size_changed: bool
    , _window_size: (i32, i32)
    , _time_data: TimeData
    , _camera_move_speed: f32
    , _keyboard_input_data: Box<input::KeyboardInputData>
    , _mouse_move_data: Box<input::MouseMoveData>
    , _mouse_input_data: Box<input::MouseInputData>
    , _scene_manager_data: Box<scene_manager::SceneManagerData>
    , _renderer_data: Box<renderer::RendererData>
    , _resources: Box<resource::Resources>
    }

impl ApplicationData {
}


pub fn run_application() {
    log::info!("run_application");

    let time_instance = time::Instant::now();
    let mut elapsed_time = time_instance.elapsed().as_secs_f64();
    let event_loop = EventLoop::new();

    let window_size: (i32, i32) = (1024, 786);
    let mouse_pos = (window_size.0/2, window_size.1/2);
    let mut renderer_data = renderer::create_renderer_data(window_size, &event_loop);
    let mut resources = resource::create_resources();
    let mut scene_manager_data = scene_manager::create_scene_manager_data(renderer_data.clone(), resources.clone());
    let mut keyboard_input_data = input::create_keyboard_input_data();
    let mut mouse_move_data = input::create_mouse_move_data(mouse_pos);
    let mut mouse_input_data = input::create_mouse_input_data();
    let mut application_data = Box::new(ApplicationData {
        _window: false,
        _window_size_changed: false,
        _window_size: window_size,
        _time_data: create_time_data(elapsed_time),
        _camera_move_speed: 1.0,
        _keyboard_input_data: keyboard_input_data.clone(),
        _mouse_move_data: mouse_move_data.clone(),
        _mouse_input_data: mouse_input_data.clone(),
        _scene_manager_data: scene_manager_data.clone(),
        _renderer_data: renderer_data.clone(),
        _resources: resources.clone(),
    });

    // main loop
    let mut render_scene = false;
    event_loop.run(move |event, window_target, control_flow|{
        application_data._time_data.updateTimeData(&time_instance);

        render_scene = false;
        match event {
            Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
                *control_flow = ControlFlow::Exit;
            },
            Event::WindowEvent { event: WindowEvent::Resized(_), .. } => {
                renderer_data.set_need_recreate_swapchain(true);
            },
            Event::RedrawEventsCleared => {
                render_scene = true;
            },
            _ => {},
        }

        if renderer_data.get_need_recreate_swapchain() {
            renderer_data.recreate_swapchain();
            renderer_data.set_need_recreate_swapchain(false);
        }

        if render_scene {
            renderer_data.render_scene();
        }
    });
}
