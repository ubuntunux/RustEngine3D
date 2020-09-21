use winit::event::Event;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;
//use winit::event_loop::EventLoopWindowTarget;
use vulkano::sync;
use vulkano::sync::GpuFuture;

use crate::resource;
use crate::renderer;

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

impl Default for TimeData {
    fn default() -> TimeData {
        TimeData
            { _acc_frame_time: 0.0
            , _acc_frame_count: 0
            , _average_frame_time: 0.0
            , _average_fps: 0.0
            , _current_time: 0.0
            , _elapsed_time: 0.0
            , _delta_time: 0.0
            }
    }
}

pub struct ApplicationData
    { _window: bool
    , _window_size_changed: bool
    , _window_size: (i32, i32)
    , _time_data: TimeData
    , _camera_move_speed: f32
    , _keyboard_input_data: bool // KeyboardInputData
    , _mouse_move_data: bool // MouseMoveData
    , _mouse_input_data: bool // MouseInputData
    , _scene_manager_data: bool // SceneManager.SceneManagerData
    , _renderer_data: Box<renderer::RendererData>
    , _resources: Box<resource::Resources> // Resource.Resources
    }

impl ApplicationData {
}


pub fn run_application() {
    let event_loop = EventLoop::new();
    let mut renderer_data = renderer::create_renderer_data(&event_loop);
    //let mut previous_frame_end = Some(sync::now(renderer_data._device.clone()).boxed());
    let mut resources = resource::Resources::new();
    let mut application_data = Box::new(ApplicationData {
        _window: false,
        _window_size_changed: false,
        _window_size: (1024, 768),
        _time_data: TimeData::default(),
        _camera_move_speed: 1.0,
        _keyboard_input_data: false,
        _mouse_move_data: false,
        _mouse_input_data: false,
        _scene_manager_data: false,
        _renderer_data: renderer_data.clone(),
        _resources: resources.clone(),
    });

    let mut previous_frame_end = Some(sync::now(renderer_data._device.clone()).boxed());

    // main loop
    event_loop.run(move |event, _, control_flow| match event {
        Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } => {
            *control_flow = ControlFlow::Exit;
        }
        Event::WindowEvent {
            event: WindowEvent::Resized(_),
            ..
        } => {
            renderer_data.set_need_recreate_swapchain(true);
        }
        Event::RedrawEventsCleared => {
            if renderer_data.get_need_recreate_swapchain() {
                renderer_data.recreate_swapchain();
                renderer_data.set_need_recreate_swapchain(false);
            }
            renderer_data.render_scene(&mut previous_frame_end);
        }
        _ => (),
    });
}