use std::sync::Arc;
use std::borrow::Cow;

use vulkano;
use vulkano::device::{
    Device,
    DeviceExtensions
};
use vulkano::image::ImageUsage;
use vulkano::instance::{
    Instance,
    PhysicalDevice
};
use vulkano::swapchain;
use vulkano::swapchain::{
    AcquireError,
    ColorSpace,
    FullscreenExclusive,
    PresentMode,
    SurfaceTransform,
    Swapchain,
    SwapchainCreationError,
};
use vulkano::sync;
use vulkano::sync::{
    FlushError,
    GpuFuture
};
use vulkano_win::VkSurfaceBuild;
use winit::event::{
    Event,
    WindowEvent
};
use winit::event_loop::{
    ControlFlow,
    EventLoop,
    EventLoopWindowTarget
};
use winit::window::WindowBuilder;
use cgmath::Matrix4;
use cgmath::SquareMatrix;
use cgmath::Vector3;

use crate::frame;
use crate::constants;
use crate::resource;
use crate::renderer;
use std::ops::Deref;

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

#[derive(Debug, Clone)]
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
    , _renderer_data: bool // Renderer.RendererData
    , _resources: Box<resource::Resources> // Resource.Resources
    }

impl Default for ApplicationData {
    fn default() -> ApplicationData {
        ApplicationData {
            _window: false,
            _window_size_changed: false,
            _window_size: (1024, 768),
            _time_data: TimeData::default(),
            _camera_move_speed: 1.0,
            _keyboard_input_data: false,
            _mouse_move_data: false,
            _mouse_input_data: false,
            _scene_manager_data: false,
            _renderer_data: false,
            _resources: resource::Resources::new()
        }
    }
}

impl ApplicationData {
    pub fn new() -> Box<ApplicationData> {
        let app = ApplicationData::default();
        Box::new(app)
    }

    pub fn initialize_application(mut self) {
        let event_loop = EventLoop::new();
        let mut renderer_data = renderer::create_renderer_data(&event_loop);

        // Here is the basic initialization for the deferred system.
        let mut frame_system = frame::FrameSystem::new(renderer_data._queue.clone(), renderer_data._swapchain.format());
        let triangle_draw_system = frame::TriangleDrawSystem::new(renderer_data._queue.clone(), frame_system.deferred_subpass());
        let mut recreate_swapchain = false;
        let mut previous_frame_end = Some(sync::now(renderer_data._device.clone()).boxed());
        let mut swapchain = renderer_data._swapchain.clone();
        let mut images = renderer_data._images.clone();

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
                recreate_swapchain = true;
            }
            Event::RedrawEventsCleared => {
                previous_frame_end.as_mut().unwrap().cleanup_finished();

                if recreate_swapchain {
                    let dimensions: [u32; 2] = renderer_data._surface.window().inner_size().into();
                    let (new_swapchain, new_images) =
                        match swapchain.recreate_with_dimensions(dimensions) {
                            Ok(r) => r,
                            Err(SwapchainCreationError::UnsupportedDimensions) => return,
                            Err(e) => panic!("Failed to recreate swapchain: {:?}", e),
                        };
                    swapchain = new_swapchain;
                    images = new_images;
                    recreate_swapchain = false;
                }

                let (image_num, suboptimal, acquire_future) =
                    match swapchain::acquire_next_image(swapchain.clone(), None) {
                        Ok(r) => r,
                        Err(AcquireError::OutOfDate) => {
                            recreate_swapchain = true;
                            return;
                        }
                        Err(e) => panic!("Failed to acquire next image: {:?}", e),
                    };

                if suboptimal {
                    recreate_swapchain = true;
                }

                let future = previous_frame_end.take().unwrap().join(acquire_future);
                let mut frame =
                    frame_system.frame(future, images[image_num].clone(), Matrix4::identity());
                let mut after_future = None;
                while let Some(pass) = frame.next_pass() {
                    match pass {
                        frame::Pass::Deferred(mut draw_pass) => {
                            let cb = triangle_draw_system.draw(draw_pass.viewport_dimensions());
                            draw_pass.execute(cb);
                        }
                        frame::Pass::Lighting(mut lighting) => {
                            lighting.ambient_light([0.1, 0.1, 0.1]);
                            lighting.directional_light(Vector3::new(0.2, -0.1, -0.7), [0.6, 0.6, 0.6]);
                            lighting.point_light(Vector3::new(0.5, -0.5, -0.1), [1.0, 0.0, 0.0]);
                            lighting.point_light(Vector3::new(-0.9, 0.2, -0.15), [0.0, 1.0, 0.0]);
                            lighting.point_light(Vector3::new(0.0, 0.5, -0.05), [0.0, 0.0, 1.0]);
                        }
                        frame::Pass::Finished(af) => {
                            after_future = Some(af);
                        }
                    }
                }

                let future = after_future
                    .unwrap()
                    .then_swapchain_present(renderer_data._queue.clone(), swapchain.clone(), image_num)
                    .then_signal_fence_and_flush();

                match future {
                    Ok(future) => {
                        previous_frame_end = Some(future.boxed());
                    }
                    Err(FlushError::OutOfDate) => {
                        recreate_swapchain = true;
                        previous_frame_end = Some(sync::now(renderer_data._device.clone()).boxed());
                    }
                    Err(e) => {
                        println!("Failed to flush future: {:?}", e);
                        previous_frame_end = Some(sync::now(renderer_data._device.clone()).boxed());
                    }
                }
            }
            _ => (),
        });
    }
}


pub fn run_application() {
    let mut app = ApplicationData::new();
    app.initialize_application();
}