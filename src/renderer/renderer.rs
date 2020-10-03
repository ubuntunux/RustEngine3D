use std::cmp::{min, max};
use std::borrow::Cow;
use std::cell::RefCell;
use std::default::Default;
use std::ffi::{
    CStr,
    CString,
};
use std::io::Cursor;
use std::mem;
use std::mem::align_of;
use std::ops::Drop;
use std::rc::Rc;
use std::sync::Arc;
use std::vec::Vec;

use ash::{
    vk,
    Device,
    Entry,
    Instance,
};
use ash::extensions::ext::DebugUtils;
use ash::extensions::khr::{
    Surface,
    Swapchain,
};
use ash::version::{
    DeviceV1_0,
    EntryV1_0,
    InstanceV1_0,
};
use ash::util::*;
use winit;
use winit::*;
use winit::dpi;
use winit::window::{
    Window,
    WindowBuilder
};
use winit::event::VirtualKeyCode;
use winit::event::Event;
use winit::event::WindowEvent;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;

use crate::constants;
use crate::resource;
use crate::vulkan_context::{
    command_buffer,
    device,
    queue,
    swapchain,
    sync,
};
use crate::vulkan_context::vulkan_context::*;

#[derive(Clone, Debug, Copy)]
struct Vertex {
    pos: [f32; 4],
    color: [f32; 4],
}


pub struct RendererData {
    _frame_index: i32,
    _swapchain_index: u32,
    _need_recreate_swapchain: bool,
    pub _window: Window,
    pub _entry: Entry,
    pub _instance: Instance,
    pub _device: Device,
    pub _surface: vk::SurfaceKHR,
    pub _surface_interface: Surface,
    pub _swapchain_data: swapchain::SwapchainData,
    pub _swapchain_support_details: swapchain::SwapchainSupportDetails,
    pub _swapchain_interface: Swapchain,
    pub _debug_util_interface: DebugUtils,
    pub _debug_call_back: vk::DebugUtilsMessengerEXT,
    pub _image_available_semaphores: FrameIndexMap<vk::Semaphore>,
    pub _render_finished_semaphores: FrameIndexMap<vk::Semaphore>,
    pub _queue_family_datas: queue::QueueFamilyDatas,
    pub _frame_fences: Vec<vk::Fence>,
    pub _command_pool: vk::CommandPool,
    pub _command_buffers: Vec<vk::CommandBuffer>,
    pub _render_features: RenderFeatures,
    //pub _debug_render_target: RenderTargetType,
    //pub _render_target_data_map: RenderTargetDataMap,
    //pub _uniform_buffer_data_map: UniformBufferDataMap,
    //pub _postprocess_ssao: PostProcessData,
    pub _resources: Rc<RefCell<resource::Resources>>
}


pub fn create_renderer_data<T>(
    app_name: &str,
    app_version: u32,
    (window_width, window_height): (u32, u32),
    event_loop: &EventLoop<T>,
    resources: Rc<RefCell<resource::Resources>>
) -> Rc<RefCell<RendererData>> {
    unsafe {
        log::info!("create_renderer_data: {}, width: {}, height: {}", constants::ENGINE_NAME, window_width, window_height);
        let window = WindowBuilder::new()
            .with_title(app_name)
            .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize {width: window_width, height: window_height}))
            .build(&event_loop)
            .unwrap();
        let entry = Entry::new().unwrap();
        let surface_extensions = ash_window::enumerate_required_extensions(&window).unwrap();
        let instance: Instance = device::create_vk_instance(&entry, &app_name, app_version, &surface_extensions);
        let surface = device::create_vk_surface(&entry, &instance, &window);
        let surface_interface = Surface::new(&entry, &instance);
        let (physical_device, swapchain_support_details, physical_device_features) = device::select_physical_device(&instance, &surface_interface, surface).unwrap();
        let deviceProperties = instance.get_physical_device_properties(physical_device);
        let msaa_samples = device::get_max_usable_sample_count(&deviceProperties);
        let queue_family_indices = queue::get_queue_family_indices(
            &instance,
            &surface_interface,
            surface,
            physical_device,
            constants::IS_CONCURRENT_MODE
        );
        let render_features = RenderFeatures {
            _physical_device_features: physical_device_features.clone(),
            _msaa_samples: msaa_samples,
        };
        let graphics_queue_index = queue_family_indices._graphics_queue_index;
        let present_queue_index = queue_family_indices._present_queue_index;
        let queue_family_index_set: Vec<u32> = if graphics_queue_index == present_queue_index {
            vec![graphics_queue_index]
        } else {
            vec![graphics_queue_index, present_queue_index]
        };
        let device = device::create_device(&instance, physical_device, &render_features, &queue_family_index_set);
        let queue_map = queue::create_queues(&device, &queue_family_index_set);
        let default_queue: &vk::Queue = queue_map.get(&queue_family_index_set[0]).unwrap();
        let queue_family_datas = queue::QueueFamilyDatas {
            _graphics_queue: queue_map.get(&graphics_queue_index).unwrap_or(default_queue).clone(),
            _present_queue: queue_map.get(&present_queue_index).unwrap_or(default_queue).clone(),
            _queue_family_index_list: queue_family_index_set.clone(),
            _queue_family_count: queue_map.len() as u32,
            _queue_family_indices: queue_family_indices.clone()
        };
        let swapchain_interface = Swapchain::new(&instance, &device);
        let swapchain_data: swapchain::SwapchainData = swapchain::create_swapchain_data(
            &device,
            &swapchain_interface,
            surface,
            &swapchain_support_details,
            &queue_family_datas,
            constants::ENABLE_IMMEDIATE_MODE
        );
        let image_available_semaphores = sync::create_semaphores(&device);
        let render_finished_semaphores = sync::create_semaphores(&device);
        let frame_fences = sync::create_fences(&device);
        let command_pool = command_buffer::create_command_pool(&device, &queue_family_datas);
        let command_buffers = command_buffer::create_command_buffers(&device, command_pool, constants::SWAPCHAIN_IMAGE_COUNT);

        // debug utils
        let debug_message_level = get_debug_message_level(vk::DebugUtilsMessageSeverityFlagsEXT::WARNING);
        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(vk::DebugUtilsMessageSeverityFlagsEXT::WARNING)
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(vulkan_debug_callback));
        let debug_util_interface = DebugUtils::new(&entry, &instance);
        let debug_call_back = debug_util_interface.create_debug_utils_messenger(&debug_info, None).unwrap();

        Rc::new(RefCell::new(RendererData {
            _frame_index: 0,
            _swapchain_index: 0,
            _need_recreate_swapchain: false,
            _window: window,
            _entry: entry,
            _instance: instance,
            _device: device,
            _surface: surface,
            _surface_interface: surface_interface,
            _swapchain_data: swapchain_data,
            _swapchain_support_details: swapchain_support_details,
            _swapchain_interface: swapchain_interface,
            _debug_util_interface: debug_util_interface,
            _debug_call_back: debug_call_back,
            _image_available_semaphores: image_available_semaphores,
            _render_finished_semaphores: render_finished_semaphores,
            _queue_family_datas: queue_family_datas,
            _frame_fences: frame_fences,
            _command_pool: command_pool,
            _command_buffers: command_buffers,
            _render_features: render_features,
            //pub _debug_render_target: RenderTargetType,
            //pub _render_target_data_map: RenderTargetDataMap,
            //pub _uniform_buffer_data_map: UniformBufferDataMap,
            //pub _postprocess_ssao: PostProcessData,
            _resources: resources.clone()
        }))
    }
}

impl RendererData {
    pub fn get_need_recreate_swapchain(&self) -> bool {
        self._need_recreate_swapchain
    }

    pub fn set_need_recreate_swapchain(&mut self, value: bool) {
        self._need_recreate_swapchain = value;
    }

    pub fn recreate_swapchain(&self) {
        log::info!("recreate_swapchain");
    }

    pub fn render_scene(&self) {

    }

    pub unsafe fn destroy_renderer_data(&self) {
        //destroyUniformBufferDatas _device _uniformBufferDataMap
        //destroyRenderTargets rendererData _renderTargetDataMap
        sync::destroy_semaphores(&self._device, &self._image_available_semaphores);
        sync::destroy_semaphores(&self._device, &self._render_finished_semaphores);
        sync::destroy_fences(&self._device, &self._frame_fences);
        command_buffer::destroy_command_buffers(&self._device, self._command_pool, &self._command_buffers);
        command_buffer::destroy_command_pool(&self._device, self._command_pool);
        swapchain::destroy_swapchain_data(&self._device, &self._swapchain_interface, &self._swapchain_data);
        device::destroy_device(&self._device);
        device::destroy_vk_surface(&self._surface_interface, self._surface);
        self._debug_util_interface.destroy_debug_utils_messenger(self._debug_call_back, None);
        device::destroy_vk_instance(&self._instance);
    }
}