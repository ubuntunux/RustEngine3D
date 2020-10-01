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

use ash;
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
use crate::vulkan_context::vulkan_context::*;

#[derive(Clone, Debug, Copy)]
struct Vertex {
    pos: [f32; 4],
    color: [f32; 4],
}

pub struct RendererData {
    pub entry: Entry,
    pub instance: Instance,
    pub device: Device,
    pub surface_loader: Surface,
    pub swapchain_loader: Swapchain,
    pub debug_utils_loader: DebugUtils,
    pub window: Window,
    pub debug_call_back: vk::DebugUtilsMessengerEXT,

    pub pdevice: vk::PhysicalDevice,
    pub device_memory_properties: vk::PhysicalDeviceMemoryProperties,
    pub queue_family_index: u32,
    pub present_queue: vk::Queue,

    pub surface: vk::SurfaceKHR,
    pub surface_format: vk::SurfaceFormatKHR,
    pub surface_resolution: vk::Extent2D,

    pub swapchain: vk::SwapchainKHR,
    pub present_images: Vec<vk::Image>,
    pub present_image_views: Vec<vk::ImageView>,

    pub pool: vk::CommandPool,
    pub draw_command_buffer: vk::CommandBuffer,
    pub setup_command_buffer: vk::CommandBuffer,

    pub depth_image: vk::Image,
    pub depth_image_view: vk::ImageView,
    pub depth_image_memory: vk::DeviceMemory,

    pub present_complete_semaphore: vk::Semaphore,
    pub rendering_complete_semaphore: vk::Semaphore,

    pub _frame_index: i32,
    pub _swapchain_index: u32,
    // _vertex_offset: vk::DeviceSize,
    pub _need_recreate_swapchain: bool,
    // _image_available_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
    // _render_finished_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
    // _vk_instance: vk::Instance,
    //pub _surface: Arc<vulkano::swapchain::Surface<Window>>,
    //pub _device: Arc<Device>,
    // _physical_device: vk::PhysicalDevice,
    //pub _swapchain: Arc<vulkano::swapchain::Swapchain<Window>>,
    //pub _images: Vec<Arc<vulkano::image::swapchain::SwapchainImage<Window>>>,
    // _swapchain_data: SwapChainData,
    // _swapchain_support_details: SwapChainSupportDetails,
    //pub _queue: Arc<Queue>,
    // _queue_family_datas: QueueFamilyDatas,
    // _frame_fences: vk::Fence,
    // _command_pool: vk::CommandPool,
    pub _command_buffer_count: i32,
    // _command_buffers: vk::CommandBuffer,
    // _render_features: vk::RenderFeatures,
    // _image_samplers: vk::ImageSamplers,
    // _debug_render_target: RenderTargetType,
    // _render_target_data_map: RenderTargetDataMap,
    // _uniform_buffer_data_map: UniformBufferDataMap,
    // _postprocess_ssao: PostProcessData,
    // _resources: Box<resource::Resources>
}


pub fn create_renderer_data<T> (app_name: &str, app_version: u32, (window_width, window_height): (u32, u32), event_loop: &EventLoop<T>) -> Rc<RefCell<RendererData>> {
    unsafe {
        log::info!("create_renderer_data: {}, width: {}, height: {}", constants::ENGINE_NAME, window_width, window_height);
        let window = WindowBuilder::new()
            .with_title(constants::ENGINE_NAME)
            .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize {width: window_width, height: window_height}))
            .build(&event_loop)
            .unwrap();
        let entry = Entry::new().unwrap();
        let app_name = CString::new(app_name).unwrap();
        let layer_names: Vec<CString> = constants::VULKAN_LAYERS
            .iter()
            .map(|layer_name| CString::new(*layer_name).unwrap())
            .collect();
        let layers_names_raw: Vec<*const i8> = layer_names
            .iter()
            .map(|raw_name| raw_name.as_ptr())
            .collect();
        let surface_extensions = ash_window::enumerate_required_extensions(&window).unwrap();
        let mut extension_names_raw = surface_extensions
            .iter()
            .map(|ext| ext.as_ptr())
            .collect::<Vec<_>>();
        extension_names_raw.push(DebugUtils::name().as_ptr());

        let appinfo = vk::ApplicationInfo::builder()
            .application_name(&app_name)
            .application_version(app_version)
            .engine_name(&app_name)
            .engine_version(constants::ENGINE_VERSION)
            .api_version(constants::API_VERSION);

        let create_info = vk::InstanceCreateInfo::builder()
            .application_info(&appinfo)
            .enabled_layer_names(&layers_names_raw)
            .enabled_extension_names(&extension_names_raw);

        let instance: Instance = entry
            .create_instance(&create_info, None)
            .expect("Instance creation error");

        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
                | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                //| vk::DebugUtilsMessageSeverityFlagsEXT::INFO,
            )
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(vulkan_debug_callback));

        let debug_utils_loader = DebugUtils::new(&entry, &instance);
        let debug_call_back = debug_utils_loader
            .create_debug_utils_messenger(&debug_info, None)
            .unwrap();
        let surface = ash_window::create_surface(&entry, &instance, &window, None).unwrap();
        let pdevices = instance
            .enumerate_physical_devices()
            .expect("Physical device error");
        let surface_loader = Surface::new(&entry, &instance);
        let (pdevice, queue_family_index) = pdevices
            .iter()
            .map(|pdevice| {
                instance
                    .get_physical_device_queue_family_properties(*pdevice)
                    .iter()
                    .enumerate()
                    .filter_map(|(index, ref info)| {
                        let supports_graphic_and_surface =
                            info.queue_flags.contains(vk::QueueFlags::GRAPHICS)
                                && surface_loader
                                .get_physical_device_surface_support(
                                    *pdevice,
                                    index as u32,
                                    surface,
                                )
                                .unwrap();
                        if supports_graphic_and_surface {
                            Some((*pdevice, index))
                        } else {
                            None
                        }
                    })
                    .next()
            })
            .filter_map(|v| v)
            .next()
            .expect("Couldn't find suitable device.");

        let queue_family_index = queue_family_index as u32;
        let priorities = [1.0];
        let queue_info = [vk::DeviceQueueCreateInfo::builder()
            .queue_family_index(queue_family_index)
            .queue_priorities(&priorities)
            .build()];

        let features = vk::PhysicalDeviceFeatures {
            shader_clip_distance: 1,
            ..Default::default()
        };
        let device_extension_names_raw = [Swapchain::name().as_ptr()];
        let device_create_info = vk::DeviceCreateInfo::builder()
            .queue_create_infos(&queue_info)
            .enabled_extension_names(&device_extension_names_raw)
            .enabled_features(&features);
        let device: Device = instance.create_device(pdevice, &device_create_info, None).unwrap();

        let surface_formats = surface_loader.get_physical_device_surface_formats(pdevice, surface).unwrap();
        println!("{:?}", surface_formats);
        let require_format = vk::SurfaceFormatKHR {
            format: vk::Format::B8G8R8_UNORM,
            color_space: sfmt.color_space,
        };
        let surface_format = surface_formats
            .iter()
            .map(|sfmt| match sfmt.format {
                vk::Format::UNDEFINED => vk::SurfaceFormatKHR {
                    format: vk::Format::B8G8R8_UNORM,
                    color_space: sfmt.color_space,
                },
                _ => *sfmt,
            })
            .next()
            .expect("Unable to find suitable surface format.");
        let surface_capabilities = surface_loader
            .get_physical_device_surface_capabilities(pdevice, surface)
            .unwrap();
        let mut desired_image_count = surface_capabilities.min_image_count + 1;
        if surface_capabilities.max_image_count > 0
            && desired_image_count > surface_capabilities.max_image_count
        {
            desired_image_count = surface_capabilities.max_image_count;
        }
        let surface_resolution = match surface_capabilities.current_extent.width {
            std::u32::MAX => vk::Extent2D {
                width: window_width,
                height: window_height,
            },
            _ => surface_capabilities.current_extent,
        };
        let pre_transform = if surface_capabilities
            .supported_transforms
            .contains(vk::SurfaceTransformFlagsKHR::IDENTITY)
        {
            vk::SurfaceTransformFlagsKHR::IDENTITY
        } else {
            surface_capabilities.current_transform
        };
        let present_modes = surface_loader.get_physical_device_surface_present_modes(pdevice, surface).unwrap();
        let present_mode = present_modes
            .iter()
            .cloned()
            .find(|&mode| mode == vk::PresentModeKHR::MAILBOX)
            .unwrap_or(vk::PresentModeKHR::FIFO);
        let swapchain_loader = Swapchain::new(&instance, &device);

        let swapchain_create_info = vk::SwapchainCreateInfoKHR::builder()
            .surface(surface)
            .min_image_count(desired_image_count)
            .image_color_space(surface_format.color_space)
            .image_format(surface_format.format)
            .image_extent(surface_resolution)
            .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
            .image_sharing_mode(vk::SharingMode::EXCLUSIVE)
            .pre_transform(pre_transform)
            .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
            .present_mode(present_mode)
            .clipped(true)
            .image_array_layers(1);

        let swapchain = swapchain_loader
            .create_swapchain(&swapchain_create_info, None)
            .unwrap();

        let pool_create_info = vk::CommandPoolCreateInfo::builder()
            .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
            .queue_family_index(queue_family_index);

        let pool = device.create_command_pool(&pool_create_info, None).unwrap();

        let command_buffer_allocate_info = vk::CommandBufferAllocateInfo::builder()
            .command_buffer_count(2)
            .command_pool(pool)
            .level(vk::CommandBufferLevel::PRIMARY);

        let command_buffers = device
            .allocate_command_buffers(&command_buffer_allocate_info)
            .unwrap();
        let setup_command_buffer = command_buffers[0];
        let draw_command_buffer = command_buffers[1];

        let present_images = swapchain_loader.get_swapchain_images(swapchain).unwrap();
        let present_image_views: Vec<vk::ImageView> = present_images
            .iter()
            .map(|&image| {
                let create_view_info = vk::ImageViewCreateInfo::builder()
                    .view_type(vk::ImageViewType::TYPE_2D)
                    .format(surface_format.format)
                    .components(vk::ComponentMapping {
                        r: vk::ComponentSwizzle::R,
                        g: vk::ComponentSwizzle::G,
                        b: vk::ComponentSwizzle::B,
                        a: vk::ComponentSwizzle::A,
                    })
                    .subresource_range(vk::ImageSubresourceRange {
                        aspect_mask: vk::ImageAspectFlags::COLOR,
                        base_mip_level: 0,
                        level_count: 1,
                        base_array_layer: 0,
                        layer_count: 1,
                    })
                    .image(image);
                device.create_image_view(&create_view_info, None).unwrap()
            }).collect();
        let device_memory_properties = instance.get_physical_device_memory_properties(pdevice);
        let depth_image_create_info = vk::ImageCreateInfo::builder()
            .image_type(vk::ImageType::TYPE_2D)
            .format(vk::Format::D16_UNORM)
            .extent(vk::Extent3D {
                width: surface_resolution.width,
                height: surface_resolution.height,
                depth: 1,
            })
            .mip_levels(1)
            .array_layers(1)
            .samples(vk::SampleCountFlags::TYPE_1)
            .tiling(vk::ImageTiling::OPTIMAL)
            .usage(vk::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT)
            .sharing_mode(vk::SharingMode::EXCLUSIVE);

        let depth_image = device.create_image(&depth_image_create_info, None).unwrap();
        let depth_image_memory_req = device.get_image_memory_requirements(depth_image);
        let depth_image_memory_index = find_memorytype_index(
            &depth_image_memory_req,
            &device_memory_properties,
            vk::MemoryPropertyFlags::DEVICE_LOCAL,
        ).expect("Unable to find suitable memory index for depth image.");

        let depth_image_allocate_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(depth_image_memory_req.size)
            .memory_type_index(depth_image_memory_index);

        let depth_image_memory = device.allocate_memory(&depth_image_allocate_info, None).unwrap();

        device.bind_image_memory(depth_image, depth_image_memory, 0)
            .expect("Unable to bind depth image memory");

        let present_queue = device.get_device_queue(queue_family_index as u32, 0);
        record_submit_commandbuffer(
            &device,
            setup_command_buffer,
            present_queue,
            &[],
            &[],
            &[],
            |device, setup_command_buffer| {
                let layout_transition_barriers = vk::ImageMemoryBarrier::builder()
                    .image(depth_image)
                    .dst_access_mask(
                        vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ
                            | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
                    )
                    .new_layout(vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
                    .old_layout(vk::ImageLayout::UNDEFINED)
                    .subresource_range(
                        vk::ImageSubresourceRange::builder()
                            .aspect_mask(vk::ImageAspectFlags::DEPTH)
                            .layer_count(1)
                            .level_count(1)
                            .build(),
                    );

                device.cmd_pipeline_barrier(
                    setup_command_buffer,
                    vk::PipelineStageFlags::BOTTOM_OF_PIPE,
                    vk::PipelineStageFlags::LATE_FRAGMENT_TESTS,
                    vk::DependencyFlags::empty(),
                    &[],
                    &[],
                    &[layout_transition_barriers.build()],
                );
            },
        );

        let depth_image_view_info = vk::ImageViewCreateInfo::builder()
            .subresource_range(
                vk::ImageSubresourceRange::builder()
                    .aspect_mask(vk::ImageAspectFlags::DEPTH)
                    .level_count(1)
                    .layer_count(1)
                    .build(),
            )
            .image(depth_image)
            .format(depth_image_create_info.format)
            .view_type(vk::ImageViewType::TYPE_2D);

        let depth_image_view = device
            .create_image_view(&depth_image_view_info, None)
            .unwrap();

        let semaphore_create_info = vk::SemaphoreCreateInfo::default();

        let present_complete_semaphore = device
            .create_semaphore(&semaphore_create_info, None)
            .unwrap();
        let rendering_complete_semaphore = device
            .create_semaphore(&semaphore_create_info, None)
            .unwrap();

        log::info!("    swapchain: {:?}", Swapchain::name());
        log::info!("    api version: {}.{}.{}", vk::version_major(constants::API_VERSION), vk::version_minor(constants::API_VERSION), vk::version_patch(constants::API_VERSION));
        log::info!("    surface_extensions: {:?}", surface_extensions);

        Rc::new(RefCell::new(RendererData {
            entry,
            instance,
            device,
            queue_family_index,
            pdevice,
            device_memory_properties,
            window,
            surface_loader,
            surface_format,
            present_queue,
            surface_resolution,
            swapchain_loader,
            swapchain,
            present_images,
            present_image_views,
            pool,
            draw_command_buffer,
            setup_command_buffer,
            depth_image,
            depth_image_view,
            present_complete_semaphore,
            rendering_complete_semaphore,
            surface,
            debug_call_back,
            debug_utils_loader,
            depth_image_memory,
            _frame_index: 0,
            _swapchain_index: 0,
            //_vertex_offset: vk::DeviceSize,
            _need_recreate_swapchain: false,
            //_image_available_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
            // _render_finished_semaphores: [vk::Semaphore; MAX_FRAME_COUNT as usize],
            // _vk_instance: vk::Instance,
            // _surface: surface,
            // _device: device,
            // _physical_device: vk::PhysicalDevice,
            // _images: images,
            // _swapchain: swapchain,
            // _swapchain_data: SwapChainData,
            // _swapchain_support_details: SwapChainSupportDetails,
            // _queue: queue,
            // _queue_family_datas: QueueFamilyDatas,
            // _frame_fences: vk::Fence,
            // _command_pool: vk::CommandPool,
            _command_buffer_count: 0,
            // _command_buffers: vk::CommandBuffer,
            // _render_features: vk::RenderFeatures,
            // _image_samplers: vk::ImageSamplers,
            // _debug_render_target: RenderTargetType,
            // _render_target_data_map: RenderTargetDataMap,
            // _uniform_buffer_data_map: UniformBufferDataMap,
            // _postprocess_ssao: PostProcessData,
            // _resources: Box<resource::Resources>
        }))
    }
}

impl RendererData {
    // pub fn resize_window(&self) {
    //     // resizeWindow :: GLFW.Window -> RendererData -> IO ()
    //     // resizeWindow window rendererData@RendererData {..} = do
    //     // logInfo "<< resizeWindow >>"
    //     //
    //     // deviceWaitIdle rendererData
    //     //
    //     // // destroy swapchain & graphics resources
    //     // unloadGraphicsDatas _resources rendererData
    //     //
    //     // destroyRenderTargets rendererData _renderTargetDataMap
    //     //
    //     // // recreate swapchain & graphics resources
    //     // recreateSwapChain rendererData window
    //     // renderTargets <- createRenderTargets rendererData _renderTargetDataMap
    //     // loadGraphicsDatas _resources rendererData
    // }
    //
    // pub fn recreate_swapchain(&mut self) {
    // }
    //
    // pub fn get_need_recreate_swapchain(&self) -> bool {
    //     self._need_recreate_swapchain
    // }
    //
    // pub fn set_need_recreate_swapchain(&mut self, value: bool) {
    //     self._need_recreate_swapchain = value;
    // }
    //
    // pub fn render_scene(&mut self) {
    // }
//     pub fn initialize_renderer(&self) {
//         unsafe {
//             let renderpass_attachments = [
//                 vk::AttachmentDescription {
//                     format: self.surface_format.format,
//                     samples: vk::SampleCountFlags::TYPE_1,
//                     load_op: vk::AttachmentLoadOp::CLEAR,
//                     store_op: vk::AttachmentStoreOp::STORE,
//                     final_layout: vk::ImageLayout::PRESENT_SRC_KHR,
//                     ..Default::default()
//                 },
//                 vk::AttachmentDescription {
//                     format: vk::Format::D16_UNORM,
//                     samples: vk::SampleCountFlags::TYPE_1,
//                     load_op: vk::AttachmentLoadOp::CLEAR,
//                     initial_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
//                     final_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
//                     ..Default::default()
//                 },
//             ];
//             let color_attachment_refs = [vk::AttachmentReference {
//                 attachment: 0,
//                 layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
//             }];
//             let depth_attachment_ref = vk::AttachmentReference {
//                 attachment: 1,
//                 layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
//             };
//             let dependencies = [vk::SubpassDependency {
//                 src_subpass: vk::SUBPASS_EXTERNAL,
//                 src_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
//                 dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ
//                     | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
//                 dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
//                 ..Default::default()
//             }];
//
//             let subpasses = [vk::SubpassDescription::builder()
//                 .color_attachments(&color_attachment_refs)
//                 .depth_stencil_attachment(&depth_attachment_ref)
//                 .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
//                 .build()];
//
//             let renderpass_create_info = vk::RenderPassCreateInfo::builder()
//                 .attachments(&renderpass_attachments)
//                 .subpasses(&subpasses)
//                 .dependencies(&dependencies);
//
//             let renderpass = self
//                 .device
//                 .create_render_pass(&renderpass_create_info, None)
//                 .unwrap();
//
//             let framebuffers: Vec<vk::Framebuffer> = self
//                 .present_image_views
//                 .iter()
//                 .map(|&present_image_view| {
//                     let framebuffer_attachments = [present_image_view, self.depth_image_view];
//                     let frame_buffer_create_info = vk::FramebufferCreateInfo::builder()
//                         .render_pass(renderpass)
//                         .attachments(&framebuffer_attachments)
//                         .width(self.surface_resolution.width)
//                         .height(self.surface_resolution.height)
//                         .layers(1);
//
//                     self.device
//                         .create_framebuffer(&frame_buffer_create_info, None)
//                         .unwrap()
//                 })
//                 .collect();
//
//             let index_buffer_data = [0u32, 1, 2];
//             let index_buffer_info = vk::BufferCreateInfo::builder()
//                 .size(std::mem::size_of_val(&index_buffer_data) as u64)
//                 .usage(vk::BufferUsageFlags::INDEX_BUFFER)
//                 .sharing_mode(vk::SharingMode::EXCLUSIVE);
//
//             let index_buffer = self.device.create_buffer(&index_buffer_info, None).unwrap();
//             let index_buffer_memory_req = self.device.get_buffer_memory_requirements(index_buffer);
//             let index_buffer_memory_index = find_memorytype_index(
//                 &index_buffer_memory_req,
//                 &self.device_memory_properties,
//                 vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
//             )
//                 .expect("Unable to find suitable memorytype for the index buffer.");
//
//             let index_allocate_info = vk::MemoryAllocateInfo {
//                 allocation_size: index_buffer_memory_req.size,
//                 memory_type_index: index_buffer_memory_index,
//                 ..Default::default()
//             };
//             let index_buffer_memory = self
//                 .device
//                 .allocate_memory(&index_allocate_info, None)
//                 .unwrap();
//             let index_ptr = self
//                 .device
//                 .map_memory(
//                     index_buffer_memory,
//                     0,
//                     index_buffer_memory_req.size,
//                     vk::MemoryMapFlags::empty(),
//                 )
//                 .unwrap();
//             let mut index_slice = Align::new(
//                 index_ptr,
//                 align_of::<u32>() as u64,
//                 index_buffer_memory_req.size,
//             );
//             index_slice.copy_from_slice(&index_buffer_data);
//             self.device.unmap_memory(index_buffer_memory);
//             self.device
//                 .bind_buffer_memory(index_buffer, index_buffer_memory, 0)
//                 .unwrap();
//
//             let vertex_input_buffer_info = vk::BufferCreateInfo {
//                 size: 3 * std::mem::size_of::<Vertex>() as u64,
//                 usage: vk::BufferUsageFlags::VERTEX_BUFFER,
//                 sharing_mode: vk::SharingMode::EXCLUSIVE,
//                 ..Default::default()
//             };
//
//             let vertex_input_buffer = self
//                 .device
//                 .create_buffer(&vertex_input_buffer_info, None)
//                 .unwrap();
//
//             let vertex_input_buffer_memory_req = self
//                 .device
//                 .get_buffer_memory_requirements(vertex_input_buffer);
//
//             let vertex_input_buffer_memory_index = find_memorytype_index(
//                 &vertex_input_buffer_memory_req,
//                 &self.device_memory_properties,
//                 vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
//             )
//                 .expect("Unable to find suitable memorytype for the vertex buffer.");
//
//             let vertex_buffer_allocate_info = vk::MemoryAllocateInfo {
//                 allocation_size: vertex_input_buffer_memory_req.size,
//                 memory_type_index: vertex_input_buffer_memory_index,
//                 ..Default::default()
//             };
//
//             let vertex_input_buffer_memory = self
//                 .device
//                 .allocate_memory(&vertex_buffer_allocate_info, None)
//                 .unwrap();
//
//             let vertices = [
//                 Vertex {
//                     pos: [-1.0, 1.0, 0.0, 1.0],
//                     color: [0.0, 1.0, 0.0, 1.0],
//                 },
//                 Vertex {
//                     pos: [1.0, 1.0, 0.0, 1.0],
//                     color: [0.0, 0.0, 1.0, 1.0],
//                 },
//                 Vertex {
//                     pos: [0.0, -1.0, 0.0, 1.0],
//                     color: [1.0, 0.0, 0.0, 1.0],
//                 },
//             ];
//
//             let vert_ptr = self
//                 .device
//                 .map_memory(
//                     vertex_input_buffer_memory,
//                     0,
//                     vertex_input_buffer_memory_req.size,
//                     vk::MemoryMapFlags::empty(),
//                 )
//                 .unwrap();
//
//             let mut vert_align = Align::new(
//                 vert_ptr,
//                 align_of::<Vertex>() as u64,
//                 vertex_input_buffer_memory_req.size,
//             );
//             vert_align.copy_from_slice(&vertices);
//             self.device.unmap_memory(vertex_input_buffer_memory);
//             self.device
//                 .bind_buffer_memory(vertex_input_buffer, vertex_input_buffer_memory, 0)
//                 .unwrap();
//             let mut vertex_spv_file =
//                 Cursor::new(&include_bytes!("../../resource/shaderCaches/default.vert.spirv")[..]);
//             let mut frag_spv_file = Cursor::new(&include_bytes!("../../resource/shaderCaches/default.frag.spirv")[..]);
//
//             let vertex_code =
//                 read_spv(&mut vertex_spv_file).expect("Failed to read vertex shader spv file");
//             let vertex_shader_info = vk::ShaderModuleCreateInfo::builder().code(&vertex_code);
//
//             let frag_code =
//                 read_spv(&mut frag_spv_file).expect("Failed to read fragment shader spv file");
//             let frag_shader_info = vk::ShaderModuleCreateInfo::builder().code(&frag_code);
//
//             let vertex_shader_module = self
//                 .device
//                 .create_shader_module(&vertex_shader_info, None)
//                 .expect("Vertex shader module error");
//
//             let fragment_shader_module = self
//                 .device
//                 .create_shader_module(&frag_shader_info, None)
//                 .expect("Fragment shader module error");
//
//             let layout_create_info = vk::PipelineLayoutCreateInfo::default();
//
//             let pipeline_layout = self
//                 .device
//                 .create_pipeline_layout(&layout_create_info, None)
//                 .unwrap();
//
//             let shader_entry_name = CString::new("main").unwrap();
//             let shader_stage_create_infos = [
//                 vk::PipelineShaderStageCreateInfo {
//                     module: vertex_shader_module,
//                     p_name: shader_entry_name.as_ptr(),
//                     stage: vk::ShaderStageFlags::VERTEX,
//                     ..Default::default()
//                 },
//                 vk::PipelineShaderStageCreateInfo {
//                     s_type: vk::StructureType::PIPELINE_SHADER_STAGE_CREATE_INFO,
//                     module: fragment_shader_module,
//                     p_name: shader_entry_name.as_ptr(),
//                     stage: vk::ShaderStageFlags::FRAGMENT,
//                     ..Default::default()
//                 },
//             ];
//             let vertex_input_binding_descriptions = [vk::VertexInputBindingDescription {
//                 binding: 0,
//                 stride: mem::size_of::<Vertex>() as u32,
//                 input_rate: vk::VertexInputRate::VERTEX,
//             }];
//             let vertex_input_attribute_descriptions = [
//                 vk::VertexInputAttributeDescription {
//                     location: 0,
//                     binding: 0,
//                     format: vk::Format::R32G32B32A32_SFLOAT,
//                     offset: offset_of!(Vertex, pos) as u32,
//                 },
//                 vk::VertexInputAttributeDescription {
//                     location: 1,
//                     binding: 0,
//                     format: vk::Format::R32G32B32A32_SFLOAT,
//                     offset: offset_of!(Vertex, color) as u32,
//                 },
//             ];
//
//             let vertex_input_state_info = vk::PipelineVertexInputStateCreateInfo {
//                 vertex_attribute_description_count: vertex_input_attribute_descriptions.len() as u32,
//                 p_vertex_attribute_descriptions: vertex_input_attribute_descriptions.as_ptr(),
//                 vertex_binding_description_count: vertex_input_binding_descriptions.len() as u32,
//                 p_vertex_binding_descriptions: vertex_input_binding_descriptions.as_ptr(),
//                 ..Default::default()
//             };
//             let vertex_input_assembly_state_info = vk::PipelineInputAssemblyStateCreateInfo {
//                 topology: vk::PrimitiveTopology::TRIANGLE_LIST,
//                 ..Default::default()
//             };
//             let viewports = [vk::Viewport {
//                 x: 0.0,
//                 y: 0.0,
//                 width: self.surface_resolution.width as f32,
//                 height: self.surface_resolution.height as f32,
//                 min_depth: 0.0,
//                 max_depth: 1.0,
//             }];
//             let scissors = [vk::Rect2D {
//                 offset: vk::Offset2D { x: 0, y: 0 },
//                 extent: self.surface_resolution,
//             }];
//             let viewport_state_info = vk::PipelineViewportStateCreateInfo::builder()
//                 .scissors(&scissors)
//                 .viewports(&viewports);
//
//             let rasterization_info = vk::PipelineRasterizationStateCreateInfo {
//                 front_face: vk::FrontFace::COUNTER_CLOCKWISE,
//                 line_width: 1.0,
//                 polygon_mode: vk::PolygonMode::FILL,
//                 ..Default::default()
//             };
//             let multisample_state_info = vk::PipelineMultisampleStateCreateInfo {
//                 rasterization_samples: vk::SampleCountFlags::TYPE_1,
//                 ..Default::default()
//             };
//             let noop_stencil_state = vk::StencilOpState {
//                 fail_op: vk::StencilOp::KEEP,
//                 pass_op: vk::StencilOp::KEEP,
//                 depth_fail_op: vk::StencilOp::KEEP,
//                 compare_op: vk::CompareOp::ALWAYS,
//                 ..Default::default()
//             };
//             let depth_state_info = vk::PipelineDepthStencilStateCreateInfo {
//                 depth_test_enable: 1,
//                 depth_write_enable: 1,
//                 depth_compare_op: vk::CompareOp::LESS_OR_EQUAL,
//                 front: noop_stencil_state,
//                 back: noop_stencil_state,
//                 max_depth_bounds: 1.0,
//                 ..Default::default()
//             };
//             let color_blend_attachment_states = [vk::PipelineColorBlendAttachmentState {
//                 blend_enable: 0,
//                 src_color_blend_factor: vk::BlendFactor::SRC_COLOR,
//                 dst_color_blend_factor: vk::BlendFactor::ONE_MINUS_DST_COLOR,
//                 color_blend_op: vk::BlendOp::ADD,
//                 src_alpha_blend_factor: vk::BlendFactor::ZERO,
//                 dst_alpha_blend_factor: vk::BlendFactor::ZERO,
//                 alpha_blend_op: vk::BlendOp::ADD,
//                 color_write_mask: vk::ColorComponentFlags::all(),
//             }];
//             let color_blend_state = vk::PipelineColorBlendStateCreateInfo::builder()
//                 .logic_op(vk::LogicOp::CLEAR)
//                 .attachments(&color_blend_attachment_states);
//
//             let dynamic_state = [vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR];
//             let dynamic_state_info = vk::PipelineDynamicStateCreateInfo::builder().dynamic_states(&dynamic_state);
//             let graphic_pipeline_info = vk::GraphicsPipelineCreateInfo::builder()
//                 .stages(&shader_stage_create_infos)
//                 .vertex_input_state(&vertex_input_state_info)
//                 .input_assembly_state(&vertex_input_assembly_state_info)
//                 .viewport_state(&viewport_state_info)
//                 .rasterization_state(&rasterization_info)
//                 .multisample_state(&multisample_state_info)
//                 .depth_stencil_state(&depth_state_info)
//                 .color_blend_state(&color_blend_state)
//                 .dynamic_state(&dynamic_state_info)
//                 .layout(pipeline_layout)
//                 .render_pass(renderpass);
//
//             let graphics_pipelines = self.device.create_graphics_pipelines(
//                 vk::PipelineCache::null(),
//                 &[graphic_pipeline_info.build()],
//                 None,
//             ).expect("Unable to create graphics pipeline");
//         }
//     }
//
//     let render_scene = (|| {
//         unsafe {
//             let graphic_pipeline = graphics_pipelines[0];
//             let (present_index, _) = self.swapchain_loader.acquire_next_image(
//                 self.swapchain,
//                 std::u64::MAX,
//                 self.present_complete_semaphore,
//                 vk::Fence::null(),
//             ).unwrap();
//             let clear_values = [
//                 vk::ClearValue {
//                     color: vk::ClearColorValue {
//                         float32: [0.0, 0.0, 0.0, 0.0],
//                     },
//                 },
//                 vk::ClearValue {
//                     depth_stencil: vk::ClearDepthStencilValue {
//                         depth: 1.0,
//                         stencil: 0,
//                     },
//                 },
//             ];
//
//             let render_pass_begin_info = vk::RenderPassBeginInfo::builder()
//                 .render_pass(renderpass)
//                 .framebuffer(framebuffers[present_index as usize])
//                 .render_area(vk::Rect2D {
//                     offset: vk::Offset2D { x: 0, y: 0 },
//                     extent: self.surface_resolution,
//                 })
//                 .clear_values(&clear_values);
//
//             record_submit_commandbuffer(
//                 &self.device,
//                 self.draw_command_buffer,
//                 self.present_queue,
//                 &[vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT],
//                 &[self.present_complete_semaphore],
//                 &[self.rendering_complete_semaphore],
//                 |device, draw_command_buffer| {
//                     device.cmd_begin_render_pass(
//                         draw_command_buffer,
//                         &render_pass_begin_info,
//                         vk::SubpassContents::INLINE,
//                     );
//                     device.cmd_bind_pipeline(
//                         draw_command_buffer,
//                         vk::PipelineBindPoint::GRAPHICS,
//                         graphic_pipeline,
//                     );
//                     device.cmd_set_viewport(draw_command_buffer, 0, &viewports);
//                     device.cmd_set_scissor(draw_command_buffer, 0, &scissors);
//                     device.cmd_bind_vertex_buffers(
//                         draw_command_buffer,
//                         0,
//                         &[vertex_input_buffer],
//                         &[0],
//                     );
//                     device.cmd_bind_index_buffer(
//                         draw_command_buffer,
//                         index_buffer,
//                         0,
//                         vk::IndexType::UINT32,
//                     );
//                     device.cmd_draw_indexed(
//                         draw_command_buffer,
//                         index_buffer_data.len() as u32,
//                         1,
//                         0,
//                         0,
//                         1,
//                     );
//                     // Or draw without the index buffer
//                     // device.cmd_draw(draw_command_buffer, 3, 1, 0, 0);
//                     device.cmd_end_render_pass(draw_command_buffer);
//                 },
//             );
//             //let mut present_info_err = mem::zeroed();
//             let wait_semaphors = [self.rendering_complete_semaphore];
//             let swapchains = [self.swapchain];
//             let image_indices = [present_index];
//             let present_info = vk::PresentInfoKHR::builder()
//                 .wait_semaphores(&wait_semaphors) // &self.rendering_complete_semaphore)
//                 .swapchains(&swapchains)
//                 .image_indices(&image_indices);
//
//             self.swapchain_loader
//                 .queue_present(self.present_queue, &present_info)
//                 .unwrap();
//         }
//     });
//
//     let destroy = (|| {
//         unsafe {
//             self.device.device_wait_idle().unwrap();
//             for pipeline in graphics_pipelines {
//                 self.device.destroy_pipeline(pipeline, None);
//             }
//             self.device.destroy_pipeline_layout(pipeline_layout, None);
//             self.device.destroy_shader_module(vertex_shader_module, None);
//             self.device.destroy_shader_module(fragment_shader_module, None);
//             self.device.free_memory(index_buffer_memory, None);
//             self.device.destroy_buffer(index_buffer, None);
//             self.device.free_memory(vertex_input_buffer_memory, None);
//             self.device.destroy_buffer(vertex_input_buffer, None);
//             for framebuffer in framebuffers {
//                 self.device.destroy_framebuffer(framebuffer, None);
//             }
//             self.device.destroy_render_pass(renderpass, None);
//         }
//     });
//
//     (initialize_renderer, render_scene destroy)
}

impl Drop for RendererData {
    fn drop(&mut self) {
        unsafe {
            self.device.device_wait_idle().unwrap();
            self.device
                .destroy_semaphore(self.present_complete_semaphore, None);
            self.device
                .destroy_semaphore(self.rendering_complete_semaphore, None);
            self.device.free_memory(self.depth_image_memory, None);
            self.device.destroy_image_view(self.depth_image_view, None);
            self.device.destroy_image(self.depth_image, None);
            for &image_view in self.present_image_views.iter() {
                self.device.destroy_image_view(image_view, None);
            }
            self.device.destroy_command_pool(self.pool, None);
            self.swapchain_loader
                .destroy_swapchain(self.swapchain, None);
            self.device.destroy_device(None);
            self.surface_loader.destroy_surface(self.surface, None);
            self.debug_utils_loader
                .destroy_debug_utils_messenger(self.debug_call_back, None);
            self.instance.destroy_instance(None);
        }
    }
}