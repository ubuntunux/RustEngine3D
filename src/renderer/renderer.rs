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
    vulkan_context
};
use crate::vulkan_context::vulkan_context::{
    FrameIndexMap,
    SwapchainIndexMap,
    RenderFeatures,
};

#[derive(Clone, Debug, Copy)]
struct Vertex {
    pos: [f32; 4],
    color: [f32; 4],
}


pub struct RendererData {
    pub _frame_index: i32,
    pub _swapchain_index: u32,
    pub _need_recreate_swapchain: bool,
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
    pub _render_features: vk::RenderFeatures,
    //pub _debug_render_target: RenderTargetType,
    //pub _render_target_data_map: RenderTargetDataMap,
    //pub _uniform_buffer_data_map: UniformBufferDataMap,
    //pub _postprocess_ssao: PostProcessData,
    pub _resources: Rc<RefCell<resource::Resources>>
}


pub fn create_renderer_data<T> (app_name: &str, app_version: u32, (window_width, window_height): (u32, u32), event_loop: &EventLoop<T>) -> Rc<RefCell<RendererData>> {
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
        let (physical_device, swapchain_support_details, physical_device_features) = device::select_physical_device(&instance, &surface_interface, &surface).unwrap();
        let deviceProperties = instance.get_physical_device_properties(physical_device);
        let msaa_samples = device::get_max_usable_sample_count(&deviceProperties);
        let queue_family_indices = queue::get_queue_family_indices(
            &instance,
            &surface_interface,
            surface,
            physical_device,
            constants::IS_CONCURRENT_MODE
        );
        let render_features = vulkan_context::RenderFeatures {
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

        let device = device::create_device(&instance, physical_device, &renderFeatures, &queue_family_index_set);
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
        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT::builder()
            .message_severity(
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
                    | vk::DebugUtilsMessageSeverityFlagsEXT::WARNING
                //| vk::DebugUtilsMessageSeverityFlagsEXT::INFO,
            )
            .message_type(vk::DebugUtilsMessageTypeFlagsEXT::all())
            .pfn_user_callback(Some(vulkan_context::vulkan_debug_callback));
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
    //     // recreateSwapchain rendererData window
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
//             let (present_index, _) = self.swapchain_interface.acquire_next_image(
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
//             self.swapchain_interface
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
            self.device.destroy_semaphore(self.present_complete_semaphore, None);
            self.device.destroy_semaphore(self.rendering_complete_semaphore, None);
            self.device.free_memory(self.depth_image_memory, None);
            self.device.destroy_image_view(self.depth_image_view, None);
            self.device.destroy_image(self.depth_image, None);
            self.device.destroy_command_pool(self.pool, None);
            swapchain::destroy_swapchain_data(&self.device, &self.swapchain_interface, &self._swapchain_data);
            device::destroy_device(&self.device);
            device::destroy_vk_surface(&self.surface_interface, &self.surface);
            self.debug_util_interface.destroy_debug_utils_messenger(self.debug_call_back, None);
            device::destroy_vk_instance(&self.instance);
        }
    }
}