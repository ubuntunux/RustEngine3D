use std::str::FromStr;
use std::collections::HashMap;
use std::cell::{ Ref, RefMut };
use std::borrow::{Cow};
use std::ffi::CStr;
use std::vec::Vec;
use ash::{
    vk,
    Device,
    Entry,
    Instance,
};
use ash::prelude::VkResult;
use ash::extensions::ext::DebugUtils;
use ash::extensions::khr::{
    Surface,
    Swapchain,
};
use ash::version::{InstanceV1_0, DeviceV1_0};
use winit;
use winit::dpi;
use winit::window::{
    Window,
    WindowBuilder
};
use winit::event_loop::EventLoop;
use nalgebra::{ Vector2, Vector3, Vector4, Matrix4 };

use crate::application::SceneManagerData;
use crate::constants;
use crate::vulkan_context::{
    buffer,
    command_buffer,
    device,
    queue,
    sync,
    texture,
};

use crate::vulkan_context::buffer::{ ShaderBufferData };
use crate::vulkan_context::descriptor::{ self, DescriptorResourceInfo };
use crate::vulkan_context::framebuffer::FramebufferData;
use crate::vulkan_context::geometry_buffer::{ self, GeometryData };
use crate::vulkan_context::render_pass::{ RenderPassData, PipelineData };
use crate::vulkan_context::swapchain::{ self, SwapchainData };
use crate::vulkan_context::texture::{ TextureCreateInfo, TextureData };
use crate::vulkan_context::vulkan_context::{ RenderFeatures, SwapchainIndexMap, FrameIndexMap };
use crate::renderer::image_sampler::{ self, ImageSamplerData };
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::render_target::{ self, RenderTargetType };
use crate::renderer::shader_buffer_datas::{
    self,
    NONE_PUSH_CONSTANT,
    ShaderBufferDataType,
    ShaderBufferDataMap,
    PushConstant_StaticRenderObject,
    PushConstant_SkeletalRenderObject,
    PushConstant_GaussianBlur,
    PushConstant_RenderCopy,
    PushConstant_RenderColor,
};
use crate::renderer::post_process::{ PostProcessData_Bloom, PostProcessData_SSAO, PostProcessData_TAA };
use crate::renderer::render_element::{ RenderElementData };
use crate::resource::{ Resources };
use crate::utilities::system::{ self, RcRefCell };

pub type RenderTargetDataMap = HashMap<RenderTargetType, TextureData>;

// NOTE : RenderMode must match with scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RenderMode {
    RenderMode_Common = 0,
    RenderMode_Shadow = 1,
}

// NOTE : RenderObjectType must match with scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum RenderObjectType {
    Static = 0,
    Skeletal = 1,
}

pub unsafe extern "system" fn vulkan_debug_callback(
    message_severity: vk::DebugUtilsMessageSeverityFlagsEXT,
    message_type: vk::DebugUtilsMessageTypeFlagsEXT,
    p_callback_data: *const vk::DebugUtilsMessengerCallbackDataEXT,
    _user_data: *mut std::os::raw::c_void,
) -> vk::Bool32 {
    let callback_data = *p_callback_data;
    let message_id_number: i32 = callback_data.message_id_number as i32;
    let message_id_name = if callback_data.p_message_id_name.is_null() {
        Cow::from("")
    } else {
        CStr::from_ptr(callback_data.p_message_id_name).to_string_lossy()
    };
    let message = if callback_data.p_message.is_null() {
        Cow::from("")
    } else {
        CStr::from_ptr(callback_data.p_message).to_string_lossy()
    };
    println!(
        "[{:?}]:{:?} [{} ({})] : {}",
        message_severity,
        message_type,
        message_id_name,
        &message_id_number.to_string(),
        message,
    );
    vk::FALSE
}

pub fn get_debug_message_level(debug_message_level: vk::DebugUtilsMessageSeverityFlagsEXT) -> vk::DebugUtilsMessageSeverityFlagsEXT {
    match debug_message_level {
        vk::DebugUtilsMessageSeverityFlagsEXT::INFO => (
            vk::DebugUtilsMessageSeverityFlagsEXT::INFO |
                vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        vk::DebugUtilsMessageSeverityFlagsEXT::WARNING => (
            vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        vk::DebugUtilsMessageSeverityFlagsEXT::ERROR => (
            vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
        _ => (
            vk::DebugUtilsMessageSeverityFlagsEXT::VERBOSE |
                vk::DebugUtilsMessageSeverityFlagsEXT::INFO |
                vk::DebugUtilsMessageSeverityFlagsEXT::WARNING |
                vk::DebugUtilsMessageSeverityFlagsEXT::ERROR
        ),
    }
}

pub struct RendererData {
    _frame_index: i32,
    _swapchain_index: u32,
    _need_recreate_swapchain: bool,
    _is_first_resize_event: bool,
    _is_first_rendering: bool,
    pub _window: Window,
    pub _entry: Entry,
    pub _instance: Instance,
    pub _device: Device,
    pub _device_properties: vk::PhysicalDeviceProperties,
    pub _device_memory_properties: vk::PhysicalDeviceMemoryProperties,
    pub _physical_device: vk::PhysicalDevice,
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
    pub _command_buffers: SwapchainIndexMap<vk::CommandBuffer>,
    pub _render_features: RenderFeatures,
    pub _image_samplers: ImageSamplerData,
    pub _debug_render_target: RenderTargetType,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _shader_buffer_data_map: ShaderBufferDataMap,
    pub _post_process_data_bloom: PostProcessData_Bloom,
    pub _post_process_data_ssao: PostProcessData_SSAO,
    pub _post_process_data_taa: PostProcessData_TAA,
    pub _resources: RcRefCell<Resources>
}

pub fn create_renderer_data<T>(
    app_name: &str,
    app_version: u32,
    (window_width, window_height): (u32, u32),
    event_loop: &EventLoop<T>,
    resources: RcRefCell<Resources>
) -> RcRefCell<RendererData> {
    unsafe {
        log::info!("create_renderer_data: {}, width: {}, height: {}", constants::ENGINE_NAME, window_width, window_height);
        let window = WindowBuilder::new()
            .with_title(app_name)
            .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width: window_width, height: window_height }))
            .build(&event_loop)
            .unwrap();
        let entry = Entry::new().unwrap();
        let surface_extensions = ash_window::enumerate_required_extensions(&window).unwrap();
        let instance: Instance = device::create_vk_instance(&entry, &app_name, app_version, &surface_extensions);
        let surface = device::create_vk_surface(&entry, &instance, &window);
        let surface_interface = Surface::new(&entry, &instance);
        let (physical_device, swapchain_support_details, physical_device_features) = device::select_physical_device(&instance, &surface_interface, surface).unwrap();
        let device_properties: vk::PhysicalDeviceProperties = instance.get_physical_device_properties(physical_device);
        let device_memory_properties: vk::PhysicalDeviceMemoryProperties = instance.get_physical_device_memory_properties(physical_device);
        let msaa_samples = device::get_max_usable_sample_count(&device_properties);
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
        let command_buffers = command_buffer::create_command_buffers(&device, command_pool, constants::SWAPCHAIN_IMAGE_COUNT as u32);

        // debug utils
        let debug_message_level = get_debug_message_level(constants::DEBUG_MESSAGE_LEVEL);
        let debug_info = vk::DebugUtilsMessengerCreateInfoEXT {
            message_severity: debug_message_level,
            message_type: vk::DebugUtilsMessageTypeFlagsEXT::all(),
            pfn_user_callback: Some(vulkan_debug_callback),
            ..Default::default()
        };
        let debug_util_interface = DebugUtils::new(&entry, &instance);
        let debug_call_back = debug_util_interface.create_debug_utils_messenger(&debug_info, None).unwrap();
        let mut renderer_data = RendererData {
            _frame_index: 0,
            _swapchain_index: 0,
            _need_recreate_swapchain: false,
            _is_first_resize_event: true,
            _is_first_rendering: true,
            _window: window,
            _entry: entry,
            _instance: instance,
            _device: device,
            _device_properties: device_properties,
            _device_memory_properties: device_memory_properties,
            _physical_device: physical_device,
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
            _image_samplers: ImageSamplerData::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _render_target_data_map: RenderTargetDataMap::new(),
            _shader_buffer_data_map: ShaderBufferDataMap::new(),
            _post_process_data_bloom: PostProcessData_Bloom::default(),
            _post_process_data_ssao: PostProcessData_SSAO::default(),
            _post_process_data_taa: PostProcessData_TAA::default(),
            _resources: resources.clone(),
        };

        renderer_data.initialize_renderer();

        system::newRcRefCell(renderer_data)
    }
}

impl RendererData {
    pub fn get_need_recreate_swapchain(&self) -> bool { self._need_recreate_swapchain }
    pub fn set_need_recreate_swapchain(&mut self, value: bool) { self._need_recreate_swapchain = value; }
    pub fn get_is_first_resize_event(&self) -> bool { self._is_first_resize_event }
    pub fn set_is_first_resize_event(&mut self, value: bool) { self._is_first_resize_event = value; }
    pub fn reset_is_first_rendering(&mut self) { self._is_first_rendering = true; }
    pub fn get_instance(&self) -> &Instance { &self._instance }
    pub fn get_device(&self) -> &Device { &self._device }
    pub fn get_device_properties(&self) -> &vk::PhysicalDeviceProperties { &self._device_properties }
    pub fn get_device_memory_properties(&self) -> &vk::PhysicalDeviceMemoryProperties { &self._device_memory_properties }
    pub fn get_physical_device(&self) -> vk::PhysicalDevice { self._physical_device }
    pub fn get_swap_chain_data(&self) -> &SwapchainData { &self._swapchain_data }
    pub fn get_swap_chain_image_views(&self) -> &SwapchainIndexMap<vk::ImageView> { &self._swapchain_data._swapchain_image_views }
    pub fn get_swap_chain_support_details(&self) -> &swapchain::SwapchainSupportDetails { &self._swapchain_support_details }
    pub fn get_swap_chain_index(&self) -> u32 { self._swapchain_index }
    pub fn get_command_pool(&self) -> vk::CommandPool { self._command_pool }
    pub fn get_command_buffers(&self) -> &SwapchainIndexMap<vk::CommandBuffer> { &self._command_buffers }
    pub fn get_command_buffer(&self, index: usize) -> vk::CommandBuffer { self._command_buffers[index] }
    pub fn get_current_command_buffer(&self) -> vk::CommandBuffer { self._command_buffers[self._swapchain_index as usize] }
    pub fn get_graphics_queue(&self) -> vk::Queue { self._queue_family_datas._graphics_queue }
    pub fn get_present_queue(&self) -> vk::Queue { self._queue_family_datas._present_queue }
    pub fn get_shader_buffer_data(&self, buffer_data_type: ShaderBufferDataType) -> &ShaderBufferData {
        &self._shader_buffer_data_map.get(&buffer_data_type).unwrap()
    }

    pub fn initialize_post_process_datas(&mut self) {
        // Bloom
        self._post_process_data_bloom.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::Bloom1).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::Bloom2).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::Bloom3).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::Bloom4).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp0).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp1).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp2).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp3).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp4).as_ref().unwrap(),
        );
        // Temporal AA
        self._post_process_data_taa.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(),
        );
        // SSAO
        self._post_process_data_ssao.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SSAO).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSAOTemp).as_ref().unwrap(),
        )
    }

    pub fn destroy_post_process_datas(&mut self) {
        self._post_process_data_bloom.destroy(&self._device);
        self._post_process_data_taa.destroy(&self._device);
        self._post_process_data_ssao.destroy(&self._device);
    }

    pub fn update_post_process_datas(&mut self) {
    }

    pub fn next_debug_render_target(&mut self) {
        self._debug_render_target = if RenderTargetType::MaxBound == self._debug_render_target {
            unsafe { std::mem::transmute(0) }
        } else {
            let enum_to_int: i32 = self._debug_render_target.clone() as i32;
            unsafe { std::mem::transmute(enum_to_int + 1) }
        };
        log::info!("Current DebugRenderTarget: {:?}", self._debug_render_target);
    }

    pub fn prev_debug_render_target(&mut self) {
        let enum_to_int: i32 = self._debug_render_target.clone() as i32;
        self._debug_render_target = if 0 == enum_to_int {
            unsafe { std::mem::transmute(RenderTargetType::MaxBound as i32 - 1) }
        } else {
            unsafe { std::mem::transmute(enum_to_int - 1) }
        };
        log::info!("Current DebugRenderTarget: {:?}", self._debug_render_target);
    }

    pub fn get_render_target(&self, render_target_type: RenderTargetType) -> &TextureData {
        &self._render_target_data_map.get(&render_target_type).unwrap()
    }

    pub fn create_render_targets(&mut self) {
        let render_taget_create_infos = render_target::get_render_target_create_infos(self);
        for render_taget_create_info in render_taget_create_infos.iter() {
            let render_target_type: RenderTargetType = RenderTargetType::from_str(render_taget_create_info._texture_name.as_str()).unwrap();
            let texture_data = self.create_render_target(render_taget_create_info);
            self._render_target_data_map.insert(render_target_type, texture_data);
        }
    }

    pub fn create_render_target<T>(&self, texture_create_info: &TextureCreateInfo<T>) -> TextureData {
        texture::create_render_target(
            self.get_instance(),
            self.get_device(),
            self.get_physical_device(),
            self.get_device_memory_properties(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            texture_create_info
        )
    }

    pub fn create_texture<T: Copy>(&self, texture_create_info: &TextureCreateInfo<T>) -> TextureData {
        texture::create_texture_data(
            self.get_instance(),
            self.get_device(),
            self.get_physical_device(),
            self.get_device_memory_properties(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            texture_create_info
        )
    }

    pub fn destroy_texture(&self, texture_data: &TextureData) {
        texture::destroy_texture_data(self.get_device(), texture_data);
    }

    pub fn destroy_render_targets(&mut self) {
        for render_target_data in self._render_target_data_map.values() {
            texture::destroy_texture_data(self.get_device(), render_target_data);
        }
        self._render_target_data_map.clear();
    }

    pub fn destroy_uniform_buffers(&mut self) {
        for shader_buffer_data in self._shader_buffer_data_map.values() {
            buffer::destroy_shader_buffer_data(self.get_device(), shader_buffer_data);
        }
        self._shader_buffer_data_map.clear();
    }

    pub fn create_geometry_buffer(
        &self,
        geometry_name: &String,
        geometry_create_info: &geometry_buffer::GeometryCreateInfo
    ) -> geometry_buffer::GeometryData {
        geometry_buffer::create_geometry_data(
            self.get_device(),
            self.get_command_pool(),
            self.get_graphics_queue(),
            self.get_device_memory_properties(),
            geometry_name,
            geometry_create_info
        )
    }

    pub fn destroy_geomtry_buffer(&self, geometry_data: &geometry_buffer::GeometryData) {
        geometry_buffer::destroy_geometry_data(self.get_device(), geometry_data);
    }

    pub fn destroy_renderer_data(&mut self) {
        unsafe {
            self.destroy_uniform_buffers();
            image_sampler::destroy_image_samplers(self.get_device(), &self._image_samplers);
            self.destroy_render_targets();
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

    pub fn render_material_instance<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        material_instance_name: &str,
        geometry_data: &GeometryData,
        custom_framebuffer_data: Option<&FramebufferData>,
        custom_descriptor_sets: Option<&SwapchainIndexMap<vk::DescriptorSet>>,
        push_constant_data: Option<&T>,
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
        let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
        self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, custom_framebuffer_data);
        self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, custom_descriptor_sets);
        if let Some(push_constant_data) = push_constant_data {
            self.upload_push_constant_data(
                command_buffer,
                &pipeline_data.borrow(),
                push_constant_data
            );
        }
        self.draw_elements(command_buffer, geometry_data);
        self.end_render_pass(command_buffer);
    }

    pub fn render_render_pass_pipeline<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_pass_pipeline_data_name: &str,
        material_instance_name: &str,
        push_constant_data: Option<&T>,
        geometry_data: &GeometryData
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name);
        let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
        let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
        self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
        self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, None);
        if let Some(push_constant_data) = push_constant_data {
            self.upload_push_constant_data(
                command_buffer,
                &pipeline_data.borrow(),
                push_constant_data
            );
        }
        self.draw_elements(command_buffer, geometry_data);
        self.end_render_pass(command_buffer);
    }

    pub fn begin_render_pass_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_pass_data: &RcRefCell<RenderPassData>,
        pipeline_data: &RcRefCell<PipelineData>,
        custom_framebuffer: Option<&FramebufferData>,
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let render_pass_data: Ref<RenderPassData> = render_pass_data.borrow();
        let framebuffer_data: *const FramebufferData = match custom_framebuffer {
            Some(custom_framebuffer) => custom_framebuffer,
            None => resources.get_framebuffer_data(render_pass_data.get_render_pass_data_name().as_str()).as_ptr()
        };
        unsafe {
            let render_pass_begin_info = (*framebuffer_data)._render_pass_begin_infos[swapchain_index as usize];
            let pipeline_bind_point = pipeline_data.borrow()._pipeline_bind_point;
            let pipeline_dynamic_states = &pipeline_data.borrow()._pipeline_dynamic_states;
            self._device.cmd_begin_render_pass(command_buffer, &render_pass_begin_info, vk::SubpassContents::INLINE);
            if pipeline_dynamic_states.contains(&vk::DynamicState::VIEWPORT) {
                self._device.cmd_set_viewport(command_buffer, 0, &[(*framebuffer_data)._framebuffer_info._framebuffer_view_port]);
            }
            if pipeline_dynamic_states.contains(&vk::DynamicState::SCISSOR) {
                self._device.cmd_set_scissor(command_buffer, 0, &[(*framebuffer_data)._framebuffer_info._framebuffer_scissor_rect]);
            }
            self._device.cmd_bind_pipeline(command_buffer, pipeline_bind_point, pipeline_data.borrow()._pipeline);
        }
    }

    pub fn bind_descriptor_sets(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        custom_descriptor_sets: Option<&SwapchainIndexMap<vk::DescriptorSet>>) {
        let pipeline_layout = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_layout;
        let pipeline_bind_point = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_bind_point;
        let descriptor_sets: &SwapchainIndexMap<vk::DescriptorSet> = match custom_descriptor_sets {
            Some(custom_descriptor_sets) => custom_descriptor_sets,
            None => &pipeline_binding_data._descriptor_sets,
        };
        if false == descriptor_sets.is_empty() {
            let dynamic_offsets: &[u32] = &[];
            unsafe {
                self._device.cmd_bind_descriptor_sets(command_buffer, pipeline_bind_point, pipeline_layout, 0, &[descriptor_sets[swapchain_index as usize]], dynamic_offsets);
            }
        }
    }

    pub fn update_descriptor_set(
        &self,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        descriptor_offset: usize,
        descriptor_resource_info: &DescriptorResourceInfo
    ) {
        let wirte_descriptor_sets: &Vec<vk::WriteDescriptorSet> = &pipeline_binding_data._write_descriptor_sets[swapchain_index as usize];
        let write_descriptor_set = descriptor::create_write_descriptor_set(wirte_descriptor_sets, descriptor_offset, descriptor_resource_info);
        let descriptor_copies: &[vk::CopyDescriptorSet] = &[];
        unsafe {
            self._device.update_descriptor_sets(&[write_descriptor_set], descriptor_copies);
        }
    }

    pub fn update_descriptor_set_mut(
        &self,
        swapchain_index: u32,
        pipeline_binding_data: &mut PipelineBindingData,
        descriptor_offset: usize,
        descriptor_resource_info: &DescriptorResourceInfo
    ) {
        let wirte_descriptor_sets: &mut Vec<vk::WriteDescriptorSet> = &mut pipeline_binding_data._write_descriptor_sets[swapchain_index as usize];
        descriptor::update_write_descriptor_set(wirte_descriptor_sets, descriptor_offset, descriptor_resource_info);
        let wirte_descriptor_set_offset = wirte_descriptor_sets[descriptor_offset];
        let descriptor_copies: &[vk::CopyDescriptorSet] = &[];
        unsafe {
            self._device.update_descriptor_sets(&[wirte_descriptor_set_offset], descriptor_copies);
        }
    }

    pub fn upload_push_constant_data<T>(&self, command_buffer: vk::CommandBuffer, pipeline_data: &PipelineData, push_constant_data: &T) {
        let constants: &[u8] = system::to_bytes(push_constant_data);
        unsafe {
            self._device.cmd_push_constants(command_buffer, pipeline_data._pipeline_layout, vk::ShaderStageFlags::ALL, 0, constants);
        }
    }

    pub fn draw_elements(&self, command_buffer: vk::CommandBuffer, geometry_data: &GeometryData) {
        unsafe {
            let offsets: &[vk::DeviceSize] = &[0];
            const INSTANCE_COUNT: u32 = 1;
            const FIRST_INDEX: u32 = 0;
            const VERTEX_OFFSET: i32 = 0;
            const FIRST_INSTANCE: u32 = 0;
            self._device.cmd_bind_vertex_buffers(command_buffer, 0, &[geometry_data._vertex_buffer_data._buffer], offsets);
            self._device.cmd_bind_index_buffer(command_buffer, geometry_data._index_buffer_data._buffer, 0, vk::IndexType::UINT32);
            self._device.cmd_draw_indexed(command_buffer, geometry_data._vertex_index_count, INSTANCE_COUNT, FIRST_INDEX, VERTEX_OFFSET, FIRST_INSTANCE);
        }
    }

    pub fn end_render_pass(&self, command_buffer: vk::CommandBuffer) {
        unsafe {
            self._device.cmd_end_render_pass(command_buffer);
        }
    }

    pub fn device_wait_idle(&self) {
        unsafe {
            self._device.device_wait_idle().expect("vkDeviceWaitIdle failed!");
        }
    }

    pub fn initialize_renderer(&mut self) {
        self._swapchain_index = 0;
        self._frame_index = 0;
        self._need_recreate_swapchain = false;
        shader_buffer_datas::regist_shader_buffer_datas(
            &self._device,
            &self._device_memory_properties,
            &mut self._shader_buffer_data_map
        );
        self._image_samplers = image_sampler::create_image_samplers(self.get_device());
        self.create_render_targets();
    }

    pub fn resize_window(&mut self) {
        log::info!("<< resizeWindow >>");
        self.device_wait_idle();

        let resources = self._resources.clone();

        // destroy swapchain & graphics resources
        self.destroy_post_process_datas();
        resources.borrow_mut().unload_graphics_datas(self);
        self.destroy_render_targets();

        // recreate swapchain & graphics resources
        self.recreate_swapchain();
        self.create_render_targets();
        resources.borrow_mut().load_graphics_datas(self);
        self.initialize_post_process_datas();
        self.reset_is_first_rendering();
    }

    pub fn recreate_swapchain(&mut self) {
        log::info!("<< recreateSwapChain >>");
        command_buffer::destroy_command_buffers(&self._device, self._command_pool, &self._command_buffers);
        swapchain::destroy_swapchain_data(&self._device, &self._swapchain_interface, &self._swapchain_data);

        self._swapchain_support_details = swapchain::query_swapchain_support(&self._surface_interface, self._physical_device, self._surface);
        self._swapchain_data = swapchain::create_swapchain_data(
            &self._device,
            &self._swapchain_interface,
            self._surface,
            &self._swapchain_support_details,
            &self._queue_family_datas,
            constants::ENABLE_IMMEDIATE_MODE
        );
        self._command_buffers = command_buffer::create_command_buffers(&self._device, self._command_pool, constants::SWAPCHAIN_IMAGE_COUNT as u32);
    }

    pub fn present_swapchain(
        &self,
        command_buffers: &[vk::CommandBuffer],
        fence: vk::Fence,
        image_available_semaphore: vk::Semaphore,
        render_finished_semaphore: vk::Semaphore,
    ) -> VkResult<bool> {
        let wait_semaphores = [image_available_semaphore];
        let wait_mask = [vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT];
        let signal_semaphores = [render_finished_semaphore];
        let submit_info = vk::SubmitInfo {
            wait_semaphore_count: wait_semaphores.len() as u32,
            p_wait_semaphores: wait_semaphores.as_ptr(),
            p_wait_dst_stage_mask: wait_mask.as_ptr(),
            command_buffer_count: command_buffers.len() as u32,
            p_command_buffers: command_buffers.as_ptr(),
            signal_semaphore_count: signal_semaphores.len() as u32,
            p_signal_semaphores: signal_semaphores.as_ptr(),
            ..Default::default()
        };

        unsafe {
            let fences = &[fence];
            self._device.reset_fences(fences).expect("failed to reset_fences");

            let waiting_for_fence = false;
            self._device.queue_submit(
                self._queue_family_datas._graphics_queue,
                &[submit_info],
                if waiting_for_fence { fence } else { vk::Fence::null() }
            ).expect("vkQueueSubmit failed!");

            if waiting_for_fence {
                self._device.wait_for_fences(fences, true, std::u64::MAX).expect("vkWaitForFences failed!");
            }

            let present_wait_semaphores = [render_finished_semaphore];
            let swapchains = [self._swapchain_data._swapchain];
            let image_indices = [self._swapchain_index];
            let present_info = vk::PresentInfoKHR {
                wait_semaphore_count: present_wait_semaphores.len() as u32,
                p_wait_semaphores: present_wait_semaphores.as_ptr(),
                swapchain_count: swapchains.len() as u32,
                p_swapchains: swapchains.as_ptr(),
                p_image_indices: image_indices.as_ptr(),
                ..Default::default()
            };

            let is_swapchain_suboptimal: VkResult<bool> = self._swapchain_interface.queue_present(self.get_present_queue(), &present_info);
            // waiting
            self._device.device_wait_idle().expect("failed to device_wait_idle");
            is_swapchain_suboptimal
        }
    }

    pub fn upload_shader_buffer_data<T>(&self, swapchain_index: u32, buffer_data_type: ShaderBufferDataType, upload_data: &T) {
        let shader_buffer_data = self.get_shader_buffer_data(buffer_data_type);
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        buffer::upload_buffer_data(&self._device, buffer_data, system::to_bytes(upload_data));
    }

    pub fn upload_shader_buffer_datas<T: Copy>(&self, swapchain_index: u32, buffer_data_type: ShaderBufferDataType, upload_data: &[T]) {
        let shader_buffer_data = self.get_shader_buffer_data(buffer_data_type);
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        buffer::upload_buffer_data(&self._device, buffer_data, upload_data);
    }

    pub fn upload_shader_buffer_data_offset<T>(&self, swapchain_index: u32, buffer_data_type: ShaderBufferDataType, upload_data: &T, offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(buffer_data_type);
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        buffer::upload_buffer_data_offset(&self._device, buffer_data, system::to_bytes(upload_data), offset);
    }

    pub fn upload_shader_buffer_datas_offset<T: Copy>(&self, swapchain_index: u32, buffer_data_type: ShaderBufferDataType, upload_data: &[T], offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(buffer_data_type);
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        buffer::upload_buffer_data_offset(&self._device, buffer_data, upload_data, offset);
    }

    pub fn render_scene(&mut self, scene_manager: RefMut<SceneManagerData>, elapsed_time: f64, delta_time: f64) {
        unsafe {
            // frame index
            let frame_index = self._frame_index as usize;
            let frame_fence = self._frame_fences[frame_index];
            let image_available_semaphore = self._image_available_semaphores[frame_index];
            let render_finished_semaphore = self._render_finished_semaphores[frame_index];

            // Begin Render
            let (swapchain_index, is_swapchain_suboptimal) = self._swapchain_interface.acquire_next_image(
                self._swapchain_data._swapchain,
                std::u64::MAX,
                image_available_semaphore,
                vk::Fence::null()
            ).unwrap();

            self._swapchain_index = swapchain_index;

            let command_buffer = self._command_buffers[swapchain_index as usize];
            let present_result: vk::Result = if false == is_swapchain_suboptimal {
                let resources = self._resources.borrow();
                let main_camera =  scene_manager.get_main_camera().borrow();
                let main_light = scene_manager.get_main_light().borrow();
                let quad_mesh = resources.get_mesh_data("quad").borrow();
                let quad_geometry_data: Ref<GeometryData> = quad_mesh.get_default_geometry_data().borrow();

                // Begin command buffer
                let command_buffer_begin_info = vk::CommandBufferBeginInfo {
                    flags: vk::CommandBufferUsageFlags::SIMULTANEOUS_USE,
                    ..Default::default()
                };
                self._device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("vkBeginCommandBuffer failed!");

                // Upload Uniform Buffers
                let ssao_constants = &self._post_process_data_ssao._ssao_constants;
                let light_constants = main_light.get_light_constants();
                let screen_width = self._swapchain_data._swapchain_extent.width as f32;
                let screen_height = self._swapchain_data._swapchain_extent.height as f32;
                let screen_size: Vector2<f32> = Vector2::new(screen_width, screen_height);
                let scene_constants = shader_buffer_datas::SceneConstants {
                    _screen_size: screen_size.clone() as Vector2<f32>,
                    _backbuffer_size: screen_size.clone() as Vector2<f32>,
                    _time: elapsed_time as f32,
                    _delta_time: delta_time as f32,
                    _scene_constants_dummy0: 0,
                    _scene_constants_dummy1: 0,
                };
                let view_constants = shader_buffer_datas::ViewConstants {
                    _view: main_camera._view.into(),
                    _inv_view: main_camera._inv_view.into(),
                    _view_origin: main_camera._view_origin.into(),
                    _inv_view_origin: main_camera._inv_view_origin.into(),
                    _projection: main_camera._projection.into(),
                    _inv_projection: main_camera._inv_projection.into(),
                    _view_projection: main_camera._view_projection.into(),
                    _inv_view_projection: main_camera._inv_view_projection.into(),
                    _view_origin_projection: main_camera._view_origin_projection.into(),
                    _inv_view_origin_projection: main_camera._inv_view_origin_projection.into(),
                    _view_origin_projection_prev: main_camera._view_origin_projection_prev.into(),
                    _projection_jitter: main_camera._projection_jitter.into(),
                    _inv_projection_jitter: main_camera._inv_projection_jitter.into(),
                    _view_projection_jitter: main_camera._view_projection_jitter.into(),
                    _inv_view_projection_jitter: main_camera._inv_view_projection_jitter.into(),
                    _view_origin_projection_jitter: main_camera._view_origin_projection_jitter.into(),
                    _inv_view_origin_projection_jitter: main_camera._inv_view_origin_projection_jitter.into(),
                    _view_origin_projection_prev_jitter: main_camera._view_origin_projection_prev_jitter.into(),
                    _camera_position: main_camera._transform_object._position.clone() as Vector3<f32>,
                    _jitter_frame: main_camera._jitter_frame,
                    _camera_position_prev: main_camera._transform_object._prev_position.clone() as Vector3<f32>,
                    _viewconstants_dummy0: 0.0,
                    _near_far: Vector2::new(constants::NEAR, constants::FAR),
                    _jitter_delta: main_camera._jitter_delta.into(),
                    _jitter_offset: main_camera._jitter.into(),
                    _viewconstants_dummy1: 0.0,
                    _viewconstants_dummy2: 0.0,
                };

                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::SceneConstants, &scene_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::ViewConstants, &view_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::LightConstants, light_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::SSAOConstants, ssao_constants);

                if self._is_first_rendering {
                    self.rendering_at_first(command_buffer, swapchain_index, &quad_geometry_data);
                    self._is_first_rendering = false;
                }

                // Render
                let static_render_elements = scene_manager.get_static_render_elements();
                let skeletal_render_elements = scene_manager.get_skeletal_render_elements();
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::RenderMode_Shadow, RenderObjectType::Static, &static_render_elements);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::RenderMode_Shadow, RenderObjectType::Skeletal, &skeletal_render_elements);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::RenderMode_Common, RenderObjectType::Static, &static_render_elements);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::RenderMode_Common, RenderObjectType::Skeletal, &skeletal_render_elements);
                self.render_pre_process(command_buffer, swapchain_index, &quad_geometry_data);
                self.render_post_process(command_buffer, swapchain_index, &quad_geometry_data);

                // Render Final
                self.render_material_instance(command_buffer, swapchain_index, "render_final", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

                // Render Debug
                //self._debug_render_target = RenderTargetType::Shadow;
                if RenderTargetType::BackBuffer != self._debug_render_target {
                    let render_debug_material_instance_name = "render_debug";
                    let render_debug_render_pass_pipeline_name = "render_debug/render_debug";
                    let mut render_debug_material_instance_data: RefMut<MaterialInstanceData> = resources.get_material_instance_data(&render_debug_material_instance_name).borrow_mut();
                    let mut render_debug_pipeline_binding_data = render_debug_material_instance_data.get_pipeline_binding_data_mut(&render_debug_render_pass_pipeline_name);
                    self.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &render_debug_pipeline_binding_data._render_pass_pipeline_data._render_pass_data,
                        &render_debug_pipeline_binding_data._render_pass_pipeline_data._pipeline_data,
                        None,
                    );

                    let image_info = self.get_render_target(self._debug_render_target);
                    self.update_descriptor_set_mut(
                        swapchain_index,
                        &mut render_debug_pipeline_binding_data,
                        0,
                        &DescriptorResourceInfo::DescriptorImageInfo(image_info._descriptor_image_info),
                    );

                    self.bind_descriptor_sets(command_buffer, swapchain_index, &render_debug_pipeline_binding_data, None);
                    self.draw_elements(command_buffer, &quad_geometry_data);
                    self.end_render_pass(command_buffer);
                }

                // End command buffer
                self._device.end_command_buffer(command_buffer).expect("vkEndCommandBuffer failed!");

                // End Render
                let present_result = self.present_swapchain(&[command_buffer], frame_fence, image_available_semaphore, render_finished_semaphore);
                match present_result {
                    Ok(is_swapchain_suboptimal) => if is_swapchain_suboptimal { vk::Result::SUBOPTIMAL_KHR } else { vk::Result::SUCCESS },
                    Err(err) => err,
                }
            } else {
                vk::Result::SUBOPTIMAL_KHR
            };

            if vk::Result::ERROR_OUT_OF_DATE_KHR == present_result || vk::Result::SUBOPTIMAL_KHR == present_result {
                self.set_need_recreate_swapchain(true);
            }

            self._frame_index = (self._frame_index + 1) % (constants::MAX_FRAME_COUNT as i32);
        }
    }

    pub fn rendering_at_first(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        let taa_resolve_framebuffer = Some(&self._post_process_data_taa._taa_resolve_framebuffer_data);
        let taa_push_constans_data = PushConstant_RenderColor {
            _color: Vector4::new(0.0, 0.0, 0.0, 0.0),
        };
        let taa_push_constants = Some(&taa_push_constans_data);
        self.render_material_instance(command_buffer, swapchain_index, "render_color", &quad_geometry_data, taa_resolve_framebuffer, None, taa_push_constants);
    }

    pub fn render_solid_object(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_mode: RenderMode,
        render_object_type: RenderObjectType,
        render_elements: &Vec<RenderElementData>
    ) {
        if 0 == render_elements.len() {
            return;
        }

        unsafe {
            let render_pass_pipeline_data_name = match (render_mode, render_object_type) {
                (RenderMode::RenderMode_Common, RenderObjectType::Static) => "render_pass_static_opaque/render_object",
                (RenderMode::RenderMode_Common, RenderObjectType::Skeletal) => "render_pass_skeletal_opaque/render_object",
                (RenderMode::RenderMode_Shadow, RenderObjectType::Static) => "render_pass_static_shadow/render_object",
                (RenderMode::RenderMode_Shadow, RenderObjectType::Skeletal) => "render_pass_skeletal_shadow/render_object",
            };

            let mut bone_metrices_offset: vk::DeviceSize = 0;
            let mut material_instance_data: *const MaterialInstanceData = std::ptr::null();
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();

            if RenderMode::RenderMode_Shadow == render_mode {
                let resources = self._resources.borrow();
                let material_instance_name = match render_object_type {
                    RenderObjectType::Static => "render_static_shadow",
                    RenderObjectType::Skeletal => "render_skeletal_shadow",
                };
                material_instance_data = resources.get_material_instance_data(&material_instance_name).as_ptr();
            }

            for render_element in render_elements.iter() {
                let render_object = render_element._render_object.borrow();
                if RenderMode::RenderMode_Common == render_mode {
                    material_instance_data = render_element._material_instance_data.as_ptr();
                }

                let pipeline_binding_data: *const PipelineBindingData = (*material_instance_data).get_pipeline_binding_data(&render_pass_pipeline_data_name);
                let render_pass_data = &(*pipeline_binding_data)._render_pass_pipeline_data._render_pass_data;
                let pipeline_data = &(*pipeline_binding_data)._render_pass_pipeline_data._pipeline_data;
                let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();

                if prev_pipeline_data != pipeline_data_ptr {
                    prev_pipeline_data = pipeline_data_ptr;
                    self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
                }

                if prev_pipeline_binding_data != pipeline_binding_data {
                    prev_pipeline_binding_data = pipeline_binding_data;
                    self.bind_descriptor_sets(command_buffer, swapchain_index, &(*pipeline_binding_data), None);
                }

                match render_object_type {
                    RenderObjectType::Static => {
                        self.upload_push_constant_data(
                            command_buffer,
                            &pipeline_data.borrow(),
                            &PushConstant_StaticRenderObject {
                                _local_matrix: render_object._transform_object.get_matrix().clone() as Matrix4<f32>
                            }
                        );
                    },
                    RenderObjectType::Skeletal => {
                        let prev_animation_buffer: &Vec<Matrix4<f32>> = render_object.get_prev_animation_buffer(0);
                        let animation_buffer: &Vec<Matrix4<f32>> = render_object.get_animation_buffer(0);
                        let bone_count = prev_animation_buffer.len() as vk::DeviceSize;
                        let prev_animation_buffer_offset = bone_metrices_offset * std::mem::size_of::<Matrix4<f32>>() as vk::DeviceSize;
                        let animation_buffer_offset = (bone_metrices_offset + bone_count) * std::mem::size_of::<Matrix4<f32>>() as vk::DeviceSize;
                        self.upload_shader_buffer_datas_offset(swapchain_index, ShaderBufferDataType::BoneMatrices, &prev_animation_buffer, prev_animation_buffer_offset);
                        self.upload_shader_buffer_datas_offset(swapchain_index, ShaderBufferDataType::BoneMatrices, &animation_buffer, animation_buffer_offset);
                        self.upload_push_constant_data(
                            command_buffer,
                            &pipeline_data.borrow(),
                            &PushConstant_SkeletalRenderObject {
                                _local_matrix: render_object._transform_object.get_matrix().clone() as Matrix4<f32>,
                                _bone_matrix_offset: bone_metrices_offset as u32,
                                _bone_matrix_count: bone_count as u32,
                                ..Default::default()
                            }
                        );
                        bone_metrices_offset += bone_count * 2;
                    },
                };
                self.draw_elements(command_buffer, &render_element._geometry_data.borrow());
            }
            self.end_render_pass(command_buffer);
        }
    }

    pub fn render_taa(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        let resources: Ref<Resources> = self._resources.borrow();

        // render_taa
        self.render_material_instance(command_buffer, swapchain_index, "render_taa", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // copy SceneColorCopy -> TAAResolve
        {
            let render_copy_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_copy").borrow();
            let pipeline_binding_data = render_copy_material_instance_data.get_default_pipeline_binding_data();
            let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
            let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
            let framebuffer = Some(&self._post_process_data_taa._taa_resolve_framebuffer_data);
            let descriptor_sets = Some(&self._post_process_data_taa._taa_descriptor_sets);
            self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, framebuffer);
            self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            self.upload_push_constant_data(
                command_buffer,
                &pipeline_data.borrow(),
                &PushConstant_RenderCopy {
                    _taget_mip_level: 0,
                    ..Default::default()
                },
            );
            self.draw_elements(command_buffer, quad_geometry_data);
            self.end_render_pass(command_buffer);
        }
    }

    pub fn render_bloom(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let render_bloom_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_bloom").borrow();
        let render_gaussian_blur_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_gaussian_blur").borrow();
        // render_bloom_highlight
        {
            let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
            let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
            let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
            self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
            self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, None);
            self.upload_push_constant_data(
                command_buffer,
                &pipeline_data.borrow(),
                &self._post_process_data_bloom._bloom_push_constants,
            );
            self.draw_elements(command_buffer, quad_geometry_data);
            self.end_render_pass(command_buffer);
        }
        // render_bloom_downsampling
        {
            let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
            let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
            let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
            let framebuffer_count = self._post_process_data_bloom._bloom_downsample_framebuffer_datas.len();
            for i in 0..framebuffer_count {
                let framebuffer = Some(&self._post_process_data_bloom._bloom_downsample_framebuffer_datas[i]);
                let descriptor_sets = Some(&self._post_process_data_bloom._bloom_downsample_descriptor_sets[i]);
                self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, framebuffer);
                self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
                self.draw_elements(command_buffer, quad_geometry_data);
                self.end_render_pass(command_buffer);
            }
        }
        // render_gaussian_blur
        {
            let pipeline_binding_data = render_gaussian_blur_material_instance_data.get_default_pipeline_binding_data();
            let render_pass_data = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data;
            let pipeline_data = &pipeline_binding_data._render_pass_pipeline_data._pipeline_data;
            let framebuffer_count = self._post_process_data_bloom._bloom_temp_framebuffer_datas.len();
            for i in 0..framebuffer_count {
                let framebuffer = Some(&self._post_process_data_bloom._bloom_temp_framebuffer_datas[i]);
                let descriptor_sets = Some(&self._post_process_data_bloom._bloom_temp_descriptor_sets[i]);
                self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, framebuffer);
                self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
                self.upload_push_constant_data(
                    command_buffer,
                    &pipeline_data.borrow(),
                    &PushConstant_GaussianBlur {
                        _blur_scale: if 0 == (i % 2) {
                            Vector2::new(1.0, 0.0)
                        } else {
                            Vector2::new(0.0, 1.0)
                        },
                        ..Default::default()
                    },
                );
                self.draw_elements(command_buffer, quad_geometry_data);
                self.end_render_pass(command_buffer);
            }
        }
    }

    pub fn render_ssao(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // render ssao
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao", quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        let framebuffer_h = Some(&self._post_process_data_ssao._ssao_blur_framebuffer_data0);
        let descriptor_sets_h = Some(&self._post_process_data_ssao._ssao_blur_descriptor_sets0);
        let framebuffer_v = Some(&self._post_process_data_ssao._ssao_blur_framebuffer_data1);
        let descriptor_sets_v = Some(&self._post_process_data_ssao._ssao_blur_descriptor_sets1);
        let push_constants_blur_h = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 0.0),
            ..Default::default()
        };
        let push_constants_blur_v = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(0.0, 1.0),
            ..Default::default()
        };
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", quad_geometry_data, framebuffer_h, descriptor_sets_h, Some(&push_constants_blur_h));
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", quad_geometry_data, framebuffer_v, descriptor_sets_v, Some(&push_constants_blur_v));
    }

    pub fn render_pre_process(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // SSAO
        self.render_ssao(command_buffer, swapchain_index, quad_geometry_data);

        // Composite GBuffer
        self.render_material_instance(command_buffer, swapchain_index, "composite_gbuffer", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
    }

    pub fn render_post_process(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // TAA
        self.render_taa(command_buffer, swapchain_index, quad_geometry_data);

        // Bloom
        self.render_bloom(command_buffer, swapchain_index, quad_geometry_data);

        // Motion Blur
        self.render_material_instance(command_buffer, swapchain_index, "render_motion_blur", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
    }
}