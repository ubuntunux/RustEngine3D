use std::cmp::{min, max};
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
use winit::window::{ Window };
use nalgebra::{ Vector2, Vector4, Matrix4 };

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
use crate::vulkan_context::vulkan_context::{ self, RenderFeatures, SwapchainArray, FrameArray, MipLevels };
use crate::renderer::fft_ocean::FFTOcean;
use crate::renderer::image_sampler::{ self, ImageSamplerData };
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::push_constants::{
    NONE_PUSH_CONSTANT,
    PushConstant_StaticRenderObject,
    PushConstant_SkeletalRenderObject,
    PushConstant_GaussianBlur,
    PushConstant_RenderCopy,
    PushConstant_RenderDebug,
    PushConstant_BlendCubeMap,
};
use crate::renderer::precomputed_atmosphere::PushConstant_Atmosphere;
use crate::renderer::camera::CameraObjectData;
use crate::renderer::render_target::{ self, RenderTargetType };
use crate::renderer::shader_buffer_datas::{
    self,
    ShaderBufferDataType,
    ShaderBufferDataMap,
};
use crate::renderer::renderer_data::{
    RendererData_Bloom,
    RendererData_SSAO,
    RendererData_TAA,
    RendererData_HierachicalMinZ,
    RendererData_SceneColorDownSampling,
    RendererData_SSR,
    RendererData_CompositeGBuffer,
    RendererData_ClearRenderTargets,
    RendererData_LightProbe,
};
use crate::renderer::render_element::{ RenderElementData };
use crate::resource::{ Resources };
use crate::utilities::system::{ self, RcRefCell, enum_to_string };
use crate::renderer::font::TextRenderData;

pub type RenderTargetDataMap = HashMap<RenderTargetType, TextureData>;

pub const DEFAULT_PIPELINE: &str = "";

// NOTE : RenderMode must match with scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RenderMode {
    GBuffer = 0,
    Forward = 1,
    Shadow = 2,
    CaptureHeightMap = 3,
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
    _is_first_rendering: bool,
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
    pub _debug_util_interface: Option<DebugUtils>,
    pub _debug_call_back: vk::DebugUtilsMessengerEXT,
    pub _image_available_semaphores: FrameArray<vk::Semaphore>,
    pub _render_finished_semaphores: FrameArray<vk::Semaphore>,
    pub _queue_family_datas: queue::QueueFamilyDatas,
    pub _frame_fences: Vec<vk::Fence>,
    pub _command_pool: vk::CommandPool,
    pub _command_buffers: SwapchainArray<vk::CommandBuffer>,
    pub _render_features: RenderFeatures,
    pub _image_samplers: ImageSamplerData,
    pub _scene_constants: shader_buffer_datas::SceneConstants,
    pub _view_constants: shader_buffer_datas::ViewConstants,
    pub _debug_render_target: RenderTargetType,
    pub _debug_render_target_layer: u32,
    pub _debug_render_target_miplevel: u32,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _shader_buffer_data_map: ShaderBufferDataMap,
    pub _renderer_data_bloom: RendererData_Bloom,
    pub _renderer_data_ssao: RendererData_SSAO,
    pub _renderer_data_taa: RendererData_TAA,
    pub _renderer_data_hiz: RendererData_HierachicalMinZ,
    pub _scene_color_downsampling: RendererData_SceneColorDownSampling,
    pub _renderer_data_ssr: RendererData_SSR,
    pub _renderer_data_composite_gbuffer: RendererData_CompositeGBuffer,
    pub _clear_render_targets: RendererData_ClearRenderTargets,
    pub _light_probe_datas: RendererData_LightProbe,
    pub _resources: RcRefCell<Resources>
}

pub fn create_renderer_data(
    app_name: &str,
    app_version: u32,
    (window_width, window_height): (u32, u32),
    window: &Window,
    resources: RcRefCell<Resources>
) -> RcRefCell<RendererData> {
    unsafe {
        log::info!("create_renderer_data: {}, width: {}, height: {}", constants::ENGINE_NAME, window_width, window_height);
        let entry = Entry::new().unwrap();
        let surface_extensions = ash_window::enumerate_required_extensions(window).unwrap();
        let instance: Instance = device::create_vk_instance(&entry, &app_name, app_version, &surface_extensions);
        let surface = device::create_vk_surface(&entry, &instance, window);
        let surface_interface = Surface::new(&entry, &instance);
        let (physical_device, swapchain_support_details, physical_device_features) = device::select_physical_device(&instance, &surface_interface, surface).unwrap();
        let device_properties: vk::PhysicalDeviceProperties = instance.get_physical_device_properties(physical_device);
        let device_memory_properties: vk::PhysicalDeviceMemoryProperties = instance.get_physical_device_memory_properties(physical_device);

        log::info!("PhysicalDeviceProperties");
        log::info!("    vulakn api_version: {}.{}.{}", vk::version_major(device_properties.api_version), vk::version_minor(device_properties.api_version), vk::version_patch(device_properties.api_version));
        log::info!("    driver_version: {}.{}.{}", vk::version_major(device_properties.driver_version), vk::version_minor(device_properties.driver_version), vk::version_patch(device_properties.driver_version));
        let device_name = CStr::from_ptr(device_properties.device_name.as_ptr() as *const std::os::raw::c_char);
        log::info!("    device: {:?} {:?} vecdor_id: {:?} device_id: {:?}", device_name, device_properties.device_type, device_properties.vendor_id, device_properties.device_id);

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
        let debug_call_back: vk::DebugUtilsMessengerEXT;
        let debug_util_interface: Option<DebugUtils>;
        if constants::ENABLE_VALIDATION_LAYER {
            let debug_message_level = get_debug_message_level(constants::DEBUG_MESSAGE_LEVEL);
            let debug_info = vk::DebugUtilsMessengerCreateInfoEXT {
                message_severity: debug_message_level,
                message_type: vk::DebugUtilsMessageTypeFlagsEXT::all(),
                pfn_user_callback: Some(vulkan_debug_callback),
                ..Default::default()
            };
            debug_util_interface = Some(DebugUtils::new(&entry, &instance));
            debug_call_back = debug_util_interface.as_ref().unwrap().create_debug_utils_messenger(&debug_info, None).unwrap();
        } else {
            debug_util_interface = None;
            debug_call_back = vk::DebugUtilsMessengerEXT::null();
        }

        let mut renderer_data = RendererData {
            _frame_index: 0,
            _swapchain_index: 0,
            _need_recreate_swapchain: false,
            _is_first_rendering: true,
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
            _scene_constants: shader_buffer_datas::SceneConstants::default(),
            _view_constants: shader_buffer_datas::ViewConstants::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _debug_render_target_layer: 0,
            _debug_render_target_miplevel: 0,
            _render_target_data_map: RenderTargetDataMap::new(),
            _shader_buffer_data_map: ShaderBufferDataMap::new(),
            _renderer_data_bloom: RendererData_Bloom::default(),
            _renderer_data_ssao: RendererData_SSAO::default(),
            _renderer_data_taa: RendererData_TAA::default(),
            _renderer_data_hiz: RendererData_HierachicalMinZ::default(),
            _scene_color_downsampling: RendererData_SceneColorDownSampling::default(),
            _renderer_data_ssr: RendererData_SSR::default(),
            _renderer_data_composite_gbuffer: RendererData_CompositeGBuffer::default(),
            _clear_render_targets: RendererData_ClearRenderTargets::default(),
            _light_probe_datas: RendererData_LightProbe::default(),
            _resources: resources.clone(),
        };

        renderer_data.initialize_renderer();

        system::newRcRefCell(renderer_data)
    }
}

impl RendererData {
    pub fn get_need_recreate_swapchain(&self) -> bool { self._need_recreate_swapchain }
    pub fn set_need_recreate_swapchain(&mut self, value: bool) { self._need_recreate_swapchain = value; }
    pub fn reset_is_first_rendering(&mut self) { self._is_first_rendering = true; }
    pub fn get_instance(&self) -> &Instance { &self._instance }
    pub fn get_device(&self) -> &Device { &self._device }
    pub fn get_device_properties(&self) -> &vk::PhysicalDeviceProperties { &self._device_properties }
    pub fn get_device_memory_properties(&self) -> &vk::PhysicalDeviceMemoryProperties { &self._device_memory_properties }
    pub fn get_physical_device(&self) -> vk::PhysicalDevice { self._physical_device }
    pub fn get_swap_chain_data(&self) -> &SwapchainData { &self._swapchain_data }
    pub fn get_swap_chain_image_views(&self) -> &SwapchainArray<vk::ImageView> { &self._swapchain_data._swapchain_image_views }
    pub fn get_swap_chain_support_details(&self) -> &swapchain::SwapchainSupportDetails { &self._swapchain_support_details }
    pub fn get_swap_chain_index(&self) -> u32 { self._swapchain_index }
    pub fn get_command_pool(&self) -> vk::CommandPool { self._command_pool }
    pub fn get_command_buffers(&self) -> &SwapchainArray<vk::CommandBuffer> { &self._command_buffers }
    pub fn get_command_buffer(&self, index: usize) -> vk::CommandBuffer { self._command_buffers[index] }
    pub fn get_current_command_buffer(&self) -> vk::CommandBuffer { self._command_buffers[self._swapchain_index as usize] }
    pub fn get_graphics_queue(&self) -> vk::Queue { self._queue_family_datas._graphics_queue }
    pub fn get_present_queue(&self) -> vk::Queue { self._queue_family_datas._present_queue }
    pub fn get_shader_buffer_data(&self, buffer_data_type: ShaderBufferDataType) -> &ShaderBufferData {
        &self._shader_buffer_data_map.get(&buffer_data_type).unwrap()
    }

    pub fn prepare_framebuffer_and_descriptors(&mut self) {
        // Bloom
        self._renderer_data_bloom.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp0).as_ref().unwrap(),
        );
        // Temporal AA
        self._renderer_data_taa.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(),
        );
        // SSAO
        self._renderer_data_ssao.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SSAO).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSAOTemp).as_ref().unwrap(),
        );
        // Hierachical Min Z
        self._renderer_data_hiz.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(),
        );
        // SceneColor Downsampling
        self._scene_color_downsampling.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(),
        );
        // SSR
        self._renderer_data_ssr.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
        );
        // Composite GBuffer
        self._renderer_data_composite_gbuffer.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
        );
        // Clear Render Targets
        self._clear_render_targets.initialize(
            &self._device,
            &self._resources,
            &[
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySky).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySkyPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForward).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForwardPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_A).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_B).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::Shadow).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
            ]
        );
        // RendererData_LightProbe
        self._light_probe_datas.initialize(
            &self._device,
            &self._resources,
            self._render_target_data_map.get(&RenderTargetType::LightProbeColor).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySky).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySkyPrev).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorForward).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorForwardPrev).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeAtmosphereColor).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeAtmosphereInscatter).as_ref().unwrap(),
            &[
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants0).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants1).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants2).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants3).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants4).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants5).as_ref().unwrap(),
            ]
        );
    }

    pub fn destroy_framebuffer_and_descriptors(&mut self) {
        self._renderer_data_bloom.destroy(&self._device);
        self._renderer_data_taa.destroy(&self._device);
        self._renderer_data_ssao.destroy(&self._device);
        self._renderer_data_hiz.destroy(&self._device);
        self._scene_color_downsampling.destroy(&self._device);
        self._renderer_data_ssr.destroy(&self._device);
        self._renderer_data_composite_gbuffer.destroy(&self._device);
        self._clear_render_targets.destroy(&self._device);
        self._light_probe_datas.destroy(&self._device);
    }

    pub fn update_post_process_datas(&mut self) {
        self._renderer_data_ssr.update();
    }

    pub fn next_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let next_enum_value: i32 = self._debug_render_target as i32 + 1;
        const MAX_BOUND: i32 = RenderTargetType::MaxBound  as i32;
        self._debug_render_target = if next_enum_value < MAX_BOUND {
            unsafe { std::mem::transmute(next_enum_value) }
        } else {
            unsafe { std::mem::transmute(0) }
        };
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn prev_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let enum_to_int: i32 = self._debug_render_target as i32;
        self._debug_render_target = if 0 == enum_to_int {
            unsafe { std::mem::transmute(RenderTargetType::MaxBound as i32 - 1) }
        } else {
            unsafe { std::mem::transmute(enum_to_int - 1) }
        };
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn next_debug_render_target_miplevel(&mut self) {
        let texture_data: &TextureData = self.get_render_target(self._debug_render_target);
        self._debug_render_target_miplevel = min(texture_data._image_mip_levels, self._debug_render_target_miplevel + 1);
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn prev_debug_render_target_miplevel(&mut self) {
        if 0 < self._debug_render_target_miplevel {
            self._debug_render_target_miplevel -= 1;
        }
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
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

    pub fn create_render_target<T: Copy>(&self, texture_create_info: &TextureCreateInfo<T>) -> TextureData {
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
            if self._debug_util_interface.is_some() {
                self._debug_util_interface.as_ref().unwrap().destroy_debug_utils_messenger(self._debug_call_back, None);
            }
            device::destroy_vk_instance(&self._instance);
        }
    }

    pub fn dispatch_material_instance<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        material_instance_name: &str,
        render_pass_pipeline_data_name: &str,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&T>,
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = if render_pass_pipeline_data_name.is_empty() {
            material_instance_data.get_default_pipeline_binding_data()
        } else {
            material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name)
        };
        self.dispatch_render_pass_pipeline(command_buffer, swapchain_index, pipeline_binding_data, group_count_x, group_count_y, group_count_z, custom_descriptor_sets, push_constant_data);
    }

    pub fn dispatch_render_pass_pipeline<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&T>,
    ) {
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        self.begin_compute_pipeline(command_buffer, pipeline_data);
        self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, custom_descriptor_sets);
        if let Some(push_constant_data) = push_constant_data {
            self.upload_push_constant_data(
                command_buffer,
                pipeline_data,
                push_constant_data
            );
        }
        self.dispatch_compute_pipeline(command_buffer, group_count_x, group_count_y, group_count_z);
    }

    pub fn render_material_instance<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        material_instance_name: &str,
        render_pass_pipeline_data_name: &str,
        geometry_data: &GeometryData,
        custom_framebuffer_data: Option<&FramebufferData>,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&T>,
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = if render_pass_pipeline_data_name.is_empty() {
            material_instance_data.get_default_pipeline_binding_data()
        } else {
            material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name)
        };
        self.render_render_pass_pipeline(command_buffer, swapchain_index, pipeline_binding_data, geometry_data, custom_framebuffer_data, custom_descriptor_sets, push_constant_data);
    }

    pub fn render_render_pass_pipeline<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        geometry_data: &GeometryData,
        custom_framebuffer_data: Option<&FramebufferData>,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&T>,
    ) {
        let render_pass_data = &pipeline_binding_data.get_render_pass_data().borrow();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        self.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, custom_framebuffer_data);
        self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, custom_descriptor_sets);
        if let Some(push_constant_data) = push_constant_data {
            self.upload_push_constant_data(
                command_buffer,
                pipeline_data,
                push_constant_data
            );
        }
        self.draw_elements(command_buffer, geometry_data);
        self.end_render_pass(command_buffer);
    }

    pub fn begin_compute_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        pipeline_data: &PipelineData,
    ) {
        unsafe {
            self._device.cmd_bind_pipeline(command_buffer, pipeline_data._pipeline_bind_point, pipeline_data._pipeline);
        }
    }

    pub fn begin_render_pass_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_pass_data: &RenderPassData,
        pipeline_data: &PipelineData,
        custom_framebuffer: Option<&FramebufferData>,
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let framebuffer_data: *const FramebufferData = match custom_framebuffer {
            Some(custom_framebuffer) => custom_framebuffer,
            None => resources.get_framebuffer_data(render_pass_data.get_render_pass_data_name().as_str()).as_ptr()
        };
        unsafe {
            let render_pass_begin_info = (*framebuffer_data)._render_pass_begin_infos[swapchain_index as usize];
            let pipeline_bind_point = pipeline_data._pipeline_bind_point;
            let pipeline_dynamic_states = &pipeline_data._pipeline_dynamic_states;
            self._device.cmd_begin_render_pass(command_buffer, &render_pass_begin_info, vk::SubpassContents::INLINE);

            if pipeline_dynamic_states.contains(&vk::DynamicState::VIEWPORT) {
                self._device.cmd_set_viewport(command_buffer, 0, &[(*framebuffer_data)._framebuffer_info._framebuffer_view_port]);
            }

            if pipeline_dynamic_states.contains(&vk::DynamicState::SCISSOR) {
                self._device.cmd_set_scissor(command_buffer, 0, &[(*framebuffer_data)._framebuffer_info._framebuffer_scissor_rect]);
            }

            self._device.cmd_bind_pipeline(command_buffer, pipeline_bind_point, pipeline_data._pipeline);
        }
    }

    pub fn bind_descriptor_sets(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>) {
        let pipeline_layout = pipeline_binding_data.get_pipeline_layout();
        let pipeline_bind_point = pipeline_binding_data.get_pipeline_bind_point();
        let descriptor_sets: &SwapchainArray<vk::DescriptorSet> = match custom_descriptor_sets {
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
        descriptor_index: usize,
        descriptor_resource_info: &DescriptorResourceInfo
    ) {
        let wirte_descriptor_sets: &Vec<vk::WriteDescriptorSet> = &pipeline_binding_data._write_descriptor_sets[swapchain_index as usize];
        let write_descriptor_set = descriptor::create_write_descriptor_set(wirte_descriptor_sets, descriptor_index, descriptor_resource_info);
        let descriptor_copies: &[vk::CopyDescriptorSet] = &[];
        unsafe {
            self._device.update_descriptor_sets(&[write_descriptor_set], descriptor_copies);
        }
    }

    pub fn update_descriptor_set_mut(
        &self,
        swapchain_index: u32,
        pipeline_binding_data: &mut PipelineBindingData,
        descriptor_index: usize,
        descriptor_resource_info: &DescriptorResourceInfo
    ) {
        let wirte_descriptor_sets: &mut Vec<vk::WriteDescriptorSet> = &mut pipeline_binding_data._write_descriptor_sets[swapchain_index as usize];
        descriptor::update_write_descriptor_set(wirte_descriptor_sets, descriptor_index, descriptor_resource_info);
        let wirte_descriptor_set_offset = wirte_descriptor_sets[descriptor_index];
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

    pub fn dispatch_compute_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32
    ) {
        unsafe {
            self._device.cmd_dispatch(command_buffer, max(1, group_count_x), max(1, group_count_y), max(1, group_count_z));
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
        self.destroy_framebuffer_and_descriptors();
        resources.borrow_mut().unload_graphics_datas(self);
        self.destroy_render_targets();

        // recreate swapchain & graphics resources
        self.recreate_swapchain();
        self.create_render_targets();
        resources.borrow_mut().load_graphics_datas(self);
        self.prepare_framebuffer_and_descriptors();
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
            if let Err(present_error) = is_swapchain_suboptimal {
                log::debug!("present_error: {:?}", present_error);
            }

            // waiting
            match self._device.device_wait_idle() {
                Err(e) => {
                    log::debug!("device_wait_idle: {:?}", e);
                    return VkResult::Err(e)
                },
                _ => ()
            }

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

    pub fn render_scene(&mut self, scene_manager: RefMut<SceneManagerData>, elapsed_time: f64, delta_time: f64, elapsed_frame: u64) {
        unsafe {
            // frame index
            let frame_index = self._frame_index as usize;
            let frame_fence = self._frame_fences[frame_index];
            let image_available_semaphore = self._image_available_semaphores[frame_index];
            let render_finished_semaphore = self._render_finished_semaphores[frame_index];

            // Begin Render
            let acquire_next_image_result: VkResult<(u32, bool)> = self._swapchain_interface.acquire_next_image(
                self._swapchain_data._swapchain,
                std::u64::MAX,
                image_available_semaphore,
                vk::Fence::null()
            );

            let (swapchain_index, is_swapchain_suboptimal) = if acquire_next_image_result.is_ok() {
                acquire_next_image_result.unwrap()
            } else {
                (0, true)
            };

            self._swapchain_index = swapchain_index;

            let present_result: vk::Result = if swapchain_index < constants::SWAPCHAIN_IMAGE_COUNT as u32 && false == is_swapchain_suboptimal {
                let command_buffer = self._command_buffers[swapchain_index as usize];
                let resources = self._resources.borrow();
                let main_camera =  scene_manager.get_main_camera().borrow();
                let main_light = scene_manager.get_main_light().borrow();
                let mut capture_height_map = scene_manager.get_capture_height_map().borrow_mut();
                let render_capture_height_map: bool = capture_height_map.get_need_to_redraw_shadow_and_reset();
                let fft_ocean =  scene_manager.get_fft_ocean().borrow();
                let mut atmosphere =  scene_manager.get_atmosphere().borrow_mut();
                let quad_mesh = resources.get_mesh_data("quad").borrow();
                let quad_geometry_data: Ref<GeometryData> = quad_mesh.get_default_geometry_data().borrow();
                let static_render_elements = scene_manager.get_static_render_elements();
                let static_shadow_render_elements = scene_manager.get_static_shadow_render_elements();
                let skeletal_render_elements = scene_manager.get_skeletal_render_elements();
                let skeletal_shadow_render_elements = scene_manager.get_skeletal_shadow_render_elements();

                // Begin command buffer
                let command_buffer_begin_info = vk::CommandBufferBeginInfo {
                    flags: vk::CommandBufferUsageFlags::SIMULTANEOUS_USE,
                    ..Default::default()
                };
                self._device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("vkBeginCommandBuffer failed!");

                // Upload Uniform Buffers
                self._scene_constants.update_scene_constants(
                    self._swapchain_data._swapchain_extent.width,
                    self._swapchain_data._swapchain_extent.height,
                    elapsed_time,
                    delta_time,
                    fft_ocean.get_height(),
                );
                self._view_constants.update_view_constants(&main_camera);
                if render_capture_height_map {
                    self._view_constants._capture_height_map_view_projection = (*capture_height_map.get_shadow_view_projection()).into();
                }

                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::SceneConstants, &self._scene_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::ViewConstants, &self._view_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::LightConstants, main_light.get_light_constants());
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::SSAOConstants, &self._renderer_data_ssao._ssao_constants);
                self.upload_shader_buffer_data(swapchain_index, ShaderBufferDataType::AtmosphereConstants, &atmosphere._atmosphere_constants);

                if self._is_first_rendering {
                    self.rendering_at_first(command_buffer, swapchain_index, &quad_geometry_data);
                    fft_ocean.compute_slope_variance_texture(command_buffer, swapchain_index, &quad_geometry_data, self, &resources);
                    atmosphere.precompute(command_buffer, swapchain_index, &quad_geometry_data, self);
                }

                // clear gbuffer
                self.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_gbuffer/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

                // shadow
                self.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_shadow/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::Shadow, RenderObjectType::Static, &static_shadow_render_elements, None);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::Shadow, RenderObjectType::Skeletal, &skeletal_shadow_render_elements, None);

                // capture height map
                if render_capture_height_map {
                    self.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_capture_height_map/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
                    self.render_solid_object(command_buffer, swapchain_index, RenderMode::CaptureHeightMap, RenderObjectType::Static, &static_render_elements, None);
                }

                // fft-simulation
                fft_ocean.simulate_fft_waves(command_buffer, swapchain_index, &quad_geometry_data, self, &resources);

                // light probe
                if self._light_probe_datas._next_refresh_time <= elapsed_time {
                    self.render_light_probe(
                        command_buffer,
                        swapchain_index,
                        &quad_geometry_data,
                        &resources,
                        &scene_manager,
                        &main_camera,
                        static_render_elements,
                        &fft_ocean
                    );
                    self._light_probe_datas._next_refresh_time = elapsed_time + self._light_probe_datas._light_probe_refresh_term;
                    self._light_probe_datas._light_probe_blend_time = 0.0;
                }

                let light_probe_term = self._light_probe_datas._light_probe_blend_term.min(self._light_probe_datas._light_probe_refresh_term);
                if self._light_probe_datas._light_probe_blend_time < light_probe_term {
                    self._light_probe_datas._light_probe_blend_time += delta_time;
                    let blend_ratio: f64 = 1.0f64.min(self._light_probe_datas._light_probe_blend_time / light_probe_term);
                    self.copy_cube_map(
                        command_buffer,
                        swapchain_index,
                        &resources,
                        "copy_cube_map/blend",
                        if constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
                            &self._light_probe_datas._light_probe_blend_from_forward_descriptor_sets
                        } else {
                            &self._light_probe_datas._light_probe_blend_from_only_sky_descriptor_sets
                        },
                        constants::LIGHT_PROBE_SIZE,
                        Some(&PushConstant_BlendCubeMap {
                            _blend_ratio: blend_ratio as f32,
                            _reserved0: 0,
                            _reserved1: 0,
                            _reserved2: 0,
                        })
                    );
                }

                // render solid object
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::GBuffer, RenderObjectType::Static, &static_render_elements, None);
                self.render_solid_object(command_buffer, swapchain_index, RenderMode::GBuffer, RenderObjectType::Skeletal, &skeletal_render_elements, None);

                // pre-process: min-z, ssr, ssao, gbuffer, downsampling scnee color
                self.render_pre_process(command_buffer, swapchain_index, &quad_geometry_data);

                // render ocean
                fft_ocean.render_ocean(command_buffer, swapchain_index, self, &resources);

                // render atmosphere
                let render_light_probe_mode: bool = false;
                atmosphere.render_precomputed_atmosphere(command_buffer, swapchain_index, &quad_geometry_data, self, render_light_probe_mode);

                // post-process: taa, bloom, motion blur
                self.render_post_process(command_buffer, swapchain_index, &quad_geometry_data);

                // Render Final
                self.render_material_instance(command_buffer, swapchain_index, "render_final", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

                // Render Debug
                if RenderTargetType::BackBuffer != self._debug_render_target {
                    let render_debug_material_instance_name = "render_debug";
                    let mut render_debug_material_instance_data: RefMut<MaterialInstanceData> = resources.get_material_instance_data(&render_debug_material_instance_name).borrow_mut();
                    let mut render_debug_pipeline_binding_data = render_debug_material_instance_data.get_default_pipeline_binding_data_mut();
                    self.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &render_debug_pipeline_binding_data.get_render_pass_data().borrow(),
                        &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                        None,
                    );

                    let debug_texture_data = self.get_render_target(self._debug_render_target);
                    //let debug_texture_data = resources.get_texture_data("fft_ocean/butterfly").borrow();
                    let descriptor_index = match debug_texture_data.get_image_view_type() {
                        vk::ImageViewType::TYPE_2D => 1,
                        vk::ImageViewType::TYPE_2D_ARRAY => 2,
                        vk::ImageViewType::TYPE_3D => 3,
                        vk::ImageViewType::CUBE => 4,
                        _ => panic!("Not implemented."),
                    };
                    self.update_descriptor_set_mut(
                        swapchain_index,
                        &mut render_debug_pipeline_binding_data,
                        descriptor_index,
                        &DescriptorResourceInfo::DescriptorImageInfo(debug_texture_data.get_default_image_info()),
                    );

                    self.upload_push_constant_data(
                        command_buffer,
                        &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                        &PushConstant_RenderDebug {
                            _debug_target: debug_texture_data.get_image_view_type().as_raw() as u32,
                            _mip_level: self._debug_render_target_miplevel,
                            ..Default::default()
                        }
                    );

                    self.bind_descriptor_sets(command_buffer, swapchain_index, &render_debug_pipeline_binding_data, None);
                    self.draw_elements(command_buffer, &quad_geometry_data);
                    self.end_render_pass(command_buffer);
                }

                // End command buffer
                self._device.end_command_buffer(command_buffer).expect("vkEndCommandBuffer failed!");

                // End Render
                self._is_first_rendering = false;
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
        // Clear render targets
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("clear_render_target").borrow();
        for (_, framebuffers) in self._clear_render_targets._color_framebuffer_datas.iter() {
            let default_frame_buffer = &framebuffers[0][0];
            let mut render_pass_pipeline_name = String::from("clear");
            for attachment_format in default_frame_buffer._framebuffer_info._framebuffer_color_attachment_formats.iter() {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            for attachment_format in default_frame_buffer._framebuffer_info._framebuffer_depth_attachment_formats.iter() {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            render_pass_pipeline_name.push_str("/clear");
            let pipeline_binding_data = material_instance_data.get_pipeline_binding_data(&render_pass_pipeline_name);
            for layer in 0..framebuffers.len() {
                for mip_level in 0.. framebuffers[layer].len() {
                    self.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &pipeline_binding_data.get_render_pass_data().borrow(),
                        &pipeline_binding_data.get_pipeline_data().borrow(),
                        Some(&framebuffers[layer][mip_level])
                    );
                    self.end_render_pass(command_buffer);
                }
            }
        }
    }

    pub fn render_text(&self, text_render_data: &TextRenderData, offset_x: i32, offset_y: i32, canvas_width: u32, canvas_height: u32) {
        // if 0 < text_render_data.render_count:
        //     self.font_shader.use_program()
        //     self.font_shader.bind_material_instance()
        //     self.font_shader.bind_uniform_data("texture_font", text_render_data.font_data.texture)
        //     self.font_shader.bind_uniform_data("font_size", text_render_data.font_size)
        //     self.font_shader.bind_uniform_data("offset", (offset_x, offset_y))
        //     self.font_shader.bind_uniform_data("inv_canvas_size", (1.0 / canvas_width, 1.0 / canvas_height))
        //     self.font_shader.bind_uniform_data("count_of_side", text_render_data.font_data.count_of_side)
        //     self.postprocess.draw_elements_instanced(text_render_data.render_count, self.font_instance_buffer, [text_render_data.render_queue, ])
    }

    pub fn copy_cube_map<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        resources: &Ref<Resources>,
        render_pass_pipeline_data_name: &str,
        mip_level_descriptor_sets: &MipLevels<SwapchainArray<vk::DescriptorSet>>,
        image_width: u32,
        push_constant_data: Option<&T>,
    ) {
        let copy_cube_map_material_instance = resources.get_material_instance_data("copy_cube_map").borrow();
        let pipeline_binding_data = copy_cube_map_material_instance.get_pipeline_binding_data(render_pass_pipeline_data_name);
        let pipeline_data = pipeline_binding_data.get_pipeline_data().borrow();
        self.begin_compute_pipeline(command_buffer, &pipeline_data);
        let mip_levels = mip_level_descriptor_sets.len();
        for mip_level in 0..mip_levels {
            if let Some(push_constant_data) = push_constant_data {
                self.upload_push_constant_data(
                    command_buffer,
                    &pipeline_data,
                    push_constant_data
                );
            }
            let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
            self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            let dispatch_count = image_width >> mip_level;
            self.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
        }
    }

    pub fn render_light_probe(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        resources: &Ref<Resources>,
        scene_manager: &RefMut<SceneManagerData>,
        main_camera: &CameraObjectData,
        static_render_elements: &Vec<RenderElementData>,
        fft_ocean: &FFTOcean,
    ) {
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("precomputed_atmosphere").borrow();
        let render_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_atmosphere/default");
        let composite_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("composite_atmosphere/default");
        let downsampling_material_instance = resources.get_material_instance_data("downsampling").borrow();
        let downsampling_pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let mut light_probe_view_constants = shader_buffer_datas::ViewConstants::default();
        let light_probe_view_constant_types = [
            ShaderBufferDataType::LightProbeViewConstants0,
            ShaderBufferDataType::LightProbeViewConstants1,
            ShaderBufferDataType::LightProbeViewConstants2,
            ShaderBufferDataType::LightProbeViewConstants3,
            ShaderBufferDataType::LightProbeViewConstants4,
            ShaderBufferDataType::LightProbeViewConstants5,
        ];
        let render_atmosphere_push_constants = PushConstant_Atmosphere {
            _render_light_probe_mode: 1,
            ..Default::default()
        };
        let main_camera_position = main_camera._transform_object.get_position();

        // copy only_sky to only_sky_prev
        self.copy_cube_map(
            command_buffer,
            swapchain_index,
            resources,
            "copy_cube_map/copy",
            &self._light_probe_datas._only_sky_copy_descriptor_sets,
            constants::LIGHT_PROBE_SIZE,
            NONE_PUSH_CONSTANT
        );

        // render atmosphere, inscatter
        for i in 0..constants::CUBE_LAYER_COUNT {
            let mut light_probe_camera = scene_manager.get_light_probe_camera(i).borrow_mut();
            light_probe_camera._transform_object.set_position(main_camera_position);
            light_probe_camera.update_camera_object_data();
            light_probe_view_constants.update_view_constants(&light_probe_camera);
            self.upload_shader_buffer_data(swapchain_index, light_probe_view_constant_types[i].clone(), &light_probe_view_constants);

            // render atmosphere
            self.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                render_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._light_probe_datas._render_atmosphere_framebuffer_datas[i]),
                Some(&self._light_probe_datas._render_atmosphere_descriptor_sets[i]),
                Some(&render_atmosphere_push_constants)
            );

            // composite atmosphere for only sky
            self.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                composite_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._light_probe_datas._composite_atmosphere_framebuffer_datas_only_sky[i]),
                Some(&self._light_probe_datas._composite_atmosphere_descriptor_sets[i]),
                NONE_PUSH_CONSTANT,
            );

            // downsampling for only sky
            self.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
            let mip_level_descriptor_sets = &self._light_probe_datas._only_sky_downsampling_descriptor_sets[i];
            let mip_levels = mip_level_descriptor_sets.len();
            for mip_level in 0..mip_levels {
                let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                self.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                self.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
            }
        }

        // render static object for light probe
        if constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
            // copy light_probe_forward to light_probe_forward_prev
            self.copy_cube_map(
                command_buffer,
                swapchain_index,
                resources,
                "copy_cube_map/copy",
                &self._light_probe_datas._light_probe_forward_copy_descriptor_sets,
                constants::LIGHT_PROBE_SIZE,
                NONE_PUSH_CONSTANT
            );

            for i in 0..constants::CUBE_LAYER_COUNT {
                // clear light probe depth
                const CLEAR_LIGHT_PROBE_PIPELINES: [&str; 6] = [
                    "clear_light_probe_depth_0/clear",
                    "clear_light_probe_depth_1/clear",
                    "clear_light_probe_depth_2/clear",
                    "clear_light_probe_depth_3/clear",
                    "clear_light_probe_depth_4/clear",
                    "clear_light_probe_depth_5/clear",
                ];
                self.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", CLEAR_LIGHT_PROBE_PIPELINES[i], &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

                // composite atmosphere
                self.render_render_pass_pipeline(
                    command_buffer,
                    swapchain_index,
                    composite_atmosphere_pipeline_binding_data,
                    quad_geometry_data,
                    Some(&self._light_probe_datas._composite_atmosphere_framebuffer_datas[i]),
                    Some(&self._light_probe_datas._composite_atmosphere_descriptor_sets[i]),
                    NONE_PUSH_CONSTANT,
                );

                // render forward for light probe
                const RENDER_FORWARD_RENDER_PASS_PIPELINE_NAMES: [&str; 6] = [
                    "render_pass_static_forward_light_probe_0/render_object",
                    "render_pass_static_forward_light_probe_1/render_object",
                    "render_pass_static_forward_light_probe_2/render_object",
                    "render_pass_static_forward_light_probe_3/render_object",
                    "render_pass_static_forward_light_probe_4/render_object",
                    "render_pass_static_forward_light_probe_5/render_object",
                ];
                self.render_solid_object(
                    command_buffer,
                    swapchain_index,
                    RenderMode::Forward,
                    RenderObjectType::Static,
                    static_render_elements,
                    Some(RENDER_FORWARD_RENDER_PASS_PIPELINE_NAMES[i])
                );

                // downsampling light probe
                self.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
                let mip_level_descriptor_sets = &self._light_probe_datas._light_probe_downsampling_descriptor_sets[i];
                let mip_levels = mip_level_descriptor_sets.len();
                for mip_level in 0..mip_levels {
                    let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                    self.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                    let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                    self.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
                }
            }
        }
    }

    pub fn render_solid_object(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_mode: RenderMode,
        render_object_type: RenderObjectType,
        render_elements: &Vec<RenderElementData>,
        custom_render_pass_pipeline_name: Option<&str>,
    ) {
        if 0 == render_elements.len() {
            return;
        }

        unsafe {
            let render_pass_pipeline_data_name: &str = if custom_render_pass_pipeline_name.is_some() {
                custom_render_pass_pipeline_name.unwrap()
            } else {
                match (render_mode, render_object_type) {
                    (RenderMode::GBuffer, RenderObjectType::Static) => "render_pass_static_gbuffer/render_object",
                    (RenderMode::Forward, RenderObjectType::Static) => "render_pass_static_forward/render_object",
                    (RenderMode::Shadow, RenderObjectType::Static) => "render_pass_static_shadow/render_object",
                    (RenderMode::CaptureHeightMap, RenderObjectType::Static) => "capture_static_height_map/render_object",
                    (RenderMode::GBuffer, RenderObjectType::Skeletal) => "render_pass_skeletal_gbuffer/render_object",
                    (RenderMode::Forward, RenderObjectType::Skeletal) => "render_pass_skeletal_forward/render_object",
                    (RenderMode::Shadow, RenderObjectType::Skeletal) => "render_pass_skeletal_shadow/render_object",
                    (RenderMode::CaptureHeightMap, RenderObjectType::Skeletal) => "capture_skeletal_height_map/render_object",
                    _ => panic!("Not implemented.")
                }
            };

            let mut bone_metrices_offset: vk::DeviceSize = 0;
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();

            for render_element in render_elements.iter() {
                let render_object = render_element._render_object.borrow();
                let pipeline_binding_data: *const PipelineBindingData = render_element._material_instance_data.borrow().get_pipeline_binding_data(&render_pass_pipeline_data_name);
                let render_pass_data = &(*pipeline_binding_data).get_render_pass_data().borrow();
                let pipeline_data = (*pipeline_binding_data).get_pipeline_data();
                let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();
                let pipeline_data: &PipelineData = &pipeline_data.borrow();

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
                            pipeline_data,
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
                            pipeline_data,
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
        // render_taa
        self.render_material_instance(command_buffer, swapchain_index, "render_taa", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // copy SceneColorCopy -> TAAResolve
        let framebuffer = Some(&self._renderer_data_taa._taa_resolve_framebuffer_data);
        let descriptor_sets = Some(&self._renderer_data_taa._taa_descriptor_sets);
        let push_constants = PushConstant_RenderCopy::default();
        self.render_material_instance(command_buffer, swapchain_index, "render_copy", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, Some(&push_constants));
    }

    pub fn render_bloom(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        let resources: Ref<Resources> = self._resources.borrow();
        let render_bloom_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_bloom").borrow();
        // render_bloom_highlight
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
        self.render_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            quad_geometry_data,
            None,
            None,
            Some(&self._renderer_data_bloom._bloom_push_constants)
        );

        // render_bloom_downsampling
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let framebuffer_count = self._renderer_data_bloom._bloom_downsample_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            self.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._renderer_data_bloom._bloom_downsample_framebuffer_datas[i]),
                Some(&self._renderer_data_bloom._bloom_downsample_descriptor_sets[i]),
                NONE_PUSH_CONSTANT
            );
        }

        // render_gaussian_blur
        let render_gaussian_blur_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance_data.get_default_pipeline_binding_data();
        let framebuffer_count = self._renderer_data_bloom._bloom_temp_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            self.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._renderer_data_bloom._bloom_temp_framebuffer_datas[i]),
                Some(&self._renderer_data_bloom._bloom_temp_descriptor_sets[i]),
                Some(&PushConstant_GaussianBlur {
                    _blur_scale: if 0 == (i % 2) {
                        Vector2::new(1.0, 0.0)
                    } else {
                        Vector2::new(0.0, 1.0)
                    },
                    ..Default::default()
                })
            );
        }
    }

    pub fn render_ssr(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // Screen Space Reflection
        self.render_material_instance(command_buffer, swapchain_index, "render_ssr", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // Screen Space Reflection Resolve
        let (framebuffer, descriptor_sets) = match self._renderer_data_ssr._current_ssr_resolved {
            RenderTargetType::SSRResolved => (Some(&self._renderer_data_ssr._framebuffer_data0), Some(&self._renderer_data_ssr._descriptor_sets0)),
            RenderTargetType::SSRResolvedPrev => (Some(&self._renderer_data_ssr._framebuffer_data1), Some(&self._renderer_data_ssr._descriptor_sets1)),
            _ => panic!("error")
        };
        self.render_material_instance(command_buffer, swapchain_index, "render_ssr_resolve", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, NONE_PUSH_CONSTANT);
    }

    pub fn render_ssao(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // render ssao
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao", DEFAULT_PIPELINE, quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // render ssao blur
        let framebuffer_h = Some(&self._renderer_data_ssao._ssao_blur_framebuffer_data0);
        let descriptor_sets_h = Some(&self._renderer_data_ssao._ssao_blur_descriptor_sets0);
        let framebuffer_v = Some(&self._renderer_data_ssao._ssao_blur_framebuffer_data1);
        let descriptor_sets_v = Some(&self._renderer_data_ssao._ssao_blur_descriptor_sets1);
        let push_constants_blur_h = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 0.0),
            ..Default::default()
        };
        let push_constants_blur_v = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(0.0, 1.0),
            ..Default::default()
        };
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_h, descriptor_sets_h, Some(&push_constants_blur_h));
        self.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_v, descriptor_sets_v, Some(&push_constants_blur_v));
    }

    pub fn composite_gbuffer(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let descriptor_sets = match self._renderer_data_ssr._current_ssr_resolved {
            RenderTargetType::SSRResolved => Some(&self._renderer_data_composite_gbuffer._descriptor_sets0),
            RenderTargetType::SSRResolvedPrev => Some(&self._renderer_data_composite_gbuffer._descriptor_sets1),
            _ => panic!("error")
        };
        self.render_material_instance(command_buffer, swapchain_index, "composite_gbuffer", DEFAULT_PIPELINE, &quad_geometry_data, None, descriptor_sets, NONE_PUSH_CONSTANT);
    }

    pub fn generate_min_z(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let resources: Ref<Resources> = self._resources.borrow();

        // Copy Scene Depth
        self.render_material_instance(command_buffer, swapchain_index, "generate_min_z", "generate_min_z/render_copy", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // Generate Hierachical Min Z
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("generate_min_z").borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._renderer_data_hiz._descriptor_sets.len();
        self.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._renderer_data_hiz._descriptor_sets[mip_level as usize]);
            self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            self.dispatch_compute_pipeline(
                command_buffer,
                self._renderer_data_hiz._dispatch_group_x >> (mip_level + 1),
                self._renderer_data_hiz._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn scene_color_downsampling(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32) {
        let resources: Ref<Resources> = self._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("downsampling").borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._scene_color_downsampling._descriptor_sets.len();
        self.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._scene_color_downsampling._descriptor_sets[mip_level as usize]);
            self.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            self.dispatch_compute_pipeline(
                command_buffer,
                self._scene_color_downsampling._dispatch_group_x >> (mip_level + 1),
                self._scene_color_downsampling._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn render_pre_process(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // Generate Hierachical Min Z
        self.generate_min_z(command_buffer, swapchain_index, quad_geometry_data);

        // Screen Space Reflection
        self.render_ssr(command_buffer, swapchain_index, quad_geometry_data);

        // SSAO
        self.render_ssao(command_buffer, swapchain_index, quad_geometry_data);

        // Composite GBuffer
        self.composite_gbuffer(command_buffer, swapchain_index, quad_geometry_data);

        // SceneColor Downsampling
        self.scene_color_downsampling(command_buffer, swapchain_index);
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
        self.render_material_instance(command_buffer, swapchain_index, "render_motion_blur", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
    }
}