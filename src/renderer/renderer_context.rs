use std::cell::Ref;
use std::borrow::Cow;
use std::os::raw::c_char;
use std::ffi::{
    CStr,
    CString,
};
use std::vec::Vec;
use std::rc::Rc;
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
use ash::extensions::nv::RayTracing;
use ash::vk::CommandBuffer;
use nalgebra::Vector2;
use winit;
use winit::window::{ Window };

use crate::constants;
use crate::application::scene_manager::ProjectSceneManagerBase;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::font::FontManager;
use crate::renderer::image_sampler::{ self, ImageSamplerData };
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::ui::{ UIManager };
use crate::renderer::push_constants::PushConstant;
use crate::renderer::renderer_data::RendererData;
use crate::resource::resource::EngineResources;
use crate::utilities::system;
use crate::utilities::system::{ptr_as_ref, ptr_as_mut};
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
use crate::vulkan_context::ray_tracing::RayTracingData;
use crate::vulkan_context::render_pass::{ RenderPassDataCreateInfo, RenderPassData, PipelineData };
use crate::vulkan_context::swapchain::{ self, SwapchainData };
use crate::vulkan_context::texture::{ TextureCreateInfo, TextureData };
use crate::vulkan_context::vulkan_context::{ RenderFeatures, SwapchainArray, FrameArray };


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

pub trait RendererDataBase {
    fn initialize_renderer_data(
        &mut self,
        renderer_context: &RendererContext,
        engine_resources: *const EngineResources,
        effect_manager: *const EffectManager
    );
    fn is_first_rendering(&self) -> bool;
    fn set_is_first_rendering(&mut self, is_first_rendering: bool);
    fn prepare_framebuffer_and_descriptors(&mut self, device: &Device, engine_resources: &EngineResources);
    fn destroy_framebuffer_and_descriptors(&mut self, device: &Device);
    fn get_shader_buffer_data_from_str(&self, buffer_data_name: &str) -> &ShaderBufferData;
    fn get_render_target_from_str(&self, render_target_type_str: &str) -> &TextureData;
    fn get_render_pass_data_create_infos(&self) -> Vec<RenderPassDataCreateInfo>;
    fn create_render_targets(&mut self, renderer_context: &RendererContext);
    fn destroy_render_targets(&mut self, device: &Device);
    fn destroy_uniform_buffers(&mut self, device: &Device);
    fn pre_update_render_scene(&mut self, delta_time: f64);
    fn render_scene(
        &mut self,
        command_buffer: CommandBuffer,
        frame_index: usize,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        project_scene_manager: &dyn ProjectSceneManagerBase,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        elapsed_time: f64,
        delta_time: f64,
        elapsed_frame: u64,
    );
}


pub struct RendererContext {
    _frame_index: i32,
    _swapchain_index: u32,
    _need_recreate_swapchain: bool,
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
    pub _ray_tracing: Rc<RayTracing>,
    pub _ray_tracing_properties: vk::PhysicalDeviceRayTracingPropertiesNV,
    pub _ray_tracing_test_data: RayTracingData,
    pub _engine_resources: *const EngineResources,
    pub _renderer_data: Box<RendererData>,
}

impl RendererContext {
    pub fn create_renderer_context(
        app_name: &str,
        app_version: u32,
        window_size: &Vector2<i32>,
        window: &Window,
        engine_resources: *const EngineResources,
    ) -> RendererContext {
        unsafe {
            log::info!("createrenderer_context: {}, width: {}, height: {}", constants::ENGINE_NAME, window_size.x, window_size.y);
            let entry = Entry::linked();
            let surface_extensions = ash_window::enumerate_required_extensions(window).unwrap();
            let required_layer_names = device::get_instance_layers(&entry, &constants::REQUIRED_INSTANCE_LAYERS);
            let required_instance_layers: Vec<*const c_char> = required_layer_names.iter().map(|layer| layer.as_ptr()).collect();
            let device_extensions: Vec<CString> = constants::REQUIRED_DEVICE_EXTENSIONS.iter().map(|str| CString::new(str.as_str()).unwrap() ).collect();
            let mut device_extension_names_raw: Vec<*const c_char> = device_extensions.iter().map(|extension| extension.as_ptr()).collect();
            let device_extensions_for_ray_tracing: Vec<CString> = if constants::USE_RAY_TRACING {
                constants::REQUIRED_RAY_TRACING_EXTENSIONS.iter().map(|str| CString::new(str.as_str()).unwrap() ).collect()
            } else {
                Vec::new()
            };
            let instance: Instance = device::create_vk_instance(&entry, &app_name, app_version, &surface_extensions, &required_instance_layers);
            let surface = device::create_vk_surface(&entry, &instance, window);
            let surface_interface = Surface::new(&entry, &instance);
            let (physical_device, swapchain_support_details, physical_device_features, has_ray_tracing_extensions) =
                device::select_physical_device(&instance, &surface_interface, surface, &device_extensions, &device_extensions_for_ray_tracing).unwrap();
            let device_properties: vk::PhysicalDeviceProperties = instance.get_physical_device_properties(physical_device);
            let device_memory_properties: vk::PhysicalDeviceMemoryProperties = instance.get_physical_device_memory_properties(physical_device);
            let device_name = CStr::from_ptr(device_properties.device_name.as_ptr() as *const c_char);
            let enable_ray_tracing = has_ray_tracing_extensions && constants::USE_RAY_TRACING;

            log::info!("PhysicalDeviceProperties");
            log::info!("    vulakn api_version: {}.{}.{}", vk::api_version_major(device_properties.api_version), vk::api_version_minor(device_properties.api_version), vk::api_version_patch(device_properties.api_version));
            log::info!("    driver_version: {}.{}.{}", vk::api_version_major(device_properties.driver_version), vk::api_version_minor(device_properties.driver_version), vk::api_version_patch(device_properties.driver_version));
            log::info!("    device: {:?} {:?} vecdor_id: {:?} device_id: {:?}", device_name, device_properties.device_type, device_properties.vendor_id, device_properties.device_id);
            log::info!("    limits: {:?}", device_properties.limits);

            // ray tracing properties
            let ray_tracing_properties = RayTracing::get_properties(&instance, physical_device);
            log::info!("NV Ray Tracing Properties:");
            log::info!("    has_ray_tracing_extensions: {}", has_ray_tracing_extensions);
            log::info!("    shader_group_handle_size: {}", ray_tracing_properties.shader_group_handle_size);
            log::info!("    max_recursion_depth: {}", ray_tracing_properties.max_recursion_depth);
            log::info!("    max_shader_group_stride: {}", ray_tracing_properties.max_shader_group_stride);
            log::info!("    shader_group_base_alignment: {}", ray_tracing_properties.shader_group_base_alignment);
            log::info!("    max_geometry_count: {}", ray_tracing_properties.max_geometry_count);
            log::info!("    max_instance_count: {}", ray_tracing_properties.max_instance_count);
            log::info!("    max_triangle_count: {}", ray_tracing_properties.max_triangle_count);
            log::info!("    max_descriptor_set_acceleration_structures: {}", ray_tracing_properties.max_descriptor_set_acceleration_structures);
            if enable_ray_tracing {
                for extension in device_extensions_for_ray_tracing.iter() {
                    device_extension_names_raw.push(extension.as_ptr());
                }
            }

            let msaa_samples = device::get_max_usable_sample_count(&device_properties);
            let queue_family_indices = queue::get_queue_family_indices(
                &instance,
                &surface_interface,
                surface,
                physical_device,
                constants::IS_CONCURRENT_MODE
            );
            let render_features = RenderFeatures {
                _physical_device_features: vk::PhysicalDeviceFeatures {
                    shader_clip_distance: if constants::SHADER_CLIP_DISTANCE { 1 } else { 0 },
                    ..physical_device_features.clone()
                },
                _msaa_samples: msaa_samples,
                _use_ray_tracing: enable_ray_tracing,
            };
            let graphics_queue_index = queue_family_indices._graphics_queue_index;
            let present_queue_index = queue_family_indices._present_queue_index;
            let queue_family_index_set: Vec<u32> = if graphics_queue_index == present_queue_index {
                vec![graphics_queue_index]
            } else {
                vec![graphics_queue_index, present_queue_index]
            };
            let device = device::create_device(&instance, physical_device, &render_features, &queue_family_index_set, &device_extension_names_raw, &required_instance_layers);
            let ray_tracing = Rc::new(RayTracing::new(&instance, &device));
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
            if vk::DebugUtilsMessageSeverityFlagsEXT::empty() != constants::DEBUG_MESSAGE_LEVEL {
                let debug_message_level = get_debug_message_level(constants::DEBUG_MESSAGE_LEVEL);
                let debug_info = vk::DebugUtilsMessengerCreateInfoEXT {
                    message_severity: debug_message_level,
                    message_type: vk::DebugUtilsMessageTypeFlagsEXT::GENERAL
                        | vk::DebugUtilsMessageTypeFlagsEXT::VALIDATION
                        | vk::DebugUtilsMessageTypeFlagsEXT::PERFORMANCE,
                    pfn_user_callback: Some(vulkan_debug_callback),
                    ..Default::default()
                };
                debug_util_interface = Some(DebugUtils::new(&entry, &instance));
                debug_call_back = debug_util_interface.as_ref().unwrap().create_debug_utils_messenger(&debug_info, None).unwrap();
            } else {
                debug_util_interface = None;
                debug_call_back = vk::DebugUtilsMessengerEXT::null();
            }

            // renderer data
            let renderer_data = Box::new(RendererData::create_renderer_data());

            RendererContext {
                _frame_index: 0,
                _swapchain_index: 0,
                _need_recreate_swapchain: false,
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
                _ray_tracing: ray_tracing,
                _ray_tracing_properties: ray_tracing_properties,
                _ray_tracing_test_data: RayTracingData::create_ray_tracing_data(),
                _engine_resources: engine_resources.clone(),
                _renderer_data: renderer_data,
            }
        }
    }

    pub fn initialize_renderer_context(&mut self, engine_resources: *const EngineResources, effect_manager: *const EffectManager) {
        self._swapchain_index = 0;
        self._frame_index = 0;
        self._need_recreate_swapchain = false;
        self._image_samplers = image_sampler::create_image_samplers(self.get_device());
        self.get_renderer_data_mut().initialize_renderer_data(self, engine_resources, effect_manager);

        // TEST CODE
        if self.get_use_ray_tracing() {
            self.create_ray_tracing_test_data();
        }
    }
    pub fn get_engine_resources(&self) -> &EngineResources {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources {
        ptr_as_mut(self._engine_resources)
    }
    pub fn get_renderer_data(&self) -> &RendererData {
        self._renderer_data.as_ref()
    }
    pub fn get_renderer_data_mut(&self) -> &mut RendererData {
        ptr_as_mut(self._renderer_data.as_ref())
    }
    pub fn get_need_recreate_swapchain(&self) -> bool { self._need_recreate_swapchain }
    pub fn set_need_recreate_swapchain(&mut self, value: bool) {
        log::info!("set_need_recreate_swapchain: {}", value);
        self._need_recreate_swapchain = value;
    }
    pub fn get_instance(&self) -> &Instance { &self._instance }
    pub fn get_device(&self) -> &Device { &self._device }
    pub fn get_use_ray_tracing(&self) -> bool { self._render_features._use_ray_tracing }
    pub fn get_ray_tracing(&self) -> &RayTracing { &self._ray_tracing }
    pub fn get_ray_tracing_properties(&self) -> &vk::PhysicalDeviceRayTracingPropertiesNV { &self._ray_tracing_properties }

    // TEST CODE
    pub fn get_ray_tracing_test_data(&self) -> &RayTracingData { &self._ray_tracing_test_data }
    pub fn create_ray_tracing_test_data(&mut self) {
        log::info!(">>> TEST CODE: create_ray_tracing_test_data");
        let mut t = RayTracingData::create_ray_tracing_data();
        t.initialize_ray_tracing_data(
            self.get_device(),
            self.get_device_memory_properties(),
            self.get_ray_tracing(),
            self.get_command_pool(),
            self.get_graphics_queue(),
        );
        self._ray_tracing_test_data = t;
    }
    //

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
    pub fn create_geometry_buffer(&self, geometry_name: &String, geometry_create_info: &geometry_buffer::GeometryCreateInfo) -> geometry_buffer::GeometryData {
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
    pub fn destroy_renderer_context(&mut self) {
        unsafe {
            self.destroy_framebuffer_and_descriptors();
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

    pub fn pipeline_barrier(
        &self,
        command_buffer: vk::CommandBuffer,
        src_stage_mask: vk::PipelineStageFlags,
        dst_stage_mask: vk::PipelineStageFlags,
        dependency_flags: vk::DependencyFlags,
        memory_barriers: &[vk::MemoryBarrier],
        buffer_memory_barriers: &[vk::BufferMemoryBarrier],
        image_memory_barriers: &[vk::ImageMemoryBarrier],
    ) {
        unsafe {
            self._device.cmd_pipeline_barrier(
                command_buffer,
                src_stage_mask,
                dst_stage_mask,
                dependency_flags,
                memory_barriers,
                buffer_memory_barriers,
                image_memory_barriers);
        }
    }

    pub fn dispatch_material_instance(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        material_instance_name: &str,
        render_pass_pipeline_data_name: &str,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&dyn PushConstant>,
    ) {
        let engine_resources = self.get_engine_resources();
        let material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = if render_pass_pipeline_data_name.is_empty() {
            material_instance_data.get_default_pipeline_binding_data()
        } else {
            material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name)
        };

        self.dispatch_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            group_count_x,
            group_count_y,
            group_count_z,
            custom_descriptor_sets,
            push_constant_data
        );
    }

    pub fn dispatch_render_pass_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&dyn PushConstant>,
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

    pub fn render_material_instance(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        material_instance_name: &str,
        render_pass_pipeline_data_name: &str,
        geometry_data: &GeometryData,
        custom_framebuffer_data: Option<&FramebufferData>,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&dyn PushConstant>,
    ) {
        let engine_resources = self.get_engine_resources();
        let material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data(material_instance_name).borrow();
        let pipeline_binding_data = if render_pass_pipeline_data_name.is_empty() {
            material_instance_data.get_default_pipeline_binding_data()
        } else {
            material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name)
        };
        self.render_render_pass_pipeline(command_buffer, swapchain_index, pipeline_binding_data, geometry_data, custom_framebuffer_data, custom_descriptor_sets, push_constant_data);
    }

    pub fn render_render_pass_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        pipeline_binding_data: &PipelineBindingData,
        geometry_data: &GeometryData,
        custom_framebuffer_data: Option<&FramebufferData>,
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>,
        push_constant_data: Option<&dyn PushConstant>,
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
        let engine_resources = self.get_engine_resources();
        let framebuffer_data: *const FramebufferData = match custom_framebuffer {
            Some(custom_framebuffer) => custom_framebuffer,
            None => engine_resources.get_framebuffer_data(render_pass_data.get_render_pass_data_name().as_str()).as_ptr()
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
        custom_descriptor_sets: Option<&SwapchainArray<vk::DescriptorSet>>
    ) {
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

    pub fn upload_push_constant_data(&self, command_buffer: vk::CommandBuffer, pipeline_data: &PipelineData, push_constant_data: &dyn PushConstant) {
        let constants: &[u8] = push_constant_data.to_bytes();
        unsafe {
            self._device.cmd_push_constants(command_buffer, pipeline_data._pipeline_layout, vk::ShaderStageFlags::ALL, 0, constants);
        }
    }

    // pub fn upload_push_constant_data2<T>(&self, command_buffer: vk::CommandBuffer, pipeline_data: &PipelineData, push_constant_data: &T) {
    //     let constants: &[u8] = system::to_bytes(push_constant_data);
    //     unsafe {
    //         self._device.cmd_push_constants(command_buffer, pipeline_data._pipeline_layout, vk::ShaderStageFlags::ALL, 0, constants);
    //     }
    // }

    pub fn dispatch_compute_pipeline(
        &self,
        command_buffer: vk::CommandBuffer,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32
    ) {
        unsafe {
            self._device.cmd_dispatch(command_buffer, 1u32.max(group_count_x), 1u32.max(group_count_y), 1u32.max(group_count_z));
        }
    }

    pub fn draw_elements(&self, command_buffer: vk::CommandBuffer, geometry_data: &GeometryData) {
        self.draw_indexed(
            command_buffer,
            &[geometry_data._vertex_buffer_data._buffer],
            &[],
            1,
            geometry_data._index_buffer_data._buffer,
            geometry_data._vertex_index_count,
        );
    }

    pub fn draw_elements_instanced(&self, command_buffer: vk::CommandBuffer, geometry_data: &GeometryData, instance_buffers: &[vk::Buffer], instance_count: u32) {
        self.draw_indexed(
            command_buffer,
            &[geometry_data._vertex_buffer_data._buffer],
            instance_buffers,
            instance_count,
            geometry_data._index_buffer_data._buffer,
            geometry_data._vertex_index_count,
        );
    }

    pub fn draw_indexed(
        &self,
        command_buffer: vk::CommandBuffer,
        vertex_buffers: &[vk::Buffer],
        instance_buffers: &[vk::Buffer],
        instance_count: u32,
        index_buffer: vk::Buffer,
        index_count: u32,
    ) {
        unsafe {
            let offsets: &[vk::DeviceSize] = &[0];
            const FIRST_INDEX: u32 = 0;
            const VERTEX_OFFSET: i32 = 0;
            const FIRST_INSTANCE: u32 = 0;
            const VERTEX_BUFFER_BINDING_INDEX: u32 = 0;
            const INSTANCE_BUFFER_BINDING_INDEX: u32 = 1;
            self._device.cmd_bind_vertex_buffers(command_buffer, VERTEX_BUFFER_BINDING_INDEX, vertex_buffers, offsets);
            if false == instance_buffers.is_empty() {
                self._device.cmd_bind_vertex_buffers(command_buffer, INSTANCE_BUFFER_BINDING_INDEX, instance_buffers, offsets);
            }
            self._device.cmd_bind_index_buffer(command_buffer, index_buffer, 0, vk::IndexType::UINT32);
            self._device.cmd_draw_indexed(command_buffer, index_count, instance_count, FIRST_INDEX, VERTEX_OFFSET, FIRST_INSTANCE);
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

    pub fn resize_window(&mut self) {
        log::info!("<< res        self.device_wait_idle();izeWindow >>");

        let engine_resources = ptr_as_mut(self._engine_resources);

        // destroy swapchain & graphics engine_resources
        self.destroy_framebuffer_and_descriptors();
        engine_resources.unload_graphics_datas(self);
        self.destroy_render_targets();

        // recreate swapchain & graphics engine_resources
        self.recreate_swapchain();
        self.create_render_targets();
        engine_resources.load_graphics_datas(self);
        self.prepare_framebuffer_and_descriptors();
        self.set_is_first_rendering(true);
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
            unsafe { constants::ENABLE_IMMEDIATE_MODE }
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
                log::error!("present_error: {:?}", present_error);
            }

            // waiting
            match self._device.device_wait_idle() {
                Err(e) => {
                    log::error!("device_wait_idle: {:?}", e);
                    return VkResult::Err(e)
                },
                _ => ()
            }

            is_swapchain_suboptimal
        }
    }

    pub fn upload_shader_buffer_data<T>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data: &ShaderBufferData, upload_data: &T) {
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        if shader_buffer_data._staging_buffers.is_some() {
            let staging_buffer_data = &shader_buffer_data._staging_buffers.as_ref().unwrap()[swapchain_index as usize];
            let upload_data_size = std::mem::size_of::<T>() as u64;
            buffer::upload_buffer_data(&self._device, staging_buffer_data, system::to_bytes(upload_data));
            buffer::copy_buffer(&self._device, command_buffer, staging_buffer_data._buffer, buffer_data._buffer, upload_data_size);
        } else {
            buffer::upload_buffer_data(&self._device, buffer_data, system::to_bytes(upload_data));
        }
    }

    pub fn upload_shader_buffer_data_offset<T>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data: &ShaderBufferData, upload_data: &T, offset: vk::DeviceSize) {
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        if shader_buffer_data._staging_buffers.is_some() {
            let staging_buffer_data = &shader_buffer_data._staging_buffers.as_ref().unwrap()[swapchain_index as usize];
            let upload_data_size = std::mem::size_of::<T>() as u64;
            buffer::upload_buffer_data_offset(&self._device, staging_buffer_data, system::to_bytes(upload_data), offset);
            buffer::copy_buffer_offset(&self._device, command_buffer, staging_buffer_data._buffer, offset, buffer_data._buffer, offset, upload_data_size);
        } else {
            buffer::upload_buffer_data_offset(&self._device, buffer_data, system::to_bytes(upload_data), offset);
        }
    }

    pub fn upload_shader_buffer_datas<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data: &ShaderBufferData, upload_data: &[T]) {
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        if shader_buffer_data._staging_buffers.is_some() {
            let staging_buffer_data = &shader_buffer_data._staging_buffers.as_ref().unwrap()[swapchain_index as usize];
            let upload_data_size = std::mem::size_of::<T>() as u64 * upload_data.len() as u64;
            buffer::upload_buffer_data(&self._device, staging_buffer_data, upload_data);
            buffer::copy_buffer(&self._device, command_buffer, staging_buffer_data._buffer, buffer_data._buffer, upload_data_size);
        } else {
            buffer::upload_buffer_data(&self._device, buffer_data, upload_data);
        }
    }

    pub fn read_shader_buffer_datas<T: Copy>(&self, swapchain_index: u32, shader_buffer_data: &ShaderBufferData, read_offset: u32, read_data: &mut [T]) {
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        buffer::read_buffer_data(&self._device, buffer_data, read_offset, read_data);
    }

    pub fn upload_shader_buffer_datas_offset<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data: &ShaderBufferData, upload_data: &[T], offset: vk::DeviceSize) {
        let buffer_data = &shader_buffer_data._buffers[swapchain_index as usize];
        if shader_buffer_data._staging_buffers.is_some() {
            let staging_buffer_data = &shader_buffer_data._staging_buffers.as_ref().unwrap()[swapchain_index as usize];
            let upload_data_size = std::mem::size_of::<T>() as u64 * upload_data.len() as u64;
            buffer::upload_buffer_data_offset(&self._device, staging_buffer_data, upload_data, offset);
            buffer::copy_buffer_offset(&self._device, command_buffer, staging_buffer_data._buffer, offset, buffer_data._buffer, offset, upload_data_size);
        } else {
            buffer::upload_buffer_data_offset(&self._device, buffer_data, upload_data, offset);
        }
    }

    pub fn render_scene(
        &mut self,
        project_scene_manager: &dyn ProjectSceneManagerBase,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        elapsed_time: f64,
        delta_time: f64,
        elapsed_frame: u64
    ) {
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

            let (swapchain_index, failed_acquire_next_image) = if acquire_next_image_result.is_ok() {
                acquire_next_image_result.unwrap()
            } else {
                (self._swapchain_index, true)
            };

            self._swapchain_index = swapchain_index;

            let present_result: vk::Result = if swapchain_index < constants::SWAPCHAIN_IMAGE_COUNT as u32 && false == failed_acquire_next_image {
                // Begin command buffer
                let command_buffer = self._command_buffers[swapchain_index as usize];
                let command_buffer_begin_info = vk::CommandBufferBeginInfo {
                    flags: vk::CommandBufferUsageFlags::SIMULTANEOUS_USE,
                    ..Default::default()
                };
                self._device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("vkBeginCommandBuffer failed!");

                // renderer - render_scene
                self.get_renderer_data_mut().render_scene(
                    command_buffer,
                    frame_index,
                    swapchain_index,
                    &self,
                    project_scene_manager,
                    font_manager,
                    ui_manager,
                    elapsed_time,
                    delta_time,
                    elapsed_frame
                );

                // End command buffer
                self._device.end_command_buffer(command_buffer).expect("vkEndCommandBuffer failed!");

                // End Render
                if self.is_first_rendering() {
                    self.set_is_first_rendering(false);
                }
                let present_swapchain_result = self.present_swapchain(&[command_buffer], frame_fence, image_available_semaphore, render_finished_semaphore);
                match present_swapchain_result {
                    Ok(is_swapchain_suboptimal) => if is_swapchain_suboptimal { vk::Result::SUBOPTIMAL_KHR } else { vk::Result::SUCCESS },
                    Err(err) => err,
                }
            } else {
                log::error!("failed_acquire_next_image: {}, swapchain_index: {}", failed_acquire_next_image, swapchain_index);
                vk::Result::ERROR_OUT_OF_DATE_KHR
            };

            if vk::Result::SUCCESS != present_result {
                log::error!("present swapchain result: {:?}", present_result);
            }

            if vk::Result::ERROR_OUT_OF_DATE_KHR == present_result || vk::Result::SUBOPTIMAL_KHR == present_result {
                self.set_need_recreate_swapchain(true);
            }

            self._frame_index = (self._frame_index + 1) % (constants::MAX_FRAME_COUNT as i32);
        }
    }

    // renderer interface
    pub fn is_first_rendering(&self) -> bool {
        self.get_renderer_data_mut().is_first_rendering()
    }

    pub fn set_is_first_rendering(&self, is_first_rendering: bool) {
        self.get_renderer_data_mut().set_is_first_rendering(is_first_rendering);
    }

    pub fn prepare_framebuffer_and_descriptors(&self) {
        log::info!("RendererContext::prepare_framebuffer_and_descriptors");
        self.get_renderer_data_mut().prepare_framebuffer_and_descriptors(&self._device, &self.get_engine_resources());
    }

    pub fn destroy_framebuffer_and_descriptors(&self) {
        log::info!("RendererContext::destroy_framebuffer_and_descriptors");
        self.get_renderer_data_mut().destroy_framebuffer_and_descriptors(&self._device);
    }

    pub fn get_shader_buffer_data_from_str(&self, buffer_data_name: &str) -> &ShaderBufferData {
        self.get_renderer_data().get_shader_buffer_data_from_str(buffer_data_name)
    }

    pub fn get_render_target_from_str(&self, render_target_type_str: &str) -> &TextureData {
        self.get_renderer_data().get_render_target_from_str(render_target_type_str)
    }

    pub fn get_render_pass_data_create_infos(&self) -> Vec<RenderPassDataCreateInfo> {
        self.get_renderer_data().get_render_pass_data_create_infos()
    }

    pub fn create_render_targets(&self) {
        log::info!("create_render_targets");
        self.get_renderer_data_mut().create_render_targets(self);
    }

    pub fn destroy_render_targets(&self) {
        log::info!("destroy_render_targets");
        self.get_renderer_data_mut().destroy_render_targets(self.get_device());
    }

    pub fn destroy_uniform_buffers(&self) {
        self.get_renderer_data_mut().destroy_uniform_buffers(self.get_device());
    }
}