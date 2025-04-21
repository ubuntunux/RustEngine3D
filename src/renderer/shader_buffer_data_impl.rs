use ash::{ext, vk, Device};
use nalgebra::{Vector2, Vector3};
use crate::constants;
use crate::effect::effect_manager::{GpuParticleCountBufferData, GpuParticleDynamicConstants, GpuParticleStaticConstants, GpuParticleUpdateBufferData};
use crate::renderer::shader_buffer_data::{AtmosphereConstants, RegisterShaderBufferCreateInfo, SceneConstants, ShaderBufferDataMap, ShaderBufferDataType, ShadowAOConstants, TransformMatrices, TransformOffsets, ViewConstants};
use crate::scene::camera::CameraObjectData;
use crate::scene::capture_height_map::CaptureHeightMap;
use crate::scene::debug_line::DebugLineInstanceData;
use crate::scene::font::FontInstanceData;
use crate::scene::light::{LightData, LightIndicesCell, PointLights};
use crate::scene::scene_manager::BoundBoxInstanceData;
use crate::scene::ui::UIRenderData;
use crate::vulkan_context::buffer;

impl SceneConstants {
    pub fn update_scene_constants(
        &mut self,
        screen_width: u32,
        screen_height: u32,
        elapsed_frame: u64,
        elapsed_time: f64,
        delta_time: f64,
        sea_height: f32,
        gpu_particle_count_buffer_offset: i32,
        gpu_particle_update_buffer_offset: i32,
        render_point_light_count: i32,
    ) {
        self._screen_size = Vector2::new(screen_width as f32, screen_height as f32);
        self._back_buffer_size = self._screen_size.into();
        self._moon_light_color = Vector3::new(0.796, 0.723, 0.618);
        self._time = elapsed_time as f32;
        self._delta_time = delta_time as f32;
        self._sea_height = sea_height;
        self._max_particle_count = unsafe { constants::MAX_PARTICLE_COUNT };
        self._max_emitter_count = unsafe { constants::MAX_EMITTER_COUNT };
        self._gpu_particle_count_buffer_offset = gpu_particle_count_buffer_offset;
        self._gpu_particle_update_buffer_offset = gpu_particle_update_buffer_offset;
        self._prev_gpu_particle_count_buffer_offset = unsafe { gpu_particle_count_buffer_offset ^ constants::MAX_EMITTER_COUNT };
        self._prev_gpu_particle_update_buffer_offset = unsafe { gpu_particle_update_buffer_offset ^ constants::MAX_PARTICLE_COUNT };
        self._render_point_light_count = render_point_light_count;
        self._elapsed_frame = elapsed_frame as u32;
    }
}

impl ViewConstants {
    pub fn update_view_constants<'a>(&mut self, camera_data: &CameraObjectData, capture_height_map: &CaptureHeightMap<'a>) {
        self._view = camera_data._view.into();
        self._inv_view = camera_data._inv_view.into();
        self._view_origin = camera_data._view_origin.into();
        self._inv_view_origin = camera_data._inv_view_origin.into();
        self._projection = camera_data._projection.into();
        self._inv_projection = camera_data._inv_projection.into();
        self._view_projection = camera_data._view_projection.into();
        self._inv_view_projection = camera_data._inv_view_projection.into();
        self._view_origin_projection = camera_data._view_origin_projection.into();
        self._inv_view_origin_projection = camera_data._inv_view_origin_projection.into();
        self._view_origin_projection_prev = camera_data._view_origin_projection_prev.into();
        self._projection_jitter = camera_data._projection_jitter.into();
        self._inv_projection_jitter = camera_data._inv_projection_jitter.into();
        self._view_projection_jitter = camera_data._view_projection_jitter.into();
        self._inv_view_projection_jitter = camera_data._inv_view_projection_jitter.into();
        self._view_origin_projection_jitter = camera_data._view_origin_projection_jitter.into();
        self._inv_view_origin_projection_jitter = camera_data._inv_view_origin_projection_jitter.into();
        self._view_origin_projection_prev_jitter = camera_data._view_origin_projection_prev_jitter.into();
        self._camera_position = camera_data._transform_object.get_position().clone() as Vector3<f32>;
        if capture_height_map.need_to_render_height_map() {
            self._capture_height_map_view_projection = capture_height_map.get_shadow_view_projection().clone();
            self._inv_capture_height_map_view_projection = capture_height_map.get_inv_shadow_view_projection().clone();
        }
        self._jitter_frame = camera_data._jitter_frame;
        self._camera_position_prev = camera_data._transform_object.get_prev_position().clone() as Vector3<f32>;
        self._view_constants_dummy0 = 0.0;
        self._near_far = Vector2::new(camera_data._near, camera_data._far);
        self._jitter_delta = camera_data._jitter_delta.into();
        self._jitter_offset = camera_data._jitter.into();
        self._view_constants_dummy1 = 0.0;
        self._view_constants_dummy2 = 0.0;
    }
}

impl ShaderBufferDataType {
    pub fn register_shader_buffer_data(
        debug_utils_device: &ext::debug_utils::Device,
        shader_buffer_create_info: &mut RegisterShaderBufferCreateInfo,
    ) {
        unsafe {
            let buffer_data_size = shader_buffer_create_info._shader_buffer_data_stride * shader_buffer_create_info._shader_buffer_data_count;
            let uniform_buffer_data = buffer::create_shader_buffer_data(
                &*shader_buffer_create_info._device,
                &*shader_buffer_create_info._memory_properties,
                debug_utils_device,
                &String::from(format!(
                    "{:?}",
                    shader_buffer_create_info._shader_buffer_data_type
                )),
                shader_buffer_create_info._buffer_usage,
                buffer_data_size as vk::DeviceSize,
                shader_buffer_create_info._create_buffer_per_swapchain_count,
                shader_buffer_create_info._has_staging_buffer,
                shader_buffer_create_info._is_device_local,
            );
            (&mut *shader_buffer_create_info._shader_buffer_data_map).insert(
                shader_buffer_create_info._shader_buffer_data_type,
                uniform_buffer_data,
            );
        }
    }

    pub fn register_uniform_buffers(
        device: &Device,
        memory_properties: &vk::PhysicalDeviceMemoryProperties,
        debug_utils_device: &ext::debug_utils::Device,
        shader_buffer_data_map: &mut ShaderBufferDataMap,
    ) {
        let uniform_buffer_create_info = RegisterShaderBufferCreateInfo {
            _device: device,
            _memory_properties: memory_properties,
            _shader_buffer_data_map: shader_buffer_data_map,
            _shader_buffer_data_type: ShaderBufferDataType::None,
            _buffer_usage: vk::BufferUsageFlags::UNIFORM_BUFFER,
            _shader_buffer_data_stride: 0,
            _shader_buffer_data_count: 1,
            _create_buffer_per_swapchain_count: true,
            _has_staging_buffer: true,
            _is_device_local: false,
        };

        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::SceneConstants,
                _shader_buffer_data_stride: std::mem::size_of::<SceneConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::ViewConstants,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightData,
                _shader_buffer_data_stride: std::mem::size_of::<LightData>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::PointLightData,
                _shader_buffer_data_stride: std::mem::size_of::<PointLights>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::ShadowAOConstants,
                _shader_buffer_data_stride: std::mem::size_of::<ShadowAOConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::AtmosphereConstants,
                _shader_buffer_data_stride: std::mem::size_of::<AtmosphereConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants0,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants1,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants2,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants3,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants4,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants5,
                _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
                ..uniform_buffer_create_info
            },
        );
    }

    pub fn register_storage_buffers(
        device: &Device,
        memory_properties: &vk::PhysicalDeviceMemoryProperties,
        debug_utils_device: &ext::debug_utils::Device,
        shader_buffer_data_map: &mut ShaderBufferDataMap,
    ) {
        let storage_buffer_create_info = RegisterShaderBufferCreateInfo {
            _device: device,
            _memory_properties: memory_properties,
            _shader_buffer_data_map: shader_buffer_data_map,
            _shader_buffer_data_type: ShaderBufferDataType::None,
            _buffer_usage: vk::BufferUsageFlags::STORAGE_BUFFER,
            _shader_buffer_data_stride: 0,
            _shader_buffer_data_count: 1,
            _create_buffer_per_swapchain_count: true,
            _has_staging_buffer: true,
            _is_device_local: false,
        };
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::TransformMatrices,
                _shader_buffer_data_stride: std::mem::size_of::<TransformMatrices>(),
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::TransformOffsets,
                _shader_buffer_data_stride: std::mem::size_of::<TransformOffsets>(),
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::BoundBoxInstanceDataBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<BoundBoxInstanceData>(),
                _shader_buffer_data_count: constants::MAX_BOUND_BOX_INSTANCE_COUNT,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::DebugLineInstanceDataBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<DebugLineInstanceData>(),
                _shader_buffer_data_count: constants::MAX_DEBUG_LINE_INSTANCE_COUNT,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::FontInstanceDataBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<FontInstanceData>(),
                _shader_buffer_data_count: constants::MAX_FONT_INSTANCE_COUNT,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::UIRenderDataBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<UIRenderData>(),
                _shader_buffer_data_count: constants::MAX_UI_INSTANCE_COUNT,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::GpuParticleStaticConstants,
                _shader_buffer_data_stride: std::mem::size_of::<GpuParticleStaticConstants>(),
                _shader_buffer_data_count: unsafe { constants::MAX_EMITTER_COUNT as usize },
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::GpuParticleDynamicConstants,
                _shader_buffer_data_stride: std::mem::size_of::<GpuParticleDynamicConstants>(),
                _shader_buffer_data_count: unsafe { constants::MAX_EMITTER_COUNT as usize },
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::GpuParticleEmitterIndexBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<i32>(),
                _shader_buffer_data_count: unsafe { constants::MAX_PARTICLE_COUNT as usize },
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::GpuParticleCountBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<GpuParticleCountBufferData>(),
                _shader_buffer_data_count: unsafe { constants::MAX_EMITTER_COUNT as usize * 2 },
                _buffer_usage: vk::BufferUsageFlags::STORAGE_BUFFER,
                _create_buffer_per_swapchain_count: false,
                _has_staging_buffer: false,
                _is_device_local: true,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::GpuParticleUpdateBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<GpuParticleUpdateBufferData>(),
                _shader_buffer_data_count: unsafe { constants::MAX_PARTICLE_COUNT as usize * 2 },
                _buffer_usage: vk::BufferUsageFlags::STORAGE_BUFFER,
                _create_buffer_per_swapchain_count: false,
                _has_staging_buffer: false,
                _is_device_local: true,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::PointLightCountBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<u32>(),
                _shader_buffer_data_count: constants::LIGHT_GRID_CELL_COUNT,
                _buffer_usage: vk::BufferUsageFlags::STORAGE_BUFFER,
                _create_buffer_per_swapchain_count: false,
                _has_staging_buffer: false,
                _is_device_local: true,
                ..storage_buffer_create_info
            },
        );
        ShaderBufferDataType::register_shader_buffer_data(
            debug_utils_device,
            &mut RegisterShaderBufferCreateInfo {
                _shader_buffer_data_type: ShaderBufferDataType::PointLightIndexBuffer,
                _shader_buffer_data_stride: std::mem::size_of::<LightIndicesCell>(),
                _shader_buffer_data_count: constants::LIGHT_GRID_CELL_COUNT,
                _buffer_usage: vk::BufferUsageFlags::STORAGE_BUFFER,
                _create_buffer_per_swapchain_count: false,
                _has_staging_buffer: false,
                _is_device_local: true,
                ..storage_buffer_create_info
            },
        );
    }

    pub fn register_shader_buffer_data_list(
        device: &Device,
        memory_properties: &vk::PhysicalDeviceMemoryProperties,
        debug_utils_device: &ext::debug_utils::Device,
        shader_buffer_data_map: &mut ShaderBufferDataMap,
    ) {
        ShaderBufferDataType::register_uniform_buffers(device, memory_properties, debug_utils_device, shader_buffer_data_map);
        ShaderBufferDataType::register_storage_buffers(device, memory_properties, debug_utils_device, shader_buffer_data_map);
    }
}