use std::cmp::{Eq, PartialEq};
use std::collections::HashMap;

use ash::{Device, vk};
use ash::ext;
use nalgebra::{Matrix4, Vector2, Vector3, Vector4};

use crate::constants;
use crate::effect::effect_manager::{
    GpuParticleCountBufferData, GpuParticleDynamicConstants, GpuParticleStaticConstants,
    GpuParticleUpdateBufferData,
};
use crate::scene::camera::CameraObjectData;
use crate::scene::debug_line::DebugLineInstanceData;
use crate::scene::font::FontInstanceData;
use crate::scene::light::{LightData, PointLightData};
use crate::scene::scene_manager::BoundBoxInstanceData;
use crate::scene::ui::UIRenderData;
use crate::vulkan_context::buffer::{self, ShaderBufferData};

pub type ShaderBufferDataMap<'a> = HashMap<ShaderBufferDataType, ShaderBufferData<'a>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ShaderBufferDataType {
    None,
    SceneConstants,
    ViewConstants,
    LightData,
    PointLightData,
    SSAOConstants,
    AtmosphereConstants,
    LightProbeViewConstants0,
    LightProbeViewConstants1,
    LightProbeViewConstants2,
    LightProbeViewConstants3,
    LightProbeViewConstants4,
    LightProbeViewConstants5,
    TransformMatrices,
    TransformOffsets,
    BoundBoxInstanceDataBuffer,
    DebugLineInstanceDataBuffer,
    FontInstanceDataBuffer,
    UIRenderDataBuffer,
    GpuParticleStaticConstants,
    GpuParticleDynamicConstants,
    GpuParticleEmitterIndexBuffer,
    GpuParticleCountBuffer,
    GpuParticleUpdateBuffer,
    PointLightCountBuffer,
    PointLightIndexBuffer,
}

// scene_constants.glsl - struct SCENE_CONSTANTS
#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct SceneConstants {
    pub _screen_size: Vector2<f32>,
    pub _back_buffer_size: Vector2<f32>,
    pub _time: f32,
    pub _delta_time: f32,
    pub _sea_height: f32,
    pub _max_particle_count: i32,
    pub _max_emitter_count: i32,
    pub _gpu_particle_count_buffer_offset: i32,
    pub _gpu_particle_update_buffer_offset: i32,
    pub _prev_gpu_particle_count_buffer_offset: i32,
    pub _prev_gpu_particle_update_buffer_offset: i32,
    pub _render_point_light_count: i32,
    pub _reserved0: i32,
    pub _reserved1: i32,
}

// scene_constants.glsl - struct VIEW_CONSTANTS
#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct ViewConstants {
    pub _view: Matrix4<f32>,
    pub _inv_view: Matrix4<f32>,
    pub _view_origin: Matrix4<f32>,
    pub _inv_view_origin: Matrix4<f32>,
    pub _projection: Matrix4<f32>,
    pub _inv_projection: Matrix4<f32>,
    pub _view_projection: Matrix4<f32>,
    pub _inv_view_projection: Matrix4<f32>,
    pub _view_origin_projection: Matrix4<f32>,
    pub _inv_view_origin_projection: Matrix4<f32>,
    pub _view_origin_projection_prev: Matrix4<f32>,
    pub _projection_jitter: Matrix4<f32>,
    pub _inv_projection_jitter: Matrix4<f32>,
    pub _view_projection_jitter: Matrix4<f32>,
    pub _inv_view_projection_jitter: Matrix4<f32>,
    pub _view_origin_projection_jitter: Matrix4<f32>,
    pub _inv_view_origin_projection_jitter: Matrix4<f32>,
    pub _view_origin_projection_prev_jitter: Matrix4<f32>,
    pub _capture_height_map_view_projection: Matrix4<f32>,
    pub _camera_position: Vector3<f32>,
    pub _jitter_frame: i32,
    pub _camera_position_prev: Vector3<f32>,
    pub _view_constants_dummy0: f32,
    pub _near_far: Vector2<f32>,
    pub _jitter_delta: Vector2<f32>,
    pub _jitter_offset: Vector2<f32>,
    pub _view_constants_dummy1: f32,
    pub _view_constants_dummy2: f32,
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct PointLights {
    pub _point_light_data: [PointLightData; constants::MAX_POINT_LIGHTS],
}

#[repr(C)]
#[derive(Clone)]
pub struct SSAOConstants {
    pub _ssao_kernel_samples: [Vector4<f32>; constants::SSAO_KERNEL_SIZE],
}

#[repr(C)]
#[derive(Clone)]
pub struct TransformMatrices {
    pub _transform_matrices: [Matrix4<f32>; constants::MAX_TRANSFORM_COUNT],
}

impl Default for TransformMatrices {
    fn default() -> TransformMatrices {
        TransformMatrices {
            _transform_matrices: [Matrix4::identity(); constants::MAX_TRANSFORM_COUNT],
        }
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct TransformOffsets {
    pub _transform_offsets: [Vector4<i32>; constants::MAX_TRANSFORM_COUNT],
}

impl Default for TransformOffsets {
    fn default() -> TransformOffsets {
        TransformOffsets {
            _transform_offsets: [Vector4::zeros(); constants::MAX_TRANSFORM_COUNT],
        }
    }
}

// pecomputed_atmosphere/atmosphere_common.glsl - struct ATMOSPHERE_CONSTANTS
#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct AtmosphereConstants {
    pub _sky_radiance_to_luminance: Vector3<f32>,
    pub _cloud_exposure: f32,

    pub _sun_radiance_to_luminance: Vector3<f32>,
    pub _cloud_altitude: f32,

    pub _cloud_height: f32,
    pub _cloud_speed: f32,
    pub _cloud_absorption: f32,
    pub _cloud_tiling: f32,

    pub _cloud_contrast: f32,
    pub _cloud_coverage: f32,
    pub _noise_tiling: f32,
    pub _noise_contrast: f32,

    pub _earth_center: Vector3<f32>,
    pub _noise_coverage: f32,

    pub _sun_size: Vector2<f32>,
    pub _atmosphere_exposure: f32,
    pub _inscatter_power: f32,
}

#[derive(Clone)]
pub struct RegisterShaderBufferCreateInfo<'a> {
    pub _device: *const Device,
    pub _memory_properties: *const vk::PhysicalDeviceMemoryProperties,
    pub _shader_buffer_data_map: *mut ShaderBufferDataMap<'a>,
    pub _shader_buffer_data_type: ShaderBufferDataType,
    pub _buffer_usage: vk::BufferUsageFlags,
    pub _shader_buffer_data_stride: usize,
    pub _shader_buffer_data_count: usize,
    pub _create_buffer_per_swapchain_count: bool,
    pub _has_staging_buffer: bool,
    pub _is_device_local: bool
}

// Interfaces
impl SceneConstants {
    pub fn update_scene_constants(
        &mut self,
        screen_width: u32,
        screen_height: u32,
        elapsed_time: f64,
        delta_time: f64,
        sea_height: f32,
        gpu_particle_count_buffer_offset: i32,
        gpu_particle_update_buffer_offset: i32,
        render_point_light_count: i32,
    ) {
        self._screen_size = Vector2::new(screen_width as f32, screen_height as f32);
        self._back_buffer_size = self._screen_size.into();
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
        self._reserved0 = 0;
        self._reserved1 = 0;
    }
}

impl ViewConstants {
    pub fn update_view_constants(&mut self, camera_data: &CameraObjectData) {
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
        self._inv_view_origin_projection_jitter =
            camera_data._inv_view_origin_projection_jitter.into();
        self._view_origin_projection_prev_jitter =
            camera_data._view_origin_projection_prev_jitter.into();
        self._camera_position =
            camera_data._transform_object.get_position().clone() as Vector3<f32>;
        self._jitter_frame = camera_data._jitter_frame;
        self._camera_position_prev =
            camera_data._transform_object.get_prev_position().clone() as Vector3<f32>;
        self._view_constants_dummy0 = 0.0;
        self._near_far = Vector2::new(camera_data._near, camera_data._far);
        self._jitter_delta = camera_data._jitter_delta.into();
        self._jitter_offset = camera_data._jitter.into();
        self._view_constants_dummy1 = 0.0;
        self._view_constants_dummy2 = 0.0;
    }
}

impl std::fmt::Display for ShaderBufferDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for ShaderBufferDataType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "None" => Ok(ShaderBufferDataType::None),
            "SceneConstants" => Ok(ShaderBufferDataType::SceneConstants),
            "ViewConstants" => Ok(ShaderBufferDataType::ViewConstants),
            "LightData" => Ok(ShaderBufferDataType::LightData),
            "PointLightData" => Ok(ShaderBufferDataType::PointLightData),
            "SSAOConstants" => Ok(ShaderBufferDataType::SSAOConstants),
            "AtmosphereConstants" => Ok(ShaderBufferDataType::AtmosphereConstants),
            "LightProbeViewConstants0" => Ok(ShaderBufferDataType::LightProbeViewConstants0),
            "LightProbeViewConstants1" => Ok(ShaderBufferDataType::LightProbeViewConstants1),
            "LightProbeViewConstants2" => Ok(ShaderBufferDataType::LightProbeViewConstants2),
            "LightProbeViewConstants3" => Ok(ShaderBufferDataType::LightProbeViewConstants3),
            "LightProbeViewConstants4" => Ok(ShaderBufferDataType::LightProbeViewConstants4),
            "LightProbeViewConstants5" => Ok(ShaderBufferDataType::LightProbeViewConstants5),
            "TransformMatrices" => Ok(ShaderBufferDataType::TransformMatrices),
            "TransformOffsets" => Ok(ShaderBufferDataType::TransformOffsets),
            "BoundBoxInstanceDataBuffer" => Ok(ShaderBufferDataType::BoundBoxInstanceDataBuffer),
            "DebugLineInstanceDataBuffer" => Ok(ShaderBufferDataType::DebugLineInstanceDataBuffer),
            "FontInstanceDataBuffer" => Ok(ShaderBufferDataType::FontInstanceDataBuffer),
            "UIRenderDataBuffer" => Ok(ShaderBufferDataType::UIRenderDataBuffer),
            "GpuParticleStaticConstants" => Ok(ShaderBufferDataType::GpuParticleStaticConstants),
            "GpuParticleDynamicConstants" => Ok(ShaderBufferDataType::GpuParticleDynamicConstants),
            "GpuParticleEmitterIndexBuffer" => Ok(ShaderBufferDataType::GpuParticleEmitterIndexBuffer),
            "GpuParticleCountBuffer" => Ok(ShaderBufferDataType::GpuParticleCountBuffer),
            "GpuParticleUpdateBuffer" => Ok(ShaderBufferDataType::GpuParticleUpdateBuffer),
            "PointLightCountBuffer" => Ok(ShaderBufferDataType::PointLightCountBuffer),
            "PointLightIndexBuffer" => Ok(ShaderBufferDataType::PointLightIndexBuffer),
            _ => Err(format!("'{}' is not a valid value for ShaderBufferDataType", s))
        }
    }
}

pub fn register_shader_buffer_data(
    debug_utils_device: &ext::debug_utils::Device,
    shdaer_buffer_create_info: &mut RegisterShaderBufferCreateInfo,
) {
    unsafe {
        let buffer_data_size = shdaer_buffer_create_info._shader_buffer_data_stride
            * shdaer_buffer_create_info._shader_buffer_data_count;
        let uniform_buffer_data = buffer::create_shader_buffer_data(
            &*shdaer_buffer_create_info._device,
            &*shdaer_buffer_create_info._memory_properties,
            debug_utils_device,
            &String::from(format!(
                "{:?}",
                shdaer_buffer_create_info._shader_buffer_data_type
            )),
            shdaer_buffer_create_info._buffer_usage,
            buffer_data_size as vk::DeviceSize,
            shdaer_buffer_create_info._create_buffer_per_swapchain_count,
            shdaer_buffer_create_info._has_staging_buffer,
            shdaer_buffer_create_info._is_device_local,
        );
        (&mut *shdaer_buffer_create_info._shader_buffer_data_map).insert(
            shdaer_buffer_create_info._shader_buffer_data_type,
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

    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::SceneConstants,
            _shader_buffer_data_stride: std::mem::size_of::<SceneConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::ViewConstants,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightData,
            _shader_buffer_data_stride: std::mem::size_of::<LightData>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::PointLightData,
            _shader_buffer_data_stride: std::mem::size_of::<PointLights>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::SSAOConstants,
            _shader_buffer_data_stride: std::mem::size_of::<SSAOConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::AtmosphereConstants,
            _shader_buffer_data_stride: std::mem::size_of::<AtmosphereConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants0,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants1,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants2,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants3,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::LightProbeViewConstants4,
            _shader_buffer_data_stride: std::mem::size_of::<ViewConstants>(),
            ..uniform_buffer_create_info
        },
    );
    register_shader_buffer_data(
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
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::TransformMatrices,
            _shader_buffer_data_stride: std::mem::size_of::<TransformMatrices>(),
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::TransformOffsets,
            _shader_buffer_data_stride: std::mem::size_of::<TransformOffsets>(),
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::BoundBoxInstanceDataBuffer,
            _shader_buffer_data_stride: std::mem::size_of::<BoundBoxInstanceData>(),
            _shader_buffer_data_count: constants::MAX_BOUND_BOX_INSTANCE_COUNT,
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::DebugLineInstanceDataBuffer,
            _shader_buffer_data_stride: std::mem::size_of::<DebugLineInstanceData>(),
            _shader_buffer_data_count: constants::MAX_DEBUG_LINE_INSTANCE_COUNT,
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::FontInstanceDataBuffer,
            _shader_buffer_data_stride: std::mem::size_of::<FontInstanceData>(),
            _shader_buffer_data_count: constants::MAX_FONT_INSTANCE_COUNT,
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::UIRenderDataBuffer,
            _shader_buffer_data_stride: std::mem::size_of::<UIRenderData>(),
            _shader_buffer_data_count: constants::MAX_UI_INSTANCE_COUNT,
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::GpuParticleStaticConstants,
            _shader_buffer_data_stride: std::mem::size_of::<GpuParticleStaticConstants>(),
            _shader_buffer_data_count: unsafe { constants::MAX_EMITTER_COUNT as usize },
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::GpuParticleDynamicConstants,
            _shader_buffer_data_stride: std::mem::size_of::<GpuParticleDynamicConstants>(),
            _shader_buffer_data_count: unsafe { constants::MAX_EMITTER_COUNT as usize },
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
        debug_utils_device,
        &mut RegisterShaderBufferCreateInfo {
            _shader_buffer_data_type: ShaderBufferDataType::GpuParticleEmitterIndexBuffer,
            _shader_buffer_data_stride: std::mem::size_of::<i32>(),
            _shader_buffer_data_count: unsafe { constants::MAX_PARTICLE_COUNT as usize },
            ..storage_buffer_create_info
        },
    );
    register_shader_buffer_data(
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
    register_shader_buffer_data(
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
}

pub fn register_shader_buffer_data_list(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    debug_utils_device: &ext::debug_utils::Device,
    shader_buffer_data_map: &mut ShaderBufferDataMap,
) {
    register_uniform_buffers(device, memory_properties, debug_utils_device, shader_buffer_data_map);
    register_storage_buffers(device, memory_properties, debug_utils_device, shader_buffer_data_map);
}
