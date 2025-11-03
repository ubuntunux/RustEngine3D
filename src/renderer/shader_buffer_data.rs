use std::cmp::{Eq, PartialEq};
use std::collections::HashMap;

use crate::constants;
use crate::vulkan_context::buffer::ShaderBufferData;
use ash::{vk, Device};
use nalgebra::{Matrix4, Vector2, Vector3, Vector4};
use strum_macros::{Display, EnumCount, EnumIter, EnumString};

pub type ShaderBufferDataMap<'a> = HashMap<ShaderBufferDataType, ShaderBufferData<'a>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Display, EnumIter, EnumString, EnumCount)]
pub enum ShaderBufferDataType {
    None,
    SceneConstants,
    ViewConstants,
    LightData,
    PointLightData,
    ShadowAOConstants,
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
    pub _moon_light_color: Vector3<f32>,
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
    pub _elapsed_frame: u32,
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
    pub _inv_capture_height_map_view_projection: Matrix4<f32>,
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
#[derive(Clone)]
pub struct ShadowAOConstants {
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

// precomputed_atmosphere/atmosphere_common.glsl - struct ATMOSPHERE_CONSTANTS
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
    pub _is_device_local: bool,
}
