use std::cmp::{
    Eq,
    PartialEq
};
use std::collections::HashMap;

use ash::{
    vk,
    Device,
};

use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix4,
};

use crate::constants;
use crate::vulkan_context::buffer::{ self, ShaderBufferData };

pub type ShaderBufferDataMap = HashMap<ShaderBufferDataType, ShaderBufferData>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ShaderBufferDataType {
    SceneConstants,
    ViewConstants,
    LightConstants,
    SSAOConstants,
    BoneMatrices,
    AtmosphereConstants,
}

// scene_constants.glsl - struct SCENE_CONSTANTS
#[derive(Clone, Debug, Default)]
pub struct SceneConstants {
    pub _screen_size: Vector2<f32>,
    pub _backbuffer_size: Vector2<f32>,
    pub _time: f32,
    pub _delta_time: f32,
    pub _sea_height: f32,
    pub _scene_constants_dummy0: i32,
}

// scene_constants.glsl - struct VIEW_CONSTANTS
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
    pub _camera_position: Vector3<f32>,
    pub _jitter_frame: i32,
    pub _camera_position_prev: Vector3<f32>,
    pub _viewconstants_dummy0: f32,
    pub _near_far: Vector2<f32>,
    pub _jitter_delta: Vector2<f32>,
    pub _jitter_offset: Vector2<f32>,
    pub _viewconstants_dummy1: f32,
    pub _viewconstants_dummy2: f32,
}

// scene_constants.glsl - struct LIGHT_CONSTANTS
#[derive(Clone, Debug)]
pub struct LightConstants {
  pub _shadow_view_projection: Matrix4<f32>,
  pub _light_position: Vector3<f32>,
  pub _shadow_exp: f32,
  pub _light_direction: Vector3<f32>,
  pub _shadow_bias: f32,
  pub _light_color: Vector3<f32>,
  pub _shadow_samples: i32,
  pub _shadow_dimensions: Vector4<f32>, // width height near far
}

// render_ssao.frag - SSAOConstants
#[derive(Clone)]
pub struct SSAOConstants {
    pub _ssao_kernel_samples: [Vector4<f32>; constants::SSAO_KERNEL_SIZE],
}

#[derive(Clone)]
pub struct BoneMatrices {
    pub _bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
}

// pecomputed_atmosphere/atmosphere_common.glsl - struct ATMOSPHERE_CONSTANTS
#[derive(Clone, Debug)]
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
    pub _reserved0: i32,
}


// Interfaces
impl std::fmt::Display for ShaderBufferDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for ShaderBufferDataType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SceneConstants" => Ok(ShaderBufferDataType::SceneConstants),
            "ViewConstants" => Ok(ShaderBufferDataType::ViewConstants),
            "LightConstants" => Ok(ShaderBufferDataType::LightConstants),
            "SSAOConstants" => Ok(ShaderBufferDataType::SSAOConstants),
            "BoneMatrices" => Ok(ShaderBufferDataType::BoneMatrices),
            "AtmosphereConstants" => Ok(ShaderBufferDataType::AtmosphereConstants),
            _ => Err(format!("'{}' is not a valid value for ShaderBufferDataType", s)),
        }
    }
}

impl Default for LightConstants {
    fn default() -> LightConstants {
        LightConstants {
            _shadow_view_projection: Matrix4::identity(),
            _light_position: Vector3::zeros(),
            _shadow_exp: constants::SHADOW_EXP,
            _light_direction: Vector3::new(-std::f32::consts::PI * 0.5, 0.0, 0.0),
            _shadow_bias: constants::SHADOW_BIAS,
            _light_color: Vector3::new(10.0, 10.0, 10.0),
            _shadow_samples: constants::SHADOW_SAMPLES,
            _shadow_dimensions: Vector4::new(
                constants::SHADOW_DISTANCE * 2.0,
                constants::SHADOW_DISTANCE * 2.0,
                -constants::SHADOW_DEPTH,
                constants::SHADOW_DEPTH
            )
        }
    }
}

impl Default for BoneMatrices {
    fn default() -> BoneMatrices {
        BoneMatrices {
            _bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
        }
    }
}

pub fn regist_shader_buffer_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    shader_buffer_data_map: &mut ShaderBufferDataMap,
    shader_buffer_data_type: ShaderBufferDataType,
    buffer_usage: vk::BufferUsageFlags,
    shader_buffer_data_size: usize
) {
    let uniform_buffer_data = buffer::create_shader_buffer_data(
        device,
        memory_properties,
        &String::from(format!("{:?}", shader_buffer_data_type)),
        buffer_usage,
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
        constants::SWAPCHAIN_IMAGE_COUNT,
        shader_buffer_data_size as vk::DeviceSize
    );
    shader_buffer_data_map.insert(shader_buffer_data_type.clone(), uniform_buffer_data);
}

pub fn regist_shader_buffer_datas(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    shader_buffer_data_map: &mut ShaderBufferDataMap,
) {
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::SceneConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SceneConstants>());
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::ViewConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>());
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<LightConstants>());
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::SSAOConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SSAOConstants>());
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::BoneMatrices, vk::BufferUsageFlags::STORAGE_BUFFER, std::mem::size_of::<BoneMatrices>());
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::AtmosphereConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<AtmosphereConstants>());
}