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
use crate::renderer::camera::{ CameraObjectData };
use crate::renderer::font::{ FontInstanceData };
use crate::renderer::ui::{ UIInstanceData };
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
    LightProbeViewConstants0,
    LightProbeViewConstants1,
    LightProbeViewConstants2,
    LightProbeViewConstants3,
    LightProbeViewConstants4,
    LightProbeViewConstants5,
    FontInstanceData,
    UIInstanceData,
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
    pub _capture_height_map_view_projection: Matrix4<f32>,
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
}

impl Default for LightConstants {
    fn default() -> LightConstants {
        LightConstants {
            _shadow_view_projection: Matrix4::identity(),
            _light_position: Vector3::zeros(),
            _shadow_exp: constants::SHADOW_EXP,
            _light_direction: Vector3::new(-std::f32::consts::PI * 0.5, 0.0, 0.0),
            _shadow_bias: constants::SHADOW_BIAS,
            _light_color: Vector3::new(1.0, 1.0, 1.0),
            _shadow_samples: constants::SHADOW_SAMPLES,
        }
    }
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

impl Default for BoneMatrices {
    fn default() -> BoneMatrices {
        BoneMatrices {
            _bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
        }
    }
}

// pecomputed_atmosphere/atmosphere_common.glsl - struct ATMOSPHERE_CONSTANTS
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
            "LightProbeViewConstants0" => Ok(ShaderBufferDataType::LightProbeViewConstants0),
            "LightProbeViewConstants1" => Ok(ShaderBufferDataType::LightProbeViewConstants1),
            "LightProbeViewConstants2" => Ok(ShaderBufferDataType::LightProbeViewConstants2),
            "LightProbeViewConstants3" => Ok(ShaderBufferDataType::LightProbeViewConstants3),
            "LightProbeViewConstants4" => Ok(ShaderBufferDataType::LightProbeViewConstants4),
            "LightProbeViewConstants5" => Ok(ShaderBufferDataType::LightProbeViewConstants5),
            "FontInstanceData" => Ok(ShaderBufferDataType::FontInstanceData),
            "UIInstanceData" => Ok(ShaderBufferDataType::UIInstanceData),
            _ => Err(format!("'{}' is not a valid value for ShaderBufferDataType", s)),
        }
    }
}

pub fn regist_shader_buffer_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    shader_buffer_data_map: &mut ShaderBufferDataMap,
    shader_buffer_data_type: ShaderBufferDataType,
    buffer_usage: vk::BufferUsageFlags,
    shader_buffer_data_size: usize,
    has_staging_buffer: bool,
) {
    let uniform_buffer_data = buffer::create_shader_buffer_data(
        device,
        memory_properties,
        &String::from(format!("{:?}", shader_buffer_data_type)),
        buffer_usage | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST,
        has_staging_buffer,
        shader_buffer_data_size as vk::DeviceSize
    );
    shader_buffer_data_map.insert(shader_buffer_data_type.clone(), uniform_buffer_data);
}

pub fn regist_shader_buffer_datas(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    shader_buffer_data_map: &mut ShaderBufferDataMap,
) {
    let has_staging_buffer: bool = false;
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::SceneConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SceneConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::ViewConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<LightConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::SSAOConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SSAOConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::BoneMatrices, vk::BufferUsageFlags::STORAGE_BUFFER, std::mem::size_of::<BoneMatrices>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::AtmosphereConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<AtmosphereConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants0, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants1, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants2, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants3, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants4, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::LightProbeViewConstants5, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::FontInstanceData, vk::BufferUsageFlags::STORAGE_BUFFER, std::mem::size_of::<FontInstanceData>(), has_staging_buffer);
    regist_shader_buffer_data(device, memory_properties, shader_buffer_data_map, ShaderBufferDataType::UIInstanceData, vk::BufferUsageFlags::STORAGE_BUFFER, std::mem::size_of::<UIInstanceData>(), has_staging_buffer);
}

impl SceneConstants {
    pub fn update_scene_constants(&mut self, screen_width: u32, screen_height: u32, elapsed_time: f64, delta_time: f64, sea_height: f32) {
            self._screen_size = Vector2::new(screen_width as f32, screen_height as f32);
            self._backbuffer_size = self._screen_size.into();
            self._time = elapsed_time as f32;
            self._delta_time = delta_time as f32;
            self._sea_height = sea_height;
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
        self._inv_view_origin_projection_jitter = camera_data._inv_view_origin_projection_jitter.into();
        self._view_origin_projection_prev_jitter = camera_data._view_origin_projection_prev_jitter.into();
        self._camera_position = camera_data._transform_object._position.clone() as Vector3<f32>;
        self._jitter_frame = camera_data._jitter_frame;
        self._camera_position_prev = camera_data._transform_object._prev_position.clone() as Vector3<f32>;
        self._viewconstants_dummy0 = 0.0;
        self._near_far = Vector2::new(camera_data._near, camera_data._far);
        self._jitter_delta = camera_data._jitter_delta.into();
        self._jitter_offset = camera_data._jitter.into();
        self._viewconstants_dummy1 = 0.0;
        self._viewconstants_dummy2 = 0.0;
    }
}