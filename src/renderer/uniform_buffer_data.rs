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
    Matrix,
    ArrayStorage,
    U4,
    U64,
};

use crate::constants;
use crate::vulkan_context::uniform_buffer;
use crate::vulkan_context::uniform_buffer::{
    UniformBufferData,
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UniformBufferType {
    SceneConstants,
    ViewConstants,
    LightConstants,
    SSAOConstants,
    BoneMatrices,
}

impl std::fmt::Display for UniformBufferType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for UniformBufferType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SceneConstants" => Ok(UniformBufferType::SceneConstants),
            "ViewConstants" => Ok(UniformBufferType::ViewConstants),
            "LightConstants" => Ok(UniformBufferType::LightConstants),
            "SSAOConstants" => Ok(UniformBufferType::SSAOConstants),
            "BoneMatrices" => Ok(UniformBufferType::BoneMatrices),
            _ => Err(format!("'{}' is not a valid value for UniformBufferType", s)),
        }
    }
}

pub type UniformBufferDataMap = HashMap<UniformBufferType, UniformBufferData>;

// scene_constants.glsl - struct SCENE_CONSTANTS
#[derive(Clone, Debug, Default)]
pub struct SceneConstants {
    pub _screen_size: Vector2<f32>,
    pub _backbuffer_size: Vector2<f32>,
    pub _time: f32,
    pub _delta_time: f32,
    pub _jitter_frame: f32,
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
    pub _camera_position: Vector3<f32>,
    pub _viewconstants_dummy0: f32,
    pub _camera_position_prev: Vector3<f32>,
    pub _viewconstants_dummy1: f32,
    pub _near_far: Vector2<f32>,
    pub _jitter_delta: Vector2<f32>,
    pub _jitter_offset: Vector2<f32>,
    pub _viewconstants_dummy2: f32,
    pub _viewconstants_dummy3: f32,
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
type Matrix4x64f = Matrix<f32, U4, U64, ArrayStorage<f32, U4, U64>>;
#[derive(Clone, Debug, Default)]
pub struct SSAOConstants {
    pub _ssao_kernel_samples: Matrix4x64f,
}

#[derive(Clone)]
pub struct BoneMatrices {
    pub _prev_bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
    pub _bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
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
            _prev_bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
            _bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
        }
    }
}

pub fn regist_uniform_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    uniform_buffer_data_map: &mut UniformBufferDataMap,
    uniform_buffer_data_type: UniformBufferType,
    uniform_buffer_data_size: usize
) {
    let uniform_buffer_data = uniform_buffer::create_uniform_buffer_data(
        device,
        memory_properties,
        &String::from(format!("{:?}", uniform_buffer_data_type)),
        uniform_buffer_data_size as vk::DeviceSize
    );
    uniform_buffer_data_map.insert(uniform_buffer_data_type.clone(), uniform_buffer_data);
}

pub fn regist_uniform_datas(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    uniform_buffer_data_map: &mut UniformBufferDataMap,
) {
    regist_uniform_data(device, memory_properties, uniform_buffer_data_map, UniformBufferType::SceneConstants, std::mem::size_of::<SceneConstants>());
    regist_uniform_data(device, memory_properties, uniform_buffer_data_map, UniformBufferType::ViewConstants, std::mem::size_of::<ViewConstants>());
    regist_uniform_data(device, memory_properties, uniform_buffer_data_map, UniformBufferType::LightConstants, std::mem::size_of::<LightConstants>());
    regist_uniform_data(device, memory_properties, uniform_buffer_data_map, UniformBufferType::SSAOConstants, std::mem::size_of::<SSAOConstants>());
    regist_uniform_data(device, memory_properties, uniform_buffer_data_map, UniformBufferType::BoneMatrices, std::mem::size_of::<BoneMatrices>());
}