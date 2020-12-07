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
use crate::vulkan_context::buffer::{ self, BufferDataInfo };

pub type BufferDataInfoMap = HashMap<BufferDataType, BufferDataInfo>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum BufferDataType {
    SceneConstants,
    ViewConstants,
    LightConstants,
    SSAOConstants,
    BoneMatrices,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_StaticRenderObject {
    pub _local_matrix: Matrix4<f32>,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_SkeletalRenderObject {
    pub _local_matrix: Matrix4<f32>,
    pub _bone_matrix_offset: u32,
    pub _bone_matrix_count: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstants_BloomHighlight {
    pub _bloom_threshold_min: f32,
    pub _bloom_threshold_max: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

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
#[derive(Clone)]
pub struct SSAOConstants {
    pub _ssao_kernel_samples: [Vector4<f32>; constants::SSAO_KERNEL_SIZE],
}

#[derive(Clone)]
pub struct BoneMatrices {
    pub _bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
}

// Interfaces
impl std::fmt::Display for BufferDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for BufferDataType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SceneConstants" => Ok(BufferDataType::SceneConstants),
            "ViewConstants" => Ok(BufferDataType::ViewConstants),
            "LightConstants" => Ok(BufferDataType::LightConstants),
            "SSAOConstants" => Ok(BufferDataType::SSAOConstants),
            "BoneMatrices" => Ok(BufferDataType::BoneMatrices),
            _ => Err(format!("'{}' is not a valid value for BufferDataType", s)),
        }
    }
}

impl Default for PushConstants_StaticRenderObject {
    fn default() -> PushConstants_StaticRenderObject {
        PushConstants_StaticRenderObject {
            _local_matrix: Matrix4::identity(),
        }
    }
}
impl Default for PushConstants_SkeletalRenderObject {
    fn default() -> PushConstants_SkeletalRenderObject {
        PushConstants_SkeletalRenderObject {
            _local_matrix: Matrix4::identity(),
            _bone_matrix_offset: 0,
            _bone_matrix_count: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

impl Default for PushConstants_BloomHighlight {
    fn default() -> PushConstants_BloomHighlight {
        PushConstants_BloomHighlight {
            _bloom_threshold_min: 0.0,
            _bloom_threshold_max: 0.0,
            _reserved0: 0,
            _reserved1: 0,
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

pub fn regist_buffer_data_info(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    buffer_data_info_map: &mut BufferDataInfoMap,
    buffer_data_info_type: BufferDataType,
    buffer_usage: vk::BufferUsageFlags,
    buffer_data_info_size: usize
) {
    let uniform_buffer_data = buffer::create_buffer_data_info(
        device,
        memory_properties,
        &String::from(format!("{:?}", buffer_data_info_type)),
        buffer_usage,
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
        constants::SWAPCHAIN_IMAGE_COUNT,
        buffer_data_info_size as vk::DeviceSize
    );
    buffer_data_info_map.insert(buffer_data_info_type.clone(), uniform_buffer_data);
}

pub fn regist_buffer_data_infos(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    buffer_data_info_map: &mut BufferDataInfoMap,
) {
    regist_buffer_data_info(device, memory_properties, buffer_data_info_map, BufferDataType::SceneConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SceneConstants>());
    regist_buffer_data_info(device, memory_properties, buffer_data_info_map, BufferDataType::ViewConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<ViewConstants>());
    regist_buffer_data_info(device, memory_properties, buffer_data_info_map, BufferDataType::LightConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<LightConstants>());
    regist_buffer_data_info(device, memory_properties, buffer_data_info_map, BufferDataType::SSAOConstants, vk::BufferUsageFlags::UNIFORM_BUFFER, std::mem::size_of::<SSAOConstants>());
    regist_buffer_data_info(device, memory_properties, buffer_data_info_map, BufferDataType::BoneMatrices, vk::BufferUsageFlags::STORAGE_BUFFER, std::mem::size_of::<BoneMatrices>());
}