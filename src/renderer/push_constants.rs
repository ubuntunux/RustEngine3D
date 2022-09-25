use std::fmt::Debug;
use std::collections::HashMap;
use ash::{ vk };
use nalgebra::{
    Vector2,
    Vector4,
    Matrix4,
};
use serde::{ Serialize, Deserialize };
use serde_json;

pub const NONE_PUSH_CONSTANT: Option<&()> = None;

pub type PushConstantsMap = HashMap<String, Vec<Box<dyn PushConstant>>>;
pub trait PushConstant: PushConstantClone + PushConstantSize + Debug {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value);
}

pub trait PushConstantClone {
    fn clone_box(&self) -> Box<dyn PushConstant>;
}

impl<T> PushConstantClone for T where T: 'static + PushConstant + Clone, {
    fn clone_box(&self) -> Box<dyn PushConstant> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn PushConstant> {
    fn clone(&self) -> Box<dyn PushConstant> {
        self.clone_box()
    }
}

pub trait PushConstantSize {
    fn get_size(&self) -> u32;
}

impl<T> PushConstantSize for T {
    fn get_size(&self) -> u32 {
        std::mem::size_of::<T>() as u32
    }
}

#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct PushConstant_StaticRenderObject {
    pub _local_matrix: Matrix4<f32>,
    pub _color: Vector4<f32>
}

impl Default for PushConstant_StaticRenderObject {
    fn default() -> PushConstant_StaticRenderObject {
        PushConstant_StaticRenderObject {
            _local_matrix: Matrix4::identity(),
            _color: Vector4::new(1.0, 1.0, 1.0, 1.0)
        }
    }
}

impl PushConstant for PushConstant_StaticRenderObject {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {

    }
}

#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct PushConstant_SkeletalRenderObject {
    pub _local_matrix: Matrix4<f32>,
    pub _local_matrix_prev: Matrix4<f32>,
    pub _bone_matrix_offset: u32,
    pub _bone_matrix_count: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _color: Vector4<f32>
}

impl Default for PushConstant_SkeletalRenderObject {
    fn default() -> PushConstant_SkeletalRenderObject {
        PushConstant_SkeletalRenderObject {
            _local_matrix: Matrix4::identity(),
            _local_matrix_prev: Matrix4::identity(),
            _bone_matrix_offset: 0,
            _bone_matrix_count: 0,
            _reserved0: 0,
            _reserved1: 0,
            _color: Vector4::new(1.0, 1.0, 1.0, 1.0)
        }
    }
}

impl PushConstant for PushConstant_SkeletalRenderObject {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {

    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_BloomHighlight {
    pub _bloom_threshold_min: f32,
    pub _bloom_threshold_max: f32,
    pub _bloom_intensity: f32,
    pub _bloom_scale: f32,
}

impl Default for PushConstant_BloomHighlight {
    fn default() -> PushConstant_BloomHighlight {
        PushConstant_BloomHighlight {
            _bloom_threshold_min: 1.25,
            _bloom_threshold_max: 10.0,
            _bloom_intensity: 0.25,
            _bloom_scale: 1.0,
        }
    }
}

impl PushConstant for PushConstant_BloomHighlight {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}


#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_GaussianBlur {
    pub _blur_scale: Vector2<f32>,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstant_GaussianBlur {
    fn default() -> PushConstant_GaussianBlur {
        PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 1.0),
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

impl PushConstant for PushConstant_GaussianBlur {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}


#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderCopy {
    pub _taget_mip_level: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl Default for PushConstant_RenderCopy {
    fn default() -> PushConstant_RenderCopy {
        PushConstant_RenderCopy {
            _taget_mip_level: 0,
            _reserved0: 0,
            _reserved1: 0,
            _reserved2: 0,
        }
    }
}

impl PushConstant for PushConstant_RenderCopy {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_BlendCubeMap {
    pub _blend_ratio: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl PushConstant for PushConstant_BlendCubeMap {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_RenderColor {
    pub _color: Vector4<f32>
}

impl PushConstant for PushConstant_RenderColor {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderDebug {
    pub _debug_target: u32,
    pub _mip_level: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstant_RenderDebug {
    fn default() -> PushConstant_RenderDebug {
        PushConstant_RenderDebug {
            _debug_target: vk::ImageViewType::TYPE_2D.as_raw() as u32,
            _mip_level: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

impl PushConstant for PushConstant_RenderDebug {
    fn update_push_constant(&mut self, _material_parameters: &serde_json::Value) {
    }
}
