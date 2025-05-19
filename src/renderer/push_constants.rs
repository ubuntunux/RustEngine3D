use std::collections::HashMap;
use std::fmt::Debug;

use ash::vk;
use nalgebra::{Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};
use serde_json;

use crate::utilities::json::convert_json_value_to_push_constant_parameter;

pub enum PushConstantParameter {
    None,
    Int(i32),
    Float(f32),
    Float2(Vector2<f32>),
    Float3(Vector3<f32>),
    Float4(Vector4<f32>),
}

pub type PushConstantsMap = HashMap<String, Vec<Box<dyn PushConstant>>>;
pub trait PushConstant: PushConstantClone + PushConstantSize + PushConstantName + Debug {
    fn get_push_constant_parameter(&self, _key: &str) -> PushConstantParameter {
        panic!("Not implemented.")
    }
    fn set_push_constant_parameter(&mut self, _key: &str, _value: &PushConstantParameter) -> bool {
        false
    }

    fn update_material_parameters(&mut self, _material_parameters: &serde_json::Map<String, serde_json::Value>) -> bool {
        // ex)
        // if let PushConstantParameter::Float4(value) = convert_json_value_to_push_constant_parameter(material_parameters, "_color") {
        //     self._color = value;
        // }
        false
    }
}

pub trait PushConstantClone {
    fn clone_box(&self) -> Box<dyn PushConstant>;
}

impl<T> PushConstantClone for T
where
    T: 'static + PushConstant + Clone,
{
    fn clone_box(&self) -> Box<dyn PushConstant> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn PushConstant> {
    fn clone(&self) -> Box<dyn PushConstant> {
        self.clone_box()
    }
}

pub trait PushConstantName {
    fn get_push_constant_name(&self) -> &str;
}

pub trait PushConstantSize {
    fn get_size(&self) -> u32;
    fn convert_to_bytes(&self) -> &[u8];
}

impl<T> PushConstantSize for T {
    fn get_size(&self) -> u32 {
        std::mem::size_of::<T>() as u32
    }
    fn convert_to_bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts((self as *const T) as *const u8, std::mem::size_of::<T>())
        }
    }
}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct PushConstant_RenderObjectBase {
    pub _transform_offset_index: u32,
    pub _bone_count: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _color: Vector4<f32>,
}

impl Default for PushConstant_RenderObjectBase {
    fn default() -> PushConstant_RenderObjectBase {
        PushConstant_RenderObjectBase {
            _transform_offset_index: 0,
            _bone_count: 0,
            _reserved0: 0,
            _reserved1: 0,
            _color: Vector4::new(1.0, 1.0, 1.0, 1.0),
        }
    }
}

impl PushConstantName for PushConstant_RenderObjectBase {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderObjectBase"
    }
}

impl PushConstant for PushConstant_RenderObjectBase {
    fn set_push_constant_parameter(&mut self, key: &str, value: &PushConstantParameter) -> bool {
        if "_transform_offset_index" == key {
            if let PushConstantParameter::Int(transform_offset_index) = value {
                self._transform_offset_index = *transform_offset_index as u32;
                return true;
            }
        } else if "_bone_count" == key {
            if let PushConstantParameter::Int(bone_count) = value {
                self._bone_count = *bone_count as u32;
                return true;
            }
        }

        false
    }

    fn update_material_parameters(&mut self, material_parameters: &serde_json::Map<String, serde_json::Value>) -> bool {
        if let PushConstantParameter::Float4(value) = convert_json_value_to_push_constant_parameter(material_parameters, "_color") {
            self._color = value;
            return true;
        }
        false
    }
}

#[repr(C)]
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

impl PushConstantName for PushConstant_BloomHighlight {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_BloomHighlight"
    }
}

impl PushConstant for PushConstant_BloomHighlight {}

#[repr(C)]
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

impl PushConstantName for PushConstant_GaussianBlur {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_GaussianBlur"
    }
}

impl PushConstant for PushConstant_GaussianBlur {}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderCopy {
    pub _target_mip_level: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl Default for PushConstant_RenderCopy {
    fn default() -> PushConstant_RenderCopy {
        PushConstant_RenderCopy {
            _target_mip_level: 0,
            _reserved0: 0,
            _reserved1: 0,
            _reserved2: 0,
        }
    }
}

impl PushConstantName for PushConstant_RenderCopy {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderCopy"
    }
}

impl PushConstant for PushConstant_RenderCopy {}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_BlendCubeMap {
    pub _blend_ratio: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl PushConstantName for PushConstant_BlendCubeMap {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_BlendCubeMap"
    }
}

impl PushConstant for PushConstant_BlendCubeMap {}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_RenderColor {
    pub _color: Vector4<f32>,
}

impl PushConstantName for PushConstant_RenderColor {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderColor"
    }
}

impl PushConstant for PushConstant_RenderColor {}

#[repr(C)]
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

impl PushConstantName for PushConstant_RenderDebug {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderDebug"
    }
}

impl PushConstant for PushConstant_RenderDebug {}
