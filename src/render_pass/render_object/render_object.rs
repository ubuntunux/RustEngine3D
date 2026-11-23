use crate::render_pass::render_object::common;
use crate::render_pass::render_object::common::{
    USER_BINDING_INDEX0, USER_BINDING_INDEX1, USER_BINDING_INDEX2, USER_BINDING_INDEX3,
};
use crate::renderer::push_constants::{
    PushConstant, PushConstant_RenderObjectBase, PushConstantName, PushConstantParameter,
};
use crate::renderer::renderer_data::RendererData;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;
use ash::vk;
use nalgebra::Vector4;
use serde::{Deserialize, Serialize};

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct PushConstant_RenderObject {
    pub push_constant_base: PushConstant_RenderObjectBase,
    pub _face_uv_rect: Vector4<f32>,
}

impl Default for PushConstant_RenderObject {
    fn default() -> PushConstant_RenderObject {
        PushConstant_RenderObject {
            push_constant_base: PushConstant_RenderObjectBase::default(),
            _face_uv_rect: Vector4::zeros(),
        }
    }
}

impl PushConstantName for PushConstant_RenderObject {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderObject"
    }
}

impl PushConstant for PushConstant_RenderObject {
    fn get_push_constant_parameter(&self, key: &str) -> PushConstantParameter {
        if "_face_uv_rect" == key {
            return PushConstantParameter::Float4(self._face_uv_rect);
        }
        self.push_constant_base.get_push_constant_parameter(key)
    }
    fn set_push_constant_parameter(&mut self, key: &str, value: &PushConstantParameter) -> bool {
        if "_face_uv_rect" == key {
            if let PushConstantParameter::Float4(rect) = value {
                self._face_uv_rect = *rect;
            }
            return true;
        }
        self.push_constant_base.set_push_constant_parameter(key, value)
    }
}

pub fn get_push_constant_data() -> Box<dyn PushConstant> {
    Box::new(PushConstant_RenderObject::default())
}

pub fn get_descriptor_data_create_infos() -> Vec<DescriptorDataCreateInfo> {
    vec![
        DescriptorDataCreateInfo {
            _descriptor_binding_index: USER_BINDING_INDEX0,
            _descriptor_name: String::from("textureBase"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: USER_BINDING_INDEX1,
            _descriptor_name: String::from("textureMaterial"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: USER_BINDING_INDEX2,
            _descriptor_name: String::from("textureNormal"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: USER_BINDING_INDEX3,
            _descriptor_name: String::from("textureFace"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
    ]
}

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    common::get_render_pass_data_create_infos(
        renderer_data,
        vk::CullModeFlags::BACK,
        "render_object",
        "render_object/render_object.vert",
        "render_object/render_object.frag",
        &get_push_constant_data(),
        &get_descriptor_data_create_infos(),
    )
}
