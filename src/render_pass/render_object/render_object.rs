use ash::vk;
use crate::render_pass::render_object::common;
use crate::render_pass::render_object::common::{USER_BINDING_INDEX0, USER_BINDING_INDEX1, USER_BINDING_INDEX2};
use crate::renderer::push_constants::{PushConstant, PushConstant_RenderObject};
use crate::renderer::renderer_data::RendererData;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;

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
        }
    ]
}

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    let push_constant_data = get_push_constant_data();
    let descriptor_data_create_infos = get_descriptor_data_create_infos();
    common::get_render_pass_data_create_infos(renderer_data, &push_constant_data, &descriptor_data_create_infos)
}