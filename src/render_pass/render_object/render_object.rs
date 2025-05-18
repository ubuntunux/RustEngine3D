use ash::vk;
use crate::render_pass::render_object::common;
use crate::render_pass::render_object::common::{USER_BINDING_INDEX0, USER_BINDING_INDEX1, USER_BINDING_INDEX2};
use crate::renderer::push_constants::PushConstant_RenderObject;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

pub fn get_push_constant_data_list() -> Vec<PipelinePushConstantData> {
    vec![PipelinePushConstantData {
        _stage_flags: vk::ShaderStageFlags::ALL,
        _offset: 0,
        _push_constant: Box::new(PushConstant_RenderObject::default()),
    }]
}

pub fn get_descriptor_data_create_infos() -> Vec<DescriptorDataCreateInfo> {
    let descriptor_data_create_infos = common::get_descriptor_data_create_infos();
    [descriptor_data_create_infos, vec![
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
    ]].concat()
}
