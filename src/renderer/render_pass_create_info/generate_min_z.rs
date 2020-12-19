use std::path::PathBuf;

use ash::{
    vk,
};

use crate::renderer::renderer::RendererData;
use crate::renderer::render_target::RenderTargetType;
use crate::vulkan_context::render_pass::{
    RenderPassDataCreateInfo,
    PipelineDataCreateInfo,
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorResourceType,
};
use crate::utilities::system::{ enum_to_string };

pub fn get_render_pass_data_create_info(renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("generate_min_z");
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("generate_min_z"),
            _pipeline_compute_shader_file: PathBuf::from("generate_min_z.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                },
            ],
            ..Default::default()
        }
    ];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: render_pass_name.clone(),
        _pipeline_data_create_infos: pipeline_data_create_infos,
        ..Default::default()
    }
}