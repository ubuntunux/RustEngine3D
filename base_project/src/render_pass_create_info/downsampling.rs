use std::path::PathBuf;

use ash::vk;
use rust_engine_3d::utilities::system::enum_to_string;
use rust_engine_3d::vulkan_context::render_pass::{
    RenderPassDataCreateInfo,
    PipelineDataCreateInfo,
};
use rust_engine_3d::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorResourceType,
};

use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer::Renderer;

pub fn get_render_pass_data_create_info(_renderer: &Renderer) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("downsampling");
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("downsampling"),
            _pipeline_compute_shader_file: PathBuf::from("downsampling.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    _descriptor_image_mip_level: 0,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    _descriptor_image_mip_level: 1,
                    ..Default::default()
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