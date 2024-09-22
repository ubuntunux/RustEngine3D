use std::path::PathBuf;

use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::{
    PipelineDataCreateInfo, PipelinePushConstantData, RenderPassDataCreateInfo,
};
use ash::vk;

use crate::renderer::push_constants::PushConstant_BlendCubeMap;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;

pub fn get_render_pass_data_create_info(_renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("copy_cube_map");
    let image_input = DescriptorDataCreateInfo {
        _descriptor_binding_index: 0,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColorForward),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };
    let image_output = DescriptorDataCreateInfo {
        _descriptor_binding_index: 6,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColorForwardPrev),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };
    let blend_input_0 = image_input.clone();
    let blend_input_1 = image_output.clone();
    let blend_output = DescriptorDataCreateInfo {
        _descriptor_binding_index: 12,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColor),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };

    let mut copy_cube_map_descriptor_data_create_infos: Vec<DescriptorDataCreateInfo> = Vec::new();
    for layer in 0..6 {
        copy_cube_map_descriptor_data_create_infos.push(
            DescriptorDataCreateInfo {
                _descriptor_semantic: String::from(format!("IMAGE_INPUT_{}", layer)),
                _descriptor_binding_index: layer,
                _descriptor_image_layer: layer,
                ..image_input.clone()
            }
        )
    }
    for layer in 0..6 {
        copy_cube_map_descriptor_data_create_infos.push(
            DescriptorDataCreateInfo {
                _descriptor_semantic: String::from(format!("IMAGE_OUTPUT_{}", layer)),
                _descriptor_binding_index: 6 + layer,
                _descriptor_image_layer: layer,
                ..image_output.clone()
            }
        )
    }

    let mut blend_cube_map_descriptor_data_create_infos: Vec<DescriptorDataCreateInfo> = Vec::new();
    for layer in 0..6 {
        blend_cube_map_descriptor_data_create_infos.push(
            DescriptorDataCreateInfo {
                _descriptor_semantic: String::from(format!("IMAGE_INPUT_0_{}", layer)),
                _descriptor_binding_index: layer,
                _descriptor_image_layer: layer,
                ..blend_input_0.clone()
            }
        )
    }
    for layer in 0..6 {
        blend_cube_map_descriptor_data_create_infos.push(
            DescriptorDataCreateInfo {
                _descriptor_semantic: String::from(format!("IMAGE_INPUT_1_{}", layer)),
                _descriptor_binding_index: 6 + layer,
                _descriptor_image_layer: layer,
                ..blend_input_1.clone()
            }
        )
    }
    for layer in 0..6 {
        blend_cube_map_descriptor_data_create_infos.push(
            DescriptorDataCreateInfo {
                _descriptor_semantic: String::from(format!("IMAGE_OUTPUT_{}", layer)),
                _descriptor_binding_index: 12 + layer,
                _descriptor_image_layer: layer,
                ..blend_output.clone()
            }
        )
    }

    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("copy"),
            _pipeline_compute_shader_file: PathBuf::from("common/copy_cube_map.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: copy_cube_map_descriptor_data_create_infos,
            ..Default::default()
        },
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("blend"),
            _pipeline_compute_shader_file: PathBuf::from("common/blend_cube_map.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _push_constant_data_list: vec![PipelinePushConstantData {
                _stage_flags: vk::ShaderStageFlags::ALL,
                _offset: 0,
                _push_constant: Box::new(PushConstant_BlendCubeMap::default()),
            }],
            _descriptor_data_create_infos: blend_cube_map_descriptor_data_create_infos,
            ..Default::default()
        },
    ];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: render_pass_name.clone(),
        _pipeline_data_create_infos: pipeline_data_create_infos,
        ..Default::default()
    }
}
