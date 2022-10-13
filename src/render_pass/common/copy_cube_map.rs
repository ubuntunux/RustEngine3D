use std::path::PathBuf;

use ash::vk;
use crate::utilities::system::enum_to_string;
use crate::vulkan_context::render_pass::{
    RenderPassDataCreateInfo,
    PipelineDataCreateInfo,
    PipelinePushConstantData
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorResourceType,
};

use crate::renderer::push_constants::PushConstant_BlendCubeMap;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::render_target::RenderTargetType;

pub fn get_render_pass_data_create_info(_renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("copy_cube_map");
    let input = DescriptorDataCreateInfo {
        _descriptor_binding_index: 0,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColorForward),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };
    let output = DescriptorDataCreateInfo {
        _descriptor_binding_index: 6,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColorForwardPrev),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };
    let blend_input_0 = input.clone();
    let blend_input_1 = output.clone();
    let blend_output = DescriptorDataCreateInfo {
        _descriptor_binding_index: 12,
        _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColor),
        _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
        _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
        _descriptor_image_layer: 0,
        _descriptor_image_mip_level: 0,
        ..Default::default()
    };

    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("copy"),
            _pipeline_compute_shader_file: PathBuf::from("common/copy_cube_map.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo { _descriptor_binding_index: 0, _descriptor_image_layer: 0, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 1, _descriptor_image_layer: 1, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 2, _descriptor_image_layer: 2, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 3, _descriptor_image_layer: 3, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 4, _descriptor_image_layer: 4, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 5, _descriptor_image_layer: 5, ..input.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 6, _descriptor_image_layer: 0, ..output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 7, _descriptor_image_layer: 1, ..output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 8, _descriptor_image_layer: 2, ..output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 9, _descriptor_image_layer: 3, ..output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 10, _descriptor_image_layer: 4, ..output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 11, _descriptor_image_layer: 5, ..output.clone() },
            ],
            ..Default::default()
        },
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("blend"),
            _pipeline_compute_shader_file: PathBuf::from("common/blend_cube_map.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _push_constant_datas: vec![
                PipelinePushConstantData {
                    _stage_flags: vk::ShaderStageFlags::ALL,
                    _offset: 0,
                    _push_constant: Box::new(PushConstant_BlendCubeMap::default())
                }
            ],
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo { _descriptor_binding_index: 0, _descriptor_image_layer: 0, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 1, _descriptor_image_layer: 1, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 2, _descriptor_image_layer: 2, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 3, _descriptor_image_layer: 3, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 4, _descriptor_image_layer: 4, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 5, _descriptor_image_layer: 5, ..blend_input_0.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 6, _descriptor_image_layer: 0, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 7, _descriptor_image_layer: 1, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 8, _descriptor_image_layer: 2, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 9, _descriptor_image_layer: 3, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 10, _descriptor_image_layer: 4, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 11, _descriptor_image_layer: 5, ..blend_input_1.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 12, _descriptor_image_layer: 0, ..blend_output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 13, _descriptor_image_layer: 1, ..blend_output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 14, _descriptor_image_layer: 2, ..blend_output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 15, _descriptor_image_layer: 3, ..blend_output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 16, _descriptor_image_layer: 4, ..blend_output.clone() },
                DescriptorDataCreateInfo { _descriptor_binding_index: 17, _descriptor_image_layer: 5, ..blend_output.clone() },
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