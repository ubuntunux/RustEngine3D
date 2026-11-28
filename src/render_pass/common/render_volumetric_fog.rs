use std::path::PathBuf;

use ash::vk;

use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::shader_buffer_data::ShaderBufferDataType;
use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::{PipelineDataCreateInfo, RenderPassDataCreateInfo};

pub const SEMANTIC_FOG_HISTORY: &str = "FOG_HISTORY";
pub const SEMANTIC_FOG_INJECT_OUT: &str = "FOG_INJECT_OUT";
pub const SEMANTIC_FOG_INJECT_IN: &str = "FOG_INJECT_IN";

pub fn get_render_pass_data_create_info(_renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("render_volumetric_fog");
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("inject"),
            _pipeline_compute_shader_file: PathBuf::from("common/volumetric_fog_inject.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::SceneConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::ViewConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::LightData),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::PointLightData),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 4,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::LocalLightingData),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 5,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::AtmosphereConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 6,
                    _descriptor_name: enum_to_string(&RenderTargetType::Shadow),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 7,
                    _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColor),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_semantic: SEMANTIC_FOG_HISTORY.to_string(),
                    _descriptor_binding_index: 8,
                    _descriptor_name: enum_to_string(&RenderTargetType::VolumetricFogInjectPrev),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_semantic: SEMANTIC_FOG_INJECT_OUT.to_string(),
                    _descriptor_binding_index: 9,
                    _descriptor_name: enum_to_string(&RenderTargetType::VolumetricFogInject),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 10,
                    _descriptor_name: String::from("texture_cloud"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
            ],
            ..Default::default()
        },
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("integrate"),
            _pipeline_compute_shader_file: PathBuf::from("common/volumetric_fog_integrate.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::SceneConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::ViewConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_semantic: SEMANTIC_FOG_INJECT_IN.to_string(),
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&RenderTargetType::VolumetricFogInject),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&RenderTargetType::VolumetricFogAccum),
                    _descriptor_resource_type: DescriptorResourceType::StorageRenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
            ],
            ..Default::default()
        },
    ];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: render_pass_name,
        _pipeline_data_create_infos: pipeline_data_create_infos,
        ..Default::default()
    }
}
