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

use crate::effect::effect_manager::PushConstant_UpdateGpuParticle;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::shader_buffer_datas::ShaderBufferDataType;


pub fn get_render_pass_data_create_info(_renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("process_gpu_particle");
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("compute_gpu_particle_count"),
            _pipeline_compute_shader_file: PathBuf::from("effect/compute_gpu_particle_count.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _push_constant_datas: vec![
                PipelinePushConstantData {
                    _stage_flags: vk::ShaderStageFlags::ALL,
                    _offset: 0,
                    _push_constant: Box::new(PushConstant_UpdateGpuParticle::default())
                }
            ],
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
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleStaticConstants),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleDynamicConstants),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleCountBuffer),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
            ],
            ..Default::default()
        },
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("update_gpu_particle"),
            _pipeline_compute_shader_file: PathBuf::from("effect/update_gpu_particle.comp"),
            _pipeline_bind_point: vk::PipelineBindPoint::COMPUTE,
            _push_constant_datas: vec![
                PipelinePushConstantData {
                    _stage_flags: vk::ShaderStageFlags::ALL,
                    _offset: 0,
                    _push_constant: Box::new(PushConstant_UpdateGpuParticle::default())
                }
            ],
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
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneDepth),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&RenderTargetType::CaptureHeightMap),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 4,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleStaticConstants),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 5,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleDynamicConstants),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 6,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleEmitterIndexBuffer),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 7,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleCountBuffer),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
                    ..Default::default()
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 8,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::GpuParticleUpdateBuffer),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::COMPUTE,
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