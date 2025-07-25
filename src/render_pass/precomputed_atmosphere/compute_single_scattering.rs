use std::path::PathBuf;

use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::framebuffer::{self, FramebufferDataCreateInfo, RenderTargetInfo};
use crate::vulkan_context::geometry_buffer::{VertexData, VertexDataBase};
use crate::vulkan_context::render_pass::{
    ImageAttachmentDescription, PipelineDataCreateInfo, PipelinePushConstantData,
    RenderPassDataCreateInfo,
};
use crate::vulkan_context::vulkan_context::{self, BlendMode};
use ash::vk;

use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::shader_buffer_data::ShaderBufferDataType;
use crate::scene::precomputed_atmosphere::{
    PushConstant_PrecomputedAtmosphere, DEFAULT_USE_COMBINED_TEXTURES,
};

pub fn get_framebuffer_data_create_info(renderer_data: &RendererData) -> FramebufferDataCreateInfo {
    let render_target0 = renderer_data
        .get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_RAYLEIGH_SCATTERING);
    let render_target1 = renderer_data
        .get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_MIE_SCATTERING);
    let render_target2 =
        renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_SCATTERING);
    let render_target3 = renderer_data
        .get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING);
    let render_target_infos: [RenderTargetInfo; 4] = [
        RenderTargetInfo {
            _texture_data: render_target0,
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        },
        RenderTargetInfo {
            _texture_data: render_target1,
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        },
        RenderTargetInfo {
            _texture_data: render_target2,
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        },
        RenderTargetInfo {
            _texture_data: render_target3,
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        },
    ];
    framebuffer::create_framebuffer_data_create_info(&render_target_infos, &[], &[])
}

pub fn get_render_pass_data_create_info(renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("compute_single_scattering");
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data);
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info
        ._framebuffer_color_attachment_formats
        .iter()
    {
        color_attachment_descriptions.push(ImageAttachmentDescription {
            _attachment_image_format: *format,
            _attachment_load_operation: vk::AttachmentLoadOp::LOAD,
            _attachment_store_operation: vk::AttachmentStoreOp::STORE,
            _attachment_initial_layout: vk::ImageLayout::GENERAL,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            ..Default::default()
        });
    }
    let subpass_dependencies = vec![vk::SubpassDependency {
        src_subpass: vk::SUBPASS_EXTERNAL,
        dst_subpass: 0,
        src_stage_mask: vk::PipelineStageFlags::TOP_OF_PIPE,
        src_access_mask: vk::AccessFlags::empty(),
        dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
        dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
        dependency_flags: vk::DependencyFlags::BY_REGION,
    }];

    let pipeline_data_create_info = PipelineDataCreateInfo {
        _pipeline_vertex_shader_file: PathBuf::from(
            "precomputed_atmosphere/render_atmosphere.vert",
        ),
        _pipeline_fragment_shader_file: PathBuf::from(
            "precomputed_atmosphere/compute_single_scattering.frag",
        ),
        _pipeline_shader_defines: vec![format!(
            "COMBINED_SCATTERING_TEXTURES={:?}",
            if DEFAULT_USE_COMBINED_TEXTURES { 1 } else { 0 }
        )],
        _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        _pipeline_color_blend_modes: vec![
            vulkan_context::get_color_blend_mode(BlendMode::None);
            color_attachment_descriptions.len()
        ],
        _pipeline_cull_mode: vk::CullModeFlags::BACK,
        _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
        _vertex_input_bind_descriptions: VertexData::get_vertex_input_binding_descriptions(),
        _vertex_input_attribute_descriptions:
            VertexData::create_vertex_input_attribute_descriptions(),
        _push_constant_data_list: vec![PipelinePushConstantData {
            _stage_flags: vk::ShaderStageFlags::ALL,
            _offset: 0,
            _push_constant: Box::new(PushConstant_PrecomputedAtmosphere::default()),
        }],
        _descriptor_data_create_infos: vec![
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 0,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::SceneConstants),
                _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 1,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::ViewConstants),
                _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 8,
                _descriptor_name: enum_to_string(
                    &RenderTargetType::PRECOMPUTED_ATMOSPHERE_TRANSMITTANCE,
                ),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
        ],
        ..Default::default()
    };

    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("default"),
            ..pipeline_data_create_info.clone()
        },
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("additive"),
            _pipeline_color_blend_modes: vec![
                vulkan_context::get_color_blend_mode(BlendMode::None),
                vulkan_context::get_color_blend_mode(BlendMode::None),
                vulkan_context::get_color_blend_mode(BlendMode::Additive),
                vulkan_context::get_color_blend_mode(BlendMode::Additive),
            ],
            ..pipeline_data_create_info.clone()
        },
    ];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: render_pass_name.clone(),
        _render_pass_framebuffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: color_attachment_descriptions,
        _depth_attachment_descriptions: Vec::new(),
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}
