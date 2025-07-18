use std::path::PathBuf;

use ash::vk;

use crate::vulkan_context::framebuffer::{self, FramebufferDataCreateInfo, RenderTargetInfo};
use crate::vulkan_context::geometry_buffer::{VertexData, VertexDataBase};
use crate::vulkan_context::render_pass::{
    DepthStencilStateCreateInfo, ImageAttachmentDescription, PipelineDataCreateInfo,
    RenderPassDataCreateInfo,
};
use crate::vulkan_context::vulkan_context::{self, BlendMode};

use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;

pub fn get_framebuffer_data_create_info(
    renderer_data: &RendererData,
    render_target_formats: &[vk::Format],
    depth_format: vk::Format,
) -> FramebufferDataCreateInfo {
    let mut color_render_targets = Vec::new();
    for render_target_format in render_target_formats.iter() {
        let render_target_type = match *render_target_format {
            vk::Format::R8G8B8A8_UNORM => RenderTargetType::SceneAlbedo,
            vk::Format::R16G16_SFLOAT => RenderTargetType::SceneVelocity,
            vk::Format::R16G16B16A16_SFLOAT => RenderTargetType::SceneColor,
            vk::Format::R32_SFLOAT => RenderTargetType::HierarchicalMinZ,
            vk::Format::R32G32B32A32_SFLOAT => {
                RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING
            }
            _ => panic!("Not implemented."),
        };
        color_render_targets.push(RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(render_target_type),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: Some(vulkan_context::get_color_clear_zero()),
        });
    }

    let mut depth_render_target = Vec::new();
    if vk::Format::UNDEFINED != depth_format {
        let depth_format_type = match depth_format {
            vk::Format::D32_SFLOAT => Some(RenderTargetType::SceneDepth),
            _ => panic!("Not implemented."),
        };
        depth_render_target.push(RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(depth_format_type.unwrap()),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: Some(vulkan_context::get_depth_stencil_clear_value(0.0, 0)),
        });
    }

    framebuffer::create_framebuffer_data_create_info(
        &color_render_targets,
        &depth_render_target,
        &[],
    )
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_target_formats: &[vk::Format],
    depth_format: vk::Format,
) -> RenderPassDataCreateInfo {
    let use_depth_target: bool = vk::Format::UNDEFINED != depth_format;
    let mut render_pass_name = String::from("clear");
    for render_target_format in render_target_formats.iter() {
        render_pass_name.push_str(&format!("_{:?}", render_target_format));
    }
    if use_depth_target {
        render_pass_name.push_str(&format!("_{:?}", depth_format));
    }
    let framebuffer_data_create_info =
        get_framebuffer_data_create_info(renderer_data, render_target_formats, depth_format);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for (i, format) in framebuffer_data_create_info
        ._framebuffer_color_attachment_formats
        .iter()
        .enumerate()
    {
        assert_eq!(render_target_formats[i], *format);
        color_attachment_descriptions.push(ImageAttachmentDescription {
            _attachment_image_format: *format,
            _attachment_image_samples: sample_count,
            _attachment_load_operation: vk::AttachmentLoadOp::CLEAR,
            _attachment_store_operation: vk::AttachmentStoreOp::STORE,
            _attachment_initial_layout: vk::ImageLayout::UNDEFINED,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            ..Default::default()
        });
    }
    let mut depth_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    if use_depth_target {
        for format in framebuffer_data_create_info
            ._framebuffer_depth_attachment_formats
            .iter()
        {
            assert_eq!(depth_format, *format);
            depth_attachment_descriptions.push(ImageAttachmentDescription {
                _attachment_image_format: *format,
                _attachment_image_samples: sample_count,
                _attachment_load_operation: vk::AttachmentLoadOp::CLEAR,
                _attachment_store_operation: vk::AttachmentStoreOp::STORE,
                _attachment_initial_layout: vk::ImageLayout::UNDEFINED,
                _attachment_final_layout: vk::ImageLayout::GENERAL,
                _attachment_reference_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                ..Default::default()
            });
        }
    }
    let subpass_dependencies = vec![vk::SubpassDependency {
        src_subpass: vk::SUBPASS_EXTERNAL,
        dst_subpass: 0,
        src_stage_mask: vk::PipelineStageFlags::TOP_OF_PIPE,
        src_access_mask: vk::AccessFlags::empty(),
        dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT | vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS | vk::PipelineStageFlags::LATE_FRAGMENT_TESTS,
        dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_WRITE | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
        dependency_flags: vk::DependencyFlags::BY_REGION,
    }];
    let pipeline_data_create_infos = vec![PipelineDataCreateInfo {
        _pipeline_data_create_info_name: String::from("clear"),
        _pipeline_vertex_shader_file: PathBuf::from("common/render_quad.vert"),
        _pipeline_fragment_shader_file: PathBuf::from("common/clear_color.frag"),
        _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        _pipeline_shader_defines: vec![format!(
            "ColorAttachmentCount={:?}",
            color_attachment_descriptions.len() as i32
        )],
        _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
        _pipeline_sample_count: sample_count,
        _pipeline_color_blend_modes: vec![
            vulkan_context::get_color_blend_mode(BlendMode::None);
            color_attachment_descriptions.len()
        ],
        _depth_stencil_state_create_info: DepthStencilStateCreateInfo {
            _depth_write_enable: false,
            ..Default::default()
        },
        _vertex_input_bind_descriptions: VertexData::get_vertex_input_binding_descriptions(),
        _vertex_input_attribute_descriptions:
            VertexData::create_vertex_input_attribute_descriptions(),
        ..Default::default()
    }];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: render_pass_name.clone(),
        _render_pass_framebuffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: color_attachment_descriptions,
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}
