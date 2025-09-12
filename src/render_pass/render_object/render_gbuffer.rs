use std::path::PathBuf;
use ash::vk;
use crate::render_pass::render_object::common;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::{RenderMode, RenderObjectType, RendererData};
use crate::vulkan_context::framebuffer::{self, FramebufferDataCreateInfo, RenderTargetInfo};
use crate::vulkan_context::geometry_buffer::{SkeletalVertexData, VertexData, VertexDataBase};
use crate::vulkan_context::render_pass::{DepthStencilStateCreateInfo, ImageAttachmentDescription, PipelineDataCreateInfo, PipelinePushConstantData, RenderPassDataCreateInfo};
use crate::vulkan_context::vulkan_context::{self, BlendOperation};
use crate::renderer::push_constants::PushConstant;
use crate::vulkan_context::descriptor::DescriptorDataCreateInfo;

pub fn get_framebuffer_data_create_info(
    renderer_data: &RendererData,
    is_clear_gbuffer: bool,
) -> FramebufferDataCreateInfo {
    framebuffer::create_framebuffer_data_create_info(
        &[
            RenderTargetInfo {
                _texture_data: renderer_data.get_render_target(RenderTargetType::SceneAlbedo),
                _target_layer: 0,
                _target_mip_level: 0,
                _clear_value: Some(vulkan_context::get_color_clear_zero()),
            },
            RenderTargetInfo {
                _texture_data: renderer_data.get_render_target(RenderTargetType::SceneMaterial),
                _target_layer: 0,
                _target_mip_level: 0,
                _clear_value: Some(vulkan_context::get_color_clear_zero()),
            },
            RenderTargetInfo {
                _texture_data: renderer_data.get_render_target(RenderTargetType::SceneNormal),
                _target_layer: 0,
                _target_mip_level: 0,
                _clear_value: Some(vulkan_context::get_color_clear_value(0.5, 0.5, 1.0, 0.0)),
            },
            RenderTargetInfo {
                _texture_data: renderer_data.get_render_target(RenderTargetType::SceneVelocity),
                _target_layer: 0,
                _target_mip_level: 0,
                _clear_value: Some(vulkan_context::get_color_clear_zero()),
            },
        ],
        &[RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(RenderTargetType::SceneDepth),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: if is_clear_gbuffer {
                Some(vulkan_context::get_depth_stencil_clear_value(0.0, 0))
            } else {
                None
            },
        }],
        &[],
    )
}

pub fn get_render_pass_name(render_object_type: RenderObjectType) -> &'static str {
    match render_object_type {
        RenderObjectType::Static => "render_pass_static_gbuffer",
        RenderObjectType::Skeletal => "render_pass_skeletal_gbuffer",
    }
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType,
    cull_mode: vk::CullModeFlags,
    pipeline_data_name: &str,
    vertex_shader_file: &str,
    pixel_shader_file: &str,
    push_constant_data: Box<dyn PushConstant>,
    descriptor_data_create_infos: Vec<DescriptorDataCreateInfo>
) -> RenderPassDataCreateInfo {
    let render_pass_name = get_render_pass_name(render_object_type);
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data, false);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_color_attachment_formats.iter() {
        color_attachment_descriptions.push(ImageAttachmentDescription {
            _attachment_image_format: *format,
            _attachment_image_samples: sample_count,
            _attachment_load_operation: vk::AttachmentLoadOp::LOAD,
            _attachment_store_operation: vk::AttachmentStoreOp::STORE,
            _attachment_initial_layout: vk::ImageLayout::GENERAL,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            ..Default::default()
        });
    }
    let mut depth_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_depth_attachment_formats.iter() {
        depth_attachment_descriptions.push(ImageAttachmentDescription {
            _attachment_image_format: *format,
            _attachment_image_samples: sample_count,
            _attachment_load_operation: vk::AttachmentLoadOp::LOAD,
            _attachment_store_operation: vk::AttachmentStoreOp::STORE,
            _attachment_initial_layout: vk::ImageLayout::GENERAL,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
            ..Default::default()
        });
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
        _pipeline_data_create_info_name: String::from(pipeline_data_name),
        _pipeline_vertex_shader_file: PathBuf::from(vertex_shader_file),
        _pipeline_fragment_shader_file: PathBuf::from(pixel_shader_file),
        _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        _pipeline_shader_defines: vec![
            format!("RenderMode={:?}", RenderMode::GBuffer as i32),
            format!("RenderObjectType={:?}", render_object_type as i32),
        ],
        _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
        _pipeline_sample_count: sample_count,
        _pipeline_cull_mode: cull_mode,
        _pipeline_front_face: vk::FrontFace::CLOCKWISE,
        _pipeline_color_blend_operations: vec![vulkan_context::get_color_blend_operation(BlendOperation::None); color_attachment_descriptions.len()],
        _depth_stencil_state_create_info: DepthStencilStateCreateInfo {
            _depth_write_enable: false,
            ..Default::default()
        },
        _vertex_input_bind_descriptions: match render_object_type {
            RenderObjectType::Static => VertexData::get_vertex_input_binding_descriptions(),
            RenderObjectType::Skeletal => SkeletalVertexData::get_vertex_input_binding_descriptions()
        },
        _vertex_input_attribute_descriptions: match render_object_type {
            RenderObjectType::Static => VertexData::create_vertex_input_attribute_descriptions(),
            RenderObjectType::Skeletal => SkeletalVertexData::create_vertex_input_attribute_descriptions()
        },
        _push_constant_data_list: vec![PipelinePushConstantData {
            _stage_flags: vk::ShaderStageFlags::ALL,
            _offset: 0,
            _push_constant: push_constant_data,
        }],
        _descriptor_data_create_infos: [common::get_descriptor_data_create_infos(), descriptor_data_create_infos].concat(),
        ..Default::default()
    }];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: String::from(render_pass_name),
        _render_pass_framebuffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: color_attachment_descriptions,
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}
