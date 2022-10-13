use std::path::PathBuf;

use ash::vk;
use crate::utilities::system::enum_to_string;
use crate::vulkan_context::framebuffer::{ self, FramebufferDataCreateInfo, RenderTargetInfo };
use crate::vulkan_context::geometry_buffer::{ VertexData, StaticVertexData };
use crate::vulkan_context::render_pass::{
    RenderPassDataCreateInfo,
    PipelineDataCreateInfo,
    PipelinePushConstantData,
    ImageAttachmentDescription,
    DepthStencilStateCreateInfo,
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorResourceType,
};
use crate::vulkan_context::vulkan_context::{
    self,
    BlendMode,
};

use crate::renderer::push_constants::PushConstant_RenderCopy;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;

pub fn get_framebuffer_data_create_info(renderer_data: &RendererData) -> FramebufferDataCreateInfo {
    framebuffer::create_framebuffer_data_create_info(
        &[RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(RenderTargetType::SceneColorCopy),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        }],
        &[],
        &[]
    )
}

pub fn get_render_pass_data_create_info(renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("render_copy");
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_color_attachment_formats.iter() {
        color_attachment_descriptions.push(
            ImageAttachmentDescription {
                _attachment_image_format: *format,
                _attachment_image_samples: sample_count,
                _attachment_load_operation: vk::AttachmentLoadOp::DONT_CARE,
                _attachment_store_operation: vk::AttachmentStoreOp::STORE,
                _attachment_final_layout: vk::ImageLayout::GENERAL,
                _attachment_reference_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
                ..Default::default()
            }
        );
    }
    let subpass_dependencies = vec![
        vk::SubpassDependency {
            src_subpass: vk::SUBPASS_EXTERNAL,
            dst_subpass: 0,
            src_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            src_access_mask: vk::AccessFlags::empty(),
            dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dependency_flags: vk::DependencyFlags::BY_REGION,
        }
    ];
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("render_copy"),
            _pipeline_vertex_shader_file: PathBuf::from("common/render_quad.vert"),
            _pipeline_fragment_shader_file: PathBuf::from("common/render_copy.frag"),
            _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
            _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
            _pipeline_sample_count: sample_count,
            _pipeline_color_blend_modes: vec![vulkan_context::get_color_blend_mode(BlendMode::None); color_attachment_descriptions.len()],
            _depth_stencil_state_create_info: DepthStencilStateCreateInfo {
                _depth_write_enable: false,
                ..Default::default()
            },
            _vertex_input_bind_descriptions: StaticVertexData::get_vertex_input_binding_descriptions(),
            _vertex_input_attribute_descriptions: StaticVertexData::create_vertex_input_attribute_descriptions(),
            _push_constant_datas: vec![
                PipelinePushConstantData {
                    _stage_flags: vk::ShaderStageFlags::ALL,
                    _offset: 0,
                    _push_constant: Box::new(PushConstant_RenderCopy::default())
                }
            ],
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                    _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                    ..Default::default()
                },
            ],
            ..Default::default()
        }
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