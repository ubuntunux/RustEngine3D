use std::path::PathBuf;

use ash::vk;

use crate::constants::{SHADOW_DEPTH_BIAS, SHADOW_DEPTH_SLOPE_BIAS};
use crate::renderer::push_constants::PushConstant_RenderObject;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::{RenderMode, RenderObjectType, RendererData};
use crate::renderer::shader_buffer_data::ShaderBufferDataType;
use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::framebuffer::{self, FramebufferDataCreateInfo, RenderTargetInfo};
use crate::vulkan_context::geometry_buffer::{SkeletalVertexData, VertexData, VertexDataBase};
use crate::vulkan_context::render_pass::{
    DepthStencilStateCreateInfo, ImageAttachmentDescription, PipelineDataCreateInfo,
    PipelinePushConstantData, RenderPassDataCreateInfo,
};
use crate::vulkan_context::vulkan_context;

pub fn get_framebuffer_data_create_info(renderer_data: &RendererData) -> FramebufferDataCreateInfo {
    framebuffer::create_framebuffer_data_create_info(
        &[],
        &[RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(RenderTargetType::Shadow),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: Some(vulkan_context::get_depth_stencil_clear_value(0.0, 0)),
        }],
        &[],
    )
}

pub fn get_render_pass_name(render_object_type: RenderObjectType) -> &'static str {
    match render_object_type {
        RenderObjectType::Static => "render_pass_static_shadow",
        RenderObjectType::Skeletal => "render_pass_skeletal_shadow",
    }
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType,
) -> RenderPassDataCreateInfo {
    let render_pass_name = get_render_pass_name(render_object_type);
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let mut depth_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info
        ._framebuffer_depth_attachment_formats
        .iter() {
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
    let subpass_dependencies = vec![
        vk::SubpassDependency {
            src_subpass: vk::SUBPASS_EXTERNAL,
            dst_subpass: 0,
            src_stage_mask: vk::PipelineStageFlags::BOTTOM_OF_PIPE,
            dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            src_access_mask: vk::AccessFlags::MEMORY_READ,
            dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ
                | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dependency_flags: vk::DependencyFlags::BY_REGION,
        },
        vk::SubpassDependency {
            src_subpass: 0,
            dst_subpass: vk::SUBPASS_EXTERNAL,
            src_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            dst_stage_mask: vk::PipelineStageFlags::BOTTOM_OF_PIPE,
            src_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ
                | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dst_access_mask: vk::AccessFlags::MEMORY_READ,
            dependency_flags: vk::DependencyFlags::BY_REGION,
        },
    ];
    let pipeline_data_create_infos = vec![PipelineDataCreateInfo {
        _pipeline_data_create_info_name: String::from("render_object"),
        _pipeline_vertex_shader_file: PathBuf::from("common/render_object.vert"),
        _pipeline_fragment_shader_file: PathBuf::from("common/render_object.frag"),
        _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        _pipeline_shader_defines: vec![
            format!("RenderMode={:?}", RenderMode::Shadow as i32),
            format!("RenderObjectType={:?}", render_object_type as i32),
        ],
        _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
        _pipeline_sample_count: sample_count,
        _pipeline_cull_mode: vk::CullModeFlags::FRONT,
        _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
        _pipeline_depth_bias_constant_factor: unsafe { SHADOW_DEPTH_BIAS },
        _pipeline_depth_bias_clamp: -1000.0,
        _pipeline_depth_bias_slope_factor: unsafe { SHADOW_DEPTH_SLOPE_BIAS },
        _depth_stencil_state_create_info: DepthStencilStateCreateInfo::default(),
        _vertex_input_bind_descriptions: match render_object_type {
            RenderObjectType::Static => VertexData::get_vertex_input_binding_descriptions(),
            RenderObjectType::Skeletal => {
                SkeletalVertexData::get_vertex_input_binding_descriptions()
            }
        },
        _vertex_input_attribute_descriptions: match render_object_type {
            RenderObjectType::Static => VertexData::create_vertex_input_attribute_descriptions(),
            RenderObjectType::Skeletal => {
                SkeletalVertexData::create_vertex_input_attribute_descriptions()
            }
        },
        _push_constant_data_list: vec![PipelinePushConstantData {
            _stage_flags: vk::ShaderStageFlags::ALL,
            _offset: 0,
            _push_constant: Box::new(PushConstant_RenderObject::default()),
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
                _descriptor_binding_index: 2,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::LightData),
                _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 4,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::TransformMatrices),
                _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 5,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::TransformOffsets),
                _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 14,
                _descriptor_name: String::from("textureBase"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
        ],
        ..Default::default()
    }];

    RenderPassDataCreateInfo {
        _render_pass_create_info_name: String::from(render_pass_name),
        _render_pass_framebuffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: Vec::new(),
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}
