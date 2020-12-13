use std::path::PathBuf;

use ash::{
    vk,
};

use crate::utilities::system::{
    enum_to_string
};
use crate::renderer::renderer::{
    RenderMode,
    RenderObjectType,
    RendererData,
};
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::shader_buffer_datas::{
    ShaderBufferDataType,
    PushConstant_StaticRenderObject,
    PushConstant_SkeletalRenderObject,
};
use crate::vulkan_context::framebuffer::{ self, FramebufferDataCreateInfo };
use crate::vulkan_context::geometry_buffer::{ VertexData, SkeletalVertexData };
use crate::vulkan_context::render_pass::{
    RenderPassDataCreateInfo,
    PipelineDataCreateInfo,
    ImageAttachmentDescription,
    DepthStencilStateCreateInfo,
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorResourceType,
};
use crate::vulkan_context::vulkan_context;

pub fn get_framebuffer_data_create_info(renderer_data: &RendererData, render_object_type: RenderObjectType) -> FramebufferDataCreateInfo {
    framebuffer::create_framebuffer_data_create_info(
        Vec::new(),
        vec![renderer_data.get_render_target(RenderTargetType::Shadow)],
        Vec::new(),
        match render_object_type {
            RenderObjectType::Static => vec![
                vulkan_context::get_depth_stencil_clear_value(1.0, 0)
            ],
            RenderObjectType::Skeletal => Vec::new(),
        },
    )
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType,
) -> RenderPassDataCreateInfo {
    let render_pass_name = match render_object_type {
        RenderObjectType::Static => String::from("render_pass_static_shadow"),
        RenderObjectType::Skeletal => String::from("render_pass_skeletal_shadow"),
    };
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data, render_object_type);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let (attachment_load_operation, attachment_initial_layout) = match render_object_type  {
        RenderObjectType::Static => (vk::AttachmentLoadOp::CLEAR, vk::ImageLayout::UNDEFINED),
        RenderObjectType::Skeletal => (vk::AttachmentLoadOp::LOAD, vk::ImageLayout::GENERAL),
    };
    let mut depth_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_depth_attachment_formats.iter() {
        depth_attachment_descriptions.push(
            ImageAttachmentDescription {
                _attachment_image_format: *format,
                _attachment_image_samples: sample_count,
                _attachment_load_operation: attachment_load_operation,
                _attachment_store_operation: vk::AttachmentStoreOp::STORE,
                _attachment_initial_layout: attachment_initial_layout,
                _attachment_final_layout: vk::ImageLayout::GENERAL,
                _attachment_reference_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                ..Default::default()
            }
        );
    }
    let subpass_dependencies = vec![
        vk::SubpassDependency {
            src_subpass: vk::SUBPASS_EXTERNAL,
            dst_subpass: 0,
            src_stage_mask: vk::PipelineStageFlags::BOTTOM_OF_PIPE,
            dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            src_access_mask: vk::AccessFlags::MEMORY_READ,
            dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dependency_flags: vk::DependencyFlags::BY_REGION,
        },
        vk::SubpassDependency {
            src_subpass: 0,
            dst_subpass: vk::SUBPASS_EXTERNAL,
            src_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            dst_stage_mask: vk::PipelineStageFlags::BOTTOM_OF_PIPE,
            src_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            dst_access_mask: vk::AccessFlags::MEMORY_READ,
            dependency_flags: vk::DependencyFlags::BY_REGION,
        }
    ];
    let pipeline_data_create_infos = vec![
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::from("render_object"),
            _pipeline_vertex_shader_file: PathBuf::from("render_object.vert"),
            _pipeline_fragment_shader_file: PathBuf::from("shadowmap.frag"),
            _pipeline_shader_defines: vec![
                format!("RenderMode={:?}", RenderMode::RenderMode_Shadow as i32),
                format!("RenderObjectType={:?}", render_object_type as i32),
            ],
            _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
            _pipeline_sample_count: sample_count,
            _pipeline_polygon_mode: vk::PolygonMode::FILL,
            _pipeline_cull_mode: vk::CullModeFlags::BACK,
            _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
            _pipeline_viewport: vk::Viewport::default(),
            _pipeline_scissor_rect: vk::Rect2D::default(),
            _pipeline_color_blend_modes: Vec::new(),
            _depth_stencil_state_create_info: DepthStencilStateCreateInfo::default(),
            _vertex_input_bind_descriptions: match render_object_type {
                RenderObjectType::Static => VertexData::get_vertex_input_binding_descriptions(),
                RenderObjectType::Skeletal => SkeletalVertexData::get_vertex_input_binding_descriptions(),
            },
            _vertex_input_attribute_descriptions: match render_object_type {
                RenderObjectType::Static => VertexData::create_vertex_input_attribute_descriptions(),
                RenderObjectType::Skeletal => SkeletalVertexData::create_vertex_input_attribute_descriptions(),
            },
            _push_constant_ranges: vec![vk::PushConstantRange {
                stage_flags: vk::ShaderStageFlags::ALL,
                offset: 0,
                size: match render_object_type {
                    RenderObjectType::Static => std::mem::size_of::<PushConstant_StaticRenderObject>() as u32,
                    RenderObjectType::Skeletal => std::mem::size_of::<PushConstant_SkeletalRenderObject>() as u32,
                }
            }],
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::SceneConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::ViewConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::LightConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&ShaderBufferDataType::BoneMatrices),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 4,
                    _descriptor_name: String::from("textureBase"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                }
            ],
        }
    ];

    RenderPassDataCreateInfo  {
        _render_pass_create_info_name: render_pass_name.clone(),
        _render_pass_framebuffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: Vec::new(),
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}