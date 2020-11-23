use std::path::PathBuf;

use ash::{
    vk,
};

use crate::constants;
use crate::utilities::system::{
    enum_to_string
};
use crate::renderer::renderer::{
    RenderMode,
    RenderObjectType,
    RendererData,
};
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::uniform_buffer_data::UniformBufferType;
use crate::renderer::push_constants::{
    PushConstantInterface,
    PushConstants_StaticRenderObject,
    PushConstants_SkeletalRenderObject,
};
use crate::vulkan_context::framebuffer::FramebufferDataCreateInfo;
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

pub fn get_render_pass_name(render_object_type: RenderObjectType) -> String {
    let render_pass_name = match render_object_type {
        RenderObjectType::Static => "render_pass_static_shadow",
        RenderObjectType::Skeletal => "render_pass_skeletal_shadow",
    };
    String::from(render_pass_name)
}


pub fn get_framebuffer_data_create_info(
    renderer_data: &RendererData,
    render_pass_name: &String,
    render_object_type: RenderObjectType
) -> FramebufferDataCreateInfo {
    let texture_shadow = renderer_data.get_render_target(RenderTargetType::Shadow);
    let (width, height, depth) = (texture_shadow._image_width, texture_shadow._image_height, texture_shadow._image_depth);
    let image_views = vec![texture_shadow._image_view];
    FramebufferDataCreateInfo {
        _framebuffer_name: render_pass_name.clone(),
        _framebuffer_width: width,
        _framebuffer_height: height,
        _framebuffer_depth: depth,
        _framebuffer_sample_count: texture_shadow._image_sample_count,
        _framebuffer_view_port: vulkan_context::create_viewport(0, 0, width, height, 0.0, 1.0),
        _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, width, height),
        _framebuffer_depth_attachment_formats: vec![ texture_shadow._image_format ],
        _framebuffer_image_views: vec![image_views; constants::SWAPCHAIN_IMAGE_COUNT],
        _framebuffer_clear_values: match render_object_type {
            RenderObjectType::Static => vec![
                vulkan_context::get_depth_stencil_clear_value(1.0, 0)
            ],
            RenderObjectType::Skeletal => Vec::new(),
        },
        ..Default::default()
    }
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType,
) -> RenderPassDataCreateInfo {
    let render_pass_name = get_render_pass_name(render_object_type);
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data, &render_pass_name, render_object_type);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let (attachment_load_operation, attachment_initial_layout) = match render_object_type  {
        RenderObjectType::Static => (vk::AttachmentLoadOp::CLEAR, vk::ImageLayout::UNDEFINED),
        RenderObjectType::Skeletal => (vk::AttachmentLoadOp::LOAD, vk::ImageLayout::GENERAL),
    };
    let mut depth_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_depth_attachment_formats.iter() {
        depth_attachment_descriptions.push({
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
        });
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
            _pipeline_viewport: framebuffer_data_create_info._framebuffer_view_port,
            _pipeline_scissor_rect: framebuffer_data_create_info._framebuffer_scissor_rect,
            _pipeline_color_blend_modes: Vec::new(),
            _depth_stencil_state_create_info: DepthStencilStateCreateInfo::default(),
            _push_constant_ranges: vec![vk::PushConstantRange {
                stage_flags: vk::ShaderStageFlags::ALL,
                offset: 0,
                size: match render_object_type {
                    RenderObjectType::Static => PushConstants_StaticRenderObject::get_push_constants_size(),
                    RenderObjectType::Skeletal => PushConstants_SkeletalRenderObject::get_push_constants_size(),
                }
            }],
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&UniformBufferType::SceneConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_type: vk::DescriptorType::UNIFORM_BUFFER,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&UniformBufferType::ViewConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_type: vk::DescriptorType::UNIFORM_BUFFER,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&UniformBufferType::LightConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_type: vk::DescriptorType::UNIFORM_BUFFER,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&UniformBufferType::BoneMatrices),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_type: vk::DescriptorType::UNIFORM_BUFFER,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 4,
                    _descriptor_name: String::from("textureBase"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_type: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                }
            ],
        }
    ];

    RenderPassDataCreateInfo  {
        _render_pass_create_info_name: render_pass_name.clone(),
        _render_pass_frame_buffer_create_info: framebuffer_data_create_info,
        _color_attachment_descriptions: Vec::new(),
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}