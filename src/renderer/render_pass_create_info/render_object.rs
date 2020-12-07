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
use crate::renderer::buffer_data_infos::{
    BufferDataType,
    PushConstants_StaticRenderObject,
    PushConstants_SkeletalRenderObject,
};
use crate::vulkan_context::framebuffer::FramebufferDataCreateInfo;
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
use crate::vulkan_context::vulkan_context::{ self, BlendMode, };

pub fn get_render_pass_name(render_object_type: RenderObjectType) -> String {
    let render_pass_name = match render_object_type {
        RenderObjectType::Static => "render_pass_static_opaque",
        RenderObjectType::Skeletal => "render_pass_skeletal_opaque",
    };
    String::from(render_pass_name)
}

pub fn get_framebuffer_data_create_infos(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType
) -> Vec<FramebufferDataCreateInfo> {
    let texture_scene_albedo = renderer_data.get_render_target(RenderTargetType::SceneAlbedo);
    let texture_scene_material = renderer_data.get_render_target(RenderTargetType::SceneMaterial);
    let texture_scene_normal = renderer_data.get_render_target(RenderTargetType::SceneNormal);
    let texture_scene_velocity = renderer_data.get_render_target(RenderTargetType::SceneVelocity);
    let texture_scene_depth = renderer_data.get_render_target(RenderTargetType::SceneDepth);
    let (width, height, depth) = (texture_scene_albedo._image_width, texture_scene_albedo._image_height, texture_scene_albedo._image_depth);
    let image_views = vec![
        texture_scene_albedo._image_view,
        texture_scene_material._image_view,
        texture_scene_normal._image_view,
        texture_scene_velocity._image_view,
        texture_scene_depth._image_view,
    ];

    vec![
        FramebufferDataCreateInfo {
            _framebuffer_name: format!("GBuffer{:?}", render_object_type),
            _framebuffer_width: width,
            _framebuffer_height: height,
            _framebuffer_depth: depth,
            _framebuffer_sample_count: texture_scene_albedo._image_sample_count,
            _framebuffer_view_port: vulkan_context::create_viewport(0, 0, width, height, 0.0, 1.0),
            _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, width, height),
            _framebuffer_color_attachment_formats: vec![
                texture_scene_albedo._image_format,
                texture_scene_material._image_format,
                texture_scene_normal._image_format,
                texture_scene_velocity._image_format,
            ],
            _framebuffer_depth_attachment_formats: vec![
                texture_scene_depth._image_format,
            ],
            _framebuffer_image_views: vec![image_views; constants::SWAPCHAIN_IMAGE_COUNT],
            _framebuffer_clear_values: match render_object_type {
                RenderObjectType::Static => vec![
                    vulkan_context::get_color_clear_zero(),
                    vulkan_context::get_color_clear_zero(),
                    vulkan_context::get_color_clear_value(0.5, 0.5, 1.0, 0.0),
                    vulkan_context::get_color_clear_zero(),
                    vulkan_context::get_depth_stencil_clear_value(1.0, 0)
                ],
                RenderObjectType::Skeletal => Vec::new(),
            },
            ..Default::default()
        }
    ]
}

pub fn get_render_pass_data_create_info(
    renderer_data: &RendererData,
    render_object_type: RenderObjectType,
) -> RenderPassDataCreateInfo {
    let render_pass_name = get_render_pass_name(render_object_type);
    let framebuffer_data_create_infos = get_framebuffer_data_create_infos(renderer_data, render_object_type);
    let framebuffer_data_create_info = &framebuffer_data_create_infos[0];
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let (attachment_load_operation, attachment_initial_layout) = match render_object_type  {
        RenderObjectType::Static => (vk::AttachmentLoadOp::CLEAR, vk::ImageLayout::UNDEFINED),
        RenderObjectType::Skeletal => (vk::AttachmentLoadOp::LOAD, vk::ImageLayout::GENERAL),
    };
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info._framebuffer_color_attachment_formats.iter() {
        color_attachment_descriptions.push(
            ImageAttachmentDescription {
                _attachment_image_format: *format,
                _attachment_image_samples: sample_count,
                _attachment_load_operation: attachment_load_operation,
                _attachment_store_operation: vk::AttachmentStoreOp::STORE,
                _attachment_initial_layout: attachment_initial_layout,
                _attachment_final_layout: vk::ImageLayout::GENERAL,
                _attachment_reference_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
                ..Default::default()
            }
        );
    }
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
            _pipeline_fragment_shader_file: PathBuf::from("render_object.frag"),
            _pipeline_shader_defines: vec![
                format!("RenderMode={:?}", RenderMode::RenderMode_Common as i32),
                format!("RenderObjectType={:?}", render_object_type as i32),
            ],
            _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
            _pipeline_sample_count: sample_count,
            _pipeline_polygon_mode: vk::PolygonMode::FILL,
            _pipeline_cull_mode: vk::CullModeFlags::BACK,
            _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
            _pipeline_viewport: vk::Viewport::default(),
            _pipeline_scissor_rect: vk::Rect2D::default(),
            _pipeline_color_blend_modes: vec![vulkan_context::get_color_blend_mode(BlendMode::None); color_attachment_descriptions.len()],
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
                    RenderObjectType::Static => std::mem::size_of::<PushConstants_StaticRenderObject>() as u32,
                    RenderObjectType::Skeletal => std::mem::size_of::<PushConstants_SkeletalRenderObject>() as u32,
                }
            }],
            _descriptor_data_create_infos: vec![
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 0,
                    _descriptor_name: enum_to_string(&BufferDataType::SceneConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 1,
                    _descriptor_name: enum_to_string(&BufferDataType::ViewConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 2,
                    _descriptor_name: enum_to_string(&BufferDataType::LightConstants),
                    _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 3,
                    _descriptor_name: enum_to_string(&BufferDataType::BoneMatrices),
                    _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
                    _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 4,
                    _descriptor_name: String::from("textureBase"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 5,
                    _descriptor_name: String::from("textureMaterial"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                },
                DescriptorDataCreateInfo {
                    _descriptor_binding_index: 6,
                    _descriptor_name: String::from("textureNormal"),
                    _descriptor_resource_type: DescriptorResourceType::Texture,
                    _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                }
            ],
        }
    ];

    RenderPassDataCreateInfo  {
        _render_pass_create_info_name: render_pass_name.clone(),
        _render_pass_frame_buffer_create_infos: framebuffer_data_create_infos,
        _color_attachment_descriptions: color_attachment_descriptions,
        _depth_attachment_descriptions: depth_attachment_descriptions,
        _resolve_attachment_descriptions: Vec::new(),
        _subpass_dependencies: subpass_dependencies,
        _pipeline_data_create_infos: pipeline_data_create_infos,
    }
}