use std::path::PathBuf;

use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::framebuffer::{self, FramebufferDataCreateInfo, RenderTargetInfo};
use crate::vulkan_context::geometry_buffer::{VertexData, VertexDataBase};
use crate::vulkan_context::render_pass::{
    DepthStencilStateCreateInfo, ImageAttachmentDescription, PipelineDataCreateInfo,
    PipelinePushConstantData, RenderPassDataCreateInfo,
};
use crate::vulkan_context::vulkan_context::{self, BlendMode};
use ash::vk;

use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::shader_buffer_data::ShaderBufferDataType;
use crate::scene::fft_ocean::PushConstant_FFT_Ocean;

pub fn get_framebuffer_data_create_info(renderer_data: &RendererData) -> FramebufferDataCreateInfo {
    framebuffer::create_framebuffer_data_create_info(
        &[RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(RenderTargetType::SceneColor),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        }],
        &[RenderTargetInfo {
            _texture_data: renderer_data.get_render_target(RenderTargetType::SceneDepth),
            _target_layer: 0,
            _target_mip_level: 0,
            _clear_value: None,
        }],
        &[],
    )
}

pub fn get_render_pass_data_create_info(renderer_data: &RendererData) -> RenderPassDataCreateInfo {
    let render_pass_name = String::from("render_fft_ocean");
    let framebuffer_data_create_info = get_framebuffer_data_create_info(renderer_data);
    let sample_count = framebuffer_data_create_info._framebuffer_sample_count;
    let mut color_attachment_descriptions: Vec<ImageAttachmentDescription> = Vec::new();
    for format in framebuffer_data_create_info
        ._framebuffer_color_attachment_formats
        .iter()
    {
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
    for format in framebuffer_data_create_info
        ._framebuffer_depth_attachment_formats
        .iter()
    {
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
        _pipeline_data_create_info_name: String::from("render_fft_ocean"),
        _pipeline_vertex_shader_file: PathBuf::from("fft_ocean/render_fft_ocean.vert"),
        _pipeline_fragment_shader_file: PathBuf::from("fft_ocean/render_fft_ocean.frag"),
        _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        _pipeline_shader_defines: Vec::new(),
        _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
        _pipeline_sample_count: sample_count,
        _pipeline_cull_mode: vk::CullModeFlags::BACK,
        _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
        _pipeline_color_blend_modes: vec![
            vulkan_context::get_color_blend_mode(BlendMode::None);
            color_attachment_descriptions.len()
        ],
        _depth_stencil_state_create_info: DepthStencilStateCreateInfo::default(),
        _vertex_input_bind_descriptions: VertexData::get_vertex_input_binding_descriptions(),
        _vertex_input_attribute_descriptions:
            VertexData::create_vertex_input_attribute_descriptions(),
        _push_constant_data_list: vec![PipelinePushConstantData {
            _stage_flags: vk::ShaderStageFlags::ALL,
            _offset: 0,
            _push_constant: Box::new(PushConstant_FFT_Ocean::default()),
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
                _descriptor_binding_index: 3,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::PointLightData),
                _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 4,
                _descriptor_name: enum_to_string(&ShaderBufferDataType::AtmosphereConstants),
                _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 5,
                _descriptor_name: enum_to_string(&RenderTargetType::FFT_A),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT
                    | vk::ShaderStageFlags::VERTEX,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 6,
                _descriptor_name: enum_to_string(&RenderTargetType::SceneColor),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_image_mip_level: 2,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 7,
                _descriptor_name: enum_to_string(&RenderTargetType::HierarchicalMinZ),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 8,
                _descriptor_name: enum_to_string(&RenderTargetType::Shadow),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT
                    | vk::ShaderStageFlags::VERTEX,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 9,
                _descriptor_name: enum_to_string(&RenderTargetType::LightProbeColor),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 10,
                _descriptor_name: String::from("texture_noise"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 11,
                _descriptor_name: String::from("texture_caustic"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 12,
                _descriptor_name: String::from("texture_foam"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 13,
                _descriptor_name: String::from("transmittance_texture"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 14,
                _descriptor_name: String::from("irradiance_texture"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 15,
                _descriptor_name: String::from("scattering_texture"),
                _descriptor_resource_type: DescriptorResourceType::Texture,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
            DescriptorDataCreateInfo {
                _descriptor_binding_index: 16,
                _descriptor_name: enum_to_string(
                    &RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING,
                ),
                _descriptor_resource_type: DescriptorResourceType::RenderTarget,
                _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX
                    | vk::ShaderStageFlags::FRAGMENT,
                ..Default::default()
            },
        ],
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
