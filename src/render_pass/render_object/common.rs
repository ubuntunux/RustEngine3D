use ash::vk;
use crate::render_pass::render_object;
use crate::renderer::push_constants::PushConstant;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::{RenderObjectType, RendererData};
use crate::renderer::shader_buffer_data::ShaderBufferDataType;
use crate::utilities::system::enum_to_string;
use crate::vulkan_context::descriptor::{DescriptorDataCreateInfo, DescriptorResourceType};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;

pub const USER_BINDING_INDEX0: u32 = 14;
pub const USER_BINDING_INDEX1: u32 = 15;
pub const USER_BINDING_INDEX2: u32 = 16;
pub const USER_BINDING_INDEX3: u32 = 17;
pub const USER_BINDING_INDEX4: u32 = 18;
pub const USER_BINDING_INDEX5: u32 = 19;
pub const USER_BINDING_INDEX6: u32 = 20;
pub const USER_BINDING_INDEX7: u32 = 21;
pub const USER_BINDING_INDEX8: u32 = 22;
pub const USER_BINDING_INDEX9: u32 = 23;
pub const USER_BINDING_INDEX10: u32 = 24;
pub const USER_BINDING_INDEX11: u32 = 25;
pub const USER_BINDING_INDEX12: u32 = 26;
pub const USER_BINDING_INDEX13: u32 = 27;
pub const USER_BINDING_INDEX14: u32 = 28;
pub const USER_BINDING_INDEX15: u32 = 29;
pub const USER_BINDING_INDEX16: u32 = 30;
pub const USER_BINDING_INDEX17: u32 = 31;

pub fn get_descriptor_data_create_infos() -> Vec<DescriptorDataCreateInfo> {
    let descriptor_data_create_infos = vec![
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 0,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::SceneConstants),
            _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 1,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::ViewConstants),
            _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 2,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::LightData),
            _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 3,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::PointLightData),
            _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 4,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::TransformMatrices),
            _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 5,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::TransformOffsets),
            _descriptor_resource_type: DescriptorResourceType::StorageBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 6,
            _descriptor_name: enum_to_string(&ShaderBufferDataType::AtmosphereConstants),
            _descriptor_resource_type: DescriptorResourceType::UniformBuffer,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 7,
            _descriptor_name: enum_to_string(&RenderTargetType::Shadow),
            _descriptor_resource_type: DescriptorResourceType::RenderTarget,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 8,
            _descriptor_name: enum_to_string(&RenderTargetType::CaptureHeightMap),
            _descriptor_resource_type: DescriptorResourceType::RenderTarget,
            _descriptor_shader_stage: vk::ShaderStageFlags::FRAGMENT,
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
            _descriptor_name: String::from("transmittance_texture"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 11,
            _descriptor_name: String::from("irradiance_texture"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 12,
            _descriptor_name: String::from("scattering_texture"),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        },
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 13,
            _descriptor_name: enum_to_string(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING),
            _descriptor_resource_type: DescriptorResourceType::RenderTarget,
            _descriptor_shader_stage: vk::ShaderStageFlags::VERTEX | vk::ShaderStageFlags::FRAGMENT,
            ..Default::default()
        }
    ];

    assert_eq!(descriptor_data_create_infos.len(), USER_BINDING_INDEX0 as usize);

    descriptor_data_create_infos
}

pub fn get_render_pass_data_create_infos(
    renderer_data: &RendererData,
    pipeline_data_name: &str,
    vertex_shader_file: &str,
    pixel_shader_file: &str,
    push_constant_data: &Box<dyn PushConstant>,
    descriptor_data_create_infos: &Vec<DescriptorDataCreateInfo>
) -> Vec<RenderPassDataCreateInfo> {
    vec![
        render_object::capture_height_map::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::depth_prepass::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Skeletal,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::depth_prepass::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_gbuffer::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Skeletal,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_gbuffer::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Skeletal,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            0,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            1,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            2,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            3,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            4,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_forward_for_light_probe::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            5,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_shadow::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Static,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        ),
        render_object::render_shadow::get_render_pass_data_create_info(
            renderer_data,
            RenderObjectType::Skeletal,
            pipeline_data_name,
            vertex_shader_file,
            pixel_shader_file,
            push_constant_data.clone(),
            descriptor_data_create_infos.clone()
        )
    ]
}