use ash::{
    vk,
};

use crate::constants;
use crate::renderer::renderer::RendererData;
use crate::vulkan_context::texture::{ TextureCreateInfo };

#[repr(i32)]
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum RenderTargetType {
    SceneColor,
    SceneColorCopy,
    SceneDepth,
    BackBuffer,
    BackBufferCopy,
    SceneAlbedo,
    SceneNormal,
    SceneMaterial,
    SceneVelocity,
    Bloom0,
    Bloom1,
    Bloom2,
    Bloom3,
    Bloom4,
    LightShaft,
    SSAO,
    Shadow,
    MaxBound,
}

impl std::fmt::Display for RenderTargetType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for RenderTargetType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SceneColor" => Ok(RenderTargetType::SceneColor),
            "SceneColorCopy" => Ok(RenderTargetType::SceneColorCopy),
            "SceneDepth" => Ok(RenderTargetType::SceneDepth),
            "BackBuffer" => Ok(RenderTargetType::BackBuffer),
            "BackBufferCopy" => Ok(RenderTargetType::BackBufferCopy),
            "SceneAlbedo" => Ok(RenderTargetType::SceneAlbedo),
            "SceneNormal" => Ok(RenderTargetType::SceneNormal),
            "SceneMaterial" => Ok(RenderTargetType::SceneMaterial),
            "SceneVelocity" => Ok(RenderTargetType::SceneVelocity),
            "Bloom0" => Ok(RenderTargetType::Bloom0),
            "Bloom1" => Ok(RenderTargetType::Bloom1),
            "Bloom2" => Ok(RenderTargetType::Bloom2),
            "Bloom3" => Ok(RenderTargetType::Bloom3),
            "Bloom4" => Ok(RenderTargetType::Bloom4),
            "LightShaft" => Ok(RenderTargetType::LightShaft),
            "SSAO" => Ok(RenderTargetType::SSAO),
            "Shadow" => Ok(RenderTargetType::Shadow),
            _ => Err(format!("'{}' is not a valid value for RenderTargetType", s)),
        }
    }
}

pub fn get_render_target_create_infos(renderer_data: &RendererData) -> Vec<TextureCreateInfo<u8>> {
    let swapchain_data = &renderer_data._swapchain_data;
    let window_width = swapchain_data._swapchain_extent.width;
    let window_height = swapchain_data._swapchain_extent.height;
    let samples = vk::SampleCountFlags::TYPE_1;
    //let samples = min(vk::SampleCountFlags::TYPE_4, renderer_data._render_features._msaa_samples);
    let _enable_mipmap = true;
    let disable_mipmap = false;
    let _enable_anisotropy = true;
    let disable_anisotropy = false;
    let _immutable = true;
    let mutable = false;
    let empty_data: Vec<u8> = Vec::new();
    let texture_create_infos = vec![
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneColor.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneColorCopy.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneDepth.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::BackBuffer.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::BackBufferCopy.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneAlbedo.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneMaterial.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneNormal.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneVelocity.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R16G16_SFLOAT,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom0.to_string(),
            _texture_width: window_width / 2,
            _texture_height: window_height / 2,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom1.to_string(),
            _texture_width: window_width / 4,
            _texture_height: window_height / 4,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom2.to_string(),
            _texture_width: window_width / 8,
            _texture_height: window_height / 8,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom3.to_string(),
            _texture_width: window_width / 16,
            _texture_height: window_height / 16,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom4.to_string(),
            _texture_width: window_width / 32,
            _texture_height: window_height / 32,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightShaft.to_string(),
            _texture_width: window_width / 2,
            _texture_height: window_height / 2,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SSAO.to_string(),
            _texture_width: window_width / 2,
            _texture_height: window_height / 2,
            _texture_format: vk::Format::R16_SFLOAT,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Shadow.to_string(),
            _texture_width: constants::SHADOW_MAP_SIZE,
            _texture_height: constants::SHADOW_MAP_SIZE,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone(),
            ..Default::default()
        }
    ];
    texture_create_infos
}
