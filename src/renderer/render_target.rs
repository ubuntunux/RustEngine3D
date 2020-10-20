use std::collections::HashMap;

use ash::{
    vk,
};

use crate::constants;
use crate::renderer::renderer::RendererData;
use crate::vulkan_context::texture::{
    TextureData,
    TextureCreateInfo
};

#[repr(i32)]
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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
    SSAO,
    Shadow,
    MaxBound,
}

pub type RenderTargetDataMap = HashMap<RenderTargetType, TextureData>;

pub fn regist_render_target(
    renderer_data: &mut RendererData,
    render_target_type: RenderTargetType,
    texture_create_info: &TextureCreateInfo
) {
    let texture_data_name = format!("{:?}", render_target_type);
    let texture_data = renderer_data.create_render_target(
        &texture_data_name,
        &texture_create_info,
    );
    renderer_data._render_target_data_map.insert(render_target_type, texture_data);
}

pub fn create_render_targets(renderer_data: &mut RendererData) {
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
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneColor,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneColorCopy,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneDepth,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::BackBuffer,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::BackBufferCopy,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneAlbedo,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::LINEAR,
            _texture_mag_filter: vk::Filter::LINEAR,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneMaterial,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneNormal,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SceneVelocity,
        &TextureCreateInfo {
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_depth: 1,
            _texture_format: vk::Format::R16G16_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::SSAO,
        &TextureCreateInfo {
            _texture_width: window_width / 2,
            _texture_height: window_height / 2,
            _texture_depth: 1,
            _texture_format: vk::Format::R16_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
    regist_render_target(
        renderer_data,
        RenderTargetType::Shadow,
        &TextureCreateInfo {
            _texture_width: constants::SHADOW_MAP_SIZE,
            _texture_height: constants::SHADOW_MAP_SIZE,
            _texture_depth: 1,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_samples: vk::SampleCountFlags::TYPE_1,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: disable_mipmap,
            _enable_anisotropy: disable_anisotropy,
            _immutable: mutable,
            _texture_initial_datas: empty_data.clone()
        }
    );
}
