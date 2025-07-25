use ash::vk;
use strum_macros::{Display, EnumString};
use crate::constants;
use crate::renderer::renderer_context::RendererContext;
use crate::scene::fft_ocean;
use crate::scene::precomputed_atmosphere;
use crate::vulkan_context::texture::{ImageLayoutTransition, TextureCreateInfo};

#[repr(i32)]
#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq, Eq, Hash, Debug, Display, EnumString, Copy)]
pub enum RenderTargetType {
    SceneColor,
    PostProcessedColor,
    SceneDepth,
    HierarchicalMinZ,
    LightProbeAtmosphereColor,
    LightProbeAtmosphereInscatter,
    LightProbeColor,
    LightProbeColorOnlySky,
    LightProbeColorOnlySkyPrev,
    LightProbeColorForward,
    LightProbeColorForwardPrev,
    LightProbeDepth,
    BackBuffer,
    BackBufferCopy,
    SceneAlbedo,
    SceneNormal,
    SceneMaterial,
    SceneVelocity,
    TAAResolve,
    Bloom0,
    BloomTemp0,
    LightShaft,
    ShadowAO,
    Shadow,
    CaptureNormalMap,
    CaptureHeightMap,
    SSR,
    SSRResolved,
    SSRResolvedPrev,
    // FFT Ocean
    FFT_A,
    FFT_B,
    FFT_SLOPE_VARIANCE,
    // Precomputed Atmosphere
    PRECOMPUTED_ATMOSPHERE_COLOR,
    PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED,
    PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV,
    PRECOMPUTED_ATMOSPHERE_INSCATTER,
    PRECOMPUTED_ATMOSPHERE_TRANSMITTANCE,
    PRECOMPUTED_ATMOSPHERE_SCATTERING,
    PRECOMPUTED_ATMOSPHERE_IRRADIANCE,
    PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING,
    PRECOMPUTED_ATMOSPHERE_DELTA_IRRADIANCE,
    PRECOMPUTED_ATMOSPHERE_DELTA_RAYLEIGH_SCATTERING,
    PRECOMPUTED_ATMOSPHERE_DELTA_MIE_SCATTERING,
    PRECOMPUTED_ATMOSPHERE_DELTA_SCATTERING_DENSITY,
    MaxBound,
}

pub fn get_render_target_create_infos(
    renderer_context: &RendererContext,
) -> Vec<TextureCreateInfo<u8>> {
    let swapchain_data = &renderer_context._swapchain_data;
    let window_width = swapchain_data._swapchain_extent.width;
    let window_height = swapchain_data._swapchain_extent.height;
    let render_viewport_width = unsafe { if constants::ENABLE_UPSCALE { window_width / 2 } else { window_width }  };
    let render_viewport_height = unsafe { if constants::ENABLE_UPSCALE { window_height / 2 } else { window_height } };
    let samples = vk::SampleCountFlags::TYPE_1;
    //let samples = min(vk::SampleCountFlags::TYPE_4, renderer_context._render_features._msaa_samples);
    let hdr_texture_create_info = TextureCreateInfo {
        _texture_format: vk::Format::R16G16B16A16_SFLOAT,
        _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
        ..Default::default()
    };

    let precomputed_atmosphere_texture_create_info = TextureCreateInfo {
        _texture_width: render_viewport_width / 4,
        _texture_height: render_viewport_width / 4,
        _texture_format: vk::Format::R16G16B16A16_SFLOAT,
        _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
        ..Default::default()
    };

    let texture_create_infos = vec![
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneColor.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PostProcessedColor.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneDepth.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::HierarchicalMinZ.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R32_SFLOAT,
            _texture_samples: samples,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeAtmosphereColor.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE / 4,
            _texture_height: constants::LIGHT_PROBE_SIZE / 4,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeAtmosphereInscatter.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE / 4,
            _texture_height: constants::LIGHT_PROBE_SIZE / 4,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeColor.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeColorOnlySky.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeColorOnlySkyPrev.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeColorForward.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeColorForwardPrev.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightProbeDepth.to_string(),
            _texture_width: constants::LIGHT_PROBE_SIZE,
            _texture_height: constants::LIGHT_PROBE_SIZE,
            _texture_layers: constants::CUBE_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::CUBE,
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::BackBuffer.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::BackBufferCopy.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_samples: samples,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneAlbedo.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneMaterial.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneNormal.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SceneVelocity.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R16G16_SFLOAT,
            _texture_min_filter: vk::Filter::NEAREST,
            _texture_mag_filter: vk::Filter::NEAREST,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::TAAResolve.to_string(),
            _texture_width: window_width,
            _texture_height: window_height,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Bloom0.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::BloomTemp0.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _enable_mipmap: true,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::LightShaft.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::ShadowAO.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::Shadow.to_string(),
            _texture_width: unsafe { constants::SHADOW_MAP_SIZE },
            _texture_height: unsafe { constants::SHADOW_MAP_SIZE },
            _texture_format: vk::Format::D32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::CaptureNormalMap.to_string(),
            _texture_width: unsafe { constants::CAPTURE_HEIGHT_MAP_SIZE },
            _texture_height: unsafe { constants::CAPTURE_HEIGHT_MAP_SIZE },
            _texture_format: vk::Format::R8G8B8A8_UNORM,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _image_layout_transition: ImageLayoutTransition::TransferUndefToColorAttachementWithTransferSrc,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::CaptureHeightMap.to_string(),
            _texture_width: unsafe { constants::CAPTURE_HEIGHT_MAP_SIZE },
            _texture_height: unsafe { constants::CAPTURE_HEIGHT_MAP_SIZE },
            _texture_format: vk::Format::R32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            _image_layout_transition: ImageLayoutTransition::TransferUndefToColorAttachementWithTransferSrc,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SSR.to_string(),
            _texture_width: render_viewport_width / 2,
            _texture_height: render_viewport_height / 2,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SSRResolved.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            ..hdr_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::SSRResolvedPrev.to_string(),
            _texture_width: render_viewport_width,
            _texture_height: render_viewport_height,
            ..hdr_texture_create_info.clone()
        },
        // FFT Ocean
        TextureCreateInfo {
            _texture_name: RenderTargetType::FFT_A.to_string(),
            _texture_width: fft_ocean::FFT_SIZE as u32,
            _texture_height: fft_ocean::FFT_SIZE as u32,
            _texture_layers: fft_ocean::FFT_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::TYPE_2D_ARRAY,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::REPEAT,
            _enable_mipmap: true,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::FFT_B.to_string(),
            _texture_width: fft_ocean::FFT_SIZE as u32,
            _texture_height: fft_ocean::FFT_SIZE as u32,
            _texture_layers: fft_ocean::FFT_LAYER_COUNT as u32,
            _texture_view_type: vk::ImageViewType::TYPE_2D_ARRAY,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::REPEAT,
            _enable_mipmap: true,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::FFT_SLOPE_VARIANCE.to_string(),
            _texture_width: fft_ocean::N_SLOPE_VARIANCE as u32,
            _texture_height: fft_ocean::N_SLOPE_VARIANCE as u32,
            _texture_layers: fft_ocean::N_SLOPE_VARIANCE as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R16G16B16A16_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        // Precomputed Atmosphere
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR.to_string(),
            ..precomputed_atmosphere_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED.to_string(),
            ..precomputed_atmosphere_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV.to_string(),
            ..precomputed_atmosphere_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_INSCATTER.to_string(),
            ..precomputed_atmosphere_texture_create_info.clone()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_TRANSMITTANCE.to_string(),
            _texture_width: precomputed_atmosphere::TRANSMITTANCE_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::TRANSMITTANCE_TEXTURE_HEIGHT as u32,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_SCATTERING.to_string(),
            _texture_width: precomputed_atmosphere::SCATTERING_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::SCATTERING_TEXTURE_HEIGHT as u32,
            _texture_layers: precomputed_atmosphere::SCATTERING_TEXTURE_DEPTH as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_IRRADIANCE.to_string(),
            _texture_width: precomputed_atmosphere::IRRADIANCE_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::IRRADIANCE_TEXTURE_HEIGHT as u32,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING
                .to_string(),
            _texture_width: precomputed_atmosphere::SCATTERING_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::SCATTERING_TEXTURE_HEIGHT as u32,
            _texture_layers: precomputed_atmosphere::SCATTERING_TEXTURE_DEPTH as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_IRRADIANCE.to_string(),
            _texture_width: precomputed_atmosphere::IRRADIANCE_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::IRRADIANCE_TEXTURE_HEIGHT as u32,
            _texture_view_type: vk::ImageViewType::TYPE_2D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_RAYLEIGH_SCATTERING
                .to_string(),
            _texture_width: precomputed_atmosphere::SCATTERING_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::SCATTERING_TEXTURE_HEIGHT as u32,
            _texture_layers: precomputed_atmosphere::SCATTERING_TEXTURE_DEPTH as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_MIE_SCATTERING
                .to_string(),
            _texture_width: precomputed_atmosphere::SCATTERING_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::SCATTERING_TEXTURE_HEIGHT as u32,
            _texture_layers: precomputed_atmosphere::SCATTERING_TEXTURE_DEPTH as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
        TextureCreateInfo {
            _texture_name: RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_SCATTERING_DENSITY
                .to_string(),
            _texture_width: precomputed_atmosphere::SCATTERING_TEXTURE_WIDTH as u32,
            _texture_height: precomputed_atmosphere::SCATTERING_TEXTURE_HEIGHT as u32,
            _texture_layers: precomputed_atmosphere::SCATTERING_TEXTURE_DEPTH as u32,
            _texture_view_type: vk::ImageViewType::TYPE_3D,
            _texture_format: vk::Format::R32G32B32A32_SFLOAT,
            _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
            ..Default::default()
        },
    ];
    texture_create_infos
}
