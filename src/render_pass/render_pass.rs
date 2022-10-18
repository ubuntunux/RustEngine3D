use ash::vk;

use crate::effect::effect_data::{ ParticleBlendMode, ParticleGeometryType };
use crate::renderer::renderer_data::RenderObjectType;
use crate::renderer::renderer_data::RendererData;
use crate::render_pass::{
    common,
    effect,
    fft_ocean,
    precomputed_atmosphere,
    ray_tracing
};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    let mut render_pass_data_create_infos = vec![
        common::clear_render_target::get_render_pass_data_create_info(renderer_data, &[vk::Format::R16G16B16A16_SFLOAT], vk::Format::UNDEFINED),
        common::clear_render_target::get_render_pass_data_create_info(renderer_data, &[vk::Format::R32_SFLOAT], vk::Format::UNDEFINED),
        common::clear_render_target::get_render_pass_data_create_info(renderer_data, &[vk::Format::R32G32B32A32_SFLOAT], vk::Format::UNDEFINED),
        common::clear_render_target::get_render_pass_data_create_info(renderer_data, &[vk::Format::R16G16B16A16_SFLOAT], vk::Format::D32_SFLOAT),
        common::clear_render_target::get_render_pass_data_create_info(renderer_data, &[], vk::Format::D32_SFLOAT),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[vk::Format::R8G8B8A8_UNORM, vk::Format::R8G8B8A8_UNORM, vk::Format::R8G8B8A8_UNORM, vk::Format::R16G16_SFLOAT],
            vk::Format::D32_SFLOAT
        ),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_gbuffer"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_shadow"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_capture_height_map"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_0"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_1"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_2"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_3"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_4"),
        common::clear_framebuffer::get_render_pass_data_create_info(renderer_data, "clear_light_probe_depth_5"),
        common::composite_gbuffer::get_render_pass_data_create_info(renderer_data),
        common::copy_cube_map::get_render_pass_data_create_info(renderer_data),
        common::downsampling::get_render_pass_data_create_info(renderer_data),
        common::generate_min_z::get_render_pass_data_create_info(renderer_data),
        common::render_bloom::get_render_pass_data_create_info(renderer_data),
        common::render_copy::get_render_pass_data_create_info(renderer_data),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R16G16B16A16_SFLOAT),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R32_SFLOAT),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R32G32B32A32_SFLOAT),
        common::render_debug::get_render_pass_data_create_info(renderer_data),
        common::render_font::get_render_pass_data_create_info(renderer_data),
        common::render_final::get_render_pass_data_create_info(renderer_data),
        common::render_gaussian_blur::get_render_pass_data_create_info(renderer_data),
        common::render_motion_blur::get_render_pass_data_create_info(renderer_data),
        common::render_gbuffer::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        common::render_gbuffer::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        common::render_forward::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        common::render_forward::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 0),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 1),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 2),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 3),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 4),
        common::render_forward_for_light_probe::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static, 5),
        common::render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        common::render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        common::capture_height_map::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        common::render_ssao::get_render_pass_data_create_info(renderer_data),
        common::render_ssao_blur::get_render_pass_data_create_info(renderer_data),
        common::render_ssr::get_render_pass_data_create_info(renderer_data),
        common::render_taa_simple::get_render_pass_data_create_info(renderer_data),
        common::render_taa::get_render_pass_data_create_info(renderer_data),
        common::render_ui::get_render_pass_data_create_info(renderer_data),
        effect::process_gpu_particle::get_render_pass_data_create_info(renderer_data),
        effect::render_particle_translucent::get_render_pass_data_create_info(renderer_data, ParticleBlendMode::AlphaBlend, ParticleGeometryType::Quad),
        fft_ocean::render_fft_init::get_render_pass_data_create_info(renderer_data),
        fft_ocean::render_fft_ocean::get_render_pass_data_create_info(renderer_data),
        fft_ocean::render_fft_variance::get_render_pass_data_create_info(renderer_data),
        fft_ocean::render_fft_waves::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::composite_atmosphere::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_transmittance::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_direct_irradiance::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_indirect_irradiance::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_multiple_scattering::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_single_scattering::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::compute_scattering_density::get_render_pass_data_create_info(renderer_data),
        precomputed_atmosphere::render_atmosphere::get_render_pass_data_create_info(renderer_data),
    ];

    if renderer_data.get_renderer_context().get_use_ray_tracing() {
        render_pass_data_create_infos.push(ray_tracing::ray_tracing::get_render_pass_data_create_info(renderer_data));
    }

    render_pass_data_create_infos
}