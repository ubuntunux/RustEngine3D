use ash::vk;
use crate::effect::effect_data::ParticleGeometryType;
use crate::render_pass::{common, effect, fft_ocean, precomputed_atmosphere, ray_tracing, render_object};
use crate::renderer::renderer_data::{BlendMode, RendererData};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;

pub fn get_render_pass_data_create_infos(
    renderer_data: &RendererData,
) -> Vec<RenderPassDataCreateInfo> {
    let mut render_pass_data_create_infos = vec![
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[vk::Format::R16G16B16A16_SFLOAT],
            vk::Format::UNDEFINED,
        ),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[vk::Format::R32_SFLOAT],
            vk::Format::UNDEFINED,
        ),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[vk::Format::R32G32B32A32_SFLOAT],
            vk::Format::UNDEFINED,
        ),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[vk::Format::R16G16B16A16_SFLOAT],
            vk::Format::D32_SFLOAT,
        ),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[],
            vk::Format::D32_SFLOAT,
        ),
        common::clear_render_target::get_render_pass_data_create_info(
            renderer_data,
            &[
                vk::Format::R8G8B8A8_UNORM,
                vk::Format::R8G8B8A8_UNORM,
                vk::Format::R8G8B8A8_UNORM,
                vk::Format::R16G16_SFLOAT,
            ],
            vk::Format::D32_SFLOAT,
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
        common::render_bound_box::get_render_pass_data_create_info(renderer_data),
        common::render_copy::get_render_pass_data_create_info(renderer_data),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R16G16B16A16_SFLOAT),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R32_SFLOAT),
        common::render_color::get_render_pass_data_create_info(renderer_data, vk::Format::R32G32B32A32_SFLOAT),
        common::render_debug::get_render_pass_data_create_info(renderer_data),
        common::render_debug_line::get_render_pass_data_create_info(renderer_data),
        common::render_font::get_render_pass_data_create_info(renderer_data),
        common::render_final::get_render_pass_data_create_info(renderer_data),
        common::render_gaussian_blur::get_render_pass_data_create_info(renderer_data),
        common::render_motion_blur::get_render_pass_data_create_info(renderer_data),
        common::render_shadow_ao::get_render_pass_data_create_info(renderer_data),
        common::render_ssr::get_render_pass_data_create_info(renderer_data),
        common::render_taa_simple::get_render_pass_data_create_info(renderer_data),
        common::render_taa::get_render_pass_data_create_info(renderer_data),
        common::render_ui::get_render_pass_data_create_info(renderer_data),
        effect::process_gpu_particle::get_render_pass_data_create_info(renderer_data),
        effect::render_particle_translucent::get_render_pass_data_create_info(renderer_data, BlendMode::AlphaBlend, ParticleGeometryType::Mesh),
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
        precomputed_atmosphere::render_atmosphere::get_render_pass_data_create_info(renderer_data)
    ];
    render_pass_data_create_infos = [render_pass_data_create_infos, render_object::render_object::get_render_pass_data_create_infos(renderer_data)].concat();

    if renderer_data.get_renderer_context().get_use_ray_tracing() {
        render_pass_data_create_infos.push(ray_tracing::ray_tracing::get_render_pass_data_create_info(renderer_data));
    }

    render_pass_data_create_infos
}
