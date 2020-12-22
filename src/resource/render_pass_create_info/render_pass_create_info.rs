use crate::renderer::renderer::{
    RenderObjectType,
    RendererData,
};
use crate::resource::render_pass_create_info::{
    clear_gbuffer,
    composite_gbuffer,
    downsampling,
    generate_min_z,
    render_bloom,
    render_copy,
    render_color,
    render_debug,
    render_final,
    render_gaussian_blur,
    render_motion_blur,
    render_object,
    render_shadow,
    render_ssao,
    render_ssao_blur,
    render_ssr,
    render_taa,
};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    vec![
        clear_gbuffer::get_render_pass_data_create_info(renderer_data),
        composite_gbuffer::get_render_pass_data_create_info(renderer_data),
        downsampling::get_render_pass_data_create_info(renderer_data),
        generate_min_z::get_render_pass_data_create_info(renderer_data),
        render_bloom::get_render_pass_data_create_info(renderer_data),
        render_copy::get_render_pass_data_create_info(renderer_data),
        render_color::get_render_pass_data_create_info(renderer_data),
        render_debug::get_render_pass_data_create_info(renderer_data),
        render_final::get_render_pass_data_create_info(renderer_data),
        render_gaussian_blur::get_render_pass_data_create_info(renderer_data),
        render_motion_blur::get_render_pass_data_create_info(renderer_data),
        render_object::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        render_object::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        render_ssao::get_render_pass_data_create_info(renderer_data),
        render_ssao_blur::get_render_pass_data_create_info(renderer_data),
        render_ssr::get_render_pass_data_create_info(renderer_data),
        render_taa::get_render_pass_data_create_info(renderer_data),
    ]
}