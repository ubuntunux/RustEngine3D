use crate::renderer::renderer::{
    RenderObjectType,
    RendererData,
};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;
use crate::renderer::render_pass_create_info::{
    composite_gbuffer,
    render_bloom,
    render_debug,
    render_final,
    render_motion_blur,
    render_object,
    render_shadow,
    render_ssao,
};

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    vec![
        composite_gbuffer::get_render_pass_data_create_info(renderer_data),
        render_bloom::get_render_pass_data_create_info(renderer_data),
        render_debug::get_render_pass_data_create_info(renderer_data),
        render_final::get_render_pass_data_create_info(renderer_data),
        render_motion_blur::get_render_pass_data_create_info(renderer_data),
        render_object::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        render_object::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Skeletal),
        render_shadow::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
        render_ssao::get_render_pass_data_create_info(renderer_data),
    ]
}