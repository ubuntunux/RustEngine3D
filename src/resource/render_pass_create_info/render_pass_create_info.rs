use crate::renderer::renderer::{
    RenderObjectType,
    RendererData,
};
use crate::vulkan_context::render_pass::RenderPassDataCreateInfo;
use crate::resource::render_pass_create_info::{
    render_object
};

pub fn get_render_pass_data_create_infos(renderer_data: &RendererData) -> Vec<RenderPassDataCreateInfo> {
    // render_debug <- RenderDebug.getRenderPassDataCreateInfo rendererData
    // render_skeletal_object <- RenderObject.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Skeletal
    // render_static_shadow <- RenderShadow.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Static
    // render_skeletal_shadow <- RenderShadow.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Skeletal
    // composite_gbuffer <- CompositeGBuffer.getRenderPassDataCreateInfo rendererData
    // render_final <- RenderFinal.getRenderPassDataCreateInfo rendererData
    // render_motion_blur <- RenderMotionBlur.getRenderPassDataCreateInfo rendererData
    // render_ssao <- RenderSSAO.getRenderPassDataCreateInfo rendererData
    vec![
        render_object::get_render_pass_data_create_info(renderer_data, RenderObjectType::Static),
    ]
}