use std::cmp::max;

use ash::{
    vk,
    Device,
};
use ash::version::DeviceV1_0;

use crate::constants;
use crate::vulkan_context::vulkan_context::{
    self,
    SwapchainIndexMap
};
use crate::vulkan_context::texture::TextureData;


#[derive(Clone)]
pub struct FramebufferDataCreateInfo {
    pub _framebuffer_width: u32,
    pub _framebuffer_height: u32,
    pub _framebuffer_depth: u32,
    pub _framebuffer_layer: u32,
    pub _framebuffer_sample_count: vk::SampleCountFlags,
    pub _framebuffer_view_port: vk::Viewport,
    pub _framebuffer_scissor_rect: vk::Rect2D,
    pub _framebuffer_color_attachment_formats: Vec<vk::Format>,
    pub _framebuffer_depth_attachment_formats: Vec<vk::Format>,
    pub _framebuffer_resolve_attachment_formats: Vec<vk::Format>,
    pub _framebuffer_image_views: SwapchainIndexMap<Vec<vk::ImageView>>,
    pub _framebuffer_clear_values: Vec<vk::ClearValue>,
}

impl Default for FramebufferDataCreateInfo {
    fn default() -> FramebufferDataCreateInfo {
        FramebufferDataCreateInfo {
            _framebuffer_width: 1024,
            _framebuffer_height: 768,
            _framebuffer_depth: 1,
            _framebuffer_layer: 1,
            _framebuffer_sample_count: vk::SampleCountFlags::TYPE_1,
            _framebuffer_view_port: vulkan_context::create_viewport(0, 0, 1024, 768, 0.0, 1.0),
            _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, 1024, 768),
            _framebuffer_color_attachment_formats: Vec::<vk::Format>::new(),
            _framebuffer_depth_attachment_formats: Vec::<vk::Format>::new(),
            _framebuffer_resolve_attachment_formats: Vec::<vk::Format>::new(),
            _framebuffer_image_views: SwapchainIndexMap::<Vec<vk::ImageView>>::new(),
            _framebuffer_clear_values: Vec::<vk::ClearValue>::new()
        }
    }
}

#[derive(Clone)]
pub struct RenderTargetInfo<'a> {
    pub _texture_data: &'a TextureData,
    pub _layer: u32,
    pub _mip_level: u32,
    pub _clear_value: Option<vk::ClearValue>,
}

#[derive(Clone, Default)]
pub struct FramebufferData {
    pub _framebuffer_name: String,
    pub _framebuffer_info: FramebufferDataCreateInfo,
    pub _framebuffers: SwapchainIndexMap<vk::Framebuffer>,
    pub _render_pass_begin_infos: SwapchainIndexMap<vk::RenderPassBeginInfo>,
}

impl FramebufferDataCreateInfo {
    pub fn is_valid(&self) -> bool {
        false == self._framebuffer_image_views.is_empty()
    }
}

pub fn create_framebuffer_data_create_info(
    color_render_targets: &[RenderTargetInfo],
    depth_render_targets: &[RenderTargetInfo],
    resolve_render_targets: &[RenderTargetInfo],
) -> FramebufferDataCreateInfo {
    let (mut width, mut height, mut _depth, _layer, sample_count, mip_level) = if false == color_render_targets.is_empty() {
        ( color_render_targets[0]._texture_data._image_width,
          color_render_targets[0]._texture_data._image_height,
          color_render_targets[0]._texture_data._image_depth,
          color_render_targets[0]._texture_data._image_layer,
          color_render_targets[0]._texture_data._image_sample_count,
          color_render_targets[0]._mip_level,

        )
    } else if false == depth_render_targets.is_empty() {
        ( depth_render_targets[0]._texture_data._image_width,
          depth_render_targets[0]._texture_data._image_height,
          depth_render_targets[0]._texture_data._image_depth,
          depth_render_targets[0]._texture_data._image_layer,
          depth_render_targets[0]._texture_data._image_sample_count,
          depth_render_targets[0]._mip_level,
        )
    } else {
        panic!("create_framebuffer_data error");
    };

    if constants::INVALID_MIP_LEVEL != mip_level {
        width = max(1, width >> mip_level);
        height = max(1, height >> mip_level);
        _depth = max(1, _depth >> mip_level);
    }

    let mut rendertarget_views = Vec::new();
    let mut color_attachment_formats = Vec::new();
    let mut depth_attachment_formats = Vec::new();
    let mut resolve_attachment_formats = Vec::new();
    let mut clear_values: Vec<vk::ClearValue> = Vec::new();
    for render_target in color_render_targets.iter() {
        rendertarget_views.push(render_target._texture_data.get_sub_image_view(render_target._layer, render_target._mip_level));
        color_attachment_formats.push(render_target._texture_data._image_format);
        if let Some(clear_value) = render_target._clear_value {
            clear_values.push(clear_value);
        }
    }
    for render_target in depth_render_targets.iter() {
        rendertarget_views.push(render_target._texture_data.get_sub_image_view(render_target._layer, render_target._mip_level));
        depth_attachment_formats.push(render_target._texture_data._image_format);
        if let Some(clear_value) = render_target._clear_value {
            clear_values.push(clear_value);
        }
    }
    for render_target in resolve_render_targets.iter() {
        rendertarget_views.push(render_target._texture_data.get_sub_image_view(render_target._layer, render_target._mip_level));
        resolve_attachment_formats.push(render_target._texture_data._image_format);
    }

    FramebufferDataCreateInfo {
        _framebuffer_width: width,
        _framebuffer_height: height,
        _framebuffer_depth: 1,
        _framebuffer_layer: 1,
        _framebuffer_sample_count: sample_count,
        _framebuffer_view_port: vulkan_context::create_viewport(0, 0, width, height, 0.0, 1.0),
        _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, width, height),
        _framebuffer_color_attachment_formats: color_attachment_formats,
        _framebuffer_depth_attachment_formats: depth_attachment_formats,
        _framebuffer_resolve_attachment_formats: resolve_attachment_formats,
        _framebuffer_image_views: vec![rendertarget_views; constants::SWAPCHAIN_IMAGE_COUNT],
        _framebuffer_clear_values: clear_values,
    }
}


pub fn create_framebuffer_data(
    device: &Device,
    render_pass: vk::RenderPass,
    framebuffer_name: &str,
    framebuffer_data_create_info: FramebufferDataCreateInfo
) -> FramebufferData {
    let layers = max(framebuffer_data_create_info._framebuffer_depth, framebuffer_data_create_info._framebuffer_layer);
    log::info!("create_framebuffer_data: {:?} {} {} {}",
        framebuffer_name,
        framebuffer_data_create_info._framebuffer_width,
        framebuffer_data_create_info._framebuffer_height,
        layers
    );

    let get_framebuffer_create_info = |index: usize| -> vk::FramebufferCreateInfo {
        vk::FramebufferCreateInfo {
            render_pass,
            attachment_count: framebuffer_data_create_info._framebuffer_image_views[index].len() as u32,
            p_attachments: framebuffer_data_create_info._framebuffer_image_views[index].as_ptr(),
            width: framebuffer_data_create_info._framebuffer_width,
            height: framebuffer_data_create_info._framebuffer_height,
            layers,
            ..Default::default()
        }
    };

    unsafe {
        let framebuffers: Vec<vk::Framebuffer> = constants::SWAPCHAIN_IMAGE_INDICES
            .iter()
            .map(|index| {
                device.create_framebuffer(&get_framebuffer_create_info(*index), None).expect("vkCreateFramebuffer failed!")
            }).collect();

        let render_pass_begin_infos: Vec<vk::RenderPassBeginInfo> = framebuffers
            .iter()
            .map(|framebuffer| {
                vk::RenderPassBeginInfo {
                    render_pass,
                    framebuffer: *framebuffer,
                    render_area: vulkan_context::create_rect_2d(
                        0,
                        0,
                        framebuffer_data_create_info._framebuffer_width,
                        framebuffer_data_create_info._framebuffer_height
                    ),
                    clear_value_count: framebuffer_data_create_info._framebuffer_clear_values.len() as u32,
                    p_clear_values: framebuffer_data_create_info._framebuffer_clear_values.as_ptr(),
                    ..Default::default()
                }
            }).collect();

        FramebufferData {
            _framebuffer_name: String::from(framebuffer_name),
            _framebuffer_info: framebuffer_data_create_info,
            _framebuffers: framebuffers,
            _render_pass_begin_infos: render_pass_begin_infos,
        }
    }
}

pub fn destroy_framebuffer_data(device: &Device, framebuffer_data: &FramebufferData) {
    log::info!("destroy_framebuffer_data: {:?} {:?}", framebuffer_data._framebuffer_name, framebuffer_data._framebuffers);
    unsafe {
        for framebuffer in framebuffer_data._framebuffers.iter() {
            device.destroy_framebuffer(*framebuffer, None);
        }
    }
}