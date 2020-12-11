use rand;
use nalgebra::{Vector3, Vector4};
use ash::{ vk, Device };

use crate::constants;
use crate::renderer::shader_buffer_datas::{ SSAOConstants };
use crate::resource::Resources;
use crate::vulkan_context::framebuffer::{ self, FramebufferData, FramebufferDataCreateInfo };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;
use crate::vulkan_context::vulkan_context;
use crate::utilities::system::RcRefCell;

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_Bloom {
    pub _bloom_framebuffer_data0: FramebufferData,
    pub _bloom_descriptor_set0: SwapchainIndexMap<vk::DescriptorSet>,
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
}

impl Default for PostProcessData_Bloom {
    fn default() -> PostProcessData_Bloom {
        PostProcessData_Bloom {
            _bloom_framebuffer_data0: FramebufferData::default(),
            _bloom_descriptor_set0: Vec::new(),
        }
    }
}

impl PostProcessData_Bloom {
    pub fn initialize(&mut self, device: &Device, resources: &RcRefCell<Resources>, rendertarget_bloom1: &TextureData) {
        let resources = resources.borrow();
        let render_pass_data = resources.get_render_pass_data("render_bloom").borrow();
        //let framebuffer_data = resources.get_framebuffer_data("render_bloom").borrow();
        let (width, height) = rendertarget_bloom1.get_default_image_size();
        let rendertarget_views = vec![rendertarget_bloom1.get_default_rendertarget_view()];
        self._bloom_framebuffer_data0.clone_from(&framebuffer::create_framebuffer_data(
            device,
            render_pass_data._render_pass,
            FramebufferDataCreateInfo {
                _framebuffer_name: rendertarget_bloom1._texture_data_name.clone(),
                _framebuffer_width: width,
                _framebuffer_height: height,
                _framebuffer_view_port: vulkan_context::create_viewport(0, 0, width, height, 0.0, 1.0),
                _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, width, height),
                _framebuffer_color_attachment_formats: vec![rendertarget_bloom1._image_format],
                _framebuffer_image_views: vec![rendertarget_views; constants::SWAPCHAIN_IMAGE_COUNT],
                ..Default::default()
            },
        ));
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._bloom_framebuffer_data0);
    }
}


impl Default for PostProcessData_SSAO {
    fn default() -> PostProcessData_SSAO {
        let mut random_normals: [Vector4<f32>; 64] = [Vector4::new(0.0, 0.0, 0.0, 0.0); constants::SSAO_KERNEL_SIZE];
        for i in 0..constants::SSAO_KERNEL_SIZE {
            let scale = rand::random::<f32>();
            let normal = Vector3::new(
                rand::random::<f32>() * 2.0 - 1.0,
                rand::random::<f32>() * 0.5 + 0.5,
                rand::random::<f32>() * 2.0 - 1.0
            ).normalize() * scale;
            random_normals[i] = Vector4::new(normal.x, normal.y, normal.z, 0.0);
        }

        PostProcessData_SSAO {
            _ssao_kernel_size: constants::SSAO_KERNEL_SIZE as i32,
            _ssao_radius: constants::SSAO_RADIUS,
            _ssao_noise_dim: constants::SSAO_NOISE_DIM,
            _ssao_constants: SSAOConstants {
                _ssao_kernel_samples: random_normals
            },
        }
    }
}
