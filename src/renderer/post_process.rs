use rand;
use nalgebra::{Vector3, Vector4};
use ash::{ vk, Device };

use crate::constants;
use crate::renderer::shader_buffer_datas::{ SSAOConstants };
use crate::resource::Resources;
use crate::vulkan_context::descriptor::{ self, DescriptorResourceInfo };
use crate::vulkan_context::framebuffer::{ self, FramebufferData, FramebufferDataCreateInfo };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;
use crate::vulkan_context::vulkan_context;
use crate::utilities::system::RcRefCell;

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_Bloom {
    pub _bloom_framebuffer_data1: FramebufferData,
    pub _bloom_descriptor_set1: SwapchainIndexMap<vk::DescriptorSet>,
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
            _bloom_framebuffer_data1: FramebufferData::default(),
            _bloom_descriptor_set1: Vec::new(),
        }
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

impl PostProcessData_Bloom {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        render_target_bloom0: &TextureData,
        render_target_bloom1: &TextureData,
        render_target_bloom2: &TextureData,
        render_target_bloom3: &TextureData,
        render_target_bloom4: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_bloom_material_instance = resources.get_material_instance_data("render_bloom").borrow();
        let pipeline_binding_data = render_bloom_material_instance.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
        let render_pass = &pipeline_binding_data._render_pass_pipeline_data._render_pass_data.borrow()._render_pass;
        let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
        let (width, height) = render_target_bloom1.get_default_image_size();
        let rendertarget_views = vec![render_target_bloom1.get_default_rendertarget_view()];
        let bloom_framebuffer_data1 = framebuffer::create_framebuffer_data(
            device,
            *render_pass,
            FramebufferDataCreateInfo {
                _framebuffer_name: render_target_bloom1._texture_data_name.clone(),
                _framebuffer_width: width,
                _framebuffer_height: height,
                _framebuffer_view_port: vulkan_context::create_viewport(0, 0, width, height, 0.0, 1.0),
                _framebuffer_scissor_rect: vulkan_context::create_rect_2d(0, 0, width, height),
                _framebuffer_color_attachment_formats: vec![render_target_bloom1._image_format],
                _framebuffer_image_views: vec![rendertarget_views; constants::SWAPCHAIN_IMAGE_COUNT],
                ..Default::default()
            },
        );

        let mut descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
        let descriptor_data = &pipeline_data._descriptor_data;
        for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
            for descriptor_resource_infos in descriptor_resource_infos_list.get_mut(*swapchain_index).iter_mut() {
                let descriptor_binding_index: usize = 2;
                descriptor_resource_infos[descriptor_binding_index] = DescriptorResourceInfo::DescriptorImageInfo(render_target_bloom0._descriptor_image_info);
            }
        }

        let descriptor_sets = descriptor::create_descriptor_sets(device, descriptor_data);
        let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
            descriptor_data_create_info._descriptor_binding_index
        }).collect();
        let _write_descriptor_sets: SwapchainIndexMap<Vec<vk::WriteDescriptorSet>> = descriptor::create_write_descriptor_sets_with_update(
            device,
            &descriptor_sets,
            &descriptor_binding_indices,
            &descriptor_data._descriptor_set_layout_bindings,
            &descriptor_resource_infos_list,
        );

        self._bloom_framebuffer_data1 = bloom_framebuffer_data1;
        self._bloom_descriptor_set1 = descriptor_sets;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._bloom_framebuffer_data1);
    }
}


