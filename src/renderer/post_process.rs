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
    pub _store_framebuffer_datas: Vec<FramebufferData>,
    pub _store_descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_framebuffer_datas: Vec<*const FramebufferData>,
    pub _bloom_descriptor_sets: Vec<*const SwapchainIndexMap<vk::DescriptorSet>>,
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
            _store_framebuffer_datas: Vec::new(),
            _store_descriptor_sets: Vec::new(),
            _bloom_framebuffer_datas: Vec::new(),
            _bloom_descriptor_sets: Vec::new(),
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
        let render_pass_data = pipeline_binding_data._render_pass_pipeline_data._render_pass_data.borrow();
        let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
        let descriptor_data = &pipeline_data._descriptor_data;
        let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
            descriptor_data_create_info._descriptor_binding_index
        }).collect();

        let create_pipeline_binding_datas = |render_target: &TextureData, input_texture: &TextureData| -> (FramebufferData, SwapchainIndexMap<vk::DescriptorSet>) {
            let framebuffer_data = framebuffer::create_framebuffer_data(
                device,
                render_pass_data._render_pass,
                format!("{}{}", render_pass_data._render_pass_data_name, render_target._texture_data_name).as_str(),
                framebuffer::create_framebuffer_data_create_info(vec![render_target], Vec::new(), Vec::new(), Vec::new()),
            );
            let mut descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
            for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
                for descriptor_resource_infos in descriptor_resource_infos_list.get_mut(*swapchain_index).iter_mut() {
                    let descriptor_binding_index: usize = 2;
                    descriptor_resource_infos[descriptor_binding_index] = DescriptorResourceInfo::DescriptorImageInfo(input_texture._descriptor_image_info);
                }
            }
            let descriptor_sets = descriptor::create_descriptor_sets(device, descriptor_data);
            let _write_descriptor_sets: SwapchainIndexMap<Vec<vk::WriteDescriptorSet>> = descriptor::create_write_descriptor_sets_with_update(
                device,
                &descriptor_sets,
                &descriptor_binding_indices,
                &descriptor_data._descriptor_set_layout_bindings,
                &descriptor_resource_infos_list,
            );
            (framebuffer_data, descriptor_sets)
        };

        let (bloom_framebuffer_data1, bloom_descriptor_set1) = create_pipeline_binding_datas(render_target_bloom1, render_target_bloom0);
        let (bloom_framebuffer_data2, bloom_descriptor_set2) = create_pipeline_binding_datas(render_target_bloom2, render_target_bloom1);
        let (bloom_framebuffer_data3, bloom_descriptor_set3) = create_pipeline_binding_datas(render_target_bloom3, render_target_bloom2);
        let (bloom_framebuffer_data4, bloom_descriptor_set4) = create_pipeline_binding_datas(render_target_bloom4, render_target_bloom3);
        self._store_framebuffer_datas.push(bloom_framebuffer_data1);
        self._store_framebuffer_datas.push(bloom_framebuffer_data2);
        self._store_framebuffer_datas.push(bloom_framebuffer_data3);
        self._store_framebuffer_datas.push(bloom_framebuffer_data4);
        self._store_descriptor_sets.push(bloom_descriptor_set1);
        self._store_descriptor_sets.push(bloom_descriptor_set2);
        self._store_descriptor_sets.push(bloom_descriptor_set3);
        self._store_descriptor_sets.push(bloom_descriptor_set4);

        let bloom_framebuffer_data0 = resources.get_framebuffer_data("render_bloom").as_ptr();
        self._bloom_framebuffer_datas.push(bloom_framebuffer_data0);
        for framebuffer_data in self._store_framebuffer_datas.iter() {
            self._bloom_framebuffer_datas.push(framebuffer_data);
        }
        self._bloom_descriptor_sets.push(&pipeline_binding_data._descriptor_sets);
        for descriptor_set in self._store_descriptor_sets.iter() {
            self._bloom_descriptor_sets.push(descriptor_set);
        }
    }

    pub fn destroy(&mut self, device: &Device) {
        for framebuffer_data in self._store_framebuffer_datas.iter() {
            framebuffer::destroy_framebuffer_data(device, &framebuffer_data);
        }
        self._store_framebuffer_datas.clear();
        self._store_descriptor_sets.clear();
        self._bloom_framebuffer_datas.clear();
        self._bloom_descriptor_sets.clear();
    }
}


