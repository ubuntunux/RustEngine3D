use rand;
use nalgebra::{Vector2, Vector3, Vector4};
use ash::{ vk, Device };

use crate::constants;
use crate::renderer::material_instance::{ PipelineBindingData };
use crate::renderer::shader_buffer_datas::{ PushConstant_BloomHighlight, PushConstant_GaussianBlur, SSAOConstants };
use crate::resource::Resources;
use crate::vulkan_context::descriptor::{ self, DescriptorResourceInfo };
use crate::vulkan_context::framebuffer::{ self, FramebufferData };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;
use crate::utilities::system::RcRefCell;

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_Bloom {
    pub _bloom_downsample_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_downsample_descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_temp_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_temp_descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_blur_framebuffer_datas: Vec<*const FramebufferData>,
    pub _bloom_blur_descriptor_sets: Vec<*const SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_push_constants: PushConstant_BloomHighlight,
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
    pub _ssao_blur_framebuffer_data0: FramebufferData,
    pub _ssao_blur_framebuffer_data1: FramebufferData,
    pub _ssao_blur_descriptor_sets0: SwapchainIndexMap<vk::DescriptorSet>,
    pub _ssao_blur_descriptor_sets1: SwapchainIndexMap<vk::DescriptorSet>,
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct PostProcessData_TAA {
    pub _enable_taa: bool,
    pub _rendertarget_width: u32,
    pub _rendertarget_height: u32,
    pub _taa_resolve_framebuffer_data: FramebufferData,
    pub _taa_descriptor_sets: SwapchainIndexMap<vk::DescriptorSet>,
}

impl Default for PostProcessData_Bloom {
    fn default() -> PostProcessData_Bloom {
        PostProcessData_Bloom {
            _bloom_downsample_framebuffer_datas: Vec::new(),
            _bloom_downsample_descriptor_sets: Vec::new(),
            _bloom_temp_framebuffer_datas: Vec::new(),
            _bloom_temp_descriptor_sets: Vec::new(),
            _bloom_blur_framebuffer_datas: Vec::new(),
            _bloom_blur_descriptor_sets: Vec::new(),
            _bloom_push_constants: PushConstant_BloomHighlight {
                _bloom_threshold_min: 0.5,
                _bloom_threshold_max: 1000.0,
                _bloom_intensity: 0.25,
                _bloom_scale: 1.0,
            },
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
            _ssao_blur_framebuffer_data0: FramebufferData::default(),
            _ssao_blur_framebuffer_data1: FramebufferData::default(),
            _ssao_blur_descriptor_sets0: SwapchainIndexMap::new(),
            _ssao_blur_descriptor_sets1: SwapchainIndexMap::new(),
        }
    }
}

impl Default for PostProcessData_TAA {
    fn default() -> PostProcessData_TAA {
        PostProcessData_TAA {
            _enable_taa: true,
            _rendertarget_width: 1024,
            _rendertarget_height: 768,
            _taa_resolve_framebuffer_data: FramebufferData::default(),
            _taa_descriptor_sets: SwapchainIndexMap::new()
        }
    }
}


pub fn create_framebuffer_and_descriptor_data(
    device: &Device,
    pipeline_binding_data: &PipelineBindingData,
    render_target: &TextureData,
    descriptor_binding_index: usize,
    input_texture: &TextureData,
) -> (FramebufferData, SwapchainIndexMap<vk::DescriptorSet>) {
    let render_pass_data = pipeline_binding_data._render_pass_pipeline_data._render_pass_data.borrow();
    let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
    let descriptor_data = &pipeline_data._descriptor_data;
    let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
        descriptor_data_create_info._descriptor_binding_index
    }).collect();
    let framebuffer_data = framebuffer::create_framebuffer_data(
        device,
        render_pass_data._render_pass,
        format!("{}{}", render_pass_data._render_pass_data_name, render_target._texture_data_name).as_str(),
        framebuffer::create_framebuffer_data_create_info(vec![render_target], Vec::new(), Vec::new(), Vec::new()),
    );
    let mut descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
    for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
        for descriptor_resource_infos in descriptor_resource_infos_list.get_mut(*swapchain_index).iter_mut() {
            descriptor_resource_infos[descriptor_binding_index] = DescriptorResourceInfo::DescriptorImageInfo(input_texture.get_default_image_info());
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
        render_target_bloom_temp0: &TextureData,
        render_target_bloom_temp1: &TextureData,
        render_target_bloom_temp2: &TextureData,
        render_target_bloom_temp3: &TextureData,
        render_target_bloom_temp4: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_bloom_material_instance = resources.get_material_instance_data("render_bloom").borrow();
        let pipeline_binding_data = render_bloom_material_instance.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let descriptor_binding_index: usize = 0;
        let (bloom_framebuffer_data1, bloom_descriptor_set1) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom1, descriptor_binding_index, render_target_bloom0);
        let (bloom_framebuffer_data2, bloom_descriptor_set2) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom2, descriptor_binding_index, render_target_bloom1);
        let (bloom_framebuffer_data3, bloom_descriptor_set3) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom3, descriptor_binding_index, render_target_bloom2);
        let (bloom_framebuffer_data4, bloom_descriptor_set4) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom4, descriptor_binding_index, render_target_bloom3);
        self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data1);
        self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data2);
        self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data3);
        self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data4);
        self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set1);
        self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set2);
        self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set3);
        self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set4);

        let render_gaussian_blur_material_instance = resources.get_material_instance_data("render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance.get_pipeline_binding_data("render_gaussian_blur/render_gaussian_blur");
        let descriptor_binding_index: usize = 0;
        let (gaussian_blur_h_framebuffer_data0, gaussian_blur_h_descriptor_sets0) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom_temp0, descriptor_binding_index, render_target_bloom0);
        let (gaussian_blur_v_framebuffer_data0, gaussian_blur_v_descriptor_sets0) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom0, descriptor_binding_index, render_target_bloom_temp0);
        let (gaussian_blur_h_framebuffer_data1, gaussian_blur_h_descriptor_sets1) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom_temp1, descriptor_binding_index, render_target_bloom1);
        let (gaussian_blur_v_framebuffer_data1, gaussian_blur_v_descriptor_sets1) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom1, descriptor_binding_index, render_target_bloom_temp1);
        let (gaussian_blur_h_framebuffer_data2, gaussian_blur_h_descriptor_sets2) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom_temp2, descriptor_binding_index, render_target_bloom2);
        let (gaussian_blur_v_framebuffer_data2, gaussian_blur_v_descriptor_sets2) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom2, descriptor_binding_index, render_target_bloom_temp2);
        let (gaussian_blur_h_framebuffer_data3, gaussian_blur_h_descriptor_sets3) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom_temp3, descriptor_binding_index, render_target_bloom3);
        let (gaussian_blur_v_framebuffer_data3, gaussian_blur_v_descriptor_sets3) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom3, descriptor_binding_index, render_target_bloom_temp3);
        let (gaussian_blur_h_framebuffer_data4, gaussian_blur_h_descriptor_sets4) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom_temp4, descriptor_binding_index, render_target_bloom4);
        let (gaussian_blur_v_framebuffer_data4, gaussian_blur_v_descriptor_sets4) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_bloom4, descriptor_binding_index, render_target_bloom_temp4);

        self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data0);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data0);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data1);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data1);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data2);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data2);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data3);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data3);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data4);
        self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data4);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets0);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets0);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets1);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets1);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets2);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets2);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets3);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets3);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets4);
        self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets4);

        let render_bloom_framebuffer_data = resources.get_framebuffer_data("render_bloom").as_ptr();
        let render_bloom_hightlight_pipeline_binding_data = render_bloom_material_instance.get_pipeline_binding_data("render_bloom/render_bloom_highlight");

        self._bloom_blur_framebuffer_datas.push(render_bloom_framebuffer_data);
        for framebuffer_data in self._bloom_downsample_framebuffer_datas.iter() {
            self._bloom_blur_framebuffer_datas.push(framebuffer_data);
        }
        self._bloom_blur_descriptor_sets.push(&render_bloom_hightlight_pipeline_binding_data._descriptor_sets);
        for descriptor_set in self._bloom_downsample_descriptor_sets.iter() {
            self._bloom_blur_descriptor_sets.push(descriptor_set);
        }
    }

    pub fn destroy(&mut self, device: &Device) {
        for framebuffer_data in self._bloom_downsample_framebuffer_datas.iter() {
            framebuffer::destroy_framebuffer_data(device, &framebuffer_data);
        }
        for framebuffer_data in self._bloom_temp_framebuffer_datas.iter() {
            framebuffer::destroy_framebuffer_data(device, &framebuffer_data);
        }
        self._bloom_downsample_framebuffer_datas.clear();
        self._bloom_downsample_descriptor_sets.clear();
        self._bloom_temp_framebuffer_datas.clear();
        self._bloom_temp_descriptor_sets.clear();
        self._bloom_blur_framebuffer_datas.clear();
        self._bloom_blur_descriptor_sets.clear();
    }
}

impl PostProcessData_TAA {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        taa_render_target: &TextureData,
        taa_resolve_texture: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_copy_material_instance = resources.get_material_instance_data("render_copy").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_pipeline_binding_data("render_copy/render_copy");
        let descriptor_binding_index: usize = 0;
        let (framebuffer_data, descriptor_sets) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, taa_resolve_texture, descriptor_binding_index, taa_render_target);
        self._taa_resolve_framebuffer_data = framebuffer_data;
        self._taa_descriptor_sets = descriptor_sets;
        self._rendertarget_width = taa_render_target._image_width;
        self._rendertarget_height = taa_render_target._image_height;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._taa_resolve_framebuffer_data);
        self._taa_descriptor_sets.clear();
    }
}

impl PostProcessData_SSAO {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        render_target_ssao: &TextureData,
        render_target_ssao_temp: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_gaussian_blur_material_instance = resources.get_material_instance_data("render_ssao_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance.get_pipeline_binding_data("render_ssao_blur/render_ssao_blur");
        let descriptor_binding_index: usize = 0;
        let (ssao_blur_framebuffer_data0, ssao_blur_descriptor_sets0) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_ssao_temp, descriptor_binding_index, render_target_ssao);
        let (ssao_blur_framebuffer_data1, ssao_blur_descriptor_sets1) = create_framebuffer_and_descriptor_data(device, pipeline_binding_data, render_target_ssao, descriptor_binding_index, render_target_ssao_temp);

        self._ssao_blur_framebuffer_data0 = ssao_blur_framebuffer_data0;
        self._ssao_blur_framebuffer_data1 = ssao_blur_framebuffer_data1;
        self._ssao_blur_descriptor_sets0 = ssao_blur_descriptor_sets0;
        self._ssao_blur_descriptor_sets1 = ssao_blur_descriptor_sets1;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._ssao_blur_framebuffer_data0);
        framebuffer::destroy_framebuffer_data(device, &self._ssao_blur_framebuffer_data1);
    }
}