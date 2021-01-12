use rand;
use nalgebra::{ Vector3, Vector4 };
use ash::{ vk, Device };

use crate::constants;
use crate::renderer::push_constants::{ PushConstant_BloomHighlight };
use crate::renderer::RenderTargetType;
use crate::renderer::shader_buffer_datas::{ SSAOConstants };
use crate::renderer::utility;
use crate::resource::Resources;
use crate::vulkan_context::descriptor::{ self, DescriptorResourceInfo };
use crate::vulkan_context::framebuffer::{ self, FramebufferData };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;
use crate::utilities::system::RcRefCell;

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_ClearRenderTargets {
    pub _framebuffer_datas: Vec<FramebufferData>,
}

impl Default for RendererData_ClearRenderTargets {
    fn default() -> RendererData_ClearRenderTargets {
        RendererData_ClearRenderTargets {
            _framebuffer_datas: Vec::new()
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_CompositeGBuffer {
    pub _descriptor_sets0: SwapchainIndexMap<vk::DescriptorSet>,
    pub _descriptor_sets1: SwapchainIndexMap<vk::DescriptorSet>,
}

impl Default for RendererData_CompositeGBuffer {
    fn default() -> RendererData_CompositeGBuffer {
        RendererData_CompositeGBuffer {
            _descriptor_sets0: Vec::new(),
            _descriptor_sets1: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_SceneColorDownSampling {
    pub _dispatch_group_x: u32,
    pub _dispatch_group_y: u32,
    pub _descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
}

impl Default for RendererData_SceneColorDownSampling {
    fn default() -> RendererData_SceneColorDownSampling {
        RendererData_SceneColorDownSampling {
            _dispatch_group_x: 1,
            _dispatch_group_y: 1,
            _descriptor_sets: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_Bloom {
    pub _bloom_downsample_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_downsample_descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_temp_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_temp_descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_blur_framebuffer_datas: Vec<*const FramebufferData>,
    pub _bloom_blur_descriptor_sets: Vec<*const SwapchainIndexMap<vk::DescriptorSet>>,
    pub _bloom_push_constants: PushConstant_BloomHighlight,
}

impl Default for RendererData_Bloom {
    fn default() -> RendererData_Bloom {
        RendererData_Bloom {
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

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
    pub _ssao_blur_framebuffer_data0: FramebufferData,
    pub _ssao_blur_framebuffer_data1: FramebufferData,
    pub _ssao_blur_descriptor_sets0: SwapchainIndexMap<vk::DescriptorSet>,
    pub _ssao_blur_descriptor_sets1: SwapchainIndexMap<vk::DescriptorSet>,
}

impl Default for RendererData_SSAO {
    fn default() -> RendererData_SSAO {
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

        RendererData_SSAO {
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

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_TAA {
    pub _enable_taa: bool,
    pub _rendertarget_width: u32,
    pub _rendertarget_height: u32,
    pub _taa_resolve_framebuffer_data: FramebufferData,
    pub _taa_descriptor_sets: SwapchainIndexMap<vk::DescriptorSet>,
}

impl Default for RendererData_TAA {
    fn default() -> RendererData_TAA {
        RendererData_TAA {
            _enable_taa: true,
            _rendertarget_width: 1024,
            _rendertarget_height: 768,
            _taa_resolve_framebuffer_data: FramebufferData::default(),
            _taa_descriptor_sets: SwapchainIndexMap::new()
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_HierachicalMinZ {
    pub _dispatch_group_x: u32,
    pub _dispatch_group_y: u32,
    pub _descriptor_sets: Vec<SwapchainIndexMap<vk::DescriptorSet>>,
}

impl Default for RendererData_HierachicalMinZ {
    fn default() -> RendererData_HierachicalMinZ {
        RendererData_HierachicalMinZ {
            _dispatch_group_x: 1,
            _dispatch_group_y: 1,
            _descriptor_sets: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RendererData_SSR {
    pub _framebuffer_data0: FramebufferData,
    pub _framebuffer_data1: FramebufferData,
    pub _descriptor_sets0: SwapchainIndexMap<vk::DescriptorSet>,
    pub _descriptor_sets1: SwapchainIndexMap<vk::DescriptorSet>,
    pub _current_ssr_resolved: RenderTargetType,
    pub _previous_ssr_resolved: RenderTargetType,
}

impl Default for RendererData_SSR {
    fn default() -> RendererData_SSR {
        RendererData_SSR {
            _framebuffer_data0: FramebufferData::default(),
            _framebuffer_data1: FramebufferData::default(),
            _descriptor_sets0: SwapchainIndexMap::new(),
            _descriptor_sets1: SwapchainIndexMap::new(),
            _current_ssr_resolved: RenderTargetType::SSRResolved,
            _previous_ssr_resolved: RenderTargetType::SSRResolvedPrev,
        }
    }
}

impl RendererData_Bloom {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        render_target_bloom0: &TextureData,
        render_target_bloom_temp0: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_bloom_material_instance = resources.get_material_instance_data("render_bloom").borrow();
        let pipeline_binding_data = render_bloom_material_instance.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let descriptor_binding_index: usize = 0;
        let layer = 0;
        for mip_level in 0..(render_target_bloom0._image_mip_levels - 1) {
            let (bloom_framebuffer_data, bloom_descriptor_set) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom0, layer, mip_level + 1,
                descriptor_binding_index, render_target_bloom0, layer, mip_level, None
            );
            self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data);
            self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set);
        }

        let render_gaussian_blur_material_instance = resources.get_material_instance_data("render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance.get_pipeline_binding_data("render_gaussian_blur/render_gaussian_blur");
        let descriptor_binding_index: usize = 0;
        for mip_level in 0..render_target_bloom0._image_mip_levels {
            let (gaussian_blur_h_framebuffer_data, gaussian_blur_h_descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom_temp0, layer, mip_level,
                descriptor_binding_index, render_target_bloom0, layer, mip_level, None);
            let (gaussian_blur_v_framebuffer_data, gaussian_blur_v_descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom0, layer, mip_level,
                descriptor_binding_index, render_target_bloom_temp0, layer, mip_level, None);
            self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data);
            self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data);
            self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets);
            self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets);
        }

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

impl RendererData_TAA {
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
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (framebuffer_data, descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            taa_resolve_texture, layer, mip_level,
            descriptor_binding_index, taa_render_target, layer, mip_level, None,
        );
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

impl RendererData_SSAO {
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
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (ssao_blur_framebuffer_data0, ssao_blur_descriptor_sets0) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            render_target_ssao_temp, layer, mip_level,
            descriptor_binding_index, render_target_ssao, layer, mip_level, None,
        );
        let (ssao_blur_framebuffer_data1, ssao_blur_descriptor_sets1) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            render_target_ssao, layer, mip_level,
            descriptor_binding_index, render_target_ssao_temp, layer, mip_level, None,
        );
        self._ssao_blur_framebuffer_data0 = ssao_blur_framebuffer_data0;
        self._ssao_blur_framebuffer_data1 = ssao_blur_framebuffer_data1;
        self._ssao_blur_descriptor_sets0 = ssao_blur_descriptor_sets0;
        self._ssao_blur_descriptor_sets1 = ssao_blur_descriptor_sets1;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._ssao_blur_framebuffer_data0);
        framebuffer::destroy_framebuffer_data(device, &self._ssao_blur_framebuffer_data1);
        self._ssao_blur_descriptor_sets0.clear();
        self._ssao_blur_descriptor_sets1.clear();
    }
}

impl RendererData_HierachicalMinZ {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        render_target_hierachical_min_z: &TextureData,
    ) {
        let resources = resources.borrow();
        let generate_min_z_material_instance = resources.get_material_instance_data("generate_min_z").borrow();
        let pipeline_binding_data = generate_min_z_material_instance.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
        let descriptor_data = &pipeline_data._descriptor_data;
        let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
            descriptor_data_create_info._descriptor_binding_index
        }).collect();

        let layer: u32 = 0;
        let mut descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
        let dispatch_count: u32 = render_target_hierachical_min_z._image_mip_levels - 1;
        for mip_level in 0..dispatch_count {
            for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
                for descriptor_resource_infos in descriptor_resource_infos_list.get_mut(*swapchain_index).iter_mut() {
                    descriptor_resource_infos[0] = DescriptorResourceInfo::DescriptorImageInfo(render_target_hierachical_min_z.get_sub_image_info(layer, mip_level));
                    descriptor_resource_infos[1] = DescriptorResourceInfo::DescriptorImageInfo(render_target_hierachical_min_z.get_sub_image_info(layer, mip_level + 1));
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
            self._descriptor_sets.push(descriptor_sets);
        }
        self._dispatch_group_x = render_target_hierachical_min_z._image_width;
        self._dispatch_group_y = render_target_hierachical_min_z._image_height;
    }

    pub fn destroy(&mut self, device: &Device) {
        for descriptor_sets in self._descriptor_sets.iter_mut() {
            descriptor_sets.clear();
        }
        self._descriptor_sets.clear();
    }
}

impl RendererData_SceneColorDownSampling {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        texture_scene_color: &TextureData,
    ) {
        let resources = resources.borrow();
        let downsampling_material_instance = resources.get_material_instance_data("downsampling").borrow();
        let pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let pipeline_data = pipeline_binding_data._render_pass_pipeline_data._pipeline_data.borrow();
        let descriptor_data = &pipeline_data._descriptor_data;
        let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
            descriptor_data_create_info._descriptor_binding_index
        }).collect();

        let layer: u32 = 0;
        let mut descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
        let dispatch_count: u32 = texture_scene_color._image_mip_levels - 1;
        for mip_level in 0..dispatch_count {
            for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
                for descriptor_resource_infos in descriptor_resource_infos_list.get_mut(*swapchain_index).iter_mut() {
                    descriptor_resource_infos[0] = DescriptorResourceInfo::DescriptorImageInfo(texture_scene_color.get_sub_image_info(layer, mip_level));
                    descriptor_resource_infos[1] = DescriptorResourceInfo::DescriptorImageInfo(texture_scene_color.get_sub_image_info(layer, mip_level + 1));
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
            self._descriptor_sets.push(descriptor_sets);
        }
        self._dispatch_group_x = texture_scene_color._image_width;
        self._dispatch_group_y = texture_scene_color._image_height;
    }

    pub fn destroy(&mut self, device: &Device) {
        self._descriptor_sets.clear();
    }
}

impl RendererData_SSR {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        texture_ssr_resolved: &TextureData,
        texture_ssr_resolved_prev: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_copy_material_instance = resources.get_material_instance_data("render_ssr_resolve").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_default_pipeline_binding_data();
        let descriptor_binding_index: usize = 1;
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (framebuffer_data0, descriptor_sets0) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            texture_ssr_resolved, layer, mip_level,
            descriptor_binding_index, texture_ssr_resolved_prev, layer, mip_level, None,
        );
        let (framebuffer_data1, descriptor_sets1) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            texture_ssr_resolved_prev, layer, mip_level,
            descriptor_binding_index, texture_ssr_resolved, layer, mip_level, None,
        );
        self._framebuffer_data0 = framebuffer_data0;
        self._framebuffer_data1 = framebuffer_data1;
        self._descriptor_sets0 = descriptor_sets0;
        self._descriptor_sets1 = descriptor_sets1;
    }

    pub fn update(&mut self) {
        let temp = self._current_ssr_resolved;
        self._current_ssr_resolved = self._previous_ssr_resolved;
        self._previous_ssr_resolved = temp;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._framebuffer_data0);
        framebuffer::destroy_framebuffer_data(device, &self._framebuffer_data1);
    }
}


impl RendererData_CompositeGBuffer {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        texture_ssr_resolved: &TextureData,
        texture_ssr_resolved_prev: &TextureData,
    ) {
        let resources = resources.borrow();
        let render_copy_material_instance = resources.get_material_instance_data("composite_gbuffer").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_default_pipeline_binding_data();
        let descriptor_binding_index: usize = 11;
        let layer: u32 = constants::WHOLE_LAYERS;
        let mip_level: u32 = constants::WHOLE_MIP_LEVELS;
        let descriptor_sets0 = utility::create_descriptor_sets(
            device, pipeline_binding_data,
            descriptor_binding_index, texture_ssr_resolved, layer, mip_level,
        );
        let descriptor_sets1 = utility::create_descriptor_sets(
            device, pipeline_binding_data,
            descriptor_binding_index, texture_ssr_resolved_prev, layer, mip_level,
        );
        self._descriptor_sets0 = descriptor_sets0;
        self._descriptor_sets1 = descriptor_sets1;
    }

    pub fn destroy(&mut self, device: &Device) {
        self._descriptor_sets0.clear();
        self._descriptor_sets1.clear();
    }
}

impl RendererData_ClearRenderTargets {
    pub fn initialize(
        &mut self,
        device: &Device,
        resources: &RcRefCell<Resources>,
        render_targets: &[&TextureData],
    ) {
        let resources = resources.borrow();
        let material_instance = resources.get_material_instance_data("render_color").borrow();
        for render_target in render_targets.iter() {
            let pipeline_binding_data = material_instance.get_pipeline_binding_data(&format!("{:?}/{:?}", render_target._image_format, render_target._image_format));
            for layer in 0..render_target._image_layers {
                for mip_level in 0..render_target._image_mip_levels {
                    self._framebuffer_datas.push(utility::create_framebuffer(device, pipeline_binding_data, render_target, layer, mip_level, None))
                }
            }
        }
    }

    pub fn destroy(&mut self, device: &Device) {
        for framebuffer_data in self._framebuffer_datas.iter() {
            framebuffer::destroy_framebuffer_data(device, framebuffer_data);
        }
        self._framebuffer_datas.clear();
    }
}
