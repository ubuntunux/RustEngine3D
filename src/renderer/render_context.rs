use std::collections::HashMap;

use rand;
use nalgebra::{ Vector3, Vector4 };
use ash::{ vk, Device };
use crate::constants;
use crate::renderer::push_constants::PushConstant_BloomHighlight;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::shader_buffer_datas::SSAOConstants;
use crate::renderer::utility;
use crate::resource::resource::EngineResources;
use crate::vulkan_context::buffer::ShaderBufferData;
use crate::vulkan_context::descriptor::DescriptorResourceInfo;
use crate::vulkan_context::framebuffer::{ self, FramebufferData, RenderTargetInfo };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::{ self, CubeMapArray, SwapchainArray, Layers, MipLevels };

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_LightProbe {
    pub _next_refresh_time: f64,
    pub _light_probe_refresh_term: f64,
    pub _light_probe_blend_time: f64,
    pub _light_probe_blend_term: f64,
    pub _light_probe_capture_count: u64,
    pub _render_atmosphere_framebuffer_datas: CubeMapArray<FramebufferData>,
    pub _render_atmosphere_descriptor_sets: CubeMapArray<SwapchainArray<vk::DescriptorSet>>,
    pub _composite_atmosphere_framebuffer_datas_only_sky: CubeMapArray<FramebufferData>,
    pub _composite_atmosphere_framebuffer_datas: CubeMapArray<FramebufferData>,
    pub _composite_atmosphere_descriptor_sets: CubeMapArray<SwapchainArray<vk::DescriptorSet>>,
    pub _only_sky_downsampling_descriptor_sets: CubeMapArray<MipLevels<SwapchainArray<vk::DescriptorSet>>>,
    pub _light_probe_downsampling_descriptor_sets: CubeMapArray<MipLevels<SwapchainArray<vk::DescriptorSet>>>,
    pub _light_probe_blend_from_only_sky_descriptor_sets: MipLevels<SwapchainArray<vk::DescriptorSet>>,
    pub _light_probe_blend_from_forward_descriptor_sets: MipLevels<SwapchainArray<vk::DescriptorSet>>,
    pub _only_sky_copy_descriptor_sets: MipLevels<SwapchainArray<vk::DescriptorSet>>,
    pub _light_probe_forward_copy_descriptor_sets: MipLevels<SwapchainArray<vk::DescriptorSet>>,
}

impl Default for RenderContext_LightProbe {
    fn default() -> RenderContext_LightProbe {
        RenderContext_LightProbe {
            _next_refresh_time: 0.0,
            _light_probe_refresh_term: 5.0,
            _light_probe_blend_time: 0.0,
            _light_probe_blend_term: 1.0,
            _light_probe_capture_count: 0,
            _render_atmosphere_framebuffer_datas: Vec::new(),
            _render_atmosphere_descriptor_sets: Vec::new(),
            _composite_atmosphere_framebuffer_datas_only_sky: Vec::new(),
            _composite_atmosphere_framebuffer_datas: Vec::new(),
            _composite_atmosphere_descriptor_sets: Vec::new(),
            _only_sky_downsampling_descriptor_sets: Vec::new(),
            _light_probe_downsampling_descriptor_sets: Vec::new(),
            _light_probe_blend_from_only_sky_descriptor_sets: Vec::new(),
            _light_probe_blend_from_forward_descriptor_sets: Vec::new(),
            _only_sky_copy_descriptor_sets: Vec::new(),
            _light_probe_forward_copy_descriptor_sets: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_ClearRenderTargets {
    pub _color_framebuffer_datas: HashMap<String, Layers<MipLevels<FramebufferData>>>,
}

impl Default for RenderContext_ClearRenderTargets {
    fn default() -> RenderContext_ClearRenderTargets {
        RenderContext_ClearRenderTargets {
            _color_framebuffer_datas: HashMap::new()
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_CompositeGBuffer {
    pub _descriptor_sets0: SwapchainArray<vk::DescriptorSet>,
    pub _descriptor_sets1: SwapchainArray<vk::DescriptorSet>,
}

impl Default for RenderContext_CompositeGBuffer {
    fn default() -> RenderContext_CompositeGBuffer {
        RenderContext_CompositeGBuffer {
            _descriptor_sets0: Vec::new(),
            _descriptor_sets1: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_SceneColorDownSampling {
    pub _dispatch_group_x: u32,
    pub _dispatch_group_y: u32,
    pub _descriptor_sets: Vec<SwapchainArray<vk::DescriptorSet>>,
}

impl Default for RenderContext_SceneColorDownSampling {
    fn default() -> RenderContext_SceneColorDownSampling {
        RenderContext_SceneColorDownSampling {
            _dispatch_group_x: 1,
            _dispatch_group_y: 1,
            _descriptor_sets: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_Bloom {
    pub _bloom_downsample_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_downsample_descriptor_sets: Vec<SwapchainArray<vk::DescriptorSet>>,
    pub _bloom_temp_framebuffer_datas: Vec<FramebufferData>,
    pub _bloom_temp_descriptor_sets: Vec<SwapchainArray<vk::DescriptorSet>>,
    pub _bloom_blur_framebuffer_datas: Vec<*const FramebufferData>,
    pub _bloom_blur_descriptor_sets: Vec<*const SwapchainArray<vk::DescriptorSet>>,
    pub _bloom_push_constants: PushConstant_BloomHighlight,
}

impl Default for RenderContext_Bloom {
    fn default() -> RenderContext_Bloom {
        RenderContext_Bloom {
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
pub struct RenderContext_SSAO {
    pub _ssao_kernel_size: i32,
    pub _ssao_radius: f32,
    pub _ssao_noise_dim: i32,
    pub _ssao_constants: SSAOConstants,
    pub _ssao_blur_framebuffer_data0: FramebufferData,
    pub _ssao_blur_framebuffer_data1: FramebufferData,
    pub _ssao_blur_descriptor_sets0: SwapchainArray<vk::DescriptorSet>,
    pub _ssao_blur_descriptor_sets1: SwapchainArray<vk::DescriptorSet>,
}

impl Default for RenderContext_SSAO {
    fn default() -> RenderContext_SSAO {
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

        RenderContext_SSAO {
            _ssao_kernel_size: constants::SSAO_KERNEL_SIZE as i32,
            _ssao_radius: constants::SSAO_RADIUS,
            _ssao_noise_dim: unsafe { constants::SSAO_NOISE_DIM },
            _ssao_constants: SSAOConstants {
                _ssao_kernel_samples: random_normals
            },
            _ssao_blur_framebuffer_data0: FramebufferData::default(),
            _ssao_blur_framebuffer_data1: FramebufferData::default(),
            _ssao_blur_descriptor_sets0: SwapchainArray::new(),
            _ssao_blur_descriptor_sets1: SwapchainArray::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_TAA {
    pub _enable_taa: bool,
    pub _rendertarget_width: u32,
    pub _rendertarget_height: u32,
    pub _taa_resolve_framebuffer_data: FramebufferData,
    pub _taa_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
}

impl Default for RenderContext_TAA {
    fn default() -> RenderContext_TAA {
        RenderContext_TAA {
            _enable_taa: true,
            _rendertarget_width: 1024,
            _rendertarget_height: 768,
            _taa_resolve_framebuffer_data: FramebufferData::default(),
            _taa_descriptor_sets: SwapchainArray::new()
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_HierachicalMinZ {
    pub _dispatch_group_x: u32,
    pub _dispatch_group_y: u32,
    pub _descriptor_sets: Vec<SwapchainArray<vk::DescriptorSet>>,
}

impl Default for RenderContext_HierachicalMinZ {
    fn default() -> RenderContext_HierachicalMinZ {
        RenderContext_HierachicalMinZ {
            _dispatch_group_x: 1,
            _dispatch_group_y: 1,
            _descriptor_sets: Vec::new(),
        }
    }
}

#[derive(Clone)]
#[allow(non_camel_case_types)]
pub struct RenderContext_TAA_Simple {
    pub _framebuffer_data0: FramebufferData,
    pub _framebuffer_data1: FramebufferData,
    pub _descriptor_sets0: SwapchainArray<vk::DescriptorSet>,
    pub _descriptor_sets1: SwapchainArray<vk::DescriptorSet>,
    pub _current_taa_resolved: RenderTargetType,
    pub _previous_taa_resolved: RenderTargetType,
}

impl Default for RenderContext_TAA_Simple {
    fn default() -> RenderContext_TAA_Simple {
        RenderContext_TAA_Simple {
            _framebuffer_data0: FramebufferData::default(),
            _framebuffer_data1: FramebufferData::default(),
            _descriptor_sets0: SwapchainArray::new(),
            _descriptor_sets1: SwapchainArray::new(),
            _current_taa_resolved: RenderTargetType::TAAResolve,
            _previous_taa_resolved: RenderTargetType::TAAResolve,
        }
    }
}

impl RenderContext_Bloom {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        render_target_bloom0: &TextureData,
        render_target_bloom_temp0: &TextureData,
    ) {
        let render_bloom_material_instance = engine_resources.get_material_instance_data("common/render_bloom").borrow();
        let pipeline_binding_data = render_bloom_material_instance.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let descriptor_binding_index: usize = 0;
        let layer = 0;
        for mip_level in 0..(render_target_bloom0._image_mip_levels - 1) {
            let (bloom_framebuffer_data, bloom_descriptor_set) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom0, layer, mip_level + 1, None,
                &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(render_target_bloom0.get_sub_image_info(layer, mip_level)))]
            );
            self._bloom_downsample_framebuffer_datas.push(bloom_framebuffer_data);
            self._bloom_downsample_descriptor_sets.push(bloom_descriptor_set);
        }

        let render_gaussian_blur_material_instance = engine_resources.get_material_instance_data("common/render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance.get_pipeline_binding_data("render_gaussian_blur/render_gaussian_blur");
        let descriptor_binding_index: usize = 0;
        for mip_level in 0..render_target_bloom0._image_mip_levels {
            let (gaussian_blur_h_framebuffer_data, gaussian_blur_h_descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom_temp0, layer, mip_level, None,
                &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(render_target_bloom0.get_sub_image_info(layer, mip_level)))]
            );
            let (gaussian_blur_v_framebuffer_data, gaussian_blur_v_descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
                device, pipeline_binding_data,
                render_target_bloom0, layer, mip_level, None,
                &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(render_target_bloom_temp0.get_sub_image_info(layer, mip_level)))]
            );
            self._bloom_temp_framebuffer_datas.push(gaussian_blur_h_framebuffer_data);
            self._bloom_temp_framebuffer_datas.push(gaussian_blur_v_framebuffer_data);
            self._bloom_temp_descriptor_sets.push(gaussian_blur_h_descriptor_sets);
            self._bloom_temp_descriptor_sets.push(gaussian_blur_v_descriptor_sets);
        }

        let render_bloom_framebuffer_data = engine_resources.get_framebuffer_data("render_bloom").as_ptr();
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

impl RenderContext_TAA {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        taa_render_target: &TextureData,
        taa_resolve_texture: &TextureData,
    ) {
        let render_copy_material_instance = engine_resources.get_material_instance_data("common/render_copy").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_pipeline_binding_data("render_copy/render_copy");
        let descriptor_binding_index: usize = 0;
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (framebuffer_data, descriptor_sets) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            taa_resolve_texture, layer, mip_level, None,
            &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(taa_render_target.get_sub_image_info(layer, mip_level)))]
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

impl RenderContext_SSAO {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        render_target_ssao: &TextureData,
        render_target_ssao_temp: &TextureData,
    ) {
        let render_gaussian_blur_material_instance = engine_resources.get_material_instance_data("common/render_ssao_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance.get_pipeline_binding_data("render_ssao_blur/render_ssao_blur");
        let descriptor_binding_index: usize = 0;
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (ssao_blur_framebuffer_data0, ssao_blur_descriptor_sets0) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            render_target_ssao_temp, layer, mip_level, None,
            &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(render_target_ssao.get_sub_image_info(layer, mip_level)))]
        );
        let (ssao_blur_framebuffer_data1, ssao_blur_descriptor_sets1) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            render_target_ssao, layer, mip_level, None,
            &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(render_target_ssao_temp.get_sub_image_info(layer, mip_level)))]
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

impl RenderContext_HierachicalMinZ {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        render_target_hierachical_min_z: &TextureData,
    ) {
        let generate_min_z_material_instance = engine_resources.get_material_instance_data("common/generate_min_z").borrow();
        let pipeline_binding_data = generate_min_z_material_instance.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let layer: u32 = 0;
        let downsampling_count: u32 = render_target_hierachical_min_z._image_mip_levels - 1;
        for mip_level in 0..downsampling_count {
            let descriptor_sets = utility::create_descriptor_sets(
                device,
                pipeline_binding_data,
                &[
                    (0, utility::create_descriptor_image_info_swapchain_array(render_target_hierachical_min_z.get_sub_image_info(layer, mip_level))),
                    (1, utility::create_descriptor_image_info_swapchain_array(render_target_hierachical_min_z.get_sub_image_info(layer, mip_level + 1))),
                ]
            );
            self._descriptor_sets.push(descriptor_sets);
        }
        self._dispatch_group_x = render_target_hierachical_min_z._image_width;
        self._dispatch_group_y = render_target_hierachical_min_z._image_height;
    }

    pub fn destroy(&mut self, _device: &Device) {
        for descriptor_sets in self._descriptor_sets.iter_mut() {
            descriptor_sets.clear();
        }
        self._descriptor_sets.clear();
    }
}

impl RenderContext_SceneColorDownSampling {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        texture_scene_color: &TextureData,
    ) {
        let downsampling_material_instance = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let layer: u32 = 0;
        let downsampling_count: u32 = texture_scene_color._image_mip_levels - 1;
        for mip_level in 0..downsampling_count {
            let descriptor_sets = utility::create_descriptor_sets(
                device,
                pipeline_binding_data,
                &[
                    (0, utility::create_descriptor_image_info_swapchain_array(texture_scene_color.get_sub_image_info(layer, mip_level))),
                    (1, utility::create_descriptor_image_info_swapchain_array(texture_scene_color.get_sub_image_info(layer, mip_level + 1))),
                ]
            );
            self._descriptor_sets.push(descriptor_sets);
        }
        self._dispatch_group_x = texture_scene_color._image_width;
        self._dispatch_group_y = texture_scene_color._image_height;
    }

    pub fn destroy(&mut self, _device: &Device) {
        self._descriptor_sets.clear();
    }
}

impl RenderContext_TAA_Simple {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        texture_ssr: &TextureData,
        texture_taa_resolved: &TextureData,
        texture_taa_resolved_prev: &TextureData,
        current_taa_resolved: RenderTargetType,
        previous_taa_resolved: RenderTargetType
    ) {
        let render_copy_material_instance = engine_resources.get_material_instance_data("common/render_taa_simple").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_default_pipeline_binding_data();
        let layer: u32 = 0;
        let mip_level: u32 = 0;
        let (framebuffer_data0, descriptor_sets0) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            texture_taa_resolved, layer, mip_level, None,
            &[
                (0, utility::create_descriptor_image_info_swapchain_array(texture_ssr.get_sub_image_info(layer, mip_level))),
                (1, utility::create_descriptor_image_info_swapchain_array(texture_taa_resolved_prev.get_sub_image_info(layer, mip_level)))
            ]
        );
        let (framebuffer_data1, descriptor_sets1) = utility::create_framebuffer_and_descriptor_sets(
            device, pipeline_binding_data,
            texture_taa_resolved_prev, layer, mip_level, None,
            &[
                (0, utility::create_descriptor_image_info_swapchain_array(texture_ssr.get_sub_image_info(layer, mip_level))),
                (1, utility::create_descriptor_image_info_swapchain_array(texture_taa_resolved.get_sub_image_info(layer, mip_level)))
            ]
        );
        self._framebuffer_data0 = framebuffer_data0;
        self._framebuffer_data1 = framebuffer_data1;
        self._descriptor_sets0 = descriptor_sets0;
        self._descriptor_sets1 = descriptor_sets1;
        self._current_taa_resolved = current_taa_resolved;
        self._previous_taa_resolved = previous_taa_resolved;
    }

    pub fn update(&mut self) {
        let temp = self._current_taa_resolved;
        self._current_taa_resolved = self._previous_taa_resolved;
        self._previous_taa_resolved = temp;
    }

    pub fn destroy(&mut self, device: &Device) {
        framebuffer::destroy_framebuffer_data(device, &self._framebuffer_data0);
        framebuffer::destroy_framebuffer_data(device, &self._framebuffer_data1);
    }
}


impl RenderContext_CompositeGBuffer {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        texture_taa_resolved: &TextureData,
        texture_taa_resolved_prev: &TextureData,
    ) {
        let render_copy_material_instance = engine_resources.get_material_instance_data("common/composite_gbuffer").borrow();
        let pipeline_binding_data = render_copy_material_instance.get_default_pipeline_binding_data();
        let descriptor_binding_index: usize = 12;
        self._descriptor_sets0 = utility::create_descriptor_sets(
            device,
            pipeline_binding_data,
            &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_taa_resolved.get_default_image_info()))],
        );
        self._descriptor_sets1 = utility::create_descriptor_sets(
            device, pipeline_binding_data,
            &[(descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_taa_resolved_prev.get_default_image_info()))]
        );
    }

    pub fn destroy(&mut self, _device: &Device) {
        self._descriptor_sets0.clear();
        self._descriptor_sets1.clear();
    }
}

impl RenderContext_ClearRenderTargets {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        render_target_infos: &[(&TextureData, vk::ClearValue)],
    ) {
        let material_instance = engine_resources.get_material_instance_data("common/clear_render_target").borrow();
        for (render_target, clear_value) in render_target_infos.iter() {
            let is_depth_format = constants::DEPTH_FOMATS.contains(&render_target._image_format);
            let render_pass_pipeline_name = format!("clear_{:?}/clear", render_target._image_format);
            let pipeline_binding_data = material_instance.get_pipeline_binding_data(&render_pass_pipeline_name);
            let mut framebuffers: Layers<MipLevels<FramebufferData>> = Vec::new();
            for layer in 0..render_target._image_layers {
                framebuffers.push(Vec::new());
                for mip_level in 0..render_target._image_mip_levels {
                    let mut color_attachments = Vec::new();
                    let mut depth_attachments = Vec::new();
                    let attachment = RenderTargetInfo {
                        _texture_data: &render_target,
                        _target_layer: layer as u32,
                        _target_mip_level: mip_level as u32,
                        _clear_value: Some(*clear_value),
                    };

                    if is_depth_format {
                        depth_attachments.push(attachment);
                    } else {
                        color_attachments.push(attachment);
                    }

                    framebuffers.last_mut().unwrap().push(
                        utility::create_framebuffers(
                            device,
                            &pipeline_binding_data.get_render_pass_data().borrow(),
                            &render_target._texture_data_name,
                            &color_attachments,
                            &depth_attachments,
                            &[],
                        )
                    );
                }
            }
            self._color_framebuffer_datas.insert(render_target._texture_data_name.clone(), framebuffers);
        }
    }

    pub fn destroy(&mut self, device: &Device) {
        let func_clear_framebuffers = |framebuffer_map: &mut HashMap<String, Layers<MipLevels<FramebufferData>>>| {
            for (_, framebuffer_datas) in framebuffer_map.iter_mut() {
                for layer in 0..framebuffer_datas.len() {
                    for mip_level in 0..framebuffer_datas[layer].len() {
                        framebuffer::destroy_framebuffer_data(device, &framebuffer_datas[layer][mip_level]);
                    }
                    framebuffer_datas[layer].clear();
                }
                framebuffer_datas.clear();
            }
            framebuffer_map.clear();
        };
        func_clear_framebuffers(&mut self._color_framebuffer_datas);
    }
}

impl RenderContext_LightProbe {
    pub fn initialize(
        &mut self,
        device: &Device,
        engine_resources: &EngineResources,
        light_probe_color: &TextureData,
        light_probe_color_only_sky: &TextureData,
        light_probe_color_only_sky_prev: &TextureData,
        light_probe_color_forward: &TextureData,
        light_probe_color_forward_prev: &TextureData,
        light_probe_atmosphere_color: &TextureData,
        light_probe_atmosphere_inscatter: &TextureData,
        light_probe_view_constants: &[&ShaderBufferData],
    ) {
        let material_instance = engine_resources.get_material_instance_data("precomputed_atmosphere/precomputed_atmosphere").borrow();
        let texture_white_image_info = DescriptorResourceInfo::DescriptorImageInfo(engine_resources.get_texture_data("common/flat_white").borrow().get_default_image_info().clone());
        let render_atmosphere_pipeline_binding_data = material_instance.get_pipeline_binding_data("render_atmosphere/default");
        let composite_atmosphere_pipeline_binding_data = material_instance.get_pipeline_binding_data("composite_atmosphere/default");
        let downsampling_material_instance = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let downsampling_pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let copy_cube_map_material_instance = engine_resources.get_material_instance_data("common/copy_cube_map").borrow();
        let copy_cube_map_pipeline_binding_data = copy_cube_map_material_instance.get_pipeline_binding_data("copy_cube_map/copy");
        let blend_cube_map_pipeline_binding_data = copy_cube_map_material_instance.get_pipeline_binding_data("copy_cube_map/blend");

        self._next_refresh_time = 0.0;
        self._light_probe_blend_time = 0.0;
        self._light_probe_capture_count = 0;

        for i in 0..constants::CUBE_LAYER_COUNT {
            // render_atmosphere
            self._render_atmosphere_framebuffer_datas.push(utility::create_framebuffers(
                device,
                &render_atmosphere_pipeline_binding_data.get_render_pass_data().borrow(),
                "render_targets_light_probe",
                &[
                    RenderTargetInfo { _texture_data: &light_probe_atmosphere_color, _target_layer: i as u32, _target_mip_level: 0, _clear_value: Some(vulkan_context::get_color_clear_value(0.0, 0.0, 0.0, 0.0)) },
                    RenderTargetInfo { _texture_data: &light_probe_atmosphere_inscatter, _target_layer: i as u32, _target_mip_level: 0, _clear_value: Some(vulkan_context::get_color_clear_value(0.0, 0.0, 0.0, 0.0)) },
                ],
                &[],
                &[],
            ));
            self._render_atmosphere_descriptor_sets.push(utility::create_descriptor_sets(
                device,
                render_atmosphere_pipeline_binding_data,
                &[
                    (1, light_probe_view_constants[i]._descriptor_buffer_infos.clone()),
                    (5, utility::create_swapchain_array(texture_white_image_info.clone())),
                ]
            ));

            // composite atmosphere
            self._composite_atmosphere_framebuffer_datas_only_sky.push(utility::create_framebuffers(
                device,
                &composite_atmosphere_pipeline_binding_data.get_render_pass_data().borrow(),
                "composite_atmosphere_light_probe",
                &[
                    RenderTargetInfo { _texture_data: &light_probe_color_only_sky, _target_layer: i as u32, _target_mip_level: 0, _clear_value: Some(vulkan_context::get_color_clear_value(0.0, 0.0, 0.0, 0.0)) },
                ],
                &[],
                &[],
            ));
            self._composite_atmosphere_framebuffer_datas.push(utility::create_framebuffers(
                device,
                &composite_atmosphere_pipeline_binding_data.get_render_pass_data().borrow(),
                "composite_atmosphere_light_probe",
                &[
                    RenderTargetInfo { _texture_data: &light_probe_color_forward, _target_layer: i as u32, _target_mip_level: 0, _clear_value: Some(vulkan_context::get_color_clear_value(0.0, 0.0, 0.0, 0.0)) },
                ],
                &[],
                &[],
            ));
            let light_probe_atmosphere_color_image_info = DescriptorResourceInfo::DescriptorImageInfo(light_probe_atmosphere_color.get_sub_image_info(i as u32, 0).clone());
            let light_probe_atmosphere_inscatter_image_info = DescriptorResourceInfo::DescriptorImageInfo(light_probe_atmosphere_inscatter.get_sub_image_info(i as u32, 0).clone());
            self._composite_atmosphere_descriptor_sets.push(utility::create_descriptor_sets(
                device,
                composite_atmosphere_pipeline_binding_data,
                &[
                    (1, light_probe_view_constants[i]._descriptor_buffer_infos.clone()),
                    (5, utility::create_swapchain_array(texture_white_image_info.clone())),
                    (15, utility::create_swapchain_array(light_probe_atmosphere_color_image_info.clone())),
                    (16, utility::create_swapchain_array(light_probe_atmosphere_inscatter_image_info.clone())),
                ]
            ));

            // downsampling only sky texture
            self._only_sky_downsampling_descriptor_sets.push(Vec::new());
            let downsampling_count: u32 = light_probe_color_only_sky._image_mip_levels - 1;
            for mip_level in 0..downsampling_count {
                let descriptor_sets = utility::create_descriptor_sets(
                    device,
                    downsampling_pipeline_binding_data,
                    &[
                        (0, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky.get_sub_image_info(i as u32, mip_level))),
                        (1, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky.get_sub_image_info(i as u32, mip_level + 1))),
                    ]
                );
                self._only_sky_downsampling_descriptor_sets.last_mut().unwrap().push(descriptor_sets);
            }

            // downsampling light probe forward
            self._light_probe_downsampling_descriptor_sets.push(Vec::new());
            let downsampling_count: u32 = light_probe_color_forward._image_mip_levels - 1;
            for mip_level in 0..downsampling_count {
                let descriptor_sets = utility::create_descriptor_sets(
                    device,
                    downsampling_pipeline_binding_data,
                    &[
                        (0, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward.get_sub_image_info(i as u32, mip_level))),
                        (1, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward.get_sub_image_info(i as u32, mip_level + 1))),
                    ]
                );
                self._light_probe_downsampling_descriptor_sets.last_mut().unwrap().push(descriptor_sets);
            }

            // copy cube map, blend cube map
            let image_mip_levels: u32 = light_probe_color_forward._image_mip_levels;
            for mip_level in 0..image_mip_levels {
                let mut light_probe_blend_from_only_sky_descriptor_resource_infos: SwapchainArray<(usize, SwapchainArray<DescriptorResourceInfo>)> = SwapchainArray::new();
                let mut light_probe_blend_from_forward_descriptor_resource_infos: SwapchainArray<(usize, SwapchainArray<DescriptorResourceInfo>)> = SwapchainArray::new();
                let mut only_sky_copy_descriptor_resource_infos: SwapchainArray<(usize, SwapchainArray<DescriptorResourceInfo>)> = SwapchainArray::new();
                let mut light_probe_forward_copy_descriptor_resource_infos: SwapchainArray<(usize, SwapchainArray<DescriptorResourceInfo>)> = SwapchainArray::new();

                // copy cube map
                for binding_index in 0..12usize {
                    let layer = (binding_index % 6) as u32;
                    if binding_index < 6 {
                        only_sky_copy_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky.get_sub_image_info(layer, mip_level))));
                        light_probe_forward_copy_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward.get_sub_image_info(layer, mip_level))));

                    } else {
                        only_sky_copy_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky_prev.get_sub_image_info(layer, mip_level))));
                        light_probe_forward_copy_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward_prev.get_sub_image_info(layer, mip_level))));
                    }
                }
                self._only_sky_copy_descriptor_sets.push(
                    utility::create_descriptor_sets(device, copy_cube_map_pipeline_binding_data, &only_sky_copy_descriptor_resource_infos)
                );
                self._light_probe_forward_copy_descriptor_sets.push(
                    utility::create_descriptor_sets(device, copy_cube_map_pipeline_binding_data, &light_probe_forward_copy_descriptor_resource_infos)
                );

                // blend cube map
                for binding_index in 0..18usize {
                    let layer = (binding_index % 6) as u32;
                    if binding_index < 6 {
                        light_probe_blend_from_only_sky_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky_prev.get_sub_image_info(layer, mip_level))));
                        light_probe_blend_from_forward_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward_prev.get_sub_image_info(layer, mip_level))));
                    } else if binding_index < 12 {
                        light_probe_blend_from_only_sky_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_only_sky.get_sub_image_info(layer, mip_level))));
                        light_probe_blend_from_forward_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color_forward.get_sub_image_info(layer, mip_level))));
                    } else {
                        light_probe_blend_from_only_sky_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color.get_sub_image_info(layer, mip_level))));
                        light_probe_blend_from_forward_descriptor_resource_infos.push((binding_index, utility::create_descriptor_image_info_swapchain_array(light_probe_color.get_sub_image_info(layer, mip_level))));
                    }
                }
                self._light_probe_blend_from_only_sky_descriptor_sets.push(
                    utility::create_descriptor_sets(device, blend_cube_map_pipeline_binding_data, &light_probe_blend_from_only_sky_descriptor_resource_infos)
                );
                self._light_probe_blend_from_forward_descriptor_sets.push(
                    utility::create_descriptor_sets(device, blend_cube_map_pipeline_binding_data, &light_probe_blend_from_forward_descriptor_resource_infos)
                );
            }
        }
    }

    pub fn destroy(&mut self, device: &Device) {
        for i in 0..constants::CUBE_LAYER_COUNT {
            framebuffer::destroy_framebuffer_data(device, &self._render_atmosphere_framebuffer_datas[i]);
            framebuffer::destroy_framebuffer_data(device, &self._composite_atmosphere_framebuffer_datas_only_sky[i]);
            framebuffer::destroy_framebuffer_data(device, &self._composite_atmosphere_framebuffer_datas[i]);
        }
        self._render_atmosphere_framebuffer_datas.clear();
        self._composite_atmosphere_framebuffer_datas_only_sky.clear();
        self._composite_atmosphere_framebuffer_datas.clear();
        self._render_atmosphere_descriptor_sets.clear();
        self._composite_atmosphere_descriptor_sets.clear();
        self._only_sky_downsampling_descriptor_sets.clear();
        self._light_probe_downsampling_descriptor_sets.clear();
        self._light_probe_blend_from_only_sky_descriptor_sets.clear();
        self._light_probe_blend_from_forward_descriptor_sets.clear();
        self._only_sky_copy_descriptor_sets.clear();
        self._light_probe_forward_copy_descriptor_sets.clear();
    }
}