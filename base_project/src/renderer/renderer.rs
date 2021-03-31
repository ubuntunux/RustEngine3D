use std::str::FromStr;
use std::collections::HashMap;
use std::cell::{ Ref, RefMut };
use std::vec::Vec;

use ash::{ vk, Device };
use nalgebra::{ Vector2, Matrix4 };
use rust_engine_3d::constants;
use rust_engine_3d::application::scene_manager::SceneManagerData;
use rust_engine_3d::renderer::camera::CameraObjectData;
use rust_engine_3d::renderer::effect::EffectManagerBase;
use rust_engine_3d::renderer::font::{ FontManager, RenderTextInfo };
use rust_engine_3d::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use rust_engine_3d::renderer::render_element::RenderElementData;
use rust_engine_3d::renderer::renderer::{ RendererBase, RendererData };
use rust_engine_3d::renderer::ui::UIManagerData;
use rust_engine_3d::resource::resource::Resources;
use rust_engine_3d::vulkan_context::buffer::{ self, ShaderBufferData };
use rust_engine_3d::vulkan_context::descriptor::{ DescriptorResourceInfo };
use rust_engine_3d::vulkan_context::geometry_buffer::{ GeometryData };
use rust_engine_3d::vulkan_context::render_pass::{ RenderPassDataCreateInfo, PipelineData };
use rust_engine_3d::vulkan_context::texture::{ self, TextureData };
use rust_engine_3d::vulkan_context::vulkan_context::{ self, SwapchainArray, MipLevels };

use crate::application_constants;
use crate::application::scene_manager::SceneManager;
use crate::renderer::effect::EffectManager;
use crate::renderer::fft_ocean::FFTOcean;
use crate::renderer::precomputed_atmosphere::PushConstant_Atmosphere;
use crate::renderer::push_constants::{
    NONE_PUSH_CONSTANT,
    PushConstant_StaticRenderObject,
    PushConstant_SkeletalRenderObject,
    PushConstant_GaussianBlur,
    PushConstant_RenderCopy,
    PushConstant_RenderDebug,
    PushConstant_BlendCubeMap,
};
use crate::renderer::render_target::{ self, RenderTargetType };
use crate::renderer::renderer_data::{
    RendererData_Bloom,
    RendererData_SSAO,
    RendererData_TAA,
    RendererData_HierachicalMinZ,
    RendererData_SceneColorDownSampling,
    RendererData_SSR,
    RendererData_CompositeGBuffer,
    RendererData_ClearRenderTargets,
    RendererData_LightProbe,
};
use crate::renderer::shader_buffer_datas::{
    self,
    ShaderBufferDataType,
    ShaderBufferDataMap,
};
use crate::render_pass_create_info::render_pass_create_info;

pub type RenderTargetDataMap = HashMap<RenderTargetType, TextureData>;

pub const DEFAULT_PIPELINE: &str = "";

// NOTE : RenderMode must match with scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RenderMode {
    GBuffer = 0,
    Forward = 1,
    Shadow = 2,
    CaptureHeightMap = 3,
}

// NOTE : RenderObjectType must match with scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum RenderObjectType {
    Static = 0,
    Skeletal = 1,
}

pub struct Renderer {
    pub _renderer_data: *const RendererData,
    pub _resources: *const Resources,
    pub _effect_manager: *const EffectManager,
    pub _is_first_rendering: bool,
    pub _scene_constants: shader_buffer_datas::SceneConstants,
    pub _view_constants: shader_buffer_datas::ViewConstants,
    pub _debug_render_target: RenderTargetType,
    pub _debug_render_target_layer: u32,
    pub _debug_render_target_miplevel: u32,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _shader_buffer_data_map: ShaderBufferDataMap,
    pub _renderer_data_bloom: RendererData_Bloom,
    pub _renderer_data_ssao: RendererData_SSAO,
    pub _renderer_data_taa: RendererData_TAA,
    pub _renderer_data_hiz: RendererData_HierachicalMinZ,
    pub _scene_color_downsampling: RendererData_SceneColorDownSampling,
    pub _renderer_data_ssr: RendererData_SSR,
    pub _renderer_data_composite_gbuffer: RendererData_CompositeGBuffer,
    pub _clear_render_targets: RendererData_ClearRenderTargets,
    pub _light_probe_datas: RendererData_LightProbe,
}

impl RendererBase for Renderer {
    fn initialize_renderer(&mut self, renderer_data: &RendererData, effect_manager: *const dyn EffectManagerBase) {
        self._renderer_data = renderer_data;
        self._resources = renderer_data._resources.as_ptr();
        self._effect_manager = effect_manager as *const EffectManager;
        shader_buffer_datas::regist_shader_buffer_datas(renderer_data.get_device(), renderer_data.get_device_memory_properties(), &mut self._shader_buffer_data_map);
        self.create_render_targets(renderer_data);
    }
    fn is_first_rendering(&self) -> bool {
        self._is_first_rendering
    }
    fn set_is_first_rendering(&mut self, is_first_rendering: bool) {
        log::info!("set_is_first_rendering: {}", is_first_rendering);
        self._is_first_rendering = is_first_rendering;
    }
    fn prepare_framebuffer_and_descriptors(&mut self, device: &Device, resources: &Resources) {
        log::info!("RendererData::prepare_framebuffer_and_descriptors");

        // Bloom
        self._renderer_data_bloom.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp0).as_ref().unwrap(),
        );
        // Temporal AA
        self._renderer_data_taa.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(),
        );
        // SSAO
        self._renderer_data_ssao.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::SSAO).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSAOTemp).as_ref().unwrap(),
        );
        // Hierachical Min Z
        self._renderer_data_hiz.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(),
        );
        // SceneColor Downsampling
        self._scene_color_downsampling.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(),
        );
        // SSR
        self._renderer_data_ssr.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
        );
        // Composite GBuffer
        self._renderer_data_composite_gbuffer.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
        );
        // Clear Render Targets
        self._clear_render_targets.initialize(
            device,
            resources,
            &[
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySky).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySkyPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForward).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForwardPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_A).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_B).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::Shadow).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
            ]
        );
        // RendererData_LightProbe
        self._light_probe_datas.initialize(
            device,
            resources,
            self._render_target_data_map.get(&RenderTargetType::LightProbeColor).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySky).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySkyPrev).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorForward).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeColorForwardPrev).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeAtmosphereColor).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::LightProbeAtmosphereInscatter).as_ref().unwrap(),
            &[
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants0).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants1).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants2).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants3).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants4).as_ref().unwrap(),
                &self._shader_buffer_data_map.get(&ShaderBufferDataType::LightProbeViewConstants5).as_ref().unwrap(),
            ]
        );
    }

    fn destroy_framebuffer_and_descriptors(&mut self, device: &Device) {
        self._renderer_data_bloom.destroy(device);
        self._renderer_data_taa.destroy(device);
        self._renderer_data_ssao.destroy(device);
        self._renderer_data_hiz.destroy(device);
        self._scene_color_downsampling.destroy(device);
        self._renderer_data_ssr.destroy(device);
        self._renderer_data_composite_gbuffer.destroy(device);
        self._clear_render_targets.destroy(device);
        self._light_probe_datas.destroy(device);
    }
    fn update_post_process_datas(&mut self) {
        self._renderer_data_ssr.update();
    }
    fn get_shader_buffer_data_from_str(&self, buffer_data_name: &str) -> &ShaderBufferData {
        self.get_shader_buffer_data(&ShaderBufferDataType::from_str(buffer_data_name).unwrap())
    }
    fn get_render_target_from_str(&self, render_target_type_str: &str) -> &TextureData {
        self.get_render_target(RenderTargetType::from_str(render_target_type_str).unwrap())
    }
    fn get_render_pass_data_create_infos(&self) -> Vec<RenderPassDataCreateInfo> {
        render_pass_create_info::get_render_pass_data_create_infos(self)
    }
    fn create_render_targets(&mut self, renderer_data: &RendererData) {
        log::info!("create_render_targets");
        let render_taget_create_infos = render_target::get_render_target_create_infos(renderer_data);
        for render_taget_create_info in render_taget_create_infos.iter() {
            let render_target_type: RenderTargetType = RenderTargetType::from_str(render_taget_create_info._texture_name.as_str()).unwrap();
            let texture_data = renderer_data.create_render_target(render_taget_create_info);
            self._render_target_data_map.insert(render_target_type, texture_data);
        }
    }
    fn destroy_render_targets(&mut self, device: &Device) {
        for render_target_data in self._render_target_data_map.values() {
            texture::destroy_texture_data(device, render_target_data);
        }
        self._render_target_data_map.clear();
    }

    fn destroy_uniform_buffers(&mut self, device: &Device) {
        for shader_buffer_data in self._shader_buffer_data_map.values_mut() {
            buffer::destroy_shader_buffer_data(device, shader_buffer_data);
        }
        self._shader_buffer_data_map.clear();
    }

    fn render_scene(
        &mut self,
        command_buffer: vk::CommandBuffer,
        _frame_index: usize,
        swapchain_index: u32,
        renderer_data: &RendererData,
        scene_manager_data: &SceneManagerData,
        font_manager: &mut FontManager,
        ui_manager_data: &mut UIManagerData,
        elapsed_time: f64,
        delta_time: f64,
        _elapsed_frame: u64,
    ) {
        let resources = renderer_data._resources.borrow();
        let scene_manager: &SceneManager = unsafe { &mut *(scene_manager_data._scene_manager as *mut SceneManager) };
        let main_camera =  scene_manager.get_main_camera().borrow();
        let main_light = scene_manager.get_main_light().borrow();
        let mut capture_height_map = scene_manager.get_capture_height_map().borrow_mut();
        let render_capture_height_map: bool = capture_height_map.get_need_to_redraw_shadow_and_reset();
        let fft_ocean =  scene_manager.get_fft_ocean().borrow();
        let mut atmosphere =  scene_manager.get_atmosphere().borrow_mut();
        let quad_mesh = resources.get_mesh_data("quad").borrow();
        let quad_geometry_data: Ref<GeometryData> = quad_mesh.get_default_geometry_data().borrow();
        let static_render_elements = scene_manager.get_static_render_elements();
        let static_shadow_render_elements = scene_manager.get_static_shadow_render_elements();
        let skeletal_render_elements = scene_manager.get_skeletal_render_elements();
        let skeletal_shadow_render_elements = scene_manager.get_skeletal_shadow_render_elements();

        // Upload Uniform Buffers
        self._scene_constants.update_scene_constants(
            renderer_data._swapchain_data._swapchain_extent.width,
            renderer_data._swapchain_data._swapchain_extent.height,
            elapsed_time,
            delta_time,
            fft_ocean.get_height(),
            self.get_effect_manager().get_gpu_particle_count_buffer_offset(),
            self.get_effect_manager().get_gpu_particle_update_buffer_offset(),
        );
        self._view_constants.update_view_constants(&main_camera);
        if render_capture_height_map {
            self._view_constants._capture_height_map_view_projection = (*capture_height_map.get_shadow_view_projection()).into();
        }

        self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::SceneConstants, &self._scene_constants);
        self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::ViewConstants, &self._view_constants);
        self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::LightConstants, main_light.get_light_constants());
        self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::SSAOConstants, &self._renderer_data_ssao._ssao_constants);
        self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::AtmosphereConstants, &atmosphere._atmosphere_constants);

        if self._is_first_rendering {
            self.rendering_at_first(command_buffer, swapchain_index, renderer_data, &resources, &quad_geometry_data);
            fft_ocean.compute_slope_variance_texture(command_buffer, swapchain_index, &quad_geometry_data, renderer_data, &resources);
            atmosphere.precompute(command_buffer, swapchain_index, &quad_geometry_data, renderer_data);
        }

        // clear gbuffer
        renderer_data.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_gbuffer/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // shadow
        renderer_data.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_shadow/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
        self.render_solid_object(renderer_data, command_buffer, swapchain_index, RenderMode::Shadow, RenderObjectType::Static, &static_shadow_render_elements, None);
        self.render_solid_object(renderer_data, command_buffer, swapchain_index, RenderMode::Shadow, RenderObjectType::Skeletal, &skeletal_shadow_render_elements, None);

        // capture height map
        if render_capture_height_map || self._is_first_rendering {
            renderer_data.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", "clear_capture_height_map/clear", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
            self.render_solid_object(renderer_data, command_buffer, swapchain_index, RenderMode::CaptureHeightMap, RenderObjectType::Static, &static_render_elements, None);
        }

        // fft-simulation
        fft_ocean.simulate_fft_waves(command_buffer, swapchain_index, &quad_geometry_data, renderer_data, &resources);

        // light probe
        if self._light_probe_datas._next_refresh_time <= elapsed_time || self._light_probe_datas._light_probe_capture_count < 2 {
            self.render_light_probe(
                renderer_data,
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                &resources,
                scene_manager,
                &main_camera,
                static_render_elements,
                &fft_ocean
            );
            self._light_probe_datas._next_refresh_time = elapsed_time + self._light_probe_datas._light_probe_refresh_term;
            self._light_probe_datas._light_probe_blend_time = 0.0;
            self._light_probe_datas._light_probe_capture_count += 1;
        }

        let light_probe_term = self._light_probe_datas._light_probe_blend_term.min(self._light_probe_datas._light_probe_refresh_term);
        if self._light_probe_datas._light_probe_blend_time < light_probe_term {
            self._light_probe_datas._light_probe_blend_time += delta_time;
            let blend_ratio: f64 = 1.0f64.min(self._light_probe_datas._light_probe_blend_time / light_probe_term);
            self.copy_cube_map(
                renderer_data,
                command_buffer,
                swapchain_index,
                &resources,
                "copy_cube_map/blend",
                if application_constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
                    &self._light_probe_datas._light_probe_blend_from_forward_descriptor_sets
                } else {
                    &self._light_probe_datas._light_probe_blend_from_only_sky_descriptor_sets
                },
                application_constants::LIGHT_PROBE_SIZE,
                Some(&PushConstant_BlendCubeMap {
                    _blend_ratio: blend_ratio as f32,
                    _reserved0: 0,
                    _reserved1: 0,
                    _reserved2: 0,
                })
            );
        }

        // render solid object
        self.render_solid_object(renderer_data, command_buffer, swapchain_index, RenderMode::GBuffer, RenderObjectType::Static, &static_render_elements, None);
        self.render_solid_object(renderer_data, command_buffer, swapchain_index, RenderMode::GBuffer, RenderObjectType::Skeletal, &skeletal_render_elements, None);

        // pre-process: min-z, ssr, ssao, gbuffer, downsampling scnee color
        self.render_pre_process(renderer_data, command_buffer, swapchain_index, &quad_geometry_data);

        // render ocean
        fft_ocean.render_ocean(command_buffer, swapchain_index, &renderer_data, &resources);

        // render atmosphere
        let render_light_probe_mode: bool = false;
        atmosphere.render_precomputed_atmosphere(command_buffer, swapchain_index, &quad_geometry_data, &renderer_data, render_light_probe_mode);

        // render translucent
        {
            let effect_manager = self.get_effect_manager_mut();
            if effect_manager.get_need_to_clear_gpu_particle_buffer() {
                effect_manager.clear_gpu_particles(command_buffer, swapchain_index, self, &resources);
                effect_manager.set_need_to_clear_gpu_particle_buffer(false);
            }
            effect_manager.process_gpu_particles(command_buffer, swapchain_index, self, &resources);
            effect_manager.render_effects(command_buffer, swapchain_index, self, &resources);
        }

        // post-process: taa, bloom, motion blur
        self.render_post_process(renderer_data, command_buffer, swapchain_index, &quad_geometry_data, &resources);

        // Render Final
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_final", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // Render UI
        ui_manager_data.render_ui(command_buffer, swapchain_index, &renderer_data, &resources);

        // Render Text
        let render_text_info = RenderTextInfo {
            _render_font_size: 20,
            _initial_column: 0,
            _initial_row: 0,
            _render_text_offset: Vector2::new(10.0, 10.0),
        };
        font_manager.render_text(command_buffer, swapchain_index, &renderer_data, &resources, &render_text_info);

        // Render Debug
        if RenderTargetType::BackBuffer != self._debug_render_target {
            let render_debug_material_instance_name = "render_debug";
            let mut render_debug_material_instance_data: RefMut<MaterialInstanceData> = resources.get_material_instance_data(&render_debug_material_instance_name).borrow_mut();
            let mut render_debug_pipeline_binding_data = render_debug_material_instance_data.get_default_pipeline_binding_data_mut();
            renderer_data.begin_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                &render_debug_pipeline_binding_data.get_render_pass_data().borrow(),
                &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                None,
            );

            let debug_texture_data = self.get_render_target(self._debug_render_target);
            //let debug_texture_data = resources.get_texture_data("fft_ocean/butterfly").borrow();
            let descriptor_index = match debug_texture_data.get_image_view_type() {
                vk::ImageViewType::TYPE_2D => 1,
                vk::ImageViewType::TYPE_2D_ARRAY => 2,
                vk::ImageViewType::TYPE_3D => 3,
                vk::ImageViewType::CUBE => 4,
                _ => panic!("Not implemented."),
            };
            renderer_data.update_descriptor_set_mut(
                swapchain_index,
                &mut render_debug_pipeline_binding_data,
                descriptor_index,
                &DescriptorResourceInfo::DescriptorImageInfo(debug_texture_data.get_default_image_info()),
            );

            renderer_data.upload_push_constant_data(
                command_buffer,
                &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                &PushConstant_RenderDebug {
                    _debug_target: debug_texture_data.get_image_view_type().as_raw() as u32,
                    _mip_level: self._debug_render_target_miplevel,
                    ..Default::default()
                }
            );

            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, &render_debug_pipeline_binding_data, None);
            renderer_data.draw_elements(command_buffer, &quad_geometry_data);
            renderer_data.end_render_pass(command_buffer);
        }
    }
}

impl Renderer {
    pub fn create_renderer_data() -> Box<Renderer> {
        Box::new(Renderer {
            _renderer_data: std::ptr::null(),
            _resources: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _is_first_rendering: true,
            _scene_constants: shader_buffer_datas::SceneConstants::default(),
            _view_constants: shader_buffer_datas::ViewConstants::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _debug_render_target_layer: 0,
            _debug_render_target_miplevel: 0,
            _render_target_data_map: RenderTargetDataMap::new(),
            _shader_buffer_data_map: ShaderBufferDataMap::new(),
            _renderer_data_bloom: RendererData_Bloom::default(),
            _renderer_data_ssao: RendererData_SSAO::default(),
            _renderer_data_taa: RendererData_TAA::default(),
            _renderer_data_hiz: RendererData_HierachicalMinZ::default(),
            _scene_color_downsampling: RendererData_SceneColorDownSampling::default(),
            _renderer_data_ssr: RendererData_SSR::default(),
            _renderer_data_composite_gbuffer: RendererData_CompositeGBuffer::default(),
            _clear_render_targets: RendererData_ClearRenderTargets::default(),
            _light_probe_datas: RendererData_LightProbe::default(),
        })
    }

    pub fn get_effect_manager(&self) -> &EffectManager { unsafe { &*self._effect_manager } }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager { unsafe { &mut *(self._effect_manager as *mut EffectManager) } }
    pub fn get_renderer_data(&self) -> &RendererData { unsafe { &*self._renderer_data } }
    pub fn get_renderer_data_mut(&self) -> &mut RendererData { unsafe { &mut *(self._renderer_data as *mut RendererData) } }
    pub fn get_resources(&self) -> &Resources { unsafe { &*self._resources } }
    pub fn get_resources_mut(&self) -> &mut Resources { unsafe { &mut *(self._resources as *mut Resources) } }
    pub fn get_shader_buffer_data(&self, buffer_data_type: &ShaderBufferDataType) -> &ShaderBufferData {
        &self._shader_buffer_data_map.get(buffer_data_type).unwrap()
    }

    pub fn next_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let next_enum_value: i32 = self._debug_render_target as i32 + 1;
        const MAX_BOUND: i32 = RenderTargetType::MaxBound  as i32;
        self._debug_render_target = if next_enum_value < MAX_BOUND {
            unsafe { std::mem::transmute(next_enum_value) }
        } else {
            unsafe { std::mem::transmute(0) }
        };
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn prev_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let enum_to_int: i32 = self._debug_render_target as i32;
        self._debug_render_target = if 0 == enum_to_int {
            unsafe { std::mem::transmute(RenderTargetType::MaxBound as i32 - 1) }
        } else {
            unsafe { std::mem::transmute(enum_to_int - 1) }
        };
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn next_debug_render_target_miplevel(&mut self) {
        let texture_data: &TextureData = self.get_render_target(self._debug_render_target);
        self._debug_render_target_miplevel = texture_data._image_mip_levels.min(self._debug_render_target_miplevel + 1);
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn prev_debug_render_target_miplevel(&mut self) {
        if 0 < self._debug_render_target_miplevel {
            self._debug_render_target_miplevel -= 1;
        }
        log::info!("Current DebugRenderTarget: {:?} mip({})", self._debug_render_target, self._debug_render_target_miplevel);
    }

    pub fn get_render_target(&self, render_target_type: RenderTargetType) -> &TextureData {
        &self._render_target_data_map.get(&render_target_type).unwrap()
    }

    pub fn upload_shader_buffer_data<T>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &T) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_data().upload_shader_buffer_data(command_buffer, swapchain_index, shader_buffer_data, upload_data);
    }

    pub fn upload_shader_buffer_datas<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &[T]) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_data().upload_shader_buffer_datas(command_buffer, swapchain_index, shader_buffer_data, upload_data);
    }

    pub fn upload_shader_buffer_data_offset<T>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &T, offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_data().upload_shader_buffer_data_offset(command_buffer, swapchain_index, shader_buffer_data, upload_data, offset);
    }

    pub fn upload_shader_buffer_datas_offset<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &[T], offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_data().upload_shader_buffer_datas_offset(command_buffer, swapchain_index, shader_buffer_data, upload_data, offset);
    }

    pub fn rendering_at_first(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_data: &RendererData,
        resources: &Resources,
        _quad_geometry_data: &GeometryData,
    ) {
        // Clear render targets
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("clear_render_target").borrow();
        for (_, framebuffers) in self._clear_render_targets._color_framebuffer_datas.iter() {
            let default_frame_buffer = &framebuffers[0][0];
            let mut render_pass_pipeline_name = String::from("clear");
            for attachment_format in default_frame_buffer._framebuffer_info._framebuffer_color_attachment_formats.iter() {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            for attachment_format in default_frame_buffer._framebuffer_info._framebuffer_depth_attachment_formats.iter() {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            render_pass_pipeline_name.push_str("/clear");
            let pipeline_binding_data = material_instance_data.get_pipeline_binding_data(&render_pass_pipeline_name);
            for layer in 0..framebuffers.len() {
                for mip_level in 0.. framebuffers[layer].len() {
                    renderer_data.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &pipeline_binding_data.get_render_pass_data().borrow(),
                        &pipeline_binding_data.get_pipeline_data().borrow(),
                        Some(&framebuffers[layer][mip_level])
                    );
                    renderer_data.end_render_pass(command_buffer);
                }
            }
        }
    }

    pub fn copy_cube_map<T>(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        resources: &Ref<Resources>,
        render_pass_pipeline_data_name: &str,
        mip_level_descriptor_sets: &MipLevels<SwapchainArray<vk::DescriptorSet>>,
        image_width: u32,
        push_constant_data: Option<&T>,
    ) {
        let copy_cube_map_material_instance = resources.get_material_instance_data("copy_cube_map").borrow();
        let pipeline_binding_data = copy_cube_map_material_instance.get_pipeline_binding_data(render_pass_pipeline_data_name);
        let pipeline_data = pipeline_binding_data.get_pipeline_data().borrow();
        renderer_data.begin_compute_pipeline(command_buffer, &pipeline_data);
        let mip_levels = mip_level_descriptor_sets.len();
        for mip_level in 0..mip_levels {
            if let Some(push_constant_data) = push_constant_data {
                renderer_data.upload_push_constant_data(
                    command_buffer,
                    &pipeline_data,
                    push_constant_data
                );
            }
            let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            let dispatch_count = image_width >> mip_level;
            renderer_data.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
        }
    }

    pub fn render_light_probe(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        resources: &Ref<Resources>,
        scene_manager: &SceneManager,
        main_camera: &CameraObjectData,
        static_render_elements: &Vec<RenderElementData>,
        _fft_ocean: &FFTOcean,
    ) {
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("precomputed_atmosphere").borrow();
        let render_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_atmosphere/default");
        let composite_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("composite_atmosphere/default");
        let downsampling_material_instance = resources.get_material_instance_data("downsampling").borrow();
        let downsampling_pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let mut light_probe_view_constants = shader_buffer_datas::ViewConstants::default();
        let light_probe_view_constant_types = [
            ShaderBufferDataType::LightProbeViewConstants0,
            ShaderBufferDataType::LightProbeViewConstants1,
            ShaderBufferDataType::LightProbeViewConstants2,
            ShaderBufferDataType::LightProbeViewConstants3,
            ShaderBufferDataType::LightProbeViewConstants4,
            ShaderBufferDataType::LightProbeViewConstants5,
        ];
        let render_atmosphere_push_constants = PushConstant_Atmosphere {
            _render_light_probe_mode: 1,
            ..Default::default()
        };
        let main_camera_position = main_camera._transform_object.get_position();

        // copy only_sky to only_sky_prev
        self.copy_cube_map(
            renderer_data,
            command_buffer,
            swapchain_index,
            resources,
            "copy_cube_map/copy",
            &self._light_probe_datas._only_sky_copy_descriptor_sets,
            application_constants::LIGHT_PROBE_SIZE,
            NONE_PUSH_CONSTANT
        );

        // render atmosphere, inscatter
        for i in 0..constants::CUBE_LAYER_COUNT {
            let mut light_probe_camera = scene_manager.get_light_probe_camera(i).borrow_mut();
            light_probe_camera._transform_object.set_position(main_camera_position);
            light_probe_camera.update_camera_object_data();
            light_probe_view_constants.update_view_constants(&light_probe_camera);
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &light_probe_view_constant_types[i].clone(), &light_probe_view_constants);

            // render atmosphere
            renderer_data.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                render_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._light_probe_datas._render_atmosphere_framebuffer_datas[i]),
                Some(&self._light_probe_datas._render_atmosphere_descriptor_sets[i]),
                Some(&render_atmosphere_push_constants)
            );

            // composite atmosphere for only sky
            renderer_data.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                composite_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._light_probe_datas._composite_atmosphere_framebuffer_datas_only_sky[i]),
                Some(&self._light_probe_datas._composite_atmosphere_descriptor_sets[i]),
                NONE_PUSH_CONSTANT,
            );

            // downsampling for only sky
            renderer_data.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
            let mip_level_descriptor_sets = &self._light_probe_datas._only_sky_downsampling_descriptor_sets[i];
            let mip_levels = mip_level_descriptor_sets.len();
            for mip_level in 0..mip_levels {
                let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                let dispatch_count = application_constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                renderer_data.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
            }
        }

        // render static object for light probe
        if application_constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
            // copy light_probe_forward to light_probe_forward_prev
            self.copy_cube_map(
                renderer_data,
                command_buffer,
                swapchain_index,
                resources,
                "copy_cube_map/copy",
                &self._light_probe_datas._light_probe_forward_copy_descriptor_sets,
                application_constants::LIGHT_PROBE_SIZE,
                NONE_PUSH_CONSTANT
            );

            for i in 0..constants::CUBE_LAYER_COUNT {
                // clear light probe depth
                const CLEAR_LIGHT_PROBE_PIPELINES: [&str; 6] = [
                    "clear_light_probe_depth_0/clear",
                    "clear_light_probe_depth_1/clear",
                    "clear_light_probe_depth_2/clear",
                    "clear_light_probe_depth_3/clear",
                    "clear_light_probe_depth_4/clear",
                    "clear_light_probe_depth_5/clear",
                ];
                renderer_data.render_material_instance(command_buffer, swapchain_index, "clear_framebuffer", CLEAR_LIGHT_PROBE_PIPELINES[i], &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

                // composite atmosphere
                renderer_data.render_render_pass_pipeline(
                    command_buffer,
                    swapchain_index,
                    composite_atmosphere_pipeline_binding_data,
                    quad_geometry_data,
                    Some(&self._light_probe_datas._composite_atmosphere_framebuffer_datas[i]),
                    Some(&self._light_probe_datas._composite_atmosphere_descriptor_sets[i]),
                    NONE_PUSH_CONSTANT,
                );

                // render forward for light probe
                const RENDER_FORWARD_RENDER_PASS_PIPELINE_NAMES: [&str; 6] = [
                    "render_pass_static_forward_light_probe_0/render_object",
                    "render_pass_static_forward_light_probe_1/render_object",
                    "render_pass_static_forward_light_probe_2/render_object",
                    "render_pass_static_forward_light_probe_3/render_object",
                    "render_pass_static_forward_light_probe_4/render_object",
                    "render_pass_static_forward_light_probe_5/render_object",
                ];
                self.render_solid_object(
                    renderer_data,
                    command_buffer,
                    swapchain_index,
                    RenderMode::Forward,
                    RenderObjectType::Static,
                    static_render_elements,
                    Some(RENDER_FORWARD_RENDER_PASS_PIPELINE_NAMES[i])
                );

                // downsampling light probe
                renderer_data.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
                let mip_level_descriptor_sets = &self._light_probe_datas._light_probe_downsampling_descriptor_sets[i];
                let mip_levels = mip_level_descriptor_sets.len();
                for mip_level in 0..mip_levels {
                    let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                    renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                    let dispatch_count = application_constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                    renderer_data.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
                }
            }
        }
    }

    pub fn render_solid_object(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_mode: RenderMode,
        render_object_type: RenderObjectType,
        render_elements: &Vec<RenderElementData>,
        custom_render_pass_pipeline_name: Option<&str>,
    ) {
        if 0 == render_elements.len() {
            return;
        }

        unsafe {
            let render_pass_pipeline_data_name: &str = if custom_render_pass_pipeline_name.is_some() {
                custom_render_pass_pipeline_name.unwrap()
            } else {
                match (render_mode, render_object_type) {
                    (RenderMode::GBuffer, RenderObjectType::Static) => "render_pass_static_gbuffer/render_object",
                    (RenderMode::Forward, RenderObjectType::Static) => "render_pass_static_forward/render_object",
                    (RenderMode::Shadow, RenderObjectType::Static) => "render_pass_static_shadow/render_object",
                    (RenderMode::CaptureHeightMap, RenderObjectType::Static) => "capture_static_height_map/render_object",
                    (RenderMode::GBuffer, RenderObjectType::Skeletal) => "render_pass_skeletal_gbuffer/render_object",
                    (RenderMode::Forward, RenderObjectType::Skeletal) => "render_pass_skeletal_forward/render_object",
                    (RenderMode::Shadow, RenderObjectType::Skeletal) => "render_pass_skeletal_shadow/render_object",
                    (RenderMode::CaptureHeightMap, RenderObjectType::Skeletal) => "capture_skeletal_height_map/render_object",
                }
            };

            // upload skeleton bone matrices
            let mut bone_metrices_offset: vk::DeviceSize = 0;
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
            for render_element in render_elements.iter() {
                let render_object = render_element._render_object.borrow();
                match render_object_type {
                    RenderObjectType::Static => {
                    },
                    RenderObjectType::Skeletal => {
                        let prev_animation_buffer: &Vec<Matrix4<f32>> = render_object.get_prev_animation_buffer(0);
                        let animation_buffer: &Vec<Matrix4<f32>> = render_object.get_animation_buffer(0);
                        let bone_count = prev_animation_buffer.len() as vk::DeviceSize;
                        let prev_animation_buffer_offset = bone_metrices_offset * std::mem::size_of::<Matrix4<f32>>() as vk::DeviceSize;
                        let animation_buffer_offset = (bone_metrices_offset + bone_count) * std::mem::size_of::<Matrix4<f32>>() as vk::DeviceSize;

                        // TODO : Upload at once
                        self.upload_shader_buffer_datas_offset(command_buffer, swapchain_index, &ShaderBufferDataType::BoneMatrices, &prev_animation_buffer, prev_animation_buffer_offset);
                        self.upload_shader_buffer_datas_offset(command_buffer, swapchain_index, &ShaderBufferDataType::BoneMatrices, &animation_buffer, animation_buffer_offset);

                        // bone_count = (curr_animation_bone_count + prev_animation_bone_count)
                        bone_metrices_offset += bone_count * 2;
                    },
                };
            }

            // render
            let mut bone_metrices_offset: vk::DeviceSize = 0;
            for render_element in render_elements.iter() {
                let render_object = render_element._render_object.borrow();
                let pipeline_binding_data: *const PipelineBindingData = render_element._material_instance_data.borrow().get_pipeline_binding_data(&render_pass_pipeline_data_name);
                let render_pass_data = &(*pipeline_binding_data).get_render_pass_data().borrow();
                let pipeline_data = (*pipeline_binding_data).get_pipeline_data();
                let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();
                let pipeline_data: &PipelineData = &pipeline_data.borrow();

                if prev_pipeline_data != pipeline_data_ptr {
                    if false == prev_pipeline_data.is_null() {
                        renderer_data.end_render_pass(command_buffer);
                    }
                    renderer_data.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
                    prev_pipeline_data = pipeline_data_ptr;
                }

                if prev_pipeline_binding_data != pipeline_binding_data {
                    prev_pipeline_binding_data = pipeline_binding_data;
                    renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, &(*pipeline_binding_data), None);
                }

                match render_object_type {
                    RenderObjectType::Static => {
                        renderer_data.upload_push_constant_data(
                            command_buffer,
                            pipeline_data,
                            &PushConstant_StaticRenderObject {
                                _local_matrix: render_object._transform_object.get_matrix().clone() as Matrix4<f32>
                            }
                        );
                    },
                    RenderObjectType::Skeletal => {
                        let prev_animation_buffer: &Vec<Matrix4<f32>> = render_object.get_prev_animation_buffer(0);
                        let bone_count = prev_animation_buffer.len() as vk::DeviceSize;
                        renderer_data.upload_push_constant_data(
                            command_buffer,
                            pipeline_data,
                            &PushConstant_SkeletalRenderObject {
                                _local_matrix: render_object._transform_object.get_matrix().clone() as Matrix4<f32>,
                                _bone_matrix_offset: bone_metrices_offset as u32,
                                _bone_matrix_count: bone_count as u32,
                                ..Default::default()
                            }
                        );
                        // bone_count = (curr_animation_bone_count + prev_animation_bone_count)
                        bone_metrices_offset += bone_count * 2;
                    },
                };
                renderer_data.draw_elements(command_buffer, &render_element._geometry_data.borrow());
            }
            renderer_data.end_render_pass(command_buffer);
        }
    }

    pub fn render_taa(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // render_taa
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_taa", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // copy SceneColorCopy -> TAAResolve
        let framebuffer = Some(&self._renderer_data_taa._taa_resolve_framebuffer_data);
        let descriptor_sets = Some(&self._renderer_data_taa._taa_descriptor_sets);
        let push_constants = PushConstant_RenderCopy::default();
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_copy", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, Some(&push_constants));
    }

    pub fn render_bloom(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_data: &RendererData,
        resources: &Resources
    ) {
        let render_bloom_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_bloom").borrow();
        // render_bloom_highlight
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
        renderer_data.render_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            quad_geometry_data,
            None,
            None,
            Some(&self._renderer_data_bloom._bloom_push_constants)
        );

        // render_bloom_downsampling
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let framebuffer_count = self._renderer_data_bloom._bloom_downsample_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            renderer_data.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._renderer_data_bloom._bloom_downsample_framebuffer_datas[i]),
                Some(&self._renderer_data_bloom._bloom_downsample_descriptor_sets[i]),
                NONE_PUSH_CONSTANT
            );
        }

        // render_gaussian_blur
        let render_gaussian_blur_material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance_data.get_default_pipeline_binding_data();
        let framebuffer_count = self._renderer_data_bloom._bloom_temp_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            renderer_data.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._renderer_data_bloom._bloom_temp_framebuffer_datas[i]),
                Some(&self._renderer_data_bloom._bloom_temp_descriptor_sets[i]),
                Some(&PushConstant_GaussianBlur {
                    _blur_scale: if 0 == (i % 2) {
                        Vector2::new(1.0, 0.0)
                    } else {
                        Vector2::new(0.0, 1.0)
                    },
                    ..Default::default()
                })
            );
        }
    }

    pub fn render_ssr(&self, renderer_data: &RendererData, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // Screen Space Reflection
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_ssr", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // Screen Space Reflection Resolve
        let (framebuffer, descriptor_sets) = match self._renderer_data_ssr._current_ssr_resolved {
            RenderTargetType::SSRResolved => (Some(&self._renderer_data_ssr._framebuffer_data0), Some(&self._renderer_data_ssr._descriptor_sets0)),
            RenderTargetType::SSRResolvedPrev => (Some(&self._renderer_data_ssr._framebuffer_data1), Some(&self._renderer_data_ssr._descriptor_sets1)),
            _ => panic!("error")
        };
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_ssr_resolve", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, NONE_PUSH_CONSTANT);
    }

    pub fn render_ssao(&self, renderer_data: &RendererData, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // render ssao
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_ssao", DEFAULT_PIPELINE, quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // render ssao blur
        let framebuffer_h = Some(&self._renderer_data_ssao._ssao_blur_framebuffer_data0);
        let descriptor_sets_h = Some(&self._renderer_data_ssao._ssao_blur_descriptor_sets0);
        let framebuffer_v = Some(&self._renderer_data_ssao._ssao_blur_framebuffer_data1);
        let descriptor_sets_v = Some(&self._renderer_data_ssao._ssao_blur_descriptor_sets1);
        let push_constants_blur_h = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 0.0),
            ..Default::default()
        };
        let push_constants_blur_v = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(0.0, 1.0),
            ..Default::default()
        };
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_h, descriptor_sets_h, Some(&push_constants_blur_h));
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_v, descriptor_sets_v, Some(&push_constants_blur_v));
    }

    pub fn composite_gbuffer(&self, renderer_data: &RendererData, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let descriptor_sets = match self._renderer_data_ssr._current_ssr_resolved {
            RenderTargetType::SSRResolved => Some(&self._renderer_data_composite_gbuffer._descriptor_sets0),
            RenderTargetType::SSRResolvedPrev => Some(&self._renderer_data_composite_gbuffer._descriptor_sets1),
            _ => panic!("error")
        };
        renderer_data.render_material_instance(command_buffer, swapchain_index, "composite_gbuffer", DEFAULT_PIPELINE, &quad_geometry_data, None, descriptor_sets, NONE_PUSH_CONSTANT);
    }

    pub fn generate_min_z(&self, renderer_data: &RendererData, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let resources: Ref<Resources> = renderer_data._resources.borrow();

        // Copy Scene Depth
        renderer_data.render_material_instance(command_buffer, swapchain_index, "generate_min_z", "generate_min_z/render_copy", &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);

        // Generate Hierachical Min Z
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("generate_min_z").borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._renderer_data_hiz._descriptor_sets.len();
        renderer_data.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._renderer_data_hiz._descriptor_sets[mip_level as usize]);
            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            renderer_data.dispatch_compute_pipeline(
                command_buffer,
                self._renderer_data_hiz._dispatch_group_x >> (mip_level + 1),
                self._renderer_data_hiz._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn scene_color_downsampling(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, renderer_data: &RendererData) {
        let resources: Ref<Resources> = renderer_data._resources.borrow();
        let material_instance_data: Ref<MaterialInstanceData> = resources.get_material_instance_data("downsampling").borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._scene_color_downsampling._descriptor_sets.len();
        renderer_data.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._scene_color_downsampling._descriptor_sets[mip_level as usize]);
            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            renderer_data.dispatch_compute_pipeline(
                command_buffer,
                self._scene_color_downsampling._dispatch_group_x >> (mip_level + 1),
                self._scene_color_downsampling._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn render_pre_process(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // Generate Hierachical Min Z
        self.generate_min_z(renderer_data, command_buffer, swapchain_index, quad_geometry_data);

        // Screen Space Reflection
        self.render_ssr(renderer_data, command_buffer, swapchain_index, quad_geometry_data);

        // SSAO
        self.render_ssao(renderer_data, command_buffer, swapchain_index, quad_geometry_data);

        // Composite GBuffer
        self.composite_gbuffer(renderer_data, command_buffer, swapchain_index, quad_geometry_data);

        // SceneColor Downsampling
        self.scene_color_downsampling(command_buffer, swapchain_index, renderer_data);
    }

    pub fn render_post_process(
        &self,
        renderer_data: &RendererData,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        resources: &Resources
    ) {
        // TAA
        self.render_taa(renderer_data, command_buffer, swapchain_index, quad_geometry_data);

        // Bloom
        self.render_bloom(command_buffer, swapchain_index, quad_geometry_data, renderer_data, resources);

        // Motion Blur
        renderer_data.render_material_instance(command_buffer, swapchain_index, "render_motion_blur", DEFAULT_PIPELINE, &quad_geometry_data, None, None, NONE_PUSH_CONSTANT);
    }
}