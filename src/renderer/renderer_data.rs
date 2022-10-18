use std::str::FromStr;
use std::collections::HashMap;
use std::cell::{ Ref, RefMut };
use std::vec::Vec;

use ash::{ vk, Device };
use nalgebra::{Vector2, Matrix4};

use crate::constants;
use crate::application::scene_manager::ProjectSceneManagerBase;
use crate::effect::effect_manager::EffectManager;
use crate::renderer::camera::CameraObjectData;
use crate::renderer::font::{ FontManager, RenderTextInfo };
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::render_element::RenderElementData;
use crate::renderer::renderer_context::{RendererDataBase, RendererContext };
use crate::renderer::fft_ocean::FFTOcean;
use crate::renderer::precomputed_atmosphere::{ Atmosphere, PushConstant_PrecomputedAtmosphere };
use crate::renderer::push_constants::{
    PushConstant_GaussianBlur,
    PushConstant_RenderCopy,
    PushConstant_RenderDebug,
    PushConstant_BlendCubeMap,
};
use crate::renderer::render_target::{ self, RenderTargetType };
use crate::renderer::render_context::{
    RenderContext_Bloom,
    RenderContext_SSAO,
    RenderContext_TAA,
    RenderContext_HierachicalMinZ,
    RenderContext_SceneColorDownSampling,
    RenderContext_TAA_Simple,
    RenderContext_CompositeGBuffer,
    RenderContext_ClearRenderTargets,
    RenderContext_LightProbe,
};
use crate::renderer::shader_buffer_datas::{
    self,
    ShaderBufferDataType,
    ShaderBufferDataMap,
};
use crate::render_pass::render_pass;
use crate::renderer::ui::UIManager;
use crate::renderer::utility;
use crate::resource::resource::EngineResources;
use crate::vulkan_context::buffer::{ self, ShaderBufferData };
use crate::vulkan_context::descriptor::{ DescriptorResourceInfo };
use crate::vulkan_context::geometry_buffer::{ GeometryData };
use crate::vulkan_context::render_pass::{ RenderPassDataCreateInfo, PipelineData };
use crate::vulkan_context::texture::{ self, TextureData };
use crate::vulkan_context::vulkan_context::{ self, SwapchainArray, MipLevels };
use crate::utilities::system::ptr_as_ref;


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

pub struct RendererData {
    pub _renderer_context: *const RendererContext,
    pub _engine_resources: *const EngineResources,
    pub _effect_manager: *const EffectManager,
    pub _is_first_rendering: bool,
    pub _scene_constants: shader_buffer_datas::SceneConstants,
    pub _view_constants: shader_buffer_datas::ViewConstants,
    pub _debug_render_target: RenderTargetType,
    pub _debug_render_target_layer: u32,
    pub _debug_render_target_miplevel: u32,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _shader_buffer_data_map: ShaderBufferDataMap,
    pub _render_context_bloom: RenderContext_Bloom,
    pub _render_context_ssao: RenderContext_SSAO,
    pub _render_context_taa: RenderContext_TAA,
    pub _render_context_hiz: RenderContext_HierachicalMinZ,
    pub _render_context_scene_color_downsampling: RenderContext_SceneColorDownSampling,
    pub _render_context_ssr: RenderContext_TAA_Simple,
    pub _render_context_composite_gbuffer: RenderContext_CompositeGBuffer,
    pub _render_context_clear_render_targets: RenderContext_ClearRenderTargets,
    pub _render_context_light_probe: RenderContext_LightProbe,
    pub _fft_ocean: FFTOcean,
    pub _atmosphere: Atmosphere,
}

impl RendererDataBase for RendererData {
    fn initialize_renderer_data(
        &mut self,
        renderer_context: &RendererContext,
        engine_resources: *const EngineResources,
        effect_manager: *const EffectManager
    ) {
        self._renderer_context = renderer_context;
        self._engine_resources = engine_resources;
        self._effect_manager = effect_manager;

        shader_buffer_datas::regist_shader_buffer_datas(renderer_context.get_device(), renderer_context.get_device_memory_properties(), &mut self._shader_buffer_data_map);

        self.create_render_targets(renderer_context);
        self.get_fft_ocean_mut().regist_fft_ocean_textures(renderer_context, self.get_engine_resources_mut());
    }

    fn is_first_rendering(&self) -> bool {
        self._is_first_rendering
    }

    fn set_is_first_rendering(&mut self, is_first_rendering: bool) {
        log::info!("set_is_first_rendering: {}", is_first_rendering);
        self._is_first_rendering = is_first_rendering;
    }

    fn prepare_framebuffer_and_descriptors(&mut self, device: &Device, engine_resources: &EngineResources) {
        // Bloom
        self._render_context_bloom.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::BloomTemp0).as_ref().unwrap(),
        );
        // Temporal AA
        self._render_context_taa.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColorCopy).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(),
        );
        // SSAO
        self._render_context_ssao.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SSAO).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSAOTemp).as_ref().unwrap(),
        );
        // Hierachical Min Z
        self._render_context_hiz.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(),
        );
        // SceneColor Downsampling
        self._render_context_scene_color_downsampling.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(),
        );
        // SSR
        self._render_context_ssr.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SSR).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
            RenderTargetType::SSRResolved,
            RenderTargetType::SSRResolvedPrev
        );
        // Composite GBuffer
        self._render_context_composite_gbuffer.initialize(
            device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(),
        );
        // RenderContext_LightProbe
        self._render_context_light_probe.initialize(
            device,
            engine_resources,
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

        self.get_fft_ocean_mut().prepare_framebuffer_and_descriptors(self, self.get_engine_resources());
        self.get_atmosphere_mut().prepare_framebuffer_and_descriptors(self, self.get_engine_resources());

        // TEST CODE
        if self.get_renderer_context().get_use_ray_tracing() {
            log::info!(">>> TEST CODE: RayTracing create_descriptor_sets");
            let material_instance = engine_resources.get_material_instance_data("ray_tracing/ray_tracing").borrow();
            let render_ray_tracing_pipeline_binding_data = material_instance.get_default_pipeline_binding_data();
            let top_level_descriptor_resource_info = self.get_renderer_context()._ray_tracing_test_data.get_top_level_descriptor_resource_info();
            let _render_ray_tracing_descriptor_sets = utility::create_descriptor_sets(
                device,
                render_ray_tracing_pipeline_binding_data,
                &[ (0, utility::create_swapchain_array(top_level_descriptor_resource_info.clone())) ],
            );
        }

        // Last - Clear Render Targets
        self._render_context_clear_render_targets.initialize(
            device,
            engine_resources,
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
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::Shadow).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeDepth).as_ref().unwrap(), vulkan_context::get_depth_clear_one()),
            ]
        );
    }

    fn destroy_framebuffer_and_descriptors(&mut self, device: &Device) {
        self.get_fft_ocean_mut().destroy_fft_ocean(self.get_renderer_context().get_device());
        self.get_atmosphere_mut().destroy_atmosphere(self.get_renderer_context().get_device());
        self._render_context_bloom.destroy(device);
        self._render_context_taa.destroy(device);
        self._render_context_ssao.destroy(device);
        self._render_context_hiz.destroy(device);
        self._render_context_scene_color_downsampling.destroy(device);
        self._render_context_ssr.destroy(device);
        self._render_context_composite_gbuffer.destroy(device);
        self._render_context_clear_render_targets.destroy(device);
        self._render_context_light_probe.destroy(device);
    }

    fn get_shader_buffer_data_from_str(&self, buffer_data_name: &str) -> &ShaderBufferData {
        self.get_shader_buffer_data(&ShaderBufferDataType::from_str(buffer_data_name).unwrap())
    }
    fn get_render_target_from_str(&self, render_target_type_str: &str) -> &TextureData {
        self.get_render_target(RenderTargetType::from_str(render_target_type_str).unwrap())
    }
    fn get_render_pass_data_create_infos(&self) -> Vec<RenderPassDataCreateInfo> {
        render_pass::get_render_pass_data_create_infos(self)
    }
    fn create_render_targets(&mut self, renderer_context: &RendererContext) {
        log::info!("create_render_targets");
        let render_taget_create_infos = render_target::get_render_target_create_infos(renderer_context);
        for render_taget_create_info in render_taget_create_infos.iter() {
            let render_target_type: RenderTargetType = RenderTargetType::from_str(render_taget_create_info._texture_name.as_str()).unwrap();
            let texture_data = renderer_context.create_render_target(render_taget_create_info);
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

    fn pre_update_render_scene(&mut self, delta_time: f64) {
        self._atmosphere.update();
        self._fft_ocean.update(delta_time);
        self._render_context_ssr.update();
    }

    fn render_scene(
        &mut self,
        command_buffer: vk::CommandBuffer,
        frame_index: usize,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        project_scene_manager: &dyn ProjectSceneManagerBase,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        elapsed_time: f64,
        delta_time: f64,
        _elapsed_frame: u64,
    ) {
        let engine_resources = renderer_context.get_engine_resources();
        let main_camera = project_scene_manager.get_main_camera();
        let main_light = project_scene_manager.get_main_light().borrow();
        let mut capture_height_map = project_scene_manager.get_capture_height_map().borrow_mut();
        let render_capture_height_map: bool = capture_height_map.get_need_to_redraw_shadow_and_reset();
        let quad_mesh = engine_resources.get_mesh_data("quad").borrow();
        let quad_geometry_data: Ref<GeometryData> = quad_mesh.get_default_geometry_data().borrow();
        let static_render_elements = project_scene_manager.get_static_render_elements();
        let static_shadow_render_elements = project_scene_manager.get_static_shadow_render_elements();
        let skeletal_render_elements = project_scene_manager.get_skeletal_render_elements();
        let skeletal_shadow_render_elements = project_scene_manager.get_skeletal_shadow_render_elements();

        // pre update
        self.pre_update_render_scene(delta_time);

        // Upload Uniform Buffers
        {
            self._scene_constants.update_scene_constants(
                renderer_context._swapchain_data._swapchain_extent.width,
                renderer_context._swapchain_data._swapchain_extent.height,
                elapsed_time,
                delta_time,
                self._fft_ocean.get_height(),
                self.get_effect_manager().get_gpu_particle_count_buffer_offset(frame_index),
                self.get_effect_manager().get_gpu_particle_update_buffer_offset(frame_index),
            );
            self._view_constants.update_view_constants(&main_camera);
            if render_capture_height_map {
                self._view_constants._capture_height_map_view_projection = (*capture_height_map.get_shadow_view_projection()).into();
            }

            self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::SceneConstants, &self._scene_constants);
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::ViewConstants, &self._view_constants);
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::LightConstants, main_light.get_light_constants());
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::SSAOConstants, &self._render_context_ssao._ssao_constants);
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &ShaderBufferDataType::AtmosphereConstants, &self._atmosphere._atmosphere_constants);

            let transform_matrix_count = project_scene_manager.get_render_element_transform_count();
            let transform_matrices: &[Matrix4<f32>] = &project_scene_manager.get_render_element_transform_metrices()[..transform_matrix_count];
            self.upload_shader_buffer_datas(command_buffer, swapchain_index, &ShaderBufferDataType::TransformMatrices, transform_matrices);
        }

        if self._is_first_rendering {
            self.clear_render_targets(command_buffer, swapchain_index, renderer_context, &engine_resources, &quad_geometry_data);
            self._fft_ocean.compute_slope_variance_texture(command_buffer, swapchain_index, &quad_geometry_data, renderer_context, &engine_resources);
            self._atmosphere.precompute(command_buffer, swapchain_index, &quad_geometry_data, renderer_context);
        }

        // clear gbuffer
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/clear_framebuffer", "clear_gbuffer/clear", &quad_geometry_data, None, None, None);

        // render shadow
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/clear_framebuffer", "clear_shadow/clear", &quad_geometry_data, None, None, None);
        self.render_solid_object(renderer_context, command_buffer, swapchain_index, "render_pass_static_shadow", &static_shadow_render_elements);
        self.render_solid_object(renderer_context, command_buffer, swapchain_index, "render_pass_skeletal_shadow", &skeletal_shadow_render_elements);

        // capture height map
        if render_capture_height_map || self._is_first_rendering {
            renderer_context.render_material_instance(command_buffer, swapchain_index, "common/clear_framebuffer", "clear_capture_height_map/clear", &quad_geometry_data, None, None, None);
            self.render_solid_object(renderer_context, command_buffer, swapchain_index, "capture_static_height_map", &static_render_elements);
        }

        // fft-simulation
        self._fft_ocean.simulate_fft_waves(command_buffer, swapchain_index, &quad_geometry_data, renderer_context, &engine_resources);

        // light probe
        if self._render_context_light_probe._next_refresh_time <= elapsed_time || self._render_context_light_probe._light_probe_capture_count < 2 {
            self.render_light_probe(
                renderer_context,
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                &engine_resources,
                project_scene_manager,
                &main_camera,
                static_render_elements,
                &self._fft_ocean
            );
            self._render_context_light_probe._next_refresh_time = elapsed_time + self._render_context_light_probe._light_probe_refresh_term;
            self._render_context_light_probe._light_probe_blend_time = 0.0;
            self._render_context_light_probe._light_probe_capture_count += 1;
        }

        let light_probe_term = self._render_context_light_probe._light_probe_blend_term.min(self._render_context_light_probe._light_probe_refresh_term);
        if self._render_context_light_probe._light_probe_blend_time < light_probe_term {
            self._render_context_light_probe._light_probe_blend_time += delta_time;
            let blend_ratio: f64 = 1.0f64.min(self._render_context_light_probe._light_probe_blend_time / light_probe_term);
            self.copy_cube_map(
                renderer_context,
                command_buffer,
                swapchain_index,
                &engine_resources,
                "copy_cube_map/blend",
                if constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
                    &self._render_context_light_probe._light_probe_blend_from_forward_descriptor_sets
                } else {
                    &self._render_context_light_probe._light_probe_blend_from_only_sky_descriptor_sets
                },
                constants::LIGHT_PROBE_SIZE,
                Some(&PushConstant_BlendCubeMap {
                    _blend_ratio: blend_ratio as f32,
                    _reserved0: 0,
                    _reserved1: 0,
                    _reserved2: 0,
                })
            );
        }

        // render solid object
        self.render_solid_object(renderer_context, command_buffer, swapchain_index, "render_pass_static_gbuffer", &static_render_elements);
        self.render_solid_object(renderer_context, command_buffer, swapchain_index, "render_pass_skeletal_gbuffer", &skeletal_render_elements);

        // process gpu particles
        {
            let effect_manager = self.get_effect_manager_mut();
            if effect_manager.get_need_to_clear_gpu_particle_buffer() {
                effect_manager.clear_gpu_particles(command_buffer, swapchain_index, renderer_context, &engine_resources);
                effect_manager.set_need_to_clear_gpu_particle_buffer(false);
            }
            effect_manager.process_gpu_particles(command_buffer, swapchain_index, self, &engine_resources);
        }

        // pre-process: min-z, ssr, ssao, gbuffer, downsampling scnee color
        self.render_pre_process(renderer_context, command_buffer, swapchain_index, &quad_geometry_data);

        // render ocean
        self._fft_ocean.render_ocean(command_buffer, swapchain_index, &renderer_context, &engine_resources);

        // render atmosphere
        let render_light_probe_mode: bool = false;
        self._atmosphere.render_precomputed_atmosphere(command_buffer, swapchain_index, &quad_geometry_data, &renderer_context, render_light_probe_mode);

        // render translucent
        self.render_translucent(command_buffer, swapchain_index, &engine_resources);

        // TEST_CODE: ray tracing test
        if renderer_context.get_use_ray_tracing() {
            self.render_ray_tracing(renderer_context, command_buffer, swapchain_index, &engine_resources);
        }

        // post-process: taa, bloom, motion blur
        self.render_post_process(renderer_context, command_buffer, swapchain_index, &quad_geometry_data, &engine_resources);

        // Render Final
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_final", DEFAULT_PIPELINE, &quad_geometry_data, None, None, None);

        // Render UI
        ui_manager.render_ui(command_buffer, swapchain_index, &renderer_context, &engine_resources);

        // Render Text
        let render_text_info = RenderTextInfo {
            _render_font_size: 12,
            _initial_column: 0,
            _initial_row: 0,
            _render_text_offset: Vector2::new(10.0, 10.0),
        };
        font_manager.render_text(command_buffer, swapchain_index, &renderer_context, &engine_resources, &render_text_info);

        // Render Debug
        if RenderTargetType::BackBuffer != self._debug_render_target {
            let mut render_debug_material_instance_data: RefMut<MaterialInstanceData> = engine_resources.get_material_instance_data(&"common/render_debug").borrow_mut();
            let mut render_debug_pipeline_binding_data = render_debug_material_instance_data.get_default_pipeline_binding_data_mut();
            renderer_context.begin_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                &render_debug_pipeline_binding_data.get_render_pass_data().borrow(),
                &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                None,
            );

            let debug_texture_data = self.get_render_target(self._debug_render_target);
            //let debug_texture_data = engine_resources.get_texture_data("fft_ocean/butterfly").borrow();
            let descriptor_index = match debug_texture_data.get_image_view_type() {
                vk::ImageViewType::TYPE_2D => 1,
                vk::ImageViewType::TYPE_2D_ARRAY => 2,
                vk::ImageViewType::TYPE_3D => 3,
                vk::ImageViewType::CUBE => 4,
                _ => panic!("Not implemented."),
            };
            renderer_context.update_descriptor_set_mut(
                swapchain_index,
                &mut render_debug_pipeline_binding_data,
                descriptor_index,
                &DescriptorResourceInfo::DescriptorImageInfo(debug_texture_data.get_default_image_info()),
            );

            renderer_context.upload_push_constant_data(
                command_buffer,
                &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                &PushConstant_RenderDebug {
                    _debug_target: debug_texture_data.get_image_view_type().as_raw() as u32,
                    _mip_level: self._debug_render_target_miplevel,
                    ..Default::default()
                }
            );

            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, &render_debug_pipeline_binding_data, None);
            renderer_context.draw_elements(command_buffer, &quad_geometry_data);
            renderer_context.end_render_pass(command_buffer);
        }

        // TEST CODE: readback image
        // let texture_data = self.get_render_target(RenderTargetType::CaptureHeightMap);
        // let buffer_size = unsafe { renderer_context.get_device().get_image_memory_requirements(texture_data._image).size };
        // let mut read_data: Vec<f32> = vec![0.0; buffer_size as usize];
        // texture::read_texture_data(
        //     renderer_context.get_device(),
        //     renderer_context.get_command_pool(),
        //     renderer_context.get_graphics_queue(),
        //     renderer_context.get_device_memory_properties(),
        //     texture_data,
        //     &mut read_data
        // );
        // println!("{:?}", read_data);
    }
}

impl RendererData {
    pub fn create_renderer_data() -> RendererData {
        RendererData {
            _renderer_context: std::ptr::null(),
            _engine_resources: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _is_first_rendering: true,
            _scene_constants: shader_buffer_datas::SceneConstants::default(),
            _view_constants: shader_buffer_datas::ViewConstants::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _debug_render_target_layer: 0,
            _debug_render_target_miplevel: 0,
            _render_target_data_map: RenderTargetDataMap::new(),
            _shader_buffer_data_map: ShaderBufferDataMap::new(),
            _render_context_bloom: RenderContext_Bloom::default(),
            _render_context_ssao: RenderContext_SSAO::default(),
            _render_context_taa: RenderContext_TAA::default(),
            _render_context_hiz: RenderContext_HierachicalMinZ::default(),
            _render_context_scene_color_downsampling: RenderContext_SceneColorDownSampling::default(),
            _render_context_ssr: RenderContext_TAA_Simple::default(),
            _render_context_composite_gbuffer: RenderContext_CompositeGBuffer::default(),
            _render_context_clear_render_targets: RenderContext_ClearRenderTargets::default(),
            _render_context_light_probe: RenderContext_LightProbe::default(),
            _fft_ocean: FFTOcean::default(),
            _atmosphere: Atmosphere::create_atmosphere(true),
        }
    }

    pub fn get_effect_manager(&self) -> &EffectManager { unsafe { &*self._effect_manager } }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager { unsafe { &mut *(self._effect_manager as *mut EffectManager) } }
    pub fn get_renderer_context(&self) -> &RendererContext { unsafe { &*self._renderer_context } }
    pub fn get_renderer_context_mut(&self) -> &mut RendererContext { unsafe { &mut *(self._renderer_context as *mut RendererContext) } }
    pub fn get_engine_resources(&self) -> &EngineResources { unsafe { &*self._engine_resources } }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources { unsafe { &mut *(self._engine_resources as *mut EngineResources) } }
    pub fn get_fft_ocean_mut(&self) -> &mut FFTOcean { unsafe { &mut *((&self._fft_ocean as *const FFTOcean) as *mut FFTOcean) } }
    pub fn get_atmosphere_mut(&self) -> &mut Atmosphere { unsafe { &mut *((&self._atmosphere as *const Atmosphere) as *mut Atmosphere) } }
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
        self.get_renderer_context().upload_shader_buffer_data(command_buffer, swapchain_index, shader_buffer_data, upload_data);
    }

    pub fn upload_shader_buffer_datas<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &[T]) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context().upload_shader_buffer_datas(command_buffer, swapchain_index, shader_buffer_data, upload_data);
    }

    pub fn upload_shader_buffer_data_offset<T>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &T, offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context().upload_shader_buffer_data_offset(command_buffer, swapchain_index, shader_buffer_data, upload_data, offset);
    }

    pub fn upload_shader_buffer_datas_offset<T: Copy>(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, shader_buffer_data_type: &ShaderBufferDataType, upload_data: &[T], offset: vk::DeviceSize) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context().upload_shader_buffer_datas_offset(command_buffer, swapchain_index, shader_buffer_data, upload_data, offset);
    }

    pub fn clear_render_targets(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources,
        _quad_geometry_data: &GeometryData,
    ) {
        let material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data("common/clear_render_target").borrow();
        for (_, framebuffers) in self._render_context_clear_render_targets._color_framebuffer_datas.iter() {
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
                    renderer_context.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &pipeline_binding_data.get_render_pass_data().borrow(),
                        &pipeline_binding_data.get_pipeline_data().borrow(),
                        Some(&framebuffers[layer][mip_level])
                    );
                    renderer_context.end_render_pass(command_buffer);
                }
            }
        }
    }

    pub fn copy_cube_map(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        engine_resources: &EngineResources,
        render_pass_pipeline_data_name: &str,
        mip_level_descriptor_sets: &MipLevels<SwapchainArray<vk::DescriptorSet>>,
        image_width: u32,
        push_constant_data: Option<&PushConstant_BlendCubeMap>,
    ) {
        let copy_cube_map_material_instance = engine_resources.get_material_instance_data("common/copy_cube_map").borrow();
        let pipeline_binding_data = copy_cube_map_material_instance.get_pipeline_binding_data(render_pass_pipeline_data_name);
        let pipeline_data = pipeline_binding_data.get_pipeline_data().borrow();
        renderer_context.begin_compute_pipeline(command_buffer, &pipeline_data);
        let mip_levels = mip_level_descriptor_sets.len();
        for mip_level in 0..mip_levels {
            if let Some(push_constant_data) = push_constant_data {
                renderer_context.upload_push_constant_data(
                    command_buffer,
                    &pipeline_data,
                    push_constant_data
                );
            }
            let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            let dispatch_count = image_width >> mip_level;
            renderer_context.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
        }
    }

    pub fn render_light_probe(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        engine_resources: &EngineResources,
        project_scene_manager: &dyn ProjectSceneManagerBase,
        main_camera: &CameraObjectData,
        static_render_elements: &Vec<RenderElementData>,
        _fft_ocean: &FFTOcean,
    ) {
        let material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data("precomputed_atmosphere/precomputed_atmosphere").borrow();
        let render_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_atmosphere/default");
        let composite_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("composite_atmosphere/default");
        let downsampling_material_instance = engine_resources.get_material_instance_data("common/downsampling").borrow();
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
        let render_atmosphere_push_constants = PushConstant_PrecomputedAtmosphere {
            _render_light_probe_mode: 1,
            ..Default::default()
        };
        let main_camera_position = main_camera._transform_object.get_position();

        // copy only_sky to only_sky_prev
        self.copy_cube_map(
            renderer_context,
            command_buffer,
            swapchain_index,
            engine_resources,
            "copy_cube_map/copy",
            &self._render_context_light_probe._only_sky_copy_descriptor_sets,
            constants::LIGHT_PROBE_SIZE,
            None
        );

        // render atmosphere, inscatter
        for i in 0..constants::CUBE_LAYER_COUNT {
            let mut light_probe_camera = project_scene_manager.get_light_probe_camera(i).borrow_mut();
            light_probe_camera._transform_object.set_position(main_camera_position);
            light_probe_camera.update_camera_object_data();
            light_probe_view_constants.update_view_constants(&light_probe_camera);
            self.upload_shader_buffer_data(command_buffer, swapchain_index, &light_probe_view_constant_types[i].clone(), &light_probe_view_constants);

            // render atmosphere
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                render_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._render_context_light_probe._render_atmosphere_framebuffer_datas[i]),
                Some(&self._render_context_light_probe._render_atmosphere_descriptor_sets[i]),
                Some(&render_atmosphere_push_constants)
            );

            // composite atmosphere for only sky
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                composite_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(&self._render_context_light_probe._composite_atmosphere_framebuffer_datas_only_sky[i]),
                Some(&self._render_context_light_probe._composite_atmosphere_descriptor_sets[i]),
                None,
            );

            // downsampling for only sky
            renderer_context.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
            let mip_level_descriptor_sets = &self._render_context_light_probe._only_sky_downsampling_descriptor_sets[i];
            let mip_levels = mip_level_descriptor_sets.len();
            for mip_level in 0..mip_levels {
                let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                renderer_context.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
            }
        }

        // render static object for light probe
        if constants::RENDER_OBJECT_FOR_LIGHT_PROBE {
            // copy light_probe_forward to light_probe_forward_prev
            self.copy_cube_map(
                renderer_context,
                command_buffer,
                swapchain_index,
                engine_resources,
                "copy_cube_map/copy",
                &self._render_context_light_probe._light_probe_forward_copy_descriptor_sets,
                constants::LIGHT_PROBE_SIZE,
                None
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
                renderer_context.render_material_instance(command_buffer, swapchain_index, "common/clear_framebuffer", CLEAR_LIGHT_PROBE_PIPELINES[i], &quad_geometry_data, None, None, None);

                // composite atmosphere
                renderer_context.render_render_pass_pipeline(
                    command_buffer,
                    swapchain_index,
                    composite_atmosphere_pipeline_binding_data,
                    quad_geometry_data,
                    Some(&self._render_context_light_probe._composite_atmosphere_framebuffer_datas[i]),
                    Some(&self._render_context_light_probe._composite_atmosphere_descriptor_sets[i]),
                    None,
                );

                // render forward for light probe
                const RENDER_FORWARD_RENDER_PASS_NAMES: [&str; 6] = [
                    "render_pass_static_forward_light_probe_0",
                    "render_pass_static_forward_light_probe_1",
                    "render_pass_static_forward_light_probe_2",
                    "render_pass_static_forward_light_probe_3",
                    "render_pass_static_forward_light_probe_4",
                    "render_pass_static_forward_light_probe_5",
                ];

                self.render_solid_object(
                    renderer_context,
                    command_buffer,
                    swapchain_index,
                    RENDER_FORWARD_RENDER_PASS_NAMES[i],
                    static_render_elements
                );

                // downsampling light probe
                renderer_context.begin_compute_pipeline(command_buffer, &downsampling_pipeline_binding_data.get_pipeline_data().borrow());
                let mip_level_descriptor_sets = &self._render_context_light_probe._light_probe_downsampling_descriptor_sets[i];
                let mip_levels = mip_level_descriptor_sets.len();
                for mip_level in 0..mip_levels {
                    let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                    renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, downsampling_pipeline_binding_data, descriptor_sets);
                    let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                    renderer_context.dispatch_compute_pipeline(command_buffer, dispatch_count, dispatch_count, 1);
                }
            }
        }
    }

    pub fn render_solid_object(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_pass_name: &str,
        render_elements: &Vec<RenderElementData>
    ) {
        if 0 == render_elements.len() {
            return;
        }

        unsafe {
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
            for render_element in render_elements.iter() {
                let material_instance = render_element._material_instance_data.borrow();
                let push_constant_datas = ptr_as_ref(render_element._push_constant_datas);
                let render_pass_pipeline_data_names = material_instance.get_render_pass_pipeline_data_names(render_pass_name);
                for render_pass_pipeline_data_name in render_pass_pipeline_data_names.iter() {
                    let pipeline_binding_data: *const PipelineBindingData = material_instance.get_pipeline_binding_data(&render_pass_pipeline_data_name);
                    let render_pass_data = &(*pipeline_binding_data).get_render_pass_data().borrow();
                    let pipeline_data = (*pipeline_binding_data).get_pipeline_data();
                    let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();
                    let pipeline_data: &PipelineData = &pipeline_data.borrow();

                    if prev_pipeline_data != pipeline_data_ptr {
                        if false == prev_pipeline_data.is_null() {
                            renderer_context.end_render_pass(command_buffer);
                        }
                        renderer_context.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
                        prev_pipeline_data = pipeline_data_ptr;
                    }

                    if prev_pipeline_binding_data != pipeline_binding_data {
                        prev_pipeline_binding_data = pipeline_binding_data;
                        renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, &(*pipeline_binding_data), None);
                    }

                    // update push constants
                    for push_constant_data in push_constant_datas.iter() {
                        renderer_context.upload_push_constant_data(
                            command_buffer,
                            pipeline_data,
                            push_constant_data._push_constant.as_ref()
                        );
                    }
                    renderer_context.draw_elements(command_buffer, &render_element._geometry_data.borrow());
                }
            }
            renderer_context.end_render_pass(command_buffer);
        }
    }

    // TEST_CODE
    fn render_ray_tracing(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        engine_resources: &EngineResources
    ) {
        // image barrier
        let scene_color = self.get_render_target(RenderTargetType::SceneColorCopy);
        let range = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        };
        let barrier = vk::ImageMemoryBarrier::builder()
            .src_access_mask(vk::AccessFlags::empty())
            .dst_access_mask(vk::AccessFlags::SHADER_WRITE)
            .old_layout(vk::ImageLayout::UNDEFINED)
            .new_layout(vk::ImageLayout::GENERAL)
            .image(scene_color._image)
            .subresource_range(range)
            .build();

        renderer_context.pipeline_barrier(
            command_buffer,
            vk::PipelineStageFlags::ALL_COMMANDS,
            vk::PipelineStageFlags::ALL_COMMANDS,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &[barrier],
        );

        let ray_tracing_properties = renderer_context.get_ray_tracing_properties();
        let material_instance = engine_resources.get_material_instance_data("ray_tracing/ray_tracing").borrow();
        let pipeline_binding_data = material_instance.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();

        if let Some(ref shader_binding_table) = pipeline_data._shader_binding_table {
            let handle_size = ray_tracing_properties.shader_group_handle_size as u64;

            // |[ raygen shader ]|[ hit shader  ]|[ miss shader ]|
            // |                 |               |               |
            // | 0               | 1             | 2             | 3

            let sbt_raygen_buffer = shader_binding_table._buffer;
            let sbt_raygen_offset = 0;

            let sbt_miss_buffer = shader_binding_table._buffer;
            let sbt_miss_offset = 2 * handle_size;
            let sbt_miss_stride = handle_size;

            let sbt_hit_buffer = shader_binding_table._buffer;
            let sbt_hit_offset = 1 * handle_size;
            let sbt_hit_stride = handle_size;

            let sbt_call_buffer = vk::Buffer::null();
            let sbt_call_offset = 0;
            let sbt_call_stride = 0;

            assert_eq!(vk::PipelineBindPoint::RAY_TRACING_NV, pipeline_binding_data.get_pipeline_bind_point(), "diff PipelineBindPoint");

            unsafe {
                renderer_context._device.cmd_bind_pipeline(
                    command_buffer,
                    pipeline_binding_data.get_pipeline_bind_point(),
                    pipeline_data._pipeline,
                );

                renderer_context.bind_descriptor_sets(
                    command_buffer,
                    swapchain_index,
                    pipeline_binding_data,
                    None
                );

                renderer_context._ray_tracing.cmd_trace_rays(
                    command_buffer,
                    sbt_raygen_buffer,
                    sbt_raygen_offset,
                    sbt_miss_buffer,
                    sbt_miss_offset,
                    sbt_miss_stride,
                    sbt_hit_buffer,
                    sbt_hit_offset,
                    sbt_hit_stride,
                    sbt_call_buffer,
                    sbt_call_offset,
                    sbt_call_stride,
                    scene_color._image_width,
                    scene_color._image_height,
                    1,
                )
            }
        }
    }

    pub fn render_translucent(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        engine_resources: &EngineResources
    ) {
        self.get_effect_manager().render_effects(command_buffer, swapchain_index, self, &engine_resources);
    }

    pub fn render_taa(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // render_taa
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_taa", DEFAULT_PIPELINE, &quad_geometry_data, None, None, None);

        // copy SceneColorCopy -> TAAResolve
        let framebuffer = Some(&self._render_context_taa._taa_resolve_framebuffer_data);
        let descriptor_sets = Some(&self._render_context_taa._taa_descriptor_sets);
        let push_constants = PushConstant_RenderCopy::default();
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_copy", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, Some(&push_constants));
    }

    pub fn render_bloom(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources
    ) {
        let render_bloom_material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data("common/render_bloom").borrow();
        // render_bloom_highlight
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
        renderer_context.render_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            quad_geometry_data,
            None,
            None,
            Some(&self._render_context_bloom._bloom_push_constants)
        );

        // render_bloom_downsampling
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let framebuffer_count = self._render_context_bloom._bloom_downsample_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._render_context_bloom._bloom_downsample_framebuffer_datas[i]),
                Some(&self._render_context_bloom._bloom_downsample_descriptor_sets[i]),
                None
            );
        }

        // render_gaussian_blur
        let render_gaussian_blur_material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data("common/render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance_data.get_default_pipeline_binding_data();
        let framebuffer_count = self._render_context_bloom._bloom_temp_framebuffer_datas.len();
        for i in 0..framebuffer_count {
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._render_context_bloom._bloom_temp_framebuffer_datas[i]),
                Some(&self._render_context_bloom._bloom_temp_descriptor_sets[i]),
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

    pub fn render_ssr(&self, renderer_context: &RendererContext, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // Screen Space Reflection
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_ssr", DEFAULT_PIPELINE, &quad_geometry_data, None, None, None);

        // Screen Space Reflection Resolve
        let (framebuffer, descriptor_sets) = match self._render_context_ssr._current_taa_resolved {
            RenderTargetType::SSRResolved => (Some(&self._render_context_ssr._framebuffer_data0), Some(&self._render_context_ssr._descriptor_sets0)),
            RenderTargetType::SSRResolvedPrev => (Some(&self._render_context_ssr._framebuffer_data1), Some(&self._render_context_ssr._descriptor_sets1)),
            _ => panic!("error")
        };
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_taa_simple", DEFAULT_PIPELINE, quad_geometry_data, framebuffer, descriptor_sets, None);
    }

    pub fn render_ssao(&self, renderer_context: &RendererContext, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        // render ssao
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_ssao", DEFAULT_PIPELINE, quad_geometry_data, None, None, None);

        // render ssao blur
        let framebuffer_h = Some(&self._render_context_ssao._ssao_blur_framebuffer_data0);
        let descriptor_sets_h = Some(&self._render_context_ssao._ssao_blur_descriptor_sets0);
        let framebuffer_v = Some(&self._render_context_ssao._ssao_blur_framebuffer_data1);
        let descriptor_sets_v = Some(&self._render_context_ssao._ssao_blur_descriptor_sets1);
        let push_constants_blur_h = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 0.0),
            ..Default::default()
        };
        let push_constants_blur_v = PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(0.0, 1.0),
            ..Default::default()
        };
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_h, descriptor_sets_h, Some(&push_constants_blur_h));
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_ssao_blur", DEFAULT_PIPELINE, quad_geometry_data, framebuffer_v, descriptor_sets_v, Some(&push_constants_blur_v));
    }

    pub fn composite_gbuffer(&self, renderer_context: &RendererContext, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let descriptor_sets = match self._render_context_ssr._current_taa_resolved {
            RenderTargetType::SSRResolved => Some(&self._render_context_composite_gbuffer._descriptor_sets0),
            RenderTargetType::SSRResolvedPrev => Some(&self._render_context_composite_gbuffer._descriptor_sets1),
            _ => panic!("error")
        };
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/composite_gbuffer", DEFAULT_PIPELINE, &quad_geometry_data, None, descriptor_sets, None);
    }

    pub fn generate_min_z(&self, renderer_context: &RendererContext, command_buffer: vk::CommandBuffer, swapchain_index: u32, quad_geometry_data: &GeometryData) {
        let engine_resources = renderer_context.get_engine_resources();

        // Copy Scene Depth
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/generate_min_z", "generate_min_z/render_copy", &quad_geometry_data, None, None, None);

        // Generate Hierachical Min Z
        let material_instance_data: Ref<MaterialInstanceData> = engine_resources.get_material_instance_data("common/generate_min_z").borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._render_context_hiz._descriptor_sets.len();
        renderer_context.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._render_context_hiz._descriptor_sets[mip_level as usize]);
            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            renderer_context.dispatch_compute_pipeline(
                command_buffer,
                self._render_context_hiz._dispatch_group_x >> (mip_level + 1),
                self._render_context_hiz._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn scene_color_downsampling(&self, command_buffer: vk::CommandBuffer, swapchain_index: u32, renderer_context: &RendererContext) {
        let engine_resources = renderer_context.get_engine_resources();
        let material_instance_data = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._render_context_scene_color_downsampling._descriptor_sets.len();
        renderer_context.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._render_context_scene_color_downsampling._descriptor_sets[mip_level as usize]);
            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
            renderer_context.dispatch_compute_pipeline(
                command_buffer,
                self._render_context_scene_color_downsampling._dispatch_group_x >> (mip_level + 1),
                self._render_context_scene_color_downsampling._dispatch_group_y >> (mip_level + 1),
                1
            );
        }
    }

    pub fn render_pre_process(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData
    ) {
        // Generate Hierachical Min Z
        self.generate_min_z(renderer_context, command_buffer, swapchain_index, quad_geometry_data);

        // Screen Space Reflection
        self.render_ssr(renderer_context, command_buffer, swapchain_index, quad_geometry_data);

        // SSAO
        self.render_ssao(renderer_context, command_buffer, swapchain_index, quad_geometry_data);

        // Composite GBuffer
        self.composite_gbuffer(renderer_context, command_buffer, swapchain_index, quad_geometry_data);

        // SceneColor Downsampling
        self.scene_color_downsampling(command_buffer, swapchain_index, renderer_context);
    }

    pub fn render_post_process(
        &self,
        renderer_context: &RendererContext,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        engine_resources: &EngineResources
    ) {
        // TAA
        self.render_taa(renderer_context, command_buffer, swapchain_index, quad_geometry_data);

        // Bloom
        self.render_bloom(command_buffer, swapchain_index, quad_geometry_data, renderer_context, engine_resources);

        // Motion Blur
        renderer_context.render_material_instance(command_buffer, swapchain_index, "common/render_motion_blur", DEFAULT_PIPELINE, &quad_geometry_data, None, None, None);
    }
}