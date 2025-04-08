use std::collections::HashMap;
use std::num::Wrapping;
use std::str::FromStr;
use std::vec::Vec;

use ash::{Device, vk};
use ash::ext;
use nalgebra::{Matrix4, Vector2, Vector4};
use crate::constants;
use crate::constants::MAX_FRAME_COUNT;
use crate::effect::effect_manager::EffectManager;
use crate::render_pass::common::{depth_prepass, render_forward_for_light_probe, render_gbuffer, render_shadow};
use crate::render_pass::ray_tracing::ray_tracing;
use crate::render_pass::render_pass;
use crate::renderer::push_constants::{
    PushConstant_BlendCubeMap, PushConstant_GaussianBlur, PushConstant_RenderCopy,
    PushConstant_RenderDebug,
};
use crate::renderer::render_context::{
    RenderContext_Bloom, RenderContext_ClearRenderTargets, RenderContext_CompositeGBuffer,
    RenderContext_HierarchicalMinZ, RenderContext_LightProbe, RenderContext_SceneColorDownSampling,
    RenderContext_ShadowAO, RenderContext_TAA, RenderContext_TAA_Simple,
};
use crate::renderer::render_target::{self, RenderTargetType};
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::shader_buffer_data::{self, ShaderBufferDataMap, ShaderBufferDataType};
use crate::renderer::utility;
use crate::resource::resource::EngineResources;
use crate::scene::camera::CameraObjectData;
use crate::scene::capture_height_map::CaptureHeightMap;
use crate::scene::debug_line::DebugLineManager;
use crate::scene::fft_ocean::FFTOcean;
use crate::scene::font::{FontManager, RenderTextInfo};
use crate::scene::light::DirectionalLight;
use crate::scene::material_instance::PipelineBindingData;
use crate::scene::precomputed_atmosphere::{Atmosphere, PushConstant_PrecomputedAtmosphere};
use crate::scene::render_element::RenderElementData;
use crate::scene::scene_manager::{BoundBoxInstanceData, SceneManager};
use crate::scene::ui::UIManager;
use crate::utilities::system::{ptr_as_mut, ptr_as_ref};
use crate::vulkan_context::buffer::{self, ShaderBufferData};
use crate::vulkan_context::debug_utils::ScopedDebugLabel;
use crate::vulkan_context::descriptor::DescriptorResourceInfo;
use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::vulkan_context::render_pass::{PipelineData, RenderPassDataCreateInfo};
use crate::vulkan_context::texture::{self, TextureData};
use crate::vulkan_context::vulkan_context::{self, MipLevels, SwapchainArray};

pub type RenderTargetDataMap = HashMap<RenderTargetType, TextureData>;

pub const DEFAULT_PIPELINE: &str = "";

// NOTE: Ensure enum values match in scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RenderMode {
    DepthPrepass = 0,
    GBuffer = 1,
    Forward = 2,
    Shadow = 3,
    CaptureHeightMap = 4,
}

// NOTE: Ensure enum values match in scene_constants.glsl
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum RenderObjectType {
    Static = 0,
    Skeletal = 1,
}

pub struct RendererData<'a> {
    pub _renderer_context: *const RendererContext<'a>,
    pub _engine_resources: *const EngineResources<'a>,
    pub _effect_manager: *const EffectManager<'a>,
    pub _scene_constants: shader_buffer_data::SceneConstants,
    pub _view_constants: shader_buffer_data::ViewConstants,
    pub _debug_render_target: RenderTargetType,
    pub _debug_render_target_layer: u32,
    pub _debug_render_target_miplevel: u32,
    pub _render_target_data_map: RenderTargetDataMap,
    pub _shader_buffer_data_map: ShaderBufferDataMap<'a>,
    pub _render_context_bloom: RenderContext_Bloom<'a>,
    pub _render_context_shadow_ao: RenderContext_ShadowAO,
    pub _render_context_taa: RenderContext_TAA<'a>,
    pub _render_context_hiz: RenderContext_HierarchicalMinZ,
    pub _render_context_scene_color_downsampling: RenderContext_SceneColorDownSampling,
    pub _render_context_ssr: RenderContext_TAA_Simple<'a>,
    pub _render_context_composite_gbuffer: RenderContext_CompositeGBuffer,
    pub _render_context_clear_render_targets: RenderContext_ClearRenderTargets<'a>,
    pub _render_context_light_probe: RenderContext_LightProbe<'a>,
    pub _fft_ocean: FFTOcean<'a>,
    pub _atmosphere: Atmosphere<'a>,
}

impl<'a> RendererData<'a> {
    pub fn initialize_renderer_data(
        &mut self,
        renderer_context: *const RendererContext<'a>,
        engine_resources: *const EngineResources<'a>,
        effect_manager: *const EffectManager<'a>,
    ) {
        self._renderer_context = renderer_context;
        self._engine_resources = engine_resources;
        self._effect_manager = effect_manager;

        let renderer_context_ref = ptr_as_ref(renderer_context);

        ShaderBufferDataType::register_shader_buffer_data_list(
            renderer_context_ref.get_device(),
            renderer_context_ref.get_device_memory_properties(),
            renderer_context_ref.get_debug_utils(),
            &mut self._shader_buffer_data_map,
        );

        self.create_render_targets(renderer_context_ref);
        if unsafe { constants::RENDER_OCEAN } {
            self.get_fft_ocean_mut()
                .register_fft_ocean_textures(renderer_context_ref, self.get_engine_resources_mut());
        }
    }

    pub fn prepare_framebuffer_and_descriptors(
        &mut self,
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        engine_resources: &EngineResources<'a>,
    ) {
        // Bloom
        self._render_context_bloom.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::Bloom0)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::BloomTemp0)
                .as_ref()
                .unwrap(),
        );

        // Temporal AA
        self._render_context_taa.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::PostProcessedColor)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::TAAResolve)
                .as_ref()
                .unwrap(),
        );

        // Hierarchical Min
        self._render_context_hiz.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::HierarchicalMinZ)
                .as_ref()
                .unwrap(),
        );

        // SceneColor Down sampling
        self._render_context_scene_color_downsampling.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::SceneColor)
                .as_ref()
                .unwrap(),
        );

        // SSR
        self._render_context_ssr.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::SSR)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::SSRResolved)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::SSRResolvedPrev)
                .as_ref()
                .unwrap(),
            RenderTargetType::SSRResolved,
            RenderTargetType::SSRResolvedPrev,
        );

        // Composite GBuffer
        self._render_context_composite_gbuffer.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(),
            self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap()
        );

        // RenderContext_LightProbe
        self._render_context_light_probe.initialize(
            device,
            debug_utils_device,
            engine_resources,
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeColor)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeColorOnlySky)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeColorOnlySkyPrev)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeColorForward)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeColorForwardPrev)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeAtmosphereColor)
                .as_ref()
                .unwrap(),
            self._render_target_data_map
                .get(&RenderTargetType::LightProbeAtmosphereInscatter)
                .as_ref()
                .unwrap(),
            &[
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants0)
                    .as_ref()
                    .unwrap(),
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants1)
                    .as_ref()
                    .unwrap(),
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants2)
                    .as_ref()
                    .unwrap(),
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants3)
                    .as_ref()
                    .unwrap(),
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants4)
                    .as_ref()
                    .unwrap(),
                &self
                    ._shader_buffer_data_map
                    .get(&ShaderBufferDataType::LightProbeViewConstants5)
                    .as_ref()
                    .unwrap(),
            ],
        );

        if unsafe { constants::RENDER_OCEAN } {
            self.get_fft_ocean_mut()
                .prepare_framebuffer_and_descriptors(self, self.get_engine_resources());
        }
        self.get_atmosphere_mut()
            .prepare_framebuffer_and_descriptors(self, self.get_engine_resources());

        // TEST CODE
        if self.get_renderer_context().get_use_ray_tracing() {
            log::info!(">>> TEST CODE: RayTracing create_descriptor_sets");
            let material_instance = engine_resources
                .get_material_instance_data("ray_tracing/ray_tracing")
                .borrow();
            let render_ray_tracing_pipeline_binding_data =
                material_instance.get_default_pipeline_binding_data();
            let top_level_descriptor_resource_info = self
                .get_renderer_context()
                ._ray_tracing_test_data
                .get_top_level_descriptor_resource_info();
            let _render_ray_tracing_descriptor_sets = utility::create_descriptor_sets_by_semantic(
                device,
                debug_utils_device,
                render_ray_tracing_pipeline_binding_data,
                &[(ray_tracing::SEMANTIC_TOP_LEVEL_ACCELERATION_STRUCTURE.to_string(), utility::create_swapchain_array(top_level_descriptor_resource_info.clone()))],
            );
        }

        // Last - Clear Render Targets
        self._render_context_clear_render_targets.initialize(
            device,
            debug_utils_device,
            engine_resources,
            &[
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySky).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorOnlySkyPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForward).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeColorForwardPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::Bloom0).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PostProcessedColor).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolved).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SSRResolvedPrev).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::TAAResolve).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_A).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::FFT_B).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::HierarchicalMinZ).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING).as_ref().unwrap(), vulkan_context::get_color_clear_zero()),
                (*self._render_target_data_map.get(&RenderTargetType::SceneDepth).as_ref().unwrap(), vulkan_context::get_default_depth_clear_value()),
                (*self._render_target_data_map.get(&RenderTargetType::Shadow).as_ref().unwrap(), vulkan_context::get_default_depth_clear_value()),
                (*self._render_target_data_map.get(&RenderTargetType::LightProbeDepth).as_ref().unwrap(), vulkan_context::get_default_depth_clear_value()),
            ]
        );
    }

    pub fn destroy_framebuffer_and_descriptors(&mut self, device: &Device) {
        if unsafe { constants::RENDER_OCEAN } {
            self.get_fft_ocean_mut()
                .destroy_fft_ocean(self.get_renderer_context().get_device());
        }
        self.get_atmosphere_mut()
            .destroy_atmosphere(self.get_renderer_context().get_device());
        self._render_context_bloom.destroy(device);
        self._render_context_taa.destroy(device);
        self._render_context_shadow_ao.destroy(device);
        self._render_context_hiz.destroy(device);
        self._render_context_scene_color_downsampling.destroy(device);
        self._render_context_ssr.destroy(device);
        self._render_context_composite_gbuffer.destroy(device);
        self._render_context_clear_render_targets.destroy(device);
        self._render_context_light_probe.destroy(device);
    }

    pub fn get_shader_buffer_data_from_str(&self, buffer_data_name: &str) -> &ShaderBufferData {
        self.get_shader_buffer_data(&ShaderBufferDataType::from_str(buffer_data_name).unwrap())
    }
    pub fn get_render_target_from_str(&self, render_target_type_str: &str) -> &TextureData {
        self.get_render_target(RenderTargetType::from_str(render_target_type_str).unwrap())
    }
    pub fn get_render_pass_data_create_infos(&self) -> Vec<RenderPassDataCreateInfo> {
        render_pass::get_render_pass_data_create_infos(self)
    }
    pub fn create_render_targets(&mut self, renderer_context: &RendererContext) {
        log::info!("create_render_targets");
        let render_target_create_infos =
            render_target::get_render_target_create_infos(renderer_context);
        for render_target_create_info in render_target_create_infos.iter() {
            let render_target_type: RenderTargetType =
                RenderTargetType::from_str(render_target_create_info._texture_name.as_str())
                    .unwrap();
            let texture_data = renderer_context.create_render_target(render_target_create_info);
            self._render_target_data_map
                .insert(render_target_type, texture_data);
        }
    }
    pub fn destroy_render_targets(&mut self, device: &Device) {
        for render_target_data in self._render_target_data_map.values() {
            texture::destroy_texture_data(device, render_target_data);
        }
        self._render_target_data_map.clear();
    }

    pub fn destroy_uniform_buffers(&mut self, device: &Device) {
        for shader_buffer_data in self._shader_buffer_data_map.values_mut() {
            buffer::destroy_shader_buffer_data(device, shader_buffer_data);
        }
        self._shader_buffer_data_map.clear();
    }

    pub fn pre_update_render_scene(&mut self, delta_time: f64) {
        self._atmosphere.update();
        if unsafe { constants::RENDER_OCEAN } {
            self._fft_ocean.update(delta_time);
        }
        self._render_context_shadow_ao.update();
        self._render_context_ssr.update();
    }

    pub fn upload_uniform_buffers(
        &mut self,
        command_buffer: vk::CommandBuffer,
        frame_index: usize,
        swapchain_index: u32,
        renderer_context: &RendererContext<'a>,
        scene_manager: &SceneManager,
        main_camera: &CameraObjectData,
        main_light: &DirectionalLight,
        capture_height_map: &CaptureHeightMap<'a>,
        elapsed_time: f64,
        delta_time: f64,
        elapsed_frame: u64,
    ) {
        // update constants
        self._scene_constants.update_scene_constants(
            renderer_context._swapchain_data._swapchain_extent.width,
            renderer_context._swapchain_data._swapchain_extent.height,
            elapsed_frame,
            elapsed_time,
            delta_time,
            self._fft_ocean.get_height(),
            self.get_effect_manager().get_gpu_particle_count_buffer_offset(frame_index),
            self.get_effect_manager().get_gpu_particle_update_buffer_offset(frame_index),
            scene_manager.get_render_point_light_count()
        );
        self._view_constants.update_view_constants(&main_camera, capture_height_map);

        // upload constants
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::SceneConstants,
            &self._scene_constants,
        );
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::ViewConstants,
            &self._view_constants,
        );
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::LightData,
            main_light.get_light_data(),
        );
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::PointLightData,
            scene_manager.get_render_point_lights(),
        );
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::ShadowAOConstants,
            &self._render_context_shadow_ao._shadow_ao_constants,
        );
        self.upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::AtmosphereConstants,
            &self._atmosphere._atmosphere_constants,
        );

        let transform_matrix_count = scene_manager.get_render_element_transform_count();
        if 0 < transform_matrix_count {
            let transform_matrices: &[Matrix4<f32>] = &scene_manager.get_render_element_transform_matrices()[..transform_matrix_count];
            self.upload_shader_buffer_data_list(
                command_buffer,
                swapchain_index,
                &ShaderBufferDataType::TransformMatrices,
                transform_matrices,
            );

            let transform_offsets: &[Vector4<i32>] = &scene_manager.get_render_element_transform_offsets()[..transform_matrix_count];
            self.upload_shader_buffer_data_list(
                command_buffer,
                swapchain_index,
                &ShaderBufferDataType::TransformOffsets,
                transform_offsets,
            );
        }
    }

    pub fn create_renderer_data() -> RendererData<'a> {
        RendererData {
            _renderer_context: std::ptr::null(),
            _engine_resources: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _scene_constants: shader_buffer_data::SceneConstants::default(),
            _view_constants: shader_buffer_data::ViewConstants::default(),
            _debug_render_target: RenderTargetType::BackBuffer,
            _debug_render_target_layer: 0,
            _debug_render_target_miplevel: 0,
            _render_target_data_map: RenderTargetDataMap::new(),
            _shader_buffer_data_map: ShaderBufferDataMap::new(),
            _render_context_bloom: RenderContext_Bloom::default(),
            _render_context_shadow_ao: RenderContext_ShadowAO::default(),
            _render_context_taa: RenderContext_TAA::default(),
            _render_context_hiz: RenderContext_HierarchicalMinZ::default(),
            _render_context_scene_color_downsampling: RenderContext_SceneColorDownSampling::default(),
            _render_context_ssr: RenderContext_TAA_Simple::default(),
            _render_context_composite_gbuffer: RenderContext_CompositeGBuffer::default(),
            _render_context_clear_render_targets: RenderContext_ClearRenderTargets::default(),
            _render_context_light_probe: RenderContext_LightProbe::default(),
            _fft_ocean: FFTOcean::default(),
            _atmosphere: Atmosphere::create_atmosphere(true),
        }
    }

    pub fn get_effect_manager(&self) -> &EffectManager<'a> {
        ptr_as_ref(self._effect_manager)
    }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager<'a> {
        ptr_as_mut(self._effect_manager)
    }
    pub fn get_renderer_context(&self) -> &RendererContext<'a> {
        ptr_as_ref(self._renderer_context)
    }
    pub fn get_renderer_context_mut(&self) -> &mut RendererContext<'a> {
        ptr_as_mut(self._renderer_context)
    }
    pub fn get_engine_resources(&self) -> &EngineResources<'a> {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources<'a> {
        ptr_as_mut(self._engine_resources)
    }
    pub fn get_fft_ocean_mut(&self) -> &mut FFTOcean<'a> {
        ptr_as_mut(&self._fft_ocean)
    }
    pub fn get_atmosphere_mut(&self) -> &mut Atmosphere<'a> {
        ptr_as_mut(&self._atmosphere)
    }
    pub fn get_shader_buffer_data(
        &self,
        buffer_data_type: &ShaderBufferDataType,
    ) -> &ShaderBufferData<'a> {
        &self._shader_buffer_data_map.get(buffer_data_type).unwrap()
    }

    pub fn next_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let next_enum_value: i32 = self._debug_render_target as i32 + 1;
        const MAX_BOUND: i32 = RenderTargetType::MaxBound as i32;
        self._debug_render_target = if next_enum_value < MAX_BOUND {
            unsafe { std::mem::transmute(next_enum_value) }
        } else {
            unsafe { std::mem::transmute(0) }
        };
        log::info!(
            "Current DebugRenderTarget: {:?} mip({})",
            self._debug_render_target,
            self._debug_render_target_miplevel
        );
    }

    pub fn prev_debug_render_target(&mut self) {
        self._debug_render_target_miplevel = 0;
        let enum_to_int: i32 = self._debug_render_target as i32;
        self._debug_render_target = if 0 == enum_to_int {
            unsafe { std::mem::transmute(RenderTargetType::MaxBound as i32 - 1) }
        } else {
            unsafe { std::mem::transmute(enum_to_int - 1) }
        };
        log::info!(
            "Current DebugRenderTarget: {:?} mip({})",
            self._debug_render_target,
            self._debug_render_target_miplevel
        );
    }

    pub fn next_debug_render_target_miplevel(&mut self) {
        let texture_data: &TextureData = self.get_render_target(self._debug_render_target);
        self._debug_render_target_miplevel = texture_data
            ._image_mip_levels
            .min(self._debug_render_target_miplevel + 1);
        log::info!(
            "Current DebugRenderTarget: {:?} mip({})",
            self._debug_render_target,
            self._debug_render_target_miplevel
        );
    }

    pub fn prev_debug_render_target_miplevel(&mut self) {
        if 0 < self._debug_render_target_miplevel {
            self._debug_render_target_miplevel -= 1;
        }
        log::info!(
            "Current DebugRenderTarget: {:?} mip({})",
            self._debug_render_target,
            self._debug_render_target_miplevel
        );
    }

    pub fn get_render_target(&self, render_target_type: RenderTargetType) -> &TextureData {
        &self
            ._render_target_data_map
            .get(&render_target_type)
            .unwrap()
    }

    pub fn upload_shader_buffer_data<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        shader_buffer_data_type: &ShaderBufferDataType,
        upload_data: &T,
    ) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context().upload_shader_buffer_data(
            command_buffer,
            swapchain_index,
            shader_buffer_data,
            upload_data,
        );
    }

    pub fn upload_shader_buffer_data_list<T: Copy>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        shader_buffer_data_type: &ShaderBufferDataType,
        upload_data: &[T],
    ) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context().upload_shader_buffer_data_list(
            command_buffer,
            swapchain_index,
            shader_buffer_data,
            upload_data,
        );
    }

    pub fn upload_shader_buffer_data_offset<T>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        shader_buffer_data_type: &ShaderBufferDataType,
        upload_data: &T,
        offset: vk::DeviceSize,
    ) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context()
            .upload_shader_buffer_data_offset(
                command_buffer,
                swapchain_index,
                shader_buffer_data,
                upload_data,
                offset,
            );
    }

    pub fn upload_shader_buffer_data_list_offset<T: Copy>(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        shader_buffer_data_type: &ShaderBufferDataType,
        upload_data: &[T],
        offset: vk::DeviceSize,
    ) {
        let shader_buffer_data = self.get_shader_buffer_data(shader_buffer_data_type);
        self.get_renderer_context()
            .upload_shader_buffer_data_list_offset(
                command_buffer,
                swapchain_index,
                shader_buffer_data,
                upload_data,
                offset,
            );
    }

    pub fn clear_render_targets(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext<'a>,
        engine_resources: &EngineResources<'a>,
        _quad_geometry_data: &GeometryData,
    ) {
        let _label_clear_render_targets = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "clear_render_targets",
        );
        let material_instance_data = engine_resources
            .get_material_instance_data("common/clear_render_target")
            .borrow();
        for (_, framebuffers) in self
            ._render_context_clear_render_targets
            ._color_framebuffer_data_list
            .iter()
        {
            let default_frame_buffer = &framebuffers[0][0];
            let mut render_pass_pipeline_name = String::from("clear");
            for attachment_format in default_frame_buffer
                ._framebuffer_info
                ._framebuffer_color_attachment_formats
                .iter()
            {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            for attachment_format in default_frame_buffer
                ._framebuffer_info
                ._framebuffer_depth_attachment_formats
                .iter()
            {
                render_pass_pipeline_name.push_str(&format!("_{:?}", attachment_format));
            }
            render_pass_pipeline_name.push_str("/clear");
            let pipeline_binding_data =
                material_instance_data.get_pipeline_binding_data(&render_pass_pipeline_name);
            for layer in 0..framebuffers.len() {
                for mip_level in 0..framebuffers[layer].len() {
                    renderer_context.begin_render_pass_pipeline(
                        command_buffer,
                        swapchain_index,
                        &pipeline_binding_data.get_render_pass_data().borrow(),
                        &pipeline_binding_data.get_pipeline_data().borrow(),
                        Some(&framebuffers[layer][mip_level]),
                    );
                    renderer_context.end_render_pass(command_buffer);
                }
            }
        }
    }

    pub fn copy_cube_map(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        engine_resources: &EngineResources<'a>,
        render_pass_pipeline_data_name: &str,
        mip_level_descriptor_sets: &MipLevels<SwapchainArray<vk::DescriptorSet>>,
        image_width: u32,
        push_constant_data: Option<&PushConstant_BlendCubeMap>,
    ) {
        let copy_cube_map_material_instance = engine_resources
            .get_material_instance_data("common/copy_cube_map")
            .borrow();
        let pipeline_binding_data = copy_cube_map_material_instance
            .get_pipeline_binding_data(render_pass_pipeline_data_name);
        let pipeline_data = pipeline_binding_data.get_pipeline_data().borrow();
        renderer_context.begin_compute_pipeline(command_buffer, &pipeline_data);
        let mip_levels = mip_level_descriptor_sets.len();
        for mip_level in 0..mip_levels {
            if let Some(push_constant_data) = push_constant_data {
                renderer_context.upload_push_constant_data(
                    command_buffer,
                    &pipeline_data,
                    push_constant_data,
                );
            }
            let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
            renderer_context.bind_descriptor_sets(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                descriptor_sets,
            );
            let dispatch_count: u32 = (Wrapping(image_width) >> mip_level).0;
            renderer_context.dispatch_compute_pipeline(
                command_buffer,
                dispatch_count,
                dispatch_count,
                1,
            );
        }
    }

    pub fn reset_render_light_probe_time(&mut self) {
        self._render_context_light_probe._next_refresh_time = 0.0;
    }

    pub fn render_light_probe(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        engine_resources: &EngineResources<'a>,
        scene_manager: &SceneManager,
        main_camera: &CameraObjectData,
        capture_height_map: &CaptureHeightMap<'a>,
        static_render_elements: &Vec<RenderElementData>,
    ) {
        let material_instance_data = engine_resources.get_material_instance_data("precomputed_atmosphere/precomputed_atmosphere").borrow();
        let render_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_atmosphere/default");
        let composite_atmosphere_pipeline_binding_data = material_instance_data.get_pipeline_binding_data("composite_atmosphere/default");
        let downsampling_material_instance = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let downsampling_pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let mut light_probe_view_constants = shader_buffer_data::ViewConstants::default();
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
            &self
                ._render_context_light_probe
                ._only_sky_copy_descriptor_sets,
            constants::LIGHT_PROBE_SIZE,
            None,
        );

        // render atmosphere, inscatter
        for layer_index in 0..constants::CUBE_LAYER_COUNT {
            let mut light_probe_camera = scene_manager
                .get_light_probe_camera(layer_index)
                .borrow_mut();
            light_probe_camera
                ._transform_object
                .set_position(main_camera_position);
            light_probe_camera.update_camera_object_data();
            light_probe_view_constants.update_view_constants(&light_probe_camera, capture_height_map);
            self.upload_shader_buffer_data(
                command_buffer,
                swapchain_index,
                &light_probe_view_constant_types[layer_index].clone(),
                &light_probe_view_constants,
            );

            // render atmosphere
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                render_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(
                    &self
                        ._render_context_light_probe
                        ._render_atmosphere_framebuffer_data_list[layer_index],
                ),
                Some(
                    &self
                        ._render_context_light_probe
                        ._render_atmosphere_descriptor_sets[layer_index],
                ),
                Some(&render_atmosphere_push_constants),
            );

            // composite atmosphere for only sky
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                composite_atmosphere_pipeline_binding_data,
                quad_geometry_data,
                Some(
                    &self
                        ._render_context_light_probe
                        ._composite_atmosphere_framebuffer_data_list_only_sky[layer_index],
                ),
                Some(
                    &self
                        ._render_context_light_probe
                        ._composite_atmosphere_descriptor_sets[layer_index],
                ),
                None,
            );

            // downsampling for only sky
            renderer_context.begin_compute_pipeline(
                command_buffer,
                &downsampling_pipeline_binding_data
                    .get_pipeline_data()
                    .borrow(),
            );
            let mip_level_descriptor_sets = &self
                ._render_context_light_probe
                ._only_sky_downsampling_descriptor_sets[layer_index];
            let mip_levels = mip_level_descriptor_sets.len();
            for mip_level in 0..mip_levels {
                let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                renderer_context.bind_descriptor_sets(
                    command_buffer,
                    swapchain_index,
                    downsampling_pipeline_binding_data,
                    descriptor_sets,
                );
                let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                renderer_context.dispatch_compute_pipeline(
                    command_buffer,
                    dispatch_count,
                    dispatch_count,
                    1,
                );
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
                &self
                    ._render_context_light_probe
                    ._light_probe_forward_copy_descriptor_sets,
                constants::LIGHT_PROBE_SIZE,
                None,
            );

            for layer_index in 0..constants::CUBE_LAYER_COUNT {
                // clear light probe depth
                const CLEAR_LIGHT_PROBE_PIPELINES: [&str; 6] = [
                    "clear_light_probe_depth_0/clear",
                    "clear_light_probe_depth_1/clear",
                    "clear_light_probe_depth_2/clear",
                    "clear_light_probe_depth_3/clear",
                    "clear_light_probe_depth_4/clear",
                    "clear_light_probe_depth_5/clear",
                ];
                renderer_context.render_material_instance(
                    command_buffer,
                    swapchain_index,
                    "common/clear_framebuffer",
                    CLEAR_LIGHT_PROBE_PIPELINES[layer_index],
                    &quad_geometry_data,
                    None,
                    None,
                    None,
                );

                // composite atmosphere
                renderer_context.render_render_pass_pipeline(
                    command_buffer,
                    swapchain_index,
                    composite_atmosphere_pipeline_binding_data,
                    quad_geometry_data,
                    Some(
                        &self
                            ._render_context_light_probe
                            ._composite_atmosphere_framebuffer_data_list[layer_index],
                    ),
                    Some(
                        &self
                            ._render_context_light_probe
                            ._composite_atmosphere_descriptor_sets[layer_index],
                    ),
                    None,
                );

                // render forward for light probe
                self.render_solid_object(
                    renderer_context,
                    command_buffer,
                    swapchain_index,
                    render_forward_for_light_probe::get_render_pass_name(RenderObjectType::Static, layer_index as u32),
                    static_render_elements,
                );

                // downsampling light probe
                renderer_context.begin_compute_pipeline(
                    command_buffer,
                    &downsampling_pipeline_binding_data
                        .get_pipeline_data()
                        .borrow(),
                );
                let mip_level_descriptor_sets = &self
                    ._render_context_light_probe
                    ._light_probe_downsampling_descriptor_sets[layer_index];
                let mip_levels = mip_level_descriptor_sets.len();
                for mip_level in 0..mip_levels {
                    let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                    renderer_context.bind_descriptor_sets(
                        command_buffer,
                        swapchain_index,
                        downsampling_pipeline_binding_data,
                        descriptor_sets,
                    );
                    let dispatch_count = constants::LIGHT_PROBE_SIZE >> (mip_level + 1);
                    renderer_context.dispatch_compute_pipeline(
                        command_buffer,
                        dispatch_count,
                        dispatch_count,
                        1,
                    );
                }
            }
        }
    }

    pub fn render_solid_object(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        render_pass_name: &str,
        render_elements: &Vec<RenderElementData>,
    ) {
        if 0 == render_elements.len() {
            return;
        }

        let _label_render_solid_object = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            render_pass_name,
        );

        unsafe {
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
            for render_element in render_elements.iter() {
                let material_instance = render_element._material_instance_data.borrow();
                let render_pass_pipeline_data_names =
                    material_instance.get_render_pass_pipeline_data_names(render_pass_name);
                for render_pass_pipeline_data_name in render_pass_pipeline_data_names.iter() {
                    let pipeline_binding_data: *const PipelineBindingData = material_instance
                        .get_pipeline_binding_data(&render_pass_pipeline_data_name);
                    let render_pass_data =
                        &(*pipeline_binding_data).get_render_pass_data().borrow();
                    let pipeline_data = (*pipeline_binding_data).get_pipeline_data();
                    let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();
                    let pipeline_data: &PipelineData = &pipeline_data.borrow();

                    if prev_pipeline_data != pipeline_data_ptr {
                        if false == prev_pipeline_data.is_null() {
                            renderer_context.end_render_pass(command_buffer);
                        }
                        renderer_context.begin_render_pass_pipeline(
                            command_buffer,
                            swapchain_index,
                            render_pass_data,
                            pipeline_data,
                            None,
                        );
                        prev_pipeline_data = pipeline_data_ptr;
                    }

                    if prev_pipeline_binding_data != pipeline_binding_data {
                        prev_pipeline_binding_data = pipeline_binding_data;
                        renderer_context.bind_descriptor_sets(
                            command_buffer,
                            swapchain_index,
                            &(*pipeline_binding_data),
                            None,
                        );
                    }

                    // upload push constants
                    for push_constant_data in render_element._push_constant_data_list.iter() {
                        renderer_context.upload_push_constant_data(
                            command_buffer,
                            pipeline_data,
                            push_constant_data._push_constant.as_ref(),
                        );
                    }

                    renderer_context.draw_elements_instanced(
                        command_buffer,
                        &render_element._geometry_data.borrow(),
                        &[],
                        render_element._num_render_instances,
                    )
                }
            }
            renderer_context.end_render_pass(command_buffer);
        }
    }

    // TEST_CODE
    fn render_ray_tracing(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        engine_resources: &EngineResources<'a>,
    ) {
        // image barrier
        let scene_color = self.get_render_target(RenderTargetType::PostProcessedColor);
        let range = vk::ImageSubresourceRange {
            aspect_mask: vk::ImageAspectFlags::COLOR,
            base_mip_level: 0,
            level_count: 1,
            base_array_layer: 0,
            layer_count: 1,
        };
        let barrier = vk::ImageMemoryBarrier::default()
            .src_access_mask(vk::AccessFlags::empty())
            .dst_access_mask(vk::AccessFlags::SHADER_WRITE)
            .old_layout(vk::ImageLayout::UNDEFINED)
            .new_layout(vk::ImageLayout::GENERAL)
            .image(scene_color._image)
            .subresource_range(range);

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
        let material_instance = engine_resources
            .get_material_instance_data("ray_tracing/ray_tracing")
            .borrow();
        let pipeline_binding_data = material_instance.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();

        if let Some(ref shader_binding_table) = pipeline_data._shader_binding_table {
            let handle_size = ray_tracing_properties.shader_group_handle_size as u64;
            //let handle_size = ray_tracing_properties.shader_group_base_alignment as u64;

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

            assert_eq!(
                vk::PipelineBindPoint::RAY_TRACING_NV,
                pipeline_binding_data.get_pipeline_bind_point(),
                "diff PipelineBindPoint"
            );

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
                    None,
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
        engine_resources: &EngineResources<'a>,
    ) {
        self.get_effect_manager().render_effects(
            command_buffer,
            swapchain_index,
            self,
            &engine_resources,
        );
    }

    pub fn render_bound_box(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext<'a>,
        engine_resources: &EngineResources<'a>,
        geometry_data: &GeometryData,
        bound_box_matrices: &Vec<BoundBoxInstanceData>
    ) {
        let instance_count = constants::MAX_BOUND_BOX_INSTANCE_COUNT.min(bound_box_matrices.len()) as u32;
        if 0 < instance_count {
            let material_instance_data = engine_resources
                .get_material_instance_data("common/render_bound_box")
                .borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let descriptor_sets = Some(&pipeline_binding_data._descriptor_sets);

            // upload storage buffer
            renderer_context.upload_shader_buffer_data_list(
                command_buffer,
                swapchain_index,
                renderer_context.get_shader_buffer_data_from_str("BoundBoxInstanceDataBuffer"),
                &bound_box_matrices,
            );

            // render
            renderer_context.render_render_pass_pipeline_instanced(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                geometry_data,
                &[],
                instance_count,
                None,
                descriptor_sets,
                None,
            );
        }
    }

    pub fn render_taa(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        // render_taa
        let _label_render_taa = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_taa",
        );
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_taa",
            DEFAULT_PIPELINE,
            &quad_geometry_data,
            None,
            None,
            None,
        );

        // copy PostProcessedColor -> TAAResolve
        let framebuffer = Some(&self._render_context_taa._taa_resolve_framebuffer_data);
        let descriptor_sets = Some(&self._render_context_taa._taa_descriptor_sets);
        let push_constants = PushConstant_RenderCopy::default();
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_copy",
            DEFAULT_PIPELINE,
            quad_geometry_data,
            framebuffer,
            descriptor_sets,
            Some(&push_constants),
        );
    }

    pub fn render_bloom(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext<'a>,
        engine_resources: &EngineResources<'a>,
    ) {
        let _label_render_bloom = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_bloom",
        );
        // render_bloom_highlight
        let render_bloom_material_instance_data = engine_resources.get_material_instance_data("common/render_bloom").borrow();
        let pipeline_binding_data = render_bloom_material_instance_data.get_pipeline_binding_data("render_bloom/render_bloom_highlight");
        renderer_context.render_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            quad_geometry_data,
            None,
            None,
            Some(&self._render_context_bloom._bloom_push_constants),
        );

        // render_bloom_downsampling
        let pipeline_binding_data = render_bloom_material_instance_data
            .get_pipeline_binding_data("render_bloom/render_bloom_downsampling");
        let framebuffer_count = self
            ._render_context_bloom
            ._bloom_downsample_framebuffer_data_list
            .len();
        for i in 0..framebuffer_count {
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(
                    &self
                        ._render_context_bloom
                        ._bloom_downsample_framebuffer_data_list[i],
                ),
                Some(&self._render_context_bloom._bloom_downsample_descriptor_sets[i]),
                None,
            );
        }

        // render_gaussian_blur
        let render_gaussian_blur_material_instance_data = engine_resources.get_material_instance_data("common/render_gaussian_blur").borrow();
        let pipeline_binding_data = render_gaussian_blur_material_instance_data.get_default_pipeline_binding_data();
        let framebuffer_count = self
            ._render_context_bloom
            ._bloom_temp_framebuffer_data_list
            .len();
        for i in 0..framebuffer_count {
            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                quad_geometry_data,
                Some(&self._render_context_bloom._bloom_temp_framebuffer_data_list[i]),
                Some(&self._render_context_bloom._bloom_temp_descriptor_sets[i]),
                Some(&PushConstant_GaussianBlur {
                    _blur_scale: if 0 == (i % 2) {
                        Vector2::new(1.0, 0.0)
                    } else {
                        Vector2::new(0.0, 1.0)
                    },
                    ..Default::default()
                }),
            );
        }
    }

    pub fn render_ssr(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        // Screen Space Reflection
        let _label_render_ssr = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_ssr",
        );
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_ssr",
            DEFAULT_PIPELINE,
            &quad_geometry_data,
            None,
            None,
            None,
        );

        // Resolve Screen Space Reflection
        let (framebuffer, descriptor_sets) = match self._render_context_ssr._current_taa_resolved {
            RenderTargetType::SSRResolved => (
                Some(&self._render_context_ssr._framebuffer_data0),
                Some(&self._render_context_ssr._descriptor_sets0),
            ),
            RenderTargetType::SSRResolvedPrev => (
                Some(&self._render_context_ssr._framebuffer_data1),
                Some(&self._render_context_ssr._descriptor_sets1),
            ),
            _ => panic!("error"),
        };
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_taa",
            DEFAULT_PIPELINE,
            quad_geometry_data,
            framebuffer,
            descriptor_sets,
            None,
        );
    }

    pub fn render_shadow_ao(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        // render shadow ao
        let _label_render_shadow_ao = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_shadow_ao",
        );

        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_shadow_ao",
            DEFAULT_PIPELINE,
            quad_geometry_data,
            None,
            None,
            None,
        );
    }

    pub fn composite_gbuffer(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        let _label_composite_gbuffer = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "composite_gbuffer",
        );
        let descriptor_sets = match self._render_context_ssr._current_taa_resolved {
            RenderTargetType::SSRResolved => {
                Some(&self._render_context_composite_gbuffer._descriptor_sets0)
            }
            RenderTargetType::SSRResolvedPrev => {
                Some(&self._render_context_composite_gbuffer._descriptor_sets1)
            }
            _ => panic!("error"),
        };
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/composite_gbuffer",
            DEFAULT_PIPELINE,
            &quad_geometry_data,
            None,
            descriptor_sets,
            None,
        );
    }

    pub fn generate_min_z(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        let _label_generate_min_z = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "generate_min_z",
        );
        let engine_resources = renderer_context.get_engine_resources();

        // Copy Scene Depth
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/generate_min_z",
            "generate_min_z/render_copy",
            &quad_geometry_data,
            None,
            None,
            None,
        );

        // Generate Hierarchical Min Z
        let material_instance_data = engine_resources.get_material_instance_data("common/generate_min_z").borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("generate_min_z/generate_min_z");
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self._render_context_hiz._descriptor_sets.len();
        renderer_context.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(&self._render_context_hiz._descriptor_sets[mip_level]);
            renderer_context.bind_descriptor_sets(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                descriptor_sets,
            );
            renderer_context.dispatch_compute_pipeline(
                command_buffer,
                self._render_context_hiz._dispatch_group_x >> (mip_level + 1),
                self._render_context_hiz._dispatch_group_y >> (mip_level + 1),
                1,
            );
        }
    }

    pub fn scene_color_downsampling(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext<'a>,
    ) {
        let _label_scene_color_downsampling = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "scene_color_downsampling",
        );
        let engine_resources = renderer_context.get_engine_resources();
        let material_instance_data = engine_resources
            .get_material_instance_data("common/downsampling")
            .borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        let dispatch_count = self
            ._render_context_scene_color_downsampling
            ._descriptor_sets
            .len();
        renderer_context.begin_compute_pipeline(command_buffer, pipeline_data);
        for mip_level in 0..dispatch_count {
            let descriptor_sets = Some(
                &self
                    ._render_context_scene_color_downsampling
                    ._descriptor_sets[mip_level],
            );
            renderer_context.bind_descriptor_sets(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                descriptor_sets,
            );
            renderer_context.dispatch_compute_pipeline(
                command_buffer,
                self._render_context_scene_color_downsampling
                    ._dispatch_group_x
                    >> (mip_level + 1),
                self._render_context_scene_color_downsampling
                    ._dispatch_group_y
                    >> (mip_level + 1),
                1,
            );
        }
    }

    pub fn render_pre_process(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
    ) {
        // Generate Hierarchical Min Z
        self.generate_min_z(
            renderer_context,
            command_buffer,
            swapchain_index,
            quad_geometry_data,
        );

        // Screen Space Reflection
        self.render_ssr(
            renderer_context,
            command_buffer,
            swapchain_index,
            quad_geometry_data,
        );

        // ShadowAO
        self.render_shadow_ao(
            renderer_context,
            command_buffer,
            swapchain_index,
            quad_geometry_data,
        );

        // Composite GBuffer
        self.composite_gbuffer(
            renderer_context,
            command_buffer,
            swapchain_index,
            quad_geometry_data,
        );

        // SceneColor Downsampling
        self.scene_color_downsampling(command_buffer, swapchain_index, renderer_context);
    }

    pub fn render_post_process(
        &self,
        renderer_context: &RendererContext<'a>,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        engine_resources: &EngineResources<'a>,
    ) {
        // TAA
        self.render_taa(
            renderer_context,
            command_buffer,
            swapchain_index,
            quad_geometry_data,
        );

        // Bloom
        self.render_bloom(
            command_buffer,
            swapchain_index,
            quad_geometry_data,
            renderer_context,
            engine_resources,
        );

        // Motion Blur
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_motion_blur",
            DEFAULT_PIPELINE,
            &quad_geometry_data,
            None,
            None,
            None,
        );
    }

    pub fn render_scene(
        &mut self,
        command_buffer: vk::CommandBuffer,
        frame_index: usize,
        swapchain_index: u32,
        renderer_context: &RendererContext<'a>,
        scene_manager: &mut SceneManager<'a>,
        debug_line_manager: &mut DebugLineManager,
        font_manager: &mut FontManager,
        ui_manager: &mut UIManager,
        elapsed_time: f64,
        delta_time: f64,
        elapsed_frame: u64,
    ) {
        let _label_render_scene = ScopedDebugLabel::create_scoped_cmd_label(
            renderer_context.get_debug_utils(),
            command_buffer,
            "render_scene",
        );

        let engine_resources = renderer_context.get_engine_resources();
        let main_camera = scene_manager.get_main_camera();
        let main_light = scene_manager.get_main_light().borrow();
        let cube_mesh = engine_resources.get_mesh_data("cube").borrow();
        let cube_geometry_data = cube_mesh.get_default_geometry_data().borrow();
        let quad_mesh = engine_resources.get_mesh_data("quad").borrow();
        let quad_geometry_data = quad_mesh.get_default_geometry_data().borrow();
        let static_render_elements = scene_manager.get_static_render_elements();
        let static_shadow_render_elements = scene_manager.get_static_shadow_render_elements();
        let skeletal_render_elements = scene_manager.get_skeletal_render_elements();
        let skeletal_shadow_render_elements = scene_manager.get_skeletal_shadow_render_elements();
        let capture_height_map = scene_manager.get_capture_height_map_mut();

        // pre update
        self.pre_update_render_scene(delta_time);

        // Upload Uniform Buffers
        {
            let _label_upload_uniform_buffers = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "upload_uniform_buffers",
            );

            self.upload_uniform_buffers(
                command_buffer,
                frame_index,
                swapchain_index,
                renderer_context,
                scene_manager,
                &main_camera,
                &main_light,
                capture_height_map,
                elapsed_time,
                delta_time,
                elapsed_frame,
            );
        }

        if renderer_context.is_first_rendering() {
            let _label_precompute_environment = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "precompute_environment",
            );
            self.clear_render_targets(
                command_buffer,
                swapchain_index,
                renderer_context,
                &engine_resources,
                &quad_geometry_data,
            );
            if unsafe { constants::RENDER_OCEAN } {
                self._fft_ocean.compute_slope_variance_texture(
                    command_buffer,
                    swapchain_index,
                    &quad_geometry_data,
                    renderer_context,
                    &engine_resources,
                );
            }
            self._atmosphere.precompute(
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                renderer_context,
            );
        }

        // capture height map
        {
            if capture_height_map.need_to_render_height_map() {
                capture_height_map.render_capture_height_map(
                    command_buffer,
                    swapchain_index,
                    &quad_geometry_data,
                    renderer_context,
                    self
                );
            }

            if capture_height_map.need_to_read_back_height_map(renderer_context.get_render_frame()) {
                capture_height_map.read_back_height_map(
                    command_buffer,
                    renderer_context,
                    self
                );
            }
        }


        // clear gbuffer
        {
            let _label_clear_gbuffer = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "clear_gbuffer",
            );
            renderer_context.render_material_instance(
                command_buffer,
                swapchain_index,
                "common/clear_framebuffer",
                "clear_gbuffer/clear",
                &quad_geometry_data,
                None,
                None,
                None,
            );
        }

        // depth prepass solid object
        {
            let _label_render_solid_object = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "depth_prepass_object",
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                depth_prepass::get_render_pass_name(RenderObjectType::Static),
                &static_render_elements,
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                depth_prepass::get_render_pass_name(RenderObjectType::Skeletal),
                &skeletal_render_elements,
            );
        }

        // render shadow
        {
            let _label_render_shadow = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render shadow",
            );
            renderer_context.render_material_instance(
                command_buffer,
                swapchain_index,
                "common/clear_framebuffer",
                "clear_shadow/clear",
                &quad_geometry_data,
                None,
                None,
                None,
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                render_shadow::get_render_pass_name(RenderObjectType::Static),
                &static_shadow_render_elements,
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                render_shadow::get_render_pass_name(RenderObjectType::Skeletal),
                &skeletal_shadow_render_elements,
            );
        }

        // fft-simulation
        if unsafe { constants::RENDER_OCEAN } {
            let _label_fft_simulation = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "fft_simulation",
            );
            self._fft_ocean.simulate_fft_waves(
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                renderer_context,
                &engine_resources,
            );
        }

        // light probe
        if self._render_context_light_probe._next_refresh_time <= elapsed_time ||
            self._render_context_light_probe._light_probe_capture_count < MAX_FRAME_COUNT
        {
            let _label_render_light_probe = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_light_probe",
            );
            self.render_light_probe(
                renderer_context,
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                &engine_resources,
                scene_manager,
                &main_camera,
                capture_height_map,
                static_render_elements,
            );
            self._render_context_light_probe._next_refresh_time = elapsed_time + self._render_context_light_probe._light_probe_refresh_term;
            self._render_context_light_probe._light_probe_blend_time = 0.0;
            self._render_context_light_probe._light_probe_capture_count += 1;
        }

        let light_probe_term = self
            ._render_context_light_probe
            ._light_probe_blend_term
            .min(self._render_context_light_probe._light_probe_refresh_term);
        if self._render_context_light_probe._light_probe_blend_time < light_probe_term {
            let _label_copy_cube_map = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "copy_cube_map",
            );
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
                }),
            );
        }

        // render solid object
        {
            let _label_render_solid_object = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_solid_object",
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                render_gbuffer::get_render_pass_name(RenderObjectType::Static),
                &static_render_elements,
            );
            self.render_solid_object(
                renderer_context,
                command_buffer,
                swapchain_index,
                render_gbuffer::get_render_pass_name(RenderObjectType::Skeletal),
                &skeletal_render_elements,
            );
        }

        // process gpu particles
        {
            let _label_process_gpu_particles = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "process_gpu_particles",
            );
            let effect_manager = self.get_effect_manager_mut();
            if effect_manager.get_need_to_clear_gpu_particle_buffer() {
                effect_manager.clear_gpu_particles(
                    command_buffer,
                    swapchain_index,
                    renderer_context,
                    &engine_resources,
                );
                effect_manager.set_need_to_clear_gpu_particle_buffer(false);
            }
            effect_manager.process_gpu_particles(
                command_buffer,
                swapchain_index,
                self,
                &engine_resources,
            );
        }

        // pre-process: min-z, ssr, shadow ao, gbuffer, downsampling scene color
        {
            let _label_pre_process = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "pre_process",
            );
            self.render_pre_process(
                renderer_context,
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
            );
        }

        // render ocean
        if unsafe { constants::RENDER_OCEAN } {
            let _label_render_ocean = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_ocean",
            );
            self._fft_ocean.render_ocean(
                command_buffer,
                swapchain_index,
                &renderer_context,
                &engine_resources,
            );
        }

        // render atmosphere
        {
            let _label_render_atmosphere = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_atmosphere",
            );
            let render_light_probe_mode: bool = false;
            self._atmosphere.render_precomputed_atmosphere(
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                &renderer_context,
                render_light_probe_mode,
            );
        }

        // render translucent
        {
            let _label_render_translucent = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_translucent",
            );
            self.render_translucent(command_buffer, swapchain_index, &engine_resources);
        }

        // TEST_CODE: ray tracing test
        if renderer_context.get_use_ray_tracing() {
            let _label_render_ray_tracing = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_ray_tracing",
            );
            self.render_ray_tracing(
                renderer_context,
                command_buffer,
                swapchain_index,
                &engine_resources,
            );
        }

        // post-process: taa, bloom, motion blur
        {
            let _label_render_post_process = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_post_process",
            );
            self.render_post_process(
                renderer_context,
                command_buffer,
                swapchain_index,
                &quad_geometry_data,
                &engine_resources,
            );
        }

        // Render Bound Box
        if unsafe { constants::RENDER_BOUND_BOX } {
            let bound_box_matrices = scene_manager.get_bound_boxes();
            let _label_render_debug_line = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_bound_box",
            );

            self.render_bound_box(
                command_buffer,
                swapchain_index,
                &renderer_context,
                &engine_resources,
                &cube_geometry_data,
                &bound_box_matrices
            );
        }

        // Render Final
        {
            let _label_render_final = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_final",
            );
            renderer_context.render_material_instance(
                command_buffer,
                swapchain_index,
                "common/render_final",
                DEFAULT_PIPELINE,
                &quad_geometry_data,
                None,
                None,
                None,
            );
        }

        // Render Debug Line
        {
            let _label_render_debug_line = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_debug_line",
            );

            debug_line_manager.render_debug_line(
                command_buffer,
                swapchain_index,
                &renderer_context,
                &engine_resources,
            );
        }

        // Render UI
        {
            let _label_render_ui = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_ui",
            );
            ui_manager.render_ui(
                command_buffer,
                swapchain_index,
                &renderer_context,
                &engine_resources,
            );
        }

        // Render Debug
        if RenderTargetType::BackBuffer != self._debug_render_target {
            let _label_render_debug = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_debug",
            );
            let mut render_debug_material_instance_data = engine_resources.get_material_instance_data(&"common/render_debug").borrow_mut();
            let mut render_debug_pipeline_binding_data = render_debug_material_instance_data.get_default_pipeline_binding_data_mut();
            renderer_context.begin_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                &render_debug_pipeline_binding_data.get_render_pass_data().borrow(),
                &render_debug_pipeline_binding_data.get_pipeline_data().borrow(),
                None,
            );

            let debug_texture_data = self.get_render_target(self._debug_render_target);
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
                &DescriptorResourceInfo::DescriptorImageInfo(
                    debug_texture_data.get_default_image_info(),
                ),
            );

            renderer_context.upload_push_constant_data(
                command_buffer,
                &render_debug_pipeline_binding_data
                    .get_pipeline_data()
                    .borrow(),
                &PushConstant_RenderDebug {
                    _debug_target: debug_texture_data.get_image_view_type().as_raw() as u32,
                    _mip_level: self._debug_render_target_miplevel,
                    ..Default::default()
                },
            );

            renderer_context.bind_descriptor_sets(
                command_buffer,
                swapchain_index,
                &render_debug_pipeline_binding_data,
                None,
            );
            renderer_context.draw_elements(command_buffer, &quad_geometry_data);
            renderer_context.end_render_pass(command_buffer);
        }

        // Render Text
        {
            let _label_render_text = ScopedDebugLabel::create_scoped_cmd_label(
                renderer_context.get_debug_utils(),
                command_buffer,
                "render_text",
            );
            let render_text_info = RenderTextInfo {
                _render_font_size: 16,
                _initial_column: 0,
                _initial_row: 0,
                _render_text_offset: Vector2::new(10.0, 10.0),
            };
            font_manager.render_text(
                command_buffer,
                swapchain_index,
                &renderer_context,
                &engine_resources,
                &render_text_info,
            );
        }
    }
}
