use ash::vk;
use nalgebra::{ Vector3, Vector4, Matrix4 };

use rust_engine_3d::constants::{
    MAX_EMITTER_COUNT,
    MAX_PARTICLE_COUNT,
    PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE,
};
use rust_engine_3d::renderer::effect::*;
use rust_engine_3d::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use rust_engine_3d::renderer::renderer::RendererData;
use rust_engine_3d::resource::resource::Resources;
use rust_engine_3d::vulkan_context::render_pass::{ RenderPassData, PipelineData };

use crate::renderer::push_constants::NONE_PUSH_CONSTANT;
use crate::renderer::renderer::Renderer;
use crate::renderer::shader_buffer_datas::ShaderBufferDataType;

// shader storage buffer
#[derive(Debug, Clone, Copy, Default)]
pub struct GpuParticleStaticConstants {
    pub _spawn_volume_transform: Matrix4<f32>,
    pub _spawn_volume_info: Vector4<f32>,
    pub _rotation_min: Vector3<f32>,
    pub _particle_lifetime_min: f32,
    pub _rotation_max: Vector3<f32>,
    pub _particle_lifetime_max: f32,
    pub _scale_min: Vector3<f32>,
    pub _spawn_volume_type: i32,
    pub _scale_max: Vector3<f32>,
    pub _max_particle_count: i32,
    pub _align_mode: i32,
    pub _geometry_type: i32,
    pub _reserved0: i32,
    pub _reserved1: i32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GpuParticleDynamicConstants {
    pub _emitter_transform: Matrix4<f32>,
    pub _spawn_count: i32,
    pub _reserved0: i32,
    pub _reserved1: i32,
    pub _reserved2: i32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GpuParticleCountBufferData {
    pub _particle_alive_count: i32,
    pub _prev_particle_alive_count: i32,
    pub _particle_dead_count: i32,
    pub _reserved0: i32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GpuParticleEmitterIndexBufferData {
    pub _emitter_index: i32,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GpuParticleUpdateBufferData {
    pub _particle_emitter_transform: Matrix4<f32>,
    pub _particle_relative_position: Vector3<f32>,
    pub _particle_elapsed_time: f32,
    pub _particle_local_position: Vector3<f32>,
    pub _particle_initial_life_time: f32,
    pub _particle_left_with_scale: Vector4<f32>,
    pub _particle_up_with_scale: Vector4<f32>,
    pub _particle_front_with_scale: Vector4<f32>,
}

// push constants
#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderParticle {
    pub _allocated_emitter_index: i32,
    pub _reserved0: i32,
    pub _reserved1: i32,
    pub _reserved2: i32,
}

impl Default for PushConstant_RenderParticle {
    fn default() -> PushConstant_RenderParticle {
        PushConstant_RenderParticle {
            _allocated_emitter_index: 0,
            _reserved0: 0,
            _reserved1: 0,
            _reserved2: 0,
        }
    }
}

// interfaces
pub struct EffectManager {
    pub _effect_manager_data: *const EffectManagerData,
    pub _effect_render_group: Vec<*const EmitterInstance>,
    pub _gpu_particle_static_constants: Vec<GpuParticleStaticConstants>,
    pub _gpu_particle_dynamic_constants: Vec<GpuParticleDynamicConstants>,
}

impl EffectManagerBase for EffectManager {
    fn initialize_effect_manager(&mut self, effect_manager_data: *const EffectManagerData) {
        self._effect_manager_data = effect_manager_data;
    }

    fn gather_render_effects(&mut self) {
        let effects = unsafe { &(*self._effect_manager_data)._effects };
        self._effect_render_group.clear();
        for (_effect_id, effect) in effects.iter() {
            let mut effect = effect.borrow_mut();
            if effect._is_alive {
                for emitter in effect._emitters.iter_mut() {
                    if emitter._is_alive {
                        self._effect_render_group.push(emitter);
                    }
                }
            }
        }
    }
}

impl EffectManager {
    pub fn create_effect_manager() -> Box<EffectManager> {
        Box::new(EffectManager {
            _effect_manager_data: std::ptr::null(),
            _effect_render_group: Vec::new(),
            _gpu_particle_static_constants: unsafe { vec![GpuParticleStaticConstants::default(); MAX_EMITTER_COUNT as usize] },
            _gpu_particle_dynamic_constants: unsafe { vec![GpuParticleDynamicConstants::default(); MAX_EMITTER_COUNT as usize] },
        })
    }

    pub fn get_effect_manager_data(&self) -> &EffectManagerData {
        unsafe { &*self._effect_manager_data }
    }

    pub fn get_effect_manager_data_mut(&self) -> &mut EffectManagerData {
        unsafe { &mut *(self._effect_manager_data as *mut EffectManagerData) }
    }

    pub fn process_gpu_particles(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer: &Renderer,
        resources: &Resources,
    ) {
        let mut process_emitter_count: i32 = 0;
        let mut process_gpu_particle_count: i32 = 0;
        let effect_manager_data: &mut EffectManagerData = unsafe { &mut *(self._effect_manager_data as *mut EffectManagerData) };
        let allocated_emitter_count = effect_manager_data._allocated_emitter_count as isize;
        let allocated_emitters: &Vec<*const EmitterInstance> = &effect_manager_data._allocated_emitters;
        for emitter_index in 0..allocated_emitter_count {
            let emitter_ptr: *const EmitterInstance = unsafe { *allocated_emitters.as_ptr().offset(emitter_index) };
            if emitter_ptr.is_null() {
                continue;
            }

            let emitter: &mut EmitterInstance = unsafe { &mut *(emitter_ptr as *mut EmitterInstance) };
            let emitter_data: &EmitterData = emitter.get_emitter_data();
            // update static constants
            if emitter._need_to_upload_static_constant_buffer {
                let gpu_particle_static_constant = &mut self._gpu_particle_static_constants[process_emitter_count as usize];
                gpu_particle_static_constant._spawn_volume_transform.clone_from(&emitter_data._spawn_volume_transform);
                gpu_particle_static_constant._spawn_volume_info.clone_from(&emitter_data._spawn_volume_info);
                gpu_particle_static_constant._spawn_volume_type = emitter_data._spawn_volume_type as i32;
                gpu_particle_static_constant._rotation_min.clone_from(&emitter_data._rotation_min);
                gpu_particle_static_constant._rotation_max.clone_from(&emitter_data._rotation_max);
                gpu_particle_static_constant._particle_lifetime_min = emitter_data._particle_lifetime_min;
                gpu_particle_static_constant._particle_lifetime_max = emitter_data._particle_lifetime_max;
                gpu_particle_static_constant._scale_min.clone_from(&emitter_data._scale_min);
                gpu_particle_static_constant._scale_max.clone_from(&emitter_data._scale_max);
                gpu_particle_static_constant._max_particle_count = emitter_data._max_particle_count;
                gpu_particle_static_constant._align_mode = emitter_data._align_mode as i32;
                gpu_particle_static_constant._geometry_type = emitter_data._geometry_type as i32;
                emitter._need_to_upload_static_constant_buffer = false;
            }

            // update dynamic constants
            {
                let gpu_particle_dynamic_constant = &mut self._gpu_particle_dynamic_constants[process_emitter_count as usize];
                gpu_particle_dynamic_constant._emitter_transform.clone_from(&emitter._emitter_world_transform);
                gpu_particle_dynamic_constant._spawn_count = emitter._particle_spawn_count;
            }

            emitter._allocated_emitter_index = process_emitter_count;
            emitter._allocated_particle_offset = process_gpu_particle_count;

            //
            process_emitter_count += 1;
            process_gpu_particle_count += emitter._allocated_particle_count;
        }

        effect_manager_data._allocated_emitter_count = process_emitter_count;
        effect_manager_data._allocated_particle_count = process_gpu_particle_count;

        // Upload Uniform Buffers
        renderer.upload_shader_buffer_datas(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::GpuParticleStaticConstants,
            &self._gpu_particle_static_constants[0..process_emitter_count as usize]
        );
        renderer.upload_shader_buffer_datas(
            command_buffer,
            swapchain_index,
            &ShaderBufferDataType::GpuParticleDynamicConstants,
            &self._gpu_particle_dynamic_constants[0..process_emitter_count as usize]
        );

        let material_instance_data = &resources.get_material_instance_data("process_gpu_particle").borrow();

        // compute gpu particle count
        let pipeline_binding_data: &PipelineBindingData = material_instance_data.get_pipeline_binding_data("process_gpu_particle/compute_gpu_particle_count");
        let thread_group_count = (process_emitter_count + PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE - 1) / PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE;
        renderer.get_renderer_data().dispatch_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            thread_group_count as u32,
            1,
            1,
            None,
            NONE_PUSH_CONSTANT,
        );

        // update gpu particles
        let pipeline_binding_data: &PipelineBindingData = material_instance_data.get_pipeline_binding_data("process_gpu_particle/update_gpu_particle");
        let thread_group_count = (process_gpu_particle_count + PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE - 1) / PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE;
        renderer.get_renderer_data().dispatch_render_pass_pipeline(
            command_buffer,
            swapchain_index,
            pipeline_binding_data,
            thread_group_count as u32,
            1,
            1,
            None,
            NONE_PUSH_CONSTANT,
        );
    }

    pub fn render_effects(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer: &Renderer,
        resources: &Resources,
    ) {
        let renderer_data: &RendererData = renderer.get_renderer_data();
        let quad_mesh = resources.get_mesh_data("quad").borrow();
        let quad_geometry_data = quad_mesh.get_default_geometry_data().borrow();
        let render_pass_pipeline_data_name = "render_particle_translucent/alpha_blend";
        let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
        let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
        for emitter in self._effect_render_group.iter() {
            let emitter: &EmitterInstance = unsafe { &**emitter };
            let emitter_data: &EmitterData = emitter.get_emitter_data();
            let material_instance_data: &MaterialInstanceData = &emitter_data._material_instance_data.borrow();
            let pipeline_binding_data: &PipelineBindingData = material_instance_data.get_pipeline_binding_data(render_pass_pipeline_data_name);
            let render_pass_data: &RenderPassData = &pipeline_binding_data.get_render_pass_data().borrow();
            let pipeline_data: &PipelineData = &pipeline_binding_data.get_pipeline_data().borrow();

            if prev_pipeline_data != pipeline_data {
                if false == prev_pipeline_data.is_null() {
                    renderer_data.end_render_pass(command_buffer);
                }
                renderer_data.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, None);
                prev_pipeline_data = pipeline_data;
            }

            if prev_pipeline_binding_data != pipeline_binding_data {
                prev_pipeline_binding_data = pipeline_binding_data;
                renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, &*pipeline_binding_data, None);
            }

            renderer_data.upload_push_constant_data(
                command_buffer,
                pipeline_data,
                &PushConstant_RenderParticle {
                    _allocated_emitter_index: 0,
                    _reserved0: 0,
                    _reserved1: 0,
                    _reserved2: 0,
                }
            );
            renderer_data.draw_elements_instanced(command_buffer, &quad_geometry_data, &[], emitter._allocated_particle_count as u32);
        }
        renderer_data.end_render_pass(command_buffer);
    }
}