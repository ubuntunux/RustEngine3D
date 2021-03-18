use ash::vk;
use nalgebra::{ Matrix4 };

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

use crate::renderer::push_constants::{ PushConstant_RenderParticle, NONE_PUSH_CONSTANT };


pub struct EffectManager {
    pub _effect_manager_data: *const EffectManagerData,
    pub _render_group: Vec<*const EmitterInstance>,
}

impl EffectManagerBase for EffectManager {
    fn initialize_effect_manager(&mut self, effect_manager_data: *const EffectManagerData) {
        self._effect_manager_data = effect_manager_data;
    }

    fn gather_render_effects(&mut self) {
        let effects = unsafe { &(*self._effect_manager_data)._effects };
        self._render_group.clear();
        for (_effect_id, effect) in effects.iter() {
            let mut effect = effect.borrow_mut();
            if effect._is_alive {
                for emitter in effect._emitters.iter_mut() {
                    if emitter._is_alive {
                        self._render_group.push(emitter);
                    }
                }
            }
        }
    }

    fn process_gpu_particles(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_data: &RendererData,
        resources: &Resources,
    ) {
        let mut process_emitter_count = self.get_effect_manager_data()._allocated_emitter_count;
        let mut process_gpu_particle_count = self.get_effect_manager_data()._allocated_particle_count;

        // upload constant buffers
        // for emitter in self._render_group.iter() {
        //     let emitter: &EmitterInstance = unsafe { &**emitter };
        //     let emitter_data: &EmitterData = emitter.get_emitter_data();
        // }

        let material_instance_data = &resources.get_material_instance_data("process_gpu_particle").borrow();

        // compute gpu particle count
        {
            let pipeline_binding_data: &PipelineBindingData = material_instance_data.get_pipeline_binding_data("process_gpu_particle/compute_gpu_particle_count");
            let thread_group_count = (process_emitter_count + PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE - 1) / PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE;
            renderer_data.dispatch_render_pass_pipeline(
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

        // update gpu particles
        {
            let pipeline_binding_data: &PipelineBindingData = material_instance_data.get_pipeline_binding_data("process_gpu_particle/update_gpu_particle");
            let thread_group_count = (process_gpu_particle_count + PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE - 1) / PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE;
            renderer_data.dispatch_render_pass_pipeline(
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
    }

    fn render_effects(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_data: &RendererData,
        resources: &Resources,
    ) {
        let quad_mesh = resources.get_mesh_data("quad").borrow();
        let quad_geometry_data = quad_mesh.get_default_geometry_data().borrow();
        let render_pass_pipeline_data_name = "render_particle_translucent/alpha_blend";
        let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
        let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
        for emitter in self._render_group.iter() {
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
                    _local_matrix: emitter._emitter_transform.get_matrix().clone() as Matrix4<f32>
                }
            );
            renderer_data.draw_elements(command_buffer, &quad_geometry_data);
        }
        renderer_data.end_render_pass(command_buffer);
    }
}

impl EffectManager {
    pub fn create_effect_manager() -> Box<EffectManager> {
        Box::new(EffectManager {
            _effect_manager_data: std::ptr::null(),
            _render_group: Vec::new(),
        })
    }

    pub fn get_effect_manager_data(&self) -> &EffectManagerData {
        unsafe { &*self._effect_manager_data }
    }

    pub fn get_effect_manager_data_mut(&self) -> &mut EffectManagerData {
        unsafe { &mut *(self._effect_manager_data as *mut EffectManagerData) }
    }
}