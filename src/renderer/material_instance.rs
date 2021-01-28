use std::collections::HashMap;

use ash::{
    vk,
    Device,
};

use crate::renderer::material::MaterialData;
use crate::vulkan_context::descriptor::{ self, DescriptorResourceInfo };
use crate::vulkan_context::vulkan_context::SwapchainArray;
use crate::vulkan_context::render_pass::{self, RenderPassPipelineData, RenderPassData, PipelineData};
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct PipelineBindingData {
    pub _render_pass_pipeline_data: RenderPassPipelineData,
    pub _descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    pub _write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>>,
    pub _descriptor_set_count: u32,
    pub _descriptor_resource_infos_list: SwapchainArray<Vec<DescriptorResourceInfo>>,
}

type PipelineBindingDataMap = HashMap<String, PipelineBindingData>;

#[derive(Clone, Debug)]
pub struct MaterialInstanceData {
    pub _material_instance_data_name: String,
    pub _material_data: RcRefCell<MaterialData>,
    pub _pipeline_binding_data_map: PipelineBindingDataMap,
    pub _default_pipeline_binding_name: String,
}

impl PipelineBindingData {
    pub fn get_render_pass_data(&self) -> &RcRefCell<RenderPassData> {
        &self._render_pass_pipeline_data._render_pass_data
    }

    pub fn get_pipeline_data(&self) -> &RcRefCell<PipelineData> {
        &self._render_pass_pipeline_data._pipeline_data
    }

    pub fn get_pipeline_bind_point(&self) -> vk::PipelineBindPoint {
        self._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_bind_point
    }

    pub fn get_pipeline_layout(&self) -> vk::PipelineLayout {
        self._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_layout
    }
}


impl MaterialInstanceData {
    pub fn create_material_instance(
        device: &Device,
        material_instance_data_name: &String,
        material_data: RcRefCell<MaterialData>,
        pipeline_bind_create_infos: Vec<(render_pass::RenderPassPipelineData, SwapchainArray<Vec<descriptor::DescriptorResourceInfo>>)>,
    ) -> MaterialInstanceData {
        log::info!("create_material_instance: {}", material_instance_data_name);
        log::debug!("    material_data: {}", material_data.borrow()._material_data_name);
        let mut pipeline_binding_data_map = PipelineBindingDataMap::new();
        let mut default_pipeline_binding_name = String::new();
        for (render_pass_pipeline_data, descriptor_resource_infos_list) in pipeline_bind_create_infos {
            let render_pass_pipeline_data_name = format!(
                "{}/{}",
                render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name,
                render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name,
            );

            if default_pipeline_binding_name.is_empty() {
                default_pipeline_binding_name = render_pass_pipeline_data_name.clone();
            }

            log::debug!("        renderpass/pipeline: {}", render_pass_pipeline_data_name);
            let descriptor_data = &render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data;
            let descriptor_sets = descriptor::create_descriptor_sets(device, descriptor_data);
            let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
                descriptor_data_create_info._descriptor_binding_index
            }).collect();
            let write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>> = descriptor::create_write_descriptor_sets_with_update(
                device,
                &descriptor_sets,
                &descriptor_binding_indices,
                &descriptor_data._descriptor_set_layout_bindings,
                &descriptor_resource_infos_list,
            );

            let pipeline_binding_data = PipelineBindingData {
                _render_pass_pipeline_data: render_pass_pipeline_data.clone(),
                _descriptor_sets: descriptor_sets,
                _write_descriptor_sets: write_descriptor_sets,
                _descriptor_set_count: descriptor_binding_indices.len() as u32,
                _descriptor_resource_infos_list: descriptor_resource_infos_list,
            };

            pipeline_binding_data_map.insert(render_pass_pipeline_data_name, pipeline_binding_data);
        }

        MaterialInstanceData {
            _material_instance_data_name: material_instance_data_name.clone(),
            _material_data: material_data.clone(),
            _pipeline_binding_data_map: pipeline_binding_data_map,
            _default_pipeline_binding_name: default_pipeline_binding_name,
        }
    }

    pub fn destroy_material_instance(&self) {
        log::info!("destroy_material_instance: {}", self._material_instance_data_name);
    }

    pub fn get_default_pipeline_binding_data(
        &self,
    ) -> &PipelineBindingData {
        self._pipeline_binding_data_map.get(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_default_pipeline_binding_data_mut(
        &mut self,
    ) -> &mut PipelineBindingData {
        self._pipeline_binding_data_map.get_mut(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_pipeline_binding_data(
        &self,
        render_pass_pipeline_data_name: &str,
    ) -> &PipelineBindingData {
        self._pipeline_binding_data_map.get(render_pass_pipeline_data_name).unwrap()
    }

    pub fn get_pipeline_binding_data_mut(
        &mut self,
        render_pass_pipeline_data_name: &str,
    ) -> &mut PipelineBindingData {
        self._pipeline_binding_data_map.get_mut(render_pass_pipeline_data_name).unwrap()
    }
}