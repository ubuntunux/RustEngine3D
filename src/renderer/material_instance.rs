use std::collections::HashMap;

use ash::{
    vk,
    Device,
};

use crate::renderer::material::MaterialData;
use crate::vulkan_context::{
    descriptor,
    render_pass,
};
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;
use crate::constants;
use ash::version::DeviceV1_0;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::render_pass::RenderPassPipelineData;

#[derive(Clone, Debug)]
pub struct PipelineBindingData {
    pub _render_pass_pipeline_data: RenderPassPipelineData,
    pub _descriptor_sets: SwapchainIndexMap<vk::DescriptorSet>,
    pub _write_descriptor_sets: SwapchainIndexMap<Vec<vk::WriteDescriptorSet>>,
    pub _descriptor_set_count: u32,
}

type PipelineBindingDataMap = HashMap<render_pass::RenderPassPipelineDataName, PipelineBindingData>;

#[derive(Clone, Debug)]
pub struct MaterialInstanceData {
    pub _material_instance_data_name: String,
    pub _material_data: RcRefCell<MaterialData>,
    pub _pipeline_binding_data_map: PipelineBindingDataMap,
}

impl MaterialInstanceData {
    pub fn create_material_instance(
        device: &Device,
        material_instance_data_name: &String,
        material_data: RcRefCell<MaterialData>,
        pipeline_bind_create_infos: &Vec<(render_pass::RenderPassPipelineData, Vec<Vec<descriptor::DescriptorResourceInfo>>)>,
    ) -> MaterialInstanceData {
        log::info!("create_material_instance: {}", material_instance_data_name);
        log::info!("    material_data: {}", material_data.borrow()._material_data_name);
        let mut pipeline_binding_data_map = PipelineBindingDataMap::new();
        for (render_pass_pipeline_data, descriptor_resource_infos_list) in pipeline_bind_create_infos {
            let render_pass_pipeline_data_name = render_pass::RenderPassPipelineDataName {
                _render_pass_data_name: render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name.clone(),
                _pipeline_data_name: render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name.clone(),
            };
            log::info!("        {:?}", render_pass_pipeline_data_name);
            let descriptor_data = &render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data;
            let descriptor_sets = descriptor::create_descriptor_sets(device, descriptor_data);
            let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
                descriptor_data_create_info._descriptor_binding_index
            }).collect();
            let descriptor_set_layout_bindings = &descriptor_data._descriptor_set_layout_bindings;
            let descriptor_set_binding_count = descriptor_binding_indices.len();
            let write_descriptor_sets: SwapchainIndexMap<Vec<vk::WriteDescriptorSet>> = constants::SWAPCHAIN_IMAGE_INDICES
                .iter()
                .map(|index| {
                    let descriptor_set = descriptor_sets[*index as usize];
                    let descriptor_resource_infos = &descriptor_resource_infos_list[*index as usize];
                    let descriptor_writes = descriptor::create_write_descriptor_sets(
                        descriptor_set,
                        &descriptor_binding_indices,
                        descriptor_set_layout_bindings,
                        descriptor_resource_infos,
                    );
                    let descriptor_writes_count = descriptor_resource_infos.len();
                    assert_eq!(descriptor_writes_count, descriptor_set_binding_count, "descriptorWritesCount Error");

                    unsafe {
                        // vkUpdateDescriptorSets
                        device.update_descriptor_sets(&descriptor_writes, &[]);
                    }
                    descriptor_writes
                }).collect();

            let pipeline_binding_data = PipelineBindingData {
                _render_pass_pipeline_data: render_pass_pipeline_data.clone(),
                _descriptor_sets: descriptor_sets,
                _write_descriptor_sets: write_descriptor_sets,
                _descriptor_set_count: descriptor_set_binding_count as u32,
            };

            pipeline_binding_data_map.insert(render_pass_pipeline_data_name, pipeline_binding_data);
        }

        MaterialInstanceData {
            _material_instance_data_name: material_instance_data_name.clone(),
            _material_data: material_data.clone(),
            _pipeline_binding_data_map: pipeline_binding_data_map
        }
    }

    pub fn destroy_material_instance(&self) {
        log::info!("destroy_material_instance: {}", self._material_instance_data_name);
    }

    pub fn get_pipeline_binding_data(
        &self,
        render_pass_pipeline_data_name: &render_pass::RenderPassPipelineDataName,
    ) -> &PipelineBindingData {
        self._pipeline_binding_data_map.get(render_pass_pipeline_data_name).unwrap()
    }

    pub fn get_pipeline_binding_data_mut(
        &mut self,
        render_pass_pipeline_data_name: &render_pass::RenderPassPipelineDataName,
    ) -> &mut PipelineBindingData {
        self._pipeline_binding_data_map.get_mut(render_pass_pipeline_data_name).unwrap()
    }
}