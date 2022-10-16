use std::collections::HashMap;
use ash::{vk, Device};
use crate::renderer::material::MaterialData;
use crate::vulkan_context::descriptor::{ DescriptorResourceInfo, create_descriptor_sets, create_write_descriptor_sets_with_update };
use crate::vulkan_context::vulkan_context::SwapchainArray;
use crate::vulkan_context::render_pass::{
    get_render_pass_pipeline_data_name,
    RenderPassPipelineData,
    RenderPassData,
    PipelineData,
    PipelinePushConstantData
};
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct PipelineBindingDataCreateInfo {
    pub _render_pass_pipeline_data: RenderPassPipelineData,
    pub _descriptor_resource_infos_list: SwapchainArray<Vec<DescriptorResourceInfo>>,
    pub _push_constant_datas: Vec<PipelinePushConstantData>,
}

#[derive(Clone, Debug)]
pub struct PipelineBindingData {
    pub _render_pass_pipeline_data: RenderPassPipelineData,
    pub _descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    pub _write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>>,
    pub _descriptor_set_count: u32,
    pub _descriptor_resource_infos_list: SwapchainArray<Vec<DescriptorResourceInfo>>,
    pub _push_constant_datas: Vec<PipelinePushConstantData>
}

type PipelineBindingDataMap = HashMap<String, PipelineBindingData>;

#[derive(Clone, Debug)]
pub struct MaterialInstanceData {
    pub _material_instance_data_name: String,
    pub _material_data: RcRefCell<MaterialData>,
    pub _material_parameters: serde_json::Map<String, serde_json::Value>,
    pub _render_pass_pipeline_data_names_map: HashMap<String, Vec<String>>,
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
        material_data: &RcRefCell<MaterialData>,
        material_parameters: &serde_json::Map<String, serde_json::Value>,
        pipeline_bind_create_infos: Vec<PipelineBindingDataCreateInfo>,
    ) -> MaterialInstanceData {
        log::debug!("create_material_instance: {}", material_instance_data_name);
        log::trace!("    material_data: {}", material_data.borrow()._material_data_name);
        let mut render_pass_pipeline_data_names_map: HashMap<String, Vec<String>> = HashMap::new();
        let mut pipeline_binding_data_map = PipelineBindingDataMap::new();
        let mut default_pipeline_binding_name = String::new();
        for pipeline_bind_create_info in pipeline_bind_create_infos {
            let render_pass_name = &pipeline_bind_create_info._render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name;
            let pipeline_name = &pipeline_bind_create_info._render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name;
            let render_pass_pipeline_data_name = get_render_pass_pipeline_data_name(render_pass_name, pipeline_name);

            if default_pipeline_binding_name.is_empty() {
                default_pipeline_binding_name = render_pass_pipeline_data_name.clone();
            }

            log::trace!("        renderpass/pipeline: {}", render_pass_pipeline_data_name);
            let descriptor_data = &pipeline_bind_create_info._render_pass_pipeline_data._pipeline_data.borrow()._descriptor_data;
            let descriptor_sets = create_descriptor_sets(device, descriptor_data);
            let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
                descriptor_data_create_info._descriptor_binding_index
            }).collect();
            let write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>> = create_write_descriptor_sets_with_update(
                device,
                &descriptor_sets,
                &descriptor_binding_indices,
                &descriptor_data._descriptor_set_layout_bindings,
                &pipeline_bind_create_info._descriptor_resource_infos_list,
            );

            let pipeline_binding_data = PipelineBindingData {
                _render_pass_pipeline_data: pipeline_bind_create_info._render_pass_pipeline_data.clone(),
                _descriptor_sets: descriptor_sets,
                _write_descriptor_sets: write_descriptor_sets,
                _descriptor_set_count: descriptor_binding_indices.len() as u32,
                _descriptor_resource_infos_list: pipeline_bind_create_info._descriptor_resource_infos_list,
                _push_constant_datas: pipeline_bind_create_info._push_constant_datas.clone()
            };

            // insert to pipeline_binding_data_map
            pipeline_binding_data_map.insert(render_pass_pipeline_data_name.clone(), pipeline_binding_data.clone());

            // insert to render_pass_pipeline_data_names_map
            if let Some(render_pass_pipeline_data_names) = render_pass_pipeline_data_names_map.get_mut(render_pass_name) {
                render_pass_pipeline_data_names.push(render_pass_pipeline_data_name.clone());
            } else {
                render_pass_pipeline_data_names_map.insert(render_pass_name.clone(), vec![render_pass_pipeline_data_name.clone()]);
            }
        }

        MaterialInstanceData {
            _material_instance_data_name: material_instance_data_name.clone(),
            _material_data: material_data.clone(),
            _material_parameters: material_parameters.clone(),
            _render_pass_pipeline_data_names_map: render_pass_pipeline_data_names_map,
            _pipeline_binding_data_map: pipeline_binding_data_map,
            _default_pipeline_binding_name: default_pipeline_binding_name,
        }
    }

    pub fn destroy_material_instance(&self) {
        log::debug!("destroy_material_instance: {}", self._material_instance_data_name);
    }

    pub fn get_default_pipeline_binding_data(&self) -> &PipelineBindingData {
        self._pipeline_binding_data_map.get(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_default_pipeline_binding_data_mut(&mut self) -> &mut PipelineBindingData {
        self._pipeline_binding_data_map.get_mut(self._default_pipeline_binding_name.as_str()).unwrap()
    }

    pub fn get_pipeline_binding_data(&self, render_pass_pipeline_data_name: &str) -> &PipelineBindingData {
        self._pipeline_binding_data_map.get(render_pass_pipeline_data_name).unwrap()
    }

    pub fn get_pipeline_binding_data_mut(&mut self, render_pass_pipeline_data_name: &str) -> &mut PipelineBindingData {
        self._pipeline_binding_data_map.get_mut(render_pass_pipeline_data_name).unwrap()
    }

    pub fn get_render_pass_pipeline_data_names(&self, render_pass_name: &str) -> &Vec<String> {
        self._render_pass_pipeline_data_names_map.get(render_pass_name).unwrap()
    }
}