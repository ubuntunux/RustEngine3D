use serde_json;

use crate::vulkan_context::render_pass::{
    RenderPassPipelineDataName,
    RenderPassPipelineData,
    RenderPassPipelineDataMap,
};

#[derive(Clone, Debug)]
pub struct MaterialData {
    pub _material_data_name: String,
    pub _render_pass_pipeline_data_map: RenderPassPipelineDataMap,
    pub _material_parameter_map: serde_json::Value,
}

impl MaterialData {
    pub fn create_material(
        material_data_name: &String,
        render_pass_pipeline_datas: &Vec<RenderPassPipelineData>,
        material_parameter_map: &serde_json::Value
    ) -> MaterialData {
        log::info!("create_material: {}", material_data_name);

        let mut render_pass_pipeline_data_map = RenderPassPipelineDataMap::new();
        for render_pass_pipeline_data in render_pass_pipeline_datas {
            let render_pass_pipeline_data_name = RenderPassPipelineDataName {
                _render_pass_data_name: render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name.clone(),
                _pipeline_data_name: render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name.clone(),
            };
            log::info!("    renderPass, pipeline: {:?}", render_pass_pipeline_data_name);
            render_pass_pipeline_data_map.insert(render_pass_pipeline_data_name, render_pass_pipeline_data.clone());
        }
        MaterialData {
            _material_data_name: material_data_name.clone(),
            _render_pass_pipeline_data_map: render_pass_pipeline_data_map,
            _material_parameter_map: material_parameter_map.clone()
        }
    }

    pub fn destroy_material(&self) {
        // nothing
    }

    pub fn get_render_pass_pipeline_data(
        &self,
        render_pass_pipeline_data_name: &RenderPassPipelineDataName
    ) -> &RenderPassPipelineData {
        self._render_pass_pipeline_data_map.get(render_pass_pipeline_data_name).unwrap()
    }
}