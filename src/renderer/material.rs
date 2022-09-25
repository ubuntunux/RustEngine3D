use serde_json;
use crate::vulkan_context::render_pass::{
    RenderPassPipelineData,
    RenderPassPipelineDataMap,
};
use crate::renderer::push_constants::PushConstant;
use crate::renderer::push_constants::PushConstantsMap;

#[derive(Clone, Debug)]
pub struct MaterialData {
    pub _material_data_name: String,
    pub _render_pass_pipeline_data_map: RenderPassPipelineDataMap,
    pub _material_resources: serde_json::Value,
    pub _push_constants_map: PushConstantsMap
}

impl MaterialData {
    pub fn create_material(
        material_data_name: &String,
        render_pass_pipeline_datas: &Vec<Option<RenderPassPipelineData>>,
        material_resources: &serde_json::Value,
        material_parameters: &serde_json::Value
    ) -> MaterialData {
        log::debug!("create_material: {}", material_data_name);

        let mut push_constants_map = PushConstantsMap::new();
        let mut render_pass_pipeline_data_map = RenderPassPipelineDataMap::new();
        for maybe_render_pass_pipeline_data in render_pass_pipeline_datas.iter() {
            if maybe_render_pass_pipeline_data.is_some() {
                let render_pass_pipeline_data = maybe_render_pass_pipeline_data.as_ref().unwrap();
                let render_pass_pipeline_data_name = format!(
                    "{}/{}",
                    render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name,
                    render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name
                );
                log::trace!("    renderPass/pipeline: {:?}", render_pass_pipeline_data_name);
                render_pass_pipeline_data_map.insert(render_pass_pipeline_data_name.clone(), render_pass_pipeline_data.clone());

                let push_constants: Vec<Box<dyn PushConstant>> = Vec::new();
                push_constants_map.insert(render_pass_pipeline_data_name.clone(), push_constants);
            }
        }
        MaterialData {
            _material_data_name: material_data_name.clone(),
            _render_pass_pipeline_data_map: render_pass_pipeline_data_map,
            _material_resources: material_resources.clone(),
            _push_constants_map: push_constants_map
        }
    }

    pub fn destroy_material(&self) {
        log::debug!("create_material: {}", self._material_data_name);
    }

    pub fn get_render_pass_pipeline_data(
        &self,
        render_pass_pipeline_data_name: &str
    ) -> &RenderPassPipelineData {
        self._render_pass_pipeline_data_map.get(render_pass_pipeline_data_name).unwrap()
    }
}