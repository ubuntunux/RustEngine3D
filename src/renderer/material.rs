use serde_json;
use crate::vulkan_context::render_pass::{
    get_render_pass_pipeline_data_name,
    PipelinePushConstantData,
    RenderPassPipelineData,
    RenderPassPipelineDataMap,
};

#[derive(Clone, Debug)]
pub struct MaterialData {
    pub _material_data_name: String,
    pub _render_pass_pipeline_data_map: RenderPassPipelineDataMap,
    pub _material_parameters: serde_json::Map<String, serde_json::Value>
}

impl MaterialData {
    pub fn create_material(
        material_data_name: &String,
        render_pass_pipeline_datas: &Vec<Option<RenderPassPipelineData>>,
        material_parameters: &serde_json::Map<String, serde_json::Value>
    ) -> MaterialData {
        log::debug!("create_material: {}", material_data_name);

        let mut check_push_consant_names: Vec<String> = Vec::new();
        let mut render_pass_pipeline_data_map = RenderPassPipelineDataMap::new();
        for maybe_render_pass_pipeline_data in render_pass_pipeline_datas.iter() {
            if maybe_render_pass_pipeline_data.is_some() {
                let render_pass_pipeline_data = maybe_render_pass_pipeline_data.as_ref().unwrap();
                let render_pass_pipeline_data_name = get_render_pass_pipeline_data_name(
                    &render_pass_pipeline_data._render_pass_data.borrow()._render_pass_data_name,
                    &render_pass_pipeline_data._pipeline_data.borrow()._pipeline_data_name
                );

                // check matched push constant
                if check_push_consant_names.is_empty() {
                    check_push_consant_names = render_pass_pipeline_data._pipeline_data.borrow()._push_constant_datas.iter().map(|push_constant_data| {
                        String::from(push_constant_data._push_constant.get_push_constant_name())
                    }).collect();
                } else {
                    let push_constant_names: Vec<String> = render_pass_pipeline_data._pipeline_data.borrow()._push_constant_datas.iter().map(
                        |push_constant_data: &PipelinePushConstantData|
                            String::from(push_constant_data._push_constant.get_push_constant_name())
                    ).collect();

                    for push_constant_data in render_pass_pipeline_data._pipeline_data.borrow()._push_constant_datas.iter() {
                        let mut find_push_constant = false;
                        for check_push_consant_name in check_push_consant_names.iter() {
                            if check_push_consant_name == push_constant_data._push_constant.get_push_constant_name() {
                                find_push_constant = true;
                                break;
                            }
                        }

                        assert!(
                            find_push_constant,
                            "not matched push constants. material name: {:?}, push_constant_names: {:?}, check_push_consant_names: {:?}",
                            material_data_name,
                            push_constant_names,
                            check_push_consant_names
                        );
                    }
                }

                log::trace!("    renderPass/pipeline: {:?}", render_pass_pipeline_data_name);
                render_pass_pipeline_data_map.insert(render_pass_pipeline_data_name, render_pass_pipeline_data.clone());
            }
        }

        MaterialData {
            _material_data_name: material_data_name.clone(),
            _render_pass_pipeline_data_map: render_pass_pipeline_data_map,
            _material_parameters: material_parameters.clone()
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