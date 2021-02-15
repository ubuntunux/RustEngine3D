
use crate::renderer::material_instance::MaterialInstanceData;
use crate::renderer::mesh::MeshData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct ModelCreateInfo {
    pub _model_data_name: String,
    pub _mesh_data: MeshData,
    pub _material_instance_datas: Vec<MaterialInstanceData>,
}

#[derive(Clone, Debug)]
pub struct ModelData {
    pub _model_data_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _material_instance_datas: Vec<RcRefCell<MaterialInstanceData>>,
}

impl ModelData {
    pub fn new_model_data(
        model_name: &String,
        mesh_data: RcRefCell<MeshData>,
        material_instance_datas: Vec<RcRefCell<MaterialInstanceData>>
    ) -> ModelData {
        log::debug!("new_model_data: {}", model_name);
        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_data: mesh_data,
            _material_instance_datas: material_instance_datas,
        }
    }

    pub fn destroy_model_data(&self) {
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_material_instance_data_count(&self) -> usize {
        self._material_instance_datas.len()
    }

    pub fn get_material_instance_datas(&self) -> &Vec<RcRefCell<MaterialInstanceData>> {
        &self._material_instance_datas
    }

    pub fn set_material_instance_datas(&mut self, material_instance_datas: Vec<RcRefCell<MaterialInstanceData>>) {
        self._material_instance_datas = material_instance_datas;
    }

    pub fn get_material_instance_data(&self, index: usize) -> &RcRefCell<MaterialInstanceData> {
        &self._material_instance_datas[index]
    }

    pub fn update_model_data(&self) {
    }
}