
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct ModelCreateInfo {
    pub _model_data_name: String,
    pub _mesh_data: MeshData,
    pub _material_instance_data_list: Vec<MaterialInstanceData>,
}

#[derive(Clone, Debug)]
pub struct ModelData {
    pub _model_data_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData>>,
}

impl ModelData {
    pub fn new_model_data(
        model_name: &String,
        mesh_data: RcRefCell<MeshData>,
        material_instance_data_list: Vec<RcRefCell<MaterialInstanceData>>
    ) -> ModelData {
        log::debug!("new_model_data: {}", model_name);
        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_data: mesh_data,
            _material_instance_data_list: material_instance_data_list,
        }
    }

    pub fn destroy_model_data(&self) {
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_material_instance_data_count(&self) -> usize {
        self._material_instance_data_list.len()
    }

    pub fn get_material_instance_data_list(&self) -> &Vec<RcRefCell<MaterialInstanceData>> {
        &self._material_instance_data_list
    }

    pub fn set_material_instance_data_list(&mut self, material_instance_data_list: Vec<RcRefCell<MaterialInstanceData>>) {
        self._material_instance_data_list = material_instance_data_list;
    }

    pub fn get_material_instance_data(&self, index: usize) -> &RcRefCell<MaterialInstanceData> {
        &self._material_instance_data_list[index]
    }

    pub fn update_model_data(&self) {
    }
}