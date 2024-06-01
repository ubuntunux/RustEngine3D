use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct ModelCreateInfo<'a> {
    pub _model_data_name: String,
    pub _mesh_data: MeshData,
    pub _material_instance_data_list: Vec<MaterialInstanceData<'a>>,
}

#[derive(Clone, Debug)]
pub struct ModelData<'a> {
    pub _model_data_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
}

impl<'a> ModelData<'a> {
    pub fn new_model_data(
        model_name: &String,
        mesh_data: RcRefCell<MeshData>,
        material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
    ) -> ModelData<'a> {
        log::debug!("new_model_data: {}", model_name);
        for (i, x) in material_instance_data_list.iter().enumerate() {
            log::info!("new_model_data [{:?}]{:?}: {:?}", i, model_name, x.borrow()._material_instance_data_name);
        }
        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_data: mesh_data,
            _material_instance_data_list: material_instance_data_list,
        }
    }

    pub fn destroy_model_data(&self) {}

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_material_instance_data_count(&self) -> usize {
        self._material_instance_data_list.len()
    }

    pub fn get_material_instance_data_list(&self) -> &Vec<RcRefCell<MaterialInstanceData<'a>>> {
        &self._material_instance_data_list
    }

    pub fn get_material_instance_data(&self, index: usize) -> &RcRefCell<MaterialInstanceData<'a>> {
        &self._material_instance_data_list[index]
    }

    pub fn update_model_data(&self) {}
}
