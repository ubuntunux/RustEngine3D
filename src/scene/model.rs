use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::utilities::math;
use crate::utilities::system::RcRefCell;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct ModelDataInfo {
    pub _mesh: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _material_instances: Vec<String>,
}

impl Default for ModelDataInfo {
    fn default() -> ModelDataInfo {
        ModelDataInfo {
            _mesh: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _material_instances: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModelData<'a> {
    pub _model_data_name: String,
    pub _local_transform: Matrix4<f32>,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
}

impl<'a> ModelData<'a> {
    pub fn create_model_data(
        model_name: &String,
        mesh_data: RcRefCell<MeshData>,
        material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
        position: &Vector3<f32>,
        rotation: &Vector3<f32>,
        scale: &Vector3<f32>,
    ) -> ModelData<'a> {
        log::debug!("new_model_data: {}", model_name);
        for (i, x) in material_instance_data_list.iter().enumerate() {
            log::info!("    material_instance[{:?}]{:?}: {:?}", i, model_name, x.borrow()._material_instance_data_name);
        }

        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_data: mesh_data,
            _local_transform: math::make_srt_transform(
                position,
                rotation,
                scale
            ),
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
