use std::collections::HashMap;
use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::animation::INVALID_BONE_INDEX;
use crate::scene::bounding_box::BoundingBox;
use crate::scene::collision::{CollisionCreateInfo, CollisionData};
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::scene::socket::{SocketData, SocketDataCreateInfo};
use crate::utilities::math;
use crate::utilities::system::{newRcRefCell, RcRefCell};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct ModelDataInfo {
    pub _mesh: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _material_instances: Vec<String>,
    pub _bounding_box: BoundingBox,
    pub _collision: CollisionCreateInfo,
    pub _sockets: HashMap<String, SocketDataCreateInfo>
}

impl Default for ModelDataInfo {
    fn default() -> ModelDataInfo {
        ModelDataInfo {
            _mesh: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _material_instances: Vec::new(),
            _bounding_box: BoundingBox::default(),
            _collision: CollisionCreateInfo::default(),
            _sockets: HashMap::new()
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModelData<'a> {
    pub _model_data_name: String,
    pub _local_transform: Matrix4<f32>,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
    pub _collision: CollisionData,
    pub _socket_data_map: HashMap<String, RcRefCell<SocketData>>
}

impl<'a> ModelData<'a> {
    pub fn create_model_data(
        model_name: &String,
        mesh_data: &RcRefCell<MeshData>,
        material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
        model_data_info: &ModelDataInfo,
    ) -> ModelData<'a> {
        log::debug!("new_model_data: {}", model_name);
        for (i, x) in material_instance_data_list.iter().enumerate() {
            log::debug!("    material_instance[{:?}]{:?}: {:?}", i, model_name, x.borrow()._material_instance_data_name);
        }

        let mut socket_data_map = HashMap::new();
        for (socket_name, socket_data_create_info) in model_data_info._sockets.iter() {
            let mut parent_bone_index: usize = INVALID_BONE_INDEX;
            for skeleton_data in mesh_data.borrow()._skeleton_data_list.iter() {
                if let Some(bone_index) = skeleton_data._bone_index_map.get(&socket_data_create_info._parent_bone) {
                    parent_bone_index = *bone_index;
                    break;
                }
            }

            if parent_bone_index != INVALID_BONE_INDEX {
                socket_data_map.insert(
                    socket_name.clone(),
                    newRcRefCell(SocketData::create_socket_data(socket_name, socket_data_create_info, parent_bone_index))
                );
            } else {
                log::info!("Failed to create socket: {:?}, parent bone: {:?}", socket_name, socket_data_create_info._parent_bone);
            }
        }

        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_data: mesh_data.clone(),
            _local_transform: math::make_srt_transform(
                &model_data_info._position,
                &model_data_info._rotation,
                &model_data_info._scale
            ),
            _material_instance_data_list: material_instance_data_list,
            _collision: CollisionData::create_collision(&model_data_info._collision),
            _socket_data_map: socket_data_map
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
