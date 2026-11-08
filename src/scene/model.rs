use crate::scene::animation::INVALID_BONE_INDEX;
use crate::scene::bounding_box::BoundingBox;
use crate::scene::collision::{CollisionCreateInfo, CollisionData};
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::scene::socket::{SocketData, SocketDataCreateInfo};
use crate::utilities::math;
use crate::utilities::system::{RcRefCell, newRcRefCell};
use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct ModelDataInfo {
    pub _mesh: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _material_instances: Vec<String>,
    pub _bounding_box: BoundingBox,
    pub _collision: CollisionCreateInfo,
    pub _sockets: HashMap<String, SocketDataCreateInfo>,
}

impl Default for ModelDataInfo {
    fn default() -> ModelDataInfo {
        ModelDataInfo {
            _mesh: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _is_render_camera: true,
            _is_render_shadow: true,
            _material_instances: Vec::new(),
            _bounding_box: BoundingBox::default(),
            _collision: CollisionCreateInfo::default(),
            _sockets: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModelData<'a> {
    pub _model_data_name: String,
    pub _mesh_name: String,
    pub _local_transform: Matrix4<f32>,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
    pub _collision: CollisionData,
    pub _socket_data_map: HashMap<String, RcRefCell<SocketData>>,
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
            log::debug!(
                "    material_instance[{:?}]{:?}: {:?}",
                i,
                model_name,
                x.borrow()._material_instance_data_name
            );
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

            socket_data_map.insert(
                socket_name.clone(),
                newRcRefCell(SocketData::create_socket_data(
                    socket_name,
                    socket_data_create_info,
                    parent_bone_index,
                )),
            );
        }

        ModelData {
            _model_data_name: model_name.clone(),
            _mesh_name: model_data_info._mesh.clone(),
            _mesh_data: mesh_data.clone(),
            _local_transform: math::make_srt_transform(
                &model_data_info._position,
                &model_data_info._rotation,
                &model_data_info._scale,
            ),
            _is_render_camera: model_data_info._is_render_camera,
            _is_render_shadow: model_data_info._is_render_shadow,
            _material_instance_data_list: material_instance_data_list,
            _collision: CollisionData::create_collision(&model_data_info._collision),
            _socket_data_map: socket_data_map,
        }
    }

    pub fn is_ready(&self) -> bool {
        let mut is_mesh_failed = false;
        if crate::core::engine_service_locator::is_engine_core_valid() && !self._mesh_name.is_empty() {
            let engine_resources = crate::core::engine_service_locator::get_engine_resources();
            if let Some(entry) = engine_resources._mesh_data_map.get(&self._mesh_name) {
                match entry.get_state() {
                    crate::resource::resource::ResourceState::Unloaded => {
                        engine_resources.request_mesh_data(&self._mesh_name);
                        return false;
                    }
                    crate::resource::resource::ResourceState::Loading => {
                        return false;
                    }
                    crate::resource::resource::ResourceState::Loaded => {
                        if let Some(loaded_mesh) = entry.get_loaded_data() {
                            if self._mesh_data.as_ptr() != loaded_mesh.as_ptr() {
                                return false;
                            }
                        }
                    }
                    crate::resource::resource::ResourceState::Failed => {
                        is_mesh_failed = true;
                    }
                }
            } else {
                is_mesh_failed = true;
            }
        }
        if !is_mesh_failed && !self._mesh_data.borrow().is_ready() {
            return false;
        }
        for mat_inst in self._material_instance_data_list.iter() {
            if !mat_inst.borrow().is_ready() {
                return false;
            }
        }
        true
    }

    pub fn update_model_data_resources(&mut self) {
        if crate::core::engine_service_locator::is_engine_core_valid() && !self._mesh_name.is_empty() {
            let engine_resources = crate::core::engine_service_locator::get_engine_resources();
            if let Some(entry) = engine_resources._mesh_data_map.get(&self._mesh_name) {
                if let Some(loaded_mesh) = entry.get_loaded_data() {
                    if self._mesh_data.as_ptr() != loaded_mesh.as_ptr() {
                        let geometry_data_count = loaded_mesh.borrow().get_geometry_data_count();
                        while self._material_instance_data_list.len() < geometry_data_count {
                            let fallback_mat = if let Some(last_mat) = self._material_instance_data_list.last() {
                                last_mat.clone()
                            } else {
                                engine_resources
                                    .get_material_instance_data(
                                        crate::resource::resource::DEFAULT_MATERIAL_INSTANCE_NAME,
                                    )
                                    .clone()
                            };
                            self._material_instance_data_list.push(fallback_mat);
                        }
                        if geometry_data_count > 0 && self._material_instance_data_list.len() > geometry_data_count {
                            self._material_instance_data_list.truncate(geometry_data_count);
                        }

                        {
                            let mesh_borrow = loaded_mesh.borrow();
                            for socket_data_cell in self._socket_data_map.values() {
                                let mut socket_data = socket_data_cell.borrow_mut();
                                let mut parent_bone_index: usize = INVALID_BONE_INDEX;
                                for skeleton_data in mesh_borrow._skeleton_data_list.iter() {
                                    if let Some(bone_index) =
                                        skeleton_data._bone_index_map.get(&socket_data._parent_bone)
                                    {
                                        parent_bone_index = *bone_index;
                                        break;
                                    }
                                }
                                socket_data._parent_bone_index = parent_bone_index;
                            }
                        }

                        self._mesh_data = loaded_mesh;
                    }
                }
            }
        }
    }

    pub fn destroy_model_data(&self) {}

    pub fn is_render_camera(&self) -> bool {
        self._is_render_camera
    }

    pub fn set_render_camera(&mut self, render: bool) {
        self._is_render_camera = render;
    }

    pub fn is_render_shadow(&self) -> bool {
        self._is_render_shadow
    }

    pub fn set_render_shadow(&mut self, render: bool) {
        self._is_render_shadow = render;
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn has_skeletal_animation_or_material(&self) -> bool {
        if self._mesh_data.borrow().has_animation_data() {
            return true;
        }
        self._material_instance_data_list
            .iter()
            .any(|material_instance| material_instance.borrow().is_skeletal_material_instance())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scene::mesh::{MeshData, MeshDataCreateInfo};
    use crate::utilities::system::newRcRefCell;

    #[test]
    fn test_model_is_ready_with_empty_or_failed_mesh() {
        let empty_mesh = newRcRefCell(MeshData::create_mesh_data(
            &String::from("empty"),
            MeshDataCreateInfo::default(),
            Vec::new(),
        ));
        let model = ModelData {
            _model_data_name: String::from("test_model"),
            _mesh_name: String::from("non_existent_mesh"),
            _mesh_data: empty_mesh,
            _local_transform: nalgebra::Matrix4::identity(),
            _is_render_camera: false,
            _is_render_shadow: false,
            _material_instance_data_list: Vec::new(),
            _collision: CollisionData::default(),
            _socket_data_map: HashMap::new(),
        };

        // When engine core is invalid (in test environment), is_ready checks empty mesh:
        assert!(!model.is_ready());
    }
}
