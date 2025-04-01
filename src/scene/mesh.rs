use nalgebra::Vector3;
use serde::{Deserialize, Serialize};

use crate::scene::animation::{
    AnimationData, AnimationNodeCreateInfo, SkeletonData, SkeletonDataCreateInfo,
};
use crate::scene::bounding_box::BoundingBox;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::geometry_buffer::{GeometryCreateInfo, GeometryData};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(default)]
pub struct MeshDataCreateInfo {
    pub _bound_box: BoundingBox,
    pub _skeleton_create_infos: Vec<SkeletonDataCreateInfo>,
    pub _animation_node_create_infos: Vec<Vec<AnimationNodeCreateInfo>>,
    pub _geometry_create_infos: Vec<GeometryCreateInfo>,
}

#[derive(Clone, Debug)]
pub struct MeshData {
    pub _name: String,
    pub _bound_box: BoundingBox,
    pub _skeleton_data_list: Vec<SkeletonData>,
    pub _animation_data_list: Vec<AnimationData>,
    pub _geometry_data_list: Vec<RcRefCell<GeometryData>>,
}

impl Default for MeshDataCreateInfo {
    fn default() -> MeshDataCreateInfo {
        MeshDataCreateInfo {
            _bound_box: BoundingBox::default(),
            _skeleton_create_infos: Vec::new(),
            _animation_node_create_infos: Vec::new(),
            _geometry_create_infos: Vec::new(),
        }
    }
}

impl MeshDataCreateInfo {
    pub fn calc_mesh_bounding_box(geometry_create_infos: &Vec<GeometryCreateInfo>) -> BoundingBox {
        let mut bound_min = Vector3::new(f32::MAX, f32::MAX, f32::MAX) * 0.5;
        let mut bound_max = Vector3::new(f32::MIN, f32::MIN, f32::MIN) * 0.5;
        for geometry_data in geometry_create_infos.iter() {
            for i in 0..3 {
                if geometry_data._bounding_box._min[i] < bound_min[i] {
                    bound_min[i] = geometry_data._bounding_box._min[i];
                }
                if bound_max[i] < geometry_data._bounding_box._max[i] {
                    bound_max[i] = geometry_data._bounding_box._max[i];
                }
            }
        }

        BoundingBox::create_bounding_box(&bound_min, &bound_max)
    }
}

impl MeshData {
    pub fn create_mesh_data(
        mesh_name: &String,
        mesh_data_create_info: MeshDataCreateInfo,
        geometry_data_list: Vec<RcRefCell<GeometryData>>,
    ) -> MeshData {
        log::debug!("create_mesh_data: {}", mesh_name);
        let mut mesh_data = MeshData {
            _name: mesh_name.clone(),
            _bound_box: mesh_data_create_info._bound_box,
            _skeleton_data_list: mesh_data_create_info
                ._skeleton_create_infos
                .iter()
                .enumerate()
                .map(|(i, skeleton_create_info)| {
                    SkeletonData::create_skeleton_data(i, skeleton_create_info)
                })
                .collect(),
            _animation_data_list: Vec::new(),
            _geometry_data_list: geometry_data_list,
        };

        for (i, animation_node_create_info) in mesh_data_create_info
            ._animation_node_create_infos
            .iter()
            .enumerate()
        {
            mesh_data
                ._animation_data_list
                .push(AnimationData::create_animation_data(
                    &format!(
                        "{}_{}",
                        mesh_data._name, mesh_data._skeleton_data_list[i]._name
                    ),
                    i,
                    &mesh_data._skeleton_data_list[i],
                    animation_node_create_info,
                ));
        }

        mesh_data
    }

    pub fn has_animation_data(&self) -> bool {
        false == self._animation_data_list.is_empty()
    }

    pub fn get_geometry_data_count(&self) -> usize {
        self._geometry_data_list.len()
    }

    pub fn get_geometry_data_list(&self) -> &Vec<RcRefCell<GeometryData>> {
        &self._geometry_data_list
    }

    pub fn get_default_geometry_data(&self) -> &RcRefCell<GeometryData> {
        &self._geometry_data_list[0]
    }

    pub fn get_geometry_data(&self, index: usize) -> &RcRefCell<GeometryData> {
        &self._geometry_data_list[index]
    }

    pub fn update_mesh_data(&self) {}
}
