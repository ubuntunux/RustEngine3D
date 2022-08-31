use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3 };

use crate::renderer::animation::{
    AnimationNodeCreateInfo,
    AnimationData,
    SkeletonDataCreateInfo,
    SkeletonData,
};
use crate::vulkan_context::geometry_buffer::{ GeometryData, GeometryCreateInfo };
use crate::utilities::system::{ RcRefCell };
use crate::utilities::bounding_box::{ BoundingBox };

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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
    pub _skeleton_datas: Vec<SkeletonData>,
    pub _animation_datas: Vec<AnimationData>,
    pub _geometry_datas: Vec<RcRefCell<GeometryData>>,
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
    pub fn create_mesh_data_crate_info(mut mesh_data_create_info: MeshDataCreateInfo) -> MeshDataCreateInfo {
        let mut bound_min = Vector3::new(std::f32::MAX, std::f32::MAX, std::f32::MAX) * 0.5;
        let mut bound_max = Vector3::new(std::f32::MIN, std::f32::MIN, std::f32::MIN) * 0.5;
        for geometry_data in mesh_data_create_info._geometry_create_infos.iter() {
            for i in 0..3 {
                if geometry_data._bounding_box._min[i] < bound_min[i] {
                    bound_min[i] = geometry_data._bounding_box._min[i];
                }
                if bound_max[i] < geometry_data._bounding_box._max[i] {
                    bound_max[i] = geometry_data._bounding_box._max[i];
                }
            }
        }

        mesh_data_create_info._bound_box = BoundingBox {
            _min: bound_min,
            _max: bound_max,
            _center: &bound_max * 0.5 + &bound_min * 0.5,
            _size: &bound_max - &bound_min,
            _radius: (&bound_max * 0.5 - &bound_min * 0.5).norm(),
        };
        mesh_data_create_info
    }
}


impl MeshData {
    pub fn create_mesh_data(
        mesh_name: &String,
        mesh_data_create_info: MeshDataCreateInfo,
        geometry_datas: Vec<RcRefCell<GeometryData>>
    ) -> MeshData {
        log::debug!("create_mesh_data: {}", mesh_name);
        let mut mesh_data = MeshData {
            _name: mesh_name.clone(),
            _bound_box: mesh_data_create_info._bound_box,
            _skeleton_datas: mesh_data_create_info._skeleton_create_infos
                .iter()
                .enumerate()
                .map(|(i, skeleton_create_info)| {
                    SkeletonData::create_skeleton_data(i, skeleton_create_info)
                }).collect(),
            _animation_datas: Vec::new(),
            _geometry_datas: geometry_datas,
        };

        for (i, animation_node_create_info) in mesh_data_create_info._animation_node_create_infos.iter().enumerate() {
            mesh_data._animation_datas.push(AnimationData::create_animation_data(
                &format!("{}_{}", mesh_data._name, mesh_data._skeleton_datas[i]._name),
                i,
                &mesh_data._skeleton_datas[i],
                animation_node_create_info,
            ));
        }

        mesh_data
    }

    pub fn has_animation_data(&self) -> bool {
        false == self._animation_datas.is_empty()
    }

    pub fn get_geometry_data_count(&self) -> usize {
        self._geometry_datas.len()
    }

    pub fn get_geomtry_datas(&self) -> &Vec<RcRefCell<GeometryData>> {
        &self._geometry_datas
    }

    pub fn get_default_geometry_data(&self) -> &RcRefCell<GeometryData> {
        &self._geometry_datas[0]
    }

    pub fn get_geomtry_data(&self, index: usize) -> &RcRefCell<GeometryData> {
        &self._geometry_datas[index]
    }

    pub fn update_mesh_data(&self) {
    }
}