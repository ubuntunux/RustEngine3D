use crate::vulkan_context::geometry_buffer::GeometryData;
use crate::utilities::system::{ RcRefCell };

#[derive(Clone, Debug)]
pub struct MeshData {
    pub _name: String,
    pub _bound_box: bool,
    pub _skeleton_datas: Vec<bool>,
    pub _animation_datas: Vec<bool>,
    pub _geometry_datas: Vec<RcRefCell<GeometryData>>,
}


impl MeshData {
    pub fn new_mesh_data(mesh_name: &String, geometry_buffer_datas: Vec<RcRefCell<GeometryData>>) -> MeshData {
        log::info!("new_mesh_data: {}", mesh_name);
        MeshData {
            _name: mesh_name.clone(),
            _bound_box: false,
            _skeleton_datas: Vec::new(),
            _animation_datas: Vec::new(),
            _geometry_datas: geometry_buffer_datas,
        }
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