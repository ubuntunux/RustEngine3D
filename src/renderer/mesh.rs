
use crate::vulkan_context::geometry_buffer::GeometryData;

#[derive(Clone, Debug)]
pub struct MeshData {
    _name: String,
    _bound_box: bool,
    _skeleton_datas: Vec<bool>,
    _animation_datas: Vec<bool>,
    _geometry_buffer_datas: Vec<GeometryData>,
}


impl MeshData {
    pub fn new_mesh_data(mesh_name: &String, geometry_buffer_datas: Vec<GeometryData>) -> MeshData {
        log::info!("new_mesh_data: {}", mesh_name);
        MeshData {
            _name: mesh_name.clone(),
            _bound_box: false,
            _skeleton_datas: Vec::new(),
            _animation_datas: Vec::new(),
            _geometry_buffer_datas: geometry_buffer_datas,
        }
    }

    pub fn get_geometry_data_count(&self) -> usize {
        self._geometry_buffer_datas.len()
    }

    pub fn get_geomtry_datas(&self) -> &Vec<GeometryData> {
        &self._geometry_buffer_datas
    }

    pub fn get_default_geometry_data(&self) -> &GeometryData {
        &self._geometry_buffer_datas[0]
    }

    pub fn get_geomtry_data(&self, index: usize) -> &GeometryData {
        &self._geometry_buffer_datas[index]
    }

    pub fn update_mesh_data(&self) {
    }
}