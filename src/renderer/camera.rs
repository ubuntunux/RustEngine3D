use nalgebra::linalg;
use nalgebra::{
    Vector3,
    Vector4,
    Matrix4,
};

use crate::constants;
use crate::utilities::math;
use crate::renderer::transform_object::TransformObjectData;

#[derive(Clone, Debug)]
pub struct CameraCreateInfo {
    pub meter_per_unit: f32,
    pub near: f32,
    pub far: f32,
    pub fov: f32,
    pub aspect: f32,
    pub position: Vector3<f32>,
}

impl Default for CameraCreateInfo {
    fn default() -> CameraCreateInfo {
        CameraCreateInfo {
            meter_per_unit: constants::METER_PER_UNIT,
            near: constants::NEAR,
            far: constants::FAR,
            fov: constants::FOV,
            aspect: 1.0,
            position: Vector3::zeros(),
        }
    }
}

pub struct CameraObjectData {
    pub _name: String,
    pub _meter_per_unit: f32,
    pub _near: f32,
    pub _far: f32,
    pub _fov: f32,
    pub _aspect: f32,
    pub _view_matrix: Matrix4<f32>,
    pub _inv_view_matrix: Matrix4<f32>,
    pub _view_origin_matrix: Matrix4<f32>,
    pub _inv_view_origin_matrix: Matrix4<f32>,
    pub _projection_matrix: Matrix4<f32>,
    pub _inv_projection_matrix: Matrix4<f32>,
    pub _view_projection_matrix: Matrix4<f32>,
    pub _inv_view_projection_matrix: Matrix4<f32>,
    pub _view_origin_projection_matrix: Matrix4<f32>,
    pub _inv_view_origin_projection_matrix: Matrix4<f32>,
    pub _view_origin_projection_matrix_prev: Matrix4<f32>,
    pub _transform_object: TransformObjectData,
    pub _updated: bool,
}

impl CameraObjectData {
    pub fn create_camera_object_data(name: &String, camera_create_info: &CameraCreateInfo) -> CameraObjectData {
        log::info!("create_camera_object_data: {:?}", name);
        let mut camera_object_data = CameraObjectData {
            _name: name.clone(),
            _meter_per_unit: camera_create_info.meter_per_unit,
            _near: camera_create_info.near,
            _far: camera_create_info.far,
            _fov: camera_create_info.fov,
            _aspect: camera_create_info.aspect,
            _view_matrix: Matrix4::identity(),
            _inv_view_matrix: Matrix4::identity(),
            _view_origin_matrix: Matrix4::identity(),
            _inv_view_origin_matrix: Matrix4::identity(),
            _projection_matrix: Matrix4::identity(),
            _inv_projection_matrix: Matrix4::identity(),
            _view_projection_matrix: Matrix4::identity(),
            _inv_view_projection_matrix: Matrix4::identity(),
            _view_origin_projection_matrix: Matrix4::identity(),
            _inv_view_origin_projection_matrix: Matrix4::identity(),
            _view_origin_projection_matrix_prev: Matrix4::identity(),
            _transform_object: TransformObjectData::new_transform_object_data(),
            _updated: true,
        };
        // initialize
        camera_object_data._transform_object.set_position(&camera_create_info.position);
        camera_object_data._transform_object.update_transform_object();
        camera_object_data
    }
    pub fn get_camera_position(&self) -> &Vector3<f32> {
        &self._transform_object.get_position()
    }
    pub fn get_camera_position_prev(&self) -> &Vector3<f32> {
        &self._transform_object.get_prev_position()
    }
    pub fn get_view_matrix(&self) -> &Matrix4<f32> {
        &self._view_matrix
    }
    pub fn get_inv_view_matrix(&self) -> &Matrix4<f32> {
        &self._inv_view_matrix
    }
    pub fn get_view_origin_matrix(&self) -> &Matrix4<f32> {
        &self._view_origin_matrix
    }
    pub fn get_inv_view_origin_matrix(&self) -> &Matrix4<f32> {
        &self._inv_view_origin_matrix
    }
    pub fn get_projection_matrix(&self) -> &Matrix4<f32> {
        &self._projection_matrix
    }
    pub fn get_inv_projection_matrix(&self) -> &Matrix4<f32> {
        &self._inv_projection_matrix
    }
    pub fn get_view_projection_matrix(&self) -> &Matrix4<f32> {
        &self._view_projection_matrix
    }
    pub fn get_inv_view_projection_matrix(&self) -> &Matrix4<f32> {
        &self._inv_view_projection_matrix
    }
    pub fn get_view_origin_projection_matrix(&self) -> &Matrix4<f32> {
        &self._view_origin_projection_matrix
    }
    pub fn get_inv_view_origin_projection_matrix(&self) -> &Matrix4<f32> {
        &self._inv_view_origin_projection_matrix
    }
    pub fn get_view_origin_projection_matrix_prev(&self) -> &Matrix4<f32> {
        &self._view_origin_projection_matrix_prev
    }
    pub fn set_aspect(&mut self, aspect: f32) {
        self._aspect = aspect;
        self.update_projection_matrix();
    }
    pub fn update_projection_matrix(&mut self) {
        self._updated = true;
        self._projection_matrix = math::get_clip_space_matrix() * math::perspective(self._aspect, self._fov, self._near, self._far);
        linalg::try_invert_to(self._projection_matrix.into(), &mut self._inv_projection_matrix);
    }
    pub fn update_camera_object_data(&mut self) {
        let updated = self._transform_object.update_transform_object();
        if updated || self._updated {
            // view matrix is inverse matrix of transform, cause it's camera.
            self._view_matrix = self._transform_object.get_inverse_matrix().clone() as Matrix4<f32>;
            self._inv_view_matrix = self._transform_object.get_matrix().clone() as Matrix4<f32>;
            self._view_origin_matrix.set_column(0, &self._view_matrix.column(0));
            self._view_origin_matrix.set_column(1, &self._view_matrix.column(1));
            self._view_origin_matrix.set_column(2, &self._view_matrix.column(2));
            self._view_origin_matrix.set_column(3, &Vector4::new(0.0, 0.0, 0.0, 1.0));
            self._inv_view_origin_matrix = self._view_origin_matrix.transpose();
            self._view_projection_matrix = &self._projection_matrix * &self._view_matrix;
            self._inv_view_projection_matrix = &self._inv_view_matrix * &self._inv_projection_matrix;
            self._view_origin_projection_matrix_prev = self._view_origin_projection_matrix.clone() as Matrix4<f32>;
            self._view_origin_projection_matrix = &self._projection_matrix * &self._view_origin_matrix;
            self._inv_view_origin_projection_matrix = &self._inv_view_origin_matrix * &self._inv_projection_matrix;
            self._updated = false;
        }
    }
}