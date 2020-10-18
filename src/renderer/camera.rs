use nalgebra::{
    Vector3,
    Matrix4,
};

use crate::constants;
use crate::renderer::transform_object::TransformObjectData;

#[derive(Clone, Debug)]
pub struct CameraCreateInfo {
    meter_per_unit: f32,
    near: f32,
    far: f32,
    fov: f32,
    aspect: f32,
    position: Vector3<f32>,
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
    _name: String,
    _meter_per_unit: f32,
    _near: f32,
    _far: f32,
    _fov: f32,
    _aspect: f32,
    _view_matrix: Matrix4<f32>,
    _inv_view_matrix: Matrix4<f32>,
    _view_origin_matrix: Matrix4<f32>,
    _inv_view_origin_matrix: Matrix4<f32>,
    _projection_matrix: Matrix4<f32>,
    _inv_projection_matrix: Matrix4<f32>,
    _view_projection_matrix: Matrix4<f32>,
    _inv_view_projection_matrix: Matrix4<f32>,
    _view_origin_projection_matrix: Matrix4<f32>,
    _inv_view_origin_projection_matrix: Matrix4<f32>,
    _view_origin_projection_matrix_prev: Matrix4<f32>,
    _transform_object: TransformObjectData
}

impl CameraObjectData {
    pub fn create_camera_object_data(name: &String, camera_create_info: &CameraCreateInfo) -> CameraObjectData {
        log::info!("createCameraObjectData: {:?}", name);
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
            _transform_object: TransformObjectData::new_transform_object_data()
        };
        // initialize
        camera_object_data._transform_object.set_position(&camera_create_info.position);
        camera_object_data._transform_object.update_transform_object(false);
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

    // pub fn set_aspect(&mut self, camera_object_data: &CameraObjectData, aspect: f32) {
    //     self._aspect = aspect;
    //     self.update_projection_matrix();
    // }

    // pub fn update_projection_matrix(&mut self) {
    //     let fovy = self._fov / 360.0 * math::TWO_PI;
    //     let proj = Perspective3::new(self._aspect, fovy, self._near, self._far);
    //     let projection_matrix =
    //     fov <- read_fov
    //     aspect <- read_aspect
    //     near <- read_near
    //     far <- read_far
    //     let projectionMatrix = contract (perspective near far (fov/360.0 * 2.0 * pi) aspect) clipSpaceMatrix
    //     write_projectionMatrix projectionMatrix
    //     write_invProjectionMatrix (inverse projectionMatrix)
    // }
    //
    // updateCameraObjectData: CameraObjectData -> IO ()
    // updateCameraObjectData cameraObjectData@CameraObjectData {..} = do
    //     updated <- updateTransformObject _transformObject
    //     projectionMatrix <- read_projectionMatrix
    //     invProjectionMatrix <- read_invProjectionMatrix
    //     viewMatrix <- getInverseMatrix _transformObject
    //     invViewMatrix <- getMatrix _transformObject
    //     write_viewMatrix viewMatrix
    //     write_invViewMatrix invViewMatrix
    //     write_viewProjectionMatrix (contract viewMatrix projectionMatrix)
    //     write_invViewProjectionMatrix (contract invProjectionMatrix invViewMatrix)
    //     let viewOriginMatrix = DF4 (viewMatrix .! Idx 0) (viewMatrix .! Idx 1) (viewMatrix .! Idx 2) (vec4 0 0 0 1)
    //         invViewOriginMatrix = (transpose viewOriginMatrix)
    //     write_viewOriginMatrix viewOriginMatrix
    //     write_invViewOriginMatrix invViewOriginMatrix
    //     viewOriginProjectionMatrixPrev <- read_viewOriginProjectionMatrix
    //     write_viewOriginProjectionMatrixPrev viewOriginProjectionMatrixPrev
    //     write_viewOriginProjectionMatrix (contract viewOriginMatrix projectionMatrix)
    //     write_invViewOriginProjectionMatrix (contract invProjectionMatrix invViewOriginMatrix)
    //

}