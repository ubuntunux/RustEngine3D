use nalgebra::linalg;
use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix4,
};
use serde::{ Serialize, Deserialize };

use crate::constants;
use crate::utilities::math;
use crate::renderer::transform_object::TransformObjectData;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct CameraCreateInfo {
    pub meter_per_unit: f32,
    pub near: f32,
    pub far: f32,
    pub fov: f32,
    pub window_size: Vector2<i32>,
    pub enable_jitter: bool,
    pub position: Vector3<f32>,
    pub rotation: Vector3<f32>,
}

impl Default for CameraCreateInfo {
    fn default() -> CameraCreateInfo {
        unsafe {
            CameraCreateInfo {
                meter_per_unit: constants::METER_PER_UNIT,
                near: constants::NEAR,
                far: constants::FAR,
                fov: constants::FOV,
                window_size: Vector2::new(1024, 768),
                enable_jitter: true,
                position: Vector3::zeros(),
                rotation: Vector3::zeros(),
            }
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
    pub _view: Matrix4<f32>,
    pub _inv_view: Matrix4<f32>,
    pub _view_origin: Matrix4<f32>,
    pub _inv_view_origin: Matrix4<f32>,
    pub _projection: Matrix4<f32>,
    pub _inv_projection: Matrix4<f32>,
    pub _view_projection: Matrix4<f32>,
    pub _inv_view_projection: Matrix4<f32>,
    pub _view_origin_projection: Matrix4<f32>,
    pub _inv_view_origin_projection: Matrix4<f32>,
    pub _view_origin_projection_prev: Matrix4<f32>,
    pub _projection_jitter: Matrix4<f32>,
    pub _inv_projection_jitter: Matrix4<f32>,
    pub _view_projection_jitter: Matrix4<f32>,
    pub _inv_view_projection_jitter: Matrix4<f32>,
    pub _view_origin_projection_jitter: Matrix4<f32>,
    pub _inv_view_origin_projection_jitter: Matrix4<f32>,
    pub _view_origin_projection_prev_jitter: Matrix4<f32>,
    pub _view_frustum_planes: [Vector3<f32>; 4],
    pub _transform_object: TransformObjectData,
    pub _window_size: Vector2<i32>,
    pub _jitter_mode_uniform2x: [Vector2<f32>; 2],
    pub _jitter_mode_hammersley4x: [Vector2<f32>; 4],
    pub _jitter_mode_hammersley8x: [Vector2<f32>; 8],
    pub _jitter_mode_hammersley16x: [Vector2<f32>; 16],
    pub _jitter: Vector2<f32>,
    pub _jitter_prev: Vector2<f32>,
    pub _jitter_delta: Vector2<f32>,
    pub _jitter_frame: i32,
    pub _enable_jitter: bool,
    pub _updated_projection: bool,
}

impl CameraObjectData {
    pub fn create_camera_object_data(name: &String, camera_create_info: &CameraCreateInfo) -> CameraObjectData {
        log::debug!("create_camera_object_data: {:?}", name);
        let mut camera_object_data = CameraObjectData {
            _name: name.clone(),
            _meter_per_unit: camera_create_info.meter_per_unit,
            _near: camera_create_info.near,
            _far: camera_create_info.far,
            _fov: camera_create_info.fov,
            _aspect: 1.0,
            _window_size: camera_create_info.window_size.into(),
            _view: Matrix4::identity(),
            _inv_view: Matrix4::identity(),
            _view_origin: Matrix4::identity(),
            _inv_view_origin: Matrix4::identity(),
            _projection: Matrix4::identity(),
            _inv_projection: Matrix4::identity(),
            _view_projection: Matrix4::identity(),
            _inv_view_projection: Matrix4::identity(),
            _view_origin_projection: Matrix4::identity(),
            _inv_view_origin_projection: Matrix4::identity(),
            _view_origin_projection_prev: Matrix4::identity(),
            _projection_jitter: Matrix4::identity(),
            _inv_projection_jitter: Matrix4::identity(),
            _view_projection_jitter: Matrix4::identity(),
            _inv_view_projection_jitter: Matrix4::identity(),
            _view_origin_projection_jitter: Matrix4::identity(),
            _inv_view_origin_projection_jitter: Matrix4::identity(),
            _view_origin_projection_prev_jitter: Matrix4::identity(),
            _view_frustum_planes: [Vector3::zeros(); 4],
            _transform_object: TransformObjectData::new_transform_object_data(),
            _jitter_mode_uniform2x: [Vector2::zeros(); 2],
            _jitter_mode_hammersley4x: [Vector2::zeros(); 4],
            _jitter_mode_hammersley8x: [Vector2::zeros(); 8],
            _jitter_mode_hammersley16x: [Vector2::zeros(); 16],
            _jitter: Vector2::new(0.0, 0.0),
            _jitter_prev: Vector2::new(0.0, 0.0),
            _jitter_delta: Vector2::new(0.0, 0.0),
            _jitter_frame: 0,
            _enable_jitter: camera_create_info.enable_jitter,
            _updated_projection: true,
        };

        // initialize
        camera_object_data.set_aspect(camera_create_info.window_size.x, camera_create_info.window_size.y);
        camera_object_data._transform_object.set_position(&camera_create_info.position);
        camera_object_data._transform_object.set_rotation(&camera_create_info.rotation);
        camera_object_data._transform_object.update_transform_object();
        camera_object_data._jitter_mode_uniform2x = [Vector2::new(0.25, 0.75) * 2.0 - Vector2::new(1.0, 1.0), Vector2::new(0.5, 0.5) * 2.0 - Vector2::new(1.0, 1.0)];
        for i in 0..4 {
            camera_object_data._jitter_mode_hammersley4x[i] = math::hammersley_2d(i as u32, 4) * 2.0 - Vector2::new(1.0, 1.0);
        }
        for i in 0..8 {
            camera_object_data._jitter_mode_hammersley8x[i] = math::hammersley_2d(i as u32, 8) * 2.0 - Vector2::new(1.0, 1.0);
        }
        for i in 0..16 {
            camera_object_data._jitter_mode_hammersley16x[i] = math::hammersley_2d(i as u32, 16) * 2.0 - Vector2::new(1.0, 1.0);
        }
        camera_object_data
    }
    pub fn get_camera_front(&self) -> &Vector3<f32> {
        &self._transform_object.get_front()
    }
    pub fn get_camera_up(&self) -> &Vector3<f32> {
        &self._transform_object.get_up()
    }
    pub fn get_camera_left(&self) -> &Vector3<f32> {
        &self._transform_object.get_left()
    }
    pub fn get_camera_position(&self) -> &Vector3<f32> {
        &self._transform_object.get_position()
    }
    pub fn get_camera_position_prev(&self) -> &Vector3<f32> { &self._transform_object.get_prev_position() }
    pub fn set_aspect(&mut self, window_width: i32, window_height: i32) {
        let aspect: f32 = if 0 != window_height { window_width as f32 / window_height as f32 } else { 1.0 };
        self._window_size.x = window_width;
        self._window_size.y = window_height;
        self._aspect = aspect;
        self.update_projection();
    }

    pub fn convert_world_to_screen(&self, world_pos: &Vector3<f32>, clamp: bool) -> Vector2<f32> {
        let mut screen_pos = math::convert_to_screen_texcoord(&self._view_projection, world_pos, clamp);
        screen_pos.x *= self._window_size.x as f32;
        screen_pos.y *= self._window_size.y as f32;
        screen_pos
    }

    pub fn convert_screen_to_world(&self, screen_pos: &Vector2<i32>) -> Vector3<f32> {
        const DEPTH: f32 = 0.0;
        let ndc: Vector4<f32> = Vector4::new(
            (screen_pos.x as f32 / self._window_size.x as f32) * 2.0 - 1.0,
            (screen_pos.y as f32 / self._window_size.y as f32) * 2.0 - 1.0,
            DEPTH,
            1.0
        );
        let mut world_pos: Vector4<f32> = &self._inv_view_projection * ndc;
        world_pos.x /= world_pos.w;
        world_pos.y /= world_pos.w;
        world_pos.z /= world_pos.w;
        Vector3::new(world_pos.x, world_pos.y, world_pos.z)
    }

    pub fn convert_screen_to_relative_world(&self, screen_pos: &Vector2<i32>) -> Vector3<f32> {
        const DEPTH: f32 = 0.0;
        let ndc: Vector4<f32> = Vector4::new(
            (screen_pos.x as f32 / self._window_size.x as f32) * 2.0 - 1.0,
            (screen_pos.y as f32 / self._window_size.y as f32) * 2.0 - 1.0,
            DEPTH,
            1.0
        );
        let mut world_pos: Vector4<f32> = &self._inv_view_origin_projection * ndc;
        world_pos.x /= world_pos.w;
        world_pos.y /= world_pos.w;
        world_pos.z /= world_pos.w;
        Vector3::new(world_pos.x, world_pos.y, world_pos.z)
    }

    pub fn update_projection(&mut self) {
        self._projection = math::get_clip_space_matrix() * math::perspective(self._aspect, self._fov, self._near, self._far);
        self._projection_jitter.copy_from(&self._projection);
        linalg::try_invert_to(self._projection.into(), &mut self._inv_projection);
        self._inv_projection_jitter.copy_from(&self._inv_projection);
        self._updated_projection = true;
    }

    pub fn update_view_frustum_planes(&mut self) {
        // Left
        self._view_frustum_planes[0].x = self._view_origin_projection.m41 + self._view_origin_projection.m11;
        self._view_frustum_planes[0].y = self._view_origin_projection.m42 + self._view_origin_projection.m12;
        self._view_frustum_planes[0].z = self._view_origin_projection.m43 + self._view_origin_projection.m13;
        self._view_frustum_planes[0] = -self._transform_object.get_up().cross(&self._view_frustum_planes[0].normalize());

        // Right
        self._view_frustum_planes[1].x = self._view_origin_projection.m41 - self._view_origin_projection.m11;
        self._view_frustum_planes[1].y = self._view_origin_projection.m42 - self._view_origin_projection.m12;
        self._view_frustum_planes[1].z = self._view_origin_projection.m43 - self._view_origin_projection.m13;
        self._view_frustum_planes[1] = self._transform_object.get_up().cross(&self._view_frustum_planes[1].normalize());

        // Top
        self._view_frustum_planes[2].x = self._view_origin_projection.m41 - self._view_origin_projection.m21;
        self._view_frustum_planes[2].y = self._view_origin_projection.m42 - self._view_origin_projection.m22;
        self._view_frustum_planes[2].z = self._view_origin_projection.m43 - self._view_origin_projection.m23;
        self._view_frustum_planes[2] = self._transform_object.get_left().cross(&self._view_frustum_planes[2].normalize());

        // Bottom
        self._view_frustum_planes[3].x = self._view_origin_projection.m41 + self._view_origin_projection.m21;
        self._view_frustum_planes[3].y = self._view_origin_projection.m42 + self._view_origin_projection.m22;
        self._view_frustum_planes[3].z = self._view_origin_projection.m43 + self._view_origin_projection.m23;
        self._view_frustum_planes[3] = -self._transform_object.get_left().cross(&self._view_frustum_planes[3].normalize());
    }

    pub fn update_camera_object_data(&mut self) {
        if self._enable_jitter {
            self._jitter_frame = (self._jitter_frame + 1) % self._jitter_mode_hammersley16x.len() as i32;
            // offset of camera projection matrix. NDC Space -1.0 ~ 1.0
            self._jitter_prev = self._jitter.into();
            self._jitter = self._jitter_mode_hammersley16x[self._jitter_frame as usize].into();
            self._jitter[0] /= self._window_size.x as f32;
            self._jitter[1] /= self._window_size.y as f32;
            // Multiplies by 0.5 because it is in screen coordinate system. 0.0 ~ 1.0
            self._jitter_delta = (&self._jitter - &self._jitter_prev) * 0.5;
        }

        // copy prev matrices
        self._view_origin_projection_prev.copy_from(&self._view_origin_projection);
        self._view_origin_projection_prev_jitter.copy_from(&self._view_origin_projection_jitter);

        // update matrices
        let updated = self._transform_object.update_transform_object() || self._updated_projection;
        self._updated_projection = false;

        if updated {
            // view matrix is inverse matrix of transform, cause it's camera.
            self._view = self._transform_object.get_inverse_matrix().clone() as Matrix4<f32>;
            self._inv_view = self._transform_object.get_matrix().clone() as Matrix4<f32>;
            self._view_origin.set_column(0, &self._view.column(0));
            self._view_origin.set_column(1, &self._view.column(1));
            self._view_origin.set_column(2, &self._view.column(2));
            self._view_origin.set_column(3, &Vector4::new(0.0, 0.0, 0.0, 1.0));
            self._inv_view_origin = self._view_origin.transpose();
            self._view_projection = &self._projection * &self._view;
            self._inv_view_projection = &self._inv_view * &self._inv_projection;
            self._view_origin_projection = &self._projection * &self._view_origin;
            self._inv_view_origin_projection = &self._inv_view_origin * &self._inv_projection;

            self.update_view_frustum_planes();
        }

        // Update projection jitter
        if self._enable_jitter {
            self._projection_jitter.column_mut(2)[0] = -self._jitter[0];
            self._projection_jitter.column_mut(2)[1] = -self._jitter[1];
            linalg::try_invert_to(self._projection_jitter.into(), &mut self._inv_projection_jitter);
            self._view_projection_jitter = &self._projection_jitter * &self._view;
            self._view_origin_projection_jitter = &self._projection_jitter * &self._view_origin;
            linalg::try_invert_to(self._view_origin_projection_jitter.into(), &mut self._inv_view_origin_projection_jitter);
        } else if updated {
            self._view_projection_jitter = self._view_projection.into();
            self._view_origin_projection_jitter = self._view_origin_projection.into();
            self._inv_view_origin_projection_jitter = self._inv_view_origin_projection.into();
        }
    }
}