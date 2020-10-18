use nalgebra::linalg;
use nalgebra::{
    Vector3,
    Vector4,
    Matrix4,
    Quaternion,
};

use crate::utilities::math::{
    get_world_left,
    get_world_up,
    get_world_front,
    make_rotation_matrix,
    make_matrix
};

#[derive(Debug, Clone)]
pub struct TransformObjectData {
    _updated: bool,
    _front: Vector3<f32>,
    _left: Vector3<f32>,
    _up: Vector3<f32>,
    _position: Vector3<f32>,
    _rotation: Vector3<f32>,
    _scale: Vector3<f32>,
    _euler_to_quaternion: Quaternion<f32>,
    _quaternion: Quaternion<f32>,
    _fianl_quaternion: Quaternion<f32>,
    _prev_position: Vector3<f32>,
    _prev_position_store: Vector3<f32>,
    _prev_rotation: Vector3<f32>,
    _prev_scale: Vector3<f32>,
    _prev_euler_to_quaternion: Quaternion<f32>,
    _prev_quaternion: Quaternion<f32>,
    _prev_fianl_quaternion: Quaternion<f32>,
    _quaternion_matrix: Matrix4<f32>,
    _euler_matrix: Matrix4<f32>,
    _rotation_matrix : Matrix4<f32>,
    _matrix: Matrix4<f32>,
    _inverse_matrix: Matrix4<f32>,
    _prev_matrix: Matrix4<f32>,
    _prev_inverse_matrix: Matrix4<f32>,
}

impl TransformObjectData {
    pub fn new_transform_object_data() -> TransformObjectData {
        TransformObjectData {
            _updated: true,
            _front: get_world_front(),
            _left: get_world_left(),
            _up: get_world_up(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _euler_to_quaternion: Quaternion::identity(),
            _quaternion: Quaternion::identity(),
            _fianl_quaternion: Quaternion::identity(),
            _prev_position: Vector3::zeros(),
            _prev_position_store: Vector3::zeros(),
            _prev_rotation: Vector3::zeros(),
            _prev_scale: Vector3::new(1.0, 1.0, 1.0),
            _prev_euler_to_quaternion: Quaternion::identity(),
            _prev_quaternion: Quaternion::identity(),
            _prev_fianl_quaternion: Quaternion::identity(),
            _quaternion_matrix: Matrix4::identity(),
            _euler_matrix: Matrix4::identity(),
            _rotation_matrix : Matrix4::identity(),
            _matrix: Matrix4::identity(),
            _inverse_matrix: Matrix4::identity(),
            _prev_matrix: Matrix4::identity(),
            _prev_inverse_matrix: Matrix4::identity(),
        }
    }
    pub fn get_matrix(&self) -> &Matrix4<f32> {
        &self._matrix
    }
    pub fn get_inverse_matrix(&self) -> &Matrix4<f32> {
        &self._inverse_matrix
    }
    pub fn get_left(&self) -> &Vector3<f32> {
        &self._left
    }
    pub fn get_front(&self) -> &Vector3<f32> {
        &self._front
    }
    pub fn get_up(&self) -> &Vector3<f32> {
        &self._up
    }
    pub fn get_position(&self) -> &Vector3<f32> {
        &self._position
    }
    pub fn set_position(&mut self, position: &Vector3<f32>) {
        self._position.copy_from(position);
    }
    pub fn get_prev_position(&self) -> &Vector3<f32> {
        &self._prev_position
    }
    pub fn move_left(&mut self, move_speed: f32) {
        self._position += (&self._left * move_speed) as Vector3<f32>;
    }
    pub fn move_up(&mut self, move_speed: f32) {
        self._position += (&self._up * move_speed) as Vector3<f32>;
    }
    pub fn move_front(&mut self, move_speed: f32) {
        self._position += (&self._front * move_speed) as Vector3<f32>;
    }
    pub fn get_rotation(&self) -> &Vector3<f32> {
        &self._rotation
    }
    pub fn set_rotation(&mut self, rotation: &Vector3<f32>) {
        self._rotation.copy_from(rotation);
        self._rotation.x = self._rotation.x % std::f32::consts::PI;
        self._rotation.y = self._rotation.y % std::f32::consts::PI;
        self._rotation.z = self._rotation.z % std::f32::consts::PI;
    }
    pub fn rotation_pitch(&mut self, rotation_speed: f32) {
        self._rotation.x = (self._rotation.x + rotation_speed) % std::f32::consts::PI;
    }
    pub fn rotation_yaw(&mut self, rotation_speed: f32) {
        self._rotation.y = (self._rotation.y + rotation_speed) % std::f32::consts::PI;
    }
    pub fn rotation_roll(&mut self, rotation_speed: f32) {
        self._rotation.z = (self._rotation.z + rotation_speed) % std::f32::consts::PI;
    }
    pub fn get_scale(&mut self) -> &Vector3<f32> {
        &self._scale
    }
    pub fn set_scale(&mut self, scale: &Vector3<f32>) {
        self._scale.copy_from(scale);
    }
    pub fn set_scale_x(&mut self, scale: f32) {
        self._scale.x = scale;
    }
    pub fn set_scale_y(&mut self, scale: f32) {
        self._scale.y = scale;
    }
    pub fn set_scale_z(&mut self, scale: f32) {
        self._scale.z = scale;
    }
    pub fn update_transform_object(&mut self) -> bool {
        self.update_transform_object_func(false)
    }
    pub fn force_update_transform_object(&mut self) -> bool {
        self.update_transform_object_func(true)
    }
    pub fn update_transform_object_func(&mut self, force_update: bool) -> bool {
        let prev_updated: bool = self._updated;
        self._prev_position_store.copy_from(&self._prev_position);
        let updated_position = force_update || self._prev_position != self._position;
        if updated_position {
            self._position.copy_from(&self._prev_position);
        }

        let updated_rotation = force_update || self._prev_rotation != self._rotation;
        if updated_rotation {
            self._rotation.copy_from(&self._prev_rotation);
        }

        let updated_scale = force_update || self._prev_scale != self._scale;
        if updated_scale {
            self._scale.copy_from(&self._prev_scale);
        }

        let updated = updated_position || updated_rotation || updated_scale;
        if prev_updated || updated {
            self._prev_matrix.copy_from(&self._matrix);
            self._prev_inverse_matrix.copy_from(&self._inverse_matrix);
        }

        if updated_rotation {
            // just rotation
            let rotation_matrix = make_rotation_matrix(self._rotation.x, self._rotation.y, self._rotation.z);
            let left: Vector4<f32> = rotation_matrix.column(0).clone().into();
            let up: Vector4<f32> = rotation_matrix.column(1).clone().into();
            let front: Vector4<f32> = rotation_matrix.column(2).clone().into();
//          look at algorithm
//             let sin_pitch = self._rotation._x.sin();
//             let cos_pitch = self._rotation._x.cos();
//             let sin_yaw = self._rotation._y.sin();
//             let cos_yaw = self._rotation._y.cos();
//             let front = Vector3::new(cos_pitch * sin_yaw, -sin_pitch, cos_pitch * cos_yaw).normalize();
//             let left = Vector3::new(0.0, 1.0, 0.0).cross(&front);
//             let up = front.cross(&left);
//             let rotation_matrix = Matrix4::from_columns(&[
//                 Vector4::new(left.x, left.y, left.z, 0.0),
//                 Vector4::new(up.x, up.y, up.z, 0.0),
//                 Vector4::new(front.x, front.y, front.z, 0.0),
//                 Vector4::new(0.0, 0.0, 0.0, 1.0),
//             ]);
            self._left.x = left.x;
            self._left.y = left.y;
            self._left.z = left.z;
            self._left.copy_from(&self._left.normalize());
            self._up.x = up.x;
            self._up.y = up.y;
            self._up.z = up.z;
            self._up.copy_from(&self._up.normalize());
            self._front.x = front.x;
            self._front.y = front.y;
            self._front.z = front.z;
            self._front.copy_from(&self._front.normalize());
        }

        if updated {
            self._matrix.copy_from(&make_matrix(&self._position, &self._rotation_matrix, &self._scale));
            //self._inverse_matrix.copy_from(&inverse_transform_matrix(&self._position, &self._rotation_matrix, &self._scale));
            linalg::try_invert_to(self._matrix.into(), &mut self._inverse_matrix);
        }
        self._updated = updated;
        updated
    }
}