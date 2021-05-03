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
    combinate_matrix,
    TWO_PI,
};

#[derive(Debug, Clone)]
pub struct TransformObjectData {
    pub _updated: bool,
    pub _front: Vector3<f32>,
    pub _left: Vector3<f32>,
    pub _up: Vector3<f32>,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _euler_to_quaternion: Quaternion<f32>,
    pub _quaternion: Quaternion<f32>,
    pub _fianl_quaternion: Quaternion<f32>,
    pub _prev_position: Vector3<f32>,
    pub _prev_position_store: Vector3<f32>,
    pub _prev_rotation: Vector3<f32>,
    pub _prev_scale: Vector3<f32>,
    pub _prev_euler_to_quaternion: Quaternion<f32>,
    pub _prev_quaternion: Quaternion<f32>,
    pub _prev_fianl_quaternion: Quaternion<f32>,
    pub _quaternion_matrix: Matrix4<f32>,
    pub _euler_matrix: Matrix4<f32>,
    pub _rotation_matrix : Matrix4<f32>,
    pub _matrix: Matrix4<f32>,
    pub _inverse_matrix: Matrix4<f32>,
    pub _prev_matrix: Matrix4<f32>,
    pub _prev_inverse_matrix: Matrix4<f32>,
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
    pub fn set_position(&mut self, position: &Vector3<f32>) { self._position.copy_from(position); }
    pub fn set_position_x(&mut self, pos_x: f32) { self._position.x = pos_x; }
    pub fn set_position_y(&mut self, pos_y: f32) { self._position.y = pos_y; }
    pub fn set_position_z(&mut self, pos_z: f32) { self._position.z = pos_z; }
    pub fn get_prev_position(&self) -> &Vector3<f32> {
        &self._prev_position
    }
    pub fn move_pos(&mut self, move_speed: &Vector3<f32>) {
        self._position += move_speed;
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
        self._rotation.x = self._rotation.x % TWO_PI;
        self._rotation.y = self._rotation.y % TWO_PI;
        self._rotation.z = self._rotation.z % TWO_PI;
    }
    pub fn rotation_pitch(&mut self, rotation_speed: f32) {
        self._rotation.x = (self._rotation.x + rotation_speed) % TWO_PI;
    }
    pub fn rotation_yaw(&mut self, rotation_speed: f32) {
        self._rotation.y = (self._rotation.y + rotation_speed) % TWO_PI;
    }
    pub fn rotation_roll(&mut self, rotation_speed: f32) {
        self._rotation.z = (self._rotation.z + rotation_speed) % TWO_PI;
    }
    pub fn get_pitch(&self) -> f32 {
        self._rotation.x
    }
    pub fn get_yaw(&self) -> f32 {
        self._rotation.y
    }
    pub fn get_roll(&self) -> f32 {
        self._rotation.z
    }
    pub fn set_pitch(&mut self, rotation: f32) {
        self._rotation.x = rotation % TWO_PI;
    }
    pub fn set_yaw(&mut self, rotation: f32) {
        self._rotation.y = rotation % TWO_PI;
    }
    pub fn set_roll(&mut self, rotation: f32) {
        self._rotation.z = rotation % TWO_PI;
    }
    pub fn get_scale(&self) -> &Vector3<f32> {
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
        self._prev_position.copy_from(&self._prev_position_store);
        let updated_position = force_update || self._prev_position_store != self._position;
        if updated_position {
            self._prev_position_store.copy_from(&self._position);
        }

        let updated_rotation = force_update || self._prev_rotation != self._rotation;
        if updated_rotation {
            self._prev_rotation.copy_from(&self._rotation);
        }

        let updated_scale = force_update || self._prev_scale != self._scale;
        if updated_scale {
            self._prev_scale.copy_from(&self._scale);
        }

        let updated = updated_position || updated_rotation || updated_scale;
        if prev_updated || updated {
            self._prev_matrix.copy_from(&self._matrix);
            self._prev_inverse_matrix.copy_from(&self._inverse_matrix);
        }

        if updated_rotation {
            // just rotation
            self._rotation_matrix = make_rotation_matrix(self._rotation.x, self._rotation.y, self._rotation.z);
            let left: Vector4<f32> = self._rotation_matrix.column(0).into();
            let up: Vector4<f32> = self._rotation_matrix.column(1).into();
            let front: Vector4<f32> = self._rotation_matrix.column(2).into();
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
            self._matrix.copy_from(&combinate_matrix(&self._position, &self._rotation_matrix, &self._scale));
            //self._inverse_matrix.copy_from(&inverse_transform_matrix(&self._position, &self._rotation_matrix, &self._scale));
            linalg::try_invert_to(self._matrix.into(), &mut self._inverse_matrix);
        }
        self._updated = updated;
        updated
    }
}