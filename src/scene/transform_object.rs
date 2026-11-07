use crate::ecs::{ComponentStorage, EntityId, INVALID_ENTITY_ID};
use crate::utilities::math;
use crate::utilities::system::ptr_as_ref;
use nalgebra::{Matrix4, Quaternion, Vector3};
use nalgebra_glm as glm;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct SimpleTransform {
    pub _position: Vector3<f32>,
    pub _rotation: Quaternion<f32>,
    pub _scale: Vector3<f32>,
}

impl Default for SimpleTransform {
    fn default() -> SimpleTransform {
        SimpleTransform {
            _position: Vector3::zeros(),
            _rotation: Quaternion::new(1.0, 0.0, 0.0, 0.0),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}
impl SimpleTransform {
    pub fn lerp(&self, other: &SimpleTransform, t: f32) -> SimpleTransform {
        SimpleTransform {
            _position: glm::lerp(&self._position, &other._position, t),
            _rotation: glm::quat_slerp(&self._rotation, &other._rotation, t),
            _scale: glm::lerp(&self._scale, &other._scale, t),
        }
    }

    pub fn to_matrix(&self) -> Matrix4<f32> {
        math::combinate_matrix(
            &self._position,
            &math::quaternion_to_matrix(&self._rotation),
            &self._scale,
        )
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct TransformObjectData {
    pub _owner_entity_id: EntityId,
    pub _updated_frame: u64,
    pub _updated: bool,
    pub _is_dirty: bool,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _prev_position: Vector3<f32>,
    pub _prev_rotation: Vector3<f32>,
    pub _prev_scale: Vector3<f32>,
    pub _rotation_matrix: Matrix4<f32>,
    pub _matrix: Matrix4<f32>,
    pub _inverse_matrix: Matrix4<f32>,
    pub _prev_matrix: Matrix4<f32>,
    pub _prev_inverse_matrix: Matrix4<f32>,
}

impl Default for TransformObjectData {
    fn default() -> Self {
        Self::create_transform_object_data()
    }
}

impl TransformObjectData {
    pub fn create_transform_object_data() -> TransformObjectData {
        TransformObjectData {
            _owner_entity_id: INVALID_ENTITY_ID,
            _updated_frame: u64::MAX,
            _updated: true,
            _is_dirty: true,
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _prev_position: Vector3::zeros(),
            _prev_rotation: Vector3::zeros(),
            _prev_scale: Vector3::new(1.0, 1.0, 1.0),
            _rotation_matrix: Matrix4::identity(),
            _matrix: Matrix4::identity(),
            _inverse_matrix: Matrix4::identity(),
            _prev_matrix: Matrix4::identity(),
            _prev_inverse_matrix: Matrix4::identity(),
        }
    }
    pub fn get_matrix(&self) -> &Matrix4<f32> {
        &self._matrix
    }
    pub fn get_prev_matrix(&self) -> &Matrix4<f32> {
        &self._prev_matrix
    }
    pub fn get_inverse_matrix(&self) -> &Matrix4<f32> {
        &self._inverse_matrix
    }
    pub fn get_right(&self) -> &Vector3<f32> {
        ptr_as_ref(self._rotation_matrix.column(0).as_ptr() as *const Vector3<f32>)
    }
    pub fn get_up(&self) -> &Vector3<f32> {
        ptr_as_ref(self._rotation_matrix.column(1).as_ptr() as *const Vector3<f32>)
    }
    pub fn get_front(&self) -> &Vector3<f32> {
        ptr_as_ref(self._rotation_matrix.column(2).as_ptr() as *const Vector3<f32>)
    }
    pub fn get_dirty(&self) -> bool {
        self._is_dirty
    }
    pub fn set_dirty(&mut self, is_dirty: bool) {
        self._is_dirty = is_dirty;
    }
    pub fn get_position(&self) -> &Vector3<f32> {
        &self._position
    }
    pub fn set_position(&mut self, position: &Vector3<f32>) {
        self._position.copy_from(position);
        self._is_dirty = true;
    }
    pub fn set_position_x(&mut self, pos_x: f32) {
        self._position.x = pos_x;
        self._is_dirty = true;
    }
    pub fn set_position_y(&mut self, pos_y: f32) {
        self._position.y = pos_y;
        self._is_dirty = true;
    }
    pub fn set_position_z(&mut self, pos_z: f32) {
        self._position.z = pos_z;
        self._is_dirty = true;
    }
    pub fn get_prev_position(&self) -> &Vector3<f32> {
        ptr_as_ref(self._prev_matrix.column(3).as_ptr() as *const Vector3<f32>)
    }
    pub fn move_position(&mut self, move_speed: &Vector3<f32>) {
        self._position += move_speed;
        self._is_dirty = true;
    }
    pub fn move_right(&mut self, move_speed: f32) {
        self._position += (self.get_right() * move_speed) as Vector3<f32>;
        self._is_dirty = true;
    }
    pub fn move_up(&mut self, move_speed: f32) {
        self._position += (self.get_up() * move_speed) as Vector3<f32>;
        self._is_dirty = true;
    }
    pub fn move_front(&mut self, move_speed: f32) {
        self._position += (self.get_front() * move_speed) as Vector3<f32>;
        self._is_dirty = true;
    }
    pub fn get_rotation(&self) -> &Vector3<f32> {
        &self._rotation
    }
    pub fn set_rotation(&mut self, rotation: &Vector3<f32>) {
        self._rotation.copy_from(rotation);
        self._rotation.x = self._rotation.x % math::TWO_PI;
        self._rotation.y = self._rotation.y % math::TWO_PI;
        self._rotation.z = self._rotation.z % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn rotation_pitch(&mut self, rotation_speed: f32) {
        self._rotation.x = (self._rotation.x + rotation_speed) % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn rotation_yaw(&mut self, rotation_speed: f32) {
        self._rotation.y = (self._rotation.y + rotation_speed) % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn rotation_roll(&mut self, rotation_speed: f32) {
        self._rotation.z = (self._rotation.z + rotation_speed) % math::TWO_PI;
        self._is_dirty = true;
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
        self._rotation.x = rotation % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn set_yaw(&mut self, rotation: f32) {
        self._rotation.y = rotation % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn set_roll(&mut self, rotation: f32) {
        self._rotation.z = rotation % math::TWO_PI;
        self._is_dirty = true;
    }
    pub fn get_scale(&self) -> &Vector3<f32> {
        &self._scale
    }
    pub fn set_scale(&mut self, scale: &Vector3<f32>) {
        self._scale.copy_from(scale);
        self._is_dirty = true;
    }
    pub fn set_scale_x(&mut self, scale: f32) {
        self._scale.x = scale;
        self._is_dirty = true;
    }
    pub fn set_scale_y(&mut self, scale: f32) {
        self._scale.y = scale;
        self._is_dirty = true;
    }
    pub fn set_scale_z(&mut self, scale: f32) {
        self._scale.z = scale;
        self._is_dirty = true;
    }
    pub fn set_position_rotation_scale(
        &mut self,
        position: &Vector3<f32>,
        rotation: &Vector3<f32>,
        scale: &Vector3<f32>,
    ) {
        self.set_position(position);
        self.set_rotation(rotation);
        self.set_scale(scale);
        self._is_dirty = true;
    }

    pub fn set_transform(&mut self, matrix: &Matrix4<f32>) {
        self.set_position(&math::extract_location(matrix));
        self.set_rotation(&math::matrix_decompose_pitch_yaw_roll(matrix));
        self.set_scale(&math::extract_scale(matrix));
        self._is_dirty = true;
    }

    pub fn update_matrix(&mut self) -> bool {
        self._rotation_matrix = math::make_rotation_matrix(self._rotation.x, self._rotation.y, self._rotation.z);
        self._matrix = math::combinate_matrix(&self._position, &self._rotation_matrix, &self._scale);
        self._inverse_matrix = math::inverse_transform_matrix(&self._position, &self._rotation_matrix, &self._scale);
        true
    }

    pub fn reset_update_flag(&mut self) {
        self._updated = false;
    }

    pub fn update_transform_object(&mut self) -> bool {
        let current_frame = crate::core::engine_core::get_current_frame();
        if self._updated_frame != current_frame {
            self._prev_matrix.copy_from(&self._matrix);
            self._prev_inverse_matrix.copy_from(&self._inverse_matrix);
            self._prev_position.copy_from(&self._position);
            self._prev_rotation.copy_from(&self._rotation);
            self._prev_scale.copy_from(&self._scale);
            self._updated_frame = current_frame;
            self._updated = false;
        }

        if self._is_dirty {
            self.update_matrix();
            self._updated = true;
            self._is_dirty = false;
        }
        self._updated
    }

    pub fn set_owner_entity_id(&mut self, entity_id: EntityId) {
        self._owner_entity_id = entity_id;
    }
}

pub type TransformStorage = ComponentStorage<TransformObjectData>;

pub struct TransformSystem;

impl TransformSystem {
    pub fn update(storage: &mut TransformStorage) {
        for transform in storage.dense_slice_mut() {
            transform.reset_update_flag();
            transform.update_transform_object();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::engine_core::{get_current_frame, set_current_frame};

    #[test]
    fn test_transform_prev_state_preservation_under_multiple_updates() {
        let initial_frame = get_current_frame();
        let test_frame_1 = initial_frame + 1;
        let test_frame_2 = initial_frame + 2;

        let mut transform = TransformObjectData::create_transform_object_data();
        // Frame 0 초기화
        set_current_frame(initial_frame);
        transform.update_transform_object();

        assert_eq!(*transform.get_position(), Vector3::zeros());
        assert_eq!(*transform.get_prev_position(), Vector3::zeros());

        // Frame 1: 첫 번째 이동 (1, 2, 3)
        set_current_frame(test_frame_1);
        transform.set_position(&Vector3::new(1.0, 2.0, 3.0));
        assert!(transform.update_transform_object());

        // Frame 1의 첫 업데이트 후: prev는 직전 프레임의 위치(0, 0, 0)
        assert_eq!(*transform.get_position(), Vector3::new(1.0, 2.0, 3.0));
        assert_eq!(*transform.get_prev_position(), Vector3::zeros());

        // Frame 1: 같은 프레임 내에서 두 번째 이동 (4, 5, 6)
        transform.set_position(&Vector3::new(4.0, 5.0, 6.0));
        assert!(transform.update_transform_object());

        // 핵심 검증: 같은 프레임에서 아무리 여러 번 update되어도 prev_position은 (1, 2, 3)으로 오염되지 않고 (0, 0, 0) 유지!
        assert_eq!(*transform.get_position(), Vector3::new(4.0, 5.0, 6.0));
        assert_eq!(*transform.get_prev_position(), Vector3::zeros());

        // Frame 2: 다음 프레임으로 넘어가서 이동 (10, 20, 30)
        set_current_frame(test_frame_2);
        transform.set_position(&Vector3::new(10.0, 20.0, 30.0));
        assert!(transform.update_transform_object());

        // Frame 2 검증: prev_position은 Frame 1의 최종 결과였던 (4, 5, 6)이어야 함!
        assert_eq!(*transform.get_position(), Vector3::new(10.0, 20.0, 30.0));
        assert_eq!(*transform.get_prev_position(), Vector3::new(4.0, 5.0, 6.0));
    }
}
