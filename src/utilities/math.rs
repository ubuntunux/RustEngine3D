use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix3,
    Matrix4,
};

pub const TWO_PI: f32 = std::f32::consts::PI as f32 * 2.0;

pub struct MathData {
    pub _clip_space_matrix: Matrix4<f32>,
    pub _world_left: Vector3<f32>,
    pub _world_front: Vector3<f32>,
    pub _world_up: Vector3<f32>,
}

impl Default for MathData {
    fn default() -> MathData {
        MathData {
            // -- ... and a {clip space -> screen space} matrix that converts points into
            // --     the vulkan screen space {x: -1..1, y: 1..-1, z: 0..1}
            _clip_space_matrix: Matrix4::new(
                1.0, 0.0, 0.0, 0.0,
                0.0, -1.0, 0.0, 0.0,
                0.0, 0.0, 0.5, 0.0,
                0.0, 0.0, 0.5, 1.0,
            ),
            _world_left: Vector3::new(1.0, 0.0, 0.0),
            _world_up: Vector3::new(0.0, 1.0, 0.0),
            _world_front: Vector3::new(0.0, 0.0, 1.0)
        }
    }
}

pub fn make_rotation_matrix(pitch: f32, yaw: f32, roll: f32) -> Matrix4<f32> {
    let ch = yaw.cos();
    let sh = yaw.sin();
    let ca = roll.cos();
    let sa = roll.sin();
    let cb = pitch.cos();
    let sb = pitch.sin();
    Matrix4::from_columns(&[
        Vector4::new(ch*ca, sh*sb - ch*sa*cb, ch*sa*sb + sh*cb, 0.0),
        Vector4::new(sa, ca*cb, -ca*sb, 0.0),
        Vector4::new(-sh*ca, sh*sa*cb + ch*sb, -sh*sa*sb + ch*cb, 0.0),
        Vector4::new(0.0, 0.0, 0.0, 1.0),
    ])
}

pub fn make_matrix(translation: &Vector3<f32>, rotation_matrix: &Matrix4<f32>, scale: &Vector3<f32>) -> Matrix4<f32> {
    Matrix4::from_columns(&[
        rotation_matrix.column(0) * scale[0],
        rotation_matrix.column(1) * scale[1],
        rotation_matrix.column(2) * scale[2],
        Vector4::new(translation[0], translation[1], translation[2], 1.0),
    ])
}

pub fn inverse_transform_matrix(translation: &Vector3<f32>, rotation_matrix: &Matrix4<f32>, scale: &Vector3<f32>) -> Matrix4<f32> {
    let mut inv_rotation_matrix = rotation_matrix.clone();
    let column0 = inv_rotation_matrix.column(0) / scale[0];
    let column1 = inv_rotation_matrix.column(1) / scale[1];
    let column2 = inv_rotation_matrix.column(2) / scale[2];
    inv_rotation_matrix.set_column(0, &column0);
    inv_rotation_matrix.set_column(1, &column1);
    inv_rotation_matrix.set_column(2, &column2);
    inv_rotation_matrix = inv_rotation_matrix.transpose().into();

    let homogeneous_pos = Vector4::new(-translation[0], -translation[1], -translation[2], 1.0);
    let x: f32 = homogeneous_pos.dot(&column0);
    let y: f32 = homogeneous_pos.dot(&column1);
    let z: f32 = homogeneous_pos.dot(&column2);

    Matrix4::from_columns(&[
        Vector4::from(inv_rotation_matrix.column(0)),
        Vector4::from(inv_rotation_matrix.column(1)),
        Vector4::from(inv_rotation_matrix.column(2)),
        Vector4::new(x, y, z, 1.0),
    ])
}