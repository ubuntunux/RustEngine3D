use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix3,
    Matrix4,
};

pub const TWO_PI: f32 = std::f32::consts::PI as f32 * 2.0;

pub struct MathData {
    _clip_space_matrix: Matrix4<f32>,
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
            )
        }
    }
}


//
// quaternion_identity :: Quater Float
// quaternion_identity = Quater 0 0 0 1
//
//
// wrap_rotation :: Float -> Float
// wrap_rotation rotation
//     | twoPI < rotation || rotation < 0.0 = mod' rotation twoPI
//     | otherwise = rotation
//
// matrixRotation :: Float -> Float -> Float -> Mat44f
// matrixRotation rx ry rz =
//     let ch = cos ry
//         sh = sin ry
//         ca = cos rz
//         sa = sin rz
//         cb = cos rx
//         sb = sin rx
//     in DF4 (vec4 (ch*ca) sa (-sh*ca) 0)
//            (vec4 (sh*sb - ch*sa*cb) (ca*cb) (sh*sa*cb + ch*sb) 0)
//            (vec4 (ch*sa*sb + sh*cb) (-ca*sb) (-sh*sa*sb + ch*cb) 0)
//            (vec4 0 0 0 1)
//
// transform_matrix :: Vec3f -> Mat44f -> Vec3f -> Mat44f
// transform_matrix translation rotation_matrix scale =
//     let (# sx, sy, sz #) = unpackV3# scale
//         row0 = (rotation_matrix .! Idx 0 :: Vec4f) * (getFloat4 sx)
//         row1 = (rotation_matrix .! Idx 1 :: Vec4f) * (getFloat4 sy)
//         row2 = (rotation_matrix .! Idx 2 :: Vec4f) * (getFloat4 sz)
//         row3 = toHomPoint translation
//     in DF4 row0 row1 row2 row3

pub fn inverse_transform_matrix(translation: &Vector3<f32>, rotation_matrix: &Matrix4<f32>, scale: &Vector3<f32>) -> Matrix4<f32> {
    let rotation_matrix_t = rotation_matrix.transpose();
    let column0 = rotation_matrix_t.column(0) / scale[0];
    let column1 = rotation_matrix_t.column(1) / scale[1];
    let column2 = rotation_matrix_t.column(2) / scale[2];
    let homogeneous_vector = Vector4::new(-translation[0], -translation[1], -translation[2], 1.0);
    let x: f32 = homogeneous_vector.dot(&rotation_matrix.column(0));
    let y: f32 = homogeneous_vector.dot(&rotation_matrix.column(1));
    let z: f32 = homogeneous_vector.dot(&rotation_matrix.column(2));
    Matrix4::from_columns(&[
        column0,
        column1,
        column2,
        Vector4::new(x, y, z, 1.0),
    ])
}
