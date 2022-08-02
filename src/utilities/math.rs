// https://docs.rs/nalgebra-glm/0.9.0/nalgebra_glm/
use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix4,
    Quaternion,
    UnitQuaternion,
};
use nalgebra_glm as glm;

pub const HALF_PI: f32 = std::f32::consts::PI * 0.5;
pub const TWO_PI: f32 = std::f32::consts::PI * 2.0;

pub fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a * (1.0 - t) + b * t
}

// https://github.com/TheRealMJP/SamplePattern/blob/master/SamplePattern.cpp
// Computes a radical inverse with base 2 using crazy bit-twiddling from "Hacker's Delight"
pub fn radical_inverse_base2(bits: u32) -> f32 {
    let mut bits = (bits << 16) | (bits >> 16);
    bits = ((bits & 0x55555555) << 1) | ((bits & 0xAAAAAAAA) >> 1);
    bits = ((bits & 0x33333333) << 2) | ((bits & 0xCCCCCCCC) >> 2);
    bits = ((bits & 0x0F0F0F0F) << 4) | ((bits & 0xF0F0F0F0) >> 4);
    bits = ((bits & 0x00FF00FF) << 8) | ((bits & 0xFF00FF00) >> 8);
    (bits as f32) * 2.3283064365386963e-10
}


// Returns a single 2D point in a Hammersley sequence of length "numSamples", using base 1 and base 2
pub fn hammersley_2d(sample_idx: u32, num_samples: u32) -> Vector2<f32> {
    Vector2::new((sample_idx as f32) / (num_samples as f32), radical_inverse_base2(sample_idx))
}

pub fn get_clip_space_matrix() -> Matrix4<f32> {
    // -- ... and a {clip space -> screen space} matrix that converts points into
    // --     the vulkan screen space {x: -1..1, y: 1..-1, z: 0..1}
    Matrix4::new(
        1.0, 0.0, 0.0, 0.0,
        0.0, -1.0, 0.0, 0.0,
        0.0, 0.0, 0.5, 0.5,
        0.0, 0.0, 0.0, 1.0,
    )
}

pub fn get_world_left() -> Vector3<f32> {
    Vector3::new(1.0, 0.0, 0.0)
}

pub fn get_world_up() -> Vector3<f32> {
    Vector3::new(0.0, 1.0, 0.0)
}

pub fn get_world_front() -> Vector3<f32> {
    Vector3::new(0.0, 0.0, 1.0)
}

pub fn degree_to_radian(degree: f32) -> f32 {
    degree / 180.0 * std::f32::consts::PI
}

pub fn radian_to_degree(degree: f32) -> f32 {
    degree / std::f32::consts::PI * 180.0
}

pub fn safe_normalize(vec: &Vector3<f32>) -> Vector3<f32> {
    let distance = (vec.x * vec.x + vec.y * vec.y + vec.z * vec.z).sqrt();
    if 0.0 < distance {
        return vec / distance;
    }
    Vector3::zeros()
}

pub fn safe_normalize_with_norm(vec: &Vector3<f32>) -> (Vector3<f32>, f32) {
    let distance = (vec.x * vec.x + vec.y * vec.y + vec.z * vec.z).sqrt();
    if 0.0 < distance {
        return (vec / distance, distance);
    }
    (Vector3::zeros(), 0.0)
}

pub fn safe_normalize_mut(vec: &mut Vector3<f32>) {
    let distance = (vec.x * vec.x + vec.y * vec.y + vec.z * vec.z).sqrt();
    if 0.0 < distance {
        vec.x /= distance;
        vec.y /= distance;
        vec.z /= distance;
    }
}

pub fn make_vector_xz(vec: &Vector3<f32>) -> Vector3<f32> {
    Vector3::new(vec.x, 0.0, vec.z)
}

pub fn make_vector_xz_mut(vec: &mut Vector3<f32>) {
    vec.y = 0.0;
}

pub fn make_normalize_xz(vec: &Vector3<f32>) -> Vector3<f32> {
    let distance = (vec.x * vec.x + vec.z * vec.z).sqrt();
    if 0.0 < distance {
        return Vector3::new(vec.x / distance, 0.0, vec.z / distance);
    }
    Vector3::zeros()
}

pub fn make_normalize_xz_with_norm(vec: &Vector3<f32>) -> (Vector3<f32>, f32) {
    let distance = (vec.x * vec.x + vec.z * vec.z).sqrt();
    if 0.0 < distance {
        return (Vector3::new(vec.x / distance, 0.0, vec.z / distance), distance);
    }
    (Vector3::zeros(), 0.0)
}

pub fn make_rotation_matrix(pitch: f32, yaw: f32, roll: f32) -> Matrix4<f32> {
    let ch = yaw.cos();
    let sh = yaw.sin();
    let ca = roll.cos();
    let sa = roll.sin();
    let cb = pitch.cos();
    let sb = pitch.sin();
    Matrix4::from_columns(&[
        Vector4::new(ch*ca, sa, -sh*ca, 0.0),
        Vector4::new(sh*sb - ch*sa*cb, ca*cb, sh*sa*cb + ch*sb, 0.0),
        Vector4::new(ch*sa*sb + sh*cb, -ca*sb, -sh*sa*sb + ch*cb, 0.0),
        Vector4::new(0.0, 0.0, 0.0, 1.0),
    ])
}

pub fn make_srt_transform(translation: &Vector3<f32>, rotation: &Vector3<f32>, scale: &Vector3<f32>) -> Matrix4<f32> {
    let rotation_matrix = make_rotation_matrix(rotation.x, rotation.y, rotation.z);
    Matrix4::from_columns(&[
        rotation_matrix.column(0) * scale[0],
        rotation_matrix.column(1) * scale[1],
        rotation_matrix.column(2) * scale[2],
        Vector4::new(translation[0], translation[1], translation[2], 1.0),
    ])
}

pub fn combinate_matrix(translation: &Vector3<f32>, rotation_matrix: &Matrix4<f32>, scale: &Vector3<f32>) -> Matrix4<f32> {
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

    let homogeneous_pos = Vector4::new(-translation[0], -translation[1], -translation[2], 0.0);
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

pub fn look_at(eye: &Vector3<f32>, target: &Vector3<f32>, up: &Vector3<f32>) -> Matrix4<f32> {
    let f: Vector3<f32> = (target - eye).normalize();
    let s: Vector3<f32> = f.cross(&up);
    let u: Vector3<f32> = s.cross(&f);
    Matrix4::from_columns(&[
        Vector4::new(s.x, s.y, s.z, 0.0),
        Vector4::new(u.x, u.y, u.z, 0.0),
        Vector4::new(f.x, f.y, f.z, 0.0),
        Vector4::new(-s.dot(eye), -u.dot(eye), -f.dot(eye), 1.0)
    ])
}

pub fn orthogonal(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) -> Matrix4<f32> {
    let mut m: Matrix4<f32> = Matrix4::identity();
    m.m11 = 2.0 / (right - left);
    m.m22 = 2.0 / (top - bottom);
    m.m33 = -2.0 / (far - near);
    m.m14 = -(right + left) / (right - left);
    m.m24 = -(top + bottom) / (top - bottom);
    m.m34 = -(far + near) / (far - near);
    m.m44 = 1.0;
    m
}

pub fn perspective(aspect: f32, fov: f32, near: f32, far: f32) -> Matrix4<f32> {
    let height: f32 = degree_to_radian(fov * 0.5).tan() * near;
    let width: f32 = height * aspect;
    let depth = far - near;
    // Compact version, it is assumed that x1 and x2 are the same.
    Matrix4::from_columns(&[
        Vector4::new(near / width, 0.0, 0.0, 0.0),
        Vector4::new(0.0, near / height, 0.0, 0.0),
        Vector4::new(0.0, 0.0, -(far + near) / depth, -1.0),
        Vector4::new(0.0, 0.0, -(2.0 * near * far) / depth, 0.0)
    ])

    // Verbose version
    // let left = -width;
    // let right = width;
    // let top = height;
    // let bottom = -height;
    // Matrix4::from_columns(&[
    //     Vector4::new(2.0 * near / (right - left), 0.0, 0.0, 0.0),
    //     Vector4::new(0.0, 2.0 * near / (top - bottom), 0.0, 0.0),
    //     Vector4::new((right + left) / (right - left), (top + bottom) / (top - bottom), -(far + near) / (far - near), -1.0),
    //     Vector4::new(0.0, 0.0, -2.0 * near * far / (far - near), 0.0),
    // ])
}

/*
# Checks if a matrix is a valid rotation matrix.
pub fn is_rotation_matrix(R):
    Rt = np.transpose(R)
    shouldBeIdentity = np.dot(Rt, R)
    I = np.identity(3, dtype=R.dtype)
    n = np.linalg.norm(I - shouldBeIdentity)
    return n < 1e-6


pub fn rotation_maxtrix_to_euler_angles(R, check_valid=False):
    if check_valid:
        assert (is_rotation_matrix(R))

    sy = math.sqrt(R[0, 0] * R[0, 0] + R[1, 0] * R[1, 0])

    singular = sy < 1e-6

    if not singular:
        x = math.atan2(R[1, 2], R[2, 2])
        y = math.atan2(-R[0, 2], sy)
        z = math.atan2(R[0, 1], R[0, 0])
    else:
        x = math.atan2(-R[2, 1], R[1, 1])
        y = math.atan2(-R[0, 2], sy)
        z = 0

    return Float3(x, y, z)


pub fn matrix_rotation(rotation_matrix, rx, ry, rz):
    ch = math.cos(ry)
    sh = math.sin(ry)
    ca = math.cos(rz)
    sa = math.sin(rz)
    cb = math.cos(rx)
    sb = math.sin(rx)

    rotation_matrix[:, 0] = [ch*ca, sh*sb - ch*sa*cb, ch*sa*sb + sh*cb, 0.0]
    rotation_matrix[:, 1] = [sa, ca*cb, -ca*sb, 0.0]
    rotation_matrix[:, 2] = [-sh*ca, sh*sa*cb + ch*sb, -sh*sa*sb + ch*cb, 0.0]


pub fn matrix_to_vectors(rotation_matrix, axis_x, axis_y, axis_z, do_normalize=False):
    if do_normalize:
        rotation_matrix[0, 0:3] = normalize(rotation_matrix[0, 0:3])
        rotation_matrix[1, 0:3] = normalize(rotation_matrix[1, 0:3])
        rotation_matrix[2, 0:3] = normalize(rotation_matrix[2, 0:3])
    axis_x[:] = rotation_matrix[0, 0:3]
    axis_y[:] = rotation_matrix[1, 0:3]
    axis_z[:] = rotation_matrix[2, 0:3]

pub fn axis_rotation(axis, radian):
    angle = radian * 0.5
    s = math.sin(angle)
    return Float4(math.cos(angle), axis[0]*s, axis[1]*s, axis[2]*s)


pub fn muliply_quaternion(quaternion1, quaternion2):
    w1, x1, y1, z1 = quaternion1
    w2, x2, y2, z2 = quaternion2
    qX = (y1 * z2) - (z1 * y2)
    qY = (z1 * x2) - (x1 * z2)
    qZ = (x1 * y2) - (y1 * x2)
    qW = (x1 * x2) + (y1 * y2) + (z1 * z2)
    qX = (x1 * w2) + (x2 * w1) + qX
    qY = (y1 * w2) + (y2 * w1) + qY
    qZ = (z1 * w2) + (z2 * w1) + qZ
    qW = (w1 * w2) - qW
    return Float4(qW, qX, qY, qZ)


pub fn muliply_quaternions(*quaternions):
    return reduce(muliply_quaternion, quaternions)


pub fn vector_multiply_quaternion(vector, quaternion):
    u = np.cross(vector, quaternion[1:])
    return vector + u * 2.0 * quaternion[0] + np.cross(quaternion[1:], u) * 2.0
*/

//====================================================================================================
// MatrixDecomposeYawPitchRoll
//
// Extract the rotation contained in the provided matrix as yaw (heading), pitch and roll (bank) in
// radiuans.
//
// Assumptions:
//  Euler:   X = Pitch, Y = Yaw, Z = Roll
//  Applied: Yaw then pitch then roll
//  Axes:    X = Right, Y = Up, Z = Forward
//  DirectX: Matrices are row major (http://www.mindcontrol.org/~hplus/graphics/matrix-layout.html)
//
// Code is based on Mike Tunnicliffe's answer to this question:
//   https://stackoverflow.com/questions/1996957/conversion-euler-to-matrix-and-matrix-to-euler
pub fn matrix_decompose_pitch_yaw_roll(matrix: &Matrix4<f32>) -> Vector3<f32> {
    let mut euler: Vector3<f32> = Vector3::zeros();
    euler.x = (-matrix.m23).asin(); // Pitch
    // Not at poles
    if 0.0001 < euler.x.cos() {
        euler.y = matrix.m13.atan2(matrix.m33); // Yaw
        euler.z = matrix.m21.atan2(matrix.m22); // Roll
    } else {
        euler.y = 0.0; // Yaw
        euler.z = (-matrix.m12).atan2(matrix.m11); // Roll
    }
    euler
}

pub fn quaternion_to_euler(quat: &Quaternion<f32>) -> Vector3<f32> {
    // convert to (pitch, yaw, roll)
    glm::quat_euler_angles(quat).zyx()
}

pub fn euler_to_quaternion(pitch: f32, yaw: f32, roll: f32) -> Quaternion<f32> {
    UnitQuaternion::from_euler_angles(pitch, yaw, roll).into_inner()

    // let t0 = (rz * 0.5).cos();
    // let t1 = (rz * 0.5).sin();
    // let t2 = (rx * 0.5).cos();
    // let t3 = (rx * 0.5).sin();
    // let t4 = (ry * 0.5).cos();
    // let t5 = (ry * 0.5).sin();
    // let t0t2 = t0 * t2;
    // let t0t3 = t0 * t3;
    // let t1t2 = t1 * t2;
    // let t1t3 = t1 * t3;
    // let qw = t0t2 * t4 + t1t3 * t5;
    // let qx = t0t3 * t4 - t1t2 * t5;
    // let qy = t0t2 * t5 + t1t3 * t4;
    // let qz = t1t2 * t4 - t0t3 * t5;
    // let n = 1.0 / (qw * qw + qx * qx + qy * qy + qz * qz).sqrt();
    // Quaternion::new(qw, qx, qy, qz) * n
}

pub fn matrix_to_quaternion(matrix: &Matrix4<f32>) -> Quaternion<f32> {
    glm::to_quat(&matrix)

    // let tr = matrix.m11 + matrix.m22 + matrix.m33;
    // if tr > 0.0 {
    //     let s = (tr + 1.0).sqrt() * 2.0;
    //     Quaternion::new(
    //         0.25 * s,
    //         (matrix.m32 - matrix.m23) / s,
    //         (matrix.m13 - matrix.m31) / s,
    //         (matrix.m21 - matrix.m12) / s,
    //     )
    // } else if matrix.m11 > matrix.m22 && matrix.m11 > matrix.m33 {
    //     let s = (1.0 + matrix.m11 - matrix.m22 - matrix.m33).sqrt() * 2.0;
    //     Quaternion::new(
    //         (matrix.m32 - matrix.m23) / s,
    //         0.25 * s,
    //         (matrix.m12 + matrix.m21) / s,
    //         (matrix.m13 + matrix.m31) / s,
    //     )
    // } else if matrix.m22 > matrix.m33 {
    //     let s = (1.0 + matrix.m22 - matrix.m11 - matrix.m33).sqrt() * 2.0;
    //     Quaternion::new(
    //         (matrix.m13 - matrix.m31) / s,
    //         (matrix.m12 + matrix.m21) / s,
    //         0.25 * s,
    //         (matrix.m23 + matrix.m32) / s,
    //     )
    // } else {
    //     let s = (1.0 + matrix.m33 - matrix.m11 - matrix.m22).sqrt() * 2.0;
    //     Quaternion::new(
    //         (matrix.m21 - matrix.m12) / s,
    //         (matrix.m13 + matrix.m31) / s,
    //         (matrix.m23 + matrix.m32) / s,
    //         0.25 * s,
    //     )
    // }
}

pub fn quaternion_to_matrix(quat: &Quaternion<f32>) -> Matrix4<f32> {
    glm::quat_to_mat4(&quat)

    // inhomogeneous expression
    // let qxqx = quat.i * quat.i * 2.0;
    // let qxqy = quat.i * quat.j * 2.0;
    // let qxqz = quat.i * quat.k * 2.0;
    // let qxqw = quat.i * quat.w * 2.0;
    // let qyqy = quat.j * quat.j * 2.0;
    // let qyqz = quat.j * quat.k * 2.0;
    // let qyqw = quat.j * quat.w * 2.0;
    // let qzqw = quat.k * quat.w * 2.0;
    // let qzqz = quat.k * quat.k * 2.0;
    // Matrix4::from_columns(&[
    //     Vector4::new(1.0 - qyqy - qzqz, qxqy + qzqw, qxqz - qyqw, 0.0),
    //     Vector4::new(qxqy - qzqw, 1.0 - qxqx - qzqz, qyqz + qxqw, 0.0),
    //     Vector4::new(qxqz + qyqw, qyqz - qxqw, 1.0 - qxqx - qyqy, 0.0),
    //     Vector4::new(0.0, 0.0, 0.0, 1.0),
    // ])
}

pub fn make_translate_matrix(position: &Vector3<f32>) -> Matrix4<f32> {
    Matrix4::new(
        1.0, 0.0, 0.0, position.x,
        0.0, 1.0, 0.0, position.y,
        0.0, 0.0, 1.0, position.z,
        0.0, 0.0, 0.0, 1.0
    )
}

pub fn set_matrix_translate(m: &mut Matrix4<f32>, x: f32, y: f32, z: f32) {
    m.m14 = x;
    m.m24 = y;
    m.m34 = z;
}

pub fn matrix_translate(m: &mut Matrix4<f32>, x: f32, y: f32, z: f32) {
    m.m14 += x;
    m.m24 += y;
    m.m34 += z;
}

pub fn make_scale_matrix(scale: &Vector3<f32>) -> Matrix4<f32> {
    Matrix4::new(
        scale.x, 0.0, 0.0, 0.0,
        0.0, scale.y, 0.0, 0.0,
        0.0, 0.0, scale.z, 0.0,
        0.0, 0.0, 0.0, 1.0
    )
}

pub fn set_scale_matrix(m: &mut Matrix4<f32>, x: f32, y: f32, z: f32) {
    m.copy_from_slice(&[
        x, 0.0, 0.0, 0.0,
        0.0, y, 0.0, 0.0,
        0.0, 0.0, z, 0.0,
        0.0, 0.0, 0.0, 1.0
    ]);
}

pub fn matrix_scale(m: &mut Matrix4<f32>, x: f32, y: f32, z: f32) {
    m.m11 *= x;
    m.m21 *= x;
    m.m31 *= x;
    m.m12 *= y;
    m.m22 *= y;
    m.m32 *= y;
    m.m13 *= z;
    m.m23 *= z;
    m.m33 *= z;
}

pub fn get_rotation_matrix_x(radian: f32) -> Matrix4<f32> {
    let cos_t = radian.cos();
    let sin_t = radian.sin();
    Matrix4::new(
        1.0, 0.0, 0.0, 0.0,
        0.0, cos_t, -sin_t, 0.0,
        0.0, sin_t, cos_t, 0.0,
        0.0, 0.0, 0.0, 1.0
    )
}

pub fn get_rotation_matrix_y(radian: f32) -> Matrix4<f32> {
    let cos_t = radian.cos();
    let sin_t = radian.sin();
    Matrix4::new(
        cos_t, 0.0, sin_t, 0.0,
        0.0, 1.0, 0.0, 0.0,
        -sin_t, 0.0, cos_t, 0.0,
        0.0, 0.0, 0.0, 1.0
    )
}

pub fn get_rotation_matrix_z(radian: f32) -> Matrix4<f32> {
    let cos_t = radian.cos();
    let sin_t = radian.sin();
    Matrix4::new(
        cos_t, -sin_t, 0.0, 0.0,
        sin_t, cos_t, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    )
}

pub fn matrix_rotate_x(m: &mut Matrix4<f32>, radian: f32) {
    m.copy_from(&(get_rotation_matrix_x(radian) * (&m as &Matrix4<f32>)));
}

pub fn matrix_rotate_y(m: &mut Matrix4<f32>, radian: f32) {
    m.copy_from(&(get_rotation_matrix_y(radian) * (&m as &Matrix4<f32>)));
}

pub fn matrix_rotate_z(m: &mut Matrix4<f32>, radian: f32) {
    m.copy_from(&(get_rotation_matrix_z(radian) * (&m as &Matrix4<f32>)));
}

/*
pub fn matrix_rotate_axis(M, radian, x, y, z):
    c, s = math.cos(radian), math.sin(radian)
    n = math.sqrt(x * x + y * y + z * z)
    x /= n
    y /= n
    z /= n
    cx, cy, cz = (1 - c) * x, (1 - c) * y, (1 - c) * z
    R = np.array([[cx * x + c, cy * x - z * s, cz * x + y * s, 0],
                  [cx * y + z * s, cy * y + c, cz * y - x * s, 0],
                  [cx * z - y * s, cy * z + x * s, cz * z + c, 0],
                  [0, 0, 0, 1]]).T
    M[...] = np.dot(M, R)


pub fn matrix_rotate(M, rx, ry, rz):
    R = MATRIX4_IDENTITY.copy()
    matrix_rotation(R, rx, ry, rz)
    M[...] = np.dot(M, R)
*/

pub fn swap_up_axis_matrix(matrix: &mut Matrix4<f32>, transpose: bool, is_inverse_matrix: bool, up_axis: &str) {
    if transpose {
        matrix.transpose_mut();
    }

    if "Z_UP" == up_axis {
        if is_inverse_matrix {
            let other = matrix as &Matrix4<f32> * get_rotation_matrix_x(HALF_PI);
            matrix.copy_from(&other);
        } else {
            let other = get_rotation_matrix_x(-HALF_PI) * matrix as &Matrix4<f32>;
            matrix.copy_from(&other);
        }
    }
}

pub fn swap_matrix(matrix: &mut Matrix4<f32>, transpose: bool, up_axis: &str) {
    if transpose {
        matrix.transpose_mut();
    }

    if "Z_UP" == up_axis {
        let other: Matrix4<f32> = matrix.clone() as Matrix4<f32>;
        matrix.set_column(0, &other.column(0));
        matrix.set_column(1, &other.column(2));
        matrix.set_column(2, &(other.column(1).clone() * -1.0));
        matrix.set_column(3, &other.column(3));
    }
}

/*
pub fn transform_matrix(M, translation, rotation_matrix, scale):
    matrix_scale(M, *scale)
    M[...] = np.dot(M, rotation_matrix)
    matrix_translate(M, *translation)


pub fn inverse_transform_matrix(M, translation, rotation_matrix, scale):
    matrix_translate(M, *(-translation))
    M[...] = np.dot(M, rotation_matrix.T)
    if all(0.0 != scale):
        matrix_scale(M, *(1.0 / scale))

*/

pub fn extract_location(matrix: &Matrix4<f32>) -> Vector3<f32> {
    Vector3::new(matrix.m14, matrix.m24, matrix.m34)
}

pub fn extract_rotation(matrix: &Matrix4<f32>) -> Matrix4<f32> {
    let scale: Vector3<f32> = extract_scale(matrix);
    let mut rotation = matrix.clone() as Matrix4<f32>;
    rotation.m11 /= scale.x;
    rotation.m21 /= scale.x;
    rotation.m31 /= scale.x;
    rotation.m41 = 0.0;
    rotation.m12 /= scale.y;
    rotation.m22 /= scale.y;
    rotation.m32 /= scale.y;
    rotation.m42 = 0.0;
    rotation.m13 /= scale.z;
    rotation.m23 /= scale.z;
    rotation.m33 /= scale.z;
    rotation.m43 = 0.0;
    rotation.m14 = 0.0;
    rotation.m24 = 0.0;
    rotation.m34 = 0.0;
    rotation.m44 = 1.0;
    rotation
}

pub fn extract_quaternion(matrix: &Matrix4<f32>) -> Quaternion<f32> {
    matrix_to_quaternion(&extract_rotation(matrix))
}

pub fn extract_scale(matrix: &Matrix4<f32>) -> Vector3<f32> {
    let sx = matrix.column(0).norm();
    let sy = matrix.column(1).norm();
    let sz = matrix.column(2).norm();
    Vector3::new(sx, sy, sz)
}

pub fn convert_triangulate(quad: &Vec<u32>) -> Vec<u32> {
    let mut triangulated_list: Vec<u32> = Vec::new();
    triangulated_list.push(quad[0]);
    triangulated_list.push(quad[1]);
    triangulated_list.push(quad[2]);
    triangulated_list.push(quad[2]);
    triangulated_list.push(quad[1]);
    triangulated_list.push(quad[3]);
    triangulated_list
}

pub fn convert_to_screen_texcoord(view_projection: &Matrix4<f32>, world_pos: &Vector3<f32>, clamp: bool) -> Vector2<f32> {
    let ndc = view_projection * Vector4::new(world_pos.x, world_pos.y, world_pos.z, 1.0);
    let mut texcoord = Vector2::new(ndc.x / ndc.w * 0.5 + 0.5, ndc.y / ndc.w * 0.5 + 0.5);
    if clamp {
        if ndc.w < 0.0 {
            texcoord.x = 1.0 - texcoord.x;
            texcoord.y = 1.0 - texcoord.y;

            if ndc.x.abs() < ndc.y.abs() {
                texcoord.y = ndc.y.signum() * 0.5 + 0.5;
            } else {
                texcoord.x = ndc.x.signum() * 0.5 + 0.5;
            }
        }

        texcoord.x = texcoord.x.max(0.0).min(1.0);
        texcoord.y = texcoord.y.max(0.0).min(1.0);
    }

    texcoord
}
