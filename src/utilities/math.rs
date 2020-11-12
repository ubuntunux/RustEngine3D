use nalgebra::{
    Vector3,
    Vector4,
    Matrix4,
};

pub const TWO_PI: f32 = std::f32::consts::PI as f32 * 2.0;

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
def is_rotation_matrix(R):
    Rt = np.transpose(R)
    shouldBeIdentity = np.dot(Rt, R)
    I = np.identity(3, dtype=R.dtype)
    n = np.linalg.norm(I - shouldBeIdentity)
    return n < 1e-6


def rotation_maxtrix_to_euler_angles(R, check_valid=False):
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


def matrix_rotation(rotation_matrix, rx, ry, rz):
    ch = math.cos(ry)
    sh = math.sin(ry)
    ca = math.cos(rz)
    sa = math.sin(rz)
    cb = math.cos(rx)
    sb = math.sin(rx)

    rotation_matrix[:, 0] = [ch*ca, sh*sb - ch*sa*cb, ch*sa*sb + sh*cb, 0.0]
    rotation_matrix[:, 1] = [sa, ca*cb, -ca*sb, 0.0]
    rotation_matrix[:, 2] = [-sh*ca, sh*sa*cb + ch*sb, -sh*sa*sb + ch*cb, 0.0]


def matrix_to_vectors(rotation_matrix, axis_x, axis_y, axis_z, do_normalize=False):
    if do_normalize:
        rotation_matrix[0, 0:3] = normalize(rotation_matrix[0, 0:3])
        rotation_matrix[1, 0:3] = normalize(rotation_matrix[1, 0:3])
        rotation_matrix[2, 0:3] = normalize(rotation_matrix[2, 0:3])
    axis_x[:] = rotation_matrix[0, 0:3]
    axis_y[:] = rotation_matrix[1, 0:3]
    axis_z[:] = rotation_matrix[2, 0:3]


def getYawPitchRoll(m):
    pitch = arcsin(-m[2][1])
    threshold = 1e-8
    test = cos(pitch)
    if test < threshold:
        roll = math.arctan2(-m[1][0], m[0][0])
        yaw = 0.0
    else:
        roll = math.arctan2(m[0][1], m[1][1])
        yaw = math.arctan2(m[2][0], m[2][2])
    return yaw, pitch, roll


def axis_rotation(axis, radian):
    angle = radian * 0.5
    s = math.sin(angle)
    return Float4(math.cos(angle), axis[0]*s, axis[1]*s, axis[2]*s)


def muliply_quaternion(quaternion1, quaternion2):
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


def muliply_quaternions(*quaternions):
    return reduce(muliply_quaternion, quaternions)


def vector_multiply_quaternion(vector, quaternion):
    u = np.cross(vector, quaternion[1:])
    return vector + u * 2.0 * quaternion[0] + np.cross(quaternion[1:], u) * 2.0


def euler_to_quaternion(rx, ry, rz, quat):
    t0 = math.cos(rz * 0.5)
    t1 = math.sin(rz * 0.5)
    t2 = math.cos(rx * 0.5)
    t3 = math.sin(rx * 0.5)
    t4 = math.cos(ry * 0.5)
    t5 = math.sin(ry * 0.5)
    t0t2 = t0 * t2
    t0t3 = t0 * t3
    t1t2 = t1 * t2
    t1t3 = t1 * t3
    qw = t0t2 * t4 + t1t3 * t5
    qx = t0t3 * t4 - t1t2 * t5
    qy = t0t2 * t5 + t1t3 * t4
    qz = t1t2 * t4 - t0t3 * t5
    n = 1.0 / math.sqrt(qw * qw + qx * qx + qy * qy + qz * qz)
    quat[0] = qw * n
    quat[1] = qx * n
    quat[2] = qy * n
    quat[3] = qz * n


def matrix_to_quaternion(matrix):
    m00, m01, m02, m03 = matrix[0, :]
    m10, m11, m12, m13 = matrix[1, :]
    m20, m21, m22, m23 = matrix[2, :]

    tr = m00 + m11 + m22
    if tr > 0.0:
        S = math.sqrt(tr+1.0) * 2.0
        qw = 0.25 * S
        qx = (m12 - m21) / S
        qy = (m20 - m02) / S
        qz = (m01 - m10) / S
    elif m00 > m11 and m00 > m22:
        S = math.sqrt(1.0 + m00 - m11 - m22) * 2.0
        qw = (m12 - m21) / S
        qx = 0.25 * S
        qy = (m10 + m01) / S
        qz = (m20 + m02) / S
    elif m11 > m22:
        S = math.sqrt(1.0 + m11 - m00 - m22) * 2.0
        qw = (m20 - m02) / S
        qx = (m10 + m01) / S
        qy = 0.25 * S
        qz = (m21 + m12) / S
    else:
        S = math.sqrt(1.0 + m22 - m00 - m11) * 2.0
        qw = (m01 - m10) / S
        qx = (m20 + m02) / S
        qy = (m21 + m12) / S
        qz = 0.25 * S
    return normalize(Float4(qw, qx, qy, qz))


def quaternion_to_matrix(quat, rotation_matrix):
    qw, qx, qy, qz = quat[:]
    # inhomogeneous expression
    qxqx = qx * qx * 2.0
    qxqy = qx * qy * 2.0
    qxqz = qx * qz * 2.0
    qxqw = qx * qw * 2.0
    qyqy = qy * qy * 2.0
    qyqz = qy * qz * 2.0
    qyqw = qy * qw * 2.0
    qzqw = qz * qw * 2.0
    qzqz = qz * qz * 2.0
    rotation_matrix[0, :] = [1.0 - qyqy - qzqz, qxqy + qzqw, qxqz - qyqw, 0.0]
    rotation_matrix[1, :] = [qxqy - qzqw, 1.0 - qxqx - qzqz, qyqz + qxqw, 0.0]
    rotation_matrix[2, :] = [qxqz + qyqw, qyqz - qxqw, 1.0 - qxqx - qyqy, 0.0]
    rotation_matrix[3, :] = [0.0, 0.0, 0.0, 1.0]
    '''
    # homogeneous expression
    qxqx = qx * qx
    qxqy = qx * qy * 2.0
    qxqz = qx * qz * 2.0
    qxqw = qx * qw * 2.0
    qyqy = qy * qy
    qyqz = qy * qz * 2.0
    qyqw = qy * qw * 2.0
    qzqw = qz * qw * 2.0
    qzqz = qz * qz
    qwqw = qw * qw
    rotation_matrix[0, :] = [qwqw + qxqx - qyqy - qzqz, qxqy + qzqw, qxqz - qyqw, 0.0]
    rotation_matrix[1, :] = [qxqy - qzqw, qwqw - qxqx + qyqy - qzqz, qyqz + qxqw, 0.0]
    rotation_matrix[2, :] = [qxqz + qyqw, qyqz - qxqw, qwqw - qxqx - qyqy + qzqz, 0.0]
    rotation_matrix[3, :] = [0.0, 0.0, 0.0, 1.0]
    '''


def quaternion_to_euler(q):
    sqw = w * w
    sqx = x * x
    sqy = y * y
    sqz = z * z
    m = Matrix3()
    m[0][0] = sqx - sqy - sqz + sqw
    m[1][1] = -sqx + sqy - sqz + sqw
    m[2][2] = -sqx - sqy + sqz + sqw
    tmp1 = x * y
    tmp2 = z * w
    m[0][1] = 2.0 * (tmp1 + tmp2)
    m[1][0] = 2.0 * (tmp1 - tmp2)
    tmp1 = x * z
    tmp2 = y * w
    m[0][2] = 2.0 * (tmp1 - tmp2)
    m[2][0] = 2.0 * (tmp1 + tmp2)
    tmp1 = y * z
    tmp2 = x * w
    m[1][2] = 2.0 * (tmp1 + tmp2)
    m[2][1] = 2.0 * (tmp1 - tmp2)


def lerp(vector1, vector2, t):
    return vector1 * (1.0 - t) + vector2 * t


def slerp(quaternion1, quaternion2, amount):
    num = amount
    num2 = 0.0
    num3 = 0.0
    num4 = np.dot(quaternion1, quaternion2)
    flag = False
    if num4 < 0.0:
        flag = True
        num4 = -num4
    if num4 > 0.999999:
        num3 = 1.0 - num
        num2 = -num if flag else num
    else:
        num5 = math.acos(num4)
        num6 = 1.0 / math.sin(num5)
        num3 = math.sin((1.0 - num) * num5) * num6
        num2 = (-math.sin(num * num5) * num6) if flag else (math.sin(num * num5) * num6)
    return (num3 * quaternion1) + (num2 * quaternion2)


def set_identity_matrix(M):
    M[...] = [[1.0, 0.0, 0.0, 0.0],
            [0.0, 1.0, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [0.0, 0.0, 0.0, 1.0]]

*/
pub fn make_translate_matrix(position: &Vector3<f32>) -> Matrix4<f32> {
    Matrix4::new(
        1.0, 0.0, 0.0, position.x,
        0.0, 1.0, 0.0, position.y,
        0.0, 0.0, 1.0, position.z,
        0.0, 0.0, 0.0, 1.0
    )
}
/*
def set_translate_matrix(M, x, y, z):
    M[:] = [[1, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 1, 0],
            [x, y, z, 1]]


def matrix_translate(M, x, y, z):
    M[3][0] += x
    M[3][1] += y
    M[3][2] += z

def get_scale_matrix(x, y, z):
    S = [[x, 0, 0, 0],
         [0, y, 0, 0],
         [0, 0, z, 0],
         [0, 0, 0, 1]]
    return np.array(S, dtype=np.float32)


def set_scale_matrix(M, x, y, z):
    M[:] = [[x, 0, 0, 0],
            [0, y, 0, 0],
            [0, 0, z, 0],
            [0, 0, 0, 1]]


def matrix_scale(M, x, y, z):
    M[0] *= x
    M[1] *= y
    M[2] *= z


def get_rotation_matrix_x(radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, cosT, sinT, 0.0],
         [0.0, -sinT, cosT, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    return R


def get_rotation_matrix_y(radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[cosT, 0.0, -sinT, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [sinT, 0.0, cosT, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    return R


def get_rotation_matrix_z(radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[cosT, sinT, 0.0, 0.0],
         [-sinT, cosT, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    return R


def matrix_rotate_x(M, radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, cosT, sinT, 0.0],
         [0.0, -sinT, cosT, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    M[...] = np.dot(M, R)


def matrix_rotate_y(M, radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[cosT, 0.0, -sinT, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [sinT, 0.0, cosT, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    M[...] = np.dot(M, R)


def matrix_rotate_z(M, radian):
    cosT = math.cos(radian)
    sinT = math.sin(radian)
    R = np.array(
        [[cosT, sinT, 0.0, 0.0],
         [-sinT, cosT, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]], dtype=np.float32)
    M[...] = np.dot(M, R)


def matrix_rotate_axis(M, radian, x, y, z):
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


def matrix_rotate(M, rx, ry, rz):
    R = MATRIX4_IDENTITY.copy()
    matrix_rotation(R, rx, ry, rz)
    M[...] = np.dot(M, R)


def swap_up_axis_matrix(matrix, transpose, isInverseMatrix, up_axis):
    if transpose:
        matrix = matrix.T
    if up_axis == 'Z_UP':
        if isInverseMatrix:
            return np.dot(get_rotation_matrix_x(HALF_PI), matrix)
        else:
            return np.dot(matrix, get_rotation_matrix_x(-HALF_PI))
    return matrix


def swap_matrix(matrix, transpose, up_axis):
    if transpose:
        matrix = matrix.T
    if up_axis == 'Z_UP':
        return np.array(
            [matrix[0, :].copy(),
             matrix[2, :].copy(),
             -matrix[1, :].copy(),
             matrix[3, :].copy()]
        )
    return matrix


def transform_matrix(M, translation, rotation_matrix, scale):
    matrix_scale(M, *scale)
    M[...] = np.dot(M, rotation_matrix)
    matrix_translate(M, *translation)


def inverse_transform_matrix(M, translation, rotation_matrix, scale):
    matrix_translate(M, *(-translation))
    M[...] = np.dot(M, rotation_matrix.T)
    if all(0.0 != scale):
        matrix_scale(M, *(1.0 / scale))


def extract_location(matrix):
    return Float3(matrix[3, 0], matrix[3, 1], matrix[3, 2])


def extract_rotation(matrix):
    scale = extract_scale(matrix)
    rotation = Matrix4()
    rotation[0, :] = matrix[0, :] / scale[0]
    rotation[1, :] = matrix[1, :] / scale[1]
    rotation[2, :] = matrix[2, :] / scale[2]
    return rotation


def extract_quaternion(matrix):
    return matrix_to_quaternion(extract_rotation(matrix))


def extract_scale(matrix):
    sX = np.linalg.norm(matrix[0, :])
    sY = np.linalg.norm(matrix[1, :])
    sZ = np.linalg.norm(matrix[2, :])
    return Float3(sX, sY, sZ)
*/