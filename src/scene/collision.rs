use nalgebra::{Matrix3, Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::bounding_box::BoundingBox;
use crate::utilities::math;

#[repr(i32)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum CollisionType {
    NONE,
    BOX,
    CYLINDER,
    SPHERE
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct CollisionCreateInfo {
    pub _collision_type: CollisionType,
    pub _location: Vector3<f32>,
    pub _extents: Vector3<f32>
}

impl Default for CollisionCreateInfo {
    fn default() -> CollisionCreateInfo {
        CollisionCreateInfo {
            _collision_type: CollisionType::NONE,
            _location: Vector3::zeros(),
            _extents: Vector3::new(0.5, 0.5, 0.5)
        }
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct CollisionData {
    pub _collision_type: CollisionType,
    pub _bounding_box: BoundingBox,
}

impl Default for CollisionData {
    fn default() -> CollisionData {
        CollisionData {
            _collision_type: CollisionType::NONE,
            _bounding_box: BoundingBox::default()
        }
    }
}

impl CollisionData {
    pub fn create_collision(collision_info: &CollisionCreateInfo) -> CollisionData {
        let pos_min = collision_info._location - collision_info._extents;
        let pos_max = collision_info._location + collision_info._extents;
        CollisionData {
            _collision_type: collision_info._collision_type,
            _bounding_box: BoundingBox::create_bounding_box(&pos_min, &pos_max)
        }
    }

    pub fn get_collision_type(&self) -> CollisionType {
        self._collision_type
    }

    pub fn is_valid_collision(&self) -> bool {
        self._collision_type != CollisionType::NONE
    }

    pub fn collide_box_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }
        let to_point = point - self._bounding_box._center;
        self._bounding_box._orientation.column(0).dot(&to_point).abs() <= self._bounding_box._extents.x &&
            self._bounding_box._orientation.column(2).dot(&to_point).abs() <= self._bounding_box._extents.z
    }

    pub fn collide_cylinder_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }

        let (to_point_dir_xz, dist) = math::make_normalize_with_norm(&(point - self._bounding_box._center));
        let d = self._bounding_box._orientation.column(0).dot(&to_point_dir_xz);
        let r = math::lerp(self._bounding_box._extents.z, self._bounding_box._extents.x, d);
        dist <= r
    }

    pub fn collide_sphere_with_point(&self, point: &Vector3<f32>) -> bool {
        let h = (point - self._bounding_box._center).normalize().y.abs() * self._bounding_box._extents.y;
        if h < (point.y - self._bounding_box._center.y).abs() {
            return false;
        }

        let (to_point_dir_xz, dist) = math::make_normalize_with_norm(&(point - self._bounding_box._center));
        let d = self._bounding_box._orientation.column(0).dot(&to_point_dir_xz);
        let r = math::lerp(self._bounding_box._extents.z, self._bounding_box._extents.x, d);
        dist <= r
    }

    pub fn collide_aabb(&self, other: &CollisionData) -> bool {
        self._bounding_box._min.x < other._bounding_box._max.x && other._bounding_box._min.x < self._bounding_box._max.x &&
        self._bounding_box._min.y < other._bounding_box._max.y && other._bounding_box._min.y < self._bounding_box._max.y &&
        self._bounding_box._min.z < other._bounding_box._max.z && other._bounding_box._min.z < self._bounding_box._max.z
    }

    pub fn collide_point(&self, point: &Vector3<f32>) -> bool {
        if self._collision_type == CollisionType::CYLINDER {
            self.collide_cylinder_with_point(point)
        } else {
            self.collide_box_with_point(point)
        }
    }

    pub fn collide_collision(&self, other: &CollisionData) -> bool {
        if other._collision_type == CollisionType::BOX {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_box(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_box_with_cylinder(other, self);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_box_with_sphere(other, self);
            }
        } else if other._collision_type == CollisionType::CYLINDER {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_cylinder(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_cylinder_with_cylinder(self, other);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_cylinder_with_sphere(other, self);
            }
        } else if other._collision_type == CollisionType::SPHERE {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_sphere(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_cylinder_with_sphere(self, other);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_sphere_with_sphere(self, other);
            }
        }
        false
    }
}

pub fn collide_box_with_box(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y || a._bounding_box._max.y < b._bounding_box._min.y {
        return false;
    }

    let a_rotation: &Matrix3<f32> = &a._bounding_box._orientation;
    let b_rotation: &Matrix3<f32> = &b._bounding_box._orientation;

    let a_half_sizes: Vector3<f32> = a._bounding_box._extents;
    let b_half_sizes: Vector3<f32> = b._bounding_box._extents;

    let to_a = a._bounding_box._center - b._bounding_box._center;
    let to_a_vertices: [Vector3<f32>; 4] = [
        to_a + a_rotation.column(0) * a_half_sizes[0] + a_rotation.column(2) * a_half_sizes[2],
        to_a - a_rotation.column(0) * a_half_sizes[0] + a_rotation.column(2) * a_half_sizes[2],
        to_a + a_rotation.column(0) * a_half_sizes[0] - a_rotation.column(2) * a_half_sizes[2],
        to_a - a_rotation.column(0) * a_half_sizes[0] - a_rotation.column(2) * a_half_sizes[2]
    ];
    let to_b_vertices: [Vector3<f32>; 4] = [
          b_rotation.column(0) * b_half_sizes[0] + b_rotation.column(2) * b_half_sizes[2] - to_a,
         -b_rotation.column(0) * b_half_sizes[0] + b_rotation.column(2) * b_half_sizes[2] - to_a,
          b_rotation.column(0) * b_half_sizes[0] - b_rotation.column(2) * b_half_sizes[2] - to_a,
         -b_rotation.column(0) * b_half_sizes[0] - b_rotation.column(2) * b_half_sizes[2] - to_a
    ];

    for i in [0, 2] {
        let axis = Vector3::new(a_rotation.column(i)[0], a_rotation.column(i)[1], a_rotation.column(i)[2]);
        let mut dot_max = f32::MIN;
        let mut dot_min = f32::MAX;
        for to_b_vertex in to_b_vertices {
            let d = axis.dot(&to_b_vertex);
            dot_max = dot_max.max(d);
            dot_min = dot_min.min(d);
        }

        let found_seperate = a_half_sizes[i] < dot_min || dot_max < -a_half_sizes[i];
        if found_seperate {
            return false;
        }
    }

    for i in [0, 2] {
        let axis = Vector3::new(b_rotation.column(i)[0], b_rotation.column(i)[1], b_rotation.column(i)[2]);
        let mut dot_max = f32::MIN;
        let mut dot_min = f32::MAX;
        for to_a_vertex in to_a_vertices {
            let d = axis.dot(&to_a_vertex);
            dot_max = dot_max.max(d);
            dot_min = dot_min.min(d);
        }

        let found_seperate = b_half_sizes[i] < dot_min || dot_max < -b_half_sizes[i];
        if found_seperate {
            return false;
        }
    }
    true
}

pub fn collide_box_with_cylinder(a_box: &CollisionData, b_cylinder: &CollisionData) -> bool {
    if b_cylinder._bounding_box._max.y < a_box._bounding_box._min.y || a_box._bounding_box._max.y < b_cylinder._bounding_box._min.y {
        return false;
    }

    let a_box_axis_x = a_box._bounding_box._orientation.column(0);
    let a_box_axis_z = a_box._bounding_box._orientation.column(2);

    let a_box_half_sizes: Vector3<f32> = a_box._bounding_box._extents;
    let to_a_box = math::make_vector_xz(&(a_box._bounding_box._center - b_cylinder._bounding_box._center));
    let b_cylinder_radius_dir = b_cylinder._bounding_box._orientation.column(0).dot(&to_a_box.normalize()).abs();
    let b_cylinder_radius: f32 = math::lerp(b_cylinder._bounding_box._extents.z, b_cylinder._bounding_box._extents.x, b_cylinder_radius_dir);

    let to_box_pos_x0 = to_a_box + a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_x1 = to_a_box - a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_z0 = to_a_box + a_box_axis_z * a_box_half_sizes.z;
    let to_box_pos_z1 = to_a_box - a_box_axis_z * a_box_half_sizes.z;

    let d_x0 = a_box_axis_x.dot(&to_box_pos_x0);
    let d_x1 = a_box_axis_x.dot(&to_box_pos_x1);
    let distance_x = if d_x0.signum() != d_x1.signum() {
        0.0
    } else {
        d_x0.abs().min(d_x1.abs())
    };

    let d_z0 = a_box_axis_z.dot(&to_box_pos_z0);
    let d_z1 = a_box_axis_z.dot(&to_box_pos_z1);
    let distance_z = if d_z0.signum() != d_z1.signum() {
        0.0
    } else {
        d_z0.abs().min(d_z1.abs())
    };

    (distance_x * distance_x + distance_z * distance_z) <= b_cylinder_radius * b_cylinder_radius
}

pub fn collide_box_with_sphere(a_box: &CollisionData, b_cylinder: &CollisionData) -> bool {
    if b_cylinder._bounding_box._max.y < a_box._bounding_box._min.y || a_box._bounding_box._max.y < b_cylinder._bounding_box._min.y {
        return false;
    }

    let a_box_axis_x = a_box._bounding_box._orientation.column(0);
    let a_box_axis_z = a_box._bounding_box._orientation.column(2);

    let a_box_half_sizes: Vector3<f32> = a_box._bounding_box._extents;
    let to_a_box = math::make_vector_xz(&(a_box._bounding_box._center - b_cylinder._bounding_box._center));
    let b_cylinder_radius_dir = b_cylinder._bounding_box._orientation.column(0).dot(&to_a_box.normalize()).abs();
    let b_cylinder_radius: f32 = math::lerp(b_cylinder._bounding_box._extents.z, b_cylinder._bounding_box._extents.x, b_cylinder_radius_dir);

    let to_box_pos_x0 = to_a_box + a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_x1 = to_a_box - a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_z0 = to_a_box + a_box_axis_z * a_box_half_sizes.z;
    let to_box_pos_z1 = to_a_box - a_box_axis_z * a_box_half_sizes.z;

    let d_x0 = a_box_axis_x.dot(&to_box_pos_x0);
    let d_x1 = a_box_axis_x.dot(&to_box_pos_x1);
    let distance_x = if d_x0.signum() != d_x1.signum() {
        0.0
    } else {
        d_x0.abs().min(d_x1.abs())
    };

    let d_z0 = a_box_axis_z.dot(&to_box_pos_z0);
    let d_z1 = a_box_axis_z.dot(&to_box_pos_z1);
    let distance_z = if d_z0.signum() != d_z1.signum() {
        0.0
    } else {
        d_z0.abs().min(d_z1.abs())
    };

    (distance_x * distance_x + distance_z * distance_z) <= b_cylinder_radius * b_cylinder_radius
}

pub fn collide_cylinder_with_cylinder(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y || a._bounding_box._max.y < b._bounding_box._min.y {
        return false;
    }

    let to_a = math::make_vector_xz(&(a._bounding_box._center - b._bounding_box._center));
    let to_a_dir = to_a.normalize();
    let a_radius_dir = a._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let a_radius: f32 = math::lerp(a._bounding_box._extents.z, a._bounding_box._extents.x, a_radius_dir);
    let b_radius_dir = b._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let b_radius: f32 = math::lerp(b._bounding_box._extents.z, b._bounding_box._extents.x, b_radius_dir);
    (to_a.x * to_a.x + to_a.z * to_a.z) <= (a_radius * a_radius + b_radius * b_radius)
}

pub fn collide_cylinder_with_sphere(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y || a._bounding_box._max.y < b._bounding_box._min.y {
        return false;
    }

    let to_a = math::make_vector_xz(&(a._bounding_box._center - b._bounding_box._center));
    let to_a_dir = to_a.normalize();
    let a_radius_dir = a._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let a_radius: f32 = math::lerp(a._bounding_box._extents.z, a._bounding_box._extents.x, a_radius_dir);
    let b_radius_dir = b._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let b_radius: f32 = math::lerp(b._bounding_box._extents.z, b._bounding_box._extents.x, b_radius_dir);
    (to_a.x * to_a.x + to_a.z * to_a.z) <= (a_radius * a_radius + b_radius * b_radius)
}

pub fn collide_sphere_with_sphere(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y || a._bounding_box._max.y < b._bounding_box._min.y {
        return false;
    }

    let to_a = math::make_vector_xz(&(a._bounding_box._center - b._bounding_box._center));
    let a_radius_dir = a._bounding_box._orientation.column(0).dot(&to_a.normalize()).abs();
    let a_radius: f32 = math::lerp(a._bounding_box._extents.z, a._bounding_box._extents.x, a_radius_dir);
    let b_radius_dir = b._bounding_box._orientation.column(0).dot(&to_a.normalize()).abs();
    let b_radius: f32 = math::lerp(b._bounding_box._extents.z, b._bounding_box._extents.x, b_radius_dir);
    (to_a.x * to_a.x + to_a.z * to_a.z) <= (a_radius * a_radius + b_radius * b_radius)
}