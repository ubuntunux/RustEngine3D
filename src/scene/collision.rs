use nalgebra::{Matrix3, Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::bounding_box::BoundingBox;

#[repr(i32)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum CollisionType {
    NONE,
    BOX,
    CYLINDER
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

    pub fn collide_cylinder_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }

        let to_point = &self._bounding_box._center;
        let length_x = self._bounding_box._orientation.column(0).dot(&to_point) / self._bounding_box._extents.x;
        let length_z = self._bounding_box._orientation.column(2).dot(&to_point) / self._bounding_box._extents.z;
        (length_x * length_x + length_z * length_z) < 1.0
    }

    pub fn collide_box_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }
        let to_point = &self._bounding_box._center;
        self._bounding_box._orientation.column(0).dot(&to_point).abs() <= self._bounding_box._extents.x &&
        self._bounding_box._orientation.column(2).dot(&to_point).abs() <= self._bounding_box._extents.z
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
        if other._collision_type == CollisionType::CYLINDER {
            if self._collision_type == CollisionType::CYLINDER {
                collide_cylinder_with_cylinder(self, other)
            } else {
                collide_box_with_cylinder(self, other)
            }
        } else {
            if self._collision_type == CollisionType::CYLINDER {
                collide_box_with_cylinder(other, self)
            } else {
                collide_box_with_box(self, other)
            }
        }
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
    let b_cylinder_radius: f32 = b_cylinder._bounding_box._extents.x.max(b_cylinder._bounding_box._extents.z);
    let to_a_box = a_box._bounding_box._center - b_cylinder._bounding_box._center;

    let to_box_pos_x0 = to_a_box + a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_x1 = to_a_box - a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_z0 = to_a_box + a_box_axis_z * a_box_half_sizes.z;
    let to_box_pos_z1 = to_a_box - a_box_axis_z * a_box_half_sizes.z;

    let d_x0 = a_box_axis_x.dot(&to_box_pos_x0).abs();
    let d_x1 = a_box_axis_x.dot(&to_box_pos_x1).abs();
    let distance_x = d_x0.min(d_x1);

    let d_z0 = a_box_axis_z.dot(&to_box_pos_z0).abs();
    let d_z1 = a_box_axis_z.dot(&to_box_pos_z1).abs();
    let distance_z = d_z0.min(d_z1);

    distance_x.abs() <= b_cylinder_radius && distance_z.abs() <= b_cylinder_radius
}

pub fn collide_cylinder_with_cylinder(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y || a._bounding_box._max.y < b._bounding_box._min.y {
        return false;
    }

    let a_radius: f32 = a._bounding_box._extents.x.max(a._bounding_box._extents.z);
    let b_radius: f32 = b._bounding_box._extents.x.max(b._bounding_box._extents.z);

    let to_a = a._bounding_box._center - b._bounding_box._center;
    (to_a.x * to_a.x + to_a.z * to_a.z) <= (a_radius * a_radius + b_radius * b_radius)
}