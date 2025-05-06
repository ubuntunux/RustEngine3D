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

    pub fn collide_collision(&self, other: &CollisionData) -> bool {
        let location = &self._bounding_box._center;
        let radius = self._bounding_box._extents.x;
        let height = self._bounding_box._extents.y * 2.0;

        let other_location = &other._bounding_box._center;
        let other_radius = other._bounding_box._extents.x;
        let other_height = other._bounding_box._extents.y * 2.0;

        if other._collision_type == CollisionType::CYLINDER {
            if self._collision_type == CollisionType::CYLINDER {
                collide_cylinder_with_cylinder(&location, radius, height, &other_location, other_radius, other_height)
            } else {
                collide_box_with_cylinder(&self._bounding_box._min, &self._bounding_box._max, &other_location, other_radius, other_height)
            }
        } else {
            if self._collision_type == CollisionType::CYLINDER {
                collide_box_with_cylinder(&other._bounding_box._min, &other._bounding_box._max, &location, radius, height)
            } else {
                collide_box_with_box(self, other)
            }
        }
    }
}

pub fn collide_aabb(box_min_pos_a: &Vector3<f32>, box_max_pos_a: &Vector3<f32>, box_min_pos_b: &Vector3<f32>, box_max_pos_b: &Vector3<f32>) -> bool {
    box_min_pos_a.x < box_max_pos_b.x && box_min_pos_b.x < box_max_pos_a.x &&
    box_min_pos_a.y < box_max_pos_b.y && box_min_pos_b.y < box_max_pos_a.y &&
    box_min_pos_a.z < box_max_pos_b.z && box_min_pos_b.z < box_max_pos_a.z
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

pub fn collide_box_with_cylinder(box_min_pos: &Vector3<f32>, box_max_pos: &Vector3<f32>, cylinder_pos: &Vector3<f32>, radius: f32, height: f32) -> bool {
    let cylinder_min_y = cylinder_pos.y - height * 0.5;
    let cylinder_max_y = cylinder_pos.y + height * 0.5;

    if cylinder_max_y < box_min_pos.y || box_max_pos.y < cylinder_min_y {
        return false;
    }

    // Find the closest point on the rectangle to the circle
    let closest_x = box_min_pos.x.max(box_max_pos.x.min(cylinder_pos.x));
    let closest_z = box_min_pos.z.max(box_max_pos.z.min(cylinder_pos.z));

    // Calculate the distance from the circle's center to this point
    let distance_x = cylinder_pos.x - closest_x;
    let distance_z = cylinder_pos.z - closest_z;

    // Check if the distance is less than or equal to the circle's radius
    //(distance_x * distance_x + distance_z * distance_z) <= (radius * radius)
    distance_x.abs() <= radius && distance_z.abs() <= radius
}

pub fn collide_cylinder_with_cylinder(cylinder_pos_a: &Vector3<f32>, radius_a: f32, height_a: f32, cylinder_pos_b: &Vector3<f32>, radius_b: f32, height_b: f32) -> bool {
    let cylinder_a_min_y = cylinder_pos_a.y - height_a * 0.5;
    let cylinder_a_max_y = cylinder_pos_a.y + height_a * 0.5;
    let cylinder_b_min_y = cylinder_pos_b.y - height_b * 0.5;
    let cylinder_b_max_y = cylinder_pos_b.y + height_b * 0.5;

    if cylinder_a_max_y < cylinder_b_min_y || cylinder_b_max_y < cylinder_a_min_y {
        return false;
    }

    let distance_x = cylinder_pos_a.x - cylinder_pos_b.x;
    let distance_z = cylinder_pos_a.z - cylinder_pos_b.z;
    (distance_x * distance_x + distance_z * distance_z) <= (radius_a * radius_a + radius_b * radius_b)
}