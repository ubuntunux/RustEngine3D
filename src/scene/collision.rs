use nalgebra::{Vector3};
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
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>
}

impl Default for CollisionCreateInfo {
    fn default() -> CollisionCreateInfo {
        CollisionCreateInfo {
            _collision_type: CollisionType::NONE,
            _min: Vector3::new(-1.0, -1.0, -1.0),
            _max: Vector3::new(1.0, 1.0, 1.0)
        }
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
        CollisionData {
            _collision_type: collision_info._collision_type,
            _bounding_box: BoundingBox::create_bounding_box(&collision_info._min, &collision_info._max)
        }
    }
}