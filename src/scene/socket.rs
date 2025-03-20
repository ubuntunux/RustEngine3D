use nalgebra::Vector3;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct SocketCreateInfo {
    pub _parent_bone: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>
}

impl Default for SocketCreateInfo {
    fn default() -> SocketCreateInfo {
        SocketCreateInfo {
            _parent_bone: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}