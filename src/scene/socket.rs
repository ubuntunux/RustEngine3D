use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use crate::utilities::math;
use crate::utilities::system::RcRefCell;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct SocketDataCreateInfo {
    pub _parent_bone: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>
}

impl Default for SocketDataCreateInfo {
    fn default() -> SocketDataCreateInfo {
        SocketDataCreateInfo {
            _parent_bone: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SocketData {
    pub _socket_name: String,
    pub _parent_bone_index: usize,
    pub _local_transform: Matrix4<f32>
}

impl SocketData {
    pub fn create_socket_data(socket_name: &String, socket_data_info: &SocketDataCreateInfo, parent_bone_index: usize) -> SocketData {
        log::debug!("create_socket_data: {:}, parent_bone_index: {:?}, socket_data_info: {:?}", socket_name, parent_bone_index, socket_data_info);
        SocketData {
            _socket_name: socket_name.clone(),
            _parent_bone_index: parent_bone_index,
            _local_transform: math::make_srt_transform(
                &socket_data_info._position,
                &socket_data_info._rotation,
                &socket_data_info._scale
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct Socket {
    pub _socket_data: RcRefCell<SocketData>,
    pub _transform: Matrix4<f32>
}

impl Socket {
    pub fn create_socket(socket_data: &RcRefCell<SocketData>, parent_bone_transform: &Matrix4<f32>) -> Socket {
        let mut socket = Socket {
            _socket_data: socket_data.clone(),
            _transform: Default::default(),
        };

        socket.update_socket(parent_bone_transform);
        socket
    }

    pub fn update_socket(&mut self, parent_bone_transform: &Matrix4<f32>) {
        self._transform = parent_bone_transform * self._socket_data.borrow()._local_transform;
    }
}
