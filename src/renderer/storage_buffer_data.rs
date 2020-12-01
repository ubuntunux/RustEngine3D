use std::cmp::{
    Eq,
    PartialEq
};
use std::collections::HashMap;

use ash::{
    vk,
    Device,
};

use nalgebra::{
    Vector2,
    Vector3,
    Vector4,
    Matrix4,
};

use crate::constants;
use crate::vulkan_context::buffer::{ self, BufferDataInfo };

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum StorageBufferType {
    BoneMatrices,
}

impl std::fmt::Display for StorageBufferType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::str::FromStr for StorageBufferType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "BoneMatrices" => Ok(StorageBufferType::BoneMatrices),
            _ => Err(format!("'{}' is not a valid value for StorageBufferType", s)),
        }
    }
}

pub type StorageBufferDataMap = HashMap<StorageBufferType, BufferDataInfo>;

#[derive(Clone)]
pub struct BoneMatrices {
    pub _prev_bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
    pub _bone_matrices: [Matrix4<f32>; constants::MAX_BONES],
}

impl Default for BoneMatrices {
    fn default() -> BoneMatrices {
        BoneMatrices {
            _prev_bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
            _bone_matrices: [Matrix4::identity() as Matrix4<f32>; constants::MAX_BONES],
        }
    }
}

pub fn regist_storage_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    storage_buffer_data_map: &mut StorageBufferDataMap,
    storage_buffer_data_type: StorageBufferType,
    storage_buffer_data_size: usize
) {
    let storage_buffer_data = buffer::create_buffer_data_info(
        device,
        memory_properties,
        &String::from(format!("{:?}", storage_buffer_data_type)),
        vk::BufferUsageFlags::STORAGE_BUFFER,
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
        constants::SWAPCHAIN_IMAGE_COUNT,
        storage_buffer_data_size as vk::DeviceSize
    );
    storage_buffer_data_map.insert(storage_buffer_data_type.clone(), storage_buffer_data);
}

pub fn regist_storage_datas(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    storage_buffer_data_map: &mut StorageBufferDataMap,
) {
    regist_storage_data(device, memory_properties, storage_buffer_data_map, StorageBufferType::BoneMatrices, std::mem::size_of::<BoneMatrices>());
}