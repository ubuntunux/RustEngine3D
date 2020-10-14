use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::constants;
use crate::vulkan_context::buffer;
use crate::vulkan_context::vulkan_context::{ SwapchainIndexMap };

#[derive(Debug, Clone)]
pub struct UniformBufferData {
    _uniform_buffer_name: String,
    _uniform_buffers: SwapchainIndexMap<buffer::BufferData>,
    _uniform_buffer_data_size: vk::DeviceSize,
    _descriptor_buffer_infos: SwapchainIndexMap<vk::DescriptorBufferInfo>
}

pub fn create_uniform_buffer(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    uniform_buffer_name: &String,
    buffer_count: u32,
    buffer_size: vk::DeviceSize
) -> Vec<buffer::BufferData> {
    log::info!("create_uniform_buffer: {}", uniform_buffer_name);
    (0..buffer_count).map(|index| {
        buffer::create_buffer_data(
            device,
            memory_properties,
            buffer_size,
            vk::BufferUsageFlags::UNIFORM_BUFFER,
            vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT
        )
    }).collect()
}

pub fn destroy_uniform_buffer(device: &Device, uniform_buffers: &SwapchainIndexMap<buffer::BufferData>) {
    log::info!("destroy_uniform_buffer");
    for uniform_buffer in uniform_buffers {
        buffer::destroy_buffer_data(device, uniform_buffer);
    }
}

pub fn create_uniform_buffer_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    uniform_buffer_name: &String,
    buffer_size: vk::DeviceSize
) -> UniformBufferData {
    let uniform_buffers = create_uniform_buffer(
        device,
        memory_properties,
        uniform_buffer_name,
        constants::SWAPCHAIN_IMAGE_COUNT,
        buffer_size
    );
    let descriptor_buffer_infos = uniform_buffers.iter().map(|buffer_data| {
        vk::DescriptorBufferInfo {
            buffer: buffer_data._buffer,
            offset: 0,
            range: buffer_size,
        }
    }).collect();

    UniformBufferData {
        _uniform_buffer_name: uniform_buffer_name.clone(),
        _uniform_buffers: uniform_buffers,
        _uniform_buffer_data_size: buffer_size,
        _descriptor_buffer_infos: descriptor_buffer_infos
    }
}

pub fn destroy_uniform_buffer_data(device: &Device, uniform_buffer_data: &UniformBufferData) {
    destroy_uniform_buffer(device, &uniform_buffer_data._uniform_buffers);
}
