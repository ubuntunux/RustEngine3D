use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::vulkan_context::queue;

pub unsafe fn create_command_pool(device: &Device, queue_family_data: &queue::QueueFamilyDatas) -> vk::CommandPool {
    let queue_family_index = queue_family_data._queue_family_indices._graphics_queue_index;
    let command_pool_create_info = vk::CommandPoolCreateInfo {
        flags: vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER,
        queue_family_index,
        ..Default::default()
    };
    log::info!("Create Command Pool: queueFamilyIndex({:?})", command_pool_create_info.queue_family_index);
    device.create_command_pool(&command_pool_create_info, None).expect("vkCreateCommandPool failed!")
}


pub unsafe fn destroy_command_pool(device: &Device, command_pool: vk::CommandPool) {
    log::info!("Destroy Command Pool: {:?}", command_pool);
    device.destroy_command_pool(command_pool, None);
}

pub unsafe fn create_command_buffers(device: &Device, command_pool: vk::CommandPool, command_buffer_count: u32) -> Vec<vk::CommandBuffer> {
    let allocation_info = vk::CommandBufferAllocateInfo::builder()
        .command_pool(command_pool)
        .level(vk::CommandBufferLevel::PRIMARY)
        .command_buffer_count(command_buffer_count);
    let command_buffers = device.allocate_command_buffers(&allocation_info).expect("vkAllocateCommandBuffers failed!");
    log::info!("Create Command Buffer: {:?}", command_buffers);
    command_buffers
}

pub unsafe fn destroy_command_buffers(device: &Device, command_pool: vk::CommandPool, command_buffers: &Vec<vk::CommandBuffer>) {
    log::info!("Destroy Command Buffers: {:?}", command_buffers);
    device.free_command_buffers(command_pool, command_buffers);
}
