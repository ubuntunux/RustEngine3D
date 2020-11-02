use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::constants;
use crate::vulkan_context::vulkan_context::{ FrameIndexMap };

pub fn create_semaphores(device: &Device) -> FrameIndexMap<vk::Semaphore> {
    unsafe {
        let semaphore_create_info = vk::SemaphoreCreateInfo::default();
        let semaphores = constants::SWAPCHAIN_IMAGE_INDICES
            .iter()
            .map(|_| {
                device.create_semaphore(&semaphore_create_info, None).expect("vkCreateSemaphore failed!")
            })
            .collect();
        log::info!("create_semaphores: {:?}", semaphores);
        semaphores
    }
}

pub fn destroy_semaphores(device: &Device, semaphores: &FrameIndexMap<vk::Semaphore>) {
    log::info!("destroy_semaphores: {:?}", semaphores);
    unsafe {
        for semaphore in semaphores.iter() {
            device.destroy_semaphore(*semaphore, None)
        }
    }
}

pub fn create_fences(device: &Device) -> FrameIndexMap<vk::Fence> {
    unsafe {
        let fence_create_info = vk::FenceCreateInfo {
            flags: vk::FenceCreateFlags::SIGNALED,
            ..Default::default()
        };
        let fences = constants::FRAME_INDICES
            .iter()
            .map(|_| {
                device.create_fence(&fence_create_info, None).expect("vkCreateSemaphore failed!")
            })
            .collect();
        log::info!("create_fences: {:?}", fences);
        fences
    }
}

pub fn destroy_fences(device: &Device, fences: &FrameIndexMap<vk::Fence>) {
    log::info!("destroy_fences: {:?}", fences);
    unsafe {
        for fence in fences.iter() {
            device.destroy_fence(*fence, None);
        }
    }
}