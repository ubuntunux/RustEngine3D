use ash::{
    vk,
    Device,
};

use crate::constants;
use crate::vulkan_context::vulkan_context::{ FrameArray };

pub fn create_semaphores(device: &Device) -> FrameArray<vk::Semaphore> {
    unsafe {
        let semaphore_create_info = vk::SemaphoreCreateInfo::default();
        let semaphores = constants::SWAPCHAIN_IMAGE_INDICES
            .iter()
            .map(|_| {
                device.create_semaphore(&semaphore_create_info, None).expect("vkCreateSemaphore failed!")
            })
            .collect();
        log::debug!("create_semaphores: {:?}", semaphores);
        semaphores
    }
}

pub fn destroy_semaphores(device: &Device, semaphores: &FrameArray<vk::Semaphore>) {
    log::debug!("destroy_semaphores: {:?}", semaphores);
    unsafe {
        for semaphore in semaphores.iter() {
            device.destroy_semaphore(*semaphore, None)
        }
    }
}

pub fn create_fences(device: &Device) -> FrameArray<vk::Fence> {
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
        log::debug!("create_fences: {:?}", fences);
        fences
    }
}

pub fn destroy_fences(device: &Device, fences: &FrameArray<vk::Fence>) {
    log::debug!("destroy_fences: {:?}", fences);
    unsafe {
        for fence in fences.iter() {
            device.destroy_fence(*fence, None);
        }
    }
}