use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::constants;
use crate::vulkan_context::vulkan_context::{ FrameIndexMap };

pub unsafe fn create_semaphores(device: &Device) -> FrameIndexMap<vk::Semaphore> {
    let semaphore_create_info = vk::SemaphoreCreateInfo::default();
    let semaphores = constants::SWAPCHAIN_IMAGE_INDICES
        .iter()
        .map(|index| {
            device.create_semaphore(&semaphore_create_info, None).expect("vkCreateSemaphore failed!")
        })
        .collect();
    log::info!("Create Semaphore: {:?}", semaphores);
    semaphores
}

pub unsafe fn destroy_semaphores(device: &Device, semaphores: &FrameIndexMap<vk::Semaphore>) {
    log::info!("Destroy Semaphore: {:?}", semaphores);
    semaphores.iter().map(|semaphore| { device.destroy_semaphore(*semaphore, None) });
}

pub unsafe fn create_fences(device: &Device) -> FrameIndexMap<vk::Fence> {
    let fence_create_info = vk::FenceCreateInfo {
        flags: vk::FenceCreateFlags::SIGNALED,
        ..Default::default()
    };
    let fences = constants::FRAME_INDICES
        .iter()
        .map(|index| {
            device.create_fence(&fence_create_info, None).expect("vkCreateSemaphore failed!")
        })
        .collect();
    log::info!("Create VkFences: {:?}", fences);
    fences
}

pub unsafe fn destroy_ences(device: &Device, fences: FrameIndexMap<vk::Fence>) {
    log::info!("Destroy Fences: {:?}", fences);
    fences.iter().map(|fence| { device.destroy_fence(*fence, None) });
}