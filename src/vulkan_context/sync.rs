use ash::{
    vk,
    Device,
    Instance,
};

use crate::constants;
use crate::vulkan_context::vulkan_context::{ FrameIndexMap };
use ash::version::DeviceV1_0;

pub unsafe fn create_semaphores(device: &Device) -> FrameIndexMap<vk::Semaphore> {
    let semaphore_create_info = vk::SemaphoreCreateInfo::default();
    let semaphores = constants::SWAPCHAIN_IMAGE_INDICES
        .iter()
        .map(|index| {
            device.create_semaphore(&semaphore_create_info, None).expect("vkCreateSemaphore failed!")
        })
        .collect();
    log::info!("Create Semaphore: {:?}" semaphores);
    semaphores
}

pub unsafe fn destroy_semaphores(device: &Device, semaphores: &FrameIndexMap<vk::Semaphore>) {
    log::info!("Destroy Semaphore: {:?}", semaphores);
    semaphores.iter().map(|semaphore| { device.destroy_semaphore(*semaphore, None) });
}

pub unsafe fn create_frame_fences(device: &Device) -> vk::Fence {
    let fence_create_info = vk::FenceCreateInfo {
        flags: vk::FenceCreateFlags::SIGNALED,
        ..Default::default()
    };
    let fences = constants::FRAME_INDICES
        .iter()
        .map(|index| {

        })
        .collect();
}

createFrameFences device = do
  frameFencesPtr <- mallocArray Constants.maxFrameCount
  let fenceCreateInfo = createVk @VkFenceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_FENCE_CREATE_SIGNALED_BIT
  forM_ [0..(Constants.maxFrameCount - 1)] $ \index -> do
    withPtr fenceCreateInfo $ \fenceCreateInfoPtr -> do
        result <- vkCreateFence device fenceCreateInfoPtr VK_NULL (ptrAtIndex frameFencesPtr index)
        validationVK result "vkCreateSemaphore failed!"
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Create VkFences: " ++ show fences
  return frameFencesPtr

destroyFrameFences :: VkDevice -> Ptr VkFence -> IO ()
destroyFrameFences device frameFencesPtr = do
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Destroy VkFence: " ++ show fences
  forM_ fences $ \fence ->
    vkDestroyFence device fence VK_NULL
  free frameFencesPtr