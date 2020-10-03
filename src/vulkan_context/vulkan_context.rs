use std::borrow::Cow;
use std::default::Default;
use std::ffi::{
    CStr,
    CString,
};
use std::mem;
use std::mem::align_of;
use std::ops::Drop;
use std::rc::Rc;
use std::sync::Arc;
use std::vec::Vec;

use ash;
use ash::{
    vk,
    Device,
    Entry,
    Instance,
};
use ash::extensions::ext::DebugUtils;
use ash::version::{
    DeviceV1_0,
    EntryV1_0,
    InstanceV1_0,
};

use crate::constants;


enum BlendMode {
    None,
    AlphaBlend
}

pub type SwapchainIndexMap<T> = Vec<T>; // equivalent to [T; constants::SWAPCHAIN_IMAGE_COUNT as usize]
pub type FrameIndexMap<T> = Vec<T>; // equivalent to [T; constants::SWAPCHAIN_IMAGE_COUNT as usize]

#[derive(Debug, Clone)]
pub struct RenderFeatures {
    pub _physical_device_features: vk::PhysicalDeviceFeatures,
    pub _msaa_samples: vk::SampleCountFlags
}

// Simple offset_of macro akin to C++ offsetof
#[macro_export]
macro_rules! offset_of {
    ($base:path, $field:ident) => {{
        #[allow(unused_unsafe)]
        unsafe {
            let b: $base = mem::zeroed();
            (&b.$field as *const _ as isize) - (&b as *const _ as isize)
        }
    }};
}

pub unsafe fn record_submit_commandbuffer<D: DeviceV1_0, F: FnOnce(&D, vk::CommandBuffer)>(
    device: &D,
    command_buffer: vk::CommandBuffer,
    submit_queue: vk::Queue,
    wait_mask: &[vk::PipelineStageFlags],
    wait_semaphores: &[vk::Semaphore],
    signal_semaphores: &[vk::Semaphore],
    func: F,
) {
    device.reset_command_buffer(command_buffer, vk::CommandBufferResetFlags::RELEASE_RESOURCES).expect("Reset command buffer failed.");
    let command_buffer_begin_info = vk::CommandBufferBeginInfo::builder()
        .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
        .build();
    device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("Begin commandbuffer");
    func(device, command_buffer);
    device.end_command_buffer(command_buffer).expect("End commandbuffer");
    let submit_fence = device.create_fence(&vk::FenceCreateInfo::default(), None).expect("Create fence failed.");
    let command_buffers = vec![command_buffer];
    let submit_info = vk::SubmitInfo::builder()
        .wait_semaphores(wait_semaphores)
        .wait_dst_stage_mask(wait_mask)
        .command_buffers(&command_buffers)
        .signal_semaphores(signal_semaphores)
        .build();
    device.queue_submit(submit_queue, &[submit_info], submit_fence).expect("queue submit failed.");
    device.wait_for_fences(&[submit_fence], true, std::u64::MAX).expect("Wait for fence failed.");
    device.destroy_fence(submit_fence, None);
}

pub unsafe fn run_commands_once<D: DeviceV1_0, F: FnOnce(&D, vk::CommandBuffer)>(
    device: &D,
    command_pool: vk::CommandPool,
    command_queue: vk::Queue,
    func: F,
) {
    let allocate_info = vk::CommandBufferAllocateInfo::builder()
        .level(vk::CommandBufferLevel::PRIMARY)
        .command_pool(command_pool)
        .command_buffer_count(1)
        .build();
    let command_buffers = device.allocate_command_buffers(&allocate_info).unwrap();
    let command_buffer = command_buffers[0];
    let command_buffer_begin_info = vk::CommandBufferBeginInfo::builder()
        .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
        .build();

    device.begin_command_buffer(command_buffer, &command_buffer_begin_info);

    // execute function
    func(device, command_buffer);

    device.end_command_buffer(command_buffer);

    let submit_info = vk::SubmitInfo::builder()
        .command_buffers(&command_buffers)
        .build();

    // TODO: a real app would need a better logic for waiting.
    // In the example below, we create a new fence every time we want to
    // execute a single command. Then, we attach this fence to our command.
    // vkWaitForFences makes the host (CPU) wait until the command is executed.
    // The other way to do this thing is vkQueueWaitIdle.
    //
    // I guess, a good approach could be to pass the fence to this function
    // from the call site. The call site would decide when it wants to wait
    // for this command to finish.
    //
    // Even if we don't pass the fence from outside, maybe we should create
    // the fence oustise of the innermost `locally` scope. This way, the
    // fence would be shared between calls (on the other hand, a possible
    // concurrency would be hurt in this case).
    const SYNC_WITH_FENCE: bool = false;
    if SYNC_WITH_FENCE {
        let submit_fence = device.create_fence(&vk::FenceCreateInfo::default(), None).expect("Create fence failed.");
        device.queue_submit(command_queue, &[submit_info], submit_fence).expect("queue submit failed.");
        device.wait_for_fences(&[submit_fence], true, std::u64::MAX).expect("Wait for fence failed.");
        device.destroy_fence(submit_fence, None);
    } else {
        device.queue_submit(command_queue, &[submit_info], *std::ptr::null::<vk::Fence>()).expect("queue submit failed.");
        device.queue_wait_idle(command_queue).expect("vkQueueWaitIdle error");
    }
    device.free_command_buffers(command_pool, &command_buffers);
}

//
// getColor32 :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
// getColor32 r g b a = (min 255 r) .|. shift (min 255 g) 8 .|. shift (min 255 b) 16 .|. shift (min 255 a) 24
//
// getColorBlendMode :: BlendMode -> VkPipelineColorBlendAttachmentState
// getColorBlendMode blendMode =
//     case blendMode of
//         BlendMode_None -> createVk @VkPipelineColorBlendAttachmentState
//             $  set @"colorWriteMask" ( VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT )
//             &* set @"blendEnable" VK_FALSE
//             &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
//             &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
//             &* set @"colorBlendOp" VK_BLEND_OP_ADD
//             &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
//             &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
//             &* set @"alphaBlendOp" VK_BLEND_OP_ADD
//         BlendMode_AlphaBlend -> createVk @VkPipelineColorBlendAttachmentState
//              $  set @"colorWriteMask" ( VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT )
//              &* set @"blendEnable" VK_TRUE
//              &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_SRC_ALPHA
//              &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
//              &* set @"colorBlendOp" VK_BLEND_OP_ADD
//              &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
//              &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
//              &* set @"alphaBlendOp" VK_BLEND_OP_ADD
//
// getColorClearValue :: [Float] -> VkClearValue
// getColorClearValue colors = createVk @VkClearValue
//     $ setVk @"color"
//         $ setVec @"float32" (toColorVector colors)
//     where
//         toColorVector :: [Float] -> Vec4f
//         toColorVector (x:y:z:w:xs) = vec4 x y z w
//         toColorVector (x:y:z:xs) = vec4 x y z 0
//         toColorVector (x:y:xs) = vec4 x y 0 0
//         toColorVector (x:xs) = vec4 x 0 0 0
//         toColorVector _ = vec4 0 0 0 0
//
// getDepthStencilClearValue :: Float -> Word32 -> VkClearValue
// getDepthStencilClearValue depthClearValue stencilClearValue = createVk @VkClearValue
//     $ setVk @"depthStencil"
//         $  set @"depth" depthClearValue
//         &* set @"stencil" stencilClearValue
//
// createViewport :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> VkViewport
// createViewport x y width height minDepth maxDepth = createVk @VkViewport
//     $  set @"x" (fromIntegral x)
//     &* set @"y" (fromIntegral y)
//     &* set @"width" (fromIntegral width)
//     &* set @"height" (fromIntegral height)
//     &* set @"minDepth" (fromIntegral minDepth)
//     &* set @"maxDepth" (fromIntegral maxDepth)
//
// createScissorRect :: Int32 -> Int32 -> Word32 -> Word32 -> VkRect2D
// createScissorRect x y width height = createVk @VkRect2D
//     $  setVk @"extent"
//         (  set @"width" width
//         &* set @"height" height
//         )
//     &* setVk @"offset"
//         (  set @"x" x
//         &* set @"y" y
//         )
//
// runCommandsOnce :: VkDevice
//                 -> VkCommandPool
//                 -> VkQueue
//                 -> (VkCommandBuffer -> IO ())
//                 -> IO ()
// runCommandsOnce device commandPool commandQueue action = do
//     let allocInfo = createVk @VkCommandBufferAllocateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
//             &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
//             &* set @"commandPool" commandPool
//             &* set @"commandBufferCount" 1
//             &* set @"pNext" VK_NULL
//
//     allocaPeek $ \commandBufferPtr -> do
//         withPtr allocInfo $ \allocInfoPtr -> do
//             vkAllocateCommandBuffers device allocInfoPtr commandBufferPtr
//         commandBuffer <- peek commandBufferPtr
//
//         let beginInfo = createVk @VkCommandBufferBeginInfo
//                 $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
//                 &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
//                 &* set @"pNext" VK_NULL
//
//         withPtr beginInfo $ \beginInfoPtr -> do
//             vkBeginCommandBuffer commandBuffer beginInfoPtr
//
//         -- run action
//         action commandBuffer
//
//         vkEndCommandBuffer commandBuffer
//
//         let submitInfo = createVk @VkSubmitInfo
//                 $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
//                 &* set @"pNext" VK_NULL
//                 &* set @"waitSemaphoreCount" 0
//                 &* set @"pWaitSemaphores"   VK_NULL
//                 &* set @"pWaitDstStageMask" VK_NULL
//                 &* set @"commandBufferCount" 1
//                 &* set @"pCommandBuffers" commandBufferPtr
//                 &* set @"signalSemaphoreCount" 0
//                 &* set @"pSignalSemaphores" VK_NULL
//
//         {- TODO: a real app would need a better logic for waiting.
//
//                  In the example below, we create a new fence every time we want to
//                  execute a single command. Then, we attach this fence to our command.
//                  vkWaitForFences makes the host (CPU) wait until the command is executed.
//                  The other way to do this thing is vkQueueWaitIdle.
//
//                  I guess, a good approach could be to pass the fence to this function
//                  from the call site. The call site would decide when it wants to wait
//                  for this command to finish.
//
//                  Even if we don't pass the fence from outside, maybe we should create
//                  the fence oustise of the innermost `locally` scope. This way, the
//                  fence would be shared between calls (on the other hand, a possible
//                  concurrency would be hurt in this case).
//                -}
//         --   fence <- createFence dev False
//         --   withVkPtr submitInfo $ \siPtr ->
//         --       vkQueueSubmit cmdQueue 1 siPtr fence
//         --   fencePtr <- newArrayPtr [fence]
//         --   vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)
//
//         withPtr submitInfo $ \submitInfoPtr -> do
//             result <- vkQueueSubmit commandQueue 1 submitInfoPtr VK_NULL_HANDLE
//             validationVK result "vkQueueSubmit error"
//
//         vkQueueWaitIdle commandQueue >>= flip validationVK "vkQueueWaitIdle error"
//
//         vkFreeCommandBuffers device commandPool 1 commandBufferPtr
//     return ()