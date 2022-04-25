use std::cmp::min;
use std::default::Default;
use std::vec::Vec;

use ash;
use ash::{
    vk,
    Device,
};

pub type CubeMapArray<T> = Vec<T>; // equivalent to [T; constants::CUBE_LAYER_COUNT as usize]
pub type SwapchainArray<T> = Vec<T>; // equivalent to [T; constants::SWAPCHAIN_IMAGE_COUNT as usize]
pub type FrameArray<T> = Vec<T>; // equivalent to [T; constants::MAX_FRAME_COUNT as usize]
pub type Layers<T> = Vec<T>; // image layer array
pub type MipLevels<T> = Vec<T>; // image mip level array

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, Copy)]
pub enum BlendMode {
    None,
    AlphaBlend,
    Additive,
    PreMultipliedAlpha,
}

#[derive(Debug, Clone)]
pub struct RenderFeatures {
    pub _physical_device_features: vk::PhysicalDeviceFeatures,
    pub _msaa_samples: vk::SampleCountFlags,
    pub _use_ray_tracing: bool,
}

pub fn get_format_size(format: vk::Format) -> u32 {
    match format {
        vk::Format::R32G32B32A32_UINT => 16,
        vk::Format::R32G32B32A32_SINT => 16,
        vk::Format::R32G32B32A32_SFLOAT => 16,
        vk::Format::R32G32B32_SFLOAT => 12,
        vk::Format::R32G32_SFLOAT => 8,
        vk::Format::R8G8B8A8_UNORM => 4,
        _ => panic!("Not implemented for format({:?})", format)
    }
}

pub fn get_color32(r: u32, g: u32, b: u32, a: u32) -> u32 {
    min(255, r) | (min(255, g) << 8) | (min(255, b) << 16) | (min(255, a) << 24)
}

pub fn get_color_blend_mode(blend_mode: BlendMode) -> vk::PipelineColorBlendAttachmentState {
    match blend_mode {
        BlendMode::AlphaBlend => vk::PipelineColorBlendAttachmentState {
            blend_enable: vk::TRUE,
            src_color_blend_factor: vk::BlendFactor::SRC_ALPHA,
            dst_color_blend_factor: vk::BlendFactor::ONE_MINUS_SRC_ALPHA,
            color_blend_op: vk::BlendOp::ADD,
            src_alpha_blend_factor: vk::BlendFactor::ONE,
            dst_alpha_blend_factor: vk::BlendFactor::ZERO,
            alpha_blend_op: vk::BlendOp::ADD,
            color_write_mask: vk::ColorComponentFlags::R | vk::ColorComponentFlags::G | vk::ColorComponentFlags::B,
        },
        BlendMode::Additive => vk::PipelineColorBlendAttachmentState {
            blend_enable: vk::TRUE,
            src_color_blend_factor: vk::BlendFactor::ONE,
            dst_color_blend_factor: vk::BlendFactor::ONE,
            color_blend_op: vk::BlendOp::ADD,
            src_alpha_blend_factor: vk::BlendFactor::ONE,
            dst_alpha_blend_factor: vk::BlendFactor::ONE,
            alpha_blend_op: vk::BlendOp::ADD,
            color_write_mask: vk::ColorComponentFlags::R | vk::ColorComponentFlags::G | vk::ColorComponentFlags::B,
        },
        BlendMode::PreMultipliedAlpha => vk::PipelineColorBlendAttachmentState {
            blend_enable: vk::TRUE,
            src_color_blend_factor: vk::BlendFactor::ONE,
            dst_color_blend_factor: vk::BlendFactor::ONE_MINUS_SRC_ALPHA,
            color_blend_op: vk::BlendOp::ADD,
            src_alpha_blend_factor: vk::BlendFactor::ONE,
            dst_alpha_blend_factor: vk::BlendFactor::ONE_MINUS_SRC_ALPHA,
            alpha_blend_op: vk::BlendOp::ADD,
            color_write_mask: vk::ColorComponentFlags::R | vk::ColorComponentFlags::G | vk::ColorComponentFlags::B,
        },
        BlendMode::None => vk::PipelineColorBlendAttachmentState {
            blend_enable: vk::FALSE,
            src_color_blend_factor: vk::BlendFactor::ONE,
            dst_color_blend_factor: vk::BlendFactor::ZERO,
            color_blend_op: vk::BlendOp::ADD,
            src_alpha_blend_factor: vk::BlendFactor::ONE,
            dst_alpha_blend_factor: vk::BlendFactor::ZERO,
            alpha_blend_op: vk::BlendOp::ADD,
            color_write_mask: vk::ColorComponentFlags::R | vk::ColorComponentFlags::G | vk::ColorComponentFlags::B | vk::ColorComponentFlags::A,
        },
    }
}

pub fn get_color_clear_zero() -> vk::ClearValue {
    vk::ClearValue {
        color: vk::ClearColorValue {
            float32: [0.0, 0.0, 0.0, 0.0]
        }
    }
}

pub fn get_color_clear_one() -> vk::ClearValue {
    vk::ClearValue {
        color: vk::ClearColorValue {
            float32: [1.0, 1.0, 1.0, 1.0]
        }
    }
}

pub fn get_color_clear_value(x: f32, y: f32, z: f32, w: f32) -> vk::ClearValue {
    vk::ClearValue {
        color: vk::ClearColorValue {
            float32: [x, y, z, w]
        }
    }
}

pub fn create_viewport(x: u32, y: u32, width: u32, height: u32, min_depth: f32, max_depth: f32) -> vk::Viewport {
    vk::Viewport {
        x: x as f32,
        y: y as f32,
        width: width as f32,
        height: height as f32,
        min_depth,
        max_depth
    }
}

pub fn create_rect_2d(x: i32, y: i32, width: u32, height: u32) -> vk::Rect2D {
    vk::Rect2D {
        offset: vk::Offset2D { x, y },
        extent: vk::Extent2D { width, height }
    }
}

pub fn get_depth_clear_one() -> vk::ClearValue {
    get_depth_stencil_clear_value(1.0, 0)
}

pub fn get_depth_stencil_clear_value(depth_clear_value: f32, stencil_clear_value: u32) -> vk::ClearValue {
    vk::ClearValue {
        depth_stencil: vk::ClearDepthStencilValue {
            depth: depth_clear_value,
            stencil: stencil_clear_value
        }
    }
}

pub fn record_submit_commandbuffer<F: FnOnce(&Device, vk::CommandBuffer)>(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    submit_queue: vk::Queue,
    wait_mask: &[vk::PipelineStageFlags],
    wait_semaphores: &[vk::Semaphore],
    signal_semaphores: &[vk::Semaphore],
    func: F,
) {
    unsafe {
        device.reset_command_buffer(command_buffer, vk::CommandBufferResetFlags::RELEASE_RESOURCES).expect("Reset command buffer failed.");
        let command_buffer_begin_info = vk::CommandBufferBeginInfo {
            flags: vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT,
            ..Default::default()
        };
        device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("Begin commandbuffer");
        func(device, command_buffer);
        device.end_command_buffer(command_buffer).expect("End commandbuffer");
        let submit_fence = device.create_fence(&vk::FenceCreateInfo::default(), None).expect("Create fence failed.");
        let command_buffers = vec![command_buffer];
        let submit_info = vk::SubmitInfo {
            wait_semaphore_count: wait_semaphores.len() as u32,
            p_wait_semaphores: wait_semaphores.as_ptr(),
            p_wait_dst_stage_mask: wait_mask.as_ptr(),
            command_buffer_count: command_buffers.len() as u32,
            p_command_buffers: command_buffers.as_ptr(),
            signal_semaphore_count: signal_semaphores.len() as u32,
            p_signal_semaphores: signal_semaphores.as_ptr(),
            ..Default::default()
        };
        device.queue_submit(submit_queue, &[submit_info], submit_fence).expect("queue submit failed.");
        device.wait_for_fences(&[submit_fence], true, std::u64::MAX).expect("Wait for fence failed.");
        device.destroy_fence(submit_fence, None);
    }
}

pub fn run_commands_once<F: FnOnce(&Device, vk::CommandBuffer)>(
    device: &Device,
    command_pool: vk::CommandPool,
    command_queue: vk::Queue,
    func: F,
) {
    unsafe {
        let allocate_info = vk::CommandBufferAllocateInfo {
            level: vk::CommandBufferLevel::PRIMARY,
            command_pool,
            command_buffer_count: 1,
            ..Default::default()
        };
        let command_buffers = device.allocate_command_buffers(&allocate_info).unwrap();
        let command_buffer = command_buffers[0];
        let command_buffer_begin_info = vk::CommandBufferBeginInfo {
            flags: vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT,
            ..Default::default()
        };

        device.begin_command_buffer(command_buffer, &command_buffer_begin_info).expect("Failed to begin_command_buffer");

        // execute function
        func(device, command_buffer);

        device.end_command_buffer(command_buffer).expect("Failed to end_command_buffer");

        let submit_info = vk::SubmitInfo {
            command_buffer_count: command_buffers.len() as u32,
            p_command_buffers: command_buffers.as_ptr(),
            ..Default::default()
        };

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
            device.queue_submit(command_queue, &[submit_info], vk::Fence::null()).expect("queue submit failed.");
            device.queue_wait_idle(command_queue).expect("vkQueueWaitIdle error");
        }
        device.free_command_buffers(command_pool, &command_buffers);
    }
}
