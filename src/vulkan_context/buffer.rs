use std::mem;

use ash::ext;
use ash::util::Align;
use ash::vk::Handle;
use ash::{vk, Device};

use crate::constants;
use crate::renderer::utility::find_memory_type_index;
use crate::vulkan_context::debug_utils;
use crate::vulkan_context::descriptor::DescriptorResourceInfo;
use crate::vulkan_context::vulkan_context::{run_commands_once, SwapchainArray};

#[derive(Debug, Clone, Copy)]
pub struct BufferData {
    pub _buffer: vk::Buffer,
    pub _buffer_memory: vk::DeviceMemory,
    pub _buffer_memory_requirements: vk::MemoryRequirements,
}

#[derive(Debug, Clone)]
pub struct ShaderBufferData<'a> {
    pub _buffer_name: String,
    pub _buffers: SwapchainArray<BufferData>,
    pub _buffer_data_size: vk::DeviceSize,
    pub _descriptor_buffer_infos: SwapchainArray<DescriptorResourceInfo<'a>>,
    pub _staging_buffers: Option<SwapchainArray<BufferData>>,
    pub _create_buffer_per_swapchain_count: bool,
}

impl Default for BufferData {
    fn default() -> BufferData {
        BufferData {
            _buffer: vk::Buffer::null(),
            _buffer_memory: vk::DeviceMemory::null(),
            _buffer_memory_requirements: vk::MemoryRequirements::default(),
        }
    }
}

pub fn create_buffer_data_with_immediate_uploads<T: Copy>(
    device: &Device,
    device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    debug_utils_device: &ext::debug_utils::Device,
    buffer_name: &str,
    dst_buffer_type: vk::BufferUsageFlags,
    upload_data_list: &Vec<T>,
) -> BufferData {
    let buffer_size = (mem::size_of::<T>() * upload_data_list.len()) as vk::DeviceSize;
    let buffer_usage_flags =
        dst_buffer_type | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST;
    let buffer_memory_property_flags =
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
    log::trace!(
        "CreateBuffer: type({:?}), size({})",
        dst_buffer_type,
        buffer_size
    );
    let dst_buffer_data = create_buffer_data(
        device,
        device_memory_properties,
        debug_utils_device,
        buffer_name,
        buffer_size,
        buffer_usage_flags,
        buffer_memory_property_flags,
    );
    upload_buffer_data(device, &dst_buffer_data, &upload_data_list);
    dst_buffer_data
}

pub fn create_buffer_data_with_uploads<T: Copy>(
    device: &Device,
    command_pool: vk::CommandPool,
    command_queue: vk::Queue,
    device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    debug_utils_device: &ext::debug_utils::Device,
    buffer_name: &str,
    dst_buffer_type: vk::BufferUsageFlags,
    upload_data_list: &Vec<T>,
) -> BufferData {
    let buffer_size = (mem::size_of::<T>() * upload_data_list.len()) as vk::DeviceSize;
    let buffer_usage_flags =
        dst_buffer_type | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST;
    let buffer_memory_property_flags = vk::MemoryPropertyFlags::DEVICE_LOCAL;
    log::trace!(
        "CreateBuffer: type({:?}), size({})",
        dst_buffer_type,
        buffer_size
    );

    // create temporary staging buffer
    let staging_buffer_usage_flags = vk::BufferUsageFlags::TRANSFER_SRC;
    let staging_buffer_memory_property_flags =
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
    let staging_buffer_data = create_buffer_data(
        device,
        device_memory_properties,
        debug_utils_device,
        buffer_name,
        buffer_size,
        staging_buffer_usage_flags,
        staging_buffer_memory_property_flags,
    );

    // upload data
    upload_buffer_data(device, &staging_buffer_data, &upload_data_list);

    // create vertex buffer & copy
    let dst_buffer_data = create_buffer_data(
        device,
        device_memory_properties,
        debug_utils_device,
        buffer_name,
        buffer_size,
        buffer_usage_flags,
        buffer_memory_property_flags,
    );

    // copy buffer
    run_commands_once(
        device,
        command_pool,
        command_queue,
        |device: &Device, command_buffer: vk::CommandBuffer| {
            copy_buffer(
                device,
                command_buffer,
                staging_buffer_data._buffer,
                dst_buffer_data._buffer,
                buffer_size,
            )
        },
    );

    // destroy temporary staging buffer
    destroy_buffer_data(device, &staging_buffer_data);

    dst_buffer_data
}

pub fn create_buffer_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    debug_utils_device: &ext::debug_utils::Device,
    buffer_name: &str,
    buffer_size: vk::DeviceSize,
    buffer_usage_flags: vk::BufferUsageFlags,
    memory_property_flags: vk::MemoryPropertyFlags,
) -> BufferData {
    unsafe {
        let buffer_create_info = vk::BufferCreateInfo {
            size: buffer_size,
            usage: buffer_usage_flags,
            sharing_mode: vk::SharingMode::EXCLUSIVE,
            ..Default::default()
        };
        let buffer = device
            .create_buffer(&buffer_create_info, None)
            .expect("vkCreateBuffer failed!");
        let buffer_memory_requirements = device.get_buffer_memory_requirements(buffer);
        let memory_type_index = find_memory_type_index(
            &buffer_memory_requirements,
            memory_properties,
            memory_property_flags,
        )
        .expect("Unable to find suitable memory type for the vertex buffer.");
        let memory_allocate_info = vk::MemoryAllocateInfo {
            allocation_size: buffer_memory_requirements.size,
            memory_type_index,
            ..Default::default()
        };
        let buffer_memory = device
            .allocate_memory(&memory_allocate_info, None)
            .expect("vkAllocateMemory failed!");
        device.bind_buffer_memory(buffer, buffer_memory, 0).unwrap();

        debug_utils::set_object_debug_info(
            debug_utils_device,
            buffer_name,
            vk::ObjectType::BUFFER,
            buffer.as_raw(),
        );

        log::trace!(
            "    Create Buffer ({:?}): ({:?}), buffer({:?}), memory({:?})",
            buffer_name,
            buffer_usage_flags,
            buffer,
            buffer_memory
        );
        log::trace!("        buffer_size: {:?}", buffer_size);
        log::trace!("        memory_type_index: {:?}", memory_type_index);
        log::trace!(
            "        memory_requirements: {:?}",
            buffer_memory_requirements
        );

        BufferData {
            _buffer: buffer,
            _buffer_memory: buffer_memory,
            _buffer_memory_requirements: buffer_memory_requirements,
        }
    }
}

pub fn destroy_buffer_data(device: &Device, buffer_data: &BufferData) {
    unsafe {
        log::trace!(
            "    Destroy Buffer: buffer({:?}), memory({:?})",
            buffer_data._buffer,
            buffer_data._buffer_memory
        );
        device.destroy_buffer(buffer_data._buffer, None);
        device.free_memory(buffer_data._buffer_memory, None);
    }
}

pub fn upload_buffer_data<T: Copy>(device: &Device, buffer_data: &BufferData, upload_data: &[T]) {
    unsafe {
        let upload_data_size = mem::size_of::<T>() as u64 * upload_data.len() as u64;
        assert!(upload_data_size <= buffer_data._buffer_memory_requirements.size);
        let buffer_ptr = device
            .map_memory(
                buffer_data._buffer_memory,
                0,
                upload_data_size,
                vk::MemoryMapFlags::empty(),
            )
            .unwrap();
        let mut slice = Align::new(buffer_ptr, mem::align_of::<T>() as u64, upload_data_size);
        slice.copy_from_slice(upload_data);
        device.unmap_memory(buffer_data._buffer_memory);
    }
}

pub fn read_buffer_data<T: Copy>(
    device: &Device,
    buffer_data: &BufferData,
    read_offset: u32,
    read_data: &mut [T],
) {
    unsafe {
        let read_data_count = read_data.len();
        let read_data_size = mem::size_of::<T>() as u64 * read_data_count as u64;
        let offset = mem::size_of::<T>() as u64 * read_offset as u64;
        assert!((offset + read_data_size) <= buffer_data._buffer_memory_requirements.size);
        let buffer_ptr = device
            .map_memory(
                buffer_data._buffer_memory,
                offset,
                read_data_size,
                vk::MemoryMapFlags::empty(),
            )
            .unwrap();
        let raw_data = std::ptr::slice_from_raw_parts(buffer_ptr, read_data_count) as *const [T];
        read_data.clone_from_slice(&*raw_data);
        device.unmap_memory(buffer_data._buffer_memory);
    }
}

pub fn upload_buffer_data_offset<T: Copy>(
    device: &Device,
    buffer_data: &BufferData,
    upload_data: &[T],
    offset: vk::DeviceSize,
) {
    unsafe {
        let upload_data_size = mem::size_of::<T>() as u64 * upload_data.len() as u64;
        assert!((upload_data_size + offset) <= buffer_data._buffer_memory_requirements.size);
        let buffer_ptr = device
            .map_memory(
                buffer_data._buffer_memory,
                offset,
                upload_data_size,
                vk::MemoryMapFlags::empty(),
            )
            .unwrap();
        let mut slice = Align::new(buffer_ptr, mem::align_of::<T>() as u64, upload_data_size);
        slice.copy_from_slice(upload_data);
        device.unmap_memory(buffer_data._buffer_memory);
    }
}

pub fn copy_buffer_region(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    src_buffer: vk::Buffer,
    dst_buffer: vk::Buffer,
    regions: &[vk::BufferCopy],
) {
    log::trace!(
        "    CopyBuffer : src_buffer({:?}), dst_buffer({:?}), regions({:?})",
        src_buffer,
        dst_buffer,
        regions
    );
    unsafe {
        device.cmd_copy_buffer(command_buffer, src_buffer, dst_buffer, regions);
    }
}

pub fn copy_buffer(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    src_buffer: vk::Buffer,
    dst_buffer: vk::Buffer,
    buffer_size: vk::DeviceSize,
) {
    let copy_region: [vk::BufferCopy; 1] = [vk::BufferCopy {
        src_offset: 0,
        dst_offset: 0,
        size: buffer_size,
    }];
    copy_buffer_region(device, command_buffer, src_buffer, dst_buffer, &copy_region);
}

pub fn copy_buffer_offset(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    src_buffer: vk::Buffer,
    src_offset: vk::DeviceSize,
    dst_buffer: vk::Buffer,
    dst_offset: vk::DeviceSize,
    buffer_size: vk::DeviceSize,
) {
    let copy_region: [vk::BufferCopy; 1] = [vk::BufferCopy {
        src_offset,
        dst_offset,
        size: buffer_size,
    }];
    copy_buffer_region(device, command_buffer, src_buffer, dst_buffer, &copy_region);
}

// ShaderBufferData
pub fn create_shader_buffer_data<'a>(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    debug_utils_device: &ext::debug_utils::Device,
    buffer_name: &String,
    buffer_usage: vk::BufferUsageFlags,
    buffer_size: vk::DeviceSize,
    create_buffer_per_swapchain_count: bool,
    has_staging_buffer: bool,
    is_device_local: bool,
) -> ShaderBufferData<'a> {
    log::debug!("create_shader_buffer_data: {}", buffer_name);

    // create buffer
    let buffer_usage_flags = if has_staging_buffer {
        buffer_usage | vk::BufferUsageFlags::TRANSFER_DST
    } else {
        if is_device_local {
            buffer_usage
        } else {
            buffer_usage | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST
        }
    };
    let memory_property_flags = if has_staging_buffer || is_device_local {
        vk::MemoryPropertyFlags::DEVICE_LOCAL
    } else {
        vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT
    };
    let buffers: SwapchainArray<BufferData> = if create_buffer_per_swapchain_count {
        (0..constants::SWAPCHAIN_IMAGE_COUNT).map(|_i| {
            create_buffer_data(
                device,
                memory_properties,
                debug_utils_device,
                buffer_name.as_str(),
                buffer_size,
                buffer_usage_flags,
                memory_property_flags,
            )
        }).collect()
    } else {
        let buffer = create_buffer_data(
            device,
            memory_properties,
            debug_utils_device,
            buffer_name.as_str(),
            buffer_size,
            buffer_usage_flags,
            memory_property_flags,
        );
        vec![buffer; constants::SWAPCHAIN_IMAGE_COUNT]
    };

    // staging buffer
    let staging_buffers: Option<SwapchainArray<BufferData>> = if has_staging_buffer {
        let staging_buffer_usage_flags = buffer_usage | vk::BufferUsageFlags::TRANSFER_SRC;
        let staging_memory_property_flags =
            vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
        Some(if create_buffer_per_swapchain_count {
            (0..constants::SWAPCHAIN_IMAGE_COUNT).map(|_i| {
                create_buffer_data(
                    device,
                    memory_properties,
                    debug_utils_device,
                    buffer_name,
                    buffer_size,
                    staging_buffer_usage_flags,
                    staging_memory_property_flags,
                )
            }).collect()
        } else {
            let buffer = create_buffer_data(
                device,
                memory_properties,
                debug_utils_device,
                buffer_name,
                buffer_size,
                staging_buffer_usage_flags,
                staging_memory_property_flags,
            );
            vec![buffer; constants::SWAPCHAIN_IMAGE_COUNT]
        })
    } else {
        None
    };

    let descriptor_buffer_infos: SwapchainArray<DescriptorResourceInfo> = buffers
        .iter()
        .map(|buffer_data| {
            DescriptorResourceInfo::DescriptorBufferInfo(vk::DescriptorBufferInfo {
                buffer: buffer_data._buffer,
                offset: 0,
                range: buffer_size,
            })
        })
        .collect();

    ShaderBufferData {
        _buffer_name: buffer_name.clone(),
        _buffers: buffers,
        _buffer_data_size: buffer_size,
        _descriptor_buffer_infos: descriptor_buffer_infos,
        _staging_buffers: staging_buffers,
        _create_buffer_per_swapchain_count: create_buffer_per_swapchain_count,
    }
}

pub fn destroy_shader_buffer_data(device: &Device, uniform_buffer_data: &mut ShaderBufferData) {
    log::debug!(
        "destroy_shader_buffer_data: {:?}",
        uniform_buffer_data._buffer_name
    );

    let buffer_count = if uniform_buffer_data._create_buffer_per_swapchain_count {
        uniform_buffer_data._buffers.len()
    } else {
        1
    };

    for i in 0..buffer_count {
        let uniform_buffer = &mut uniform_buffer_data._buffers[i];
        destroy_buffer_data(device, uniform_buffer);
    }
    uniform_buffer_data._buffers.clear();

    if let Some(staging_buffers) = &mut uniform_buffer_data._staging_buffers {
        for i in 0..buffer_count {
            let staging_buffer = &mut staging_buffers[i];
            destroy_buffer_data(device, staging_buffer);
        }
    }
    uniform_buffer_data._staging_buffers = None;
}
