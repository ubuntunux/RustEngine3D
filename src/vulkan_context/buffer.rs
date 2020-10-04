use std::os::raw::c_void;

use ash::{
    vk,
    Device,
    Instance,
};
use ash::version::{DeviceV1_0, InstanceV1_0};
use ash::util::Align;

pub struct BufferData {
    pub _buffer: vk::Buffer,
    pub _buffer_memory: vk::DeviceMemory,
    pub _buffer_memory_requirements: vk::MemoryRequirements
}

pub fn find_memory_type_index(
    memory_requirments: &vk::MemoryRequirements,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    flags: vk::MemoryPropertyFlags
) -> Option<u32> {
    let memory_type_bits = memory_requirments.memory_type_bits;
    // Try to find an exactly matching memory flag
    // for (index, ref memory_type) in memory_properties.memory_types.iter().enumerate() {
    //     let property_flags = memory_types[index].property_flags;
    //     if (0 != (memory_type_bits & (1 << index as u32))) && (flags == property_flags) {
    //         return Some(index as u32);
    //     }
    // }
    // Otherwise find a memory flag that works
    for (index, ref memory_type) in memory_properties.memory_types.iter().enumerate() {
        let property_flags = memory_type.property_flags;
        if (0 != (memory_type_bits & (1 << index as u32))) && (flags == (flags & property_flags)) {
            return Some(index as u32);
        }
    }
    None
}

pub fn create_buffer_data(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    buffer_size: vk::DeviceSize,
    buffer_usage_flags: vk::BufferUsageFlags,
    memory_property_flags: vk::MemoryPropertyFlags
) -> BufferData {
    unsafe {
        let buffer_create_info = vk::BufferCreateInfo {
            size: buffer_size,
            usage: buffer_usage_flags,
            sharing_mode: vk::SharingMode::EXCLUSIVE,
            ..Default::default()
        };
        let buffer = device.create_buffer(&buffer_create_info, None).expect("vkCreateBuffer failed!");
        let buffer_memory_requirements = device.get_buffer_memory_requirements(buffer);
        let memory_type_index = find_memory_type_index(&buffer_memory_requirements, memory_properties, memory_property_flags)
            .expect("Unable to find suitable memorytype for the vertex buffer.");
        let memory_allocate_info = vk::MemoryAllocateInfo {
            allocation_size: buffer_memory_requirements.size,
            memory_type_index: memory_type_index,
            ..Default::default()
        };
        let buffer_memory = device.allocate_memory(&memory_allocate_info, None).expect("vkAllocateMemory failed!");
        device.bind_buffer_memory(buffer, buffer_memory, 0).unwrap();

        log::info!("    Create Buffer: buffer({:?}), memory({:?})", buffer, buffer_memory);
        log::info!("        buffer_size: {:?}", buffer_size);
        log::info!("        memory_type_index: {:?}", memory_type_index);
        log::info!("        memory_requirements: {:?}", buffer_memory_requirements);

        BufferData {
            _buffer: buffer,
            _buffer_memory: buffer_memory,
            _buffer_memory_requirements: buffer_memory_requirements,
        }
    }
}

pub fn destroy_buffer_data(device: &Device, buffer_data: &BufferData) {
    unsafe {
        log::info!("    Destroy Buffer : buffer({:?}), memory({:?})", buffer_data._buffer, buffer_data._buffer_memory);
        device.destroy_buffer(buffer_data._buffer, None);
        device.free_memory(buffer_data._buffer_memory, None);
    }
}

pub fn upload_buffer_data<T: Copy> (device: &Device, buffer_data: &BufferData, upload_data: &[T]) {
    unsafe {
        let buffer_ptr = device.map_memory(buffer_data._buffer_memory, 0, buffer_data._buffer_memory_requirements.size, vk::MemoryMapFlags::empty()).unwrap();
        let mut slice = Align::new(
            buffer_ptr,
            buffer_data._buffer_memory_requirements.alignment,
            buffer_data._buffer_memory_requirements.size,
        );
        slice.copy_from_slice(upload_data);
        device.unmap_memory(buffer_data._buffer_memory);
    }
}

pub fn upload_buffer_data_offset<T: Copy> (device: &Device, buffer_data: &BufferData, upload_data: &[T], data_size: vk::DeviceSize, offset: vk::DeviceSize) {
    unsafe {
        let buffer_ptr = device.map_memory(buffer_data._buffer_memory, offset, data_size, vk::MemoryMapFlags::empty()).unwrap();
        let mut slice = Align::new(
            buffer_ptr,
            buffer_data._buffer_memory_requirements.alignment,
            data_size,
        );
        slice.copy_from_slice(upload_data);
        device.unmap_memory(buffer_data._buffer_memory);
    }
}

pub fn copy_buffer_region(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    src_buffer: vk::Buffer,
    dst_buffer: vk::Buffer,
    regions: &[vk::BufferCopy]
) {
    log::info!("\nPlease call copy_buffer with run_commands_once for immediatelly execution!!!!!\n");
    log::info!("    CopyBuffer : src_buffer({:?}), dst_buffer({:?}), regions({:?})", src_buffer, dst_buffer, regions);
    unsafe {
        device.cmd_copy_buffer(command_buffer, src_buffer, dst_buffer, regions);
    }
}

pub fn copy_buffer(
   device: &Device,
   command_buffer: vk::CommandBuffer,
   src_buffer: vk::Buffer,
   dst_buffer: vk::Buffer,
   buffer_size: vk::DeviceSize
) {
    let copy_region: [vk::BufferCopy; 1] = [vk::BufferCopy {
        src_offset: 0,
        dst_offset: 0,
        size: buffer_size
    }];
    unsafe {
        copy_buffer_region(device, command_buffer, src_buffer, dst_buffer, &copy_region);
    }
}