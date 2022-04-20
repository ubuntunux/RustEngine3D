use ash::extensions:: {
    khr,
    nv,
};
use ash::extensions::nv::RayTracing;
use ash::{
    vk,
    Device,
    Instance,
};
use ash::util::Align;

use nalgebra::Vector3;

use crate::constants;
use crate::renderer::utility::find_exactly_matching_memory_type_index;
use crate::vulkan_context::buffer;
use crate::vulkan_context::buffer::BufferData;
use crate::vulkan_context::vulkan_context::run_commands_once;

#[repr(C)]
#[derive(Clone, Debug, Copy)]
pub struct GeometryInstance {
    transform: [f32; 12],
    instance_id_and_mask: u32,
    instance_offset_and_flags: u32,
    acceleration_handle: u64,
}

#[derive(Debug, Clone)]
pub struct RayTracingCreateInfo {
}

#[derive(Debug, Clone)]
pub struct RayTracingData {
    pub _vertex_buffer_data: BufferData,
    pub _index_buffer_data: BufferData,
    pub _geometry_buffer_datas: Vec<vk::GeometryNV>,
    pub _instance_buffer_data: BufferData,
    pub _bottom_accel_struct: vk::AccelerationStructureNV,
    pub _bottom_accel_struct_memory: vk::DeviceMemory,
    pub _bottom_write_descriptor_set_accel_struct: vk::WriteDescriptorSetAccelerationStructureNV,
    pub _top_accel_struct: vk::AccelerationStructureNV,
    pub _top_accel_struct_memory: vk::DeviceMemory,
    pub _top_write_descriptor_set_accel_struct: vk::WriteDescriptorSetAccelerationStructureNV,
    pub _scratch_buffer_data: BufferData,
}


impl GeometryInstance {
    fn new(
        transform: [f32; 12],
        id: u32,
        mask: u8,
        offset: u32,
        flags: vk::GeometryInstanceFlagsNV,
        acceleration_handle: u64,
    ) -> Self {
        let mut instance = GeometryInstance {
            transform,
            instance_id_and_mask: 0,
            instance_offset_and_flags: 0,
            acceleration_handle,
        };
        instance.set_id(id);
        instance.set_mask(mask);
        instance.set_offset(offset);
        instance.set_flags(flags);
        instance
    }

    fn set_id(&mut self, id: u32) {
        let id = id & 0x00ffffff;
        self.instance_id_and_mask |= id;
    }

    fn set_mask(&mut self, mask: u8) {
        let mask = mask as u32;
        self.instance_id_and_mask |= mask << 24;
    }

    fn set_offset(&mut self, offset: u32) {
        let offset = offset & 0x00ffffff;
        self.instance_offset_and_flags |= offset;
    }

    fn set_flags(&mut self, flags: vk::GeometryInstanceFlagsNV) {
        let flags = flags.as_raw() as u32;
        self.instance_offset_and_flags |= flags << 24;
    }
}

pub fn create_acceleration_structure(
    device: &Device,
    device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    ray_tracing: &RayTracing,
    accel_create_info: &vk::AccelerationStructureCreateInfoNV
) -> (vk::AccelerationStructureNV, vk::DeviceMemory, vk::WriteDescriptorSetAccelerationStructureNV) {
    unsafe {
        let accel_struct = ray_tracing.create_acceleration_structure(accel_create_info, None).unwrap();
        let memory_requirements = ray_tracing.get_acceleration_structure_memory_requirements(
            &vk::AccelerationStructureMemoryRequirementsInfoNV {
                ty: vk::AccelerationStructureMemoryRequirementsTypeNV::OBJECT,
                acceleration_structure: accel_struct,
                ..Default::default()
            }
        );

        let memory_type_index = find_exactly_matching_memory_type_index(
            &memory_requirements.memory_requirements,
            device_memory_properties,
            vk::MemoryPropertyFlags::DEVICE_LOCAL,
        ).unwrap();

        let memory_allocate_create_info = vk::MemoryAllocateInfo {
            allocation_size: memory_requirements.memory_requirements.size,
            memory_type_index,
            ..Default::default()
        };

        let accel_struct_memory = device.allocate_memory(&memory_allocate_create_info, None).unwrap();
        let memory_info = vk::BindAccelerationStructureMemoryInfoNV {
            acceleration_structure: accel_struct,
            memory: accel_struct_memory,
            ..Default::default()
        };
        ray_tracing.bind_acceleration_structure_memory(&[memory_info]).unwrap();

        let write_descriptor_set_accel_struct = vk::WriteDescriptorSetAccelerationStructureNV {
            p_acceleration_structures: &accel_struct,
            acceleration_structure_count: 1,
            ..Default::default()
        };

        return (accel_struct, accel_struct_memory, write_descriptor_set_accel_struct);
    }
}

impl RayTracingData {
    pub fn create_ray_tracing_data() -> RayTracingData {
        RayTracingData {
            _vertex_buffer_data: BufferData::default(),
            _index_buffer_data: BufferData::default(),
            _geometry_buffer_datas: Vec::new(),
            _instance_buffer_data: BufferData::default(),
            _bottom_accel_struct: vk::AccelerationStructureNV::null(),
            _bottom_write_descriptor_set_accel_struct: vk::WriteDescriptorSetAccelerationStructureNV::default(),
            _bottom_accel_struct_memory: vk::DeviceMemory::null(),
            _top_accel_struct: vk::AccelerationStructureNV::null(),
            _top_write_descriptor_set_accel_struct: vk::WriteDescriptorSetAccelerationStructureNV::default(),
            _top_accel_struct_memory: vk::DeviceMemory::null(),
            _scratch_buffer_data: BufferData::default(),
        }
    }

    pub fn initialize_ray_tracing_data(
        &mut self,
        device: &Device,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
        ray_tracing: &RayTracing,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
    ) {
        unsafe {
            log::info!("create_acceleration_structures.");
            log::info!("////////////////////////////////////////////////");
            log::info!("");
            log::info!("TODO :: Clean up RayTracingData");
            log::info!("CEHCK :: do i need _geometry_buffer_datas as a member?");
            log::info!("CEHCK :: do i need _scratch_buffer_data as a member?");
            log::info!("CEHCK :: do i need present_queue ??");
            log::info!("");
            log::info!("////////////////////////////////////////////////");

            // Create vertex buffer
            let vertex_datas: Vec<Vector3<f32>> = vec![
                Vector3::new(-0.5, -0.5, 0.0),
                Vector3::new(0.0, 0.5, 0.0),
                Vector3::new(0.5, -0.5, 0.0)
            ];
            let vertex_stride = std::mem::size_of::<Vector3<f32>>();
            self._vertex_buffer_data = buffer::create_buffer_data_with_uploads(
                device,
                command_pool,
                command_queue,
                device_memory_properties,
                vk::BufferUsageFlags::VERTEX_BUFFER,
                &vertex_datas,
            );

            // Create index buffer
            let indices: Vec<u32> = vec![0, 1, 2];
            self._index_buffer_data = buffer::create_buffer_data_with_uploads(
                device,
                command_pool,
                command_queue,
                device_memory_properties,
                vk::BufferUsageFlags::INDEX_BUFFER,
                &indices
            );

            // Create geometry buffer
            self._geometry_buffer_datas = vec![
                vk::GeometryNV {
                    geometry_type: vk::GeometryTypeNV::TRIANGLES,
                    geometry: vk::GeometryDataNV {
                        triangles: vk::GeometryTrianglesNV {
                            vertex_data: self._vertex_buffer_data._buffer,
                            vertex_offset: 0,
                            vertex_count: vertex_datas.len() as u32,
                            vertex_stride: vertex_stride as u64,
                            vertex_format: vk::Format::R32G32B32_SFLOAT,
                            index_data: self._index_buffer_data._buffer,
                            index_offset: 0,
                            index_count: indices.len() as u32,
                            index_type: vk::IndexType::UINT16,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    flags: vk::GeometryFlagsNV::OPAQUE,
                    ..Default::default()
                }
            ];

            // Create bottom-level acceleration structure
            let bottom_structure_create_info = vk::AccelerationStructureCreateInfoNV {
                compacted_size: 0,
                info: vk::AccelerationStructureInfoNV {
                    ty: vk::AccelerationStructureTypeNV::BOTTOM_LEVEL,
                    geometry_count: self._geometry_buffer_datas.len() as u32,
                    p_geometries: self._geometry_buffer_datas.as_ptr(),
                    flags: vk::BuildAccelerationStructureFlagsNV::PREFER_FAST_TRACE,
                    ..Default::default()
                },
                ..Default::default()
            };

            (self._bottom_accel_struct, self._bottom_accel_struct_memory, self._bottom_write_descriptor_set_accel_struct) =
                create_acceleration_structure(
                    device,
                    device_memory_properties,
                    ray_tracing,
                    &bottom_structure_create_info
                );

            // Create instance buffer
            let accel_handle = ray_tracing.get_acceleration_structure_handle(self._bottom_accel_struct).unwrap();
            let transform_0: [f32; 12] = [1.0, 0.0, 0.0, -1.5, 0.0, 1.0, 0.0, 1.1, 0.0, 0.0, 1.0, 0.0];
            let transform_1: [f32; 12] = [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.1, 0.0, 0.0, 1.0, 0.0];
            let transform_2: [f32; 12] = [1.0, 0.0, 0.0, 1.5, 0.0, 1.0, 0.0, 1.1, 0.0, 0.0, 1.0, 0.0];
            let instance_datas = vec![
                GeometryInstance::new(transform_0, 0, 0xff, 0, vk::GeometryInstanceFlagsNV::TRIANGLE_FACING_CULL_DISABLE, accel_handle),
                GeometryInstance::new(transform_1, 1, 0xff, 0, vk::GeometryInstanceFlagsNV::TRIANGLE_FACING_CULL_DISABLE, accel_handle),
                GeometryInstance::new(transform_2, 2, 0xff, 0, vk::GeometryInstanceFlagsNV::TRIANGLE_FACING_CULL_DISABLE, accel_handle),
            ];
            self._instance_buffer_data = buffer::create_buffer_data_with_uploads(
                device,
                command_pool,
                command_queue,
                device_memory_properties,
                vk::BufferUsageFlags::RAY_TRACING_NV,
                &instance_datas
            );

            // Create top-level acceleration structure
            let top_structure_info = vk::AccelerationStructureCreateInfoNV {
                compacted_size: 0,
                info: vk::AccelerationStructureInfoNV {
                    ty: vk::AccelerationStructureTypeNV::TOP_LEVEL,
                    instance_count: instance_datas.len() as u32,
                    ..Default::default()
                },
                ..Default::default()
            };

            (self._top_accel_struct, self._top_accel_struct_memory, self._top_write_descriptor_set_accel_struct) =
                create_acceleration_structure(
                    device,
                    device_memory_properties,
                    ray_tracing,
                    &top_structure_info
                );

            // Build acceleration structures
            let bottom_memory_requirements = ray_tracing.get_acceleration_structure_memory_requirements(
                &vk::AccelerationStructureMemoryRequirementsInfoNV {
                    ty: vk::AccelerationStructureMemoryRequirementsTypeNV::BUILD_SCRATCH,
                    acceleration_structure: self._bottom_accel_struct,
                    ..Default::default()
                }
            );
            let top_memory_requirements = ray_tracing.get_acceleration_structure_memory_requirements(
                &vk::AccelerationStructureMemoryRequirementsInfoNV {
                    ty: vk::AccelerationStructureMemoryRequirementsTypeNV::BUILD_SCRATCH,
                    acceleration_structure: self._top_accel_struct,
                    ..Default::default()
                }
            );

            let scratch_buffer_size = std::cmp::max(bottom_memory_requirements.memory_requirements.size, top_memory_requirements.memory_requirements.size);
            self._scratch_buffer_data = buffer::create_buffer_data(
                device,
                device_memory_properties,
                scratch_buffer_size,
                vk::BufferUsageFlags::RAY_TRACING_NV,
                vk::MemoryPropertyFlags::DEVICE_LOCAL
            );

            // Begin - Run commands
            run_commands_once(device, command_pool, command_queue, |device: &Device, command_buffer: vk::CommandBuffer| {
                let memory_barrier = vk::MemoryBarrier {
                    src_access_mask: vk::AccessFlags::ACCELERATION_STRUCTURE_WRITE_NV | vk::AccessFlags::ACCELERATION_STRUCTURE_READ_NV,
                    dst_access_mask: vk::AccessFlags::ACCELERATION_STRUCTURE_WRITE_NV | vk::AccessFlags::ACCELERATION_STRUCTURE_READ_NV,
                    ..Default::default()
                };

                ray_tracing.cmd_build_acceleration_structure(
                    command_buffer,
                    &vk::AccelerationStructureInfoNV {
                        ty: vk::AccelerationStructureTypeNV::BOTTOM_LEVEL,
                        geometry_count: self._geometry_buffer_datas.len() as u32,
                        p_geometries: self._geometry_buffer_datas.as_ptr(),
                        ..Default::default()
                    },
                    vk::Buffer::null(),
                    0,
                    false,
                    self._bottom_accel_struct,
                    vk::AccelerationStructureNV::null(),
                    self._scratch_buffer_data._buffer,
                    0,
                );

                device.cmd_pipeline_barrier(
                    command_buffer,
                    vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
                    vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
                    vk::DependencyFlags::empty(),
                    &[memory_barrier],
                    &[],
                    &[],
                );

                ray_tracing.cmd_build_acceleration_structure(
                    command_buffer,
                    &vk::AccelerationStructureInfoNV {
                        ty: vk::AccelerationStructureTypeNV::TOP_LEVEL,
                        instance_count: instance_datas.len() as u32,
                        ..Default::default()
                    },
                    self._index_buffer_data._buffer,
                    0,
                    false,
                    self._top_accel_struct,
                    vk::AccelerationStructureNV::null(),
                    self._scratch_buffer_data._buffer,
                    0,
                );

                device.cmd_pipeline_barrier(
                    command_buffer,
                    vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
                    vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
                    vk::DependencyFlags::empty(),
                    &[memory_barrier],
                    &[],
                    &[],
                );
            }); // End - run_commands_once
        } // End - create_acceleration_structure_datas
    }

    pub fn destroy_ray_tracing_data(&mut self, device: &Device, ray_tracing: &RayTracing) {
        log::info!("destroy_ray_tracing_data");
        buffer::destroy_buffer_data(device, &self._vertex_buffer_data);
        buffer::destroy_buffer_data(device, &self._index_buffer_data);
        buffer::destroy_buffer_data(device, &self._instance_buffer_data);
        buffer::destroy_buffer_data(device, &self._scratch_buffer_data);
        unsafe {
            ray_tracing.destroy_acceleration_structure(self._top_accel_struct, None);
            device.free_memory(self._top_accel_struct_memory, None);

            ray_tracing.destroy_acceleration_structure(self._bottom_accel_struct, None);
            device.free_memory(self._bottom_accel_struct_memory, None);
        }
    }
}