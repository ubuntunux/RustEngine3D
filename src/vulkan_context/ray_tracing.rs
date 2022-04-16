use ash::extensions:: {
    khr,
    nv,
};
use ash::extensions::nv::RayTracing;

#[derive(Debug, Clone)]
pub struct RayTracingCreateInfo {
}

#[derive(Debug, Clone)]
pub struct RayTracingData {
}

impl Default for RayTracingData {
    fn default() -> RayTracingData {
        RayTracingData {
        }
    }
}

// pub fn create_acceleration_structures() {
//     unsafe {
//         // Create geometry
//         let vertices = [
//             Vertex {
//                 pos: [-0.5, -0.5, 0.0],
//             },
//             Vertex {
//                 pos: [0.0, 0.5, 0.0],
//             },
//             Vertex {
//                 pos: [0.5, -0.5, 0.0],
//             },
//         ];
//
//         let vertex_count = vertices.len();
//         let vertex_stride = std::mem::size_of::<Vertex>();
//
//         let vertex_buffer_size = vertex_stride * vertex_count;
//         let mut vertex_buffer = BufferResource::new(
//             vertex_buffer_size as u64,
//             vk::BufferUsageFlags::VERTEX_BUFFER,
//             vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
//             self.base.clone(),
//         );
//         vertex_buffer.store(&vertices);
//
//         let indices = [0u16, 1, 2];
//         let index_count = indices.len();
//         let index_buffer_size = std::mem::size_of::<u16>() * index_count;
//         let mut index_buffer = BufferResource::new(
//             index_buffer_size as u64,
//             vk::BufferUsageFlags::INDEX_BUFFER,
//             vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
//             self.base.clone(),
//         );
//         index_buffer.store(&indices);
//
//         let geometry = vec![vk::GeometryNV::builder()
//             .geometry_type(vk::GeometryTypeNV::TRIANGLES)
//             .geometry(
//                 vk::GeometryDataNV::builder()
//                     .triangles(
//                         vk::GeometryTrianglesNV::builder()
//                             .vertex_data(vertex_buffer.buffer)
//                             .vertex_offset(0)
//                             .vertex_count(vertex_count as u32)
//                             .vertex_stride(vertex_stride as u64)
//                             .vertex_format(vk::Format::R32G32B32_SFLOAT)
//                             .index_data(index_buffer.buffer)
//                             .index_offset(0)
//                             .index_count(index_count as u32)
//                             .index_type(vk::IndexType::UINT16)
//                             .build(),
//                     )
//                     .build(),
//             )
//             .flags(vk::GeometryFlagsNV::OPAQUE)
//             .build()];
//
//         // Create bottom-level acceleration structure
//
//         let accel_info = vk::AccelerationStructureCreateInfoNV::builder()
//             .compacted_size(0)
//             .info(
//                 vk::AccelerationStructureInfoNV::builder()
//                     .ty(vk::AccelerationStructureTypeNV::BOTTOM_LEVEL)
//                     .geometries(&geometry)
//                     .flags(vk::BuildAccelerationStructureFlagsNV::PREFER_FAST_TRACE)
//                     .build(),
//             )
//             .build();
//
//         self.bottom_as = self
//             .ray_tracing
//             .create_acceleration_structure(&accel_info, None)
//             .unwrap();
//
//         let memory_requirements = self
//             .ray_tracing
//             .get_acceleration_structure_memory_requirements(
//                 &vk::AccelerationStructureMemoryRequirementsInfoNV::builder()
//                     .acceleration_structure(self.bottom_as)
//                     .ty(vk::AccelerationStructureMemoryRequirementsTypeNV::OBJECT)
//                     .build(),
//             );
//
//         self.bottom_as_memory = self
//             .base
//             .device
//             .allocate_memory(
//                 &vk::MemoryAllocateInfo::builder()
//                     .allocation_size(memory_requirements.memory_requirements.size)
//                     .memory_type_index(
//                         find_memorytype_index(
//                             &memory_requirements.memory_requirements,
//                             &self.base.device_memory_properties,
//                             vk::MemoryPropertyFlags::DEVICE_LOCAL,
//                         )
//                             .unwrap(),
//                     )
//                     .build(),
//                 None,
//             )
//             .unwrap();
//
//         self.ray_tracing
//             .bind_acceleration_structure_memory(&[
//                 vk::BindAccelerationStructureMemoryInfoNV::builder()
//                     .acceleration_structure(self.bottom_as)
//                     .memory(self.bottom_as_memory)
//                     .build(),
//             ])
//             .unwrap();
//
//         // Create instance buffer
//
//         let accel_handle = self
//             .ray_tracing
//             .get_acceleration_structure_handle(self.bottom_as)
//             .unwrap();
//
//         let transform_0: [f32; 12] =
//             [1.0, 0.0, 0.0, -1.5, 0.0, 1.0, 0.0, 1.1, 0.0, 0.0, 1.0, 0.0];
//
//         let transform_1: [f32; 12] =
//             [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.1, 0.0, 0.0, 1.0, 0.0];
//
//         let transform_2: [f32; 12] =
//             [1.0, 0.0, 0.0, 1.5, 0.0, 1.0, 0.0, 1.1, 0.0, 0.0, 1.0, 0.0];
//
//         let instances = vec![
//             GeometryInstance::new(
//                 transform_0,
//                 0, /* instance id */
//                 0xff,
//                 0,
//                 vk::GeometryInstanceFlagsNV::TRIANGLE_CULL_DISABLE,
//                 accel_handle,
//             ),
//             GeometryInstance::new(
//                 transform_1,
//                 1, /* instance id */
//                 0xff,
//                 0,
//                 vk::GeometryInstanceFlagsNV::TRIANGLE_CULL_DISABLE,
//                 accel_handle,
//             ),
//             GeometryInstance::new(
//                 transform_2,
//                 2, /* instance id */
//                 0xff,
//                 0,
//                 vk::GeometryInstanceFlagsNV::TRIANGLE_CULL_DISABLE,
//                 accel_handle,
//             ),
//         ];
//
//         let instance_buffer_size = std::mem::size_of::<GeometryInstance>() * instances.len();
//         let mut instance_buffer = BufferResource::new(
//             instance_buffer_size as u64,
//             vk::BufferUsageFlags::RAY_TRACING_NV,
//             vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT,
//             self.base.clone(),
//         );
//         instance_buffer.store(&instances);
//
//         // Create top-level acceleration structure
//
//         let accel_info = vk::AccelerationStructureCreateInfoNV::builder()
//             .compacted_size(0)
//             .info(
//                 vk::AccelerationStructureInfoNV::builder()
//                     .ty(vk::AccelerationStructureTypeNV::TOP_LEVEL)
//                     .instance_count(instances.len() as u32)
//                     .build(),
//             )
//             .build();
//
//         self.top_as = self
//             .ray_tracing
//             .create_acceleration_structure(&accel_info, None)
//             .unwrap();
//
//         let memory_requirements = self
//             .ray_tracing
//             .get_acceleration_structure_memory_requirements(
//                 &vk::AccelerationStructureMemoryRequirementsInfoNV::builder()
//                     .acceleration_structure(self.top_as)
//                     .ty(vk::AccelerationStructureMemoryRequirementsTypeNV::OBJECT)
//                     .build(),
//             );
//
//         self.top_as_memory = self
//             .base
//             .device
//             .allocate_memory(
//                 &vk::MemoryAllocateInfo::builder()
//                     .allocation_size(memory_requirements.memory_requirements.size)
//                     .memory_type_index(
//                         find_memorytype_index(
//                             &memory_requirements.memory_requirements,
//                             &self.base.device_memory_properties,
//                             vk::MemoryPropertyFlags::DEVICE_LOCAL,
//                         )
//                             .unwrap(),
//                     )
//                     .build(),
//                 None,
//             )
//             .unwrap();
//
//         self.ray_tracing
//             .bind_acceleration_structure_memory(&[
//                 vk::BindAccelerationStructureMemoryInfoNV::builder()
//                     .acceleration_structure(self.top_as)
//                     .memory(self.top_as_memory)
//                     .build(),
//             ])
//             .unwrap();
//
//         // Build acceleration structures
//
//         let bottom_as_size = {
//             let requirements = self
//                 .ray_tracing
//                 .get_acceleration_structure_memory_requirements(
//                     &vk::AccelerationStructureMemoryRequirementsInfoNV::builder()
//                         .acceleration_structure(self.bottom_as)
//                         .ty(vk::AccelerationStructureMemoryRequirementsTypeNV::BUILD_SCRATCH)
//                         .build(),
//                 );
//             requirements.memory_requirements.size
//         };
//
//         let top_as_size = {
//             let requirements = self
//                 .ray_tracing
//                 .get_acceleration_structure_memory_requirements(
//                     &vk::AccelerationStructureMemoryRequirementsInfoNV::builder()
//                         .acceleration_structure(self.top_as)
//                         .ty(vk::AccelerationStructureMemoryRequirementsTypeNV::BUILD_SCRATCH)
//                         .build(),
//                 );
//             requirements.memory_requirements.size
//         };
//
//         let scratch_buffer_size = std::cmp::max(bottom_as_size, top_as_size);
//         let scratch_buffer = BufferResource::new(
//             scratch_buffer_size,
//             vk::BufferUsageFlags::RAY_TRACING_NV,
//             vk::MemoryPropertyFlags::DEVICE_LOCAL,
//             self.base.clone(),
//         );
//
//         let allocate_info = vk::CommandBufferAllocateInfo::builder()
//             .command_buffer_count(1)
//             .command_pool(self.base.pool)
//             .level(vk::CommandBufferLevel::PRIMARY)
//             .build();
//
//         let command_buffers = self
//             .base
//             .device
//             .allocate_command_buffers(&allocate_info)
//             .unwrap();
//         let build_command_buffer = command_buffers[0];
//
//         self.base
//             .device
//             .begin_command_buffer(
//                 build_command_buffer,
//                 &vk::CommandBufferBeginInfo::builder()
//                     .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT)
//                     .build(),
//             )
//             .unwrap();
//
//         let memory_barrier = vk::MemoryBarrier::builder()
//             .src_access_mask(
//                 vk::AccessFlags::ACCELERATION_STRUCTURE_WRITE_NV
//                     | vk::AccessFlags::ACCELERATION_STRUCTURE_READ_NV,
//             )
//             .dst_access_mask(
//                 vk::AccessFlags::ACCELERATION_STRUCTURE_WRITE_NV
//                     | vk::AccessFlags::ACCELERATION_STRUCTURE_READ_NV,
//             )
//             .build();
//
//         self.ray_tracing.cmd_build_acceleration_structure(
//             build_command_buffer,
//             &vk::AccelerationStructureInfoNV::builder()
//                 .ty(vk::AccelerationStructureTypeNV::BOTTOM_LEVEL)
//                 .geometries(&geometry)
//                 .build(),
//             vk::Buffer::null(),
//             0,
//             false,
//             self.bottom_as,
//             vk::AccelerationStructureNV::null(),
//             scratch_buffer.buffer,
//             0,
//         );
//
//         self.base.device.cmd_pipeline_barrier(
//             build_command_buffer,
//             vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
//             vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
//             vk::DependencyFlags::empty(),
//             &[memory_barrier],
//             &[],
//             &[],
//         );
//
//         self.ray_tracing.cmd_build_acceleration_structure(
//             build_command_buffer,
//             &vk::AccelerationStructureInfoNV::builder()
//                 .ty(vk::AccelerationStructureTypeNV::TOP_LEVEL)
//                 .instance_count(instances.len() as u32)
//                 .build(),
//             instance_buffer.buffer,
//             0,
//             false,
//             self.top_as,
//             vk::AccelerationStructureNV::null(),
//             scratch_buffer.buffer,
//             0,
//         );
//
//         self.base.device.cmd_pipeline_barrier(
//             build_command_buffer,
//             vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
//             vk::PipelineStageFlags::ACCELERATION_STRUCTURE_BUILD_NV,
//             vk::DependencyFlags::empty(),
//             &[memory_barrier],
//             &[],
//             &[],
//         );
//
//         self.base
//             .device
//             .end_command_buffer(build_command_buffer)
//             .unwrap();
//
//         self.base
//             .device
//             .queue_submit(
//                 self.base.present_queue,
//                 &[vk::SubmitInfo::builder()
//                     .command_buffers(&[build_command_buffer])
//                     .build()],
//                 vk::Fence::null(),
//             )
//             .expect("queue submit failed.");
//
//         match self.base.device.queue_wait_idle(self.base.present_queue) {
//             Ok(_) => println!("Successfully built acceleration structures"),
//             Err(err) => {
//                 println!("Failed to build acceleration structures: {:?}", err);
//                 panic!("GPU ERROR");
//             }
//         }
//
//         self.base
//             .device
//             .free_command_buffers(self.base.pool, &[build_command_buffer]);
//     }
// }