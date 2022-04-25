use std::ffi::c_void;

use ash::{
    vk,
    Device,
};

use crate::constants;
use crate::renderer::utility::ptr_chain_iter;
use crate::vulkan_context::vulkan_context::SwapchainArray;


#[derive(Debug, Clone)]
pub enum DescriptorResourceInfo {
    DescriptorBufferInfo(vk::DescriptorBufferInfo),
    DescriptorImageInfo(vk::DescriptorImageInfo),
    WriteDescriptorSetAccelerationStructure(*const vk::WriteDescriptorSetAccelerationStructureNV),
    InvalidDescriptorInfo
}

#[derive(Debug, Clone)]
pub enum DescriptorResourceType {
    UniformBuffer,
    StorageBuffer,
    Texture,
    RenderTarget,
    StorageTexture,
    StorageRenderTarget,
    AccelerationStructure,
}

#[derive(Debug, Clone)]
pub struct DescriptorDataCreateInfo {
    pub _descriptor_binding_index: u32,
    pub _descriptor_name: String,
    pub _descriptor_resource_type: DescriptorResourceType,
    pub _descriptor_image_layer: u32,
    pub _descriptor_image_mip_level: u32,
    pub _descriptor_shader_stage: vk::ShaderStageFlags,
}

impl Default for DescriptorDataCreateInfo {
    fn default() -> DescriptorDataCreateInfo {
        DescriptorDataCreateInfo {
            _descriptor_binding_index: 0,
            _descriptor_name: String::new(),
            _descriptor_resource_type: DescriptorResourceType::Texture,
            _descriptor_image_layer: constants::WHOLE_LAYERS,
            _descriptor_image_mip_level: constants::WHOLE_MIP_LEVELS,
            _descriptor_shader_stage: vk::ShaderStageFlags::ALL,
        }
    }
}

impl DescriptorDataCreateInfo {
    pub fn use_sub_image(&self) -> bool {
        constants::WHOLE_LAYERS != self._descriptor_image_layer || constants::WHOLE_MIP_LEVELS != self._descriptor_image_mip_level
    }
}

#[derive(Debug, Clone)]
pub struct DescriptorData {
    pub _descriptor_data_create_infos: Vec<DescriptorDataCreateInfo>,
    pub _descriptor_set_layout_bindings: Vec<vk::DescriptorSetLayoutBinding>,
    pub _descriptor_pool_sizes: Vec<vk::DescriptorPoolSize>,
    pub _descriptor_pool: vk::DescriptorPool,
    pub _descriptor_set_layout: vk::DescriptorSetLayout,
    pub _max_descriptor_sets_count: u32,
}

impl Default for DescriptorData {
    fn default() -> DescriptorData {
        DescriptorData {
            _descriptor_data_create_infos: Vec::<DescriptorDataCreateInfo>::new(),
            _descriptor_set_layout_bindings: Vec::<vk::DescriptorSetLayoutBinding>::new(),
            _descriptor_pool_sizes: Vec::<vk::DescriptorPoolSize>::new(),
            _descriptor_pool: vk::DescriptorPool::null(),
            _descriptor_set_layout: vk::DescriptorSetLayout::null(),
            _max_descriptor_sets_count: 0,
        }
    }
}


pub fn write_descriptor_set_push_next<T: vk::ExtendsWriteDescriptorSet>(w: &mut vk::WriteDescriptorSet, next_ptr: *const T) {
    unsafe {
        let next_mut = &mut (*(next_ptr as *mut T));
        let last_next = ptr_chain_iter(next_mut).last().unwrap();
        (*last_next).p_next = w.p_next as _;
        w.p_next = next_ptr as *const c_void;
    }
}

pub fn convert_resource_type_to_descriptor_type(resource_type: &DescriptorResourceType) -> vk::DescriptorType {
    match resource_type {
        DescriptorResourceType::UniformBuffer => vk::DescriptorType::UNIFORM_BUFFER,
        DescriptorResourceType::StorageBuffer => vk::DescriptorType::STORAGE_BUFFER,
        DescriptorResourceType::Texture => vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
        DescriptorResourceType::RenderTarget => vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
        DescriptorResourceType::StorageTexture => vk::DescriptorType::STORAGE_IMAGE,
        DescriptorResourceType::StorageRenderTarget => vk::DescriptorType::STORAGE_IMAGE,
        DescriptorResourceType::AccelerationStructure => vk::DescriptorType::ACCELERATION_STRUCTURE_NV,
    }
}

pub fn create_descriptor_pool(
    device: &Device,
    pool_sizes: &Vec<vk::DescriptorPoolSize>,
    max_descriptor_sets_count: u32
) -> vk::DescriptorPool {
    if 0 < pool_sizes.len() {
        let pool_create_info = vk::DescriptorPoolCreateInfo {
            // Note: for manually free descriptorSets - vk::DescriptorPoolCreateFlags::FREE_DESCRIPTOR_SET
            flags: vk::DescriptorPoolCreateFlags::empty(),
            pool_size_count: pool_sizes.len() as u32,
            p_pool_sizes: pool_sizes.as_ptr(),
            max_sets: max_descriptor_sets_count,
            ..Default::default()
        };
        unsafe {
            let descriptor_pool = device.create_descriptor_pool(&pool_create_info, None).expect("vkCreateDescriptorPool failed!");
            log::trace!("    CreateDescriptorPool : {:?}", descriptor_pool);
            return descriptor_pool;
        }
    }
    vk::DescriptorPool::null()
}

pub fn destroy_descriptor_pool(device: &Device, descriptor_pool: vk::DescriptorPool) {
    log::trace!("    DestroyDescriptorPool : {:?}", descriptor_pool);
    unsafe {
        device.destroy_descriptor_pool(descriptor_pool, None);
    }
}

pub fn create_descriptor_set_layout(
    device: &Device,
    layout_bindings: &Vec<vk::DescriptorSetLayoutBinding>
) -> vk::DescriptorSetLayout {
    let layout_create_info = vk::DescriptorSetLayoutCreateInfo {
        binding_count: layout_bindings.len() as u32,
        p_bindings: layout_bindings.as_ptr(),
        ..Default::default()
    };
    unsafe {
        let descriptor_set_layout = device.create_descriptor_set_layout(&layout_create_info, None).expect("vkCreateDescriptorSetLayout failed!");
        log::trace!("    CreateDescriptorSetLayout: {:?}", descriptor_set_layout);
        descriptor_set_layout
    }
}

pub fn destroy_descriptor_set_layout(device: &Device, descriptor_set_layout: vk::DescriptorSetLayout) {
    log::trace!("    DestroyDescriptorSetLayout: {:?}", descriptor_set_layout);
    unsafe {
        device.destroy_descriptor_set_layout(descriptor_set_layout, None);
    }
}

pub fn create_descriptor_data(
    device: &Device,
    descriptor_data_create_infos: &Vec<DescriptorDataCreateInfo>,
    max_descriptor_sets_count: u32
) -> DescriptorData {
    log::trace!("create_descriptor_data");
    let descriptor_layout_bindings = descriptor_data_create_infos
        .iter()
        .map(|descriptor_data_create_info| {
            vk::DescriptorSetLayoutBinding {
                binding: descriptor_data_create_info._descriptor_binding_index,
                descriptor_type: convert_resource_type_to_descriptor_type(&descriptor_data_create_info._descriptor_resource_type),
                descriptor_count: 1,
                stage_flags: descriptor_data_create_info._descriptor_shader_stage,
                ..Default::default()
            }
        })
        .collect();
    let descriptor_pool_sizes = descriptor_data_create_infos
        .iter()
        .map(|descriptor_data_create_info| {
            vk::DescriptorPoolSize {
                ty: convert_resource_type_to_descriptor_type(&descriptor_data_create_info._descriptor_resource_type),
                descriptor_count: max_descriptor_sets_count,
            }
        })
        .collect();
    let descriptor_set_layout = create_descriptor_set_layout(device, &descriptor_layout_bindings);
    let descriptor_pool = create_descriptor_pool(device, &descriptor_pool_sizes, max_descriptor_sets_count);
    DescriptorData {
        _descriptor_data_create_infos: descriptor_data_create_infos.clone(),
        _descriptor_set_layout_bindings: descriptor_layout_bindings,
        _descriptor_pool_sizes: descriptor_pool_sizes,
        _descriptor_pool: descriptor_pool,
        _descriptor_set_layout: descriptor_set_layout,
        _max_descriptor_sets_count: max_descriptor_sets_count,
    }
}

pub fn destroy_descriptor_data(device: &Device, descriptor_data: &DescriptorData) {
    log::trace!("destroyDescriptorData");
    unsafe {
        device.destroy_descriptor_set_layout(descriptor_data._descriptor_set_layout, None);
        device.destroy_descriptor_pool(descriptor_data._descriptor_pool, None);
    }
}

pub fn create_descriptor_sets(
    device: &Device,
    descriptor_data: &DescriptorData
) -> SwapchainArray<vk::DescriptorSet> {
    if vk::DescriptorPool::null() != descriptor_data._descriptor_pool {
        let descriptor_set_layouts: [vk::DescriptorSetLayout; constants::SWAPCHAIN_IMAGE_COUNT] = [
            descriptor_data._descriptor_set_layout; constants::SWAPCHAIN_IMAGE_COUNT
        ];
        let allocation_info = vk::DescriptorSetAllocateInfo {
            descriptor_pool: descriptor_data._descriptor_pool,
            descriptor_set_count: descriptor_set_layouts.len() as u32,
            p_set_layouts: descriptor_set_layouts.as_ptr(),
            ..Default::default()
        };
        unsafe {
            let descriptor_sets = device.allocate_descriptor_sets(&allocation_info).expect("failed to allocate_descriptor_sets");
            log::trace!("    CreateDescriptorSet: {:?}", descriptor_sets);
            return descriptor_sets;
        }
    }
    Vec::new()
}

pub fn destroy_descriptor_sets(
    device: &Device,
    descriptor_pool: vk::DescriptorPool,
    descriptor_sets: &SwapchainArray<vk::DescriptorSet>,
) {
    log::trace!("    destroyDescriptorSet: {:?}", descriptor_sets);
    // need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
    unsafe {
        device.free_descriptor_sets(descriptor_pool, &descriptor_sets).expect("failed to free_descriptor_sets");
    }
}

pub fn create_write_descriptor_sets_with_update(
    device: &Device,
    descriptor_sets: &SwapchainArray<vk::DescriptorSet>,
    descriptor_bind_indices: &Vec<u32>,
    descriptor_set_layout_bindings: &Vec<vk::DescriptorSetLayoutBinding>,
    descriptor_resource_infos_list: &SwapchainArray<Vec<DescriptorResourceInfo>>,
) -> SwapchainArray<Vec<vk::WriteDescriptorSet>> {
    if descriptor_sets.is_empty() {
        return Vec::new();
    }

    constants::SWAPCHAIN_IMAGE_INDICES
        .iter()
        .map(|index| {
            let descriptor_set = descriptor_sets[*index as usize];
            let descriptor_resource_infos = &descriptor_resource_infos_list[*index as usize];
            let mut write_descriptor_sets = Vec::<vk::WriteDescriptorSet>::new();
            let count = descriptor_bind_indices.len();
            for index in 0..count {
                let mut write_descriptor_set = vk::WriteDescriptorSet::default();

                match &descriptor_resource_infos[index] {
                    DescriptorResourceInfo::DescriptorBufferInfo(buffer_info) => {
                        write_descriptor_set.p_buffer_info = buffer_info;
                        write_descriptor_set.descriptor_count = 1;
                    },
                    DescriptorResourceInfo::DescriptorImageInfo(image_info) => {
                        write_descriptor_set.p_image_info = image_info;
                        write_descriptor_set.descriptor_count = 1;
                    },
                    DescriptorResourceInfo::WriteDescriptorSetAccelerationStructure(write_descriptor_set_accel_struct) => {
                        write_descriptor_set_push_next(&mut write_descriptor_set, *write_descriptor_set_accel_struct);
                        write_descriptor_set.descriptor_count = 1;
                    },
                    DescriptorResourceInfo::InvalidDescriptorInfo => {
                        write_descriptor_set.descriptor_count = 0;
                    }
                }

                if 0 < write_descriptor_set.descriptor_count {
                    write_descriptor_set.dst_set = descriptor_set;
                    write_descriptor_set.dst_binding = descriptor_bind_indices[index];
                    write_descriptor_set.dst_array_element = 0;
                    write_descriptor_set.descriptor_type = descriptor_set_layout_bindings[index].descriptor_type;

                    write_descriptor_sets.push(write_descriptor_set);
                }
            }

            // Upload
            if false == write_descriptor_sets.is_empty() {
                unsafe {
                    device.update_descriptor_sets(&write_descriptor_sets, &[]);
                }
            }

            write_descriptor_sets
        }).collect()
}

pub fn create_write_descriptor_set(
    write_descriptor_sets: &Vec<vk::WriteDescriptorSet>,
    descriptor_index: usize,
    descriptor_resource_info: &DescriptorResourceInfo
) -> vk::WriteDescriptorSet {
    let mut write_descriptor_set = write_descriptor_sets[descriptor_index].clone();
    match descriptor_resource_info {
        DescriptorResourceInfo::DescriptorBufferInfo(buffer_info) => {
            write_descriptor_set.p_buffer_info = buffer_info;
            write_descriptor_set.descriptor_count = 1;
        },
        DescriptorResourceInfo::DescriptorImageInfo(image_info) => {
            write_descriptor_set.p_image_info = image_info;
            write_descriptor_set.descriptor_count = 1;
        },
        DescriptorResourceInfo::WriteDescriptorSetAccelerationStructure(write_descriptor_set_accel_struct) => {
            write_descriptor_set_push_next(&mut write_descriptor_set, *write_descriptor_set_accel_struct);
            write_descriptor_set.descriptor_count = 1;
        },
        DescriptorResourceInfo::InvalidDescriptorInfo => {
        }
    }
    write_descriptor_set
}

pub fn update_write_descriptor_set(
    write_descriptor_sets: &mut Vec<vk::WriteDescriptorSet>,
    descriptor_index: usize,
    descriptor_resource_info: &DescriptorResourceInfo
) {
    let mut write_descriptor_set = &mut write_descriptor_sets[descriptor_index];
    match descriptor_resource_info {
        DescriptorResourceInfo::DescriptorBufferInfo(buffer_info) => {
            write_descriptor_set.p_buffer_info = buffer_info;
            write_descriptor_set.p_image_info = std::ptr::null();
        },
        DescriptorResourceInfo::DescriptorImageInfo(image_info) => {
            write_descriptor_set.p_buffer_info = std::ptr::null();
            write_descriptor_set.p_image_info = image_info;
        },
        _ => {
            write_descriptor_set.p_buffer_info = std::ptr::null();
            write_descriptor_set.p_image_info = std::ptr::null();
        }
    }
}