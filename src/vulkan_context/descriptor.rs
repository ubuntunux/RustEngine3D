use ash::{
    vk,
    Device,
};
use ash::version::DeviceV1_0;

use crate::constants;
use crate::vulkan_context::vulkan_context::SwapchainIndexMap;

#[derive(Debug, Clone)]
pub enum DescriptorResourceInfo {
    DescriptorBufferInfo(vk::DescriptorBufferInfo),
    DescriptorImageInfo(vk::DescriptorImageInfo),
    InvalidDescriptorInfo
}

#[derive(Debug, Clone)]
pub enum DescriptorResourceType {
    UniformBuffer,
    Texture,
    RenderTarget,
}

#[derive(Debug, Clone)]
pub struct DescriptorDataCreateInfo {
    _descriptor_binding_index: u32,
    _descriptor_name: String,
    _descriptor_resource_type: DescriptorResourceType,
    _descriptor_type: vk::DescriptorType,
    _descriptor_shader_stage: vk::ShaderStageFlags,
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

pub fn create_descriptor_pool(
    device: &Device,
    pool_sizes: &Vec<vk::DescriptorPoolSize>,
    max_descriptor_sets_count: u32
) -> vk::DescriptorPool {
    let pool_create_info = vk::DescriptorPoolCreateInfo::builder()
        // Note: for manually free descriptorSets - vk::DescriptorPoolCreateFlags::FREE_DESCRIPTOR_SET
        .flags(vk::DescriptorPoolCreateFlags::empty())
        .pool_sizes(pool_sizes)
        .max_sets(max_descriptor_sets_count)
        .build();
    unsafe {
        let descriptor_pool = device.create_descriptor_pool(&pool_create_info, None).expect("vkCreateDescriptorPool failed!");
        log::info!("    CreateDescriptorPool : {:?}", descriptor_pool);
        descriptor_pool
    }
}

pub fn destroy_descriptor_pool(device: &Device, descriptor_pool: vk::DescriptorPool) {
    log::info!("    DestroyDescriptorPool : {:?}", descriptor_pool);
    unsafe {
        device.destroy_descriptor_pool(descriptor_pool, None);
    }
}

pub fn create_descriptor_set_layout(
    device: &Device,
    layout_bindings: &Vec<vk::DescriptorSetLayoutBinding>
) -> vk::DescriptorSetLayout {
    let layout_create_info = vk::DescriptorSetLayoutCreateInfo::builder()
        .bindings(&layout_bindings)
        .build();
    unsafe {
        let descriptor_set_layout = device.create_descriptor_set_layout(&layout_create_info, None).expect("vkCreateDescriptorSetLayout failed!");
        log::info!("    CreateDescriptorSetLayout: {:?}", descriptor_set_layout);
        descriptor_set_layout
    }
}

pub fn destroy_descriptor_set_layout(device: &Device, descriptor_set_layout: vk::DescriptorSetLayout) {
    log::info!("    DestroyDescriptorSetLayout: {:?}", descriptor_set_layout);
    unsafe {
        device.destroy_descriptor_set_layout(descriptor_set_layout, None);
    }
}

pub fn create_descriptor_data(
    device: &Device,
    descriptor_data_create_infos: &Vec<DescriptorDataCreateInfo>,
    max_descriptor_sets_count: u32
) -> DescriptorData {
    log::info!("createDescriptorData");
    let descriptor_layout_bindings = descriptor_data_create_infos
        .iter()
        .map(|descriptor_data_create_info| {
            vk::DescriptorSetLayoutBinding::builder()
                .binding(descriptor_data_create_info._descriptor_binding_index)
                .descriptor_type(descriptor_data_create_info._descriptor_type)
                .descriptor_count(1)
                .stage_flags(descriptor_data_create_info._descriptor_shader_stage)
                .build()
        })
        .collect();
    let descriptor_pool_sizes = descriptor_data_create_infos
        .iter()
        .map(|descriptor_data_create_info| {
            vk::DescriptorPoolSize::builder()
                .ty(descriptor_data_create_info._descriptor_type)
                .descriptor_count(max_descriptor_sets_count)
                .build()
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
    log::info!("destroyDescriptorData");
    unsafe {
        device.destroy_descriptor_set_layout(descriptor_data._descriptor_set_layout, None);
        device.destroy_descriptor_pool(descriptor_data._descriptor_pool, None);
    }
}

pub fn create_descriptor_sets(
    device: &Device,
    descriptor_data: &DescriptorData
) -> SwapchainIndexMap<vk::DescriptorSet> {
    let descriptor_set_layouts: [vk::DescriptorSetLayout; constants::SWAPCHAIN_IMAGE_COUNT as usize] = [
        descriptor_data._descriptor_set_layout; constants::SWAPCHAIN_IMAGE_COUNT as usize
    ];
    let allocation_info = vk::DescriptorSetAllocateInfo::builder()
        .descriptor_pool(descriptor_data._descriptor_pool)
        .set_layouts(&descriptor_set_layouts)
        .build();
    unsafe {
        let descriptor_sets = device.allocate_descriptor_sets(&allocation_info).expect("");
        log::info!("    CreateDescriptorSet: {:?}", descriptor_sets);
        descriptor_sets
    }
}

pub fn destroy_descriptor_sets(
    device: &Device,
    descriptor_pool: vk::DescriptorPool,
    descriptor_sets: &SwapchainIndexMap<vk::DescriptorSet>,
) {
    log::info!("    destroyDescriptorSet: {:?}", descriptor_sets);
    // need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
    unsafe {
        device.free_descriptor_sets(descriptor_pool, &descriptor_sets);
    }
}

pub fn create_write_descriptor_sets(
    descriptor_set: vk::DescriptorSet,
    descriptor_bind_indices: &Vec<u32>,
    descriptor_set_layout_bindings: &Vec<vk::DescriptorSetLayoutBinding>,
    descriptor_resource_infos: &Vec<DescriptorResourceInfo>,
) -> Vec<vk::WriteDescriptorSet> {
    let mut write_descriptor_sets = Vec::<vk::WriteDescriptorSet>::new();
    let count = descriptor_bind_indices.len();
    for index in 0..count {
        let mut write_descriptor_set = vk::WriteDescriptorSet {
            dst_set: descriptor_set,
            dst_binding: descriptor_bind_indices[index],
            dst_array_element: 0,
            descriptor_type: descriptor_set_layout_bindings[index].descriptor_type,
            descriptor_count: 1,
            ..Default::default()
        };

        match &descriptor_resource_infos[index] {
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

        write_descriptor_sets.push(write_descriptor_set);
    }
    write_descriptor_sets
}

pub fn update_write_descriptor_set(
    write_descriptor_sets: &mut Vec<vk::WriteDescriptorSet>,
    descriptor_offset: usize,
    descriptor_resource_info: &DescriptorResourceInfo
) {
    let mut write_descriptor_set = &mut write_descriptor_sets[descriptor_offset];
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