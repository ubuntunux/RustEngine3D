use std::collections::HashMap;

use ash::{
    vk,
    Device,
    Instance,
};
use ash::extensions::khr::{
    Surface,
};

use crate::constants;

#[derive(Debug, Clone)]
pub struct QueueFamilyIndices {
    pub _graphics_queue_index: u32,
    pub _present_queue_index: u32,
    pub _compute_queue_index: u32,
    pub _transfer_queue_index: u32,
    pub _sparse_binding_queue_index: u32
}

#[derive(Debug, Clone)]
pub struct QueueFamilyDatas {
    pub _graphics_queue: vk::Queue,
    pub _present_queue: vk::Queue,
    pub _queue_family_index_list: Vec<u32>,
    pub _queue_family_count: u32,
    pub _queue_family_indices: QueueFamilyIndices
}

fn select_queue_family(
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    physical_device: vk::PhysicalDevice,
    queue_family_properties: &Vec<vk::QueueFamilyProperties>,
    queue_flags: vk::QueueFlags
) -> Vec<u32> {
    unsafe {
        queue_family_properties
            .iter()
            .enumerate()
            .filter_map(|(index, ref queue_family_property)| {
                let has_specify_queue = queue_family_property.queue_flags.contains(queue_flags);
                let surface_support = surface_interface.get_physical_device_surface_support(
                    physical_device,
                    index as u32,
                    surface,
                ).expect("vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support.");
                if has_specify_queue && surface_support {
                    Some(index as u32)
                } else {
                    None
                }
            })
            .collect()
    }
}

fn select_presentation_queue_family(
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    physical_device: vk::PhysicalDevice,
    queue_family_properties: &Vec<vk::QueueFamilyProperties>
) -> Vec<u32> {
    unsafe {
        queue_family_properties
            .iter()
            .enumerate()
            .filter_map(|(index, ref __queue_family_property)| {
                let surface_support = surface_interface.get_physical_device_surface_support(
                    physical_device,
                    index as u32,
                    surface,
                ).expect("vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support.");
                if surface_support {
                    Some(index as u32)
                } else {
                    None
                }
            })
            .collect()
    }
}

pub fn get_queue_families(
    instance: &Instance,
    physical_device: vk::PhysicalDevice
) -> Vec<vk::QueueFamilyProperties> {
    unsafe {
        let queue_family_properties: Vec<vk::QueueFamilyProperties> = instance.get_physical_device_queue_family_properties(physical_device);
        let family_count = queue_family_properties.len();
        assert!(0 < family_count, "Zero queue family count!");
        log::info!("Found {} queue families.", family_count);
        queue_family_properties
    }
}

pub fn get_queue_family_indices(
    instance: &Instance,
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    physical_device: vk::PhysicalDevice,
    is_concurrent_mode: bool
) -> QueueFamilyIndices {
    let queue_faimilies = get_queue_families(&instance, physical_device);
    let presentation_queue_family_indices = select_presentation_queue_family(surface_interface, surface, physical_device, &queue_faimilies);
    let graphics_queue_family_indices = select_queue_family(surface_interface, surface, physical_device, &queue_faimilies, vk::QueueFlags::GRAPHICS);
    let compute_queue_family_indices = select_queue_family(surface_interface, surface, physical_device, &queue_faimilies, vk::QueueFlags::COMPUTE);
    let transfer_queue_family_indices = select_queue_family(surface_interface, surface, physical_device, &queue_faimilies, vk::QueueFlags::TRANSFER);
    let sparse_binding_queue_family_indices = select_queue_family(surface_interface, surface, physical_device, &queue_faimilies, vk::QueueFlags::SPARSE_BINDING);
    let default_index = graphics_queue_family_indices[0];
    let fn_get_queue_family_index = |indices: &Vec<u32>| -> u32 {
        if false == indices.is_empty() {
            if is_concurrent_mode && indices.contains(&default_index) {
                return default_index;
            } else if false {
                return *indices
                    .iter()
                    .filter(|&&x| x != default_index)
                    .next()
                    .unwrap();
            } else {
                return default_index;
            }
        }
        constants::INVALID_QUEUE_INDEX
    };

    let queue_family_indices = QueueFamilyIndices {
        _graphics_queue_index: default_index,
        _present_queue_index: fn_get_queue_family_index(&presentation_queue_family_indices),
        _compute_queue_index: fn_get_queue_family_index(&compute_queue_family_indices),
        _transfer_queue_index: fn_get_queue_family_index(&transfer_queue_family_indices),
        _sparse_binding_queue_index: fn_get_queue_family_index(&sparse_binding_queue_family_indices)
    };

    log::info!("Graphics Queue Index : {}", queue_family_indices._graphics_queue_index);
    log::info!("Presentation Queue Index : {} / {:?}", queue_family_indices._present_queue_index, presentation_queue_family_indices);
    log::info!("Computer Queue Index : {} / {:?}", queue_family_indices._compute_queue_index, compute_queue_family_indices);
    log::info!("Transfer Queue Index : {} / {:?}", queue_family_indices._transfer_queue_index, transfer_queue_family_indices);
    log::info!("Sparse Binding Queue Index : {} / {:?}", queue_family_indices._sparse_binding_queue_index, sparse_binding_queue_family_indices);

    queue_family_indices
}

pub fn create_queues(device: &Device, queue_family_index_set: &Vec<u32>) -> HashMap<u32, vk::Queue> {
    unsafe {
        let mut queue_map = HashMap::new();
        for queue_family_index in queue_family_index_set.iter() {
            let queue = device.get_device_queue(*queue_family_index, 0);
            queue_map.insert(*queue_family_index, queue);
        }
        log::info!("Created Queues: {:?}", queue_map);
        queue_map
    }
}
