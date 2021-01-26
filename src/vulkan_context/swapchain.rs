use std::cmp::{ min, max };

use ash::{
    vk,
    Device,
};
use ash::extensions::khr::{
    Surface,
    Swapchain,
};

use crate::constants;
use crate::vulkan_context::queue;
use crate::vulkan_context::texture;
use crate::vulkan_context::vulkan_context::{ SwapchainArray };

#[derive(Debug)]
pub struct SwapchainSupportDetails {
    pub _capabilities: vk::SurfaceCapabilitiesKHR,
    pub _formats: Vec<vk::SurfaceFormatKHR>,
    pub _present_modes: Vec<vk::PresentModeKHR>
}

#[derive(Debug)]
pub struct SwapchainData {
    pub _swapchain: vk::SwapchainKHR,
    pub _swapchain_image_format: vk::Format,
    pub _swapchain_images: SwapchainArray<vk::Image>,
    pub _swapchain_image_views: SwapchainArray<vk::ImageView>,
    pub _swapchain_extent: vk::Extent2D
}

impl SwapchainData {
    pub fn get_swapchain_image_view(&self, swapchain_index: usize) -> vk::ImageView {
        self._swapchain_image_views[swapchain_index]
    }
}

pub fn choose_swapchain_surface_format(
    swap_chain_support_details: &SwapchainSupportDetails,
    require_surface_formats: &[vk::SurfaceFormatKHR],
) -> vk::SurfaceFormatKHR {
    for require_surface_format in require_surface_formats.iter() {
        for format in swap_chain_support_details._formats.iter() {
            if format.format == require_surface_format.format && format.color_space == require_surface_format.color_space {
                return require_surface_format.clone();
            }
        }
    }
    let mut surface_format = require_surface_formats[0].clone();
    if 0 < swap_chain_support_details._formats.len() && vk::Format::UNDEFINED != swap_chain_support_details._formats[0].format {
        surface_format = swap_chain_support_details._formats[0].clone();
    }
    surface_format
}

pub fn choose_swapchain_present_mode(swapchain_support_details: &SwapchainSupportDetails) -> vk::PresentModeKHR {
    if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::FIFO) {
        return vk::PresentModeKHR::FIFO;
    } else if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::FIFO_RELAXED) {
        return vk::PresentModeKHR::FIFO_RELAXED;
    } else if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::MAILBOX) {
        return vk::PresentModeKHR::MAILBOX;
    } else if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::IMMEDIATE) {
        return vk::PresentModeKHR::IMMEDIATE;
    }
    vk::PresentModeKHR::FIFO
}

pub fn choose_swapchain_extent(swapchain_support_details: &SwapchainSupportDetails) -> vk::Extent2D {
    let capabilities: &vk::SurfaceCapabilitiesKHR = &swapchain_support_details._capabilities;
    vk::Extent2D {
        width: max(capabilities.min_image_extent.width, min(capabilities.max_image_extent.width, capabilities.current_extent.width)),
        height: max(capabilities.min_image_extent.height, min(capabilities.max_image_extent.height, capabilities.current_extent.height))
    }
}

pub fn is_valid_swapchain_support(swapchain_support_details: &SwapchainSupportDetails) -> bool {
    (false == swapchain_support_details._formats.is_empty()) && (false == swapchain_support_details._present_modes.is_empty())
}

pub fn query_swapchain_support(
    surface_loader: &Surface,
    physical_device: vk::PhysicalDevice,
    surface: vk::SurfaceKHR
) -> SwapchainSupportDetails {
    unsafe {
        let capabilities: vk::SurfaceCapabilitiesKHR = surface_loader.get_physical_device_surface_capabilities(physical_device, surface).unwrap();
        let formats = surface_loader.get_physical_device_surface_formats(physical_device, surface).unwrap();
        let present_modes = surface_loader.get_physical_device_surface_present_modes(physical_device, surface).unwrap();
        SwapchainSupportDetails {
            _capabilities: capabilities,
            _formats: formats,
            _present_modes: present_modes
        }
    }
}

pub fn create_swapchain_data(
    device: &Device,
    swapchain_interface: &Swapchain,
    surface: vk::SurfaceKHR,
    swapchain_support_details: &SwapchainSupportDetails,
    queue_family_datas: &queue::QueueFamilyDatas,
    immediate_mode: bool
) -> SwapchainData
{
    let surface_format = choose_swapchain_surface_format(swapchain_support_details, &constants::SWAPCHAIN_SURFACE_FORMATS);
    #[cfg(target_os = "android")]
        let present_mode = if immediate_mode {
        vk::PresentModeKHR::IMMEDIATE
    } else {
        vk::PresentModeKHR::FIFO
    };
    #[cfg(not(target_os = "android"))]
    let present_mode = if immediate_mode {
        vk::PresentModeKHR::IMMEDIATE
    } else {
        choose_swapchain_present_mode(swapchain_support_details)
    };
    let image_extent = choose_swapchain_extent(swapchain_support_details);
    let max_image_count = swapchain_support_details._capabilities.max_image_count;
    let min_image_count = swapchain_support_details._capabilities.min_image_count;
    let image_count = if max_image_count <= 0 {
        max(min_image_count, constants::SWAPCHAIN_IMAGE_COUNT as u32)
    } else {
        min(max_image_count, max(min_image_count, constants::SWAPCHAIN_IMAGE_COUNT as u32))
    };
    let pre_transform = if swapchain_support_details._capabilities.supported_transforms.contains(vk::SurfaceTransformFlagsKHR::IDENTITY) {
        vk::SurfaceTransformFlagsKHR::IDENTITY
    } else {
        swapchain_support_details._capabilities.current_transform
    };

    let mut swapchain_create_info = vk::SwapchainCreateInfoKHR {
        surface,
        min_image_count: image_count,
        image_color_space: surface_format.color_space,
        image_format: surface_format.format,
        image_extent,
        image_usage: vk::ImageUsageFlags::COLOR_ATTACHMENT,
        image_array_layers: 1,
        pre_transform,
        composite_alpha: vk::CompositeAlphaFlagsKHR::OPAQUE,
        present_mode,
        clipped: 1,
        ..Default::default()
    };
    if queue_family_datas._queue_family_indices._graphics_queue_index != queue_family_datas._queue_family_indices._present_queue_index {
        swapchain_create_info.image_sharing_mode = vk::SharingMode::CONCURRENT;
        swapchain_create_info.queue_family_index_count = queue_family_datas._queue_family_index_list.len() as u32;
        swapchain_create_info.p_queue_family_indices = (&queue_family_datas._queue_family_index_list).as_ptr();
    } else {
        swapchain_create_info.image_sharing_mode = vk::SharingMode::EXCLUSIVE;
    }

    unsafe {
        let swapchain = swapchain_interface.create_swapchain(&swapchain_create_info, None).expect("vkCreateSwapchainKHR failed!");
        let swapchain_images: SwapchainArray<vk::Image> = swapchain_interface.get_swapchain_images(swapchain).expect("vkGetSwapchainImagesKHR error!");
        let swapchain_image_views = create_swapchain_image_views(&device, &swapchain_images, swapchain_create_info.image_format);

        log::info!("create_swapchain_data : {:?}", swapchain);
        log::info!("    present_mode : {:?}", present_mode);
        log::info!("    image_count : {} {:?}", image_count, swapchain_images);
        log::info!("    image_format : {:?}", surface_format.format);
        log::info!("    color_space : {:?}", surface_format.color_space);
        log::info!("    image_views : {:?}", swapchain_image_views);
        log::info!("    image_extent : {:?}", image_extent);
        log::info!("    image_sharing_mode : {:?}", swapchain_create_info.image_sharing_mode);

        SwapchainData {
            _swapchain: swapchain,
            _swapchain_images: swapchain_images,
            _swapchain_image_format: surface_format.format,
            _swapchain_image_views: swapchain_image_views,
            _swapchain_extent: image_extent
        }
    }
}

pub fn destroy_swapchain_data(device: &Device, swapchain_interface: &Swapchain, swapchain_data: &SwapchainData) {
    destroy_swapchain_image_views(device, &swapchain_data._swapchain_image_views);
    log::info!("destroy_swapchain_data");
    unsafe {
        swapchain_interface.destroy_swapchain(swapchain_data._swapchain, None);
    }
}

pub fn create_swapchain_image_views(
    device: &Device,
    swapchain_images: &SwapchainArray<vk::Image>,
    image_format: vk::Format,
) -> SwapchainArray<vk::ImageView> {
    swapchain_images
        .iter()
        .map(|image| {
            texture::create_image_view(device, *image, vk::ImageViewType::TYPE_2D, image_format, vk::ImageAspectFlags::COLOR, 0, 1, 0, 1)
        })
        .collect()
}

pub fn destroy_swapchain_image_views(device: &Device, swapchain_image_views: &SwapchainArray<vk::ImageView>) {
    for image_view in swapchain_image_views.iter() {
        texture::destroy_image_view(device, *image_view);
    }
}