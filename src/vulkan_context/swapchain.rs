use std::cmp::{ min, max };

use ash::{
    vk,
    Device,
    Instance,
};
use ash::extensions::khr::{
    Surface,
    Swapchain,
};

use crate::constants;
use crate::vulkan_context::queue;
use crate::vulkan_context::texture;
use crate::vulkan_context::vulkan_context::{ SwapchainIndexMap };

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
    pub _swapchain_images: SwapchainIndexMap<vk::Image>,
    pub _swapchain_image_views: SwapchainIndexMap<vk::ImageView>,
    pub _swapchain_extent: vk::Extent2D
}

pub unsafe fn choose_swapchain_surface_format(
    swap_chain_support_details: &SwapchainSupportDetails,
    require_format: vk::Format,
    require_color_space: vk::ColorSpaceKHR
) -> vk::SurfaceFormatKHR {
    for format in swap_chain_support_details._formats.iter() {
        if ((require_format == format.format) || (vk::Format::UNDEFINED == format.format)) && (require_color_space == format.color_space) {
            return format.clone();
        }
    }
    vk::SurfaceFormatKHR {
        format: require_format,
        color_space: require_color_space
    }
}

pub fn choose_swapchain_present_mode(swapchain_support_details: &SwapchainSupportDetails) -> vk::PresentModeKHR {
    if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::MAILBOX) {
        return vk::PresentModeKHR::MAILBOX;
    } else if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::FIFO) {
        return vk::PresentModeKHR::FIFO;
    } else if swapchain_support_details._present_modes.contains(&vk::PresentModeKHR::FIFO_RELAXED) {
        return vk::PresentModeKHR::FIFO_RELAXED;
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

pub unsafe fn is_valid_swapchain_support(swapchain_support_details: &SwapchainSupportDetails) -> bool {
    (false == swapchain_support_details._formats.is_empty()) && (false == swapchain_support_details._present_modes.is_empty())
}

pub unsafe fn query_swapchain_support(
    surface_loader: &Surface,
    physical_device: &vk::PhysicalDevice,
    surface: &vk::SurfaceKHR
) -> SwapchainSupportDetails {
    let capabilities: vk::SurfaceCapabilitiesKHR = surface_loader.get_physical_device_surface_capabilities(*physical_device, *surface).unwrap();
    let formats = surface_loader.get_physical_device_surface_formats(*physical_device, *surface).unwrap();
    let present_modes = surface_loader.get_physical_device_surface_present_modes(*physical_device, *surface).unwrap();
    SwapchainSupportDetails {
        _capabilities: capabilities,
        _formats: formats,
        _present_modes: present_modes
    }
}
// querySwapchainSupport :: VkPhysicalDevice -> V/kSurfaceKHR -> IO SwapchainSupportDetails
// querySwapchainSupport physicalDevice vkSurface = do
//   capabilities <- newVkData $ \pSurfaceCapabilities -> do
//     result <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice vkSurface pSurfaceCapabilities
//     validationVK result "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
//   formats <- asListVK $ \counterPtr valuePtr -> do
//     result <- vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice vkSurface counterPtr valuePtr
//     validationVK result "vkGetPhysicalDeviceSurfaceFormatsKHR error"
//   presentModes <- asListVK $ \counterPtr valuePtr -> do
//     result <- vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice vkSurface counterPtr valuePtr
//     validationVK result "vkGetPhysicalDeviceSurfacePresentModesKHR error"
//   return SwapchainSupportDetails { _capabilities = capabilities
//                                  , _formats = formats
//                                  , _presentModes = presentModes }
//

pub unsafe fn create_swapchain_data(
    device: &Device,
    swapchain_interface: &Swapchain,
    surface: vk::SurfaceKHR,
    swapchain_support_details: &SwapchainSupportDetails,
    queue_family_datas: &queue::QueueFamilyDatas,
    immediate_mode: bool
) -> SwapchainData
{
    let surface_format = choose_swapchain_surface_format(swapchain_support_details, constants::SWAPCHAIN_IMAGE_FORMAT, constants::SWAPCHAIN_COLOR_SPACE);
    let present_mode = if immediate_mode {
        vk::PresentModeKHR::IMMEDIATE
    } else {
        choose_swapchain_present_mode(swapchain_support_details)
    };
    let image_extent = choose_swapchain_extent(swapchain_support_details);
    let max_image_count = swapchain_support_details._capabilities.max_image_count;
    let min_image_count = swapchain_support_details._capabilities.min_image_count;
    let image_count = if max_image_count <= 0 {
        max(min_image_count, constants::SWAPCHAIN_IMAGE_COUNT)
    } else {
        min(max_image_count, max(min_image_count, constants::SWAPCHAIN_IMAGE_COUNT))
    };
    let pre_transform = if swapchain_support_details._capabilities.supported_transforms.contains(vk::SurfaceTransformFlagsKHR::IDENTITY) {
        vk::SurfaceTransformFlagsKHR::IDENTITY
    } else {
        swapchain_support_details._capabilities.current_transform
    };

    let mut swapchain_create_info = vk::SwapchainCreateInfoKHR::builder()
        .surface(surface)
        .min_image_count(image_count)
        .image_color_space(surface_format.color_space)
        .image_format(surface_format.format)
        .image_extent(image_extent)
        .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
        .image_array_layers(1)
        .pre_transform(pre_transform)
        .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
        .present_mode(present_mode)
        .clipped(true);
    if queue_family_datas._queue_family_indices._graphics_queue_index != queue_family_datas._queue_family_indices._present_queue_index {
        swapchain_create_info = swapchain_create_info
            .image_sharing_mode(vk::SharingMode::CONCURRENT)
            .queue_family_indices(&queue_family_datas._queue_family_index_list);
    } else {
        swapchain_create_info = swapchain_create_info.image_sharing_mode(vk::SharingMode::EXCLUSIVE);
    }
    let swapchain = swapchain_interface.create_swapchain(&swapchain_create_info, None).expect("vkCreateSwapchainKHR failed!");
    let swapchain_images: SwapchainIndexMap<vk::Image> = swapchain_interface.get_swapchain_images(swapchain).expect("vkGetSwapchainImagesKHR error!");
    let swapchain_image_views = create_swapchain_image_views(&device, &swapchain_images, swapchain_create_info.image_format);

    log::info!("Create Swapchain : {:?}", swapchain);
    log::info!("    presentMode : {:?}", present_mode);
    log::info!("    image_count : {} {:?}", image_count, swapchain_images);
    log::info!("    imageFormat : {:?}", surface_format.format);
    log::info!("    imageColorSpace : {:?}", surface_format.color_space);
    log::info!("    imageViews : {:?}", swapchain_image_views);
    log::info!("    image_extent : {:?}", image_extent);
    log::info!("    imageSharingMode : {:?}", swapchain_create_info.image_sharing_mode);

    SwapchainData {
        _swapchain: swapchain,
        _swapchain_images: swapchain_images,
        _swapchain_image_format: surface_format.format,
        _swapchain_image_views: swapchain_image_views,
        _swapchain_extent: image_extent
    }
}

pub unsafe fn destroy_swapchain_data(device: &Device, swapchain_interface: &Swapchain, swapchain_data: &SwapchainData) {
    destroy_swapchain_image_views(device, &swapchain_data._swapchain_image_views);
    log::info!("Destroy Swapchain");
    swapchain_interface.destroy_swapchain(swapchain_data._swapchain, None);
}

pub unsafe fn create_swapchain_image_views(
    device: &Device,
    swapChain_images: &SwapchainIndexMap<vk::Image>,
    image_format: vk::Format
) -> SwapchainIndexMap<vk::ImageView> {
    swapChain_images
        .iter()
        .map(|image| {
            texture::create_image_view(device, *image, vk::ImageViewType::TYPE_2D, image_format, vk::ImageAspectFlags::COLOR, 1, 1)
        })
        .collect()
}

pub unsafe fn destroy_swapchain_image_views(device: &Device, swapchain_image_views: &SwapchainIndexMap<vk::ImageView>) {
    for image_view in swapchain_image_views.iter() {
        texture::destroy_image_view(device, *image_view);
    }
}