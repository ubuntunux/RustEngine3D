use std::cmp::{ min, max, Ordering };
use std::os::raw::c_char;
use std::ffi::{
    CStr,
    CString,
};
use std::vec::Vec;

use ash::{
    vk,
    Device,
    Entry,
    Instance,
};
use ash::extensions::ext::DebugUtils;
use ash::extensions::khr::{
    Surface,
    Swapchain,
};
use ash::version::{
    DeviceV1_0,
    EntryV1_0,
    InstanceV1_0,
};
use ash::util::*;
use winit::window::{
    Window,
    WindowBuilder
};

use crate::constants;
use crate::vulkan_context::swapchain;
use crate::vulkan_context::vulkan_context;


pub fn get_extension_names(extension_type: &str, available_extensions: &Vec<vk::ExtensionProperties>) -> Vec<CString> {
    log::info!("Available {} extentions: {}", extension_type, available_extensions.len());
    let mut extension_names: Vec<CString> = Vec::new();
    for available_extension in available_extensions {
        unsafe {
            let extension_name = CString::from(CStr::from_ptr(available_extension.extension_name.as_ptr() as *const c_char));
            //log::info!("    {}", extension_name.to_str().unwrap());
            extension_names.push(extension_name);
        }
    }
    extension_names
}

pub fn get_instance_extension_supports(entry: &Entry) -> Vec<CString> {
    let available_instance_extensions: Vec<vk::ExtensionProperties> = entry.enumerate_instance_extension_properties()
        .expect("vkEnumerateInstanceExtensionProperties error");
    get_extension_names(&"Instance", &available_instance_extensions)
}

pub fn get_device_extension_supports(instance: &Instance, physical_device: vk::PhysicalDevice) -> Vec<CString> {
    unsafe {
        let available_device_extensions: Vec<vk::ExtensionProperties> = instance.enumerate_device_extension_properties(physical_device)
            .expect("vkEnumerateInstanceExtensionProperties error");
        get_extension_names(&"Device", &available_device_extensions)
    }
}

pub unsafe fn check_extension_support(
    extension_type: &str,
    available_extensions: &Vec<CString>,
    require_extensions: &Vec<CString>
) -> bool {
    log::info!("Require {} Extensions: {} / {} availables.", extension_type, require_extensions.len(), require_extensions.len());
    let mut result: bool = true;
    for require_extension in require_extensions {
        let mut found: bool = false;
        for available_extension in available_extensions {
            if require_extension == available_extension {
                found = true;
                log::info!("    {} (OK)", require_extension.to_str().unwrap());
                break;
            }
        }
        if false == found {
            result = false;
            log::info!("    {} (Failed)", require_extension.to_str().unwrap());
        }
    }
    result
}

pub unsafe fn get_max_usable_sample_count(device_properties: &vk::PhysicalDeviceProperties) -> vk::SampleCountFlags {
    let sample_count_limit = min(device_properties.limits.framebuffer_color_sample_counts, device_properties.limits.framebuffer_depth_sample_counts);
    let sample_count = *[
        vk::SampleCountFlags::TYPE_64,
        vk::SampleCountFlags::TYPE_32,
        vk::SampleCountFlags::TYPE_16,
        vk::SampleCountFlags::TYPE_8,
        vk::SampleCountFlags::TYPE_4,
        vk::SampleCountFlags::TYPE_2,
        vk::SampleCountFlags::TYPE_1,
    ].iter().filter(|&&x| sample_count_limit.contains(x)).next().unwrap();
    log::info!("MSAA Samples: {:?}", sample_count);
    sample_count
}

pub unsafe fn create_vk_instance(
    entry: &Entry,
    app_name: &str,
    app_version: u32,
    surface_extensions: &Vec<&'static CStr>
) -> Instance {
    let app_name = CString::new(app_name).unwrap();
    let layer_names: Vec<CString> = constants::VULKAN_LAYERS
        .iter()
        .map(|layer_name| CString::new(*layer_name).unwrap())
        .collect();
    let layers_names_raw: Vec<*const i8> = layer_names
        .iter()
        .map(|raw_name| raw_name.as_ptr())
        .collect();
    let mut extension_names_raw = surface_extensions
        .iter()
        .map(|ext| ext.as_ptr())
        .collect::<Vec<_>>();
    extension_names_raw.push(DebugUtils::name().as_ptr());

    let require_extension_names = surface_extensions
        .iter()
        .map(|ext| CString::from(*ext) )
        .collect();
    let available_instance_extensions: Vec<CString> = get_instance_extension_supports(entry);
    check_extension_support(&"Instance", &available_instance_extensions, &require_extension_names);

    let appinfo = vk::ApplicationInfo::builder()
        .application_name(&app_name)
        .application_version(app_version)
        .engine_name(&app_name)
        .engine_version(constants::ENGINE_VERSION)
        .api_version(constants::API_VERSION)
        .build();

    let create_info = vk::InstanceCreateInfo::builder()
        .application_info(&appinfo)
        .enabled_layer_names(&layers_names_raw)
        .enabled_extension_names(&extension_names_raw)
        .build();

    log::info!(
        "Create Vulkan Instance: {:?}, api version: {}.{}.{}",
        app_name,
        vk::version_major(constants::API_VERSION),
        vk::version_minor(constants::API_VERSION),
        vk::version_patch(constants::API_VERSION)
    );

    entry.create_instance(&create_info, None).expect("Instance creation error")
}

pub unsafe fn destroy_vk_instance(instance: &Instance) {
    log::info!("Destroy Vulkan Instance");
    instance.destroy_instance(None);
}

pub unsafe fn create_vk_surface(entry: &Entry, instance: &Instance, window: &Window) -> vk::SurfaceKHR {
    log::info!("Create VkSurfaceKHR");
    ash_window::create_surface(entry, instance, window, None).unwrap()
}

pub unsafe fn destroy_vk_surface(surface_interface: &Surface, surface: vk::SurfaceKHR) {
    log::info!("Destroy VkSurfaceKHR");
    surface_interface.destroy_surface(surface, None);
}

pub unsafe fn is_device_suitable(
    instance: &Instance,
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    physical_device: vk::PhysicalDevice
) -> (bool, swapchain::SwapchainSupportDetails, vk::PhysicalDeviceFeatures) {
    let available_device_extensions = get_device_extension_supports(instance, physical_device);
    let device_extension_names: Vec<CString> = constants::REQUIRE_DEVICE_EXTENSIONS.iter().map(|str| CString::new(*str).unwrap() ).collect();
    let has_extension: bool = check_extension_support(&"Device", &available_device_extensions, &device_extension_names);
    let physical_device_features = instance.get_physical_device_features(physical_device);
    let swapchain_support_details = swapchain::query_swapchain_support(surface_interface, physical_device, surface);
    let result = swapchain::is_valid_swapchain_support(&swapchain_support_details);
    (has_extension && result, swapchain_support_details, physical_device_features)
}

pub unsafe fn select_physical_device(
    instance: &Instance,
    surface_interface: &Surface,
    surface: vk::SurfaceKHR
) -> Option<(vk::PhysicalDevice, swapchain::SwapchainSupportDetails, vk::PhysicalDeviceFeatures)> {
    let physical_devices = instance.enumerate_physical_devices().expect("Physical device error");
    log::info!("Found {} devices", physical_devices.len());
    for physical_device in physical_devices {
        let (result, swapchain_support_details, mut physical_device_features) = is_device_suitable(instance, surface_interface, surface, physical_device);
        if result {
            // set enable clip distance
            physical_device_features.shader_clip_distance = 1;
            return Some((physical_device, swapchain_support_details, physical_device_features));
        }
    }
    None
}

pub unsafe fn create_device(
    instance: &Instance,
    physical_device: vk::PhysicalDevice,
    render_features: &vulkan_context::RenderFeatures,
    queue_family_index_set: &Vec<u32>
) -> Device {
    let queue_priorities = [1.0];
    let queue_create_infos: Vec<vk::DeviceQueueCreateInfo> = queue_family_index_set
        .iter()
        .map(|queue_family_index| {
            vk::DeviceQueueCreateInfo::builder()
                .queue_family_index(*queue_family_index)
                .queue_priorities(&queue_priorities)
                .build()
        })
        .collect();
    let layer_names: Vec<CString> = constants::VULKAN_LAYERS.iter().map(|layer_name| { CString::new(*layer_name).unwrap() }).collect();
    let layer_names_raw: Vec<*const c_char> = layer_names.iter().map(|layer_name| { layer_name.as_ptr() }).collect();
    let device_extension_names: Vec<CString> = constants::REQUIRE_DEVICE_EXTENSIONS.iter().map(|extension| { CString::new(*extension).unwrap() }).collect();
    let device_extension_names_raw: Vec<*const c_char> = device_extension_names.iter().map(|extension| { extension.as_ptr() }).collect();
    let device_create_info = vk::DeviceCreateInfo::builder()
        .queue_create_infos(&queue_create_infos)
        .enabled_layer_names(&layer_names_raw)
        .enabled_extension_names(&device_extension_names_raw)
        .enabled_features(&render_features._physical_device_features)
        .build();
    let device: Device = instance.create_device(physical_device, &device_create_info, None).unwrap();
    log::info!("Created Device: {:?}, {:?}", constants::VULKAN_LAYERS, constants::REQUIRE_DEVICE_EXTENSIONS);
    device
}

pub unsafe fn destroy_device(device: &Device) {
    log::info!("Destroy VkDevice");
    device.destroy_device(None);
}