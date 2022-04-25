use std::cmp::{ min };
use std::os::raw::c_char;
use std::ffi::{ CStr, CString, c_void };
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
};

use winit::window::{
    Window
};

use crate::constants;
use crate::vulkan_context::swapchain;
use crate::vulkan_context::vulkan_context;
use crate::renderer::utility::ptr_chain_iter;


pub fn device_create_info_set_push_next<T: vk::ExtendsDeviceCreateInfo>(create_info: &mut vk::DeviceCreateInfo, next_ptr: *const T) {
    unsafe {
        let next_mut = &mut (*(next_ptr as *mut T));
        let last_next = ptr_chain_iter(next_mut).last().unwrap();
        (*last_next).p_next = create_info.p_next as _;
        create_info.p_next = next_ptr as *const c_void;
    }
}

pub fn get_extension_names(extension_type: &str, available_extensions: &Vec<vk::ExtensionProperties>) -> Vec<CString> {
    log::info!("Available {} extentions: {}", extension_type, available_extensions.len());
    let mut extension_names: Vec<CString> = Vec::new();
    for available_extension in available_extensions {
        unsafe {
            let extension_name = CString::from(CStr::from_ptr(available_extension.extension_name.as_ptr() as *const c_char));
            extension_names.push(extension_name);
        }
    }
    extension_names
}

pub fn get_instance_extension_supports(entry: &Entry) -> Vec<CString> {
    let available_instance_extensions: Vec<vk::ExtensionProperties> = entry.enumerate_instance_extension_properties(None)
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

pub fn check_extension_support(
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

pub unsafe fn get_instance_layers(entry: &Entry, required_instance_layers: &Vec<String>) -> Vec<CString> {
    let available_layers: Vec<vk::LayerProperties> = entry.enumerate_instance_layer_properties().unwrap();
    let mut available_layer_names: Vec<CString> = Vec::new();
    log::info!("available layers:");
    for layer in available_layers.iter() {
        log::info!("    {:?}", layer);
        available_layer_names.push(CString::from(CStr::from_ptr(layer.layer_name.as_ptr())));
    }

    log::info!("required layers:");
    let required_layer_names: Vec<CString> = required_instance_layers.iter().map(|layer_name| CString::new(layer_name.as_str()).unwrap()).collect();
    let mut required_instance_layers: Vec<CString> = Vec::new();
    for required_layer in required_layer_names.iter() {
        let mut found: bool = false;
        for available_layer in available_layer_names.iter() {
            if *available_layer == *required_layer {
                required_instance_layers.push(required_layer.clone());
                found = true;
                break;
            }
        }

        if found {
            log::info!("    layer: {:?} (OK)", required_layer);
        } else {
            log::error!("    layer: {:?} (not found)", required_layer);
        }
    }
    required_instance_layers
}

pub fn get_max_usable_sample_count(device_properties: &vk::PhysicalDeviceProperties) -> vk::SampleCountFlags {
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

pub fn create_vk_instance(
    entry: &Entry,
    app_name: &str,
    app_version: u32,
    surface_extensions: &[*const c_char],
    instance_layer_names_raw: &Vec<*const c_char>,
) -> Instance {
    let app_name = CString::new(app_name).unwrap();

    let mut extension_names_raw = surface_extensions.to_vec();
    if unsafe { vk::DebugUtilsMessageSeverityFlagsEXT::empty() != constants::DEBUG_MESSAGE_LEVEL } {
        extension_names_raw.push(DebugUtils::name().as_ptr());
    }

    let require_extension_names = unsafe { surface_extensions
        .iter()
        .map(|ext| CString::from(CStr::from_ptr(*ext)))
        .collect()
    };
    let available_instance_extensions: Vec<CString> = get_instance_extension_supports(entry);
    check_extension_support(&"Instance", &available_instance_extensions, &require_extension_names);

    let appinfo = vk::ApplicationInfo {
        p_application_name: app_name.as_ptr(),
        application_version: app_version,
        p_engine_name: app_name.as_ptr(),
        engine_version: constants::ENGINE_VERSION,
        api_version: unsafe { constants::VULKAN_API_VERSION },
        ..Default::default()
    };

    let create_info = vk::InstanceCreateInfo {
        p_application_info: &appinfo,
        enabled_layer_count: instance_layer_names_raw.len() as u32,
        pp_enabled_layer_names: instance_layer_names_raw.as_ptr(),
        enabled_extension_count: extension_names_raw.len() as u32,
        pp_enabled_extension_names: extension_names_raw.as_ptr(),
        ..Default::default()
    };

    log::info!("create_instance");
    log::info!("    app name: {:?}", app_name);
    log::info!("    engine version: {}.{}.{}", vk::api_version_major(constants::ENGINE_VERSION), vk::api_version_minor(constants::ENGINE_VERSION), vk::api_version_patch(constants::ENGINE_VERSION));
    unsafe {
        log::info!("    require vulkan api version: {}.{}.{}", vk::api_version_major(constants::VULKAN_API_VERSION), vk::api_version_minor(constants::VULKAN_API_VERSION), vk::api_version_patch(constants::VULKAN_API_VERSION));
    }
    log::info!("    instance_layer_names: {:?}", instance_layer_names_raw);
    log::info!("    surface_extensions: {:?}", surface_extensions);
    unsafe {
        entry.create_instance(&create_info, None).expect("Instance creation error")
    }
}

pub fn destroy_vk_instance(instance: &Instance) {
    log::info!("destroy_vk_instance");
    unsafe {
        instance.destroy_instance(None);
    }
}

pub fn create_vk_surface(entry: &Entry, instance: &Instance, window: &Window) -> vk::SurfaceKHR {
    log::info!("create_vk_surface");
    unsafe {
        ash_window::create_surface(entry, instance, window, None).unwrap()
    }
}

pub fn destroy_vk_surface(surface_interface: &Surface, surface: vk::SurfaceKHR) {
    log::info!("destroy_vk_surface");
    unsafe {
        surface_interface.destroy_surface(surface, None);
    }
}

pub fn is_device_suitable(
    instance: &Instance,
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    physical_device: vk::PhysicalDevice,
    device_extension_names: &Vec<CString>,
    ray_tracing_extension_names: &Vec<CString>,
) -> (bool, swapchain::SwapchainSupportDetails, vk::PhysicalDeviceFeatures, bool) {
    unsafe {
        let available_device_extensions = get_device_extension_supports(instance, physical_device);
        let has_extension: bool = check_extension_support(&"Device", &available_device_extensions, device_extension_names);
        let enable_raytracing: bool = false == ray_tracing_extension_names.is_empty() && check_extension_support(&"Ray Tracing", &available_device_extensions, ray_tracing_extension_names);
        let physical_device_features = instance.get_physical_device_features(physical_device);
        let swapchain_support_details = swapchain::query_swapchain_support(surface_interface, physical_device, surface);
        let result = swapchain::is_valid_swapchain_support(&swapchain_support_details);
        (has_extension && result, swapchain_support_details, physical_device_features, enable_raytracing)
    }
}

pub fn select_physical_device(
    instance: &Instance,
    surface_interface: &Surface,
    surface: vk::SurfaceKHR,
    device_extension_names: &Vec<CString>,
    ray_tracing_extension_names: &Vec<CString>,
) -> Option<(vk::PhysicalDevice, swapchain::SwapchainSupportDetails, vk::PhysicalDeviceFeatures, bool)> {
    unsafe {
        let physical_devices = instance.enumerate_physical_devices().expect("Physical device error");
        log::info!("Found {} physical devices.", physical_devices.len());

        for physical_device in physical_devices {
            let (result, swapchain_support_details, physical_device_features, enable_ray_tracing) = is_device_suitable(
                instance,
                surface_interface,
                surface,
                physical_device,
                device_extension_names,
                ray_tracing_extension_names
            );

            if result {
                log::info!("Select physical devices: [{:?}]", physical_device);
                return Some((physical_device, swapchain_support_details, physical_device_features, enable_ray_tracing));
            }
        }
    }
    None
}

pub fn create_device(
    instance: &Instance,
    physical_device: vk::PhysicalDevice,
    render_features: &vulkan_context::RenderFeatures,
    queue_family_index_set: &Vec<u32>,
    device_extension_names_raw: &Vec<*const c_char>,
    instance_layer_names_raw: &Vec<*const c_char>
) -> Device {
    let queue_priorities = [1.0];
    let queue_create_infos: Vec<vk::DeviceQueueCreateInfo> = queue_family_index_set
        .iter()
        .map(|queue_family_index| {
            vk::DeviceQueueCreateInfo {
                queue_family_index: *queue_family_index,
                queue_count: queue_priorities.len() as u32,
                p_queue_priorities: queue_priorities.as_ptr(),
                ..Default::default()
            }
        })
        .collect();

    #[cfg(target_os = "android")]
    let device_features = vk::PhysicalDeviceFeatures {
        sampler_anisotropy: 0,
        shader_clip_distance: 0,
        ..render_features._physical_device_features
    };
    #[cfg(not(target_os = "android"))]
    let device_features = render_features._physical_device_features.clone();

    let mut device_create_info = vk::DeviceCreateInfo {
        queue_create_info_count: queue_create_infos.len() as u32,
        p_queue_create_infos: queue_create_infos.as_ptr(),
        enabled_layer_count: instance_layer_names_raw.len() as u32,
        pp_enabled_layer_names: instance_layer_names_raw.as_ptr(),
        enabled_extension_count: device_extension_names_raw.len() as u32,
        pp_enabled_extension_names: device_extension_names_raw.as_ptr(),
        p_enabled_features: &device_features,
        ..Default::default()
    };

    // physical device features - must keep lifetime until create_device completes
    let accel_feature = vk::PhysicalDeviceAccelerationStructureFeaturesKHR {
        s_type: vk::StructureType::PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR,
        ..Default::default()
    };
    let ray_tracing_pipeline_feature = vk::PhysicalDeviceRayTracingPipelineFeaturesKHR {
        s_type: vk::StructureType::PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR,
        ..Default::default()
    };
    let clock_feature = vk::PhysicalDeviceShaderClockFeaturesKHR {
        s_type: vk::StructureType::PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR,
        ..Default::default()
    };

    if render_features._use_ray_tracing {
        device_create_info_set_push_next(&mut device_create_info, &accel_feature);
        device_create_info_set_push_next(&mut device_create_info, &ray_tracing_pipeline_feature);
        device_create_info_set_push_next(&mut device_create_info, &clock_feature);
    }

    unsafe {
        let device: Device = instance.create_device(physical_device, &device_create_info, None).unwrap();
        log::info!("create device: {:?}", device.handle());
        device
    }
}

pub fn destroy_device(device: &Device) {
    log::info!("destroy_device");
    unsafe {
        device.destroy_device(None);
    }
}