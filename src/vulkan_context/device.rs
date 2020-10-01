use std::cmp::Ordering;
use std::os::raw::c_char;
use std::ffi::{
    CStr,
    CString,
};
use std::vec::Vec;

use ash;
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
use crate::vulkan_context::swap_chain::*;
use crate::vulkan_context::vulkan_context::*;

// getExtensionNames :: (Traversable t1, VulkanMarshal t) => [Char] -> t1 t -> IO (t1 String)
// getExtensionNames extensionType availableExtensionArrayPtr = do
//   availableExtensionNames <- mapM getExtensionName availableExtensionArrayPtr
//   logInfo $ "Available " ++ extensionType ++ " extensions : " ++ (show (length availableExtensionNames))
//   --mapM (\extensionName -> logInfo $ "    " ++ extensionName) availableExtensionNames
//   return availableExtensionNames
//   where
//     getExtensionName extensionPtr =
//       let extensionNamePtr = plusPtr (unsafePtr extensionPtr) (fieldOffset @"extensionName" @VkExtensionProperties)
//       in peekCString $ castPtr extensionNamePtr
//
// getInstanceExtensionSupport :: IO [String]
// getInstanceExtensionSupport = do
//     availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
//         result <- vkEnumerateInstanceExtensionProperties VK_NULL_HANDLE counterPtr valueArrayPtr
//         validationVK result "vkEnumerateInstanceExtensionProperties error"
//     getExtensionNames "Instance" availableExtensionArrayPtr
//
// getDeviceExtensionSupport :: VkPhysicalDevice -> IO [String]
// getDeviceExtensionSupport physicalDevice = do
//     availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
//         result <- vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL_HANDLE counterPtr valueArrayPtr
//         validationVK result "vkEnumerateInstanceExtensionProperties error"
//     getExtensionNames "Device" availableExtensionArrayPtr
//

//

//
// getMaxUsableSampleCount :: VkPhysicalDeviceProperties -> IO VkSampleCountFlagBits
// getMaxUsableSampleCount deviceProperties = do
//     let limits = getField @"limits" deviceProperties
//         colorSampleCounts = getField @"framebufferColorSampleCounts" limits
//         depthSampleCounts = getField @"framebufferDepthSampleCounts" limits
//         counts = min colorSampleCounts depthSampleCounts
//         splitCounts = filter ((/= VK_ZERO_FLAGS) . (counts .&.))
//             [ VK_SAMPLE_COUNT_64_BIT
//             , VK_SAMPLE_COUNT_32_BIT
//             , VK_SAMPLE_COUNT_16_BIT
//             , VK_SAMPLE_COUNT_8_BIT
//             , VK_SAMPLE_COUNT_4_BIT
//             , VK_SAMPLE_COUNT_2_BIT
//             , VK_SAMPLE_COUNT_1_BIT ]
//         highestCount = head $ splitCounts >>= maskToBits
//     logInfo $ "MSAA Samples: " ++ show highestCount
//     return highestCount
//
//

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

    let appinfo = vk::ApplicationInfo::builder()
        .application_name(&app_name)
        .application_version(app_version)
        .engine_name(&app_name)
        .engine_version(constants::ENGINE_VERSION)
        .api_version(constants::API_VERSION);

    let create_info = vk::InstanceCreateInfo::builder()
        .application_info(&appinfo)
        .enabled_layer_names(&layers_names_raw)
        .enabled_extension_names(&extension_names_raw);

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

pub unsafe fn destroy_vk_surface(surface_loader: &Surface, surface: &vk::SurfaceKHR) {
    log::info!("Destroy VkSurfaceKHR");
    surface_loader.destroy_surface(*surface, None);
}

//
// getPhysicalDeviceProperties :: VkPhysicalDevice -> IO VkPhysicalDeviceProperties
// getPhysicalDeviceProperties physicalDevice = do
//     deviceProperties <- alloca $ \propertiesPtr -> do
//         vkGetPhysicalDeviceProperties physicalDevice propertiesPtr
//         peek propertiesPtr
//     return deviceProperties
//


pub unsafe fn check_extension_support(
    available_device_extensions: &Vec<vk::ExtensionProperties>,
    require_extensions: &Vec<&CStr>
) -> bool {
    for available_device_extension in available_device_extensions {
        for require_extension in require_extensions {
            let available_device_extension_name = CStr::from_ptr(available_device_extension.extension_name.as_ptr() as *const c_char);
            if Some(Ordering::Equal) == require_extension.partial_cmp(&available_device_extension_name) {
                return true;
            }
        }
    }
    false
}

/*
isDeviceSuitable :: Maybe VkSurfaceKHR
                 -> VkPhysicalDevice
                 -> IO (Bool, Maybe SwapChainSupportDetails, VkPhysicalDeviceFeatures)
isDeviceSuitable maybeVkSurface physicalDevice = do
    deviceExtensionNames <- getDeviceExtensionSupport physicalDevice
    hasExtension <- checkExtensionSupport deviceExtensionNames Constants.requireDeviceExtensions
    supportedFeatures <- allocaPeek $ vkGetPhysicalDeviceFeatures physicalDevice
    (maybeSwapChainSupportDetails, result) <- case maybeVkSurface of
        Nothing -> pure (Nothing, False)
        Just vkSurface -> do
            swapChainSupportDetails <- querySwapChainSupport physicalDevice vkSurface
            let result = isValidSwapChainSupport swapChainSupportDetails
            return (Just swapChainSupportDetails, result)
    pure (hasExtension && result, maybeSwapChainSupportDetails, supportedFeatures)
    */

pub unsafe fn is_device_suitable(instance: &Instance, surface_loader: &Surface, surface: &vk::SurfaceKHR, physical_device: &vk::PhysicalDevice)
    -> (bool, SwapChainSupportDetails, vk::PhysicalDeviceFeatures)
{
    let available_device_extensions: Vec<vk::ExtensionProperties> = instance.enumerate_device_extension_properties(*physical_device).unwrap();
    let device_extension_names = vec![Swapchain::name()];
    let has_extension: bool = check_extension_support(&available_device_extensions, &device_extension_names);
    let physical_device_features = instance.get_physical_device_features(*physical_device);
    let swapchain_support_details = query_swapchain_support(surface_loader, physical_device, surface);
    let result = is_valid_swapchain_support(&swapchain_support_details);
    (has_extension && result, swapchain_support_details, physical_device_features)
}

pub unsafe fn select_physical_device(instance: &Instance, surface_loader: &Surface, surface: &vk::SurfaceKHR)
    -> Option<(vk::PhysicalDevice, SwapChainSupportDetails, vk::PhysicalDeviceFeatures)>
{
    let physical_devices = instance.enumerate_physical_devices().expect("Physical device error");
    log::info!("Found {} devices", physical_devices.len());
    for physical_device in physical_devices {
        let (result, swapchain_support_details, physical_device_features) = is_device_suitable(instance, surface_loader, surface, &physical_device);
        if result {
            return Some((physical_device, swapchain_support_details, physical_device_features));
        }
    }
    None
}
//
//
// createDevice :: VkPhysicalDevice -> [Word32] -> IO VkDevice
// createDevice physicalDevice queueFamilyList = do
//     queuePrioritiesPtr <- new 1.0
//     queueCreateInfoList <- forM queueFamilyList $ \queueFamilyIndex ->
//         newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
//             writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
//             writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
//             writeField @"flags" queueCreateInfoPtr VK_ZERO_FLAGS
//             writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
//             writeField @"queueCount" queueCreateInfoPtr 1
//             writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
//     physicalDeviceFeatures <- newVkData @VkPhysicalDeviceFeatures $
//         \physicalDeviceFeaturesPtr -> do
//             clearStorable physicalDeviceFeaturesPtr
//             writeField @"samplerAnisotropy" physicalDeviceFeaturesPtr VK_TRUE
//     queueCreateInfoArrayPtr <- newArray queueCreateInfoList
//     requireDeviceExtensionsPtr <- newArray Constants.requireDeviceExtensions
//     deviceCreateInfo <- withCStringList Constants.vulkanLayers $ \layerCount layerNames -> do
//         newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
//             writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
//             writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
//             writeField @"flags" devCreateInfoPtr VK_ZERO_FLAGS
//             writeField @"pQueueCreateInfos" devCreateInfoPtr queueCreateInfoArrayPtr
//             writeField @"queueCreateInfoCount" devCreateInfoPtr (fromIntegral $ length queueCreateInfoList)
//             writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
//             writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
//             writeField @"enabledExtensionCount" devCreateInfoPtr (fromIntegral $ length Constants.requireDeviceExtensions)
//             writeField @"ppEnabledExtensionNames" devCreateInfoPtr requireDeviceExtensionsPtr
//             writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
//     device <- alloca $ \devicePtr -> do
//         result <- vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
//         validationVK result "vkCreateDevice: failed to create vkDevice"
//         peek devicePtr
//     logInfo $ "Created Device: " ++ show device
//     touchVkData deviceCreateInfo
//     touchVkData physicalDeviceFeatures
//     free requireDeviceExtensionsPtr
//     free queueCreateInfoArrayPtr
//     free queuePrioritiesPtr
//     return device
//
// destroyDevice :: VkDevice -> IO ()
// destroyDevice device = do
//     logInfo "Destroy VkDevice"
//     vkDestroyDevice device VK_NULL_HANDLE
