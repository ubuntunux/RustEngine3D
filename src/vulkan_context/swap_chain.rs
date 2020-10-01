use ash;
use ash::{
    vk,
};
use ash::extensions::khr::{
    Surface,
    Swapchain,
};

use crate::constants;

pub type SwapChainIndexMap<T> = [T; constants::SWAP_CHAIN_IMAGE_COUNT as usize];

#[derive(Debug)]
pub struct SwapChainSupportDetails {
    _capabilities: vk::SurfaceCapabilitiesKHR,
    _formats: Vec<vk::SurfaceFormatKHR>,
    _present_modes: Vec<vk::PresentModeKHR>
}

#[derive(Debug)]
pub struct SwapChainData {
    _swap_chain: vk::SwapchainKHR,
    _swap_chain_image_format: vk::Format,
    _swap_chain_images: SwapChainIndexMap<vk::Image>,
    _swap_chain_image_views: SwapChainIndexMap<vk::ImageView>,
    _swap_chain_extent: vk::Extent2D
}


// chooseSwapSurfaceFormat :: SwapChainSupportDetails -> VkFormat -> IO VkSurfaceFormatKHR
// chooseSwapSurfaceFormat swapChainSupportDetails requireFormat =
//     findAvailableFormat (_formats swapChainSupportDetails)
//     where
//         formats = _formats swapChainSupportDetails
//         getFormat = getField @"format"
//         getColorSpace = getField @"colorSpace"
//         findAvailableFormat :: [VkSurfaceFormatKHR] -> IO VkSurfaceFormatKHR
//         findAvailableFormat [] = newVkData $ \surfaceFormatPtr -> do
//             writeField @"format" surfaceFormatPtr requireFormat
//             writeField @"colorSpace" surfaceFormatPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
//         findAvailableFormat (x:xs) =
//             if (requireFormat == getFormat x || VK_FORMAT_UNDEFINED == getFormat x) && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x
//             then return x
//             else findAvailableFormat xs
//
// chooseSwapPresentMode :: SwapChainSupportDetails -> IO VkPresentModeKHR
// chooseSwapPresentMode swapChainSupportDetails
//     | VK_PRESENT_MODE_MAILBOX_KHR `elem` presentModes = return VK_PRESENT_MODE_MAILBOX_KHR
//     | VK_PRESENT_MODE_FIFO_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_KHR
//     | VK_PRESENT_MODE_FIFO_RELAXED_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_RELAXED_KHR
//     | VK_PRESENT_MODE_IMMEDIATE_KHR `elem` presentModes = return VK_PRESENT_MODE_IMMEDIATE_KHR
//     | otherwise = return VK_PRESENT_MODE_FIFO_KHR
//     where
//         presentModes = _presentModes swapChainSupportDetails
//
// chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
// chooseSwapExtent swapChainSupportDetails = do
//   imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
//     writeField @"width" extentPtr $ max (width $ getField @"minImageExtent" capabilities)
//                              $ min (width $ getField @"maxImageExtent" capabilities)
//                                    (width $ getField @"currentExtent"  capabilities)
//     writeField @"height" extentPtr $ max (height $ getField @"minImageExtent" capabilities)
//                               $ min (height $ getField @"maxImageExtent" capabilities)
//                                     (height $ getField @"currentExtent"  capabilities)
//   return imageExtent
//   where
//     capabilities = _capabilities swapChainSupportDetails
//     width = getField @"width"
//     height = getField @"height"
//
//

pub unsafe fn is_valid_swapchain_support(swapchain_support_details: &SwapChainSupportDetails) -> bool {
    (false == swapchain_support_details._formats.is_empty()) && (false == swapchain_support_details._present_modes.is_empty())
}

pub unsafe fn query_swapchain_support(surface_loader: &Surface, physical_device: &vk::PhysicalDevice, surface: &vk::SurfaceKHR)
    -> SwapChainSupportDetails
{
    let capabilities: vk::SurfaceCapabilitiesKHR = surface_loader.get_physical_device_surface_capabilities(*physical_device, *surface).unwrap();
    let formats = surface_loader.get_physical_device_surface_formats(*physical_device, *surface).unwrap();
    let present_modes = surface_loader.get_physical_device_surface_present_modes(*physical_device, *surface).unwrap();
    SwapChainSupportDetails {
        _capabilities: capabilities,
        _formats: formats,
        _present_modes: present_modes
    }
}
// querySwapChainSupport :: VkPhysicalDevice -> VkSurfaceKHR -> IO SwapChainSupportDetails
// querySwapChainSupport physicalDevice vkSurface = do
//   capabilities <- newVkData $ \pSurfaceCapabilities -> do
//     result <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice vkSurface pSurfaceCapabilities
//     validationVK result "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
//   formats <- asListVK $ \counterPtr valuePtr -> do
//     result <- vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice vkSurface counterPtr valuePtr
//     validationVK result "vkGetPhysicalDeviceSurfaceFormatsKHR error"
//   presentModes <- asListVK $ \counterPtr valuePtr -> do
//     result <- vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice vkSurface counterPtr valuePtr
//     validationVK result "vkGetPhysicalDeviceSurfacePresentModesKHR error"
//   return SwapChainSupportDetails { _capabilities = capabilities
//                                  , _formats = formats
//                                  , _presentModes = presentModes }
//
// createSwapChainData :: VkDevice
//                     -> SwapChainSupportDetails
//                     -> QueueFamilyDatas
//                     -> VkSurfaceKHR
//                     -> Bool
//                     -> IO SwapChainData
// createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface immediateMode = do
//   surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails Constants.swapChainImageFormat
//   presentMode <- if not immediateMode then chooseSwapPresentMode swapChainSupportDetails else return VK_PRESENT_MODE_IMMEDIATE_KHR
//   imageExtent <- chooseSwapExtent swapChainSupportDetails
//   queueFamilyIndicesPtr <- newArray (_queueFamilyIndexList queueFamilyDatas)
//
//   let maxImageCount = getField @"maxImageCount" $ _capabilities swapChainSupportDetails
//       minImageCount = getField @"minImageCount" $ _capabilities swapChainSupportDetails
//       imageCount' = if maxImageCount <= 0
//                    then max minImageCount (fromIntegral Constants.swapChainImageCount)
//                    else min maxImageCount $ max minImageCount (fromIntegral Constants.swapChainImageCount)
//
//   -- write VkSwapchainCreateInfoKHR
//   swapChainCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swapChainCreateInfoPtr -> do
//     writeField @"sType" swapChainCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
//     writeField @"pNext" swapChainCreateInfoPtr VK_NULL_HANDLE
//     writeField @"flags" swapChainCreateInfoPtr VK_ZERO_FLAGS
//     writeField @"surface" swapChainCreateInfoPtr vkSurface
//     writeField @"minImageCount" swapChainCreateInfoPtr imageCount'
//     writeField @"imageFormat" swapChainCreateInfoPtr (getField @"format" surfaceFormat)
//     writeField @"imageColorSpace" swapChainCreateInfoPtr (getField @"colorSpace" surfaceFormat)
//     writeField @"imageExtent" swapChainCreateInfoPtr imageExtent
//     writeField @"imageArrayLayers" swapChainCreateInfoPtr 1
//     writeField @"imageUsage" swapChainCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
//     if (_graphicsQueue queueFamilyDatas) /= (_presentQueue queueFamilyDatas)
//     then do
//       writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_CONCURRENT
//       writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr (_queueFamilyCount queueFamilyDatas)
//       writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr queueFamilyIndicesPtr
//     else do
//       writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
//       writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr 0
//       writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr VK_NULL_HANDLE
//     writeField @"preTransform" swapChainCreateInfoPtr (getField @"currentTransform" $ _capabilities swapChainSupportDetails)
//     writeField @"compositeAlpha" swapChainCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
//     writeField @"presentMode" swapChainCreateInfoPtr presentMode
//     writeField @"clipped" swapChainCreateInfoPtr VK_TRUE
//     writeField @"oldSwapchain" swapChainCreateInfoPtr VK_NULL_HANDLE
//
//   swapChain <- alloca $ \swapChainPtr -> do
//       result <- vkCreateSwapchainKHR device (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
//       validationVK result "vkCreateSwapchainKHR failed!"
//       peek swapChainPtr
//
//   swapChainImageList <- asListVK $ \counterPtr valueArrayPtr -> do
//       result <- vkGetSwapchainImagesKHR device swapChain counterPtr valueArrayPtr
//       validationVK result "vkGetSwapchainImagesKHR error"
//   let swapChainImages = swapChainIndexMapFromList swapChainImageList
//       swapChainImageFormat = getField @"imageFormat" swapChainCreateInfo
//       swapChainExtent = getField @"imageExtent" swapChainCreateInfo
//
//   touchVkData swapChainCreateInfo
//   free queueFamilyIndicesPtr
//
//   swapChainImageViews <- createSwapChainImageViews device swapChainImages swapChainImageFormat
//
//   logInfo $ "Create SwapChain : " ++ (show swapChain)
//   logInfo $ "    presentMode : " ++ show presentMode
//   logInfo $ "    imageCount : " ++ (show imageCount') ++ " " ++ (show swapChainImages)
//   logInfo $ "    imageFormat : " ++ (show $ getField @"imageFormat" swapChainCreateInfo)
//   logInfo $ "    imageColorSpace : " ++ (show $ getField @"imageColorSpace" swapChainCreateInfo)
//   logInfo $ "    imageViews : " ++ (show swapChainImageViews)
//   logInfo $ "    imageExtent : " ++ (show $ getField @"imageExtent" swapChainCreateInfo)
//   logInfo $ "    imageSharingMode : " ++ (show $ getField @"imageSharingMode" swapChainCreateInfo)
//
//   let swapChainData = SwapChainData { _swap_chain = swapChain
//                                     , _swap_chain_images = swapChainImages
//                                     , _swap_chain_image_format = swapChainImageFormat
//                                     , _swap_chain_image_views = swapChainImageViews
//                                     , _swapChainExtent = swapChainExtent }
//   return swapChainData
//
// destroySwapChainData :: VkDevice -> SwapChainData -> IO ()
// destroySwapChainData device swapChainData = do
//   destroySwapChainImageViews device (_swap_chain_image_views swapChainData)
//   logInfo "Destroy SwapChain"
//   vkDestroySwapchainKHR device (_swap_chain swapChainData) VK_NULL_HANDLE
//
// createSwapChainImageViews :: VkDevice -> SwapChainIndexMap VkImage -> VkFormat -> IO (SwapChainIndexMap VkImageView)
// createSwapChainImageViews device swapChainImages swapChainImageFormat = do
//     applyIOSwapChainIndex createImageView' swapChainImages
//     where
//         createImageView' swapChainImage = createImageView device swapChainImage VK_IMAGE_VIEW_TYPE_2D swapChainImageFormat VK_IMAGE_ASPECT_COLOR_BIT 1 1
//
// destroySwapChainImageViews :: VkDevice -> SwapChainIndexMap VkImageView -> IO (SwapChainIndexMap ())
// destroySwapChainImageViews device imageViews = applyIOSwapChainIndex (destroyImageView device) imageViews