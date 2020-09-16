{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.SwapChain
  ( SwapChainSupportDetails (..)
  , SwapChainData (..)
  , isValidSwapChainSupport
  , querySwapChainSupport
  , createSwapChainData
  , destroySwapChainData
  ) where


import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Queue
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.Vulkan


data SwapChainSupportDetails = SwapChainSupportDetails
    { _capabilities :: VkSurfaceCapabilitiesKHR
    , _formats      :: [VkSurfaceFormatKHR]
    , _presentModes :: [VkPresentModeKHR]
    } deriving (Eq, Show)

data SwapChainData = SwapChainData
    { _swapChain :: VkSwapchainKHR
    , _swapChainImageFormat :: VkFormat
    , _swapChainImages :: SwapChainIndexMap VkImage
    , _swapChainImageViews :: SwapChainIndexMap VkImageView
    , _swapChainExtent :: VkExtent2D
    } deriving (Eq, Show)


chooseSwapSurfaceFormat :: SwapChainSupportDetails -> VkFormat -> IO VkSurfaceFormatKHR
chooseSwapSurfaceFormat swapChainSupportDetails requireFormat =
    findAvailableFormat (_formats swapChainSupportDetails)
    where
        formats = _formats swapChainSupportDetails
        getFormat = getField @"format"
        getColorSpace = getField @"colorSpace"
        findAvailableFormat :: [VkSurfaceFormatKHR] -> IO VkSurfaceFormatKHR
        findAvailableFormat [] = newVkData $ \surfaceFormatPtr -> do
            writeField @"format" surfaceFormatPtr requireFormat
            writeField @"colorSpace" surfaceFormatPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
        findAvailableFormat (x:xs) =
            if (requireFormat == getFormat x || VK_FORMAT_UNDEFINED == getFormat x) && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x
            then return x
            else findAvailableFormat xs

chooseSwapPresentMode :: SwapChainSupportDetails -> IO VkPresentModeKHR
chooseSwapPresentMode swapChainSupportDetails
    | VK_PRESENT_MODE_MAILBOX_KHR `elem` presentModes = return VK_PRESENT_MODE_MAILBOX_KHR
    | VK_PRESENT_MODE_FIFO_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_KHR
    | VK_PRESENT_MODE_FIFO_RELAXED_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_RELAXED_KHR
    | VK_PRESENT_MODE_IMMEDIATE_KHR `elem` presentModes = return VK_PRESENT_MODE_IMMEDIATE_KHR
    | otherwise = return VK_PRESENT_MODE_FIFO_KHR
    where
        presentModes = _presentModes swapChainSupportDetails

chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
chooseSwapExtent swapChainSupportDetails = do
  imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
    writeField @"width" extentPtr $ max (width $ getField @"minImageExtent" capabilities)
                             $ min (width $ getField @"maxImageExtent" capabilities)
                                   (width $ getField @"currentExtent"  capabilities)
    writeField @"height" extentPtr $ max (height $ getField @"minImageExtent" capabilities)
                              $ min (height $ getField @"maxImageExtent" capabilities)
                                    (height $ getField @"currentExtent"  capabilities)
  return imageExtent
  where
    capabilities = _capabilities swapChainSupportDetails
    width = getField @"width"
    height = getField @"height"


isValidSwapChainSupport :: SwapChainSupportDetails -> Bool
isValidSwapChainSupport swapChainSupportDetails =
    not (null (_formats swapChainSupportDetails)) && not (null (_presentModes swapChainSupportDetails))

querySwapChainSupport :: VkPhysicalDevice -> VkSurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport physicalDevice vkSurface = do
  capabilities <- newVkData $ \pSurfaceCapabilities -> do
    result <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice vkSurface pSurfaceCapabilities
    validationVK result "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
  formats <- asListVK $ \counterPtr valuePtr -> do
    result <- vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice vkSurface counterPtr valuePtr
    validationVK result "vkGetPhysicalDeviceSurfaceFormatsKHR error"
  presentModes <- asListVK $ \counterPtr valuePtr -> do
    result <- vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice vkSurface counterPtr valuePtr
    validationVK result "vkGetPhysicalDeviceSurfacePresentModesKHR error"
  return SwapChainSupportDetails { _capabilities = capabilities
                                 , _formats = formats
                                 , _presentModes = presentModes }

createSwapChainData :: VkDevice
                    -> SwapChainSupportDetails
                    -> QueueFamilyDatas
                    -> VkSurfaceKHR
                    -> Bool
                    -> IO SwapChainData
createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface immediateMode = do
  surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails Constants.swapChainImageFormat
  presentMode <- if not immediateMode then chooseSwapPresentMode swapChainSupportDetails else return VK_PRESENT_MODE_IMMEDIATE_KHR
  imageExtent <- chooseSwapExtent swapChainSupportDetails
  queueFamilyIndicesPtr <- newArray (_queueFamilyIndexList queueFamilyDatas)

  let maxImageCount = getField @"maxImageCount" $ _capabilities swapChainSupportDetails
      minImageCount = getField @"minImageCount" $ _capabilities swapChainSupportDetails
      imageCount' = if maxImageCount <= 0
                   then max minImageCount (fromIntegral Constants.swapChainImageCount)
                   else min maxImageCount $ max minImageCount (fromIntegral Constants.swapChainImageCount)

  -- write VkSwapchainCreateInfoKHR
  swapChainCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swapChainCreateInfoPtr -> do
    writeField @"sType" swapChainCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags" swapChainCreateInfoPtr VK_ZERO_FLAGS
    writeField @"surface" swapChainCreateInfoPtr vkSurface
    writeField @"minImageCount" swapChainCreateInfoPtr imageCount'
    writeField @"imageFormat" swapChainCreateInfoPtr (getField @"format" surfaceFormat)
    writeField @"imageColorSpace" swapChainCreateInfoPtr (getField @"colorSpace" surfaceFormat)
    writeField @"imageExtent" swapChainCreateInfoPtr imageExtent
    writeField @"imageArrayLayers" swapChainCreateInfoPtr 1
    writeField @"imageUsage" swapChainCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if (_graphicsQueue queueFamilyDatas) /= (_presentQueue queueFamilyDatas)
    then do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr (_queueFamilyCount queueFamilyDatas)
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr queueFamilyIndicesPtr
    else do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr 0
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"preTransform" swapChainCreateInfoPtr (getField @"currentTransform" $ _capabilities swapChainSupportDetails)
    writeField @"compositeAlpha" swapChainCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode" swapChainCreateInfoPtr presentMode
    writeField @"clipped" swapChainCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain" swapChainCreateInfoPtr VK_NULL_HANDLE

  swapChain <- alloca $ \swapChainPtr -> do
      result <- vkCreateSwapchainKHR device (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
      validationVK result "vkCreateSwapchainKHR failed!"
      peek swapChainPtr

  swapChainImageList <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkGetSwapchainImagesKHR device swapChain counterPtr valueArrayPtr
      validationVK result "vkGetSwapchainImagesKHR error"
  let swapChainImages = swapChainIndexMapFromList swapChainImageList
      swapChainImageFormat = getField @"imageFormat" swapChainCreateInfo
      swapChainExtent = getField @"imageExtent" swapChainCreateInfo

  touchVkData swapChainCreateInfo
  free queueFamilyIndicesPtr

  swapChainImageViews <- createSwapChainImageViews device swapChainImages swapChainImageFormat

  logInfo $ "Create SwapChain : " ++ (show swapChain)
  logInfo $ "    presentMode : " ++ show presentMode
  logInfo $ "    imageCount : " ++ (show imageCount') ++ " " ++ (show swapChainImages)
  logInfo $ "    imageFormat : " ++ (show $ getField @"imageFormat" swapChainCreateInfo)
  logInfo $ "    imageColorSpace : " ++ (show $ getField @"imageColorSpace" swapChainCreateInfo)
  logInfo $ "    imageViews : " ++ (show swapChainImageViews)
  logInfo $ "    imageExtent : " ++ (show $ getField @"imageExtent" swapChainCreateInfo)
  logInfo $ "    imageSharingMode : " ++ (show $ getField @"imageSharingMode" swapChainCreateInfo)

  let swapChainData = SwapChainData { _swapChain = swapChain
                                    , _swapChainImages = swapChainImages
                                    , _swapChainImageFormat = swapChainImageFormat
                                    , _swapChainImageViews = swapChainImageViews
                                    , _swapChainExtent = swapChainExtent }
  return swapChainData

destroySwapChainData :: VkDevice -> SwapChainData -> IO ()
destroySwapChainData device swapChainData = do
  destroySwapChainImageViews device (_swapChainImageViews swapChainData)
  logInfo "Destroy SwapChain"
  vkDestroySwapchainKHR device (_swapChain swapChainData) VK_NULL_HANDLE

createSwapChainImageViews :: VkDevice -> SwapChainIndexMap VkImage -> VkFormat -> IO (SwapChainIndexMap VkImageView)
createSwapChainImageViews device swapChainImages swapChainImageFormat = do
    applyIOSwapChainIndex createImageView' swapChainImages
    where
        createImageView' swapChainImage = createImageView device swapChainImage VK_IMAGE_VIEW_TYPE_2D swapChainImageFormat VK_IMAGE_ASPECT_COLOR_BIT 1 1

destroySwapChainImageViews :: VkDevice -> SwapChainIndexMap VkImageView -> IO (SwapChainIndexMap ())
destroySwapChainImageViews device imageViews = applyIOSwapChainIndex (destroyImageView device) imageViews