{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.Device
  ( getMaxUsableSampleCount
  , getInstanceExtensionSupport
  , getDeviceExtensionSupport
  , checkExtensionSupport
  , selectPhysicalDevice
  , getPhysicalDeviceProperties
  , createVulkanInstance
  , destroyVulkanInstance
  , createVkSurface
  , destroyVkSurface
  , createDevice
  , destroyDevice
  ) where

import Control.Monad
import Data.Bits
import Data.List ((\\))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Marshal.Create
import qualified Graphics.UI.GLFW as GLFW

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.SwapChain


getExtensionNames :: (Traversable t1, VulkanMarshal t) => [Char] -> t1 t -> IO (t1 String)
getExtensionNames extensionType availableExtensionArrayPtr = do
  availableExtensionNames <- mapM getExtensionName availableExtensionArrayPtr
  logInfo $ "Available " ++ extensionType ++ " extensions : " ++ (show (length availableExtensionNames))
  --mapM (\extensionName -> logInfo $ "    " ++ extensionName) availableExtensionNames
  return availableExtensionNames
  where
    getExtensionName extensionPtr =
      let extensionNamePtr = plusPtr (unsafePtr extensionPtr) (fieldOffset @"extensionName" @VkExtensionProperties)
      in peekCString $ castPtr extensionNamePtr

getInstanceExtensionSupport :: IO [String]
getInstanceExtensionSupport = do
    availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
        result <- vkEnumerateInstanceExtensionProperties VK_NULL_HANDLE counterPtr valueArrayPtr
        validationVK result "vkEnumerateInstanceExtensionProperties error"
    getExtensionNames "Instance" availableExtensionArrayPtr

getDeviceExtensionSupport :: VkPhysicalDevice -> IO [String]
getDeviceExtensionSupport physicalDevice = do
    availableExtensionArrayPtr <- asListVK $ \counterPtr valueArrayPtr -> do
        result <- vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL_HANDLE counterPtr valueArrayPtr
        validationVK result "vkEnumerateInstanceExtensionProperties error"
    getExtensionNames "Device" availableExtensionArrayPtr

checkExtensionSupport :: [String] -> [CString] -> IO Bool
checkExtensionSupport availableDeviceExtensions requireExtensions = do
    requireExtensionNames <- mapM peekCString requireExtensions
    logInfo $ "Require Extensions: " ++ show (length requireExtensionNames) ++ " / " ++ show (length availableDeviceExtensions) ++ " availables."
    isAvailable requireExtensionNames
    return . null $ requireExtensionNames \\ availableDeviceExtensions
    where
        isAvailable [] = return ()
        isAvailable (x:xs) = do
            if elem x availableDeviceExtensions
                then logInfo ("    " ++ x ++ " (OK)")
                else logInfo ("    " ++ x ++ " (Failed)")
            isAvailable xs

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

getMaxUsableSampleCount :: VkPhysicalDeviceProperties -> IO VkSampleCountFlagBits
getMaxUsableSampleCount deviceProperties = do
    let limits = getField @"limits" deviceProperties
        colorSampleCounts = getField @"framebufferColorSampleCounts" limits
        depthSampleCounts = getField @"framebufferDepthSampleCounts" limits
        counts = min colorSampleCounts depthSampleCounts
        splitCounts = filter ((/= VK_ZERO_FLAGS) . (counts .&.))
            [ VK_SAMPLE_COUNT_64_BIT
            , VK_SAMPLE_COUNT_32_BIT
            , VK_SAMPLE_COUNT_16_BIT
            , VK_SAMPLE_COUNT_8_BIT
            , VK_SAMPLE_COUNT_4_BIT
            , VK_SAMPLE_COUNT_2_BIT
            , VK_SAMPLE_COUNT_1_BIT ]
        highestCount = head $ splitCounts >>= maskToBits
    logInfo $ "MSAA Samples: " ++ show highestCount
    return highestCount


createVulkanInstance :: String -> String -> [String] -> [CString] -> IO VkInstance
createVulkanInstance progName engineName layers extensions = do
    let applicationInfo = createVk @VkApplicationInfo
            $ set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
            &* set @"pNext" VK_NULL
            &* setStrRef @"pApplicationName" progName
            &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
            &* setStrRef @"pEngineName" engineName
            &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
            &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 0)
        instanceCreateInfo = createVk @VkInstanceCreateInfo
            $ set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* setVkRef @"pApplicationInfo" applicationInfo
            &* set @"enabledLayerCount" (fromIntegral $ length layers)
            &* setStrListRef @"ppEnabledLayerNames" layers
            &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
            &* setListRef @"ppEnabledExtensionNames" extensions
    vkInstance <- alloca $ \vkInstPtr -> do
        result <- vkCreateInstance (unsafePtr instanceCreateInfo) VK_NULL vkInstPtr
        validationVK result "vkCreateInstance: Failed to create vkInstance."
        peek vkInstPtr
    touchVkData instanceCreateInfo
    return vkInstance

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance = do
    logInfo "Destroy VulkanInstance"
    vkDestroyInstance vkInstance VK_NULL

createVkSurface :: Ptr vkInstance -> GLFW.Window -> IO VkSurfaceKHR
createVkSurface vkInstance window = do
  vkSurface <- alloca $ \vkSurfacePtr -> do
    result <- GLFW.createWindowSurface vkInstance window VK_NULL_HANDLE vkSurfacePtr
    validationVK result "glfwCreateWindowSurface: failed to create window surface"
    logInfo $ "Createad surface: " ++ show vkSurfacePtr
    peek vkSurfacePtr
  return vkSurface

destroyVkSurface :: VkInstance -> VkSurfaceKHR -> IO ()
destroyVkSurface vkInstance vkSurface = do
  destroySurfaceFunc <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
  destroySurfaceFunc vkInstance vkSurface VK_NULL_HANDLE
  logInfo "Destroy VkSurfaceKHR"

getPhysicalDeviceProperties :: VkPhysicalDevice -> IO VkPhysicalDeviceProperties
getPhysicalDeviceProperties physicalDevice = do
    deviceProperties <- alloca $ \propertiesPtr -> do
        vkGetPhysicalDeviceProperties physicalDevice propertiesPtr
        peek propertiesPtr
    return deviceProperties

selectPhysicalDevice :: VkInstance
                     -> Maybe VkSurfaceKHR
                     -> IO (VkPhysicalDevice, Maybe SwapChainSupportDetails, VkPhysicalDeviceFeatures)
selectPhysicalDevice vkInstance maybeVkSurface = do
    devices <- asListVK $ \counterPtr valueArrayPtr -> do
        result <- vkEnumeratePhysicalDevices vkInstance counterPtr valueArrayPtr
        validationVK result "pickPhysicalDevice: Failed to enumerate physical devices."
    when (null devices) $ throwVKMsg "Zeo device count!"
    logInfo $ "Found " ++ show (length devices) ++ " devices."
    selectFirstSuitable devices
    where
        selectFirstSuitable [] = throwVKMsg "No suitable devices!"
        selectFirstSuitable (physicalDevice:physicalDeviceArray) = do
            (result, maybeSwapChainSupportDetails, supportedFeatures) <-
                isDeviceSuitable maybeVkSurface physicalDevice
            if result then do
                logInfo $ "Selected physical device: " ++ show physicalDevice
                pure (physicalDevice, maybeSwapChainSupportDetails, supportedFeatures)
            else
                selectFirstSuitable physicalDeviceArray


createDevice :: VkPhysicalDevice -> [Word32] -> IO VkDevice
createDevice physicalDevice queueFamilyList = do
    queuePrioritiesPtr <- new 1.0
    queueCreateInfoList <- forM queueFamilyList $ \queueFamilyIndex ->
        newVkData @VkDeviceQueueCreateInfo $ \queueCreateInfoPtr -> do
            writeField @"sType" queueCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
            writeField @"pNext" queueCreateInfoPtr VK_NULL_HANDLE
            writeField @"flags" queueCreateInfoPtr VK_ZERO_FLAGS
            writeField @"queueFamilyIndex" queueCreateInfoPtr queueFamilyIndex
            writeField @"queueCount" queueCreateInfoPtr 1
            writeField @"pQueuePriorities" queueCreateInfoPtr queuePrioritiesPtr
    physicalDeviceFeatures <- newVkData @VkPhysicalDeviceFeatures $
        \physicalDeviceFeaturesPtr -> do
            clearStorable physicalDeviceFeaturesPtr
            writeField @"samplerAnisotropy" physicalDeviceFeaturesPtr VK_TRUE
    queueCreateInfoArrayPtr <- newArray queueCreateInfoList
    requireDeviceExtensionsPtr <- newArray Constants.requireDeviceExtensions
    deviceCreateInfo <- withCStringList Constants.vulkanLayers $ \layerCount layerNames -> do
        newVkData @VkDeviceCreateInfo $ \devCreateInfoPtr -> do
            writeField @"sType" devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
            writeField @"pNext" devCreateInfoPtr VK_NULL_HANDLE
            writeField @"flags" devCreateInfoPtr VK_ZERO_FLAGS
            writeField @"pQueueCreateInfos" devCreateInfoPtr queueCreateInfoArrayPtr
            writeField @"queueCreateInfoCount" devCreateInfoPtr (fromIntegral $ length queueCreateInfoList)
            writeField @"enabledLayerCount" devCreateInfoPtr (fromIntegral layerCount)
            writeField @"ppEnabledLayerNames" devCreateInfoPtr layerNames
            writeField @"enabledExtensionCount" devCreateInfoPtr (fromIntegral $ length Constants.requireDeviceExtensions)
            writeField @"ppEnabledExtensionNames" devCreateInfoPtr requireDeviceExtensionsPtr
            writeField @"pEnabledFeatures" devCreateInfoPtr (unsafePtr physicalDeviceFeatures)
    device <- alloca $ \devicePtr -> do
        result <- vkCreateDevice physicalDevice (unsafePtr deviceCreateInfo) VK_NULL_HANDLE devicePtr
        validationVK result "vkCreateDevice: failed to create vkDevice"
        peek devicePtr
    logInfo $ "Created Device: " ++ show device
    touchVkData deviceCreateInfo
    touchVkData physicalDeviceFeatures
    free requireDeviceExtensionsPtr
    free queueCreateInfoArrayPtr
    free queuePrioritiesPtr
    return device

destroyDevice :: VkDevice -> IO ()
destroyDevice device = do
    logInfo "Destroy VkDevice"
    vkDestroyDevice device VK_NULL_HANDLE
