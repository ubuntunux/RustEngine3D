{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.Queue
    ( QueueFamilyIndices (..)
    , QueueFamilyDatas (..)
    , createQueues
    , getQueueFamilyIndices
    ) where

import Control.Monad
import Data.Bits
import qualified Data.Map as Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

data QueueFamilyIndices = QueueFamilyIndices
    { _graphicsQueueIndex :: Word32
    , _presentQueueIndex :: Word32
    , _computeQueueIndex :: Word32
    , _transferQueueIndex :: Word32
    , _sparseBindingQueueIndex :: Word32
    } deriving (Eq, Show)

data QueueFamilyDatas = QueueFamilyDatas
    { _graphicsQueue :: VkQueue
    , _presentQueue :: VkQueue
    , _queueFamilyIndexList :: [Word32]
    , _queueFamilyCount :: Word32
    , _queueFamilyIndices :: QueueFamilyIndices
    } deriving (Eq, Show)

selectQueueFamily :: VkQueueBitmask FlagMask -> [(Word32, VkQueueFamilyProperties)] -> IO [Word32]
selectQueueFamily _ [] = return []
selectQueueFamily requireQueueFlag ((queueFamilyIndex, queueFamilyProperty):xs) = do
  if 0 < queueCount && (queueFlags .&. requireQueueFlag) /= zeroBits
    then do
      result <- selectQueueFamily requireQueueFlag xs
      return (queueFamilyIndex:result)
    else
      selectQueueFamily requireQueueFlag xs
  where
    queueCount = getField @"queueCount" queueFamilyProperty
    queueFlags = getField @"queueFlags" queueFamilyProperty

selectPresentationFamily :: VkPhysicalDevice
                         -> VkSurfaceKHR
                         -> [(Word32, VkQueueFamilyProperties)]
                         -> IO [Word32]
selectPresentationFamily _ _ [] = return []
selectPresentationFamily device surface (queueFamilies:xs) = do
  let queueFamilyIndex = fst queueFamilies
  supported <- alloca $ \supportedPtr -> do
    result <- vkGetPhysicalDeviceSurfaceSupportKHR device queueFamilyIndex surface supportedPtr
    validationVK result "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
    peek supportedPtr
  if VK_TRUE == supported
    then do
      result <- selectPresentationFamily device surface xs
      return (queueFamilyIndex:result)
    else
      selectPresentationFamily device surface xs

getQueueFamilies :: VkPhysicalDevice -> IO [(Word32, VkQueueFamilyProperties)]
getQueueFamilies physicalDevice = alloca $ \queueFamilyCountPtr -> do
  vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamilyCountPtr VK_NULL_HANDLE
  familyCount <- fromIntegral <$> peek queueFamilyCountPtr
  when (familyCount <= 0) $ throwVKMsg "Zero queue family count!"
  logInfo $ "Found " ++ show familyCount ++ " queue families."
  queueFaimilies <- allocaArray familyCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties physicalDevice queueFamilyCountPtr familiesPtr
    zip [0..] <$> peekArray familyCount familiesPtr
  mapM_ (\(x,y) -> logInfo $ "    [" ++ (show x) ++ "] " ++ (show y) ) queueFaimilies
  return queueFaimilies

getQueueFamilyIndices :: VkPhysicalDevice -> VkSurfaceKHR -> Bool -> IO QueueFamilyIndices
getQueueFamilyIndices physicalDevice vkSurface isConcurrentMode = do
  queueFaimilies <- getQueueFamilies physicalDevice
  presentationFamilyIndices <- selectPresentationFamily physicalDevice vkSurface queueFaimilies
  graphicsQueueIndices <- selectQueueFamily VK_QUEUE_GRAPHICS_BIT queueFaimilies
  computeFamilyIndices <- selectQueueFamily VK_QUEUE_COMPUTE_BIT queueFaimilies
  transferFamilyIndices <- selectQueueFamily VK_QUEUE_TRANSFER_BIT queueFaimilies
  sparseBindingFamilyIndices <- selectQueueFamily VK_QUEUE_SPARSE_BINDING_BIT queueFaimilies
  let
    defaultIndex = graphicsQueueIndices !! 0
    queueFamilyIndices = QueueFamilyIndices
      { _graphicsQueueIndex = defaultIndex
      , _presentQueueIndex = getFamilyIndex presentationFamilyIndices defaultIndex
      , _computeQueueIndex = getFamilyIndex computeFamilyIndices defaultIndex
      , _transferQueueIndex = getFamilyIndex transferFamilyIndices defaultIndex
      , _sparseBindingQueueIndex = getFamilyIndex sparseBindingFamilyIndices defaultIndex
      }
  logInfo $ "Graphics Queue Index : " ++ show (_graphicsQueueIndex queueFamilyIndices)
  logInfo $ "Presentation Queue Index : " ++ show (_presentQueueIndex queueFamilyIndices) ++ " / " ++ show presentationFamilyIndices
  logInfo $ "Computer Queue Index : " ++ show (_computeQueueIndex queueFamilyIndices) ++ " / " ++ show computeFamilyIndices
  logInfo $ "Transfer Queue Index : " ++ show (_transferQueueIndex queueFamilyIndices) ++ " / " ++ show transferFamilyIndices
  logInfo $ "Sparse Binding Queue Index : " ++ show (_sparseBindingQueueIndex queueFamilyIndices)  ++ " / " ++ show sparseBindingFamilyIndices
  return queueFamilyIndices
  where
    getFamilyIndex [] _ = Constants.invalidQueueIndex
    getFamilyIndex indices defaultIndex =
      let result = [x | x <- indices, x /= defaultIndex]
      in
        if isConcurrentMode && (elem defaultIndex indices) then defaultIndex
        else if 0 < (length result) then result !! 0
        else defaultIndex

createQueues :: VkDevice -> [Word32] -> IO (Map.Map Word32 VkQueue)
createQueues device queueFamilyIndices = do
  queueList <- forM queueFamilyIndices $ \queueFamilyIndex -> do
    queue <- alloca $ \queuePtr -> do
      vkGetDeviceQueue device queueFamilyIndex 0 queuePtr
      peek queuePtr
    return (queueFamilyIndex, queue)
  logInfo $ "Created Queues: " ++ show queueList
  return $ Map.fromList queueList