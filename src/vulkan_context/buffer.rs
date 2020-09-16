{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE RecordWildCards  #-}

module HulkanEngine3D.Vulkan.Buffer
  ( findMemoryType
  , createBuffer
  , destroyBuffer
  , copyBuffer
  , updateBufferData
  ) where

import Data.Void
import Data.Bits
import Foreign.Ptr
import Foreign.Storable

import Numeric.DataFrame
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Vulkan


data BufferData = BufferData
    { _buffer ::VkBuffer
    , _bufferMemory :: VkDeviceMemory
    , _bufferDescriptor :: VkDescriptorBufferInfo
    , _bufferSize :: VkDeviceSize
    , _bufferAlignment :: VkDeviceSize
    , _bufferMapped :: Ptr Void
    , _bufferUsageFlags :: VkBufferUsageFlags
    , _bufferMemoryPropertyFlags :: VkMemoryPropertyFlags
    }


class BufferInterface a where
    uploadBufferData :: (PrimBytes b) => VkDevice -> a -> b -> IO ()
    uploadBufferDataOffset :: (PrimBytes b) => VkDevice -> a -> b -> VkDeviceSize -> VkDeviceSize -> IO ()
    destroyBufferData :: VkDevice -> a -> IO ()

instance BufferInterface BufferData where
    uploadBufferData :: (PrimBytes b) => VkDevice -> BufferData -> b -> IO ()
    uploadBufferData device bufferData uploadData =
        uploadBufferDataOffset device bufferData uploadData (bSizeOf uploadData::VkDeviceSize) (0::VkDeviceSize)

    uploadBufferDataOffset :: (PrimBytes b) => VkDevice -> BufferData -> b -> VkDeviceSize -> VkDeviceSize -> IO ()
    uploadBufferDataOffset device bufferData@BufferData{..} uploadData size offset = do
        bufferDataPtr <- allocaPeek $ \mappedDataPtr ->
            vkMapMemory device _bufferMemory offset size VK_ZERO_FLAGS mappedDataPtr
        poke (castPtr bufferDataPtr) (scalar uploadData)
        vkUnmapMemory device _bufferMemory

    destroyBufferData :: VkDevice -> BufferData -> IO ()
    destroyBufferData device bufferData@BufferData{..} = do
        destroyBuffer device _buffer _bufferMemory



-- | Return an index of a memory type for a device
findMemoryType :: VkPhysicalDevice
               -> Word32 -- ^ type filter bitfield
               -> VkMemoryPropertyFlags -- ^ desired memory properties
               -> IO Word32
findMemoryType physicalDevice typeFilter propertyFlags = do
    memProps <- allocaPeek $ \ptr -> vkGetPhysicalDeviceMemoryProperties physicalDevice ptr
    let mtCount = getField @"memoryTypeCount" memProps
        memTypes = getVec @"memoryTypes" memProps
        flags index = getField @"propertyFlags" (ixOff (fromIntegral index) memTypes)
        go i | i == mtCount = return i
             | otherwise = if testBit typeFilter (fromIntegral i) &&
                              (propertyFlags == (flags i .&. propertyFlags))
                           then return i
                           else go (i + 1)
    go 0



createBuffer :: VkPhysicalDevice
             -> VkDevice
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> IO (VkDeviceMemory, VkBuffer)
createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags = do    
    let bufferCreateInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bufferSize
          &* set @"usage" bufferUsageFlags
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL

    -- create buffer
    buffer <- allocaPeek $ \bufferPtr -> do
        withPtr bufferCreateInfo $ \createInfoPtr -> do
            result <- vkCreateBuffer device createInfoPtr VK_NULL bufferPtr
            validationVK result "vkCreateBuffer failed!"

    memoryRequirements <- allocaPeek $ vkGetBufferMemoryRequirements device buffer
    memoryTypeIndex <- findMemoryType physicalDevice (getField @"memoryTypeBits" memoryRequirements) memoryPropertyFlags
    
    let allocInfo = createVk @VkMemoryAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"allocationSize" (getField @"size" memoryRequirements)
          &* set @"memoryTypeIndex" memoryTypeIndex
          
    -- create allocate memory
    bufferMemory <- allocaPeek $ \bufferMemoryPtr -> do
        withPtr allocInfo $ \allocInfoPtr -> do
            result <- vkAllocateMemory device allocInfoPtr VK_NULL bufferMemoryPtr
            validationVK result "vkAllocateMemory failed!"

    logTrivialInfo $ "    Create Buffer : "  ++ show buffer ++ ", Memory : " ++ show bufferMemory
    logTrivialInfo $ "        bufferSize : " ++ show bufferSize
    logTrivialInfo $ "        memoryTypeIndex : " ++ show memoryTypeIndex
    logTrivialInfo $ "        " ++ show memoryRequirements

    let memoryOffset = 0 :: VkDeviceSize
    vkBindBufferMemory device buffer bufferMemory memoryOffset

    return (bufferMemory, buffer)

destroyBuffer :: VkDevice -> VkBuffer -> VkDeviceMemory -> IO ()
destroyBuffer device buffer memory = do
    logTrivialInfo $ "    Destroy Buffer : buffer "  ++ show buffer ++ ", memory " ++ show memory
    vkDestroyBuffer device buffer VK_NULL
    vkFreeMemory device memory VK_NULL


-- | @copyBuffer dev pool queue src dest n@ copies @n@ bytes from @src@ buffer to @dest@ buffer.
copyBuffer :: VkDevice
           -> VkCommandPool
           -> VkQueue
           -> VkBuffer 
           -> VkBuffer 
           -> VkDeviceSize 
           -> IO ()
copyBuffer device commandPool commandQueue srcBuffer dstBuffer bufferSize = do
    logTrivialInfo $ "    CopyBuffer : " ++ show srcBuffer ++ " -> " ++ show dstBuffer ++ " { size = " ++ show bufferSize ++ " }"
    runCommandsOnce device commandPool commandQueue $ \commandBuffer -> do
        let copyRegion = createVk @VkBufferCopy
                $  set @"srcOffset" 0
                &* set @"dstOffset" 0
                &* set @"size" bufferSize
        withPtr copyRegion $ \copyRegionPtr -> do
            vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer 1 copyRegionPtr


updateBufferData :: (Storable a) => VkDevice -> VkDeviceMemory -> a -> IO ()
updateBufferData device buffer bufferData = do
    bufferDataPtr <- allocaPeek $ \mappedDataPtr ->
        vkMapMemory device buffer 0 (fromIntegral $ sizeOf bufferData) VK_ZERO_FLAGS mappedDataPtr
    poke (castPtr bufferDataPtr) bufferData
    vkUnmapMemory device buffer