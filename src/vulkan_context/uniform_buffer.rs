{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Vulkan.UniformBuffer
    ( UniformBufferData (..)
    , defaultUniformBufferData
    , createUniformBufferData
    , destroyUniformBufferData
    ) where


import Control.Monad
import Data.Bits ((.|.))
import qualified Data.Text as Text

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Vulkan.Vulkan

data UniformBufferData = UniformBufferData
    { _uniformBufferName :: Text.Text
    , _uniformBuffers :: SwapChainIndexMap VkBuffer
    , _uniformBufferMemories :: SwapChainIndexMap VkDeviceMemory
    , _uniformBufferDataSize :: VkDeviceSize
    , _descriptorBufferInfos :: SwapChainIndexMap VkDescriptorBufferInfo
    } deriving (Eq, Show)

defaultUniformBufferData :: UniformBufferData
defaultUniformBufferData = UniformBufferData
    { _uniformBufferName = ""
    , _uniformBuffers = SwapChainIndexMapEmpty
    , _uniformBufferMemories = SwapChainIndexMapEmpty
    , _uniformBufferDataSize = 0
    , _descriptorBufferInfos = SwapChainIndexMapEmpty
    }

createUniformBuffer :: VkPhysicalDevice -> VkDevice -> Text.Text -> Int -> VkDeviceSize -> IO [(VkDeviceMemory, VkBuffer)]
createUniformBuffer physicalDevice device uniformBufferName bufferCount bufferSize = do
    logInfo $ "createUniformBuffer : " ++ Text.unpack uniformBufferName
    replicateM bufferCount $ createBuffer
        physicalDevice
        device
        bufferSize
        VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

destroyUniformBuffer :: VkDevice -> SwapChainIndexMap VkBuffer -> SwapChainIndexMap VkDeviceMemory -> IO ()
destroyUniformBuffer device buffers memories = do
    logInfo "destroyUniformBuffers"
    forM_ (zip (swapChainIndexMapToList buffers) (swapChainIndexMapToList memories)) $ \(buffer, memory) ->
        destroyBuffer device buffer memory

createUniformBufferData :: VkPhysicalDevice -> VkDevice -> Text.Text -> VkDeviceSize -> IO UniformBufferData
createUniformBufferData physicalDevice device uniformBufferName bufferSize = do
    (uniformBufferMemories, uniformBuffers) <- unzip <$> createUniformBuffer
        physicalDevice
        device
        uniformBufferName
        Constants.swapChainImageCount
        bufferSize
    let createVkDescriptorBufferInfo uniformBuffer =
            createVk @VkDescriptorBufferInfo
                $  set @"buffer" uniformBuffer
                &* set @"offset" 0
                &* set @"range" bufferSize
        descriptorBufferInfos = map createVkDescriptorBufferInfo uniformBuffers
    return UniformBufferData
        { _uniformBufferName = uniformBufferName
        , _uniformBuffers = swapChainIndexMapFromList uniformBuffers
        , _uniformBufferMemories = swapChainIndexMapFromList uniformBufferMemories
        , _uniformBufferDataSize = bufferSize
        , _descriptorBufferInfos = swapChainIndexMapFromList descriptorBufferInfos
        }

destroyUniformBufferData :: VkDevice -> UniformBufferData -> IO ()
destroyUniformBufferData device uniformBufferData =
    destroyUniformBuffer device (_uniformBuffers uniformBufferData) (_uniformBufferMemories uniformBufferData)
