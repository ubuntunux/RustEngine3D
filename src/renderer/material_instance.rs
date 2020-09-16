{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.MaterialInstance where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Render.Material as Material
import HulkanEngine3D.Vulkan.Vulkan
import qualified HulkanEngine3D.Vulkan.RenderPass as RenderPass
import qualified HulkanEngine3D.Vulkan.Descriptor as Descriptor
import HulkanEngine3D.Utilities.Logger

data PipelineBindingData = PipelineBindingData
    { _renderPassData :: RenderPass.RenderPassData
    , _pipelineData :: RenderPass.PipelineData
    , _descriptorSetsPtr :: Ptr VkDescriptorSet
    , _writeDescriptorSetPtrs :: SwapChainIndexMap (Ptr VkWriteDescriptorSet)
    , _descriptorSetCount :: Int
    } deriving Show

type PipelineBindingDataMap = Map.Map RenderPass.RenderPassPipelineDataName PipelineBindingData

data MaterialInstanceData = MaterialInstanceData
    { _materialInstanceDataName :: Text.Text
    , _materialData :: Material.MaterialData
    , _pipelineBindingDataMap :: PipelineBindingDataMap
    } deriving Show


createMaterialInstance :: VkDevice
                       -> Text.Text
                       -> Material.MaterialData
                       -> [(RenderPass.RenderPassData, RenderPass.PipelineData, [[Descriptor.DescriptorResourceInfo]])]
                       -> IO MaterialInstanceData
createMaterialInstance device materialInstanceDataName materialData pipelineBindingCreateInfoList = do
    logInfo $ "createMaterialInstance : " ++ Text.unpack materialInstanceDataName
    logTrivialInfo $ "    materialData : " ++ Text.unpack (Material._materialDataName materialData)
    pipelineBindingDataList <- forM pipelineBindingCreateInfoList $ \(renderPassData, pipelineData, descriptorResourceInfosList) -> do
        let renderPassPipelineDataName = (RenderPass._renderPassDataName renderPassData, RenderPass._pipelineDataName pipelineData)
        logTrivialInfo $ "        (RenderPass, Pipeline) : " ++ show renderPassPipelineDataName
        descriptorSets <- Descriptor.createDescriptorSet device (RenderPass._descriptorData pipelineData)
        descriptorSetsPtr <- mallocArray (length descriptorSets)
        pokeArray descriptorSetsPtr descriptorSets
        let descriptorData = RenderPass._descriptorData $ pipelineData
            descriptorBindingIndices = map Descriptor._descriptorBindingIndex' (Descriptor._descriptorDataCreateInfoList descriptorData)
            descriptorSetLayoutBindingList = Descriptor._descriptorSetLayoutBindingList descriptorData
            descriptorSetBindingCount = length descriptorBindingIndices
        writeDescriptorSetPtrs <- forM (zip descriptorSets descriptorResourceInfosList) $ \(descriptorSet, descriptorResourceInfos) -> do
            let descriptorWrites = Descriptor.createWriteDescriptorSets descriptorSet descriptorBindingIndices descriptorSetLayoutBindingList descriptorResourceInfos
                descriptorWritesCount = length descriptorWrites
            when (descriptorWritesCount /= descriptorSetBindingCount) $ error "descriptorWritesCount Error"
            writeDescriptorSetPtr <- mallocArray descriptorWritesCount
            pokeArray writeDescriptorSetPtr descriptorWrites
            vkUpdateDescriptorSets device (fromIntegral descriptorWritesCount) writeDescriptorSetPtr 0 VK_NULL
            return writeDescriptorSetPtr
        let pipelineBindingData = PipelineBindingData
                { _renderPassData = renderPassData
                , _pipelineData = pipelineData
                , _descriptorSetsPtr = descriptorSetsPtr
                , _writeDescriptorSetPtrs = swapChainIndexMapFromList writeDescriptorSetPtrs
                , _descriptorSetCount = descriptorSetBindingCount
                }
        return (renderPassPipelineDataName, pipelineBindingData)
    return MaterialInstanceData
        { _materialInstanceDataName = materialInstanceDataName
        , _materialData = materialData
        , _pipelineBindingDataMap = Map.fromList pipelineBindingDataList
        }

destroyMaterialInstance :: VkDevice -> MaterialInstanceData -> IO ()
destroyMaterialInstance device materialInstanceData = do
    forM_ (_pipelineBindingDataMap materialInstanceData) $ \pipelineBindingData -> do
        free (_descriptorSetsPtr pipelineBindingData)
        applyIOSwapChainIndex' free (_writeDescriptorSetPtrs pipelineBindingData)

getRenderPassPipelineData :: MaterialInstanceData -> RenderPass.RenderPassPipelineDataName -> (RenderPass.RenderPassData, RenderPass.PipelineData)
getRenderPassPipelineData materialInstanceData renderPassPipelineDataName =
    let pipelineBindingData = getPipelineBindingData materialInstanceData renderPassPipelineDataName
    in (_renderPassData pipelineBindingData, _pipelineData pipelineBindingData)

getPipelineBindingData :: MaterialInstanceData -> RenderPass.RenderPassPipelineDataName -> PipelineBindingData
getPipelineBindingData materialInstanceData renderPassPipelineDataName = Maybe.fromJust $ Map.lookup renderPassPipelineDataName (_pipelineBindingDataMap materialInstanceData)
