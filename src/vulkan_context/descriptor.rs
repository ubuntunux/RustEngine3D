{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}


module HulkanEngine3D.Vulkan.Descriptor where

import Control.Monad
import qualified Data.Text as Text
import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

data DescriptorResourceInfo =
    DescriptorBufferInfo VkDescriptorBufferInfo |
    DescriptorImageInfo VkDescriptorImageInfo |
    InvalidDescriptorInfo
    deriving (Eq, Show)

data DescriptorResourceType =
    DescriptorResourceType_UniformBuffer |
    DescriptorResourceType_Texture |
    DescriptorResourceType_RenderTarget
    deriving (Eq, Show)

data DescriptorDataCreateInfo = DescriptorDataCreateInfo
    { _descriptorBindingIndex' :: Word32
    , _descriptorName' :: Text.Text
    , _descriptorResourceType' :: DescriptorResourceType
    , _descriptorType' :: VkDescriptorType
    , _descriptorShaderStage' :: VkShaderStageFlags
    } deriving (Eq, Show)

data DescriptorData = DescriptorData
    { _descriptorDataCreateInfoList :: [DescriptorDataCreateInfo]
    , _descriptorSetLayoutBindingList :: [VkDescriptorSetLayoutBinding]
    , _descriptorPoolSizeList :: [VkDescriptorPoolSize]
    , _descriptorPool :: VkDescriptorPool
    , _descriptorSetLayout :: VkDescriptorSetLayout
    , _maxDescriptorSetsCount :: Int
    } deriving (Eq, Show)

defaultDescriptorData :: DescriptorData
defaultDescriptorData = DescriptorData
    { _descriptorDataCreateInfoList = []
    , _descriptorSetLayoutBindingList = []
    , _descriptorPoolSizeList = []
    , _descriptorPool = VK_NULL
    , _descriptorSetLayout = VK_NULL
    , _maxDescriptorSetsCount = 0
    }

createDescriptorPool :: VkDevice -> [VkDescriptorPoolSize] -> Int -> IO VkDescriptorPool
createDescriptorPool device poolSizeList maxDescriptorSetsCount = do
    let poolCreateInfo = createVk @VkDescriptorPoolCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS -- manually free descriptorSets - VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            &* set @"poolSizeCount" (fromIntegral $ length poolSizeList)
            &* setListRef @"pPoolSizes" poolSizeList
            &* set @"maxSets" (fromIntegral maxDescriptorSetsCount)
    descriptorPool <- allocaPeek $ \descriptorPoolPtr ->
        withPtr poolCreateInfo $ \poolCreateInfoPtr ->
            vkCreateDescriptorPool device poolCreateInfoPtr VK_NULL descriptorPoolPtr
    logTrivialInfo $ "    createDescriptorPool : " ++ show descriptorPool
    return descriptorPool

destroyDescriptorPool :: VkDevice -> VkDescriptorPool -> IO ()
destroyDescriptorPool device descriptorPool = do
    logTrivialInfo $ "    destroyDescriptorPool : " ++ show descriptorPool
    vkDestroyDescriptorPool device descriptorPool VK_NULL


createDescriptorSetLayout :: VkDevice -> [VkDescriptorSetLayoutBinding] -> IO VkDescriptorSetLayout
createDescriptorSetLayout device layoutBindingList = do
    let layoutCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"bindingCount" (fromIntegral $ length layoutBindingList)
            &* setListRef @"pBindings" layoutBindingList
    descriptorSetLayout <- allocaPeek $ \descriptorSetLayoutPtr ->
        withPtr layoutCreateInfo $ \layoutCreateInfoPtr ->
            vkCreateDescriptorSetLayout device layoutCreateInfoPtr VK_NULL descriptorSetLayoutPtr
    logTrivialInfo $ "    createDescriptorSetLayout : " ++ show descriptorSetLayout
    return descriptorSetLayout


destroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> IO ()
destroyDescriptorSetLayout device descriptorSetLayout = do
    logTrivialInfo $ "    destroyDescriptorSetLayout : " ++ show descriptorSetLayout
    vkDestroyDescriptorSetLayout device descriptorSetLayout VK_NULL


createDescriptorData :: VkDevice
                     -> [DescriptorDataCreateInfo]
                     -> Int
                     -> IO DescriptorData
createDescriptorData device descriptorDataCreateInfoList maxDescriptorSetsCount = do
    logInfo "createDescriptorData"
    descriptorLayoutBindingWithPoolSizeList <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
        let descriptorType = _descriptorType' descriptorDataCreateInfo
            bindingIndex = _descriptorBindingIndex' descriptorDataCreateInfo
            shaderStageFlags = _descriptorShaderStage' descriptorDataCreateInfo
            descriptorLayoutBinding = createVk @VkDescriptorSetLayoutBinding
                $  set @"binding" bindingIndex
                &* set @"descriptorType" descriptorType
                &* set @"descriptorCount" 1
                &* set @"stageFlags" shaderStageFlags
                &* set @"pImmutableSamplers" VK_NULL
            descriptorPoolSize = createVk @VkDescriptorPoolSize
                $  set @"type" descriptorType
                &* set @"descriptorCount" (fromIntegral maxDescriptorSetsCount)
        return (descriptorLayoutBinding, descriptorPoolSize)
    let (descriptorSetLayoutBindingList, descriptorPoolSizeList) = unzip descriptorLayoutBindingWithPoolSizeList
    descriptorSetLayout <- createDescriptorSetLayout device descriptorSetLayoutBindingList
    descriptorPool <- createDescriptorPool device descriptorPoolSizeList maxDescriptorSetsCount
    let descriptorData = DescriptorData
            { _descriptorDataCreateInfoList = descriptorDataCreateInfoList
            , _descriptorSetLayoutBindingList = descriptorSetLayoutBindingList
            , _descriptorPoolSizeList = descriptorPoolSizeList
            , _descriptorPool = descriptorPool
            , _descriptorSetLayout = descriptorSetLayout
            , _maxDescriptorSetsCount = maxDescriptorSetsCount
            }
    return descriptorData


destroyDescriptorData :: VkDevice
                      -> DescriptorData
                      -> IO ()
destroyDescriptorData device descriptorData@DescriptorData{..} = do
    logInfo "destroyDescriptorData"
    destroyDescriptorSetLayout device _descriptorSetLayout
    destroyDescriptorPool device _descriptorPool


createDescriptorSet :: VkDevice -> DescriptorData -> IO [VkDescriptorSet]
createDescriptorSet device descriptorData@DescriptorData {..} = do
    let descriptorSetLayouts = replicate Constants.descriptorSetCountAtOnce _descriptorSetLayout
    allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
        pokeArray descriptorSetLayoutsPtr descriptorSetLayouts
        let allocateInfo = createVk @VkDescriptorSetAllocateInfo
                $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                &* set @"pNext" VK_NULL
                &* set @"descriptorPool" _descriptorPool
                &* set @"descriptorSetCount" (fromIntegral . length $ descriptorSetLayouts)
                &* set @"pSetLayouts" descriptorSetLayoutsPtr
        descriptorSets <- allocaPeekArray Constants.descriptorSetCountAtOnce $ \descriptorSetPtr ->
            withPtr allocateInfo $ \allocateInfoPtr ->
                vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
        logTrivialInfo $ "    createDescriptorSet : " ++ show descriptorSets
        return descriptorSets

destroyDescriptorSet :: VkDevice -> VkDescriptorPool -> [VkDescriptorSet] -> Ptr VkDescriptorSet -> IO ()
destroyDescriptorSet device descriptorPool descriptorSets descriptorSetPtr = do
    logTrivialInfo $ "    destroyDescriptorSet : " ++ show descriptorSets
    -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
    when (descriptorSetPtr /= VK_NULL) $
        vkFreeDescriptorSets device descriptorPool (fromIntegral . length $ descriptorSets) descriptorSetPtr
            >>= flip validationVK "destroyDescriptorSetData failed!"


createWriteDescriptorSets :: VkDescriptorSet
                          -> [Word32]
                          -> [VkDescriptorSetLayoutBinding]
                          -> [DescriptorResourceInfo]
                          -> [VkWriteDescriptorSet]
createWriteDescriptorSets descriptorSet descriptorBindIndices descriptorSetLayoutBindingList descriptorResourceInfos = do
    zipWith3 writeDescriptorSet descriptorBindIndices descriptorSetLayoutBindingList descriptorResourceInfos
    where
        writeDescriptorSet :: Word32 -> VkDescriptorSetLayoutBinding -> DescriptorResourceInfo -> VkWriteDescriptorSet
        writeDescriptorSet bindingIndex descriptorSetLayoutBinding descriptorResourceInfo =
            createVk @VkWriteDescriptorSet
                $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                &* set @"pNext" VK_NULL
                &* set @"dstSet" descriptorSet
                &* set @"dstBinding" bindingIndex
                &* set @"dstArrayElement" 0
                &* set @"descriptorType" (getField @"descriptorType" descriptorSetLayoutBinding)
                &* set @"descriptorCount" 1
                &* case descriptorResourceInfo of
                        DescriptorBufferInfo bufferInfo ->
                            setVkRef @"pBufferInfo" bufferInfo
                            &* set @"pImageInfo" VK_NULL
                        DescriptorImageInfo imageInfo ->
                            set @"pBufferInfo" VK_NULL
                            &* setVkRef @"pImageInfo" imageInfo
                        otherwise ->
                            set @"pBufferInfo" VK_NULL
                            &* set @"pImageInfo" VK_NULL
                &* set @"pTexelBufferView" VK_NULL


updateWriteDescriptorSet :: Ptr VkWriteDescriptorSet -> Int -> DescriptorResourceInfo -> IO ()
updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo = do
    let writeDescriptorSetPtrOffset = ptrAtIndex writeDescriptorSetPtr descriptorOffset
    case descriptorResourceInfo of
        DescriptorBufferInfo bufferInfo ->
            withPtr bufferInfo $ \bufferInfoPtr ->
                writeField @"pBufferInfo" writeDescriptorSetPtrOffset bufferInfoPtr
        DescriptorImageInfo imageInfo ->
            withPtr imageInfo $ \imageInfoPtr ->
                writeField @"pImageInfo" writeDescriptorSetPtrOffset imageInfoPtr
        otherwise -> return ()