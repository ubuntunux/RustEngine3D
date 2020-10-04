use ash::{
    vk,
    Device,
};
use ash::version::DeviceV1_0;

#[derive(Debug, Clone)]
pub enum DescriptorResourceInfo {
    DescriptorBufferInfo(vk::DescriptorBufferInfo),
    DescriptorImageInfo(vk::DescriptorImageInfo),
    InvalidDescriptorInfo
}

#[derive(Debug, Clone)]
pub enum DescriptorResourceType {
    UniformBuffer,
    Texture,
    RenderTarget,
}

#[derive(Debug, Clone)]
pub struct DescriptorDataCreateInfo {
    _descriptor_binding_index: u32,
    _descriptor_name: String,
    _descriptor_resource_type: DescriptorResourceType,
    _descriptor_type: vk::DescriptorType,
    _descriptor_shader_stage: vk::ShaderStageFlags,
}

#[derive(Debug, Clone)]
pub struct DescriptorData {
    pub _descriptor_data_create_info_list: Vec<DescriptorDataCreateInfo>,
    pub _descriptor_set_layout_binding_list: Vec<vk::DescriptorSetLayoutBinding>,
    pub _descriptor_pool_size_list: Vec<vk::DescriptorPoolSize>,
    pub _descriptor_pool: vk::DescriptorPool,
    pub _descriptor_set_layout: vk::DescriptorSetLayout,
    pub _max_descriptor_sets_count: u32,
}

impl Default for DescriptorData {
    fn default() -> DescriptorData {
        DescriptorData {
            _descriptor_data_create_info_list: Vec::<DescriptorDataCreateInfo>::new(),
            _descriptor_set_layout_binding_list: Vec::<vk::DescriptorSetLayoutBinding>::new(),
            _descriptor_pool_size_list: Vec::<vk::DescriptorPoolSize>::new(),
            _descriptor_pool: vk::DescriptorPool::null(),
            _descriptor_set_layout: vk::DescriptorSetLayout::null(),
            _max_descriptor_sets_count: 0,
        }
    }
}

pub fn create_descriptor_pool(
    device: &Device,
    pool_sizes: &Vec<vk::DescriptorPoolSize>,
    max_descriptor_sets_count: u32
) -> vk::DescriptorPool {
    let pool_create_info = vk::DescriptorPoolCreateInfo::builder()
        // Note: for manually free descriptorSets - vk::DescriptorPoolCreateFlags::FREE_DESCRIPTOR_SET
        .flags(vk::DescriptorPoolCreateFlags::empty())
        .pool_sizes(pool_sizes)
        .max_sets(max_descriptor_sets_count)
        .build();
    unsafe {
        let descriptor_pool = device.create_descriptor_pool(&pool_create_info, None).expect("vkCreateDescriptorPool failed!");
        log::info!("    CreateDescriptorPool : {:?}", descriptor_pool);
        descriptor_pool
    }
}

pub fn destroy_descriptor_pool(device: &Device, descriptor_pool: vk::DescriptorPool) {
    log::info!("    DestroyDescriptorPool : {:?}", descriptor_pool);
    unsafe {
        device.destroy_descriptor_pool(descriptor_pool, None);
    }
}

pub fn create_descriptor_set_layout(
    device: &Device,
    layout_bindings: &Vec<vk::DescriptorSetLayoutBinding>
) -> vk::DescriptorSetLayout {
    let layout_create_info = vk::DescriptorSetLayoutCreateInfo::builder()
        .bindings(&layout_bindings)
        .build();
    unsafe {
        let descriptor_set_layout = device.create_descriptor_set_layout(&layout_create_info, None).expect("vkCreateDescriptorSetLayout failed!");
        log::info!("    CreateDescriptorSetLayout: {:?}", descriptor_set_layout);
        descriptor_set_layout
    }
}

pub fn destroy_descriptor_set_layout(device: &Device, descriptor_set_layout: vk::DescriptorSetLayout) {
    log::info!("    DestroyDescriptorSetLayout: {:?}", descriptor_set_layout);
    unsafe {
        device.destroy_descriptor_set_layout(descriptor_set_layout, None);
    }
}


//
//
// createDescriptorData :: VkDevice
//                      -> [DescriptorDataCreateInfo]
//                      -> Int
//                      -> IO DescriptorData
// createDescriptorData device descriptorDataCreateInfoList maxDescriptorSetsCount = do
//     logInfo "createDescriptorData"
//     descriptorLayoutBindingWithPoolSizeList <- forM descriptorDataCreateInfoList $ \descriptorDataCreateInfo -> do
//         let descriptorType = _descriptor_type' descriptorDataCreateInfo
//             bindingIndex = _descriptor_binding_index' descriptorDataCreateInfo
//             shaderStageFlags = _descriptor_shader_stage' descriptorDataCreateInfo
//             descriptorLayoutBinding = createVk @VkDescriptorSetLayoutBinding
//                 $  set @"binding" bindingIndex
//                 &* set @"descriptorType" descriptorType
//                 &* set @"descriptorCount" 1
//                 &* set @"stageFlags" shaderStageFlags
//                 &* set @"pImmutableSamplers" VK_NULL
//             descriptorPoolSize = createVk @VkDescriptorPoolSize
//                 $  set @"type" descriptorType
//                 &* set @"descriptorCount" (fromIntegral maxDescriptorSetsCount)
//         return (descriptorLayoutBinding, descriptorPoolSize)
//     let (descriptorSetLayoutBindingList, descriptorPoolSizeList) = unzip descriptorLayoutBindingWithPoolSizeList
//     descriptorSetLayout <- create_descriptor_set_layout device descriptorSetLayoutBindingList
//     descriptorPool <- createDescriptorPool device descriptorPoolSizeList maxDescriptorSetsCount
//     let descriptorData = DescriptorData
//             { _descriptor_data_create_info_list = descriptorDataCreateInfoList
//             , _descriptor_set_layout_binding_list = descriptorSetLayoutBindingList
//             , _descriptor_pool_size_list = descriptorPoolSizeList
//             , _descriptor_pool = descriptorPool
//             , _descriptor_set_layout = descriptorSetLayout
//             , _max_descriptor_sets_count = maxDescriptorSetsCount
//             }
//     return descriptorData
//
//
// destroyDescriptorData :: VkDevice
//                       -> DescriptorData
//                       -> IO ()
// destroyDescriptorData device descriptorData@DescriptorData{..} = do
//     logInfo "destroyDescriptorData"
//     destroyDescriptorSetLayout device _descriptor_set_layout
//     destroyDescriptorPool device _descriptor_pool
//
//
// createDescriptorSet :: VkDevice -> DescriptorData -> IO [VkDescriptorSet]
// createDescriptorSet device descriptorData@DescriptorData {..} = do
//     let descriptorSetLayouts = replicate Constants.descriptorSetCountAtOnce _descriptor_set_layout
//     allocaArray (length descriptorSetLayouts) $ \descriptorSetLayoutsPtr -> do
//         pokeArray descriptorSetLayoutsPtr descriptorSetLayouts
//         let allocateInfo = createVk @VkDescriptorSetAllocateInfo
//                 $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
//                 &* set @"pNext" VK_NULL
//                 &* set @"descriptorPool" _descriptor_pool
//                 &* set @"descriptorSetCount" (fromIntegral . length $ descriptorSetLayouts)
//                 &* set @"pSetLayouts" descriptorSetLayoutsPtr
//         descriptorSets <- allocaPeekArray Constants.descriptorSetCountAtOnce $ \descriptorSetPtr ->
//             withPtr allocateInfo $ \allocateInfoPtr ->
//                 vkAllocateDescriptorSets device allocateInfoPtr descriptorSetPtr
//         logTrivialInfo $ "    createDescriptorSet : " ++ show descriptorSets
//         return descriptorSets
//
// destroyDescriptorSet :: VkDevice -> VkDescriptorPool -> [VkDescriptorSet] -> Ptr VkDescriptorSet -> IO ()
// destroyDescriptorSet device descriptorPool descriptorSets descriptorSetPtr = do
//     logTrivialInfo $ "    destroyDescriptorSet : " ++ show descriptorSets
//     -- need VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag for vkFreeDescriptorSets
//     when (descriptorSetPtr /= VK_NULL) $
//         vkFreeDescriptorSets device descriptorPool (fromIntegral . length $ descriptorSets) descriptorSetPtr
//             >>= flip validationVK "destroyDescriptorSetData failed!"
//
//
// createWriteDescriptorSets :: VkDescriptorSet
//                           -> [u32]
//                           -> [VkDescriptorSetLayoutBinding]
//                           -> [DescriptorResourceInfo]
//                           -> [VkWriteDescriptorSet]
// createWriteDescriptorSets descriptorSet descriptorBindIndices descriptorSetLayoutBindingList descriptorResourceInfos = do
//     zipWith3 writeDescriptorSet descriptorBindIndices descriptorSetLayoutBindingList descriptorResourceInfos
//     where
//         writeDescriptorSet :: u32 -> VkDescriptorSetLayoutBinding -> DescriptorResourceInfo -> VkWriteDescriptorSet
//         writeDescriptorSet bindingIndex descriptorSetLayoutBinding descriptorResourceInfo =
//             createVk @VkWriteDescriptorSet
//                 $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
//                 &* set @"pNext" VK_NULL
//                 &* set @"dstSet" descriptorSet
//                 &* set @"dstBinding" bindingIndex
//                 &* set @"dstArrayElement" 0
//                 &* set @"descriptorType" (getField @"descriptorType" descriptorSetLayoutBinding)
//                 &* set @"descriptorCount" 1
//                 &* case descriptorResourceInfo of
//                         DescriptorBufferInfo bufferInfo ->
//                             setVkRef @"pBufferInfo" bufferInfo
//                             &* set @"pImageInfo" VK_NULL
//                         DescriptorImageInfo imageInfo ->
//                             set @"pBufferInfo" VK_NULL
//                             &* setVkRef @"pImageInfo" imageInfo
//                         otherwise ->
//                             set @"pBufferInfo" VK_NULL
//                             &* set @"pImageInfo" VK_NULL
//                 &* set @"pTexelBufferView" VK_NULL
//
//
// updateWriteDescriptorSet :: Ptr VkWriteDescriptorSet -> Int -> DescriptorResourceInfo -> IO ()
// updateWriteDescriptorSet writeDescriptorSetPtr descriptorOffset descriptorResourceInfo = do
//     let writeDescriptorSetPtrOffset = ptrAtIndex writeDescriptorSetPtr descriptorOffset
//     case descriptorResourceInfo of
//         DescriptorBufferInfo bufferInfo ->
//             withPtr bufferInfo $ \bufferInfoPtr ->
//                 writeField @"pBufferInfo" writeDescriptorSetPtrOffset bufferInfoPtr
//         DescriptorImageInfo imageInfo ->
//             withPtr imageInfo $ \imageInfoPtr ->
//                 writeField @"pImageInfo" writeDescriptorSetPtrOffset imageInfoPtr
//         otherwise -> return ()