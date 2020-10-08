use std::collections::HashMap;
use std::path::{
    Path,
    PathBuf,
};
    
use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::vulkan_context::framebuffer::{
    FramebufferDataCreateInfo,
    FramebufferData,
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorData,
};


pub struct RenderPassPipelineDataName {
    _render_pass_data_name: String,
    _pipeline_data_name: String,
}

#[derive(Clone)]
pub struct RenderPassDataCreateInfo  {
    _render_pass_create_info_name: String,
    _render_pass_frame_buffer_create_info: FramebufferDataCreateInfo,
    _color_attachment_descriptions: Vec<ImageAttachmentDescription>,
    _depth_attachment_descriptions: Vec<ImageAttachmentDescription>,
    _resolve_attachment_descriptions: Vec<ImageAttachmentDescription>,
    _subpass_dependencies: Vec<vk::SubpassDependency>,
    _pipeline_data_create_infos: Vec<PipelineDataCreateInfo>
}

#[derive(Clone, Debug)]
pub struct PipelineDataCreateInfo {
    _pipeline_data_create_info_name: String,
    _pipeline_vertex_shader_file: PathBuf,
    _pipeline_fragment_shader_file: PathBuf,
    _pipeline_shader_defines: Vec<String>,
    _pipeline_dynamic_state_list: Vec<vk::DynamicState>,
    _pipeline_sample_count: vk::SampleCountFlags,
    _pipeline_polygon_mode: vk::PolygonMode,
    _pipeline_cull_mode: vk::CullModeFlags,
    _pipeline_front_face: vk::FrontFace,
    _pipeline_viewport: vk::Viewport,
    _pipeline_scissor_rect: vk::Rect2D,
    _pipeline_color_blend_modes: Vec<vk::PipelineColorBlendAttachmentState>,
    _depth_stencil_state_create_info: DepthStencilStateCreateInfo,
    _descriptor_data_create_info_list: Vec<DescriptorDataCreateInfo>,
}

#[derive(Clone, Debug)]
pub struct DepthStencilStateCreateInfo {
    _depth_test_enable: vk::Bool32,
    _depth_write_enable: vk::Bool32,
    _depth_compare_op: vk::CompareOp,
    _stencil_test_enable: vk::Bool32,
    _front_fail_op: vk::StencilOp, // fail the stencil test
    _front_pass_op: vk::StencilOp, // pass both the depth and stencil tests
    _front_depth_fail_op: vk::StencilOp, // pass the stencil test and fail the depth test
    _front_compare_op: vk::CompareOp,
    _front_compare_mask: u32,
    _front_write_mask: u32,
    _front_reference: u32,
    _back_fail_op: vk::StencilOp, // fail the stencil test
    _back_pass_op: vk::StencilOp, // pass both the depth and stencil tests
    _back_depth_fail_op: vk::StencilOp, // pass the stencil test and fail the depth test
    _back_compare_op: vk::CompareOp,
    _back_compare_mask: u32,
    _back_write_mask: u32,
    _back_reference: u32,
}

impl Default for DepthStencilStateCreateInfo {
    fn default() -> DepthStencilStateCreateInfo {
        DepthStencilStateCreateInfo {
            _depth_test_enable: vk::TRUE,
            _depth_write_enable: vk::TRUE,
            _depth_compare_op: vk::CompareOp::LESS,
            _stencil_test_enable: vk::TRUE,
            _front_fail_op: vk::StencilOp::KEEP, // fail the stencil test
            _front_pass_op: vk::StencilOp::KEEP, // pass both the depth and stencil tests
            _front_depth_fail_op: vk::StencilOp::KEEP, // pass the stencil test and fail the depth test
            _front_compare_op: vk::CompareOp::NEVER,
            _front_compare_mask: 0,
            _front_write_mask: 0,
            _front_reference: 0,
            _back_fail_op: vk::StencilOp::KEEP, // fail the stencil test
            _back_pass_op: vk::StencilOp::KEEP, // pass both the depth and stencil tests
            _back_depth_fail_op: vk::StencilOp::KEEP, // pass the stencil test and fail the depth test
            _back_compare_op: vk::CompareOp::NEVER,
            _back_compare_mask: 0,
            _back_write_mask: 0,
            _back_reference: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ImageAttachmentDescription {
    _attachment_image_format: vk::Format,
    _attachment_image_samples: vk::SampleCountFlags,
    _attachment_load_operation: vk::AttachmentLoadOp,
    _attachment_store_operation: vk::AttachmentStoreOp,
    _attachment_stencil_load_operation: vk::AttachmentLoadOp,
    _attachment_stencil_store_operation: vk::AttachmentStoreOp,
    _attachment_initial_layout: vk::ImageLayout,
    _attachment_final_layout: vk::ImageLayout,
    _attachment_reference_layout: vk::ImageLayout,
}

impl Default for ImageAttachmentDescription {
    fn default() -> ImageAttachmentDescription {
        ImageAttachmentDescription {
            _attachment_image_format: vk::Format::UNDEFINED,
            _attachment_image_samples: vk::SampleCountFlags::TYPE_1,
            _attachment_load_operation: vk::AttachmentLoadOp::DONT_CARE,
            _attachment_store_operation: vk::AttachmentStoreOp::DONT_CARE,
            _attachment_stencil_load_operation: vk::AttachmentLoadOp::DONT_CARE,
            _attachment_stencil_store_operation: vk::AttachmentStoreOp::DONT_CARE,
            _attachment_initial_layout: vk::ImageLayout::UNDEFINED,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::GENERAL,
        }
    }
}

type PipelineDataMap = HashMap<String, PipelineData>;

#[derive(Clone, Debug)]
pub struct PipelineData {
    _pipeline_data_name: String,
    _vertex_shader_create_info: vk::PipelineShaderStageCreateInfo,
    _fragment_shader_create_info: vk::PipelineShaderStageCreateInfo,
    _pipeline: vk::Pipeline,
    _pipeline_layout: vk::PipelineLayout,
    _pipeline_dynamic_states: Vec<vk::DynamicState>,
    _descriptor_data: DescriptorData,
}

#[derive(Clone, Debug)]
pub struct RenderPassData {
    _renderPassDataName: String,
    _renderPass: vk::RenderPass,
    _renderPassFramebufferName: String,
    _defaultPipelineData: PipelineData,
    _pipelineDataMap: PipelineDataMap,
}

pub fn create_subpass_dependency(
    src_subpass: u32,
    dstSubpass: u32,
    srcStageMask: vk::PipelineStageFlags,
    dstStageMask: vk::PipelineStageFlags,
    srcAccessMask: vk::AccessFlags,
    dstAccessMask: vk::AccessFlags,
    dependencyFlags: vk::DependencyFlags
 ) -> vk::SubpassDependency
 
//     createvk:: @vk::SubpassDependency
//         $  set @"srcSubpass" srcSubpass
//         &* set @"dstSubpass" dstSubpass
//         &* set @"srcStageMask" srcStageMask
//         &* set @"dstStageMask" dstStageMask
//         &* set @"srcAccessMask" srcAccessMask
//         &* set @"dstAccessMask" dstAccessMask
//         &* set @"dependencyFlags" dependencyFlags
//
//
// createRenderPassData :: vk::Device -> RenderPassDataCreateInfo -> Vec<DescriptorData> -> IO RenderPassData
// createRenderPassData device renderPassDataCreateInfo@RenderPassDataCreateInfo {..} descriptorDatas = do
//     renderPass <- createRenderPass device renderPassDataCreateInfo
//     pipelineDataList <- forM (zip _pipelineDataCreateInfos descriptorDatas) $ \(pipelineDataCreateInfo, descriptorData) -> do
//         graphicsPipelineData <- createGraphicsPipelineData device renderPass pipelineDataCreateInfo descriptorData
//         return graphicsPipelineData
//     pipelineDataMap <- HashTable.new
//     forM_ pipelineDataList $ \pipelineData ->
//         HashTable.insert pipelineDataMap (_pipelineDataName pipelineData) pipelineData
//     log::info!("CreateRenderPassData : " ++ (Text.unpack _renderPassCreateInfoName)
//     return RenderPassData
//         { _renderPassDataName = _renderPassCreateInfoName
//         , _renderPass = renderPass
//         , _renderPassFramebufferName = (_frameBufferName _renderPassFramebufferCreateInfo)
//         , _defaultPipelineData = pipelineDataList !! 0
//         , _pipelineDataMap = pipelineDataMap
//         }
//
// destroyRenderPassData :: vk::Device -> RenderPassData -> IO ()
// destroyRenderPassData device renderPassData@RenderPassData {..} = do
//     logInfo "DestroyRenderPassData"
//     clearHashTable _pipelineDataMap (\(k, v) -> destroyPipelineData device v)
//     destroyRenderPass device _renderPass _renderPassDataName
//
//
// createRenderPass :: vk::Device -> RenderPassDataCreateInfo -> IO vk::RenderPass
// createRenderPass device renderPassDataCreateInfo@RenderPassDataCreateInfo {..} = do
//     let imageAttachment :: ImageAttachmentDescription -> vk::AttachmentDescription
//         imageAttachment attachmentDescription = createvk:: @vk::AttachmentDescription
//             $  set @"flags" VK_ZERO_FLAGS
//             &* set @"format" (_attachmentImageFormat attachmentDescription)
//             &* set @"samples" (_attachmentImageSamples attachmentDescription)
//             &* set @"loadOp" (_attachmentLoadOperation attachmentDescription)
//             &* set @"storeOp" (_attachmentStoreOperation attachmentDescription)
//             &* set @"stencilLoadOp" (_attachmentStencilLoadOperation attachmentDescription)
//             &* set @"stencilStoreOp" (_attachmentStencilStoreOperation attachmentDescription)
//             &* set @"initialLayout" (_attachmentInitialLayout attachmentDescription)
//             &* set @"finalLayout" (_attachmentFinalLayout attachmentDescription)
//         imageAttachmentReference :: ImageAttachmentDescription -> Int -> vk::AttachmentReference
//         imageAttachmentReference attachmentDescription index = createvk:: @vk::AttachmentReference
//             $  set @"attachment" (fromIntegral index)
//             &* set @"layout" (_attachmentReferenceLayout attachmentDescription)
//         attachmentDescriptions = _colorAttachmentDescriptions ++ _depthAttachmentDescriptions ++ _resolveAttachmentDescriptions
//         imageAttachments = map imageAttachment attachmentDescriptions
//         colorAttachmentCount = length _colorAttachmentDescriptions
//         depthAttachmentCount = length _depthAttachmentDescriptions
//         resolveAttachmentCount = length _resolveAttachmentDescriptions
//         colorAttachmentReferences = zipWith imageAttachmentReference _colorAttachmentDescriptions [0..]
//         depthAttachmentReferences = zipWith imageAttachmentReference _depthAttachmentDescriptions [colorAttachmentCount..]
//         resolveAttachmentReferences = zipWith imageAttachmentReference _resolveAttachmentDescriptions [(colorAttachmentCount + depthAttachmentCount)..]
//         subpass = createvk:: @vk::SubpassDescription
//             $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
//             &* set @"colorAttachmentCount" (fromIntegral . length $ colorAttachmentReferences)
//             &* case colorAttachmentReferences of
//                 [] -> set @"pColorAttachments" VK_NULL
//                 otherwise -> setListRef @"pColorAttachments" colorAttachmentReferences
//             &* case depthAttachmentReferences of
//                 [] -> set @"pDepthStencilAttachment" VK_NULL
//                 otherwise -> setListRef @"pDepthStencilAttachment" depthAttachmentReferences
//             &* case resolveAttachmentReferences of
//                 [] -> set @"pResolveAttachments" VK_NULL
//                 otherwise -> setListRef @"pResolveAttachments" resolveAttachmentReferences
//             &* set @"pPreserveAttachments" VK_NULL
//             &* set @"pInputAttachments" VK_NULL
//         renderPassCreateInfo = createvk:: @vk::RenderPassCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"attachmentCount" (fromIntegral . length $ imageAttachments)
//             &* setListRef @"pAttachments" imageAttachments
//             &* set @"subpassCount" 1
//             &* setvk::Ref @"pSubpasses" subpass
//             &* set @"dependencyCount" (fromIntegral . length $ _subpassDependencies)
//             &* setListRef @"pDependencies" _subpassDependencies
//     renderPass <- alloca $ \renderPassPtr -> do
//         result <- vkCreateRenderPass device (unsafePtr renderPassCreateInfo) VK_NULL renderPassPtr
//         validationVK result "vkCreatePipelineLayout failed!"
//         peek renderPassPtr
//     touchvk::Data renderPassCreateInfo
//     log::info!("Create RenderPass : " ++ Text.unpack _renderPassCreateInfoName ++ " " ++ show renderPass
//     return renderPass
//
//
// destroyRenderPass :: vk::Device -> vk::RenderPass -> String -> IO ()
// destroyRenderPass device renderPass renderPassName = do
//     log::info!("Destroy RenderPass : " ++ Text.unpack renderPassName ++ " " ++ show renderPass
//     vkDestroyRenderPass device renderPass VK_NULL
//
//
// createPipelineLayout :: vk::Device -> [vk::PushConstantRange] -> [vk::DescriptorSetLayout] -> IO vk::PipelineLayout
// createPipelineLayout device pushConstantRanges descriptorSetLayouts = do
//     let pipelineCreateInfo = createvk:: @vk::PipelineLayoutCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"setLayoutCount" (fromIntegral . length $ descriptorSetLayouts)
//             &* setListRef @"pSetLayouts" descriptorSetLayouts
//             &* set @"pushConstantRangeCount" (fromIntegral . length $ pushConstantRanges)
//             &* setListRef @"pPushConstantRanges" pushConstantRanges
//     pipelineLayout <- alloca $ \pipelineLayoutPtr -> do
//         result <- vkCreatePipelineLayout device (unsafePtr pipelineCreateInfo) VK_NULL pipelineLayoutPtr
//         validationVK result "vkCreatePipelineLayout failed!"
//         peek pipelineLayoutPtr
//     touchvk::Data pipelineCreateInfo
//     logTrivialInfo $ "Create PipelineLayout : " ++ (show pipelineLayout)
//     return pipelineLayout
//
//
// destroyPipelineLayout :: vk::Device -> vk::PipelineLayout -> IO ()
// destroyPipelineLayout device pipelineLayout = do
//     logTrivialInfo $ "Destroy PipelineLayout" ++ show pipelineLayout
//     vkDestroyPipelineLayout device pipelineLayout VK_NULL
//
//
// createGraphicsPipelineData :: vk::Device
//                            -> vk::RenderPass
//                            -> PipelineDataCreateInfo
//                            -> DescriptorData
//                            -> IO PipelineData
// createGraphicsPipelineData device renderPass pipelineDataCreateInfo@PipelineDataCreateInfo {..} descriptorData = do
//     vertexShaderCreateInfo <- createShaderStageCreateInfo device _pipelineVertexShaderFile _pipelineShaderDefines VK_SHADER_STAGE_VERTEX_BIT
//     fragmentShaderCreateInfo <- createShaderStageCreateInfo device _pipelineFragmentShaderFile _pipelineShaderDefines VK_SHADER_STAGE_FRAGMENT_BIT
//
//     let depthStencilStateCreateInfo@DepthStencilStateCreateInfo {..} = _depthStencilStateCreateInfo
//         pushConstantData = PushConstantData { modelMatrix = matrix4x4_indentity }
//         pushConstantRange = getPushConstantRange pushConstantData VK_SHADER_STAGE_ALL
//         shaderStageInfos = [vertexShaderCreateInfo, fragmentShaderCreateInfo]
//         shaderStageInfoCount = length shaderStageInfos
//         descriptorSetCount = Constants.swapChainImageCount
//
//     pipelineLayout <- createPipelineLayout device [pushConstantRange] [_descriptorSetLayout descriptorData]
//
//     let vertexInputInfo = createvk:: @vk::PipelineVertexInputStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"vertexBindingDescriptionCount" 1
//             &* setDFRef @"pVertexBindingDescriptions" (scalar vertexInputBindDescription)
//             &* set @"vertexAttributeDescriptionCount" (fromIntegral . totalDim $ inSpaceOf dims vertexInputAttributeDescriptions)
//             &* setDFRef @"pVertexAttributeDescriptions" vertexInputAttributeDescriptions
//         inputAssembly = createvk:: @vk::PipelineInputAssemblyStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
//             &* set @"primitiveRestartEnable" VK_FALSE
//         dynamicState = createvk:: @vk::PipelineDynamicStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"dynamicStateCount" (fromIntegral . length $ _pipelineDynamicStateList)
//             &* case _pipelineDynamicStateList of
//                 [] -> set @"pDynamicStates" VK_NULL
//                 otherwise -> setListRef @"pDynamicStates" _pipelineDynamicStateList
//         viewPortState = createvk:: @vk::PipelineViewportStateCreateInfo
//             $ set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"viewportCount" 1
//             &* setvk::Ref @"pViewports" _pipelineViewport
//             &* set @"scissorCount" 1
//             &* setvk::Ref @"pScissors" _pipelineScissorRect
//         rasterizer = createvk:: @vk::PipelineRasterizationStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"depthClampEnable" VK_FALSE
//             &* set @"rasterizerDiscardEnable" VK_FALSE
//             &* set @"polygonMode" _pipelinePolygonMode
//             &* set @"cullMode" (bitToMask _pipelineCullMode)
//             &* set @"frontFace" _pipelineFrontFace
//             &* set @"depthBiasEnable" VK_FALSE
//             &* set @"depthBiasConstantFactor" 0
//             &* set @"depthBiasClamp" 0
//             &* set @"depthBiasSlopeFactor" 0
//             &* set @"lineWidth" 1.0
//         multisampling = createvk:: @vk::PipelineMultisampleStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"sampleShadingEnable" VK_FALSE
//             &* set @"rasterizationSamples" _pipelineSampleCount
//             &* set @"minSampleShading" 1.0
//             &* set @"pSampleMask" VK_NULL
//             &* set @"alphaToCoverageEnable" VK_FALSE
//             &* set @"alphaToOneEnable" VK_FALSE
//         colorBlending = createvk:: @vk::PipelineColorBlendStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"logicOpEnable" VK_FALSE
//             &* set @"logicOp" VK_LOGIC_OP_COPY
//             &* set @"attachmentCount" (fromIntegral . length $ _pipelineColorBlendModes)
//             &* setListRef @"pAttachments" _pipelineColorBlendModes
//             &* setAt @"blendConstants" @0 0.0
//             &* setAt @"blendConstants" @1 0.0
//             &* setAt @"blendConstants" @2 0.0
//             &* setAt @"blendConstants" @3 0.0
//         depthStencilState = createvk:: @vk::PipelineDepthStencilStateCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"depthTestEnable" _depthTestEnable
//             &* set @"depthWriteEnable" _depthWriteEnable
//             &* set @"depthCompareOp" _depthCompareOp
//             &* set @"depthBoundsTestEnable" VK_FALSE
//             &* set @"minDepthBounds" 0.0
//             &* set @"maxDepthBounds" 1.0
//             &* set @"stencilTestEnable" _stencilTestEnable
//             &* setvk:: @"front"
//                 (  set @"failOp" _frontFailOp
//                 &* set @"passOp" _frontPassOp
//                 &* set @"depthFailOp" _frontDepthFailOp
//                 &* set @"compareOp" _frontCompareOp
//                 &* set @"compareMask" _frontCompareMask
//                 &* set @"writeMask" _frontWriteMask
//                 &* set @"reference" _frontReference )
//             &* setvk:: @"back"
//                 (  set @"failOp" _backFailOp
//                 &* set @"passOp" _backPassOp
//                 &* set @"depthFailOp" _backDepthFailOp
//                 &* set @"compareOp" _backCompareOp
//                 &* set @"compareMask" _backCompareMask
//                 &* set @"writeMask" _backWriteMask
//                 &* set @"reference" _backReference )
//         graphicsPipelineCreateInfo = createvk:: @vk::GraphicsPipelineCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" VK_ZERO_FLAGS
//             &* set @"stageCount" (fromIntegral shaderStageInfoCount)
//             &* setListRef @"pStages" shaderStageInfos
//             &* setvk::Ref @"pVertexInputState" vertexInputInfo
//             &* setvk::Ref @"pInputAssemblyState" inputAssembly
//             &* set @"pTessellationState" VK_NULL
//             &* setvk::Ref @"pViewportState" viewPortState
//             &* setvk::Ref @"pRasterizationState" rasterizer
//             &* setvk::Ref @"pMultisampleState" multisampling
//             &* setvk::Ref @"pDepthStencilState" depthStencilState
//             &* setvk::Ref @"pColorBlendState" colorBlending
//             &* case _pipelineDynamicStateList of
//                 [] -> set @"pDynamicState" VK_NULL
//                 otherwise -> setvk::Ref @"pDynamicState" dynamicState
//             &* set @"layout" pipelineLayout
//             &* set @"renderPass" renderPass
//             &* set @"subpass" 0
//             &* set @"basePipelineHandle" VK_NULL_HANDLE
//             &* set @"basePipelineIndex" (-1)
//
//     graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
//         withPtr graphicsPipelineCreateInfo $ \graphicsPipelineCreateInfoPtr -> do
//             result <- vkCreateGraphicsPipelines device VK_NULL_HANDLE 1 graphicsPipelineCreateInfoPtr VK_NULL graphicsPipelinePtr
//             validationVK result "vkCreateGraphicsPipelines failed!"
//         peek graphicsPipelinePtr
//
//     log::info!("createGraphicsPipeline : " ++ Text.unpack _pipelineDataCreateInfoName ++ " (" ++ show graphicsPipeline ++ ")"
//     logTrivialInfo $ "    shaderDefines : " ++ show _pipelineShaderDefines
//     logTrivialInfo $ "    vertexShader : " ++ show _pipelineVertexShaderFile
//     logTrivialInfo $ "    fragmentShader : " ++ show _pipelineFragmentShaderFile
//
//     return PipelineData
//         { _pipelineDataName = _pipelineDataCreateInfoName
//         , _vertexShaderCreateInfo = vertexShaderCreateInfo
//         , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
//         , _pipeline = graphicsPipeline
//         , _pipelineLayout = pipelineLayout
//         , _pipelineDynamicStates = _pipelineDynamicStateList
//         , _descriptorData = descriptorData
//         }
//
//
// destroyPipelineData :: vk::Device -> PipelineData -> IO ()
// destroyPipelineData device pipelineData@PipelineData {..} = do
//     log::info!("Destroy GraphicsPipeline : " ++ Text.unpack _pipelineDataName ++ "pipeline " ++ show _pipeline ++ ", pipelineLayout" ++ show _pipelineLayout
//     vkDestroyPipeline device _pipeline VK_NULL
//     destroyPipelineLayout device _pipelineLayout
//     destroyShaderStageCreateInfo device _vertexShaderCreateInfo
//     destroyShaderStageCreateInfo device _fragmentShaderCreateInfo
