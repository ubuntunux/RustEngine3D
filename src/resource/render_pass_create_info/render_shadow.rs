{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo.RenderShadow where

import Data.Bits
import qualified Data.Text as Text

import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTargetDeclaration
import HulkanEngine3D.Render.UniformBufferDatas (UniformBufferType (..))
import HulkanEngine3D.Vulkan.Descriptor
import HulkanEngine3D.Vulkan.FrameBuffer
import HulkanEngine3D.Vulkan.RenderPass
import HulkanEngine3D.Vulkan.Texture
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.System (toText)


getRenderPassName :: Constants.RenderObjectType -> Text.Text
getRenderPassName Constants.RenderObject_Static = "render_pass_static_shadow"
getRenderPassName Constants.RenderObject_Skeletal = "render_pass_skeletal_shadow"

getFrameBufferDataCreateInfo :: RendererData -> Text.Text -> Constants.RenderObjectType -> IO FrameBufferDataCreateInfo
getFrameBufferDataCreateInfo rendererData renderPassName renderObjectType = do
    textureDepth <- getRenderTarget rendererData RenderTarget_Shadow
    let (width, height, depth) = (_imageWidth textureDepth, _imageHeight textureDepth, _imageDepth textureDepth)
    return defaultFrameBufferDataCreateInfo
        { _frameBufferName = renderPassName
        , _frameBufferWidth = width
        , _frameBufferHeight = height
        , _frameBufferDepth = depth
        , _frameBufferSampleCount = _imageSampleCount textureDepth
        , _frameBufferViewPort = createViewport 0 0 width height 0 1
        , _frameBufferScissorRect = createScissorRect 0 0 width height
        , _frameBufferColorAttachmentFormats = []
        , _frameBufferDepthAttachmentFormats = [_imageFormat textureDepth]
        , _frameBufferImageViewLists = swapChainIndexMapSingleton [ _imageView textureDepth ]
        , _frameBufferClearValues =
            case renderObjectType of
                Constants.RenderObject_Static -> [ getDepthStencilClearValue 1.0 0 ]
                otherwise -> []
        }

getRenderPassDataCreateInfo :: RendererData -> Constants.RenderObjectType -> IO RenderPassDataCreateInfo
getRenderPassDataCreateInfo rendererData renderObjectType = do
    let renderPassName = getRenderPassName renderObjectType
    frameBufferDataCreateInfo <- getFrameBufferDataCreateInfo rendererData renderPassName renderObjectType
    let sampleCount = _frameBufferSampleCount frameBufferDataCreateInfo
        (attachmentLoadOperation, attachmentInitialLayout) = case renderObjectType of
            Constants.RenderObject_Static -> (VK_ATTACHMENT_LOAD_OP_CLEAR, VK_IMAGE_LAYOUT_UNDEFINED)
            otherwise -> (VK_ATTACHMENT_LOAD_OP_LOAD, VK_IMAGE_LAYOUT_GENERAL)
        colorAttachmentDescriptions = []
        depthAttachmentDescriptions =
            [ defaultAttachmentDescription
                { _attachmentImageFormat = format
                , _attachmentImageSamples = sampleCount
                , _attachmentLoadOperation = attachmentLoadOperation
                , _attachmentStoreOperation = VK_ATTACHMENT_STORE_OP_STORE
                , _attachmentInitialLayout = attachmentInitialLayout
                , _attachmentFinalLayout = VK_IMAGE_LAYOUT_GENERAL
                , _attachmentReferenceLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                } | format <- _frameBufferDepthAttachmentFormats frameBufferDataCreateInfo
            ]
        subpassDependencies =
            [ createSubpassDependency
                VK_SUBPASS_EXTERNAL
                0
                VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                VK_ACCESS_MEMORY_READ_BIT
                (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                VK_DEPENDENCY_BY_REGION_BIT
            , createSubpassDependency
                0
                VK_SUBPASS_EXTERNAL
                VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                VK_ACCESS_MEMORY_READ_BIT
                VK_DEPENDENCY_BY_REGION_BIT
            ]
        pipelineDataCreateInfos =
            [ PipelineDataCreateInfo
                { _pipelineDataCreateInfoName = "render_object"
                , _pipelineVertexShaderFile = "render_object.vert"
                , _pipelineFragmentShaderFile = "shadowmap.frag"
                , _pipelineShaderDefines = [Text.append "RenderMode=" (Text.pack . show . fromEnum $ Constants.RenderMode_Shadow)]
                , _pipelineDynamicStateList = [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR]
                , _pipelineSampleCount = sampleCount
                , _pipelinePolygonMode = VK_POLYGON_MODE_FILL
                , _pipelineCullMode = VK_CULL_MODE_BACK_BIT
                , _pipelineFrontFace = VK_FRONT_FACE_CLOCKWISE
                , _pipelineViewport = _frameBufferViewPort frameBufferDataCreateInfo
                , _pipelineScissorRect = _frameBufferScissorRect frameBufferDataCreateInfo
                , _pipelineColorBlendModes = replicate (length colorAttachmentDescriptions) $ getColorBlendMode BlendMode_None
                , _depthStencilStateCreateInfo = defaultDepthStencilStateCreateInfo
                , _descriptorDataCreateInfoList =
                    [ DescriptorDataCreateInfo
                        0
                        (toText UniformBuffer_SceneConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        1
                        (toText UniformBuffer_ViewConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        2
                        (toText UniformBuffer_LightConstants)
                        DescriptorResourceType_UniformBuffer
                        VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT)
                    , DescriptorDataCreateInfo
                        3
                        "textureBase"
                        DescriptorResourceType_Texture
                        VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                        VK_SHADER_STAGE_FRAGMENT_BIT
                    ]
                }
            ]
    return RenderPassDataCreateInfo
        { _renderPassCreateInfoName = getRenderPassName renderObjectType
        , _renderPassFrameBufferCreateInfo = frameBufferDataCreateInfo
        , _colorAttachmentDescriptions = colorAttachmentDescriptions
        , _depthAttachmentDescriptions = depthAttachmentDescriptions
        , _resolveAttachmentDescriptions = []
        , _subpassDependencies = subpassDependencies
        , _pipelineDataCreateInfos = pipelineDataCreateInfos
        }