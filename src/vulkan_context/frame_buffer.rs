{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Vulkan.FrameBuffer where

import qualified Data.Text as Text
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger


data FrameBufferDataCreateInfo = FrameBufferDataCreateInfo
    { _frameBufferName :: Text.Text
    , _frameBufferWidth :: Word32
    , _frameBufferHeight :: Word32
    , _frameBufferDepth :: Word32
    , _frameBufferSampleCount :: VkSampleCountFlagBits
    , _frameBufferViewPort :: VkViewport
    , _frameBufferScissorRect :: VkRect2D
    , _frameBufferColorAttachmentFormats :: [VkFormat]
    , _frameBufferDepthAttachmentFormats :: [VkFormat]
    , _frameBufferResolveAttachmentFormats :: [VkFormat]
    , _frameBufferImageViewLists :: SwapChainIndexMap [VkImageView]
    , _frameBufferClearValues :: [VkClearValue]
    }  deriving (Eq, Show)

defaultFrameBufferDataCreateInfo :: FrameBufferDataCreateInfo
defaultFrameBufferDataCreateInfo = FrameBufferDataCreateInfo
    { _frameBufferName = ""
    , _frameBufferWidth = 1024
    , _frameBufferHeight = 768
    , _frameBufferDepth = 1
    , _frameBufferSampleCount = VK_SAMPLE_COUNT_1_BIT
    , _frameBufferViewPort = createViewport 0 0 1024 768 0 1
    , _frameBufferScissorRect = createScissorRect 0 0 1024 768
    , _frameBufferColorAttachmentFormats = []
    , _frameBufferDepthAttachmentFormats = []
    , _frameBufferResolveAttachmentFormats = []
    , _frameBufferImageViewLists = SwapChainIndexMapEmpty
    , _frameBufferClearValues = []
    }

data FrameBufferData = FrameBufferData
    { _frameBufferInfo :: FrameBufferDataCreateInfo
    , _frameBuffers :: SwapChainIndexMap VkFramebuffer
    , _renderPassBeginInfos :: SwapChainIndexMap VkRenderPassBeginInfo
    }

createFrameBufferData :: VkDevice
                      -> VkRenderPass
                      -> FrameBufferDataCreateInfo
                      -> IO FrameBufferData
createFrameBufferData device renderPass frameBufferDataCreateInfo = do
    logInfo $ "Create Framebuffers : "
        ++ (Text.unpack $ _frameBufferName frameBufferDataCreateInfo)
        ++ show ( _frameBufferWidth $ frameBufferDataCreateInfo
                , _frameBufferHeight $ frameBufferDataCreateInfo
                , _frameBufferDepth $ frameBufferDataCreateInfo
                )
    frameBuffers <- applyIOSwapChainIndex createFrameBuffer (_frameBufferImageViewLists frameBufferDataCreateInfo)
    let renderPassBeginInfo frameBuffer = createVk @VkRenderPassBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
            &* set @"pNext" VK_NULL
            &* set @"renderPass" renderPass
            &* set @"framebuffer" frameBuffer
            &* setVk @"renderArea"
                (  setVk @"offset"
                        (  set @"x" 0
                        &* set @"y" 0
                        )
                &* setVk @"extent"
                    (  set @"width" (_frameBufferWidth frameBufferDataCreateInfo)
                    &* set @"height" (_frameBufferHeight frameBufferDataCreateInfo)
                    )
                )
            &* set @"clearValueCount" (fromIntegral . length $  _frameBufferClearValues frameBufferDataCreateInfo)
            &* setListRef @"pClearValues" (_frameBufferClearValues frameBufferDataCreateInfo)
    return FrameBufferData
        { _frameBufferInfo = frameBufferDataCreateInfo
        , _frameBuffers = frameBuffers
        , _renderPassBeginInfos = applySwapChainIndex renderPassBeginInfo frameBuffers
        }
    where
        createFrameBuffer :: [VkImageView] -> IO VkFramebuffer
        createFrameBuffer imageViews = do
            let frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_ZERO_FLAGS
                    &* set @"renderPass" renderPass
                    &* set @"attachmentCount" (fromIntegral $ length imageViews)
                    &* setListRef @"pAttachments" imageViews
                    &* set @"width" (fromIntegral . _frameBufferWidth $ frameBufferDataCreateInfo)
                    &* set @"height" (fromIntegral . _frameBufferHeight $ frameBufferDataCreateInfo)
                    &* set @"layers" (fromIntegral . _frameBufferDepth $ frameBufferDataCreateInfo)
            frameBuffer <- alloca $ \framebufferPtr -> do
                result <- vkCreateFramebuffer device (unsafePtr frameBufferCreateInfo) VK_NULL framebufferPtr
                validationVK result "vkCreateFramebuffer failed!"
                peek framebufferPtr
            touchVkData frameBufferCreateInfo
            return frameBuffer


destroyFrameBufferData :: VkDevice -> FrameBufferData -> IO ()
destroyFrameBufferData device frameBufferData = do
    logInfo $ "Destroy Framebuffers : " ++ show (_frameBufferName . _frameBufferInfo $ frameBufferData) ++ " "  ++ show (_frameBuffers frameBufferData)
    flip applyIOSwapChainIndex' (_frameBuffers frameBufferData) $ \frameBuffer ->
        vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE