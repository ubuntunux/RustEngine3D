{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module HulkanEngine3D.Vulkan.Vulkan where

import Data.Bits
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame

import HulkanEngine3D.Utilities.System

data SwapChainIndexMap a
    = SwapChainIndexMap a a a
    | SwapChainIndexMapEmpty
    deriving (Eq, Show)

data FrameIndexMap a
    = FrameIndexMap a a
    | FrameIndexMapEmpty
    deriving (Eq, Show)

data RenderFeatures = RenderFeatures
    { _anisotropyEnable :: VkBool32
    , _msaaSamples :: VkSampleCountFlagBits
    } deriving (Eq, Show)

data BlendMode = BlendMode_None | BlendMode_AlphaBlend

applySwapChainIndex :: (a -> b) -> SwapChainIndexMap a -> SwapChainIndexMap b
applySwapChainIndex f (SwapChainIndexMap a b c) = SwapChainIndexMap (f a) (f b) (f c)
applySwapChainIndex f _ = SwapChainIndexMapEmpty

applyIOSwapChainIndex :: (a -> IO b) -> SwapChainIndexMap a -> IO (SwapChainIndexMap b)
applyIOSwapChainIndex f (SwapChainIndexMap a b c) = do
    a' <- f a
    b' <- f b
    c' <- f c
    return $ SwapChainIndexMap a' b' c'
applyIOSwapChainIndex f _ = return SwapChainIndexMapEmpty

applyIOSwapChainIndex' :: (a -> IO b) -> SwapChainIndexMap a -> IO ()
applyIOSwapChainIndex' f abc = do
    applyIOSwapChainIndex f abc
    return ()

atSwapChainIndex :: Int -> SwapChainIndexMap a -> a
atSwapChainIndex 0 (SwapChainIndexMap a b c) = a
atSwapChainIndex 1 (SwapChainIndexMap a b c) = b
atSwapChainIndex 2 (SwapChainIndexMap a b c) = c
atSwapChainIndex _ _ = undefined

swapChainIndexMapSingleton :: a -> SwapChainIndexMap a
swapChainIndexMapSingleton a = SwapChainIndexMap a a a

swapChainIndexMapFromList :: [a] -> SwapChainIndexMap a
swapChainIndexMapFromList (a:b:c:[]) = SwapChainIndexMap a b c
swapChainIndexMapFromList _ = SwapChainIndexMapEmpty

swapChainIndexMapToList :: SwapChainIndexMap a -> [a]
swapChainIndexMapToList (SwapChainIndexMap a b c) = [a, b, c]
swapChainIndexMapToList _ = []

atFrameIndex :: Int -> FrameIndexMap a -> a
atFrameIndex 0 (FrameIndexMap a b) = a
atFrameIndex 1 (FrameIndexMap a b) = b
atFrameIndex _ _ = undefined

applyFrameIndex :: (a -> b) -> FrameIndexMap a -> FrameIndexMap b
applyFrameIndex f (FrameIndexMap a b) = FrameIndexMap (f a) (f b)
applyFrameIndex f _ = FrameIndexMapEmpty

applyIOFrameIndex :: (a -> IO b) -> FrameIndexMap a -> IO (FrameIndexMap b)
applyIOFrameIndex f (FrameIndexMap a b) = do
    a' <- f a
    b' <- f b
    return $ FrameIndexMap a' b'
applyIOFrameIndex f _ = return FrameIndexMapEmpty

applyIOFrameIndex' :: (a -> IO b) -> FrameIndexMap a -> IO ()
applyIOFrameIndex' f ab = do
    applyIOFrameIndex f ab
    return ()

frameIndexMapSingleton :: a -> FrameIndexMap a
frameIndexMapSingleton a = FrameIndexMap a a

frameIndexMapFromList :: [a] -> FrameIndexMap a
frameIndexMapFromList (a:b:[]) = FrameIndexMap a b
frameIndexMapFromList _ = FrameIndexMapEmpty

frameIndexMapToList :: FrameIndexMap a -> [a]
frameIndexMapToList (FrameIndexMap a b) = [a, b]
frameIndexMapToList _ = []

getColor32 :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
getColor32 r g b a = (min 255 r) .|. shift (min 255 g) 8 .|. shift (min 255 b) 16 .|. shift (min 255 a) 24

getColorBlendMode :: BlendMode -> VkPipelineColorBlendAttachmentState
getColorBlendMode blendMode =
    case blendMode of
        BlendMode_None -> createVk @VkPipelineColorBlendAttachmentState
            $  set @"colorWriteMask" ( VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT )
            &* set @"blendEnable" VK_FALSE
            &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
            &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
            &* set @"colorBlendOp" VK_BLEND_OP_ADD
            &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
            &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
            &* set @"alphaBlendOp" VK_BLEND_OP_ADD
        BlendMode_AlphaBlend -> createVk @VkPipelineColorBlendAttachmentState
             $  set @"colorWriteMask" ( VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT )
             &* set @"blendEnable" VK_TRUE
             &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_SRC_ALPHA
             &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
             &* set @"colorBlendOp" VK_BLEND_OP_ADD
             &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
             &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
             &* set @"alphaBlendOp" VK_BLEND_OP_ADD

getColorClearValue :: [Float] -> VkClearValue
getColorClearValue colors = createVk @VkClearValue
    $ setVk @"color"
        $ setVec @"float32" (toColorVector colors)
    where
        toColorVector :: [Float] -> Vec4f
        toColorVector (x:y:z:w:xs) = vec4 x y z w
        toColorVector (x:y:z:xs) = vec4 x y z 0
        toColorVector (x:y:xs) = vec4 x y 0 0
        toColorVector (x:xs) = vec4 x 0 0 0
        toColorVector _ = vec4 0 0 0 0

getDepthStencilClearValue :: Float -> Word32 -> VkClearValue
getDepthStencilClearValue depthClearValue stencilClearValue = createVk @VkClearValue
    $ setVk @"depthStencil"
        $  set @"depth" depthClearValue
        &* set @"stencil" stencilClearValue

createViewport :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> VkViewport
createViewport x y width height minDepth maxDepth = createVk @VkViewport
    $  set @"x" (fromIntegral x)
    &* set @"y" (fromIntegral y)
    &* set @"width" (fromIntegral width)
    &* set @"height" (fromIntegral height)
    &* set @"minDepth" (fromIntegral minDepth)
    &* set @"maxDepth" (fromIntegral maxDepth)

createScissorRect :: Int32 -> Int32 -> Word32 -> Word32 -> VkRect2D
createScissorRect x y width height = createVk @VkRect2D
    $  setVk @"extent"
        (  set @"width" width
        &* set @"height" height
        )
    &* setVk @"offset"
        (  set @"x" x
        &* set @"y" y
        )

runCommandsOnce :: VkDevice
                -> VkCommandPool
                -> VkQueue
                -> (VkCommandBuffer -> IO ())
                -> IO ()
runCommandsOnce device commandPool commandQueue action = do
    let allocInfo = createVk @VkCommandBufferAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
            &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
            &* set @"commandPool" commandPool
            &* set @"commandBufferCount" 1
            &* set @"pNext" VK_NULL

    allocaPeek $ \commandBufferPtr -> do
        withPtr allocInfo $ \allocInfoPtr -> do
            vkAllocateCommandBuffers device allocInfoPtr commandBufferPtr
        commandBuffer <- peek commandBufferPtr

        let beginInfo = createVk @VkCommandBufferBeginInfo
                $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                &* set @"pNext" VK_NULL

        withPtr beginInfo $ \beginInfoPtr -> do
            vkBeginCommandBuffer commandBuffer beginInfoPtr

        -- run action
        action commandBuffer

        vkEndCommandBuffer commandBuffer

        let submitInfo = createVk @VkSubmitInfo
                $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
                &* set @"pNext" VK_NULL
                &* set @"waitSemaphoreCount" 0
                &* set @"pWaitSemaphores"   VK_NULL
                &* set @"pWaitDstStageMask" VK_NULL
                &* set @"commandBufferCount" 1
                &* set @"pCommandBuffers" commandBufferPtr
                &* set @"signalSemaphoreCount" 0
                &* set @"pSignalSemaphores" VK_NULL

        {- TODO: a real app would need a better logic for waiting.

                 In the example below, we create a new fence every time we want to
                 execute a single command. Then, we attach this fence to our command.
                 vkWaitForFences makes the host (CPU) wait until the command is executed.
                 The other way to do this thing is vkQueueWaitIdle.

                 I guess, a good approach could be to pass the fence to this function
                 from the call site. The call site would decide when it wants to wait
                 for this command to finish.

                 Even if we don't pass the fence from outside, maybe we should create
                 the fence oustise of the innermost `locally` scope. This way, the
                 fence would be shared between calls (on the other hand, a possible
                 concurrency would be hurt in this case).
               -}
        --   fence <- createFence dev False
        --   withVkPtr submitInfo $ \siPtr ->
        --       vkQueueSubmit cmdQueue 1 siPtr fence
        --   fencePtr <- newArrayPtr [fence]
        --   vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound :: Word64)

        withPtr submitInfo $ \submitInfoPtr -> do
            result <- vkQueueSubmit commandQueue 1 submitInfoPtr VK_NULL_HANDLE
            validationVK result "vkQueueSubmit error"

        vkQueueWaitIdle commandQueue >>= flip validationVK "vkQueueWaitIdle error"

        vkFreeCommandBuffers device commandPool 1 commandBufferPtr
    return ()