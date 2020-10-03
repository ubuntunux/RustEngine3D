{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Render.RenderTarget where

import qualified Data.HashTable.IO as HashTable

import Graphics.Vulkan.Core_1_0

import HulkanEngine3D.Render.Renderer
import HulkanEngine3D.Render.RenderTargetDeclaration
import HulkanEngine3D.Vulkan.Swapchain
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import HulkanEngine3D.Utilities.System
import qualified HulkanEngine3D.Constants as Constants

data RenderTargetType = RenderTarget_SceneColor
                      | RenderTarget_SceneColorCopy 
                      | RenderTarget_SceneDepth
                      | RenderTarget_BackBuffer
                      | RenderTarget_BackBufferCopy
                      | RenderTarget_SceneAlbedo
                      | RenderTarget_SceneNormal
                      | RenderTarget_SceneMaterial
                      | RenderTarget_SceneVelocity
                      | RenderTarget_SSAO
                      | RenderTarget_Shadow
                      deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance Hashable RenderTargetType

type RenderTargetDataMap = HashTable.BasicHashTable RenderTargetType Texture.TextureData


createRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
createRenderTargets rendererData renderTargetDataMap = do
    swapChainData <- getSwapchainData rendererData
    let windowWidth = getField @"width" (_swapChainExtent swapChainData)
        windowHeight = getField @"height" (_swapChainExtent swapChainData)
        samples = VK_SAMPLE_COUNT_1_BIT -- min VK_SAMPLE_COUNT_4_BIT (_msaaSamples . _renderFeatures $ rendererData)
        enableMipmap = True
        disableMipmap = False
        enableAnisotropy = True
        disableAnisotropy = False
        immutable = True
        mutable = False
        emptyData = (Texture._textureCreateInfoData Texture.defaultTextureCreateInfo)
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneColor $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16B16A16_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneColorCopy $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16B16A16_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneDepth $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_D32_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_BackBuffer $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_B8G8R8A8_UNORM
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_BackBufferCopy $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_B8G8R8A8_UNORM
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneAlbedo $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R8G8B8A8_UNORM
            VK_IMAGE_VIEW_TYPE_2D
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneMaterial $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R8G8B8A8_UNORM
            VK_IMAGE_VIEW_TYPE_2D
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneNormal $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R8G8B8A8_UNORM
            VK_IMAGE_VIEW_TYPE_2D
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SceneVelocity $
        Texture.TextureCreateInfo
            windowWidth
            windowHeight
            1
            VK_FORMAT_R16G16_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_SSAO $
        Texture.TextureCreateInfo
            (div windowWidth 2)
            (div windowHeight 2)
            1
            VK_FORMAT_R16_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            VK_SAMPLE_COUNT_1_BIT
            VK_FILTER_LINEAR
            VK_FILTER_LINEAR
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    registRenderTarget rendererData renderTargetDataMap RenderTarget_Shadow $
        Texture.TextureCreateInfo
            Constants.shadowMapSize
            Constants.shadowMapSize
            1
            VK_FORMAT_D32_SFLOAT
            VK_IMAGE_VIEW_TYPE_2D
            samples
            VK_FILTER_NEAREST
            VK_FILTER_NEAREST
            VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            disableMipmap
            disableAnisotropy
            mutable
            emptyData
    where
        registRenderTarget :: RendererData -> RenderTargetDataMap -> RenderTargetType -> Texture.TextureCreateInfo -> IO ()
        registRenderTarget rendererData renderTargetDataMap renderTargetType textureCreateInfo = do
            textureData <- createRenderTarget rendererData (toText renderTargetType) textureCreateInfo
            HashTable.insert renderTargetDataMap renderTargetType textureData

destroyRenderTargets :: RendererData -> RenderTargetDataMap -> IO ()
destroyRenderTargets rendererData renderTargetDataMap =
    clearHashTable renderTargetDataMap (\(k, v) -> destroyTexture rendererData v)
