{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo where

import HulkanEngine3D.Render.Renderer
import qualified HulkanEngine3D.Constants as Constants
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.CompositeGBuffer as CompositeGBuffer
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderDebug as RenderDebug
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderObject as RenderObject
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderFinal as RenderFinal
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderMotionBlur as RenderMotionBlur
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderSSAO as RenderSSAO
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderShadow as RenderShadow
import HulkanEngine3D.Vulkan.RenderPass


getRenderPassDataCreateInfos :: RendererData -> IO [RenderPassDataCreateInfo]
getRenderPassDataCreateInfos rendererData = do
    render_debug <- RenderDebug.getRenderPassDataCreateInfo rendererData
    render_static_object <- RenderObject.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Static
    render_skeletal_object <- RenderObject.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Skeletal
    render_static_shadow <- RenderShadow.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Static
    render_skeletal_shadow <- RenderShadow.getRenderPassDataCreateInfo rendererData Constants.RenderObject_Skeletal
    composite_gbuffer <- CompositeGBuffer.getRenderPassDataCreateInfo rendererData
    render_final <- RenderFinal.getRenderPassDataCreateInfo rendererData
    render_motion_blur <- RenderMotionBlur.getRenderPassDataCreateInfo rendererData
    render_ssao <- RenderSSAO.getRenderPassDataCreateInfo rendererData

    return [ render_debug
           , render_static_object
           , render_skeletal_object
           , render_static_shadow
           , render_skeletal_shadow
           , composite_gbuffer
           , render_final
           , render_motion_blur
           , render_ssao
           ]