{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Render.RenderElement where

import HulkanEngine3D.Render.MaterialInstance
import HulkanEngine3D.Render.RenderObject
import HulkanEngine3D.Vulkan.GeometryBuffer

data RenderElementData = RenderElementData
    { _renderObject :: RenderObjectData
    , _geometryData :: GeometryData
    , _materialInstanceData :: MaterialInstanceData
    } deriving Show