{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.PushConstant
    ( PushConstantData (..)
    , getPushConstantRange
    ) where

import GHC.Generics (Generic)
import Foreign.Storable

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import Numeric.DataFrame
import Numeric.PrimBytes


data PushConstantData = PushConstantData
  { modelMatrix :: Mat44f
  } deriving (Eq, Ord, Show, Generic)

instance PrimBytes PushConstantData

instance Storable PushConstantData where
    sizeOf _ = bSizeOf (undefined :: PushConstantData)
    alignment _ = bAlignOf (undefined :: PushConstantData)
    peek ptr = bPeek ptr
    poke ptr pushConstantData = bPoke ptr pushConstantData


getPushConstantRange :: PushConstantData -> VkShaderStageFlags -> VkPushConstantRange
getPushConstantRange pushConstantData shaderStage = createVk @VkPushConstantRange
    $ set @"stageFlags" shaderStage
    &* set @"size" (bSizeOf @PushConstantData pushConstantData)
    &* set @"offset" 0