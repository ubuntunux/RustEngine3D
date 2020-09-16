{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Render.UniformBufferDatas where

import GHC.Generics (Generic)

import Graphics.Vulkan
import Data.Hashable
import qualified Data.HashTable.IO as HashTable
import Foreign.Storable

import Numeric.DataFrame
import Numeric.PrimBytes

import HulkanEngine3D.Vulkan.UniformBuffer
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Math
import qualified HulkanEngine3D.Constants as Constants

data UniformBufferType = UniformBuffer_SceneConstants
                       | UniformBuffer_ViewConstants
                       | UniformBuffer_LightConstants
                       | UniformBuffer_SSAOConstants
                       deriving (Enum, Eq, Ord, Show, Read, Generic)

instance Hashable UniformBufferType

type UniformBufferDataMap = HashTable.BasicHashTable UniformBufferType UniformBufferData

-- scene_constants.glsl - struct SCENE_CONSTANTS
data SceneConstants = SceneConstants
    { _SCREEN_SIZE :: Vec2f
    , _BACKBUFFER_SIZE :: Vec2f
    , _TIME :: Scalar Float
    , _DELTA_TIME :: Scalar Float
    , _JITTER_FRAME :: Scalar Float
    , _SceneConstantsDummy0 :: Scalar Int32
    } deriving (Show, Generic)

-- scene_constants.glsl - struct VIEW_CONSTANTS
data ViewConstants = ViewConstants
    { _VIEW :: Mat44f
    , _INV_VIEW :: Mat44f
    , _VIEW_ORIGIN :: Mat44f
    , _INV_VIEW_ORIGIN :: Mat44f
    , _PROJECTION :: Mat44f
    , _INV_PROJECTION :: Mat44f
    , _VIEW_PROJECTION :: Mat44f
    , _INV_VIEW_PROJECTION :: Mat44f
    , _VIEW_ORIGIN_PROJECTION :: Mat44f
    , _INV_VIEW_ORIGIN_PROJECTION :: Mat44f
    , _VIEW_ORIGIN_PROJECTION_PREV :: Mat44f
    , _CAMERA_POSITION :: Vec3f
    , _VIEWCONSTANTS_DUMMY0 :: Scalar Float
    , _CAMERA_POSITION_PREV :: Vec3f
    , _VIEWCONSTANTS_DUMMY1 :: Scalar Float
    , _NEAR_FAR :: Vec2f
    , _JITTER_DELTA :: Vec2f
    , _JITTER_OFFSET :: Vec2f
    , _VIEWCONSTANTS_DUMMY2 :: Scalar Float
    , _VIEWCONSTANTS_DUMMY3 :: Scalar Float
    } deriving (Show, Generic)

-- scene_constants.glsl - struct LIGHT_CONSTANTS
data LightConstants = LightConstants
  { _SHADOW_VIEW_PROJECTION :: Mat44f
  , _LIGHT_POSITION :: Vec3f
  , _SHADOW_EXP :: Scalar Float
  , _LIGHT_DIRECTION :: Vec3f
  , _SHADOW_BIAS :: Scalar Float
  , _LIGHT_COLOR :: Vec3f
  , _SHADOW_SAMPLES :: Scalar Int32
  , _SHADOW_DIMENSIONS :: Vec4f -- width height near far
  } deriving (Show, Generic)

-- render_ssao.frag - UBOSSAOKernel
data SSAOConstants = SSAOConstants
  { _SSAO_KERNEL_SAMPLES :: DataFrame Float '[64, 4]
  } deriving (Show, Generic)

instance PrimBytes SceneConstants
instance PrimBytes ViewConstants
instance PrimBytes LightConstants
instance PrimBytes SSAOConstants

instance Storable SceneConstants where
    sizeOf _ = bSizeOf (undefined :: SceneConstants)
    alignment _ = bAlignOf (undefined :: SceneConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable ViewConstants where
    sizeOf _ = bSizeOf (undefined :: ViewConstants)
    alignment _ = bAlignOf (undefined :: ViewConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable LightConstants where
    sizeOf _ = bSizeOf (undefined :: LightConstants)
    alignment _ = bAlignOf (undefined :: LightConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

instance Storable SSAOConstants where
    sizeOf _ = bSizeOf (undefined :: SSAOConstants)
    alignment _ = bAlignOf (undefined :: SSAOConstants)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v

defaultLightConstants :: LightConstants
defaultLightConstants = LightConstants
    { _SHADOW_VIEW_PROJECTION = matrix4x4_indentity
    , _LIGHT_POSITION = vec3 0 0 0
    , _SHADOW_EXP = scalar Constants.shadowExp
    , _LIGHT_DIRECTION = vec3 (-3.141592 * 0.5) 0 0
    , _SHADOW_BIAS = scalar Constants.shadowBias
    , _LIGHT_COLOR = vec3 10 10 10
    , _SHADOW_SAMPLES = scalar (fromIntegral Constants.shadowSamples)
    , _SHADOW_DIMENSIONS = vec4 (Constants.shadowDistance * 2.0) (Constants.shadowDistance * 2.0) (-Constants.shadowDepth) Constants.shadowDepth
    }

registUniformBufferDatas :: VkPhysicalDevice -> VkDevice -> UniformBufferDataMap -> IO ()
registUniformBufferDatas physicalDevice device uniformBufferDataMap = do
    registUniformBufferData uniformBufferDataMap UniformBuffer_SceneConstants (bSizeOf @SceneConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_ViewConstants (bSizeOf @ViewConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_LightConstants (bSizeOf @LightConstants undefined)
    registUniformBufferData uniformBufferDataMap UniformBuffer_SSAOConstants (bSizeOf @SSAOConstants undefined)
    where
        registUniformBufferData uniformBufferDataMap uniformBufferType sizeOfUniformBuffer = do
            uniformBufferData <- createUniformBufferData physicalDevice device (toText uniformBufferType) sizeOfUniformBuffer
            HashTable.insert uniformBufferDataMap uniformBufferType uniformBufferData

destroyUniformBufferDatas :: VkDevice -> UniformBufferDataMap -> IO ()
destroyUniformBufferDatas device uniformBufferDataMap = do
    clearHashTable uniformBufferDataMap (\(k, v) -> destroyUniformBufferData device v)