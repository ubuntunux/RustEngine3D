{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE InstanceSigs           #-}

module HulkanEngine3D.Render.RenderObject where

import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Numeric.DataFrame

import qualified HulkanEngine3D.Render.Mesh as Mesh
import HulkanEngine3D.Render.Model
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System()

data RenderObjectCreateData = RenderObjectCreateData
    { _modelData' :: ModelData
    , _position' :: Vec3f
    , _rotation' :: Vec3f
    , _scale' :: Vec3f
    , _has_animation_data' :: Bool
    } deriving Show

data RenderObjectData = RenderObjectData
    { _renderObjectName :: Text.Text
    , _modelData :: ModelData
    , _transformObject :: TransformObjectData
    , _animation_play_info :: AnimationPlayInfo
    } deriving Show

data AnimationPlayInfo
    = EmptyAnimationPlayInfo
    | AnimationPlayInfo
        { _last_animation_frame :: Float
        , _animation_loop :: Bool
        , _animation_blend_time :: Float
        , _animation_elapsed_time :: Float
        , _animation_speed :: Float
        , _animation_frame :: Float
        , _animation_start_time :: Float
        , _animation_end_time :: Float
        , _is_animation_end :: Bool
        , _animation_buffers :: Vector.Vector Mat44f
        , _prev_animation_buffers :: Vector.Vector Mat44f
        , _blend_animation_buffers :: Vector.Vector Mat44f
        , _animation_count :: Int
        , _animation_mesh :: Mesh.MeshData
        }
    deriving Show

defaultRenderObjectCreateData :: RenderObjectCreateData
defaultRenderObjectCreateData = RenderObjectCreateData
    { _modelData' = EmptyModelData
    , _position' = vec3 0 0 0
    , _rotation' = vec3 0 0 0
    , _scale' = vec3 1 1 1
    , _has_animation_data' = False
    }

default_animation_play_info :: AnimationPlayInfo
default_animation_play_info = AnimationPlayInfo
    { _last_animation_frame = 0.0
    , _animation_loop = True
    , _animation_blend_time = 0.5
    , _animation_elapsed_time = 0.0
    , _animation_speed = 1.0
    , _animation_frame = 0.0
    , _animation_start_time = 0.0
    , _animation_end_time = -1.0
    , _is_animation_end = False
    , _animation_buffers = Vector.empty
    , _prev_animation_buffers = Vector.empty
    , _blend_animation_buffers = Vector.empty
    , _animation_count = 0
    , _animation_mesh = Mesh.EmptyMeshData
    }

class RenderObjectInterface a where
    createRenderObjectData :: Text.Text -> RenderObjectCreateData -> IO a
    getModelData :: a -> ModelData
    getTransformObjectData :: a -> TransformObjectData
    updateRenderObjectData :: a -> IO ()

instance RenderObjectInterface RenderObjectData where
    createRenderObjectData :: Text.Text -> RenderObjectCreateData -> IO RenderObjectData
    createRenderObjectData renderObjectName renderObjectCreateData = do
        logInfo $ "createRenderObjectData :: " ++ show renderObjectName
        transformObjectData <- newTransformObjectData
        setPosition transformObjectData $ _position' renderObjectCreateData
        setRotation transformObjectData $ _rotation' renderObjectCreateData
        setScale transformObjectData $ _scale' renderObjectCreateData
        return RenderObjectData
            { _renderObjectName = renderObjectName
            , _modelData = _modelData' renderObjectCreateData
            , _transformObject = transformObjectData
            , _animation_play_info =
                case _has_animation_data' renderObjectCreateData of
                    True -> default_animation_play_info
                    otherwise -> EmptyAnimationPlayInfo
            }

    getModelData :: RenderObjectData -> ModelData
    getModelData renderObjectData = _modelData renderObjectData

    getTransformObjectData :: RenderObjectData -> TransformObjectData
    getTransformObjectData renderObjectData = _transformObject renderObjectData

    updateRenderObjectData :: RenderObjectData -> IO ()
    updateRenderObjectData renderObjectData = do
        updateTransformObject (_transformObject renderObjectData)
        return ()

