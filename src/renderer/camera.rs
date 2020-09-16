{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module HulkanEngine3D.Render.Camera where

--import Control.Monad
import Data.IORef
import qualified Data.Text as Text

import Numeric.DataFrame
import Numeric.Dimensions

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Render.TransformObject
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math


data CameraCreateData = CameraCreateData
    { meterPerUnit :: Float
    , near :: Float
    , far :: Float
    , fov :: Float
    , aspect :: Float
    , position :: Vec3f
    } deriving (Show)


getDefaultCameraCreateData :: CameraCreateData
getDefaultCameraCreateData = CameraCreateData
    { meterPerUnit = Constants.meterPerUnit
    , near = Constants.near
    , far = Constants.far
    , fov = Constants.fov
    , aspect = 1.0
    , position = vec3 0 0 0
    }


data CameraObjectData = CameraObjectData
    { _name :: IORef Text.Text
    , _meterPerUnit :: IORef Float
    , _near :: IORef Float
    , _far :: IORef Float
    , _fov :: IORef Float
    , _aspect :: IORef Float
    , _viewMatrix :: IORef Mat44f
    , _invViewMatrix :: IORef Mat44f
    , _viewOriginMatrix :: IORef Mat44f
    , _invViewOriginMatrix :: IORef Mat44f
    , _projectionMatrix :: IORef Mat44f
    , _invProjectionMatrix :: IORef Mat44f
    , _viewProjectionMatrix :: IORef Mat44f
    , _invViewProjectionMatrix :: IORef Mat44f
    , _viewOriginProjectionMatrix :: IORef Mat44f
    , _invViewOriginProjectionMatrix :: IORef Mat44f
    , _viewOriginProjectionMatrixPrev :: IORef Mat44f
    , _transformObject :: TransformObjectData
    } deriving (Show)


class CameraObjectInterface a where
    createCameraObjectData :: Text.Text -> CameraCreateData -> IO a
    getCameraPosition :: a -> IO Vec3f
    getCameraPositionPrev :: a -> IO Vec3f
    getViewMatrix :: a -> IO Mat44f
    getInvViewMatrix :: a -> IO Mat44f
    getViewOriginMatrix :: a -> IO Mat44f
    getInvViewOriginMatrix :: a -> IO Mat44f
    getProjectionMatrix :: a -> IO Mat44f
    getInvProjectionMatrix :: a -> IO Mat44f
    getViewProjectionMatrix :: a -> IO Mat44f
    getInvViewProjectionMatrix :: a -> IO Mat44f
    getViewOriginProjectionMatrix :: a -> IO Mat44f
    getInvViewOriginProjectionMatrix :: a -> IO Mat44f
    getViewOriginProjectionMatrixPrev :: a -> IO Mat44f
    setAspect :: a -> Float -> IO ()
    updateCameraObjectData :: a -> IO ()
    updateProjectionMatrix :: a -> IO ()

instance CameraObjectInterface CameraObjectData where
    createCameraObjectData :: Text.Text -> CameraCreateData -> IO CameraObjectData
    createCameraObjectData name cameraCreateData = do
        logInfo $ "createCameraObjectData :: " ++ show name
        nameRef <- newIORef name
        meterPerUnitRef <- newIORef (meterPerUnit cameraCreateData)
        nearRef <- newIORef (near cameraCreateData)
        farRef <- newIORef (far cameraCreateData)
        fovRef <- newIORef (fov cameraCreateData)
        aspectRef <- newIORef (aspect cameraCreateData)
        viewMatrix <- newIORef matrix4x4_indentity
        invViewMatrix <- newIORef matrix4x4_indentity
        viewOriginMatrix <- newIORef matrix4x4_indentity
        invViewOriginMatrix <- newIORef matrix4x4_indentity
        projectionMatrix <- newIORef matrix4x4_indentity
        invProjectionMatrix <- newIORef matrix4x4_indentity
        viewProjectionMatrix <- newIORef matrix4x4_indentity
        invViewProjectionMatrix <- newIORef matrix4x4_indentity
        viewOriginProjectionMatrix <- newIORef matrix4x4_indentity
        invViewOriginProjectionMatrix <- newIORef matrix4x4_indentity
        viewOriginProjectionMatrixPrev <- newIORef matrix4x4_indentity
        transformObjectData <- newTransformObjectData
        let cameraObjectData = CameraObjectData
                { _name = nameRef
                , _meterPerUnit = meterPerUnitRef
                , _near = nearRef
                , _far = farRef
                , _fov = fovRef
                , _aspect = aspectRef
                , _viewMatrix = viewMatrix
                , _invViewMatrix = invViewMatrix
                , _viewOriginMatrix = viewOriginMatrix
                , _invViewOriginMatrix = invViewOriginMatrix
                , _projectionMatrix = projectionMatrix
                , _invProjectionMatrix = invProjectionMatrix
                , _viewProjectionMatrix = viewProjectionMatrix
                , _invViewProjectionMatrix = invViewProjectionMatrix
                , _viewOriginProjectionMatrix = viewOriginProjectionMatrix
                , _invViewOriginProjectionMatrix = invViewOriginProjectionMatrix
                , _viewOriginProjectionMatrixPrev = viewOriginProjectionMatrixPrev
                , _transformObject = transformObjectData
                }

        -- initialize
        setPosition transformObjectData (position cameraCreateData)
        updateProjectionMatrix cameraObjectData
        return cameraObjectData

    getCameraPosition cameraObjectData = getPosition $ _transformObject cameraObjectData
    getCameraPositionPrev cameraObjectData = getPrevPosition $ _transformObject cameraObjectData
    getViewMatrix cameraObjectData = readIORef (_viewMatrix cameraObjectData)
    getInvViewMatrix cameraObjectData = readIORef (_invViewMatrix cameraObjectData)
    getViewOriginMatrix cameraObjectData = readIORef (_viewOriginMatrix cameraObjectData)
    getInvViewOriginMatrix cameraObjectData = readIORef (_invViewOriginMatrix cameraObjectData)
    getProjectionMatrix cameraObjectData = readIORef (_projectionMatrix cameraObjectData)
    getInvProjectionMatrix cameraObjectData = readIORef (_invProjectionMatrix cameraObjectData)
    getViewProjectionMatrix cameraObjectData = readIORef (_viewProjectionMatrix cameraObjectData)
    getInvViewProjectionMatrix cameraObjectData = readIORef (_invViewProjectionMatrix cameraObjectData)
    getViewOriginProjectionMatrix cameraObjectData = readIORef (_viewOriginProjectionMatrix cameraObjectData)
    getInvViewOriginProjectionMatrix cameraObjectData = readIORef (_invViewOriginProjectionMatrix cameraObjectData)
    getViewOriginProjectionMatrixPrev cameraObjectData = readIORef (_viewOriginProjectionMatrixPrev cameraObjectData)

    setAspect :: CameraObjectData -> Float -> IO ()
    setAspect cameraObjectData aspect = do
        writeIORef (_aspect cameraObjectData) aspect
        updateProjectionMatrix cameraObjectData

    updateCameraObjectData :: CameraObjectData -> IO ()
    updateCameraObjectData cameraObjectData@CameraObjectData {..} = do
        updated <- updateTransformObject _transformObject
        projectionMatrix <- readIORef _projectionMatrix
        invProjectionMatrix <- readIORef _invProjectionMatrix
        viewMatrix <- getInverseMatrix _transformObject
        invViewMatrix <- getMatrix _transformObject
        writeIORef _viewMatrix viewMatrix
        writeIORef _invViewMatrix invViewMatrix
        writeIORef _viewProjectionMatrix (contract viewMatrix projectionMatrix)
        writeIORef _invViewProjectionMatrix (contract invProjectionMatrix invViewMatrix)
        let viewOriginMatrix = DF4 (viewMatrix .! Idx 0) (viewMatrix .! Idx 1) (viewMatrix .! Idx 2) (vec4 0 0 0 1)
            invViewOriginMatrix = (transpose viewOriginMatrix)
        writeIORef _viewOriginMatrix viewOriginMatrix
        writeIORef _invViewOriginMatrix invViewOriginMatrix
        viewOriginProjectionMatrixPrev <- readIORef _viewOriginProjectionMatrix
        writeIORef _viewOriginProjectionMatrixPrev viewOriginProjectionMatrixPrev
        writeIORef _viewOriginProjectionMatrix (contract viewOriginMatrix projectionMatrix)
        writeIORef _invViewOriginProjectionMatrix (contract invProjectionMatrix invViewOriginMatrix)

    updateProjectionMatrix :: CameraObjectData -> IO ()
    updateProjectionMatrix cameraObjectData@CameraObjectData {..} = do
        fov <- readIORef _fov
        aspect <- readIORef _aspect
        near <- readIORef _near
        far <- readIORef _far
        let projectionMatrix = contract (perspective near far (fov/360.0 * 2.0 * pi) aspect) clipSpaceMatrix
        writeIORef _projectionMatrix projectionMatrix
        writeIORef _invProjectionMatrix (inverse projectionMatrix)