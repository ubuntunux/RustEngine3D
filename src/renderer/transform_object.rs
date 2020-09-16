{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE InstanceSigs        #-}

module HulkanEngine3D.Render.TransformObject
  ( TransformObjectData (..)
  , TransformObjectInterface (..)
  ) where

import Control.Monad
import Data.IORef
import GHC.Generics (Generic)
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Quaternion

import HulkanEngine3D.Utilities.Math
import HulkanEngine3D.Utilities.System()
--import HulkanEngine3D.Utilities.Logger

data TransformObjectData = TransformObjectData
    { _updated :: IORef Bool

    , _front :: IORef Vec3f
    , _left :: IORef Vec3f
    , _up :: IORef Vec3f

    , _position :: IORef Vec3f
    , _rotation :: IORef Vec3f
    , _scale :: IORef Vec3f
    , _eulerToQuaternion :: IORef (Quater Float)
    , _quaternion :: IORef (Quater Float)
    , _fianlQuaternion :: IORef (Quater Float)

    , _prevPosition :: IORef Vec3f
    , _prevPositionStore :: IORef Vec3f
    , _prevRotation :: IORef Vec3f
    , _prevScale :: IORef Vec3f
    , _prevEulerToQuaternion :: IORef (Quater Float)
    , _prevQuaternion :: IORef (Quater Float)
    , _prevFianlQuaternion :: IORef (Quater Float)

    , _quaternionMatrix :: IORef Mat44f
    , _eulerMatrix :: IORef Mat44f
    , _rotationMatrix  :: IORef Mat44f

    , _matrix :: IORef Mat44f
    , _inverseMatrix :: IORef Mat44f
    , _prevMatrix :: IORef Mat44f
    , _prevInverseMatrix :: IORef Mat44f
    } deriving (Show, Generic)


class TransformObjectInterface a where
    newTransformObjectData :: IO a
    getMatrix :: a -> IO Mat44f
    getInverseMatrix :: a -> IO Mat44f
    getLeft :: a -> IO Vec3f
    getUp :: a -> IO Vec3f
    getFront :: a -> IO Vec3f
    getPosition :: a -> IO Vec3f
    getPrevPosition :: a -> IO Vec3f
    setPosition :: a -> Vec3f -> IO ()
    moveFunc :: a -> Vec3f -> Float -> IO ()
    moveLeft :: a -> Float -> IO ()
    moveUp :: a -> Float -> IO ()
    moveFront :: a -> Float -> IO ()
    getRotation :: a -> IO Vec3f
    setRotation :: a -> Vec3f -> IO ()
    rotationFunc :: a -> Word -> Float -> IO ()
    rotationPitch :: a -> Float -> IO ()
    rotationYaw :: a -> Float -> IO ()
    rotationRoll :: a -> Float -> IO ()
    getScale :: a -> IO Vec3f
    setScale :: a -> Vec3f -> IO ()
    updateTransformObject :: a -> IO Bool


instance TransformObjectInterface TransformObjectData where
    newTransformObjectData = do
        updated <- newIORef True
        front <- newIORef world_front
        left <- newIORef world_left
        up <- newIORef world_up

        position <- newIORef float3_zero
        rotation <- newIORef float3_zero
        scale <- newIORef (getFloat3 1.0)
        eulerToQuaternion <- newIORef quaternion_identity
        quaternion <- newIORef quaternion_identity
        fianlQuaternion <- newIORef quaternion_identity

        prevPositionStore <- newIORef float3_zero
        prevPosition <- newIORef float3_zero
        prevRotation <- newIORef float3_zero
        prevScale <- newIORef (getFloat3 1.0)
        prevEulerToQuaternion <- newIORef quaternion_identity
        prevQuaternion <- newIORef quaternion_identity
        prevFianlQuaternion <- newIORef quaternion_identity

        quaternionMatrix <- newIORef matrix4x4_indentity
        eulerMatrix <- newIORef matrix4x4_indentity
        rotationMatrix  <- newIORef matrix4x4_indentity

        matrix <- newIORef matrix4x4_indentity
        inverseMatrix <- newIORef matrix4x4_indentity
        prevMatrix <- newIORef matrix4x4_indentity
        prevInverseMatrix <- newIORef matrix4x4_indentity

        return TransformObjectData
            { _updated = updated
            , _front = front
            , _left = left
            , _up = up
            , _position = position
            , _rotation = rotation
            , _scale = scale
            , _eulerToQuaternion = eulerToQuaternion
            , _quaternion = quaternion
            , _fianlQuaternion = fianlQuaternion
            , _prevPosition = prevPosition
            , _prevPositionStore = prevPositionStore
            , _prevRotation = prevRotation
            , _prevScale = prevScale
            , _prevEulerToQuaternion = prevEulerToQuaternion
            , _prevQuaternion = prevQuaternion
            , _prevFianlQuaternion = prevFianlQuaternion
            , _quaternionMatrix = quaternionMatrix
            , _eulerMatrix = eulerMatrix
            , _rotationMatrix  = rotationMatrix
            , _matrix = matrix
            , _inverseMatrix = inverseMatrix
            , _prevMatrix = prevMatrix
            , _prevInverseMatrix = prevInverseMatrix
            }

    getMatrix :: TransformObjectData -> IO Mat44f
    getMatrix transformObjectData = readIORef (_matrix transformObjectData)

    getInverseMatrix :: TransformObjectData -> IO Mat44f
    getInverseMatrix transformObjectData = readIORef (_inverseMatrix transformObjectData)

    getLeft transformObjectData = readIORef (_left transformObjectData)
    getUp transformObjectData = readIORef (_up transformObjectData)
    getFront transformObjectData = readIORef (_front transformObjectData)

    getPosition transformObjectData = readIORef (_position transformObjectData)
    getPrevPosition transformObjectData = readIORef (_prevPositionStore transformObjectData)
    setPosition transformObjectData vector = writeIORef (_position transformObjectData) vector

    moveFunc transformObjectData vector moveSpeed = do
         position <- readIORef (_position transformObjectData)
         writeIORef (_position transformObjectData) $ position + vector * (fromScalar . scalar $ moveSpeed)

    moveLeft transformObjectData moveSpeed = do
        leftVector <- readIORef (_left transformObjectData)
        moveFunc transformObjectData leftVector moveSpeed

    moveUp transformObjectData moveSpeed = do
        upVector <- readIORef (_up transformObjectData)
        moveFunc transformObjectData upVector moveSpeed

    moveFront transformObjectData moveSpeed = do
        frontVector <- readIORef (_front transformObjectData)
        moveFunc transformObjectData frontVector moveSpeed

    getRotation transformObjectData =
        readIORef (_rotation transformObjectData)

    setRotation transformObjectData vector = do
        let (# pitch, yaw, roll #) = unpackV3# vector
            rotation = vec3 (wrap_rotation pitch) (wrap_rotation yaw) (wrap_rotation roll)
        writeIORef (_rotation transformObjectData) rotation

    rotationFunc transformObjectData index rotationSpeed = do
        rotation <- readIORef (_rotation transformObjectData)
        let pitch = scalar . wrap_rotation $ (unScalar $ rotation .! Idx index) + rotationSpeed
        writeIORef (_rotation transformObjectData) $ update (Idx index :* U) pitch rotation

    rotationPitch transformObjectData rotationSpeed = do
        rotationFunc transformObjectData 0 rotationSpeed

    rotationYaw transformObjectData rotationSpeed = do
        rotationFunc transformObjectData 1 rotationSpeed

    rotationRoll transformObjectData rotationSpeed = do
        rotationFunc transformObjectData 2 rotationSpeed

    getScale transformObjectData =
        readIORef (_scale transformObjectData)

    setScale transformObjectData vector =
        writeIORef (_scale transformObjectData) vector

    updateTransformObject transformObjectData@TransformObjectData {..} = do
        updateInverseMatrix <- pure True
        forceUpdate <- pure False
        prevUpdated <- readIORef _updated

        position <- readIORef _position
        rotation <- readIORef _rotation
        scale <- readIORef _scale

        prevPosition <- readIORef _prevPosition
        prevRotation <- readIORef _prevRotation
        prevScale <- readIORef _prevScale

        writeIORef _prevPositionStore prevPosition

        updatedPosition <- let updated = (forceUpdate || (position /= prevPosition)) in do
            when updated $ do
                writeIORef _prevPosition position
            return updated

        updatedRotation <- let updated = (forceUpdate || (rotation /= prevRotation)) in do
            when updated $ writeIORef _prevRotation rotation
            return updated

        updatedScale <- let updated = (forceUpdate || (scale /= prevScale)) in do
            when updated $ writeIORef _prevScale scale
            return updated

        let updated = updatedPosition || updatedRotation || updatedScale

        when (prevUpdated || updated) $ do
            matrix <- readIORef _matrix
            writeIORef _prevMatrix matrix
            when updateInverseMatrix $ do
                inverseMatrix <- readIORef _inverseMatrix
                writeIORef _prevInverseMatrix inverseMatrix

        when updatedRotation $ do
            let (# pitch, yaw, roll #) = unpackV3# rotation
                --rotationMatrix = rotateEuler pitch yaw roll
                --rotationMatrix = contract (rotateX pitch) $ contract (rotateY yaw) (rotateZ roll)
                rotationMatrix = matrixRotation pitch yaw roll

                left = normalized $ fromHom (rotationMatrix .! Idx 0)
                up = normalized $ fromHom (rotationMatrix .! Idx 1)
                front = normalized $ fromHom (rotationMatrix .! Idx 2)

--                look at algorithm
--                (sin_pitch, cos_pitch) = (sin pitch, cos pitch)
--                (sin_yaw, cos_yaw) = (sin yaw, cos yaw)
--                front = normalized $ vec3 (cos_pitch * sin_yaw) (-sin_pitch) (cos_pitch * cos_yaw)
--                left = normalized $ cross (vec3 0 1 0) front
--                up = normalized $ cross front left
--                (# lX, lY, lZ #) = unpackV3# left
--                (# uX, uY, uZ #) = unpackV3# up
--                (# fX, fY, fZ #) = unpackV3# front
--                rotationMatrix = DF4
--                    (vec4 lX lY lZ 0)
--                    (vec4 uX uY uZ 0)
--                    (vec4 fX fY fZ 0)
--                    (vec4 0  0  0  1)
            writeIORef _rotationMatrix rotationMatrix
            writeIORef _left left
            writeIORef _up up
            writeIORef _front front

        when updated $ do
            rotationMatrix <- readIORef _rotationMatrix
            let matrix = transform_matrix position rotationMatrix scale
            writeIORef _matrix matrix
            when updateInverseMatrix $ do
--                writeIORef _inverseMatrix $ inverse_transform_matrix position rotationMatrix scale
                writeIORef _inverseMatrix (inverse matrix)
        writeIORef _updated updated
        return updated