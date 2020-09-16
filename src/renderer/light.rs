{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UnboxedTuples          #-}


module HulkanEngine3D.Render.Light where

import GHC.Generics (Generic)
import Control.Monad
import Data.IORef
import qualified Data.Text as Text
import Foreign.Storable

import Numeric.DataFrame
import Numeric.PrimBytes
import Graphics.Vulkan

import HulkanEngine3D.Render.TransformObject
import qualified HulkanEngine3D.Render.UniformBufferDatas as UniformBufferDatas
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.Math

data LightCreateInfo = DirectionalLightCreateInfo
    { _directionalLightPosition' :: Vec3f
    , _directionalLightRotation' :: Vec3f
    , _directionalLightConstants' :: UniformBufferDatas.LightConstants
    } deriving (Show, Generic)

instance PrimBytes LightCreateInfo

instance Storable LightCreateInfo where
    sizeOf _ = bSizeOf (undefined :: LightCreateInfo)
    alignment _ = bAlignOf (undefined :: LightCreateInfo)
    peek ptr = bPeek ptr
    poke ptr v = bPoke ptr v


data DirectionalLightData = DirectionalLightData
    { _directionalLightName :: IORef Text.Text
    , _directionalLightConstants :: IORef UniformBufferDatas.LightConstants
    , _directionalLightShadowProjection :: IORef Mat44f
    , _directionalLightTransformObject :: TransformObjectData
    , _directionalLightDataChanged :: IORef Bool
    } deriving (Show)


class LightInterface a where
    createLightData :: Text.Text -> LightCreateInfo -> IO a
    getLightConstants :: a -> IO UniformBufferDatas.LightConstants
    getLightPosition :: a -> IO Vec3f
    getLightDirection :: a -> IO Vec3f
    getLightColor :: a -> IO Vec3f
    getLightShadowSamples :: a -> IO (Scalar Int32)
    getLightShadowExp :: a -> IO (Scalar Float)
    getLightShadowBias :: a -> IO (Scalar Float)
    getShadowViewProjectionMatrix :: a -> IO Mat44f
    updateShadowOrthogonal :: a -> IO ()
    updateLightData :: a -> Vec3f -> IO ()

instance LightInterface DirectionalLightData where
    createLightData :: Text.Text -> LightCreateInfo -> IO DirectionalLightData
    createLightData name directionalLightCreateData = do
        logInfo $ "createLightData : " ++ Text.unpack name
        directionalLightName <- newIORef name
        directionalLightConstants <- newIORef (_directionalLightConstants' directionalLightCreateData)
        shadowOrthogonal <- newIORef matrix4x4_indentity
        transformObjectData <- newTransformObjectData
        dataChanged <- newIORef False
        let lightData = DirectionalLightData
                { _directionalLightName = directionalLightName
                , _directionalLightConstants = directionalLightConstants
                , _directionalLightShadowProjection = shadowOrthogonal
                , _directionalLightTransformObject = transformObjectData
                , _directionalLightDataChanged = dataChanged
                }
        setPosition transformObjectData (_directionalLightPosition' directionalLightCreateData)
        setRotation transformObjectData (_directionalLightRotation' directionalLightCreateData)
        updateShadowOrthogonal lightData
        updateLightData lightData float3_zero
        return lightData

    getLightConstants lightData = readIORef (_directionalLightConstants lightData)
    getLightPosition lightData = getPosition $ _directionalLightTransformObject lightData
    getLightDirection lightData = getFront $ _directionalLightTransformObject lightData
    getLightColor lightData = UniformBufferDatas._LIGHT_COLOR <$> readIORef (_directionalLightConstants lightData)
    getLightShadowSamples lightData = UniformBufferDatas._SHADOW_SAMPLES <$> readIORef (_directionalLightConstants lightData)
    getLightShadowExp lightData = UniformBufferDatas._SHADOW_EXP <$> readIORef (_directionalLightConstants lightData)
    getLightShadowBias lightData = UniformBufferDatas._SHADOW_BIAS <$> readIORef (_directionalLightConstants lightData)
    getShadowViewProjectionMatrix lightData = UniformBufferDatas._SHADOW_VIEW_PROJECTION <$> readIORef (_directionalLightConstants lightData)

    updateShadowOrthogonal :: DirectionalLightData -> IO ()
    updateShadowOrthogonal lightData@DirectionalLightData {..} = do
        directionalLightConstants <- readIORef _directionalLightConstants
        let (# width, height, near, far #) = unpackV4# (UniformBufferDatas._SHADOW_DIMENSIONS directionalLightConstants)
        writeIORef _directionalLightShadowProjection $ contract (orthogonal near far (width * 2.0) (height * 2.0)) clipSpaceMatrix
        writeIORef _directionalLightDataChanged True

    updateLightData :: DirectionalLightData -> Vec3f -> IO ()
    updateLightData lightData@DirectionalLightData {..} viewPosition = do
        updatedTransform <- updateTransformObject _directionalLightTransformObject
        dataChangedPrev <- readIORef _directionalLightDataChanged
        let dataChanged = True || dataChangedPrev || updatedTransform
            translationMatrix = translate3 (-viewPosition)
        when dataChanged $ do
            lightFront <- getLightDirection lightData
            inverseMatrix <- getInverseMatrix _directionalLightTransformObject
            shadowProjection <- readIORef _directionalLightShadowProjection
            modifyIORef _directionalLightConstants $ \directionalLightConstants -> directionalLightConstants
                { UniformBufferDatas._SHADOW_VIEW_PROJECTION = (contract (contract translationMatrix inverseMatrix) shadowProjection)
                , UniformBufferDatas._LIGHT_DIRECTION = lightFront
                }
        writeIORef _directionalLightDataChanged False


defaultDirectionalLightCreateInfo :: LightCreateInfo
defaultDirectionalLightCreateInfo = DirectionalLightCreateInfo
    { _directionalLightPosition' = vec3 0 0 0
    , _directionalLightRotation' = vec3 (-3.141592 * 0.5) 0 0
    , _directionalLightConstants' = UniformBufferDatas.defaultLightConstants
    }