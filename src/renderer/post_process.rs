{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HulkanEngine3D.Render.PostProcess where

import Control.Monad
import System.Random

import Numeric.DataFrame
import Numeric.Dimensions

import qualified HulkanEngine3D.Constants as Constants


data PostProcessData
    = PostProcessData_SSAO
        { _ssao_kernel_size :: {-# UNPACK #-} !Int
        , _ssao_radius :: {-# UNPACK #-} !Float
        , _ssao_noise_dim :: {-# UNPACK #-} !Int
        , _ssao_kernel_samples :: DataFrame Float '[64, 4]
        }
    deriving (Eq, Show)


initializePostProcessData_SSAO :: IO PostProcessData
initializePostProcessData_SSAO = do
    randomNormals <- getRandomNormals Constants._SSAO_KERNEL_SIZE
    return PostProcessData_SSAO
        { _ssao_kernel_size = Constants._SSAO_KERNEL_SIZE
        , _ssao_radius = Constants._SSAO_RADIUS
        , _ssao_noise_dim = Constants._SSAO_NOISE_DIM
        , _ssao_kernel_samples = iwgen @Float @'[64] @'[4] (\(Idx i:*U) -> randomNormals !! fromIntegral i)
        }
    where
        getRandomNormals :: Int -> IO [Vec4f]
        getRandomNormals count = replicateM count $ do
            x <- randomIO :: IO Float
            y <- randomIO :: IO Float
            z <- randomIO :: IO Float
            scale <- randomIO :: IO Float
            pure . toHomVector $ (normalized (vec3 (x * 2.0 - 1.0) (y * 0.5 + 0.5) (z * 2.0 - 1.0))) * vec3 scale scale scale
