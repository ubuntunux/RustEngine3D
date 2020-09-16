{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Render.ImageSampler
    ( ImageSamplers (..)
    , defaultImageSamplers
    , createImageSamplers
    , destroyImageSamplers
    ) where

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import HulkanEngine3D.Vulkan.Texture


data ImageSamplers = ImageSamplers
    { _imageSamplerPointClamp :: VkSampler
    , _imageSamplerLinearClamp :: VkSampler
    } deriving (Show)


defaultImageSamplers :: ImageSamplers
defaultImageSamplers = ImageSamplers
    { _imageSamplerPointClamp = VK_NULL
    , _imageSamplerLinearClamp = VK_NULL
    }

createImageSamplers :: VkDevice -> IO ImageSamplers
createImageSamplers device = do
    imageSamplerPointClamp <- createImageSampler device 0 VK_FILTER_NEAREST VK_FILTER_NEAREST VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE VK_FALSE
    imageSamplerLinearClamp <- createImageSampler device 0 VK_FILTER_LINEAR VK_FILTER_LINEAR VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE VK_FALSE
    return ImageSamplers
        { _imageSamplerPointClamp = imageSamplerPointClamp
        , _imageSamplerLinearClamp = imageSamplerLinearClamp
        }

destroyImageSamplers :: VkDevice -> ImageSamplers -> IO ()
destroyImageSamplers device imageSamplers = do
    destroyImageSampler device (_imageSamplerPointClamp imageSamplers)
    destroyImageSampler device (_imageSamplerLinearClamp imageSamplers)