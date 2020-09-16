{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.TextureGenerator where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Storable as SVector
import qualified Data.Text as Text ()
import System.Directory
import System.FilePath.Posix
import System.Random

import Graphics.Vulkan.Core_1_0
import qualified Codec.Picture as Image
import qualified Codec.Picture.Types as Image

import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Render.PostProcess as PostProcess
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import qualified HulkanEngine3D.Vulkan.Vulkan as Vulkan
import qualified HulkanEngine3D.Utilities.Vector as Vector


generateFlatColorImageRGBA8 :: Int -> Int -> (Word8, Word8, Word8, Word8) -> Image.Image Image.PixelRGBA8
generateFlatColorImageRGBA8 imageWidth imageHeight (r, g, b, a) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBA8 r g b a)
                go (x + 1) y
      go 0 0

generateFlatColorImageRGBF :: Int -> Int -> (Float, Float, Float) -> Image.Image Image.PixelRGBF
generateFlatColorImageRGBF imageWidth imageHeight (r, g, b) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBF r g b)
                go (x + 1) y
      go 0 0

generateFlatColorImageRGBA16 :: Int -> Int -> (Word16, Word16, Word16, Word16) -> Image.Image Image.PixelRGBA16
generateFlatColorImageRGBA16 imageWidth imageHeight (r, g, b, a) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBA16 r g b a)
                go (x + 1) y
      go 0 0

generateImages :: Renderer.RendererData -> FilePath -> IO ()
generateImages rendererData textureFilePath = do
    ifNotExistSaveImage textureFilePath "common/flat_none.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 0, 0))
    ifNotExistSaveImage textureFilePath "common/flat_black.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_gray.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (128, 128, 128, 255))
    ifNotExistSaveImage textureFilePath "common/flat_white.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (255, 255, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_red.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (255, 0, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_green.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 255, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_blue.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_normal.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (128, 128, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_normal_f32.png" (Image.ImageRGBF $ generateFlatColorImageRGBF 2 2 (0.5, 0.5, 1.0))
    ifNotExistSaveImage textureFilePath "common/flat_normal_u16.png" (Image.ImageRGBA16 $ generateFlatColorImageRGBA16 2 2 (32767, 32767, 65535, 65535))
    where
        ifNotExistSaveImage :: FilePath -> FilePath -> Image.DynamicImage -> IO ()
        ifNotExistSaveImage textureFilePath imageFileName image = do
            let imageFilePath = combine textureFilePath imageFileName
            doesFileExist imageFilePath >>= \result -> when (not result) $ do
                createDirectoryIfMissing True (takeDirectory imageFilePath)
                Image.savePngImage imageFilePath image

generateRandomNormals :: Int -> Int -> IO [Vector.Vec4]
generateRandomNormals width height = do
    replicateM (width * height) $ do
        x <- randomIO :: IO Float
        y <- randomIO :: IO Float
        z <- randomIO :: IO Float
        pure $ Vector.Vec4 (x * 2.0 - 1.0) (y * 2.0 - 1.0) z 1.0


generateTextures :: Renderer.RendererData -> IO [Texture.TextureData]
generateTextures rendererData = do
    let postprocess_ssao = (Renderer._postprocess_ssao rendererData)
        ssao_noise_dim = PostProcess._ssao_noise_dim postprocess_ssao
        white = Vulkan.getColor32 255 255 255 255
        black = Vulkan.getColor32 0 0 0 255
        red = Vulkan.getColor32 255 0 0 255
        green = Vulkan.getColor32 0 255 0 255
        blue = Vulkan.getColor32 0 0 255 255
        yellow = Vulkan.getColor32 255 255 0 255

    randomNormals <- SVector.fromList <$> (generateRandomNormals ssao_noise_dim ssao_noise_dim)
    textureRandomNormal <- Renderer.createTexture rendererData "random_normal" Texture.defaultTextureCreateInfo
        { Texture._textureCreateInfoWidth = fromIntegral ssao_noise_dim
        , Texture._textureCreateInfoHeight = fromIntegral ssao_noise_dim
        , Texture._textureCreateInfoFormat = VK_FORMAT_R32G32B32A32_SFLOAT
        , Texture._textureCreateInfoData = SVector.unsafeCast randomNormals
        }
    textureCheck <- Renderer.createTexture rendererData "checker" Texture.defaultTextureCreateInfo
        { Texture._textureCreateInfoWidth = 2
        , Texture._textureCreateInfoHeight = 2
        , Texture._textureCreateInfoViewType = VK_IMAGE_VIEW_TYPE_2D
        , Texture._textureCreateInfoMinFilter = VK_FILTER_NEAREST
        , Texture._textureCreateInfoMagFilter = VK_FILTER_NEAREST
        , Texture._textureCreateInfoData = SVector.unsafeCast $
            SVector.fromList [ white, black, black, white ]
        }
    textureColorCube <- Renderer.createTexture rendererData "color_cube" Texture.defaultTextureCreateInfo
            { Texture._textureCreateInfoWidth = 1
            , Texture._textureCreateInfoHeight = 1
            , Texture._textureCreateInfoViewType = VK_IMAGE_VIEW_TYPE_CUBE
            , Texture._textureCreateInfoData = SVector.unsafeCast $
                SVector.fromList [ white, black, red, green, blue, yellow ]
            }
    return [ textureRandomNormal, textureCheck, textureColorCube ]