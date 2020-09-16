module HulkanEngine3D.Constants where

import Graphics.Vulkan
import Graphics.Vulkan.Ext.VK_KHR_swapchain

engineName :: String
engineName = "HulkanEngine3D"

vulkanLayers :: [String]
vulkanLayers = ["VK_LAYER_LUNARG_standard_validation"]

requireDeviceExtensions :: [CString]
requireDeviceExtensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

depthFomats :: [VkFormat]
depthFomats = [VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT, VK_FORMAT_D16_UNORM_S8_UINT, VK_FORMAT_D16_UNORM]

depthStencilFormats :: [VkFormat]
depthStencilFormats = [VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]

invalidQueueIndex :: Word32
invalidQueueIndex = maxBound

swapChainImageCount :: Int
swapChainImageCount = 3

swapChainImageIndices :: [Int]
swapChainImageIndices = [0..(swapChainImageCount-1)]

swapChainImageFormat :: VkFormat
swapChainImageFormat = VK_FORMAT_B8G8R8A8_SRGB

maxFrameCount :: Int
maxFrameCount = 2

descriptorSetCountAtOnce :: Int
descriptorSetCountAtOnce = swapChainImageCount

maxDescriptorPoolAllocCount :: Int
maxDescriptorPoolAllocCount = 100

enableImmediateMode :: Bool
enableImmediateMode = True

enableValidationLayer :: Bool
enableValidationLayer = True

isConcurrentMode :: Bool
isConcurrentMode = True

meterPerUnit :: Float
meterPerUnit = 1.0::Float

near :: Float
near = 0.1

far :: Float
far = 2000.0

fov :: Float
fov = 60.0

cameraMoveSpeed :: Float
cameraMoveSpeed = 10.0

cameraPanSpeed :: Float
cameraPanSpeed = 0.05

cameraRotationSpeed :: Float
cameraRotationSpeed = 0.005

shadowMapSize :: Word32
shadowMapSize = 2048

shadowSamples :: Int
shadowSamples = 4

shadowExp :: Float
shadowExp = 100.0

shadowBias :: Float
shadowBias = 0.005

shadowDistance :: Float
shadowDistance = 25.0

shadowDepth :: Float
shadowDepth = 50.0

shadowUpdateDistance :: Float
shadowUpdateDistance = 10.0

_SSAO_KERNEL_SIZE :: Int
_SSAO_KERNEL_SIZE = 64

_SSAO_RADIUS :: Float
_SSAO_RADIUS = 2.0

_SSAO_NOISE_DIM :: Int
_SSAO_NOISE_DIM = 4

-- NOTE : sync with scene_constants.glsl
data RenderMode = RenderMode_Common | RenderMode_Shadow deriving (Eq, Enum, Ord, Show, Read)
data RenderObjectType = RenderObject_Static | RenderObject_Skeletal deriving (Eq, Enum, Ord, Show, Read)