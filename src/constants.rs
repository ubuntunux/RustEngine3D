use std;
use ash::vk;

pub const ENGINE_NAME: &str = "RustEngine3D";
pub const ENGINE_VERSION: u32 = vk::make_version(1, 0, 0);
#[cfg(target_os = "android")]
pub const VULKAN_API_VERSION: u32 = vk::make_version(1, 0, 0);
#[cfg(not(target_os = "android"))]
pub const VULKAN_API_VERSION: u32 = vk::make_version(1, 2, 0);
pub const DEBUG_MESSAGE_LEVEL: vk::DebugUtilsMessageSeverityFlagsEXT = vk::DebugUtilsMessageSeverityFlagsEXT::WARNING;
pub const VULKAN_LAYERS: [&str; 1] = ["VK_LAYER_LUNARG_standard_validation"];
pub const REQUIRE_DEVICE_EXTENSIONS: [&str; 1] = ["VK_KHR_swapchain"];
pub const DEPTH_FOMATS: [vk::Format; 5] = [
    vk::Format::D32_SFLOAT,
    vk::Format::D32_SFLOAT_S8_UINT,
    vk::Format::D24_UNORM_S8_UINT,
    vk::Format::D16_UNORM_S8_UINT,
    vk::Format::D16_UNORM
];
pub const DEPTH_STENCIL_FORMATS: [vk::Format; 3] = [
    vk::Format::D32_SFLOAT_S8_UINT,
    vk::Format::D24_UNORM_S8_UINT,
    vk::Format::D16_UNORM_S8_UINT
];
pub const CUBE_LAYER_COUNT: usize = 6;
pub const CUBE_TEXTURE_FACES: [&str; CUBE_LAYER_COUNT] = ["right", "left", "top", "bottom", "front", "back"];
pub const INVALID_QUEUE_INDEX: u32 = std::u32::MAX;
pub const WHOLE_LAYERS: u32 = std::u32::MAX;
pub const WHOLE_MIP_LEVELS: u32 = std::u32::MAX;
#[cfg(target_os = "android")]
pub const SWAPCHAIN_IMAGE_COUNT: usize = 3;
#[cfg(target_os = "android")]
pub const SWAPCHAIN_IMAGE_INDICES: [usize; SWAPCHAIN_IMAGE_COUNT] = [0, 1, 2];
#[cfg(not(target_os = "android"))]
pub const SWAPCHAIN_IMAGE_COUNT: usize = 3; // Default: 3
#[cfg(not(target_os = "android"))]
pub const SWAPCHAIN_IMAGE_INDICES: [usize; SWAPCHAIN_IMAGE_COUNT] = [0, 1, 2];
pub const SWAPCHAIN_IMAGE_FORMAT:vk::Format = vk::Format::B8G8R8A8_SRGB;
pub const SWAPCHAIN_COLOR_SPACE:vk::ColorSpaceKHR = vk::ColorSpaceKHR::SRGB_NONLINEAR;
pub const MAX_FRAME_COUNT: usize = 2;
pub const FRAME_INDICES: [usize; MAX_FRAME_COUNT] = [0, 1];
pub const MAX_DESCRIPTOR_POOL_ALLOC_COUNT: usize = 100;
#[cfg(target_os = "android")]
pub const ENABLE_IMMEDIATE_MODE: bool = false;
#[cfg(not(target_os = "android"))]
pub const ENABLE_IMMEDIATE_MODE: bool = true;
#[cfg(target_os = "android")]
pub const ENABLE_VALIDATION_LAYER: bool = false;
#[cfg(not(target_os = "android"))]
pub const ENABLE_VALIDATION_LAYER: bool = true;
#[cfg(target_os = "android")]
pub const IS_CONCURRENT_MODE: bool = false;
#[cfg(not(target_os = "android"))]
pub const IS_CONCURRENT_MODE: bool = true;
pub const METER_PER_UNIT: f32 = 1.0;
pub const NEAR: f32 = 0.1;
pub const FAR: f32 = 2000.0;
pub const FOV: f32 = 60.0;
pub const CAMERA_MOVE_SPEED: f32 = 10.0;
pub const CAMERA_PAN_SPEED: f32 = 0.05;
pub const CAMERA_ROTATION_SPEED: f32 = 0.005;
pub const SHADOW_MAP_SIZE: u32 = 2048;
pub const SHADOW_SAMPLES: i32 = 4;
pub const SHADOW_EXP: f32 = 100.0;
pub const SHADOW_BIAS: f32 = 0.005;
pub const SHADOW_DISTANCE: f32 = 25.0;
pub const SHADOW_DEPTH: f32 = 50.0;
pub const SHADOW_UPDATE_DISTANCE: f32 = 10.0;
// SSAO_KERNEL_SIZE must match with scene_constants.glsl
pub const SSAO_KERNEL_SIZE: usize = 64;
pub const SSAO_RADIUS: f32 = 2.0;
pub const SSAO_NOISE_DIM: i32 = 4;
// MAX_BONES must match with scene_constants.glsl
pub const MAX_BONES: usize = 128 * 128;
pub const PRECOMPUTED_ROOT_MATRIX: bool = true; // precompute bone animation matrix with ancestor bone matrices.
pub const PRECOMPUTED_COMBINE_INV_BIND_MATRIX: bool = PRECOMPUTED_ROOT_MATRIX && false; // combine animation matrix with inv_bind_matrix.
pub const LIGHT_PROBE_SIZE: u32 = 256;