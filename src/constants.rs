use std;
use ash::vk;

pub const ENGINE_NAME: &str = "RustEngine3D";
pub const ENGINE_VERSION: u32 = vk::make_version(0, 1, 0);
pub const API_VERSION: u32 = vk::make_version(1, 0, 0);

pub const DEBUG_MESSAGE_LEVEL: vk::DebugUtilsMessageSeverityFlagsEXT = vk::DebugUtilsMessageSeverityFlagsEXT::WARNING;
pub const VULKAN_LAYERS: [&str; 1] = ["VK_LAYER_LUNARG_standard_validation"];
//pub const VULKAN_LAYERS: [&str; 0] = [];
pub const REQUIRE_DEVICE_EXTENSIONS: [&str; 1] = ["VK_KHR_swapchain"];

//pub const DEPTH_FOMATS: [Format; 5] = [Format::D32Sfloat, Format::D32Sfloat_S8Uint, Format::D24Unorm_S8Uint, Format::D16Unorm_S8Uint, Format::D16Unorm];
//pub const DEPTH_STENCIL_FORMATS: [Format; 3] = [Format::D32Sfloat_S8Uint, Format::D24Unorm_S8Uint, Format::D16Unorm_S8Uint];

pub const INVALID_QUEUE_INDEX: u32 = std::u32::MAX;
pub const SWAPCHAIN_IMAGE_COUNT: u32 = 3;
pub const SWAPCHAIN_IMAGE_INDICES: [u32; SWAPCHAIN_IMAGE_COUNT as usize] = [0, 1, 2];
pub const SWAPCHAIN_IMAGE_FORMAT:vk::Format = vk::Format::B8G8R8A8_SRGB;
pub const SWAPCHAIN_COLOR_SPACE:vk::ColorSpaceKHR = vk::ColorSpaceKHR::SRGB_NONLINEAR;
pub const MAX_FRAME_COUNT: u32 = 2;
pub const FRAME_INDICES: [u32; MAX_FRAME_COUNT as usize] = [0, 1];
pub const MAX_DESCRIPTOR_POOL_ALLOC_COUNT: u32 = 100;
pub const ENABLE_IMMEDIATE_MODE: bool = true;
pub const ENABLE_VALIDATION_LAYER: bool = true;
pub const IS_CONCURRENT_MODE: bool = true;
pub const METER_PER_UNIT: f32 = 1.0;
pub const NEAR: f32 = 0.1;
pub const FAR: f32 = 2000.0;
pub const FOV: f32 = 60.0;
pub const CAMERA_MOVE_SPEED: f32 = 10.0;
pub const CAMERA_PAN_SPEED: f32 = 0.05;
pub const CAMERA_ROTATION_SPEED: f32 = 0.005;
pub const SHADOW_MAP_SIZE: i32 = 2048;
pub const SHADOW_SAMPLES: i32 = 4;
pub const SHADOW_EXP: f32 = 100.0;
pub const SHADOW_BIAS: f32 = 0.005;
pub const SHADOW_DISTANCE: f32 = 25.0;
pub const SHADOW_DEPTH: f32 = 50.0;
pub const SHADOW_UPDATE_DISTANCE: f32 = 10.0;
pub const SSAO_KERNEL_SIZE: i32 = 64;
pub const SSAO_RADIUS: f32 = 2.0;
pub const SSAO_NOISE_DIM: i32 = 4;

// -- NOTE : sync with scene_constants.glsl
enum RenderMode {
    RenderModeCommon,
    RenderModeShadow
}

enum RenderObjectType {
    RenderObjectStatic,
    RenderObjectSkeletal
}