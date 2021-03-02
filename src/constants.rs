use std;
use ash::vk;

pub const ENGINE_NAME: &str = "RustEngine3D";
pub const ENGINE_VERSION: u32 = vk::make_version(1, 0, 0);
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
pub const SWAPCHAIN_IMAGE_COUNT: usize = 3;
pub const SWAPCHAIN_IMAGE_INDICES: [usize; SWAPCHAIN_IMAGE_COUNT] = [0, 1, 2];
pub const SWAPCHAIN_SURFACE_FORMATS: [vk::SurfaceFormatKHR; 2] = [
    vk::SurfaceFormatKHR { format: vk::Format::R8G8B8A8_SRGB, color_space: vk::ColorSpaceKHR::SRGB_NONLINEAR },
    vk::SurfaceFormatKHR { format: vk::Format::B8G8R8A8_SRGB, color_space: vk::ColorSpaceKHR::SRGB_NONLINEAR },
];
pub const MAX_FRAME_COUNT: usize = 2;
pub const FRAME_INDICES: [usize; MAX_FRAME_COUNT] = [0, 1];

// application configs
pub static mut VULKAN_API_VERSION: u32 = vk::make_version(1, 2, 0);
pub static mut DEBUG_MESSAGE_LEVEL: vk::DebugUtilsMessageSeverityFlagsEXT = vk::DebugUtilsMessageSeverityFlagsEXT::WARNING;
pub static mut VULKAN_LAYERS: Vec<String> = Vec::new(); // vec!["VK_LAYER_LUNARG_standard_validation".to_string()];
pub static mut REQUIRE_DEVICE_EXTENSIONS: Vec<String> = Vec::new(); // vec!["VK_KHR_swapchain".to_string()];
pub static mut MAX_DESCRIPTOR_POOL_ALLOC_COUNT: usize = 512;
pub static mut ENABLE_IMMEDIATE_MODE: bool = true;
pub static mut ENABLE_VALIDATION_LAYER: bool = true;
pub static mut IS_CONCURRENT_MODE: bool = true;
pub static mut METER_PER_UNIT: f32 = 1.0;
pub static mut NEAR: f32 = 0.1;
pub static mut FAR: f32 = 2000.0;
pub static mut FOV: f32 = 60.0;
pub static mut MAX_FONT_INSTANCE_COUNT: usize = 1024; // must match with render_font_common.glsl
pub static mut MAX_UI_INSTANCE_COUNT: usize = 1024; // must match with render_ui_common.glsl
pub static mut SHADOW_MAP_SIZE: u32 = 2048;
pub static mut SHADOW_SAMPLES: i32 = 4;
pub static mut SHADOW_EXP: f32 = 100.0;
pub static mut SHADOW_BIAS: f32 = 0.005;
pub static mut SHADOW_DISTANCE: f32 = 50.0;
pub static mut SHADOW_DEPTH: f32 = 50.0;
pub static mut SSAO_NOISE_DIM: i32 = 4;

pub const PRECOMPUTED_ROOT_MATRIX: bool = true; // precompute bone animation matrix with ancestor bone matrices.
pub const PRECOMPUTED_COMBINE_INV_BIND_MATRIX: bool = PRECOMPUTED_ROOT_MATRIX && false; // combine animation matrix with inv_bind_matrix.