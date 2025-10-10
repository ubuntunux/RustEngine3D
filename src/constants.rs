use ash::vk;

pub static mut DEVELOPMENT: bool = false;
pub const ENGINE_NAME: &str = "RustEngine3D";
pub const ENGINE_VERSION: u32 = vk::make_api_version(0, 1, 0, 0);
pub const DEPTH_FORMATS: [vk::Format; 2] = [
    vk::Format::D32_SFLOAT,
    vk::Format::D16_UNORM,
];
pub const DEPTH_STENCIL_FORMATS: [vk::Format; 3] = [
    vk::Format::D32_SFLOAT_S8_UINT,
    vk::Format::D24_UNORM_S8_UINT,
    vk::Format::D16_UNORM_S8_UINT,
];
pub const CUBE_LAYER_COUNT: usize = 6;
pub const CUBE_TEXTURE_FACES: [&str; CUBE_LAYER_COUNT] = ["right", "left", "top", "bottom", "front", "back"];
pub const INVALID_QUEUE_INDEX: u32 = u32::MAX;
pub const WHOLE_LAYERS: u32 = u32::MAX;
pub const WHOLE_MIP_LEVELS: u32 = u32::MAX;
pub const SWAPCHAIN_IMAGE_COUNT: usize = 3;
pub const SWAPCHAIN_IMAGE_INDICES: [usize; SWAPCHAIN_IMAGE_COUNT] = [0, 1, 2];
pub const SWAPCHAIN_SURFACE_FORMATS: [vk::SurfaceFormatKHR; 2] = [
    vk::SurfaceFormatKHR {
        format: vk::Format::R8G8B8A8_SRGB,
        color_space: vk::ColorSpaceKHR::SRGB_NONLINEAR,
    },
    vk::SurfaceFormatKHR {
        format: vk::Format::B8G8R8A8_SRGB,
        color_space: vk::ColorSpaceKHR::SRGB_NONLINEAR,
    },
];
pub const MAX_FRAME_COUNT: u64 = 2;
pub const FRAME_INDICES: [u64; MAX_FRAME_COUNT as usize] = [0, 1];

// application configs - default values
pub static mut VULKAN_API_VERSION: u32 = vk::make_api_version(0, 1, 3, 0);
pub static mut DEBUG_MESSAGE_LEVEL: vk::DebugUtilsMessageSeverityFlagsEXT = vk::DebugUtilsMessageSeverityFlagsEXT::empty();
pub static mut REQUIRED_INSTANCE_LAYERS: Vec<String> = Vec::new();
pub static mut REQUIRED_DEVICE_EXTENSIONS: Vec<String> = Vec::new();
pub static mut USE_RAY_TRACING: bool = false;
pub static mut REQUIRED_RAY_TRACING_EXTENSIONS: Vec<String> = Vec::new();
pub static mut MAX_DESCRIPTOR_POOL_ALLOC_COUNT: usize = 1024;
pub static mut ENABLE_IMMEDIATE_MODE: bool = true;
pub static mut IS_CONCURRENT_MODE: bool = true;
pub static mut SHADER_CLIP_DISTANCE: bool = true;
pub static mut METER_PER_UNIT: f32 = 1.0;
pub static mut NEAR: f32 = 0.1;
pub static mut FAR: f32 = 200000.0;
pub static mut FOV: f32 = 60.0;
pub static MAX_BOUND_BOX_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub static MAX_DEBUG_LINE_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub static MAX_FONT_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub static MAX_UI_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub static mut SHADOW_MAP_SIZE: u32 = 2048;
pub static mut SHADOW_SAMPLES: i32 = 12;
pub static mut SHADOW_DISTANCE: f32 = 100.0;
pub static mut SHADOW_DEPTH: f32 = 10000.0;
pub static mut SHADOW_DEPTH_BIAS: f32 = 0.0;
pub static mut SHADOW_DEPTH_SLOPE_BIAS: f32 = 2.5;
pub static mut SHADOW_UPDATE_DISTANCE: f32 = 10.0;
pub static mut SSAO_NOISE_DIM: i32 = 4;
pub static mut CAPTURE_HEIGHT_MAP_SIZE: u32 = 512;
pub static mut ENABLE_UPSCALE: bool = false;
pub const SSAO_KERNEL_SIZE: usize = 64; // NOTE: Ensure enum values match in scene_constants.glsl
pub const SSAO_RADIUS: f32 = 2.0;
pub const MAX_POINT_LIGHTS: usize = 32; // NOTE: Ensure enum values match in scene_constants.glsl
pub const LIGHT_GRID_DIMENSION: [usize; 3] = [ 10, 10, 10 ];
pub const LIGHT_GRID_CELL_COUNT: usize = LIGHT_GRID_DIMENSION[0] * LIGHT_GRID_DIMENSION[1] * LIGHT_GRID_DIMENSION[2];
pub const MAX_LIGHT_COUNT_PER_CELL: usize = 4;
pub const LIGHT_PROBE_SIZE: u32 = 256;
pub const RENDER_OBJECT_FOR_LIGHT_PROBE: bool = false;
pub const MAX_TRANSFORM_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const COMBINED_INVERSE_BIND_MATRIX: bool = false; // combine animation matrix with inv_bind_matrix.
pub const CONVERT_COORDINATE_SYSTEM_RIGHT_HANDED_TO_LEFT_HANDED: bool = true;
pub const INSTANCING_BLOCK_SIZE: f32 = 30.0;
pub const COLLISION_BLOCK_SIZE: f32 = 10.0;

// render option
pub static mut RENDER_OCEAN: bool = true;
pub static mut RENDER_BOUND_BOX: bool = false;

// application
pub const JOYSTICK_SENSOR_DEAD_ZONE: i16 = 10_000;

// effect
pub static mut MAX_EMITTER_COUNT: i32 = 1024;
pub static mut MAX_PARTICLE_COUNT: i32 = 262144;
pub const PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE: i32 = 64;

// audio
pub const DEFAULT_AUDIO_VOLUME: i32 = 32;
pub const MAX_AUDIO_CHANNEL_COUNT: i32 = 128;
