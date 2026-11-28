use std::sync::{OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::renderer::renderer_data::{ColorGradingLut, RenderOption, RenderQualityLevel, ToneMappingMode};
use ash::vk;

pub const ENGINE_NAME: &str = "RustEngine3D";
pub const ENGINE_VERSION: u32 = vk::make_api_version(0, 1, 0, 0);
pub const DEPTH_FORMATS: [vk::Format; 2] = [vk::Format::D32_SFLOAT, vk::Format::D16_UNORM];
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
pub const SWAPCHAIN_IMAGE_COUNT: usize = 4;
pub const SWAPCHAIN_IMAGE_INDICES: [usize; SWAPCHAIN_IMAGE_COUNT] = [0, 1, 2, 3];
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

// Fixed timestep defaults
pub const DEFAULT_FIXED_TIMESTEP: f64 = 1.0 / 60.0;
pub const DEFAULT_MAX_FIXED_STEPS_PER_FRAME: u32 = 5;

pub const DISTANCE_CULL_FACTOR: f32 = 3500.0;
pub const MAX_BOUND_BOX_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const MAX_DEBUG_LINE_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const MAX_FONT_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const MAX_UI_INSTANCE_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const UPSCALE_FACTOR: f32 = 0.75;
pub const RCAS_SHARPNESS_NATIVE: f32 = 0.25;
pub const RCAS_SHARPNESS_UPSCALE: f32 = 0.45;
pub const SSAO_KERNEL_SIZE: usize = 64; // NOTE: Ensure enum values match in scene_constants.glsl
pub const SSAO_RADIUS: f32 = 2.0;
pub const MAX_POINT_LIGHTS: usize = 32; // NOTE: Ensure enum values match in scene_constants.glsl
pub const LIGHT_GRID_DIMENSION: [usize; 3] = [10, 10, 10];
pub const LIGHT_GRID_CELL_COUNT: usize = LIGHT_GRID_DIMENSION[0] * LIGHT_GRID_DIMENSION[1] * LIGHT_GRID_DIMENSION[2];
pub const MAX_LIGHT_COUNT_PER_CELL: usize = 4;
pub const LIGHT_PROBE_SIZE: u32 = 256;
pub const RENDER_OBJECT_FOR_LIGHT_PROBE: bool = false;
pub const MAX_TRANSFORM_COUNT: usize = 65536; // NOTE: Ensure enum values match in scene_constants.glsl
pub const COMBINED_INVERSE_BIND_MATRIX: bool = false; // combine animation matrix with inv_bind_matrix.
pub const CONVERT_COORDINATE_SYSTEM_RIGHT_HANDED_TO_LEFT_HANDED: bool = true;
pub const INSTANCING_BLOCK_SIZE: f32 = 30.0;
pub const COLLISION_BLOCK_SIZE: f32 = 10.0;

// application
pub const JOYSTICK_SENSOR_DEAD_ZONE: i16 = 10_000;

// effect
pub const PROCESS_GPU_PARTICLE_WORK_GROUP_SIZE: i32 = 64;

// audio
pub const DEFAULT_AUDIO_VOLUME: i32 = 32;
pub const MAX_AUDIO_VOLUME: i32 = 128;
pub const MAX_AUDIO_CHANNEL_COUNT: i32 = 128;

#[derive(Clone, Debug)]
pub struct EngineConfig {
    pub _development: bool,
    pub _show_debug_text: bool,
    pub _vulkan_api_version: u32,
    pub _debug_message_level: vk::DebugUtilsMessageSeverityFlagsEXT,
    pub _required_instance_layers: Vec<String>,
    pub _required_device_extensions: Vec<String>,
    pub _use_ray_tracing: bool,
    pub _required_ray_tracing_extensions: Vec<String>,
    pub _max_descriptor_pool_alloc_count: usize,
    pub _enable_immediate_mode: bool,
    pub _target_frame_rate: u32,
    pub _enable_dynamic_target_fps: bool,
    pub _ignore_monitor_refresh_rate: bool,
    pub _is_concurrent_mode: bool,
    pub _shader_clip_distance: bool,
    pub _meter_per_unit: f32,
    pub _near: f32,
    pub _far: f32,
    pub _fov: f32,
    pub _shadow_map_size: u32,
    pub _shadow_samples: i32,
    pub _shadow_distance: f32,
    pub _shadow_depth: f32,
    pub _shadow_depth_bias: f32,
    pub _shadow_depth_slope_bias: f32,
    pub _shadow_update_distance: f32,
    pub _ssao_noise_dim: i32,
    pub _capture_height_map_size: u32,
    pub _enable_upscale: bool,
    pub _render_quality_level: RenderQualityLevel,
    pub _tone_mapping_mode: ToneMappingMode,
    pub _color_grading_lut: ColorGradingLut,
    pub _exposure: f32,
    pub _dof_intensity: f32,
    pub _bloom_intensity: f32,
    pub _god_rays_intensity: f32,
    pub _height_fog_intensity: f32,
    pub _motion_blur_intensity: f32,
    pub _current_render_option: u32,
    pub _render_bound_box: bool,
    pub _max_emitter_count: i32,
    pub _max_particle_count: i32,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            _development: false,
            _show_debug_text: false,
            _vulkan_api_version: vk::make_api_version(0, 1, 3, 0),
            _debug_message_level: vk::DebugUtilsMessageSeverityFlagsEXT::empty(),
            _required_instance_layers: Vec::new(),
            _required_device_extensions: Vec::new(),
            _use_ray_tracing: false,
            _required_ray_tracing_extensions: Vec::new(),
            _max_descriptor_pool_alloc_count: 1024,
            _enable_immediate_mode: true,
            _target_frame_rate: 0,
            _enable_dynamic_target_fps: false,
            _ignore_monitor_refresh_rate: true,
            _is_concurrent_mode: true,
            _shader_clip_distance: true,
            _meter_per_unit: 1.0,
            _near: 0.1,
            _far: 200000.0,
            _fov: 60.0,
            _shadow_map_size: 2048,
            _shadow_samples: 12,
            _shadow_distance: 100.0,
            _shadow_depth: 10000.0,
            _shadow_depth_bias: 0.0,
            _shadow_depth_slope_bias: 2.5,
            _shadow_update_distance: 10.0,
            _ssao_noise_dim: 4,
            _capture_height_map_size: 512,
            _enable_upscale: false,
            _render_quality_level: RenderQualityLevel::Medium,
            _tone_mapping_mode: ToneMappingMode::AgX,
            _color_grading_lut: ColorGradingLut::None,
            _exposure: 0.90,
            _dof_intensity: 1.0,
            _bloom_intensity: 1.0,
            _god_rays_intensity: 1.0,
            _height_fog_intensity: 1.0,
            _motion_blur_intensity: 1.0,
            _current_render_option: RenderOption::ALL as u32,
            _render_bound_box: false,
            _max_emitter_count: 1024,
            _max_particle_count: 262144,
        }
    }
}

static ENGINE_CONFIG: OnceLock<RwLock<EngineConfig>> = OnceLock::new();

fn engine_config_lock() -> &'static RwLock<EngineConfig> {
    ENGINE_CONFIG.get_or_init(|| RwLock::new(EngineConfig::default()))
}

pub fn get_engine_config() -> RwLockReadGuard<'static, EngineConfig> {
    engine_config_lock().read().unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub fn get_engine_config_mut() -> RwLockWriteGuard<'static, EngineConfig> {
    engine_config_lock().write().unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub fn with_engine_config<R>(f: impl FnOnce(&EngineConfig) -> R) -> R {
    let guard = get_engine_config();
    f(&guard)
}

pub fn with_engine_config_mut<R>(f: impl FnOnce(&mut EngineConfig) -> R) -> R {
    let mut guard = get_engine_config_mut();
    f(&mut guard)
}

pub fn is_development() -> bool {
    get_engine_config()._development
}

pub fn set_development(value: bool) {
    get_engine_config_mut()._development = value;
}

pub fn is_show_debug_text() -> bool {
    get_engine_config()._show_debug_text
}

pub fn set_show_debug_text(value: bool) {
    get_engine_config_mut()._show_debug_text = value;
}

pub fn is_enable_upscale() -> bool {
    get_engine_config()._enable_upscale
}

pub fn set_enable_upscale(value: bool) {
    get_engine_config_mut()._enable_upscale = value;
}

pub fn is_render_bound_box() -> bool {
    get_engine_config()._render_bound_box
}

pub fn set_render_bound_box(value: bool) {
    get_engine_config_mut()._render_bound_box = value;
}

pub fn is_enable_dynamic_target_fps() -> bool {
    get_engine_config()._enable_dynamic_target_fps
}

pub fn set_enable_dynamic_target_fps(value: bool) {
    get_engine_config_mut()._enable_dynamic_target_fps = value;
}

pub fn get_max_emitter_count() -> i32 {
    get_engine_config()._max_emitter_count
}

pub fn get_max_particle_count() -> i32 {
    get_engine_config()._max_particle_count
}

pub fn get_shadow_map_size() -> u32 {
    get_engine_config()._shadow_map_size
}

pub fn get_capture_height_map_size() -> u32 {
    get_engine_config()._capture_height_map_size
}

pub fn get_render_quality_level() -> RenderQualityLevel {
    get_engine_config()._render_quality_level
}

pub fn get_render_viewport_size(window_width: u32, window_height: u32) -> (u32, u32) {
    if is_enable_upscale() {
        let w = (((window_width as f32 * UPSCALE_FACTOR) as u32) / 4) * 4;
        let h = (((window_height as f32 * UPSCALE_FACTOR) as u32) / 4) * 4;
        (w.max(4), h.max(4))
    } else {
        (window_width.max(1), window_height.max(1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_config_defaults_and_viewport_scaling() {
        let cfg = EngineConfig::default();
        assert_eq!(cfg._shadow_map_size, 2048);
        assert_eq!(cfg._max_emitter_count, 1024);
        assert_eq!(cfg._max_particle_count, 262144);
        assert!(!cfg._development);
        assert!(!cfg._enable_upscale);
    }

    #[test]
    fn test_engine_config_thread_safe_read_write() {
        let prev = is_render_bound_box();
        set_render_bound_box(!prev);
        assert_eq!(is_render_bound_box(), !prev);
        set_render_bound_box(prev);
        assert_eq!(is_render_bound_box(), prev);
    }
}
