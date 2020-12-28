use ash::{ vk };
use nalgebra::{
    Vector2,
    Vector4,
    Matrix4,
};

pub const NONE_PUSH_CONSTANT: Option<&()> = None;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_StaticRenderObject {
    pub _local_matrix: Matrix4<f32>,
}

impl Default for PushConstant_StaticRenderObject {
    fn default() -> PushConstant_StaticRenderObject {
        PushConstant_StaticRenderObject {
            _local_matrix: Matrix4::identity(),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_SkeletalRenderObject {
    pub _local_matrix: Matrix4<f32>,
    pub _bone_matrix_offset: u32,
    pub _bone_matrix_count: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstant_SkeletalRenderObject {
    fn default() -> PushConstant_SkeletalRenderObject {
        PushConstant_SkeletalRenderObject {
            _local_matrix: Matrix4::identity(),
            _bone_matrix_offset: 0,
            _bone_matrix_count: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_BloomHighlight {
    pub _bloom_threshold_min: f32,
    pub _bloom_threshold_max: f32,
    pub _bloom_intensity: f32,
    pub _bloom_scale: f32,
}

impl Default for PushConstant_BloomHighlight {
    fn default() -> PushConstant_BloomHighlight {
        PushConstant_BloomHighlight {
            _bloom_threshold_min: 1.25,
            _bloom_threshold_max: 10.0,
            _bloom_intensity: 0.25,
            _bloom_scale: 1.0,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_GaussianBlur {
    pub _blur_scale: Vector2<f32>,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstant_GaussianBlur {
    fn default() -> PushConstant_GaussianBlur {
        PushConstant_GaussianBlur {
            _blur_scale: Vector2::new(1.0, 1.0),
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderCopy {
    pub _taget_mip_level: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl Default for PushConstant_RenderCopy {
    fn default() -> PushConstant_RenderCopy {
        PushConstant_RenderCopy {
            _taget_mip_level: 0,
            _reserved0: 0,
            _reserved1: 0,
            _reserved2: 0,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderColor {
    pub _color: Vector4<f32>
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_FFT_Waves {
    pub _pass: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_FFT_Init {
    pub _inverse_grid_sizes: [f32; 4],
    pub _fft_size: f32,
    pub _t: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_FFT_Variance {
    pub _grid_sizes: [f32; 4],
    pub _n_slope_variance: f32,
    pub _fft_size: u32,
    pub _slope_variance_delta: f32,
    pub _c: f32,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_FFT_Ocean {
    pub _simulation_size: Vector4<f32>,
    pub _cell_size: Vector2<f32>,
    pub _height: f32,
    pub _simulation_wind: f32,
    pub _simulation_amplitude: f32,
    pub _t: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderDebug {
    pub _debug_target: u32,
    pub _mip_level: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
}

impl Default for PushConstant_RenderDebug {
    fn default() -> PushConstant_RenderDebug {
        PushConstant_RenderDebug {
            _debug_target: vk::ImageViewType::TYPE_2D.as_raw() as u32,
            _mip_level: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}