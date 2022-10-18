use ash::{ vk, Device };
use nalgebra::{ Vector2, Vector3, Matrix3 };

use crate::renderer::renderer_context::RendererContext;
use crate::renderer::utility;
use crate::resource::resource::EngineResources;
use crate::vulkan_context::geometry_buffer::{ GeometryData };
use crate::vulkan_context::framebuffer::{ self, FramebufferData, RenderTargetInfo };
use crate::vulkan_context::vulkan_context::{Layers, SwapchainArray};
use crate::renderer::render_context::RenderContext_TAA_Simple;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::{DEFAULT_PIPELINE, RendererData};
use crate::renderer::push_constants::{PushConstant, PushConstantName};
use crate::renderer::shader_buffer_datas::{ AtmosphereConstants };

pub const USE_BAKED_PRECOMPUTED_ATMOSPHERE_TEXTURES: bool = true;
pub const DEFAULT_LUMINANCE_TYPE: Luminance = Luminance::NONE; // macro: USE_LUMINANCE
pub const DEFAULT_USE_COMBINED_TEXTURES: bool = true; // macro: COMBINED_SCATTERING_TEXTURES

pub const K_LAMBDA_R: f32 = 680.0;
pub const K_LAMBDA_G: f32 = 550.0;
pub const K_LAMBDA_B: f32 = 440.0;

pub const K_PI: f32 = std::f32::consts::PI;
pub const K_SUN_ANGULAR_RADIUS: f32 = 0.0235 / 2.0; // default: 0.00935 / 2.0
pub const K_SUN_SOLID_ANGLE: f32 = K_PI * K_SUN_ANGULAR_RADIUS * K_SUN_ANGULAR_RADIUS;
pub const K_LENGTH_UNIT_IN_METERS: f32 = 1000.0;

// Values from "Reference Solar Spectral Irradiance: ASTM G-173", ETR column
// http://rredc.nrel.gov/solar/spectra/am1.5/ASTMG173/ASTMG173.html
// summed and averaged in each bin (e.g. the value for 360nm is the average
// of the ASTM G-173 values for all wavelengths between 360 and 370nm).
// Values in W.m^-2.
pub const K_LAMBDA_MIN: f32 = 360.0;
pub const K_LAMBDA_MAX: f32 = 830.0;
pub const K_SOLAR_IRRADIANCE: [f32; 48] = [
  1.11776, 1.14259, 1.01249, 1.14716, 1.72765, 1.73054, 1.6887, 1.61253,
  1.91198, 2.03474, 2.02042, 2.02212, 1.93377, 1.95809, 1.91686, 1.8298,
  1.8685, 1.8931, 1.85149, 1.8504, 1.8341, 1.8345, 1.8147, 1.78158, 1.7533,
  1.6965, 1.68194, 1.64654, 1.6048, 1.52143, 1.55622, 1.5113, 1.474, 1.4482,
  1.41018, 1.36775, 1.34188, 1.31429, 1.28303, 1.26758, 1.2367, 1.2082,
  1.18737, 1.14683, 1.12362, 1.1058, 1.07124, 1.04992
];

// Values from http://www.iup.uni-bremen.de/gruppen/molspec/databases/
// referencespectra/o3spectra2011/index.html for 233K, summed and averaged in
// each bin (e.g. the value for 360nm is the average of the original values
// for all wavelengths between 360 and 370nm). Values in m^2.
pub const K_OZONE_CROSS_SECTION: [f32; 48] = [
  1.18e-27, 2.182e-28, 2.818e-28, 6.636e-28, 1.527e-27, 2.763e-27, 5.52e-27,
  8.451e-27, 1.582e-26, 2.316e-26, 3.669e-26, 4.924e-26, 7.752e-26, 9.016e-26,
  1.48e-25, 1.602e-25, 2.139e-25, 2.755e-25, 3.091e-25, 3.5e-25, 4.266e-25,
  4.672e-25, 4.398e-25, 4.701e-25, 5.019e-25, 4.305e-25, 3.74e-25, 3.215e-25,
  2.662e-25, 2.238e-25, 1.852e-25, 1.473e-25, 1.209e-25, 9.423e-26, 7.455e-26,
  6.566e-26, 5.105e-26, 4.15e-26, 4.228e-26, 3.237e-26, 2.451e-26, 2.801e-26,
  2.534e-26, 1.624e-26, 1.465e-26, 2.078e-26, 1.383e-26, 7.105e-27
];

// From https://en.wikipedia.org/wiki/Dobson_unit, in molecules.m^-2.
pub const K_DOBSON_UNIT: f32 = 2.687e20;
// Maximum number density of ozone molecules, in m^-3 (computed so at to get
// 300 Dobson units of ozone - for this we divide 300 DU by the integral of
// the ozone density profile defined below, which is equal to 15km).
pub const K_MAX_OZONE_NUMBER_DENSITY: f32 = 300.0 * K_DOBSON_UNIT / 15000.0;
// Wavelength independent solar irradiance "spectrum" (not physically
// realistic, but was used in the original implementation).
pub const K_CONSTANT_SOLAR_IRRADIANCE: f32 = 1.5;
pub const K_BOTTOM_RADIUS: f32 = 6361000.0;  // default : 6360000.0
pub const K_TOP_RADIUS: f32 = 6420000.0;
pub const K_RAYLEIGH: f32 = 1.24062e-6;
pub const K_RAYLEIGH_SCALE_HEIGHT: f32 = 8000.0;
pub const K_MIE_SCALE_HEIGHT: f32 = 1200.0;
pub const K_MIE_ANGSTROM_ALPHA: f32 = 0.0;
pub const K_MIE_ANGSTROM_BETA: f32 = 5.328e-3;
pub const K_MIE_SINGLE_SCATTERING_ALBEDO: f32 = 0.9;
pub const K_MIE_PHASE_FUNCTION_G: f32 = 0.8;
pub const K_GROUND_ALBEDO: f32 = 0.1;

pub const TRANSMITTANCE_TEXTURE_WIDTH: i32 = 256;
pub const TRANSMITTANCE_TEXTURE_HEIGHT: i32 = 64;

pub const SCATTERING_TEXTURE_R_SIZE: i32 = 32;
pub const SCATTERING_TEXTURE_MU_SIZE: i32 = 128;
pub const SCATTERING_TEXTURE_MU_S_SIZE: i32 = 32;
pub const SCATTERING_TEXTURE_NU_SIZE: i32 = 8;

pub const SCATTERING_TEXTURE_WIDTH: i32 = SCATTERING_TEXTURE_NU_SIZE * SCATTERING_TEXTURE_MU_S_SIZE;
pub const SCATTERING_TEXTURE_HEIGHT: i32 = SCATTERING_TEXTURE_MU_SIZE;
pub const SCATTERING_TEXTURE_DEPTH: i32 = SCATTERING_TEXTURE_R_SIZE;

pub const IRRADIANCE_TEXTURE_WIDTH: i32 = 64;
pub const IRRADIANCE_TEXTURE_HEIGHT: i32 = 16;

// The conversion factor between watts and lumens.
pub const MAX_LUMINOUS_EFFICACY: f32 = 683.0;

// Values from "CIE (1931) 2-deg color matching functions", see
// "http://web.archive.org/web/20081228084047/
//    http://www.cvrl.org/database/data/cmfs/ciexyz31.txt".
pub const CIE_2_DEG_COLOR_MATCHING_FUNCTIONS: [f32; 380] = [
  360.0, 0.000129900000, 0.000003917000, 0.000606100000,
  365.0, 0.000232100000, 0.000006965000, 0.001086000000,
  370.0, 0.000414900000, 0.000012390000, 0.001946000000,
  375.0, 0.000741600000, 0.000022020000, 0.003486000000,
  380.0, 0.001368000000, 0.000039000000, 0.006450001000,
  385.0, 0.002236000000, 0.000064000000, 0.010549990000,
  390.0, 0.004243000000, 0.000120000000, 0.020050010000,
  395.0, 0.007650000000, 0.000217000000, 0.036210000000,
  400.0, 0.014310000000, 0.000396000000, 0.067850010000,
  405.0, 0.023190000000, 0.000640000000, 0.110200000000,
  410.0, 0.043510000000, 0.001210000000, 0.207400000000,
  415.0, 0.077630000000, 0.002180000000, 0.371300000000,
  420.0, 0.134380000000, 0.004000000000, 0.645600000000,
  425.0, 0.214770000000, 0.007300000000, 1.039050100000,
  430.0, 0.283900000000, 0.011600000000, 1.385600000000,
  435.0, 0.328500000000, 0.016840000000, 1.622960000000,
  440.0, 0.348280000000, 0.023000000000, 1.747060000000,
  445.0, 0.348060000000, 0.029800000000, 1.782600000000,
  450.0, 0.336200000000, 0.038000000000, 1.772110000000,
  455.0, 0.318700000000, 0.048000000000, 1.744100000000,
  460.0, 0.290800000000, 0.060000000000, 1.669200000000,
  465.0, 0.251100000000, 0.073900000000, 1.528100000000,
  470.0, 0.195360000000, 0.090980000000, 1.287640000000,
  475.0, 0.142100000000, 0.112600000000, 1.041900000000,
  480.0, 0.095640000000, 0.139020000000, 0.812950100000,
  485.0, 0.057950010000, 0.169300000000, 0.616200000000,
  490.0, 0.032010000000, 0.208020000000, 0.465180000000,
  495.0, 0.014700000000, 0.258600000000, 0.353300000000,
  500.0, 0.004900000000, 0.323000000000, 0.272000000000,
  505.0, 0.002400000000, 0.407300000000, 0.212300000000,
  510.0, 0.009300000000, 0.503000000000, 0.158200000000,
  515.0, 0.029100000000, 0.608200000000, 0.111700000000,
  520.0, 0.063270000000, 0.710000000000, 0.078249990000,
  525.0, 0.109600000000, 0.793200000000, 0.057250010000,
  530.0, 0.165500000000, 0.862000000000, 0.042160000000,
  535.0, 0.225749900000, 0.914850100000, 0.029840000000,
  540.0, 0.290400000000, 0.954000000000, 0.020300000000,
  545.0, 0.359700000000, 0.980300000000, 0.013400000000,
  550.0, 0.433449900000, 0.994950100000, 0.008749999000,
  555.0, 0.512050100000, 1.000000000000, 0.005749999000,
  560.0, 0.594500000000, 0.995000000000, 0.003900000000,
  565.0, 0.678400000000, 0.978600000000, 0.002749999000,
  570.0, 0.762100000000, 0.952000000000, 0.002100000000,
  575.0, 0.842500000000, 0.915400000000, 0.001800000000,
  580.0, 0.916300000000, 0.870000000000, 0.001650001000,
  585.0, 0.978600000000, 0.816300000000, 0.001400000000,
  590.0, 1.026300000000, 0.757000000000, 0.001100000000,
  595.0, 1.056700000000, 0.694900000000, 0.001000000000,
  600.0, 1.062200000000, 0.631000000000, 0.000800000000,
  605.0, 1.045600000000, 0.566800000000, 0.000600000000,
  610.0, 1.002600000000, 0.503000000000, 0.000340000000,
  615.0, 0.938400000000, 0.441200000000, 0.000240000000,
  620.0, 0.854449900000, 0.381000000000, 0.000190000000,
  625.0, 0.751400000000, 0.321000000000, 0.000100000000,
  630.0, 0.642400000000, 0.265000000000, 0.000049999990,
  635.0, 0.541900000000, 0.217000000000, 0.000030000000,
  640.0, 0.447900000000, 0.175000000000, 0.000020000000,
  645.0, 0.360800000000, 0.138200000000, 0.000010000000,
  650.0, 0.283500000000, 0.107000000000, 0.000000000000,
  655.0, 0.218700000000, 0.081600000000, 0.000000000000,
  660.0, 0.164900000000, 0.061000000000, 0.000000000000,
  665.0, 0.121200000000, 0.044580000000, 0.000000000000,
  670.0, 0.087400000000, 0.032000000000, 0.000000000000,
  675.0, 0.063600000000, 0.023200000000, 0.000000000000,
  680.0, 0.046770000000, 0.017000000000, 0.000000000000,
  685.0, 0.032900000000, 0.011920000000, 0.000000000000,
  690.0, 0.022700000000, 0.008210000000, 0.000000000000,
  695.0, 0.015840000000, 0.005723000000, 0.000000000000,
  700.0, 0.011359160000, 0.004102000000, 0.000000000000,
  705.0, 0.008110916000, 0.002929000000, 0.000000000000,
  710.0, 0.005790346000, 0.002091000000, 0.000000000000,
  715.0, 0.004109457000, 0.001484000000, 0.000000000000,
  720.0, 0.002899327000, 0.001047000000, 0.000000000000,
  725.0, 0.002049190000, 0.000740000000, 0.000000000000,
  730.0, 0.001439971000, 0.000520000000, 0.000000000000,
  735.0, 0.000999949300, 0.000361100000, 0.000000000000,
  740.0, 0.000690078600, 0.000249200000, 0.000000000000,
  745.0, 0.000476021300, 0.000171900000, 0.000000000000,
  750.0, 0.000332301100, 0.000120000000, 0.000000000000,
  755.0, 0.000234826100, 0.000084800000, 0.000000000000,
  760.0, 0.000166150500, 0.000060000000, 0.000000000000,
  765.0, 0.000117413000, 0.000042400000, 0.000000000000,
  770.0, 0.000083075270, 0.000030000000, 0.000000000000,
  775.0, 0.000058706520, 0.000021200000, 0.000000000000,
  780.0, 0.000041509940, 0.000014990000, 0.000000000000,
  785.0, 0.000029353260, 0.000010600000, 0.000000000000,
  790.0, 0.000020673830, 0.000007465700, 0.000000000000,
  795.0, 0.000014559770, 0.000005257800, 0.000000000000,
  800.0, 0.000010253980, 0.000003702900, 0.000000000000,
  805.0, 0.000007221456, 0.000002607800, 0.000000000000,
  810.0, 0.000005085868, 0.000001836600, 0.000000000000,
  815.0, 0.000003581652, 0.000001293400, 0.000000000000,
  820.0, 0.000002522525, 0.000000910930, 0.000000000000,
  825.0, 0.000001776509, 0.000000641530, 0.000000000000,
  830.0, 0.000001251141, 0.000000451810, 0.000000000000,
];

// The conversion matrix from XYZ to linear sRGB color spaces.
// Values from https://en.wikipedia.org/wiki/SRGB.
pub const XYZ_TO_SRGB: [f32; 9] = [
    3.2406, -1.5372, -0.4986,
    -0.9689, 1.8758, 0.0415,
    0.0557, -0.2040, 1.0570
];

#[derive(Copy, Clone, PartialEq)]
pub enum Luminance {
    NONE = 0,
    APPROXIMATE = 1,
    PRECOMPUTED = 2,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_PrecomputedAtmosphere {
    pub _luminance_from_radiance: Matrix3<f32>,
    pub _luminance_from_radiance_reserved: [f32; 3],
    pub _scattering_order: i32,
    pub _layer: i32,
    pub _render_light_probe_mode: i32,
}

impl PushConstantName for PushConstant_PrecomputedAtmosphere {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_PrecomputedAtmosphere"
    }
}

impl PushConstant for PushConstant_PrecomputedAtmosphere {
}

fn cie_color_matching_function_table_value(wavelength: f32, column: i32) -> f32 {
    if wavelength <= K_LAMBDA_MIN || K_LAMBDA_MAX <= wavelength {
        return 0.0;
    }

    let mut u: f32 = (wavelength - K_LAMBDA_MIN) / 5.0;
    let row: i32 = u as i32;
    assert!(0 <= row && (row + 1) < 95);
    assert!(CIE_2_DEG_COLOR_MATCHING_FUNCTIONS[4 * row as usize] <= wavelength && wavelength <= CIE_2_DEG_COLOR_MATCHING_FUNCTIONS[4 * (row as usize + 1)]);

    u -= row as f32;
    CIE_2_DEG_COLOR_MATCHING_FUNCTIONS[(4 * row + column) as usize] * (1.0 - u) + CIE_2_DEG_COLOR_MATCHING_FUNCTIONS[(4 * (row + 1) + column) as usize] * u
}


fn interpolate(wavelengths: &[f32], wavelength_function: &[f32], wavelength: f32) -> f32 {
    assert_eq!(wavelength_function.len(), wavelengths.len());
    if wavelength < wavelengths[0] {
        return wavelength_function[0];
    }

    for i in 0..(wavelengths.len() - 1) {
        if wavelength < wavelengths[i + 1] {
            let u = (wavelength - wavelengths[i]) / (wavelengths[i + 1] - wavelengths[i]);
            return wavelength_function[i] * (1.0 - u) + wavelength_function[i + 1] * u;
        }
    }
    wavelength_function[wavelength_function.len() - 1]
}

// The returned constants are in lumen.nm / watt.
fn compute_spectral_radiance_to_luminance_factors(wavelengths: &[f32], solar_irradiance: &[f32], lambda_power: f32) -> Vector3<f32> {
    let mut k_r: f32 = 0.0;
    let mut k_g: f32 = 0.0;
    let mut k_b: f32 = 0.0;

    let solar_r = interpolate(wavelengths, solar_irradiance, K_LAMBDA_R);
    let solar_g = interpolate(wavelengths, solar_irradiance, K_LAMBDA_G);
    let solar_b = interpolate(wavelengths, solar_irradiance, K_LAMBDA_B);

    for l in (K_LAMBDA_MIN as usize)..(K_LAMBDA_MAX as usize) {
        let x_bar: f32 = cie_color_matching_function_table_value(l as f32, 1);
        let y_bar: f32 = cie_color_matching_function_table_value(l as f32, 2);
        let z_bar: f32 = cie_color_matching_function_table_value(l as f32, 3);
        let r_bar: f32 = XYZ_TO_SRGB[0] * x_bar + XYZ_TO_SRGB[1] * y_bar + XYZ_TO_SRGB[2] * z_bar;
        let g_bar: f32 = XYZ_TO_SRGB[3] * x_bar + XYZ_TO_SRGB[4] * y_bar + XYZ_TO_SRGB[5] * z_bar;
        let b_bar: f32 = XYZ_TO_SRGB[6] * x_bar + XYZ_TO_SRGB[7] * y_bar + XYZ_TO_SRGB[8] * z_bar;
        let irradiance = interpolate(wavelengths, solar_irradiance, l as f32);
        k_r += r_bar * irradiance / solar_r * (l as f32 / K_LAMBDA_R).powf(lambda_power);
        k_g += g_bar * irradiance / solar_g * (l as f32 / K_LAMBDA_G).powf(lambda_power);
        k_b += b_bar * irradiance / solar_b * (l as f32 / K_LAMBDA_B).powf(lambda_power);
    }
    k_r *= MAX_LUMINOUS_EFFICACY;
    k_g *= MAX_LUMINOUS_EFFICACY;
    k_b *= MAX_LUMINOUS_EFFICACY;
    Vector3::new(k_r, k_g, k_b)
}

fn _convert_spectrum_to_linear_srgb(wavelengths: &[f32], spectrum: &[f32]) -> [f32; 3] {
    let mut x: f32 = 0.0;
    let mut y: f32 = 0.0;
    let mut z: f32 = 0.0;
    for l in (K_LAMBDA_MIN as usize)..(K_LAMBDA_MAX as usize) {
        let value: f32 = interpolate(wavelengths, spectrum, l as f32);
        x += cie_color_matching_function_table_value(l as f32, 1) * value;
        y += cie_color_matching_function_table_value(l as f32, 2) * value;
        z += cie_color_matching_function_table_value(l as f32, 3) * value;
    }

    let r = MAX_LUMINOUS_EFFICACY * (XYZ_TO_SRGB[0] * x + XYZ_TO_SRGB[1] * y + XYZ_TO_SRGB[2] * z);
    let g = MAX_LUMINOUS_EFFICACY * (XYZ_TO_SRGB[3] * x + XYZ_TO_SRGB[4] * y + XYZ_TO_SRGB[5] * z);
    let b = MAX_LUMINOUS_EFFICACY * (XYZ_TO_SRGB[6] * x + XYZ_TO_SRGB[7] * y + XYZ_TO_SRGB[8] * z);
    [r, g, b]
}

#[derive(Clone, Copy)]
pub struct DensityProfileLayer {
    pub _width: f32,
    pub _exp_term: f32,
    pub _exp_scale: f32,
    pub _linear_term: f32,
    pub _constant_term: f32,
}

impl DensityProfileLayer {
    pub fn default() -> DensityProfileLayer {
        DensityProfileLayer {
            _width: 0.0,
            _exp_term: 0.0,
            _exp_scale: 0.0,
            _linear_term: 0.0,
            _constant_term: 0.0,
        }
    }
}

#[derive(Clone)]
pub struct AtmosphereModel {
    pub _wavelengths: Vec<f32>,
    pub _solar_irradiance: Vec<f32>,
    pub _sun_angular_radius: f32,
    pub _bottom_radius: f32,
    pub _top_radius: f32,
    pub _rayleigh_density: Vec<DensityProfileLayer>,
    pub _rayleigh_scattering: Vec<f32>,
    pub _mie_density: Vec<DensityProfileLayer>,
    pub _mie_scattering: Vec<f32>,
    pub _mie_extinction: Vec<f32>,
    pub _mie_phase_function_g: f32,
    pub _absorption_density: Vec<DensityProfileLayer>,
    pub _absorption_extinction: Vec<f32>,
    pub _ground_albedo: Vec<f32>,
    pub _max_sun_zenith_angle: f32,
    pub _length_unit_in_meters: f32,
    pub _num_precomputed_wavelengths: i32,
    pub _luminance_type: Luminance,
    pub _use_combined_textures: bool,
}

#[derive(Clone)]
pub struct Atmosphere {
    pub _is_render_atmosphere: bool,
    pub _use_constant_solar_spectrum: bool,
    pub _use_ozone: bool,
    pub _use_combined_textures: bool,
    pub _luminance_type: Luminance,
    pub _num_precomputed_wavelengths: i32,
    pub _do_white_balance: bool,
    pub _show_help: bool,
    pub _view_distance_meters: f32,
    pub _view_zenith_angle_radians: f32,
    pub _view_azimuth_angle_radians: f32,
    pub _sun_zenith_angle_radians: f32,
    pub _sun_azimuth_angle_radians: f32,
    pub _white_point: Vector3<f32>,
    pub _earth_center: Vector3<f32>,
    pub _sun_size: Vector2<f32>,
    pub _sky: Vector3<f32>,
    pub _sun: Vector3<f32>,
    pub _atmosphere_constants: AtmosphereConstants,
    pub _atmosphere_exposure: f32,
    pub _cloud_exposure: f32,
    pub _cloud_altitude: f32,
    pub _cloud_height: f32,
    pub _cloud_speed: f32,
    pub _cloud_absorption: f32,
    pub _cloud_contrast: f32,
    pub _cloud_coverage: f32,
    pub _cloud_tiling: f32,
    pub _inscatter_power: f32,
    pub _noise_contrast: f32,
    pub _noise_coverage: f32,
    pub _noise_tiling: f32,
    pub _compute_multiple_scattering_framebuffers: Layers<FramebufferData>,
    pub _compute_single_scattering_framebuffers: Layers<FramebufferData>,
    pub _compute_scattering_density_framebuffers: Layers<FramebufferData>,
    pub _render_context_precomputed_atmosphere: RenderContext_TAA_Simple,
    pub _composite_atmosphere_descriptor_sets0: SwapchainArray<vk::DescriptorSet>,
    pub _composite_atmosphere_descriptor_sets1: SwapchainArray<vk::DescriptorSet>,
}

impl DensityProfileLayer {
    pub fn create_density_profile_layer(width: f32, exp_term: f32, exp_scale: f32, linear_term: f32, constant_term: f32) -> DensityProfileLayer {
        DensityProfileLayer {
            _width: width,
            _exp_term: exp_term,
            _exp_scale: exp_scale,
            _linear_term: linear_term,
            _constant_term: constant_term,
        }
    }
}

impl AtmosphereModel {
    pub fn create_atmosphere_model(
        wavelengths: Vec<f32>,
        solar_irradiance: Vec<f32>,
        sun_angular_radius: f32,
        bottom_radius: f32,
        top_radius: f32,
        rayleigh_density: Vec<DensityProfileLayer>,
        rayleigh_scattering: Vec<f32>,
        mie_density: Vec<DensityProfileLayer>,
        mie_scattering: Vec<f32>,
        mie_extinction: Vec<f32>,
        mie_phase_function_g: f32,
        absorption_density: Vec<DensityProfileLayer>,
        absorption_extinction: Vec<f32>,
        ground_albedo: Vec<f32>,
        max_sun_zenith_angle: f32,
        length_unit_in_meters: f32,
        num_precomputed_wavelengths: i32,
        luminance_type: Luminance,
        use_combined_textures: bool
    ) -> AtmosphereModel {
        AtmosphereModel {
            _wavelengths: wavelengths,
            _solar_irradiance: solar_irradiance,
            _sun_angular_radius: sun_angular_radius,
            _bottom_radius: bottom_radius,
            _top_radius: top_radius,
            _rayleigh_density: rayleigh_density,
            _rayleigh_scattering: rayleigh_scattering,
            _mie_density: mie_density,
            _mie_scattering: mie_scattering,
            _mie_extinction: mie_extinction,
            _mie_phase_function_g: mie_phase_function_g,
            _absorption_density: absorption_density,
            _absorption_extinction: absorption_extinction,
            _ground_albedo: ground_albedo,
            _max_sun_zenith_angle: max_sun_zenith_angle,
            _length_unit_in_meters: length_unit_in_meters,
            _num_precomputed_wavelengths: num_precomputed_wavelengths,
            _luminance_type: luminance_type,
            _use_combined_textures: use_combined_textures,
        }
    }

    fn shader_header_factory(&mut self, lambdas: &[f32; 3]) -> String {
        let wavelengths = self._wavelengths.clone();
        let to_string = |v: &[f32], lambdas: &[f32; 3], scale: f32| -> String {
            let r = interpolate(&wavelengths, v, lambdas[0]) * scale;
            let g = interpolate(&wavelengths, v, lambdas[1]) * scale;
            let b = interpolate(&wavelengths, v, lambdas[2]) * scale;
            format!("vec3({}, {}, {})", r, g, b)
        };

        let length_unit_in_meters = self._length_unit_in_meters;
        let density_layer = |layer: &DensityProfileLayer| -> String {
            format!("DensityProfileLayer({}, {}, {}, {}, {})",
                    layer._width / length_unit_in_meters,
                    layer._exp_term,
                    layer._exp_scale * length_unit_in_meters,
                    layer._linear_term * length_unit_in_meters,
                    layer._constant_term
            )
        };

        let density_profile = |layers: &mut Vec<DensityProfileLayer>| -> String {
            const K_LAYER_COUNT: usize = 2;
            while layers.len() < K_LAYER_COUNT {
                layers.insert(0, DensityProfileLayer::default());
            }
            let mut result = format!("DensityProfile(DensityProfileLayer[{}](", K_LAYER_COUNT);
            for i in 0..K_LAYER_COUNT {
                result.push_str(&density_layer(&layers[i]));
                if i < K_LAYER_COUNT - 1 {
                    result.push_str(",");
                } else {
                    result.push_str("))");
                }
            }
            result
        };

        // header
        vec![
            format!("const int TRANSMITTANCE_TEXTURE_WIDTH = {};", TRANSMITTANCE_TEXTURE_WIDTH),
            format!("const int TRANSMITTANCE_TEXTURE_HEIGHT = {};", TRANSMITTANCE_TEXTURE_HEIGHT),
            format!("const int SCATTERING_TEXTURE_R_SIZE = {};", SCATTERING_TEXTURE_R_SIZE),
            format!("const int SCATTERING_TEXTURE_MU_SIZE = {};", SCATTERING_TEXTURE_MU_SIZE),
            format!("const int SCATTERING_TEXTURE_MU_S_SIZE = {};", SCATTERING_TEXTURE_MU_S_SIZE),
            format!("const int SCATTERING_TEXTURE_NU_SIZE = {};", SCATTERING_TEXTURE_NU_SIZE),
            format!("const int IRRADIANCE_TEXTURE_WIDTH = {};", IRRADIANCE_TEXTURE_WIDTH),
            format!("const int IRRADIANCE_TEXTURE_HEIGHT = {};", IRRADIANCE_TEXTURE_HEIGHT),
            format!("const vec2 IRRADIANCE_TEXTURE_SIZE = vec2({}, {});", IRRADIANCE_TEXTURE_WIDTH, IRRADIANCE_TEXTURE_HEIGHT),
            String::from(""),
            String::from("const AtmosphereParameters ATMOSPHERE = AtmosphereParameters("),
            format!("{}, ", to_string(&self._solar_irradiance, lambdas, 1.0)),
            format!("{}, ", self._sun_angular_radius),
            format!("{}, ", self._bottom_radius / self._length_unit_in_meters),
            format!("{}, ", self._top_radius / self._length_unit_in_meters),
            format!("{}, ", density_profile(&mut self._rayleigh_density)),
            format!("{}, ", to_string(&self._rayleigh_scattering, lambdas, self._length_unit_in_meters)),
            format!("{}, ", density_profile(&mut self._mie_density)),
            format!("{}, ", to_string(&self._mie_scattering, lambdas, self._length_unit_in_meters)),
            format!("{}, ", to_string(&self._mie_extinction, lambdas, self._length_unit_in_meters)),
            format!("{}, ", self._mie_phase_function_g),
            format!("{}, ", density_profile(&mut self._absorption_density)),
            format!("{}, ", to_string(&self._absorption_extinction, lambdas, self._length_unit_in_meters)),
            format!("{}, ", to_string(&self._ground_albedo, lambdas, 1.0)),
            format!("{});", self._max_sun_zenith_angle.cos()),
            String::from(""),
        ].join("\n")
    }

    pub fn generate(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        atmosphere: &Atmosphere,
        num_scattering_orders: i32
    ) {
        if Luminance::PRECOMPUTED != self._luminance_type {
            let lambdas: [f32; 3] = [K_LAMBDA_R, K_LAMBDA_G, K_LAMBDA_B];
            let luminance_from_radiance = Matrix3::identity();
            let blend = false;
            self.precompute(
                command_buffer,
                swapchain_index,
                quad_geometry_data,
                renderer_context,
                atmosphere,
                &lambdas,
                &luminance_from_radiance,
                blend,
                num_scattering_orders
            );
        } else {
            let num_iterations: f32 = (self._num_precomputed_wavelengths as f32 + 2.0) / 3.0;
            let dlambda: f32 = (K_LAMBDA_MAX - K_LAMBDA_MIN) as f32 / (3.0 * num_iterations);

            let coeff = |l: f32, component: usize| -> f32 {
                let x: f32 = cie_color_matching_function_table_value(l, 1);
                let y: f32 = cie_color_matching_function_table_value(l, 2);
                let z: f32 = cie_color_matching_function_table_value(l, 3);
                (XYZ_TO_SRGB[component * 3] * x + XYZ_TO_SRGB[component * 3 + 1] * y + XYZ_TO_SRGB[component * 3 + 2] * z) * dlambda
            };

            for i in 0..num_iterations as i32 {
                let lambdas: [f32; 3] = [
                    K_LAMBDA_MIN as f32 + (3.0 * i as f32 + 0.5) * dlambda,
                    K_LAMBDA_MIN as f32 + (3.0 * i as f32 + 1.5) * dlambda,
                    K_LAMBDA_MIN as f32 + (3.0 * i as f32 + 2.5) * dlambda
                ];

                let luminance_from_radiance = Matrix3::from_columns(&[
                    Vector3::new(coeff(lambdas[0], 0), coeff(lambdas[1], 0), coeff(lambdas[2], 0)),
                    Vector3::new(coeff(lambdas[0], 1), coeff(lambdas[1], 1), coeff(lambdas[2], 1)),
                    Vector3::new(coeff(lambdas[0], 2), coeff(lambdas[1], 2), coeff(lambdas[2], 2)),
                ]).transpose();

                let blend = 0 < i;
                self.precompute(
                    command_buffer,
                    swapchain_index,
                    quad_geometry_data,
                    renderer_context,
                    atmosphere,
                    &lambdas,
                    &luminance_from_radiance,
                    blend,
                    num_scattering_orders
                );
            }
        }

        // recompute_transmittance
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "precomputed_atmosphere/precomputed_atmosphere",
            "compute_transmittance/recompute_transmittance",
            quad_geometry_data,
            None,
            None,
            None,
        );
    }

    fn precompute(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        atmosphere: &Atmosphere,
        lambdas: &[f32; 3],
        luminance_from_radiance: &Matrix3<f32>,
        blend: bool,
        num_scattering_orders: i32
    ) {
        let shader_header = self.shader_header_factory(lambdas);
        println!("{}", shader_header);

        let mut push_constant = PushConstant_PrecomputedAtmosphere {
            _luminance_from_radiance: (*luminance_from_radiance).into(),
            _luminance_from_radiance_reserved: [0.0, 0.0, 0.0],
            _scattering_order: 0,
            _layer: 0,
            _render_light_probe_mode: 0,
        };

        // compute_transmittance
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "precomputed_atmosphere/precomputed_atmosphere",
            "compute_transmittance/default",
            quad_geometry_data,
            None,
            None,
            None,
        );

        // compute_direct_irradiance
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "precomputed_atmosphere/precomputed_atmosphere",
            if blend { "compute_direct_irradiance/default" } else { "compute_direct_irradiance/additive" },
            quad_geometry_data,
            None,
            None,
            None,
        );

        // compute_single_scattering
        for layer in 0..SCATTERING_TEXTURE_DEPTH as usize {
            push_constant._layer = layer as i32;
            renderer_context.render_material_instance(
                command_buffer,
                swapchain_index,
                "precomputed_atmosphere/precomputed_atmosphere",
                if blend { "compute_single_scattering/additive" } else { "compute_single_scattering/default" },
                quad_geometry_data,
                Some(&atmosphere._compute_single_scattering_framebuffers[layer]),
                None,
                Some(&push_constant),
            );
        }

        for scattering_order in 2..(num_scattering_orders + 1) {
            // compute_scattering_density
            for layer in 0..SCATTERING_TEXTURE_DEPTH as usize {
                push_constant._scattering_order = scattering_order;
                push_constant._layer = layer as i32;
                renderer_context.render_material_instance(
                    command_buffer,
                    swapchain_index,
                    "precomputed_atmosphere/precomputed_atmosphere",
                    "compute_scattering_density/default",
                    quad_geometry_data,
                    Some(&atmosphere._compute_scattering_density_framebuffers[layer]),
                    None,
                    Some(&push_constant),
                );
            }

            // compute_indirect_irradiance
            push_constant._scattering_order = scattering_order - 1;
            renderer_context.render_material_instance(
                command_buffer,
                swapchain_index,
                "precomputed_atmosphere/precomputed_atmosphere",
                "compute_indirect_irradiance/default",
                quad_geometry_data,
                None,
                None,
                Some(&push_constant),
            );

            // compute_multiple_scattering
            for layer in 0..SCATTERING_TEXTURE_DEPTH as usize {
                push_constant._layer = layer as i32;
                renderer_context.render_material_instance(
                    command_buffer,
                    swapchain_index,
                    "precomputed_atmosphere/precomputed_atmosphere",
                    "compute_multiple_scattering/default",
                    quad_geometry_data,
                    Some(&atmosphere._compute_multiple_scattering_framebuffers[layer]),
                    None,
                    Some(&push_constant),
                );
            }
        }
    }
}


impl Atmosphere {
    pub fn create_atmosphere(is_render_atmosphere: bool) -> Atmosphere {
        let luminance_type: Luminance = DEFAULT_LUMINANCE_TYPE;

        Atmosphere {
            _is_render_atmosphere: is_render_atmosphere,
            _use_constant_solar_spectrum: false,
            _use_ozone: true,
            _use_combined_textures: DEFAULT_USE_COMBINED_TEXTURES,
            _luminance_type: luminance_type,
            _num_precomputed_wavelengths: if Luminance::PRECOMPUTED == luminance_type { 15 } else { 3 },
            _do_white_balance: false,
            _show_help: true,
            _view_distance_meters: 9000.0,
            _view_zenith_angle_radians: 1.47,
            _view_azimuth_angle_radians: -0.1,
            _sun_zenith_angle_radians: 1.3,
            _sun_azimuth_angle_radians: 2.9,
            _white_point: Vector3::zeros(),
            _earth_center: Vector3::new(0.0, -K_BOTTOM_RADIUS / K_LENGTH_UNIT_IN_METERS, 0.0),
            _sun_size: Vector2::new(K_SUN_ANGULAR_RADIUS.tan(), K_SUN_ANGULAR_RADIUS.cos()),
            _sky: Vector3::new(1.0, 1.0, 1.0),
            _sun: Vector3::new(1.0, 1.0, 1.0),
            _atmosphere_constants: AtmosphereConstants::default(),
            _atmosphere_exposure: 0.0001,
            _cloud_exposure: 0.1,
            _cloud_altitude: 100.0,
            _cloud_height: 500.0,
            _cloud_speed: 0.001,
            _cloud_absorption: 0.15,
            _cloud_contrast: 2.0,
            _cloud_coverage: 0.8,
            _cloud_tiling: 0.0004,
            _inscatter_power: 0.25,
            _noise_contrast: 1.0,
            _noise_coverage: 1.0,
            _noise_tiling: 0.0003,
            _compute_multiple_scattering_framebuffers: Layers::new(),
            _compute_single_scattering_framebuffers: Layers::new(),
            _compute_scattering_density_framebuffers: Layers::new(),
            _render_context_precomputed_atmosphere: RenderContext_TAA_Simple::default(),
            _composite_atmosphere_descriptor_sets0: Vec::new(),
            _composite_atmosphere_descriptor_sets1: Vec::new(),
        }
    }

    pub fn precompute(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext
    ) {
        // precompute constants
        let max_sun_zenith_angle = 120.0 / 180.0 * K_PI;

        let rayleigh_layer = DensityProfileLayer::create_density_profile_layer(0.0, 1.0, -1.0 / K_RAYLEIGH_SCALE_HEIGHT, 0.0, 0.0);
        let mie_layer = DensityProfileLayer::create_density_profile_layer(0.0, 1.0, -1.0 / K_MIE_SCALE_HEIGHT, 0.0, 0.0);
        let ozone_density: Vec<DensityProfileLayer> = vec![
            DensityProfileLayer::create_density_profile_layer(25000.0, 0.0, 0.0, 1.0 / 15000.0, -2.0 / 3.0),
            DensityProfileLayer::create_density_profile_layer(0.0, 0.0, 0.0, -1.0 / 15000.0, 8.0 / 3.0)
        ];

        let mut wavelengths: Vec<f32> = Vec::new();
        let mut solar_irradiance: Vec<f32> = Vec::new();
        let mut rayleigh_scattering: Vec<f32> = Vec::new();
        let mut mie_scattering: Vec<f32> = Vec::new();
        let mut mie_extinction: Vec<f32> = Vec::new();
        let mut absorption_extinction: Vec<f32> = Vec::new();
        let mut ground_albedo: Vec<f32> = Vec::new();

        for i in ((K_LAMBDA_MIN as usize)..(K_LAMBDA_MAX as usize + 1)).step_by(10) {
            let l = i as f32 * 1e-3;  // micro-meters
            let mie = K_MIE_ANGSTROM_BETA / K_MIE_SCALE_HEIGHT * l.powf(-K_MIE_ANGSTROM_ALPHA);
            wavelengths.push(i as f32);
            if self._use_constant_solar_spectrum {
                solar_irradiance.push(K_CONSTANT_SOLAR_IRRADIANCE);
            } else {
                solar_irradiance.push(K_SOLAR_IRRADIANCE[(i as usize - K_LAMBDA_MIN as usize) / 10]);
            }
            rayleigh_scattering.push(K_RAYLEIGH * l.powf(-4.0));
            mie_scattering.push(mie * K_MIE_SINGLE_SCATTERING_ALBEDO);
            mie_extinction.push(mie);
            if self._use_ozone {
                absorption_extinction.push(K_MAX_OZONE_NUMBER_DENSITY * K_OZONE_CROSS_SECTION[(i as usize - K_LAMBDA_MIN as usize) / 10])
            } else {
                absorption_extinction.push(0.0);
            }
            ground_albedo.push(K_GROUND_ALBEDO);
        }

        let rayleigh_density: Vec<DensityProfileLayer> = vec![rayleigh_layer];
        let mie_density: Vec<DensityProfileLayer> = vec![mie_layer];

        if Luminance::PRECOMPUTED == self._luminance_type {
            self._sky = Vector3::new(MAX_LUMINOUS_EFFICACY, MAX_LUMINOUS_EFFICACY, MAX_LUMINOUS_EFFICACY);
        } else {
            self._sky = compute_spectral_radiance_to_luminance_factors(&wavelengths, &solar_irradiance, -3.0);
        }
        self._sun = compute_spectral_radiance_to_luminance_factors(&wavelengths, &solar_irradiance, 0.0);

        self._atmosphere_constants = AtmosphereConstants {
            _sky_radiance_to_luminance: &self._sky * self._atmosphere_exposure,
            _cloud_exposure: self._cloud_exposure,
            _sun_radiance_to_luminance: &self._sky * self._atmosphere_exposure,
            _cloud_altitude: self._cloud_altitude,
            _cloud_height: self._cloud_height,
            _cloud_speed: self._cloud_speed,
            _cloud_absorption: self._cloud_absorption,
            _cloud_tiling: self._cloud_tiling,
            _cloud_contrast: self._cloud_contrast,
            _cloud_coverage: self._cloud_coverage,
            _noise_tiling: self._noise_tiling,
            _noise_contrast: self._noise_contrast,
            _earth_center: self._earth_center.into(),
            _noise_coverage: self._noise_coverage,
            _sun_size: self._sun_size.into(),
            _atmosphere_exposure: self._atmosphere_exposure,
            _inscatter_power: self._inscatter_power,
        };

        // generate precomputed textures
        if false == USE_BAKED_PRECOMPUTED_ATMOSPHERE_TEXTURES {
            let mut atmosphere_model = AtmosphereModel::create_atmosphere_model(
                wavelengths,
                solar_irradiance,
                K_SUN_ANGULAR_RADIUS,
                K_BOTTOM_RADIUS,
                K_TOP_RADIUS,
                rayleigh_density,
                rayleigh_scattering,
                mie_density,
                mie_scattering,
                mie_extinction,
                K_MIE_PHASE_FUNCTION_G,
                ozone_density,
                absorption_extinction,
                ground_albedo,
                max_sun_zenith_angle,
                K_LENGTH_UNIT_IN_METERS,
                self._num_precomputed_wavelengths,
                self._luminance_type,
                self._use_combined_textures
            );

            let num_scattering_orders: i32 = 4;
            atmosphere_model.generate(
                command_buffer,
                swapchain_index,
                quad_geometry_data,
                renderer_context,
                self,
                num_scattering_orders,
            );
        }
    }

    pub fn prepare_framebuffer_and_descriptors(&mut self, renderer_data: &RendererData, engine_resources: &EngineResources) {
        let device = renderer_data.get_renderer_context().get_device();
        let material_instance = engine_resources.get_material_instance_data("precomputed_atmosphere/precomputed_atmosphere").borrow();

        // render precomputed atmosphere
        self._render_context_precomputed_atmosphere.initialize(
            device,
            engine_resources,
            renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR),
            renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED),
            renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV),
            RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED,
            RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV
        );

        // composite atmosphere
        let composite_atmosphere_pipeline_binding_data = material_instance.get_pipeline_binding_data("composite_atmosphere/default");
        let composite_atmosphere_descriptor_binding_index: usize = 15;
        let precomputed_atmosphere_color_resolved = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED);
        let precomputed_atmosphere_color_resolved_prev = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV);
        self._composite_atmosphere_descriptor_sets0 = utility::create_descriptor_sets(
            device,
            composite_atmosphere_pipeline_binding_data,
            &[(composite_atmosphere_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(precomputed_atmosphere_color_resolved.get_default_image_info()))],
        );
        self._composite_atmosphere_descriptor_sets1 = utility::create_descriptor_sets(
            device,
            composite_atmosphere_pipeline_binding_data,
            &[(composite_atmosphere_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(precomputed_atmosphere_color_resolved_prev.get_default_image_info()))]
        );

        // precomputed atmosphere
        if false == USE_BAKED_PRECOMPUTED_ATMOSPHERE_TEXTURES {
            let delta_scattering_density = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_SCATTERING_DENSITY);
            let delta_rayleigh_scattering = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_RAYLEIGH_SCATTERING);
            let delta_mie_scattering = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_DELTA_MIE_SCATTERING);
            let scattering = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_SCATTERING);
            let optional_single_mie_scattering = renderer_data.get_render_target(RenderTargetType::PRECOMPUTED_ATMOSPHERE_OPTIONAL_SINGLE_MIE_SCATTERING);
            let compute_multiple_scattering_pipeline_binding_data = material_instance.get_pipeline_binding_data("compute_multiple_scattering/default");
            let compute_single_scattering_pipeline_binding_data = material_instance.get_pipeline_binding_data("compute_single_scattering/default");
            let compute_scattering_density_pipeline_binding_data = material_instance.get_pipeline_binding_data("compute_scattering_density/default");
            for layer in 0..SCATTERING_TEXTURE_DEPTH as u32 {
                // compute_multiple_scattering
                self._compute_multiple_scattering_framebuffers.push(
                    utility::create_framebuffers(
                        device,
                        &compute_multiple_scattering_pipeline_binding_data.get_render_pass_data().borrow(),
                        "compute_multiple_scattering",
                        &[
                            // DELTA_RAYLEIGH_SCATTERING equal to DELTA_MULTIPLE_SCATTERING_TEXTURE
                            RenderTargetInfo { _texture_data: &delta_rayleigh_scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None },
                            RenderTargetInfo { _texture_data: &scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None },
                        ],
                        &[],
                        &[],
                    )
                );
                // compute_single_scattering
                let compute_single_scattering_rendertargets = vec![
                    RenderTargetInfo { _texture_data: &delta_rayleigh_scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None },
                    RenderTargetInfo { _texture_data: &delta_mie_scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None },
                    RenderTargetInfo { _texture_data: &scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None },
                    RenderTargetInfo { _texture_data: &optional_single_mie_scattering, _target_layer: layer, _target_mip_level: 0, _clear_value: None }
                ];

                self._compute_single_scattering_framebuffers.push(
                    utility::create_framebuffers(
                        device,
                        &compute_single_scattering_pipeline_binding_data.get_render_pass_data().borrow(),
                        "compute_single_scattering",
                        &compute_single_scattering_rendertargets,
                        &[],
                        &[],
                    )
                );
                // compute_scattering_density
                self._compute_scattering_density_framebuffers.push(
                    utility::create_framebuffers(
                        device,
                        &compute_scattering_density_pipeline_binding_data.get_render_pass_data().borrow(),
                        "compute_scattering_density",
                        &[RenderTargetInfo { _texture_data: &delta_scattering_density, _target_layer: layer, _target_mip_level: 0, _clear_value: None }],
                        &[],
                        &[],
                    )
                );
            }
        }
    }

    pub fn destroy_atmosphere(&mut self, device: &Device) {
        if false == USE_BAKED_PRECOMPUTED_ATMOSPHERE_TEXTURES {
            for layer in 0..SCATTERING_TEXTURE_DEPTH as usize {
                framebuffer::destroy_framebuffer_data(device, &self._compute_multiple_scattering_framebuffers[layer]);
                framebuffer::destroy_framebuffer_data(device, &self._compute_single_scattering_framebuffers[layer]);
                framebuffer::destroy_framebuffer_data(device, &self._compute_scattering_density_framebuffers[layer]);
            }
        }
        self._compute_multiple_scattering_framebuffers.clear();
        self._compute_single_scattering_framebuffers.clear();
        self._compute_scattering_density_framebuffers.clear();
        self._composite_atmosphere_descriptor_sets0.clear();
        self._composite_atmosphere_descriptor_sets1.clear();
        self._render_context_precomputed_atmosphere.destroy(device);
    }

    pub fn update(&mut self) {
        self._render_context_precomputed_atmosphere.update();
    }

    pub fn render_precomputed_atmosphere(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        render_light_probe_mode: bool
    ) {
        // Render Atmosphere
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "precomputed_atmosphere/precomputed_atmosphere",
            "render_atmosphere/default",
            quad_geometry_data,
            None,
            None,
            Some(&PushConstant_PrecomputedAtmosphere {
                _render_light_probe_mode: if render_light_probe_mode { 1 } else { 0 },
                ..Default::default()
            }),
        );

        // Anti-Aliasing
        let (taa_framebuffer, taa_descriptor_sets, composite_descriptor_sets) = match self._render_context_precomputed_atmosphere._current_taa_resolved {
            RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED => (
                Some(&self._render_context_precomputed_atmosphere._framebuffer_data0),
                Some(&self._render_context_precomputed_atmosphere._descriptor_sets0),
                Some(&self._composite_atmosphere_descriptor_sets1)
            ),
            RenderTargetType::PRECOMPUTED_ATMOSPHERE_COLOR_RESOLVED_PREV => (
                Some(&self._render_context_precomputed_atmosphere._framebuffer_data1),
                Some(&self._render_context_precomputed_atmosphere._descriptor_sets1),
                Some(&self._composite_atmosphere_descriptor_sets0)
            ),
            _ => panic!("not matched render target. {:?}", self._render_context_precomputed_atmosphere._current_taa_resolved)
        };
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "common/render_taa_simple",
            DEFAULT_PIPELINE,
            quad_geometry_data,
            taa_framebuffer,
            taa_descriptor_sets,
            None
        );

        // Composite Atmosphere
        renderer_context.render_material_instance(
            command_buffer,
            swapchain_index,
            "precomputed_atmosphere/precomputed_atmosphere",
            "composite_atmosphere/default",
            quad_geometry_data,
            None,
            composite_descriptor_sets,
            None,
        );
    }
}