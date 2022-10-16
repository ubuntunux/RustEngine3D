
use std::cmp::max;

use ash::{ vk, Device };
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::render_target::RenderTargetType;
use crate::renderer::renderer_data::RendererData;
use crate::renderer::push_constants::{PushConstant, PushConstantName};
use crate::renderer::utility;
use crate::resource::resource::EngineResources;
use crate::vulkan_context::geometry_buffer::{ self, GeometryData };
use crate::vulkan_context::texture::TextureCreateInfo;
use crate::vulkan_context::framebuffer::{ self, FramebufferData };
use crate::vulkan_context::vulkan_context::{ SwapchainArray, Layers, MipLevels };
use crate::utilities::system::newRcRefCell;

const CM: f64 = 0.23;
const KM: f64 = 370.0;
const WIND: f32 = 5.0;
const OMEGA: f32 = 0.84;
const AMPLITUDE: f32 = 0.5;
const _CHOPPY_FACTOR: [f32; 4] = [2.3, 2.1, 1.3, 0.9];
const PASSES: i32 = 8; // number of passes needed for the FFT 6 -> 64, 7 -> 128, 8 -> 256, etc
pub const FFT_SIZE: i32 = 1 << PASSES; // size of the textures storing the waves in frequency and spatial domains
pub const N_SLOPE_VARIANCE: i32 = 10;
pub const FFT_LAYER_COUNT: i32 = 5;
const GRID1_SIZE: f32 = 5488.0;
const GRID2_SIZE: f32 = 392.0;
const GRID3_SIZE: f32 = 28.0;
const GRID4_SIZE: f32 = 2.0;
const GRID_SIZES: [f32; 4] = [GRID1_SIZE, GRID2_SIZE, GRID3_SIZE, GRID4_SIZE];
const INVERSE_GRID_SIZES: [f32; 4] = [
    2.0 * std::f32::consts::PI * FFT_SIZE as f32 / GRID1_SIZE,
    2.0 * std::f32::consts::PI * FFT_SIZE as f32 / GRID2_SIZE,
    2.0 * std::f32::consts::PI * FFT_SIZE as f32 / GRID3_SIZE,
    2.0 * std::f32::consts::PI * FFT_SIZE as f32 / GRID4_SIZE
];
const GRID_VERTEX_COUNT: u32 = 200;
const GRID_CELL_SIZE: [f32; 2] = [1.0 / GRID_VERTEX_COUNT as f32, 1.0 / GRID_VERTEX_COUNT as f32];
const DEFAULT_FFT_SEED: i32 = 1234;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_FFT_Ocean {
    pub _grid_sizes: [f32; 4],
    pub _inverse_grid_sizes: [f32; 4],
    pub _simulation_size: [f32; 4],
    pub _cell_size: [f32; 2],
    pub _simulation_wind: f32,
    pub _simulation_amplitude: f32,
    pub _t: f32,
    pub _fft_size: i32,
    pub _pass: f32,
    pub _n_slope_variance: f32,
    pub _slope_variance_delta: f32,
    pub _c: f32,
    pub _reserved0: i32,
    pub _reserved1: i32,
}

impl PushConstantName for PushConstant_FFT_Ocean {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_FFT_Ocean"
    }
}

impl PushConstant for PushConstant_FFT_Ocean {
}

pub struct FFTOcean {
    _name: String,
    _height: f32,
    _wind: f32,
    _omega: f32,
    _amplitude: f32,
    _simulation_wind: f32,
    _simulation_amplitude: f32,
    _simulation_scale: f32,
    _is_render_ocean: bool,
    _acc_time: f32,
    _fft_seed: i32,
    _simulation_size: [f32; 4],
    _caustic_index: u32,
    _theoretic_slope_variance: f64,
    _total_slope_variance: f64,
    _fft_variance_framebuffers: Vec<FramebufferData>,
    _fft_wave_x_fft_a_framebuffer: FramebufferData,
    _fft_wave_x_fft_b_framebuffer: FramebufferData,
    _fft_wave_x_fft_a_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    _fft_wave_x_fft_b_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    _fft_wave_y_fft_a_framebuffer: FramebufferData,
    _fft_wave_y_fft_b_framebuffer: FramebufferData,
    _fft_wave_y_fft_a_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    _fft_wave_y_fft_b_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
    _fft_a_generate_mips_descriptor_sets: Layers<MipLevels<SwapchainArray<vk::DescriptorSet>>>,
    _fft_a_generate_mips_dispatch_group_x: u32,
    _fft_a_generate_mips_dispatch_group_y: u32,
    _render_fft_ocean_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
}

impl Default for FFTOcean {
    fn default() -> FFTOcean {
        let simulation_scale = 1.0;
        FFTOcean {
            _name: String::from("ocean"),
            _height: 0.0,
            _wind: WIND,
            _omega: OMEGA,
            _amplitude: AMPLITUDE,
            _simulation_wind: 1.0,
            _simulation_amplitude: 3.0,
            _simulation_scale: simulation_scale,
            _is_render_ocean: true,
            _acc_time: 0.0,
            _fft_seed: DEFAULT_FFT_SEED,
            _simulation_size: [GRID_SIZES[0] * simulation_scale, GRID_SIZES[1] * simulation_scale, GRID_SIZES[2] * simulation_scale, GRID_SIZES[3] * simulation_scale],
            _caustic_index: 0,
            _theoretic_slope_variance: 0.0,
            _total_slope_variance: 0.0,
            _fft_variance_framebuffers: Vec::new(),
            _fft_wave_x_fft_a_framebuffer: FramebufferData::default(),
            _fft_wave_x_fft_b_framebuffer: FramebufferData::default(),
            _fft_wave_x_fft_a_descriptor_sets: Vec::new(),
            _fft_wave_x_fft_b_descriptor_sets: Vec::new(),
            _fft_wave_y_fft_a_framebuffer: FramebufferData::default(),
            _fft_wave_y_fft_b_framebuffer: FramebufferData::default(),
            _fft_wave_y_fft_a_descriptor_sets: Vec::new(),
            _fft_wave_y_fft_b_descriptor_sets: Vec::new(),
            _fft_a_generate_mips_descriptor_sets: Vec::new(),
            _fft_a_generate_mips_dispatch_group_x: 1,
            _fft_a_generate_mips_dispatch_group_y: 1,
            _render_fft_ocean_descriptor_sets: Vec::new(),
        }
    }
}

fn log(t: f64) -> f64 {
    t.log(std::f64::consts::E)
}

fn sqr(x: f64) -> f64 {
    x * x
}

fn get_omega(k: f64) -> f64 {
    (9.81 * k * (1.0 + sqr(k / KM))).sqrt()
}

fn frandom(seed_data: i32) -> f64 {
    (seed_data >> (31 - 24)) as f64 / (1 << 24) as f64
}

fn bit_reverse(i: i32, n: i32) -> i32 {
    let mut sum: i32 = 0;
    let mut w: i32 = 1;
    let mut m: i32 = (n / 2) as i32;
    while 0 != m {
        if (i & m) > (m - 1) {
            sum += w;
        }
        w *= 2;
        m = (m / 2) as i32;
    }
    sum
}

fn compute_weight(n: i32, k: f64) -> (f64, f64) {
    ((2.0 * std::f64::consts::PI * k / n as f64).cos(), (2.0 * std::f64::consts::PI * k / n as f64).sin())
}

impl FFTOcean {
    pub fn get_height(&self) -> f32 {
        self._height
    }

    pub fn set_height(&mut self, sea_height: f32) {
        self._height = sea_height
    }

    pub fn regist_fft_ocean_textures(&mut self, renderer_context: &RendererContext, engine_resources: &mut EngineResources) {
        let mut spectrum12_data: Vec<f32> = vec![0.0; (FFT_SIZE * FFT_SIZE * 4) as usize];
        let mut spectrum34_data: Vec<f32> = vec![0.0; (FFT_SIZE * FFT_SIZE * 4) as usize];
        let mut butterfly_data: Vec<f32> = vec![0.0; (FFT_SIZE * PASSES * 4) as usize];

        self._fft_seed = DEFAULT_FFT_SEED;

        // create fft mesh & textures
        if false == engine_resources.has_mesh_data("fft_grid") {
            let fft_grid = geometry_buffer::plane_mesh_create_info(GRID_VERTEX_COUNT, GRID_VERTEX_COUNT, false);
            engine_resources.regist_mesh_data(&renderer_context, &String::from("fft_grid"), fft_grid);
        }

        self.generate_waves_spectrum(&mut spectrum12_data, &mut spectrum34_data);
        self.compute_butterfly_lookup_texture(&mut butterfly_data);

        if false == engine_resources.has_texture_data("fft_ocean/spectrum_1_2") {
            let texture_spectrum_1_2 = renderer_context.create_texture(&TextureCreateInfo {
                _texture_name: String::from("fft_ocean/spectrum_1_2"),
                _texture_width: FFT_SIZE as u32,
                _texture_height: FFT_SIZE as u32,
                _texture_format: vk::Format::R32G32B32A32_SFLOAT,
                _texture_min_filter: vk::Filter::NEAREST,
                _texture_mag_filter: vk::Filter::NEAREST,
                _texture_initial_datas: spectrum12_data.clone(),
                ..Default::default()
            });
            engine_resources.regist_texture_data(texture_spectrum_1_2._texture_data_name.clone(), newRcRefCell(texture_spectrum_1_2));
        }

        if false == engine_resources.has_texture_data("fft_ocean/spectrum_3_4") {
            let texture_spectrum_3_4 = renderer_context.create_texture(&TextureCreateInfo {
                _texture_name: String::from("fft_ocean/spectrum_3_4"),
                _texture_width: FFT_SIZE as u32,
                _texture_height: FFT_SIZE as u32,
                _texture_format: vk::Format::R32G32B32A32_SFLOAT,
                _texture_min_filter: vk::Filter::NEAREST,
                _texture_mag_filter: vk::Filter::NEAREST,
                _texture_initial_datas: spectrum34_data.clone(),
                ..Default::default()
            });
            engine_resources.regist_texture_data(texture_spectrum_3_4._texture_data_name.clone(), newRcRefCell(texture_spectrum_3_4));
        }

        if false == engine_resources.has_texture_data("fft_ocean/butterfly") {
            let texture_butterfly = renderer_context.create_texture(&TextureCreateInfo {
                _texture_name: String::from("fft_ocean/butterfly"),
                _texture_width: FFT_SIZE as u32,
                _texture_height: PASSES as u32,
                _texture_format: vk::Format::R32G32B32A32_SFLOAT,
                _texture_min_filter: vk::Filter::NEAREST,
                _texture_mag_filter: vk::Filter::NEAREST,
                _texture_wrap_mode: vk::SamplerAddressMode::CLAMP_TO_EDGE,
                _texture_initial_datas: butterfly_data.clone(),
                ..Default::default()
            });
            engine_resources.regist_texture_data(texture_butterfly._texture_data_name.clone(), newRcRefCell(texture_butterfly));
        }

        self._theoretic_slope_variance = 0.0;
        let mut k: f64 = 0.005;
        while k < 1000.0 {
            let next_k: f64 = k * 1.001;
            self._theoretic_slope_variance += k * k * self.spectrum(k, 0.0, true) * (next_k - k);
            k = next_k;
        }

        self._total_slope_variance = 0.0;
        for y in 0..FFT_SIZE as i32 {
            for x in 0..FFT_SIZE as i32 {
                let offset = 4 * (x + y * FFT_SIZE) as usize;
                let i: f32 = 2.0 * std::f32::consts::PI * (if (FFT_SIZE / 2) <= x { x - FFT_SIZE } else { x }) as f32;
                let j: f32 = 2.0 * std::f32::consts::PI * (if (FFT_SIZE / 2) <= y { y - FFT_SIZE } else { y }) as f32;
                self._total_slope_variance += self.get_slope_variance(i / GRID1_SIZE, j / GRID1_SIZE, spectrum12_data[offset    ], spectrum12_data[offset + 1]);
                self._total_slope_variance += self.get_slope_variance(i / GRID2_SIZE, j / GRID2_SIZE, spectrum12_data[offset + 2], spectrum12_data[offset + 3]);
                self._total_slope_variance += self.get_slope_variance(i / GRID3_SIZE, j / GRID3_SIZE, spectrum34_data[offset    ], spectrum34_data[offset + 1]);
                self._total_slope_variance += self.get_slope_variance(i / GRID4_SIZE, j / GRID4_SIZE, spectrum34_data[offset + 2], spectrum34_data[offset + 3]);
            }
        }
    }

    pub fn destroy_fft_ocean(&mut self, device: &Device) {
        for framebuffer_data in self._fft_variance_framebuffers.iter() {
            framebuffer::destroy_framebuffer_data(device, framebuffer_data);
        }
        framebuffer::destroy_framebuffer_data(device, &self._fft_wave_x_fft_a_framebuffer);
        framebuffer::destroy_framebuffer_data(device, &self._fft_wave_x_fft_b_framebuffer);
        framebuffer::destroy_framebuffer_data(device, &self._fft_wave_y_fft_a_framebuffer);
        framebuffer::destroy_framebuffer_data(device, &self._fft_wave_y_fft_b_framebuffer);

        self._fft_variance_framebuffers.clear();
        self._fft_wave_x_fft_a_descriptor_sets.clear();
        self._fft_wave_x_fft_b_descriptor_sets.clear();
        self._fft_wave_y_fft_a_descriptor_sets.clear();
        self._fft_wave_y_fft_b_descriptor_sets.clear();
        self._fft_a_generate_mips_descriptor_sets.clear();
        self._render_fft_ocean_descriptor_sets.clear();
    }

    pub fn prepare_framebuffer_and_descriptors(&mut self, renderer_data: &RendererData, engine_resources: &EngineResources) {
        let device = renderer_data.get_renderer_context().get_device();
        // fft Variance
        let material_instance = engine_resources.get_material_instance_data("fft_ocean/render_fft_ocean").borrow();
        let pipeline_binding_data = material_instance.get_pipeline_binding_data("render_fft_variance/render_fft_variance");
        let render_target = renderer_data.get_render_target(RenderTargetType::FFT_SLOPE_VARIANCE);
        let mip_level = 0;
        for layer in 0..render_target._image_layers {
            self._fft_variance_framebuffers.push(utility::create_framebuffer(device, &pipeline_binding_data.get_render_pass_data().borrow(), render_target, layer, mip_level, None))
        }

        // fft waves
        let mip_level = 0;
        let material_instance = engine_resources.get_material_instance_data("fft_ocean/render_fft_ocean").borrow();
        let texture_fft_a = renderer_data.get_render_target(RenderTargetType::FFT_A);
        let texture_fft_b = renderer_data.get_render_target(RenderTargetType::FFT_B);

        // fft wave x
        let pipeline_binding_data = material_instance.get_pipeline_binding_data("render_fft_waves/render_fft_x");
        self._fft_wave_x_fft_a_framebuffer = utility::create_framebuffer_2d_array(device, &pipeline_binding_data.get_render_pass_data().borrow(), texture_fft_a, mip_level, None);
        self._fft_wave_x_fft_b_framebuffer = utility::create_framebuffer_2d_array(device, &pipeline_binding_data.get_render_pass_data().borrow(), texture_fft_b, mip_level, None);
        let fft_waves_descriptor_binding_index = 1;
        self._fft_wave_x_fft_a_descriptor_sets = utility::create_descriptor_sets(
            device,
            pipeline_binding_data,
            &[(fft_waves_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_fft_a.get_default_image_info()))]
        );
        self._fft_wave_x_fft_b_descriptor_sets = utility::create_descriptor_sets(
            device,
            pipeline_binding_data,
            &[(fft_waves_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_fft_b.get_default_image_info()))]
        );

        // fft wave y
        let pipeline_binding_data = material_instance.get_pipeline_binding_data("render_fft_waves/render_fft_y");
        self._fft_wave_y_fft_a_framebuffer = utility::create_framebuffer_2d_array(device, &pipeline_binding_data.get_render_pass_data().borrow(), texture_fft_a, mip_level, None);
        self._fft_wave_y_fft_b_framebuffer = utility::create_framebuffer_2d_array(device, &pipeline_binding_data.get_render_pass_data().borrow(), texture_fft_b, mip_level, None);
        self._fft_wave_y_fft_a_descriptor_sets = utility::create_descriptor_sets(
            device,
            pipeline_binding_data,
            &[(fft_waves_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_fft_a.get_default_image_info()))]
        );
        self._fft_wave_y_fft_b_descriptor_sets = utility::create_descriptor_sets(
            device,
            pipeline_binding_data,
            &[(fft_waves_descriptor_binding_index, utility::create_descriptor_image_info_swapchain_array(texture_fft_b.get_default_image_info()))]
        );

        // fft a generate mips
        let downsampling_material_instance = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let pipeline_binding_data = downsampling_material_instance.get_default_pipeline_binding_data();
        let layer_count = texture_fft_a._image_layers;
        let dispatch_count: u32 = texture_fft_a._image_mip_levels - 1;
        for layer in 0..layer_count {
            self._fft_a_generate_mips_descriptor_sets.push(Vec::new());
            for mip_level in 0..dispatch_count {
                let descriptor_sets = utility::create_descriptor_sets(
                    device,
                    pipeline_binding_data,
                    &[
                        (0, utility::create_descriptor_image_info_swapchain_array(texture_fft_a.get_sub_image_info(layer, mip_level))),
                        (1, utility::create_descriptor_image_info_swapchain_array(texture_fft_a.get_sub_image_info(layer, mip_level + 1))),
                    ]
                );
                self._fft_a_generate_mips_descriptor_sets.last_mut().unwrap().push(descriptor_sets);
            }
        }
        self._fft_a_generate_mips_dispatch_group_x = texture_fft_a._image_width;
        self._fft_a_generate_mips_dispatch_group_y = texture_fft_a._image_height;
    }

    fn get_slope_variance(&self, kx: f32, ky: f32, spectrum_sample0: f32, spectrum_sample1: f32) -> f64 {
        let k_square = (kx * kx + ky * ky) as f64;
        let real = spectrum_sample0 as f64;
        let img = spectrum_sample1 as f64;
        let h_square = real * real + img * img;
        k_square * h_square * 2.0
    }

    #[allow(non_snake_case)]
    fn spectrum(&self, kx: f64, ky: f64, omnispectrum: bool) -> f64 {
        let U10: f64 = (self._wind as f64).max(0.001);
        let Omega: f64 = self._omega as f64;
        let Amp: f64 = self._amplitude as f64;

        let k = (kx * kx + ky * ky).sqrt();
        let c = get_omega(k) / k;

        // spectral peak
        let kp = 9.81 * sqr(Omega / U10);
        let cp = get_omega(kp) / kp;

        // friction velocity
        let z0 = 3.7e-5 * sqr(U10) / 9.81 * (U10 / cp).powf(0.9);
        let u_star = 0.41 * U10 / log(10.0 / z0);

        let Lpm = (-5.0 / 4.0 * sqr(kp / k)).exp();
        let gamma = if Omega < 1.0 { 1.7 } else { 1.7 + 6.0 * log(Omega) };
        let sigma = 0.08 * (1.0 + 4.0 / Omega.powf(3.0));
        let Gamma = (-1.0 / (2.0 * sqr(sigma)) * sqr((k / kp).sqrt() - 1.0)).exp();
        let Jp = gamma.powf(Gamma);
        let Fp = Lpm * Jp * (- Omega / (10.0f64).sqrt() * ((k / kp).sqrt() - 1.0)).exp();
        let alphap = 0.006 * Omega.sqrt();
        let mut Bl = 0.5 * alphap * cp / c * Fp;
        let mut alpham = 0.01;
        if u_star < CM {
            alpham *= 1.0 + log(u_star / CM);
        } else {
            alpham *= 1.0 + 3.0 * log(u_star / CM);
        }

        let Fm = (-0.25 * sqr(k / KM - 1.0)).exp();
        let mut Bh = 0.5 * alpham * CM / c * Fm * Lpm;

        if omnispectrum {
            return Amp * (Bl + Bh) / (k * sqr(k));
        }

        let a0 = log(2.0) / 4.0;
        let ap = 4.0;
        let am = 0.13 * u_star / CM;
        let Delta = (a0 + ap * (c / cp).powf(2.5) + am * (CM / c).powf(2.5)).tanh();
        let phi = ky.atan2(kx);

        if kx < 0.0 {
            return 0.0;
        } else {
            Bl *= 2.0;
            Bh *= 2.0;
        }

        Amp * (Bl + Bh) * (1.0 + Delta * (2.0 * phi).cos()) / (2.0 * std::f64::consts::PI * sqr(sqr(k)))
    }

    fn get_spectrum_sample(&mut self, i: i32, j: i32, length_scale: f64, k_min: f64) -> (f64, f64) {
        let dk = 2.0 * std::f64::consts::PI / length_scale;
        let kx = i as f64 * dk;
        let ky = j as f64 * dk;
        if kx.abs() < k_min && ky.abs() < k_min {
            return (0.0, 0.0);
        }

        let s = self.spectrum(kx, ky, false);
        let h = (s / 2.0).sqrt() * dk;
        self._fft_seed = (self._fft_seed * 1103515245 + 12345) & 0x7FFFFFFF;
        let phi = frandom(self._fft_seed) * 2.0 * std::f64::consts::PI;
        (h * phi.cos(), h * phi.sin())
    }

    fn compute_butterfly_lookup_texture(&self, butterfly_data: &mut Vec<f32>) {
        for i in 0..PASSES {
            let blocks: i32 = (2.0f64).powf((PASSES - 1 - i) as f64) as i32;
            let inputs: i32 = (2.0f64).powf(i as f64) as i32;
            for j in 0..blocks {
                for k in 0..inputs {
                    let i1: i32;
                    let i2: i32;
                    let j1: i32;
                    let j2: i32;
                    if i == 0 {
                        i1 = j * inputs * 2 + k;
                        i2 = j * inputs * 2 + inputs + k;
                        j1 = bit_reverse(i1 as i32, FFT_SIZE as i32);
                        j2 = bit_reverse(i2 as i32, FFT_SIZE as i32);
                    } else {
                        i1 = j * inputs * 2 + k;
                        i2 = j * inputs * 2 + inputs + k;
                        j1 = i1;
                        j2 = i2;
                    }

                    let (wr, wi) = compute_weight(FFT_SIZE as i32, (k * blocks) as f64);

                    let offset1 = 4 * (i1 as usize + (i * FFT_SIZE) as usize);
                    butterfly_data[offset1 + 0] = ((j1 as f64 + 0.5) / FFT_SIZE as f64) as f32;
                    butterfly_data[offset1 + 1] = ((j2 as f64 + 0.5) / FFT_SIZE as f64) as f32;
                    butterfly_data[offset1 + 2] = wr as f32;
                    butterfly_data[offset1 + 3] = wi as f32;

                    let offset2 = 4 * (i2 as usize + (i * FFT_SIZE) as usize);
                    butterfly_data[offset2 + 0] = ((j1 as f64 + 0.5) / FFT_SIZE as f64) as f32;
                    butterfly_data[offset2 + 1] = ((j2 as f64 + 0.5) / FFT_SIZE as f64) as f32;
                    butterfly_data[offset2 + 2] = -wr as f32;
                    butterfly_data[offset2 + 3] = -wi as f32;
                }
            }
        }
    }

    fn generate_waves_spectrum(&mut self, spectrum12_data: &mut Vec<f32>, spectrum34_data: &mut Vec<f32>) {
        for y in 0..FFT_SIZE {
            for x in 0..FFT_SIZE {
                let offset = 4 * (x + y * FFT_SIZE) as usize;
                let i: i32 = if (FFT_SIZE / 2) <= x { x - FFT_SIZE } else { x };
                let j: i32 = if (FFT_SIZE / 2) <= y { y - FFT_SIZE } else { y };
                let (s12_0, s12_1) = self.get_spectrum_sample(i, j, GRID1_SIZE as f64, std::f64::consts::PI / GRID1_SIZE as f64);
                let (s12_2, s12_3) = self.get_spectrum_sample(i, j, GRID2_SIZE as f64, std::f64::consts::PI * FFT_SIZE as f64 / GRID1_SIZE as f64);
                let (s34_0, s34_1) = self.get_spectrum_sample(i, j, GRID3_SIZE as f64, std::f64::consts::PI * FFT_SIZE as f64 / GRID2_SIZE as f64);
                let (s34_2, s34_3) = self.get_spectrum_sample(i, j, GRID4_SIZE as f64, std::f64::consts::PI * FFT_SIZE as f64 / GRID3_SIZE as f64);
                spectrum12_data[offset + 0] = s12_0 as f32;
                spectrum12_data[offset + 1] = s12_1 as f32;
                spectrum12_data[offset + 2] = s12_2 as f32;
                spectrum12_data[offset + 3] = s12_3 as f32;
                spectrum34_data[offset + 0] = s34_0 as f32;
                spectrum34_data[offset + 1] = s34_1 as f32;
                spectrum34_data[offset + 2] = s34_2 as f32;
                spectrum34_data[offset + 3] = s34_3 as f32;
            }
        }
    }

    pub fn compute_slope_variance_texture(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources,
    ) {
        // fft variance
        let material_instance_data = engine_resources.get_material_instance_data("fft_ocean/render_fft_ocean").borrow();
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_fft_variance/render_fft_variance");
        let framebuffer_count = self._fft_variance_framebuffers.len();
        let mut push_constants = PushConstant_FFT_Ocean {
            _grid_sizes: GRID_SIZES,
            _n_slope_variance: N_SLOPE_VARIANCE as f32,
            _fft_size: FFT_SIZE,
            _slope_variance_delta: ((self._theoretic_slope_variance - self._total_slope_variance) * 0.5) as f32,
            _c: 0.0,
            ..Default::default()
        };

        for i in 0..framebuffer_count {
            push_constants._c = i as f32;

            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                &quad_geometry_data,
                Some(&self._fft_variance_framebuffers[i]),
                None,
                Some(&push_constants),
            );
        }
    }

    pub fn update(&mut self, delta_time: f64) {
        self._acc_time += delta_time as f32;
        let caustic_count = self._render_fft_ocean_descriptor_sets.len();
        if 0 < caustic_count {
            self._caustic_index = (self._acc_time * 20.0) as u32 % self._render_fft_ocean_descriptor_sets.len() as u32;
        }
    }

    pub fn simulate_fft_waves(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        quad_geometry_data: &GeometryData,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources,
    ) {
        let material_instance_data = engine_resources.get_material_instance_data("fft_ocean/render_fft_ocean").borrow();

        // fft init
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_fft_init/render_fft_init");
        let push_constants = PushConstant_FFT_Ocean {
            _inverse_grid_sizes: INVERSE_GRID_SIZES,
            _fft_size: FFT_SIZE,
            _t: self._acc_time * self._simulation_wind,
            ..Default::default()
        };
        renderer_context.render_render_pass_pipeline(command_buffer, swapchain_index, pipeline_binding_data, &quad_geometry_data, None, None, Some(&push_constants));

        // fft wave x
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_fft_waves/render_fft_x");
        let mut push_constants = PushConstant_FFT_Ocean {
            _pass: 0.0,
            ..Default::default()
        };
        for i in 0..PASSES {
            push_constants._pass = (i as f32 + 0.5) / PASSES as f32;

            let (framebuffer, descriptor_sets) = if 0 == (i % 2) {
                (&self._fft_wave_x_fft_b_framebuffer, &self._fft_wave_x_fft_a_descriptor_sets)
            } else {
                (&self._fft_wave_x_fft_a_framebuffer, &self._fft_wave_x_fft_b_descriptor_sets)
            };

            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                &quad_geometry_data,
                Some(framebuffer),
                Some(descriptor_sets),
                Some(&push_constants)
            );
        }

        // fft wave y
        let pipeline_binding_data = material_instance_data.get_pipeline_binding_data("render_fft_waves/render_fft_y");
        for i in PASSES..(PASSES * 2) {
            push_constants._pass = ((i - PASSES) as f32 + 0.5) / PASSES as f32;

            let (framebuffer, descriptor_sets) = if 0 == (i % 2) {
                (&self._fft_wave_y_fft_b_framebuffer, &self._fft_wave_y_fft_a_descriptor_sets)
            } else {
                (&self._fft_wave_y_fft_a_framebuffer, &self._fft_wave_y_fft_b_descriptor_sets)
            };

            renderer_context.render_render_pass_pipeline(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                &quad_geometry_data,
                Some(framebuffer),
                Some(descriptor_sets),
                Some(&push_constants)
            );
        }

        // fft a generate mips
        let material_instance_data = engine_resources.get_material_instance_data("common/downsampling").borrow();
        let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
        let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
        renderer_context.begin_compute_pipeline(command_buffer, pipeline_data);
        let layer_count = self._fft_a_generate_mips_descriptor_sets.len();
        for layer in 0..layer_count {
            let mip_level_descriptor_sets = &self._fft_a_generate_mips_descriptor_sets[layer];
            let mip_levels = mip_level_descriptor_sets.len();
            for mip_level in 0..mip_levels {
                let descriptor_sets = Some(&mip_level_descriptor_sets[mip_level]);
                renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, descriptor_sets);
                renderer_context.dispatch_compute_pipeline(
                    command_buffer,
                    max(1, self._fft_a_generate_mips_dispatch_group_x >> (mip_level + 1)),
                    max(1, self._fft_a_generate_mips_dispatch_group_y >> (mip_level + 1)),
                    1
                );
            }
        }
    }

    pub fn render_ocean(
        &self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources,
    ) {
        let fft_grid_mesh = engine_resources.get_mesh_data("fft_grid").borrow();
        let fft_grid = fft_grid_mesh.get_default_geometry_data().borrow();
        let push_constant = PushConstant_FFT_Ocean {
            _simulation_size: self._simulation_size.clone(),
            _cell_size: GRID_CELL_SIZE.clone(),
            _simulation_wind: self._simulation_wind,
            _simulation_amplitude: self._simulation_amplitude,
            _t: self._acc_time * self._simulation_wind,
            ..Default::default()
        };
        renderer_context.render_material_instance(command_buffer, swapchain_index, "fft_ocean/render_fft_ocean", "render_fft_ocean/render_fft_ocean", &fft_grid, None, None, Some(&push_constant));
    }
}