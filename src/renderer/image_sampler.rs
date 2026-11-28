use std::collections::HashMap;

use ash::ext;
use ash::{Device, vk};

use crate::vulkan_context::texture;

// Global immutable sampler indices (must match SAMPLER_* defines in scene_constants.glsl)
pub const GLOBAL_SAMPLER_LINEAR_REPEAT: usize = 0;
pub const GLOBAL_SAMPLER_LINEAR_CLAMP: usize = 1;
pub const GLOBAL_SAMPLER_POINT_REPEAT: usize = 2;
pub const GLOBAL_SAMPLER_POINT_CLAMP: usize = 3;
pub const GLOBAL_SAMPLER_ANISO_REPEAT: usize = 4;
pub const GLOBAL_SAMPLER_ANISO_CLAMP: usize = 5;
pub const GLOBAL_SAMPLER_COUNT: usize = 6;
pub const GLOBAL_SAMPLERS_DESCRIPTOR_NAME: &str = "g_samplers";

#[derive(Clone, Debug)]
pub struct ImageSamplerData {
    _samplers: HashMap<String, vk::Sampler>,
    _global_samplers: [vk::Sampler; GLOBAL_SAMPLER_COUNT],
}

impl Default for ImageSamplerData {
    fn default() -> ImageSamplerData {
        ImageSamplerData {
            _samplers: HashMap::new(),
            _global_samplers: [vk::Sampler::null(); GLOBAL_SAMPLER_COUNT],
        }
    }
}

impl ImageSamplerData {
    fn create_image_sampler(
        &mut self,
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        sampler_name: &str,
        mip_levels: u32,
        min_filter: vk::Filter,
        mag_filter: vk::Filter,
        sampler_address_mode: vk::SamplerAddressMode,
        anisotropy_enable: vk::Bool32,
    ) -> vk::Sampler {
        let sampler = texture::create_image_sampler(
            device,
            debug_utils_device,
            sampler_name,
            mip_levels,
            min_filter,
            mag_filter,
            sampler_address_mode,
            anisotropy_enable,
        );

        self._samplers.insert(String::from(sampler_name), sampler);

        sampler
    }

    pub fn initialize_image_samplers(
        &mut self,
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        enable_anisotropy: bool,
    ) {
        let mip_levels = 16; // log2(65536)
        let aniso_flag = if enable_anisotropy { vk::TRUE } else { vk::FALSE };

        let linear_repeat = self.create_image_sampler(
            device,
            debug_utils_device,
            "linear_repeat",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::REPEAT,
            vk::FALSE,
        );
        let linear_clamp = self.create_image_sampler(
            device,
            debug_utils_device,
            "linear_clamp",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::CLAMP_TO_EDGE,
            vk::FALSE,
        );
        let point_repeat = self.create_image_sampler(
            device,
            debug_utils_device,
            "point_repeat",
            mip_levels,
            vk::Filter::NEAREST,
            vk::Filter::NEAREST,
            vk::SamplerAddressMode::REPEAT,
            vk::FALSE,
        );
        let point_clamp = self.create_image_sampler(
            device,
            debug_utils_device,
            "point_clamp",
            mip_levels,
            vk::Filter::NEAREST,
            vk::Filter::NEAREST,
            vk::SamplerAddressMode::CLAMP_TO_EDGE,
            vk::FALSE,
        );
        let aniso_repeat = self.create_image_sampler(
            device,
            debug_utils_device,
            "aniso_repeat",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::REPEAT,
            aniso_flag,
        );
        let aniso_clamp = self.create_image_sampler(
            device,
            debug_utils_device,
            "aniso_clamp",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::CLAMP_TO_EDGE,
            aniso_flag,
        );

        self._global_samplers = [
            linear_repeat,
            linear_clamp,
            point_repeat,
            point_clamp,
            aniso_repeat,
            aniso_clamp,
        ];
    }

    pub fn destroy_image_samplers(&mut self, device: &Device) {
        for (_key, sampler) in self._samplers.iter() {
            texture::destroy_image_sampler(device, *sampler);
        }
        self._samplers.clear();
        self._global_samplers = [vk::Sampler::null(); GLOBAL_SAMPLER_COUNT];
    }

    pub fn get_sampler_from_str(&self, sampler_name: &str) -> &vk::Sampler {
        self._samplers.get(sampler_name).unwrap()
    }

    pub fn get_global_samplers(&self) -> &[vk::Sampler; GLOBAL_SAMPLER_COUNT] {
        &self._global_samplers
    }
}

pub fn create_image_samplers(
    device: &Device,
    debug_utils_device: &ext::debug_utils::Device,
    enable_anisotropy: bool,
) -> ImageSamplerData {
    let mut image_sampler_data = ImageSamplerData::default();
    image_sampler_data.initialize_image_samplers(device, debug_utils_device, enable_anisotropy);
    image_sampler_data
}
