use std::collections::HashMap;

use ash::{Device, vk};
use ash::ext;

use crate::vulkan_context::texture;

#[derive(Clone, Debug)]
pub struct ImageSamplerData {
    _samplers: HashMap<String, vk::Sampler>
}

impl Default for ImageSamplerData {
    fn default() -> ImageSamplerData {
        ImageSamplerData {
            _samplers: HashMap::new()
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

    pub fn initialize_image_samplers(&mut self, device: &Device, debug_utils_device: &ext::debug_utils::Device) {
        let mip_levels = 16; // log2(65536)
        self.create_image_sampler(
            device,
            debug_utils_device,
            "point_clamp",
            mip_levels,
            vk::Filter::NEAREST,
            vk::Filter::NEAREST,
            vk::SamplerAddressMode::CLAMP_TO_EDGE,
            vk::FALSE,
        );
        self.create_image_sampler(
            device,
            debug_utils_device,
            "point_repeat",
            mip_levels,
            vk::Filter::NEAREST,
            vk::Filter::NEAREST,
            vk::SamplerAddressMode::REPEAT,
            vk::FALSE,
        );
        self.create_image_sampler(
            device,
            debug_utils_device,
            "linear_clamp",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::CLAMP_TO_EDGE,
            vk::FALSE,
        );
        self.create_image_sampler(
            device,
            debug_utils_device,
            "linear_repeat",
            mip_levels,
            vk::Filter::LINEAR,
            vk::Filter::LINEAR,
            vk::SamplerAddressMode::REPEAT,
            vk::FALSE,
        );
    }

    pub fn destroy_image_samplers(&mut self, device: &Device) {
        for (_key, sampler) in self._samplers.iter() {
            texture::destroy_image_sampler(device, *sampler);
        }
        self._samplers.clear();
    }

    pub fn get_sampler_from_str(&self, sampler_name: &str) -> &vk::Sampler {
        self._samplers.get(sampler_name).unwrap()
    }
}

pub fn create_image_samplers(device: &Device, debug_utils_device: &ext::debug_utils::Device) -> ImageSamplerData {
    let mut image_sampler_data = ImageSamplerData::default();
    image_sampler_data.initialize_image_samplers(device, debug_utils_device);
    image_sampler_data
}