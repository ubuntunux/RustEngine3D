use ash::extensions::ext::DebugUtils;
use ash::{vk, Device};

use crate::vulkan_context::texture;

#[derive(Clone, Debug)]
pub struct ImageSamplerData {
    _point_clamp: vk::Sampler,
    _linear_clamp: vk::Sampler,
}

impl Default for ImageSamplerData {
    fn default() -> ImageSamplerData {
        ImageSamplerData {
            _point_clamp: vk::Sampler::null(),
            _linear_clamp: vk::Sampler::null(),
        }
    }
}

// TODO: replace individual image samplers
pub fn create_image_samplers(device: &Device, debug_utils: &DebugUtils) -> ImageSamplerData {
    let point_clamp = texture::create_image_sampler(
        device,
        debug_utils,
        "point_clamp",
        0,
        vk::Filter::NEAREST,
        vk::Filter::NEAREST,
        vk::SamplerAddressMode::CLAMP_TO_EDGE,
        vk::FALSE,
    );
    let linear_clamp = texture::create_image_sampler(
        device,
        debug_utils,
        "linear_clamp",
        0,
        vk::Filter::LINEAR,
        vk::Filter::LINEAR,
        vk::SamplerAddressMode::CLAMP_TO_EDGE,
        vk::FALSE,
    );
    ImageSamplerData {
        _point_clamp: point_clamp,
        _linear_clamp: linear_clamp,
    }
}

pub fn destroy_image_samplers(device: &Device, image_sampler_data: &ImageSamplerData) {
    texture::destroy_image_sampler(device, image_sampler_data._point_clamp);
    texture::destroy_image_sampler(device, image_sampler_data._linear_clamp);
}
