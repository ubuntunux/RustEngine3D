use std::cmp::max;
use ash::{
    vk,
    Device,
    Instance,
};
use ash::extensions::khr::{
    Surface,
    Swapchain,
};
use ash::version::{DeviceV1_0, InstanceV1_0};

use crate::constants;

#[derive(Debug, Clone)]
pub struct TextureCreateInfo {
    _texture_create_info_width: u32,
    _texture_create_info_height: u32,
    _texture_create_info_depth: u32,
    _texture_create_info_format: vk::Format,
    _texture_create_info_view_type: vk::ImageViewType,
    _texture_create_info_samples: vk::SampleCountFlags,
    _texture_create_info_min_filter: vk::Filter,
    _texture_create_info_mag_filter: vk::Filter,
    _texture_create_info_wrap_mode: vk::SamplerAddressMode,
    _texture_create_info_enable_mipmap: bool,
    _texture_create_info_enable_anisotropy: bool,
    _texture_create_info_immutable: bool,
    _texture_create_info_data: Vec<u8>
}

#[derive(Debug, Clone)]
pub struct TextureData {
    _texture_data_name: String,
    _image: vk::Image,
    _image_view: vk::ImageView,
    _image_memory: vk::DeviceMemory,
    _image_sampler:vk::Sampler,
    _image_format: vk::Format,
    _image_width: u32,
    _image_height: u32,
    _image_depth: u32,
    _image_mip_levels: u32,
    _image_sample_count: vk::SampleCountFlags,
    _descriptor_image_info: vk::DescriptorImageInfo,
}

#[allow(non_camel_case)]
pub enum ImageLayoutTransition {
    TransferUndefToTransferDst,
    TransferDstToShaderReadOnly,
    TransferUndefToDepthStencilAttachemnt,
    TransferUndefToColorAttachemnt,
}

#[derive(Debug, Clone)]
pub struct TransitionDependent {
    _old_layout: vk::ImageLayout,
    _new_layout: vk::ImageLayout,
    _src_access_mask: vk::AccessFlags,
    _dst_access_mask: vk::AccessFlags,
    _src_stage_mask: vk::PipelineStageFlags,
    _dst_stage_mask: vk::PipelineStageFlags,
}

impl Default for TextureCreateInfo {
    fn default() -> TextureCreateInfo {
        TextureCreateInfo {
            _texture_create_info_width: 1,
            _texture_create_info_height: 1,
            _texture_create_info_depth: 1,
            _texture_create_info_format: vk::Format::R8G8B8A8_UNORM,
            _texture_create_info_view_type: vk::ImageViewType::TYPE_2D,
            _texture_create_info_samples: vk::SampleCountFlags::TYPE_1,
            _texture_create_info_min_filter: vk::Filter::LINEAR,
            _texture_create_info_mag_filter: vk::Filter::LINEAR,
            _texture_create_info_wrap_mode: vk::SamplerAddressMode::REPEAT,
            _texture_create_info_enable_mipmap: true,
            _texture_create_info_enable_anisotropy: true,
            _texture_create_info_immutable: true,
            _texture_create_info_data: Vec::new(),
        }
    }
}

pub fn get_transition_dependent(image_layout_transition: ImageLayoutTransition) -> TransitionDependent {
    match image_layout_transition {
        ImageLayoutTransition::TransferUndefToTransferDst => TransitionDependent {
            _old_layout: vk::ImageLayout::UNDEFINED,
            _new_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
            _src_access_mask: vk::AccessFlags::empty(),
            _dst_access_mask: vk::AccessFlags::TRANSFER_WRITE,
            _src_stage_mask: vk::PipelineStageFlags::TOP_OF_PIPE,
            _dst_stage_mask: vk::PipelineStageFlags::TRANSFER,
        },
        ImageLayoutTransition::TransferDstToShaderReadOnly => TransitionDependent {
            _old_layout: vk::ImageLayout::TRANSFER_DST_OPTIMAL,
            _new_layout: vk::ImageLayout::SHADER_READ_ONLY_OPTIMAL,
            _src_access_mask: vk::AccessFlags::TRANSFER_WRITE,
            _dst_access_mask: vk::AccessFlags::SHADER_READ,
            _src_stage_mask: vk::PipelineStageFlags::TRANSFER,
            _dst_stage_mask: vk::PipelineStageFlags::FRAGMENT_SHADER,
        },
        ImageLayoutTransition::TransferUndefToDepthStencilAttachemnt => TransitionDependent {
            _old_layout: vk::ImageLayout::UNDEFINED,
            _new_layout: vk::ImageLayout::DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
            _src_access_mask: vk::AccessFlags::empty(),
            _dst_access_mask: vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_READ | vk::AccessFlags::DEPTH_STENCIL_ATTACHMENT_WRITE,
            _src_stage_mask: vk::PipelineStageFlags::TOP_OF_PIPE,
            _dst_stage_mask: vk::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
        },
        ImageLayoutTransition::TransferUndefToColorAttachemnt => TransitionDependent {
            _old_layout: vk::ImageLayout::UNDEFINED,
            _new_layout: vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL,
            _src_access_mask: vk::AccessFlags::empty(),
            _dst_access_mask: vk::AccessFlags::COLOR_ATTACHMENT_READ | vk::AccessFlags::COLOR_ATTACHMENT_WRITE,
            _src_stage_mask: vk::PipelineStageFlags::TOP_OF_PIPE,
            _dst_stage_mask: vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
        },
    }
}

pub fn image_view_type_to_image_type(image_view_type: vk::ImageViewType) -> vk::ImageType {
    match image_view_type {
        vk::ImageViewType::TYPE_1D => vk::ImageType::TYPE_1D,
        vk::ImageViewType::TYPE_2D => vk::ImageType::TYPE_2D,
        vk::ImageViewType::TYPE_2D_ARRAY => vk::ImageType::TYPE_2D,
        vk::ImageViewType::CUBE => vk::ImageType::TYPE_2D,
        vk::ImageViewType::CUBE_ARRAY => vk::ImageType::TYPE_2D,
        vk::ImageViewType::TYPE_3D => vk::ImageType::TYPE_3D,
        _ => vk::ImageType::TYPE_2D,
    }
}

pub fn next_mipmap_size(n: i32) -> i32 {
    if 1 < n { n/2 } else { 1 }
}

pub fn calc_mip_levels(image_width: u32, image_height: u32, image_depth: u32) -> u32 {
    let max_size: f32 = max(image_width, max(image_height, image_depth)) as f32;
    max_size.log2().floor() as u32 + 1
}

pub fn image_barrier_struct(
    image: vk::Image,
    aspect_mask: vk::ImageAspectFlags,
    mip_level: u32,
    layer_count: u32,
    old_layout: vk::ImageLayout,
    new_layout: vk::ImageLayout,
    src_access_mask: vk::AccessFlags,
    dst_access_mask: vk::AccessFlags,
) -> vk::ImageMemoryBarrier {
    vk::ImageMemoryBarrier {
        old_layout,
        new_layout,
        src_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
        dst_queue_family_index: vk::QUEUE_FAMILY_IGNORED,
        image,
        subresource_range: vk::ImageSubresourceRange {
            aspect_mask, // vk::ImageAspectFlags::COLOR,
            base_mip_level: mip_level,
            level_count: 1,
            base_array_layer: 0,
            layer_count,
            ..Default::default()
        },
        src_access_mask,
        dst_access_mask,
        ..Default::default()
    }
}

pub fn image_blit_struct(
    image_aspect_mask: vk::ImageAspectFlags,
    mip_level: u32,
    src_width: i32,
    src_height: i32,
    src_depth: i32,
    layer_count: u32
) -> vk::ImageBlit {
    vk::ImageBlit {
        src_offsets: [
            vk::Offset3D { x: 0, y: 0, z: 0 },
            vk::Offset3D { x: src_width, y: src_height, z: src_depth }
        ],
        dst_offsets: [
            vk::Offset3D { x: 0, y: 0, z: 0 },
            vk::Offset3D {
                x: next_mipmap_size(src_width),
                y: next_mipmap_size(src_height),
                z: next_mipmap_size(src_depth)
            }
        ],
        src_subresource: vk::ImageSubresourceLayers {
            aspect_mask: image_aspect_mask,
            mip_level: (mip_level - 1),
            base_array_layer: 0,
            layer_count
        },
        dst_subresource: vk::ImageSubresourceLayers {
            aspect_mask: image_aspect_mask,
            mip_level,
            base_array_layer: 0,
            layer_count
        },
        ..Default::default()
    }
}

pub fn create_mipmap(
    device: &Device,
    command_buffer: vk::CommandBuffer,
    image: vk::Image,
    aspect_mask: vk::ImageAspectFlags,
    mip_level: u32,
    src_width: i32,
    src_height: i32,
    src_depth: i32,
    layer_count: u32
) {
    unsafe {
        let barriers: [vk::ImageMemoryBarrier; 1] = [
            image_barrier_struct(
                image,
                aspect_mask,
                (mip_level - 1),
                layer_count,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                vk::AccessFlags::TRANSFER_WRITE,
                vk::AccessFlags::TRANSFER_READ,
            )
        ];
        device.cmd_pipeline_barrier(
            command_buffer,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::TRANSFER,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &barriers
        );

        let image_blits = [
            image_blit_struct(
                aspect_mask,
                mip_level,
                src_width,
                src_height,
                src_depth,
                layer_count
            )
        ];
        device.cmd_blit_image(
            command_buffer,
            image,
            vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
            image,
            vk::ImageLayout::TRANSFER_DST_OPTIMAL,
            &image_blits,
            vk::Filter::LINEAR
        );

        let barriers: [vk::ImageMemoryBarrier; 1] = [
            image_barrier_struct(
                image,
                aspect_mask,
                (mip_level - 1),
                layer_count,
                vk::ImageLayout::TRANSFER_SRC_OPTIMAL,
                vk::ImageLayout::SHADER_READ_ONLY_OPTIMAL,
                vk::AccessFlags::TRANSFER_READ,
                vk::AccessFlags::SHADER_READ,
            )
        ];
        device.cmd_pipeline_barrier(
            command_buffer,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::FRAGMENT_SHADER,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &barriers
        );
    }
}

pub fn generate_mipmaps(
    instance: &Instance,
    device: &Device,
    physical_device: vk::PhysicalDevice,
    command_buffer: vk::CommandBuffer,
    image: vk::Image,
    aspect_mask: vk::ImageAspectFlags,
    format: vk::Format,
    width: i32,
    height: i32,
    depth: i32,
    mip_levels: u32,
    layer_count: u32,
) {
    unsafe {
        let format_properties: vk::FormatProperties = instance.get_physical_device_format_properties(physical_device, format);
        let supported = format_properties.optimal_tiling_features & vk::FormatFeatureFlags::SAMPLED_IMAGE_FILTER_LINEAR;
        if supported == vk::FormatFeatureFlags::empty() {
            panic!("texture image format does not support linear blitting!");
        }

        let mut mip_width: i32 = width;
        let mut mip_height: i32 = height;
        let mut mip_depth: i32 = depth;
        for mip_level in 1..mip_levels {
            mip_width = next_mipmap_size(mip_width);
            mip_height = next_mipmap_size(mip_height);
            mip_depth = next_mipmap_size(mip_depth);
            create_mipmap(
                device,
                command_buffer,
                image,
                aspect_mask,
                mip_level,
                mip_width,
                mip_height,
                mip_depth,
                layer_count
            );
        }

        let barriers: [vk::ImageMemoryBarrier; 1] = [
            image_barrier_struct(
                image,
                aspect_mask,
                (mip_levels - 1),
                layer_count,
                vk::ImageLayout::TRANSFER_DST_OPTIMAL,
                vk::ImageLayout::SHADER_READ_ONLY_OPTIMAL,
                vk::AccessFlags::TRANSFER_WRITE,
                vk::AccessFlags::SHADER_READ,
            )
        ];
        device.cmd_pipeline_barrier(
            command_buffer,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::FRAGMENT_SHADER,
            vk::DependencyFlags::empty(),
            &[],
            &[],
            &barriers
        );
    }
}

pub fn find_supported_format(
    instance: &Instance,
    physical_device: vk::PhysicalDevice,
    require_format: vk::Format,
    tiling: vk::ImageTiling,
    features: vk::FormatFeatureFlags,
) -> vk::Format {
    unsafe {
        let candidates: Vec<vk::Format> = constants::DEPTH_FOMATS.iter().filter(|format| {
            let format_properties = instance.get_physical_device_format_properties(physical_device, require_format);
            match tiling {
                vk::ImageTiling::LINEAR => (format_properties.linear_tiling_features & features) == features,
                vk::ImageTiling::OPTIMAL => (format_properties.optimal_tiling_features & features) == features,
                _ => false
            }
        }).map(|format| *format).collect();

        if 0 < candidates.len() {
            return if candidates.contains(&require_format) {
                require_format
            } else {
                candidates[0]
            }
        }
    }
    panic!("failed to find supported format");
}

pub fn create_image_sampler(
    device: &Device,
    mip_levels: u32,
    min_filter: vk::Filter,
    mag_filter: vk::Filter,
    sampler_address_mode: vk::SamplerAddressMode,
    anisotropy_enable: vk::Bool32
) -> vk::Sampler {
    let sampler_create_info = vk::SamplerCreateInfo {
        min_filter,
        mag_filter,
        address_mode_u: sampler_address_mode,
        address_mode_v: sampler_address_mode,
        address_mode_w: sampler_address_mode,
        anisotropy_enable,
        max_anisotropy: 16.0,
        border_color: vk::BorderColor::INT_OPAQUE_BLACK,
        unnormalized_coordinates: vk::FALSE,
        compare_enable: vk::FALSE,
        compare_op: vk::CompareOp::NEVER,
        mipmap_mode: vk::SamplerMipmapMode::LINEAR,
        mip_lod_bias: 0.0,
        min_lod: 0.0,
        max_lod: mip_levels as f32,
        ..Default::default()
    };

    unsafe {
        device.create_sampler(&sampler_create_info, None).expect("Failed to create sampler.")
    }
}

pub fn destroy_image_sampler(device: &Device, sampler: vk::Sampler) {
    unsafe {
        device.destroy_sampler(sampler, None);
    }
}

pub fn create_image_view(
    device: &Device,
    image: vk::Image,
    view_type:vk::ImageViewType,
    format: vk::Format,
    aspect_flags: vk::ImageAspectFlags,
    layer_count: u32,
    mip_levels: u32
) -> vk::ImageView {
    let create_view_info = vk::ImageViewCreateInfo::builder()
        .image(image)
        .view_type(view_type)
        .format(format)
        .components(vk::ComponentMapping {
            r: vk::ComponentSwizzle::IDENTITY,
            g: vk::ComponentSwizzle::IDENTITY,
            b: vk::ComponentSwizzle::IDENTITY,
            a: vk::ComponentSwizzle::IDENTITY,
        })
        .subresource_range(vk::ImageSubresourceRange {
            aspect_mask: aspect_flags,
            base_mip_level: 0,
            level_count: layer_count,
            base_array_layer: 0,
            layer_count: mip_levels,
        })
        .build();
    unsafe {
        device.create_image_view(&create_view_info, None).expect("vkCreateImageView failed!")
    }
}

pub fn destroy_image_view(device: &Device, image_view: vk::ImageView) {
    unsafe {
        device.destroy_image_view(image_view, None);
    }
}
//
// //
// transitionImageLayout :: vk::Image
//                       -> vk::Format
//                       -> ImageLayoutTransition
//                       -> u32
//                       -> u32
//                       -> vk::CommandBuffer
//                       -> IO ()
// transitionImageLayout image format transition layerCount mipLevels commandBuffer = do
//     let TransitionDependent {..} = transitionDependent transition
//         aspectMask = case _newLayout of
//             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
//                 | elem format Constants.depthStencilFormats -> VK_IMAGE_ASPECT_DEPTH_BIT .|. VK_IMAGE_ASPECT_STENCIL_BIT
//                 | otherwise -> VK_IMAGE_ASPECT_DEPTH_BIT
//             otherwise -> VK_IMAGE_ASPECT_COLOR_BIT
//         barrier = createvk:: @vk::ImageMemoryBarrier
//             $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
//             &* set @"pNext" VK_NULL
//             &* set @"oldLayout" _oldLayout
//             &* set @"newLayout" _newLayout
//             &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
//             &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
//             &* set @"image" image
//             &* setvk:: @"subresourceRange"
//                 (  set @"aspectMask" aspectMask
//                 &* set @"baseMipLevel" 0
//                 &* set @"levelCount" mipLevels
//                 &* set @"baseArrayLayer" 0
//                 &* set @"layerCount" layerCount)
//             &* set @"srcAccessMask" _srcAccessMask
//             &* set @"dstAccessMask" _dstAccessMask
//     withPtr barrier $ \barrierPtr -> vkCmdPipelineBarrier
//         commandBuffer
//         _srcStageMask
//         _dstStageMask
//         VK_ZERO_FLAGS
//         0 VK_NULL
//         0 VK_NULL
//         1 barrierPtr
// //
// //
// createImage :: vk::PhysicalDevice
//             -> vk::Device
//             -> vk::ImageType
//             -> u32
//             -> u32
//             -> u32
//             -> u32
//             -> u32
//             -> vk::SampleCountFlagBits
//             -> vk::Format
//             -> vk::ImageTiling
//             -> vk::ImageUsageFlags
//             -> vk::ImageCreateFlags
//             -> vk::MemoryPropertyFlags
//             -> IO (vk::DeviceMemory, vk::Image)
// createImage physicalDevice device imageType width height depth layerCount mipLevels samples format tiling usage imageCreateFlags memoryPropertyFlags = do
//     let imageCreateInfo = createvk:: @vk::ImageCreateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"flags" imageCreateFlags
//             &* set @"imageType" imageType
//             &* setvk:: @"extent"
//                 (  set @"width" width
//                 &* set @"height" height
//                 &* set @"depth" depth
//                 )
//             &* set @"mipLevels" mipLevels
//             &* set @"arrayLayers" layerCount
//             &* set @"format" format
//             &* set @"tiling" tiling
//             &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
//             &* set @"usage" usage
//             &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
//             &* set @"samples" samples
//             &* set @"queueFamilyIndexCount" 0
//             &* set @"pQueueFamilyIndices" VK_NULL
// //
//     imageFormatProperties <- allocaPeek $ \pImageFormatProperties ->
//         throwingVK "vkGetPhysicalDeviceImageFormatProperties failed!" $
//             vkGetPhysicalDeviceImageFormatProperties physicalDevice format imageType tiling usage imageCreateFlags pImageFormatProperties
// //
//     image <- withPtr imageCreateInfo $ \imageCreateInfoPtr -> allocaPeek $ \imagePtr ->
//         throwingVK "vkCreateImage failed!" $
//             vkCreateImage device imageCreateInfoPtr VK_NULL imagePtr
// //
//     memoryRequirements <- allocaPeek $ \memoryRequirementsPtr ->
//         vkGetImageMemoryRequirements device image memoryRequirementsPtr
// //
//     memoryType <- findMemoryType physicalDevice(getField @"memoryTypeBits" memoryRequirements) memoryPropertyFlags
// //
//     let allocInfo = createvk:: @vk::MemoryAllocateInfo
//             $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
//             &* set @"pNext" VK_NULL
//             &* set @"allocationSize" (getField @"size" memoryRequirements)
//             &* set @"memoryTypeIndex" memoryType
// //
//     imageMemory <- withPtr allocInfo $ \allocInfoPtr ->
//         allocaPeek $ \imageMemoryPtr ->
//             throwingVK "vkAllocateMemory failed!" $ vkAllocateMemory device allocInfoPtr VK_NULL imageMemoryPtr
// //
//     vkBindImageMemory device image imageMemory 0
// //
//     return (imageMemory, image)
// //
// destroyImage :: vk::Device -> vk::Image -> vk::DeviceMemory -> IO ()
// destroyImage device image imageMemory = do
//     vkDestroyImage device image VK_NULL
//     vkFreeMemory device imageMemory VK_NULL
// //
// copyBufferToImage :: vk::Device
//                   -> vk::CommandPool
//                   -> vk::Queue
//                   -> vk::Buffer
//                   -> vk::Image
//                   -> u32
//                   -> u32
//                   -> u32
//                   -> u32
//                   -> IO ()
// copyBufferToImage device commandBufferPool commandQueue buffer image width height depth layerCount =
//     runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
//         let region = createvk:: @vk::BufferImageCopy
//                 $  set @"bufferOffset" 0
//                 &* set @"bufferRowLength" 0
//                 &* set @"bufferImageHeight" 0
//                 &* setvk:: @"imageSubresource"
//                     (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
//                     &* set @"mipLevel" 0
//                     &* set @"baseArrayLayer" 0
//                     &* set @"layerCount" layerCount)
//                 &* setvk:: @"imageOffset"
//                     (  set @"x" 0
//                     &* set @"y" 0
//                     &* set @"z" 0)
//                 &* setvk:: @"imageExtent"
//                     (  set @"width" width
//                     &* set @"height" height
//                     &* set @"depth" depth)
//         in withPtr region $ \regionPtr ->
//             vkCmdCopyBufferToImage commandBuffer buffer image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 regionPtr
// //
// createRenderTarget :: String
//                    -> vk::PhysicalDevice
//                    -> vk::Device
//                    -> vk::CommandPool
//                    -> vk::Queue
//                    -> TextureCreateInfo
//                    -> IO TextureData
// createRenderTarget textureDataName physicalDevice device commandBufferPool queue textureCreateInfo@TextureCreateInfo {..} = do
//     let enableAnisotropy = if _textureCreateInfoEnableAnisotropy then VK_TRUE else VK_FALSE
//         textureCreateFlags = if (VK_IMAGE_VIEW_TYPE_CUBE == _textureCreateInfoViewType) then VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT else VK_ZERO_FLAGS
//         layerCount = if (VK_IMAGE_VIEW_TYPE_CUBE == _textureCreateInfoViewType) then 6 else 1
//         mipLevels = case _textureCreateInfoEnableMipmap of
//             True -> calcMipLevels _textureCreateInfoWidth _textureCreateInfoHeight _textureCreateInfoDepth
//             False -> 1
//         isDepthFormat = elem _textureCreateInfoFormat Constants.depthFomats
//         commonUsage = VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT .|. VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT
//         imageType = imageViewTypeToImageType _textureCreateInfoViewType
//     (imageUsage, imageAspect, imageLayoutTransition, imageFormat) <-
//         if isDepthFormat then do
//             depthFormat <- findSupportedFormat physicalDevice _textureCreateInfoFormat VK_IMAGE_TILING_OPTIMAL VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
//             return ( commonUsage .|. VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
//                    , VK_IMAGE_ASPECT_DEPTH_BIT
//                    , TransferUndef_DepthStencilAttachemnt
//                    , depthFormat
//                    )
//         else
//             return ( commonUsage .|. VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
//                    , VK_IMAGE_ASPECT_COLOR_BIT
//                    , TransferUndef_ColorAttachemnt
//                    , _textureCreateInfoFormat
//                    )
//     (imageMemory, image) <- createImage
//         physicalDevice
//         device
//         imageType
//         _textureCreateInfoWidth
//         _textureCreateInfoHeight
//         _textureCreateInfoDepth
//         layerCount
//         mipLevels
//         _textureCreateInfoSamples
//         imageFormat
//         VK_IMAGE_TILING_OPTIMAL
//         imageUsage
//         textureCreateFlags
//         VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
// //
//     runCommandsOnce device commandBufferPool queue $ \commandBuffer ->
//         transitionImageLayout image imageFormat imageLayoutTransition layerCount mipLevels commandBuffer
// //
//     imageView <- createImageView device image _textureCreateInfoViewType imageFormat imageAspect layerCount mipLevels
//     imageSampler <- createImageSampler device mipLevels _textureCreateInfoMinFilter _textureCreateInfoMagFilter _textureCreateInfoWrapMode enableAnisotropy
//     let descriptorImageInfo = createDescriptorImageInfo VK_IMAGE_LAYOUT_GENERAL imageView imageSampler
//         textureData = TextureData
//             { _textureDataName = textureDataName
//             , _imageView = imageView
//             , _image = image
//             , _imageMemory = imageMemory
//             , _imageSampler = imageSampler
//             , _imageFormat = imageFormat
//             , _imageWidth = _textureCreateInfoWidth
//             , _imageHeight = _textureCreateInfoHeight
//             , _imageDepth = _textureCreateInfoDepth
//             , _imageSampleCount = _textureCreateInfoSamples
//             , _imageMipLevels = mipLevels
//             , _descriptorImageInfo = descriptorImageInfo
//             }
//     log::info!("createRenderTarget : "
//         ++ Text.unpack textureDataName
//         ++ " " ++ show _textureCreateInfoViewType
//         ++ " " ++ show _textureCreateInfoFormat
//         ++ " "  ++ show _textureCreateInfoWidth
//         ++ ", " ++ show _textureCreateInfoHeight
//         ++ ", " ++ show _textureCreateInfoDepth
//     logTrivialInfo $ "    TextureData : image " ++ show image ++ ", imageView " ++ show imageView ++ ", imageMemory " ++ show imageMemory ++ ", sampler " ++ show imageSampler
//     return textureData
// //
// //
// createTextureData :: String
//                   -> vk::PhysicalDevice
//                   -> vk::Device
//                   -> vk::CommandPool
//                   -> vk::Queue
//                   -> TextureCreateInfo
//                   -> IO TextureData
// createTextureData textureDataName physicalDevice device commandBufferPool commandQueue textureCreateInfo@TextureCreateInfo {..} = do
//     let (imageDataForeignPtr, imageDataLen) = SVector.unsafeToForeignPtr0 _textureCreateInfoData
//         bufferSize = (fromIntegral imageDataLen)::vk::DeviceSize
//         enableAnisotropy = if _textureCreateInfoEnableAnisotropy then VK_TRUE else VK_FALSE
//         textureCreateFlags = if (VK_IMAGE_VIEW_TYPE_CUBE == _textureCreateInfoViewType) then VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT else VK_ZERO_FLAGS
//         layerCount = if (VK_IMAGE_VIEW_TYPE_CUBE == _textureCreateInfoViewType) then 6 else 1
//         mipLevels = case _textureCreateInfoEnableMipmap of
//             True -> calcMipLevels _textureCreateInfoWidth _textureCreateInfoHeight _textureCreateInfoDepth
//             False -> 1
//         imageType = imageViewTypeToImageType _textureCreateInfoViewType
//     -- we don't need to access the vk::DeviceMemory of the image, copyBufferToImage works with the vk::Image
//     (imageMemory, image) <- createImage
//         physicalDevice
//         device
//         imageType
//         _textureCreateInfoWidth
//         _textureCreateInfoHeight
//         _textureCreateInfoDepth
//         layerCount
//         mipLevels
//         _textureCreateInfoSamples
//         _textureCreateInfoFormat
//         VK_IMAGE_TILING_OPTIMAL
//         (VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
//         textureCreateFlags
//         VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
//     -- run command
//     runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
//         transitionImageLayout image _textureCreateInfoFormat TransferUndef_TransferDst layerCount mipLevels commandBuffer
// //
//     -- create temporary staging buffer
//     let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
//         stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
//     (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags
// //
//     -- upload data
//     stagingDataPtr <- allocaPeek $
//         vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
//     withForeignPtr imageDataForeignPtr $ \imageDataPtr ->
//         copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
//     vkUnmapMemory device stagingBufferMemory
// //
//     copyBufferToImage device commandBufferPool commandQueue stagingBuffer image _textureCreateInfoWidth _textureCreateInfoHeight _textureCreateInfoDepth layerCount
// //
//     runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
//         -- generateMipmaps does this as a side effect:
//         -- transitionImageLayout image VK_FORMAT_R8G8B8A8_UNORM TransferDst_ShaderReadOnly mipLevels
//         generateMipmaps
//             physicalDevice
//             image
//             _textureCreateInfoFormat
//             _textureCreateInfoWidth
//             _textureCreateInfoHeight
//             _textureCreateInfoDepth
//             mipLevels
//             layerCount
//             commandBuffer
//     destroyBuffer device stagingBuffer stagingBufferMemory
// //
//     imageView <- createImageView device image _textureCreateInfoViewType _textureCreateInfoFormat VK_IMAGE_ASPECT_COLOR_BIT layerCount mipLevels
//     imageSampler <- createImageSampler device mipLevels _textureCreateInfoMinFilter _textureCreateInfoMagFilter _textureCreateInfoWrapMode enableAnisotropy
//     let descriptorImageInfo = createDescriptorImageInfo VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL imageView imageSampler
//         textureData@TextureData {..} = TextureData
//             { _textureDataName = textureDataName
//             , _image = image
//             , _imageView = imageView
//             , _imageMemory = imageMemory
//             , _imageSampler = imageSampler
//             , _imageFormat = _textureCreateInfoFormat
//             , _imageWidth = _textureCreateInfoWidth
//             , _imageHeight = _textureCreateInfoHeight
//             , _imageDepth = _textureCreateInfoDepth
//             , _imageSampleCount = _textureCreateInfoSamples
//             , _imageMipLevels = fromIntegral mipLevels
//             , _descriptorImageInfo = descriptorImageInfo
//             }
// //
//     log::info!("createTextureData : "
//         ++ Text.unpack textureDataName
//         ++ " " ++ show _textureCreateInfoViewType
//         ++ " " ++ show _textureCreateInfoFormat
//         ++ " "  ++ show _textureCreateInfoWidth
//         ++ ", " ++ show _textureCreateInfoHeight
//         ++ ", " ++ show _textureCreateInfoDepth
//     logTrivialInfo $ "    TextureData : image " ++ show _image ++ ", imageView " ++ show _imageView ++ ", imageMemory " ++ show _imageMemory ++ ", sampler " ++ show _imageSampler
// //
//     return textureData
// //
// destroyTextureData :: vk::Device -> TextureData -> IO ()
// destroyTextureData device textureData@TextureData{..} = do
//     log::info!("destroyTextureData(" ++ (Text.unpack _textureDataName) ++ ") : image " ++ show _image ++ ", imageView " ++ show _imageView ++ ", imageMemory " ++ show _imageMemory ++ ", sampler " ++ show _imageSampler
//     destroyImageSampler device _imageSampler
//     destroyImageView device _imageView
//     destroyImage device _image _imageMemory