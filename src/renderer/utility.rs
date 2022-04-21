use ash::{ vk, Device };
use ash::vk::BaseOutStructure;

use crate::constants;
use crate::renderer::material_instance::{ PipelineBindingData };
use crate::vulkan_context::descriptor::{
    self,
    DescriptorResourceInfo,
};
use crate::vulkan_context::framebuffer::{ self, FramebufferData, RenderTargetInfo };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainArray;
use crate::vulkan_context::render_pass::RenderPassData;

pub fn create_swapchain_array<T: Clone>(a: T) -> SwapchainArray<T> {
    vec![a; constants::SWAPCHAIN_IMAGE_COUNT]
}

pub fn create_descriptor_image_info_swapchain_array(image_info: vk::DescriptorImageInfo) -> SwapchainArray<DescriptorResourceInfo> {
    vec![DescriptorResourceInfo::DescriptorImageInfo(image_info); constants::SWAPCHAIN_IMAGE_COUNT]
}

pub fn create_framebuffer(
    device: &Device,
    render_pass_data: &RenderPassData,
    render_target: &TextureData,
    render_target_layer: u32,
    render_target_miplevel: u32,
    clear_value: Option<vk::ClearValue>,
) -> FramebufferData {
    framebuffer::create_framebuffer_data(
        device,
        render_pass_data._render_pass,
        format!("{}_{}", render_pass_data._render_pass_data_name, render_target._texture_data_name).as_str(),
        framebuffer::create_framebuffer_data_create_info(
            &[RenderTargetInfo {
                _texture_data: render_target,
                _target_layer: render_target_layer,
                _target_mip_level: render_target_miplevel,
                _clear_value: clear_value,
            }],
            &[],
            &[]
        ),
    )
}

pub fn create_framebuffers(
    device: &Device,
    render_pass_data: &RenderPassData,
    framebuffer_name: &str,
    color_render_targets: &[RenderTargetInfo],
    depth_render_targets: &[RenderTargetInfo],
    resolve_render_targets: &[RenderTargetInfo],
) -> FramebufferData {
    framebuffer::create_framebuffer_data(
        device,
        render_pass_data._render_pass,
        format!("{}_{}", render_pass_data._render_pass_data_name, framebuffer_name).as_str(),
        framebuffer::create_framebuffer_data_create_info(color_render_targets, depth_render_targets, resolve_render_targets),
    )
}

pub fn create_framebuffer_2d_array(
    device: &Device,
    render_pass_data: &RenderPassData,
    render_target: &TextureData,
    render_target_miplevel: u32,
    clear_value: Option<vk::ClearValue>,
) -> FramebufferData {
    let render_target_infos: Vec<RenderTargetInfo> = (0..render_target._image_layers).map(|layer|
        RenderTargetInfo {
            _texture_data: render_target,
            _target_layer: layer,
            _target_mip_level: render_target_miplevel,
            _clear_value: clear_value,
        }
    ).collect();
    framebuffer::create_framebuffer_data(
        device,
        render_pass_data._render_pass,
        format!("{}_{}", render_pass_data._render_pass_data_name, render_target._texture_data_name).as_str(),
        framebuffer::create_framebuffer_data_create_info(
            &render_target_infos,
            &[],
            &[]
        ),
    )
}

pub fn create_descriptor_sets(
    device: &Device,
    pipeline_binding_data: &PipelineBindingData,
    descriptor_resource_infos_list: &[(usize, SwapchainArray<DescriptorResourceInfo>)]
) -> SwapchainArray<vk::DescriptorSet> {
    let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
    let descriptor_data = &pipeline_data._descriptor_data;
    let descriptor_binding_indices: Vec<u32> = descriptor_data._descriptor_data_create_infos.iter().map(|descriptor_data_create_info| {
        descriptor_data_create_info._descriptor_binding_index
    }).collect();
    let mut new_descriptor_resource_infos_list = pipeline_binding_data._descriptor_resource_infos_list.clone();
    for (descriptor_binding_index, descriptor_resource_infos) in descriptor_resource_infos_list {
        for (index, binding_index) in descriptor_binding_indices.iter().enumerate() {
            if (*binding_index) as usize == (*descriptor_binding_index) {
                for swapchain_index in constants::SWAPCHAIN_IMAGE_INDICES.iter() {
                    new_descriptor_resource_infos_list[*swapchain_index][index] = descriptor_resource_infos[*swapchain_index].clone();
                }
            }
        }
    }
    let descriptor_sets = descriptor::create_descriptor_sets(device, descriptor_data);
    let _write_descriptor_sets: SwapchainArray<Vec<vk::WriteDescriptorSet>> = descriptor::create_write_descriptor_sets_with_update(
        device,
        &descriptor_sets,
        &descriptor_binding_indices,
        &descriptor_data._descriptor_set_layout_bindings,
        &new_descriptor_resource_infos_list,
    );
    descriptor_sets
}

pub fn create_framebuffer_and_descriptor_sets(
    device: &Device,
    pipeline_binding_data: &PipelineBindingData,
    render_target: &TextureData,
    render_target_layer: u32,
    render_target_miplevel: u32,
    clear_value: Option<vk::ClearValue>,
    descriptor_resource_infos_list: &[(usize, SwapchainArray<DescriptorResourceInfo>)],
) -> (FramebufferData, SwapchainArray<vk::DescriptorSet>) {
    let framebuffer_data = create_framebuffer(
        device,
        &pipeline_binding_data.get_render_pass_data().borrow(),
        render_target,
        render_target_layer,
        render_target_miplevel,
        clear_value
    );
    let descriptor_sets = create_descriptor_sets(
        device,
        pipeline_binding_data,
        descriptor_resource_infos_list
    );
    (framebuffer_data, descriptor_sets)
}

pub fn create_framebuffers_and_descriptor_sets(
    device: &Device,
    pipeline_binding_data: &PipelineBindingData,
    framebuffer_name: &str,
    color_render_targets: &[RenderTargetInfo],
    depth_render_targets: &[RenderTargetInfo],
    resolve_render_targets: &[RenderTargetInfo],
    descriptor_resource_infos_list: &[(usize, SwapchainArray<DescriptorResourceInfo>)],
) -> (FramebufferData, SwapchainArray<vk::DescriptorSet>) {
    let framebuffer_data = create_framebuffers(
        device,
        &pipeline_binding_data.get_render_pass_data().borrow(),
        framebuffer_name,
        color_render_targets,
        depth_render_targets,
        resolve_render_targets,
    );
    let descriptor_sets = create_descriptor_sets(
        device,
        pipeline_binding_data,
        descriptor_resource_infos_list
    );
    (framebuffer_data, descriptor_sets)
}

pub fn find_exactly_matching_memory_type_index(
    memory_requirements: &vk::MemoryRequirements,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    flags: vk::MemoryPropertyFlags
) -> Option<u32> {
    let memory_type_bits = memory_requirements.memory_type_bits;

    // Try to find an exactly matching memory flag
    for (index, ref memory_type) in memory_properties.memory_types.iter().enumerate() {
        let property_flags = memory_type.property_flags;
        if (0 != (memory_type_bits & (1 << index as u32))) && (flags == property_flags) {
            return Some(index as u32);
        }
    }

    // Otherwise find a memory flag that works
    return find_memory_type_index(memory_requirements, memory_properties, flags);
}

pub fn find_memory_type_index(
    memory_requirements: &vk::MemoryRequirements,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    flags: vk::MemoryPropertyFlags
) -> Option<u32> {
    let memory_type_bits = memory_requirements.memory_type_bits;
    for (index, ref memory_type) in memory_properties.memory_types.iter().enumerate() {
        let property_flags = memory_type.property_flags;
        if (0 != (memory_type_bits & (1 << index as u32))) && (flags == (flags & property_flags)) {
            return Some(index as u32);
        }
    }
    None
}

pub unsafe fn ptr_chain_iter<T>(ptr: &mut T) -> impl Iterator<Item = *mut BaseOutStructure> {
    let ptr = <*mut T>::cast::<BaseOutStructure>(ptr);
    (0..).scan(ptr, |p_ptr, _| {
        if p_ptr.is_null() {
            return None;
        }
        let n_ptr = (**p_ptr).p_next;
        let old = *p_ptr;
        *p_ptr = n_ptr;
        Some(old)
    })
}