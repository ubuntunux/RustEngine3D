use std::collections::HashMap;
use std::path::{
    PathBuf,
};

use ash::{
    vk,
    Device,
};
use ash::vk::Handle;
use ash::extensions::nv::RayTracing;

use crate::vulkan_context::buffer::{
    self,
    BufferData
};
use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorData,
};
use crate::vulkan_context::geometry_buffer::{
    VertexData,
    StaticVertexData
};
use crate::vulkan_context::framebuffer::{ FramebufferDataCreateInfo };
use crate::vulkan_context::shader::{ create_shader_stage_create_info, destroy_shader_stage_create_info};
use crate::utilities::system::{ RcRefCell, newRcRefCell };
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::push_constants::PushConstant;

pub fn get_render_pass_pipeline_data_name(render_pass_data_name: &str, pipeline_data_name: &str) -> String {
    format!("{}/{}", render_pass_data_name, pipeline_data_name)
}


#[derive(Clone, Debug)]
pub struct RenderPassPipelineData {
    pub _render_pass_data: RcRefCell<RenderPassData>,
    pub _pipeline_data: RcRefCell<PipelineData>,
}

#[derive(Clone)]
pub struct RenderPassDataCreateInfo  {
    pub _render_pass_create_info_name: String,
    pub _render_pass_framebuffer_create_info: FramebufferDataCreateInfo,
    pub _color_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _depth_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _resolve_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _subpass_dependencies: Vec<vk::SubpassDependency>,
    pub _pipeline_data_create_infos: Vec<PipelineDataCreateInfo>
}

impl Default for RenderPassDataCreateInfo {
    fn default() -> RenderPassDataCreateInfo {
        RenderPassDataCreateInfo {
            _render_pass_create_info_name: String::new(),
            _render_pass_framebuffer_create_info: FramebufferDataCreateInfo::default(),
            _color_attachment_descriptions: Vec::new(),
            _depth_attachment_descriptions: Vec::new(),
            _resolve_attachment_descriptions: Vec::new(),
            _subpass_dependencies: Vec::new(),
            _pipeline_data_create_infos: Vec::new(),
        }
    }
}

impl RenderPassDataCreateInfo {
    pub fn get_pipeline_data_create_info_clone(&self, key: &str) -> PipelineDataCreateInfo {
        for pipeline_data_create_info in self._pipeline_data_create_infos.iter() {
            if key == pipeline_data_create_info._pipeline_data_create_info_name {
                return pipeline_data_create_info.clone();
            }
        }
        panic!("Not found pipeline_data_create_info: {:?}", key)
    }
}

#[derive(Clone, Debug)]
pub struct PipelineDataCreateInfo {
    pub _pipeline_data_create_info_name: String,
    pub _pipeline_bind_point: vk::PipelineBindPoint,
    pub _pipeline_create_flags: vk::PipelineCreateFlags,
    pub _pipeline_ray_generation_shader_file: PathBuf,
    pub _pipeline_ray_closet_hit_shader_file: PathBuf,
    pub _pipeline_ray_miss_shader_file: PathBuf,
    pub _pipeline_compute_shader_file: PathBuf,
    pub _pipeline_vertex_shader_file: PathBuf,
    pub _pipeline_fragment_shader_file: PathBuf,
    pub _pipeline_shader_defines: Vec<String>,
    pub _pipeline_dynamic_states: Vec<vk::DynamicState>,
    pub _pipeline_sample_count: vk::SampleCountFlags,
    pub _pipeline_polygon_mode: vk::PolygonMode,
    pub _pipeline_cull_mode: vk::CullModeFlags,
    pub _pipeline_front_face: vk::FrontFace,
    pub _pipeline_depth_bias_constant_factor: f32,
    pub _pipeline_depth_bias_clamp: f32,
    pub _pipeline_depth_bias_slope_factor: f32,
    pub _pipeline_line_width: f32,
    pub _pipeline_viewport: vk::Viewport,
    pub _pipeline_scissor_rect: vk::Rect2D,
    pub _pipeline_color_blend_modes: Vec<vk::PipelineColorBlendAttachmentState>,
    pub _depth_stencil_state_create_info: DepthStencilStateCreateInfo,
    pub _vertex_input_bind_descriptions: Vec<vk::VertexInputBindingDescription>,
    pub _vertex_input_attribute_descriptions: Vec<vk::VertexInputAttributeDescription>,
    pub _push_constant_datas: Vec<PipelinePushConstantData>,
    pub _descriptor_data_create_infos: Vec<DescriptorDataCreateInfo>,
}

impl Default for PipelineDataCreateInfo {
    fn default() -> PipelineDataCreateInfo {
        PipelineDataCreateInfo {
            _pipeline_data_create_info_name: String::new(),
            _pipeline_create_flags: vk::PipelineCreateFlags::default(),
            _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
            _pipeline_compute_shader_file: PathBuf::new(),
            _pipeline_vertex_shader_file: PathBuf::new(),
            _pipeline_fragment_shader_file: PathBuf::new(),
            _pipeline_ray_generation_shader_file: PathBuf::new(),
            _pipeline_ray_closet_hit_shader_file: PathBuf::new(),
            _pipeline_ray_miss_shader_file: PathBuf::new(),
            _pipeline_shader_defines: Vec::new(),
            _pipeline_dynamic_states: vec![vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR],
            _pipeline_sample_count: vk::SampleCountFlags::TYPE_1,
            _pipeline_polygon_mode: vk::PolygonMode::FILL,
            _pipeline_cull_mode: vk::CullModeFlags::NONE,
            _pipeline_front_face: vk::FrontFace::COUNTER_CLOCKWISE,
            _pipeline_depth_bias_constant_factor: 0.0,
            _pipeline_depth_bias_clamp: 0.0,
            _pipeline_depth_bias_slope_factor: 0.0,
            _pipeline_line_width: 1.0,
            _pipeline_viewport: vk::Viewport::default(),
            _pipeline_scissor_rect: vk::Rect2D::default(),
            _pipeline_color_blend_modes: Vec::new(),
            _depth_stencil_state_create_info: DepthStencilStateCreateInfo::default(),
            _vertex_input_bind_descriptions: StaticVertexData::get_vertex_input_binding_descriptions(),
            _vertex_input_attribute_descriptions: StaticVertexData::create_vertex_input_attribute_descriptions(),
            _push_constant_datas: Vec::new(),
            _descriptor_data_create_infos: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PipelinePushConstantData {
    pub _stage_flags: vk::ShaderStageFlags,
    pub _offset: u32,
    pub _push_constant: Box<dyn PushConstant>
}

#[derive(Clone, Debug)]
pub struct DepthStencilStateCreateInfo {
    pub _depth_test_enable: bool,
    pub _depth_write_enable: bool,
    pub _depth_compare_op: vk::CompareOp,
    pub _stencil_test_enable: bool,
    pub _front_fail_op: vk::StencilOp, // fail the stencil test
    pub _front_pass_op: vk::StencilOp, // pass both the depth and stencil tests
    pub _front_depth_fail_op: vk::StencilOp, // pass the stencil test and fail the depth test
    pub _front_compare_op: vk::CompareOp,
    pub _front_compare_mask: u32,
    pub _front_write_mask: u32,
    pub _front_reference: u32,
    pub _back_fail_op: vk::StencilOp, // fail the stencil test
    pub _back_pass_op: vk::StencilOp, // pass both the depth and stencil tests
    pub _back_depth_fail_op: vk::StencilOp, // pass the stencil test and fail the depth test
    pub _back_compare_op: vk::CompareOp,
    pub _back_compare_mask: u32,
    pub _back_write_mask: u32,
    pub _back_reference: u32,
}

impl Default for DepthStencilStateCreateInfo {
    fn default() -> DepthStencilStateCreateInfo {
        DepthStencilStateCreateInfo {
            _depth_test_enable: true,
            _depth_write_enable: true,
            _depth_compare_op: vk::CompareOp::LESS,
            _stencil_test_enable: false,
            _front_fail_op: vk::StencilOp::KEEP, // fail the stencil test
            _front_pass_op: vk::StencilOp::KEEP, // pass both the depth and stencil tests
            _front_depth_fail_op: vk::StencilOp::KEEP, // pass the stencil test and fail the depth test
            _front_compare_op: vk::CompareOp::NEVER,
            _front_compare_mask: 0,
            _front_write_mask: 0,
            _front_reference: 0,
            _back_fail_op: vk::StencilOp::KEEP, // fail the stencil test
            _back_pass_op: vk::StencilOp::KEEP, // pass both the depth and stencil tests
            _back_depth_fail_op: vk::StencilOp::KEEP, // pass the stencil test and fail the depth test
            _back_compare_op: vk::CompareOp::NEVER,
            _back_compare_mask: 0,
            _back_write_mask: 0,
            _back_reference: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ImageAttachmentDescription {
    pub _attachment_image_format: vk::Format,
    pub _attachment_image_samples: vk::SampleCountFlags,
    pub _attachment_load_operation: vk::AttachmentLoadOp,
    pub _attachment_store_operation: vk::AttachmentStoreOp,
    pub _attachment_stencil_load_operation: vk::AttachmentLoadOp,
    pub _attachment_stencil_store_operation: vk::AttachmentStoreOp,
    pub _attachment_initial_layout: vk::ImageLayout,
    pub _attachment_final_layout: vk::ImageLayout,
    pub _attachment_reference_layout: vk::ImageLayout,
}

impl Default for ImageAttachmentDescription {
    fn default() -> ImageAttachmentDescription {
        ImageAttachmentDescription {
            _attachment_image_format: vk::Format::UNDEFINED,
            _attachment_image_samples: vk::SampleCountFlags::TYPE_1,
            _attachment_load_operation: vk::AttachmentLoadOp::DONT_CARE,
            _attachment_store_operation: vk::AttachmentStoreOp::DONT_CARE,
            _attachment_stencil_load_operation: vk::AttachmentLoadOp::DONT_CARE,
            _attachment_stencil_store_operation: vk::AttachmentStoreOp::DONT_CARE,
            _attachment_initial_layout: vk::ImageLayout::UNDEFINED,
            _attachment_final_layout: vk::ImageLayout::GENERAL,
            _attachment_reference_layout: vk::ImageLayout::GENERAL,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PipelineData {
    pub _pipeline_data_name: String,
    pub _compute_shader_create_info: vk::PipelineShaderStageCreateInfo,
    pub _vertex_shader_create_info: vk::PipelineShaderStageCreateInfo,
    pub _fragment_shader_create_info: vk::PipelineShaderStageCreateInfo,
    pub _ray_tracing_shader_create_infos: [vk::PipelineShaderStageCreateInfo; 3],
    pub _pipeline: vk::Pipeline,
    pub _pipeline_bind_point: vk::PipelineBindPoint,
    pub _pipeline_layout: vk::PipelineLayout,
    pub _pipeline_dynamic_states: Vec<vk::DynamicState>,
    pub _descriptor_data: DescriptorData,
    pub _shader_binding_table: Option<BufferData>,
    pub _push_constant_datas: Vec<PipelinePushConstantData>
}

impl Default for PipelineData {
    fn default() -> PipelineData {
        PipelineData {
            _pipeline_data_name: String::new(),
            _compute_shader_create_info: vk::PipelineShaderStageCreateInfo::default(),
            _vertex_shader_create_info: vk::PipelineShaderStageCreateInfo::default(),
            _fragment_shader_create_info: vk::PipelineShaderStageCreateInfo::default(),
            _ray_tracing_shader_create_infos: [vk::PipelineShaderStageCreateInfo::default(); 3],
            _pipeline: vk::Pipeline::null(),
            _pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
            _pipeline_layout: vk::PipelineLayout::null(),
            _pipeline_dynamic_states: Vec::new(),
            _descriptor_data: DescriptorData::default(),
            _shader_binding_table: None,
            _push_constant_datas: Vec::new()
        }
    }
}

#[derive(Clone, Debug)]
pub struct RenderPassData {
    pub _render_pass_data_name: String,
    pub _render_pass: vk::RenderPass,
    pub _default_pipeline_data: RcRefCell<PipelineData>,
    pub _pipeline_data_map: PipelineDataMap,
}

pub type PipelineDataMap = HashMap<String, RcRefCell<PipelineData>>;
pub type RenderPassPipelineDataMap = HashMap<String, RenderPassPipelineData>;

impl RenderPassData {
    pub fn get_render_pass_data_name(&self) -> &String {
        &self._render_pass_data_name
    }

    pub fn get_render_pass(&self) -> vk::RenderPass {
        self._render_pass
    }

    pub fn get_default_pipeline_data(&self) -> &RcRefCell<PipelineData> {
        &self._default_pipeline_data
    }

    pub fn get_pipeline_data(&self, pipeline_data_name: &str) -> &RcRefCell<PipelineData> {
        let maybe_pipeline_data = self._pipeline_data_map.get(pipeline_data_name);
        match maybe_pipeline_data {
            Some(pipeline_data) => pipeline_data,
            None => self.get_default_pipeline_data(),
        }
    }
}

pub fn create_render_pass_data(
    renderer_context: &RendererContext,
    render_pass_data_create_info: &RenderPassDataCreateInfo,
    descriptor_datas: &Vec<RcRefCell<DescriptorData>>
) -> RenderPassData {
    let device = renderer_context.get_device();
    let render_pass = create_render_pass(device, &render_pass_data_create_info);
    let count = render_pass_data_create_info._pipeline_data_create_infos.len();
    let mut pipeline_data_map: PipelineDataMap = HashMap::new();
    let mut default_pipeline_data_name: String = String::new();
    for i in 0..count {
        let bind_point = render_pass_data_create_info._pipeline_data_create_infos[i]._pipeline_bind_point;
        let pipeline_data =
            if vk::PipelineBindPoint::GRAPHICS == bind_point {
                create_graphics_pipeline_data(
                    device,
                    render_pass,
                    &render_pass_data_create_info._pipeline_data_create_infos[i],
                    false == render_pass_data_create_info._depth_attachment_descriptions.is_empty(),
                    &descriptor_datas[i].borrow()
                )
            } else if vk::PipelineBindPoint::COMPUTE == bind_point {
                create_compute_pipeline_data(
                    device,
                    &render_pass_data_create_info._pipeline_data_create_infos[i],
                    &descriptor_datas[i].borrow()
                )
            } else if vk::PipelineBindPoint::RAY_TRACING_KHR == bind_point || vk::PipelineBindPoint::RAY_TRACING_NV == bind_point {
                create_ray_tracing_pipeline_data(
                    device,
                    renderer_context.get_command_pool(),
                    renderer_context.get_graphics_queue(),
                    renderer_context.get_device_memory_properties(),
                    renderer_context.get_ray_tracing(),
                    renderer_context.get_ray_tracing_properties(),
                    &render_pass_data_create_info._pipeline_data_create_infos[i],
                    &descriptor_datas[i].borrow()
                )
            } else {
                panic!("Request::from_raw can not be used Client-side.")
            };
        if 0 == i {
            default_pipeline_data_name = pipeline_data._pipeline_data_name.clone();
        }
        pipeline_data_map.insert(pipeline_data._pipeline_data_name.clone(), newRcRefCell(pipeline_data));
    }
    log::trace!("    create_render_pass_data: {}", render_pass_data_create_info._render_pass_create_info_name);
    let default_pipeline_data = pipeline_data_map.get(&default_pipeline_data_name).unwrap();
    RenderPassData {
        _render_pass_data_name: render_pass_data_create_info._render_pass_create_info_name.clone(),
        _render_pass: render_pass,
        _default_pipeline_data: default_pipeline_data.clone(),
        _pipeline_data_map: pipeline_data_map,
    }
}

pub fn destroy_render_pass_data(device: &Device, render_pass_data: &RenderPassData) {
    destroy_render_pass(device, render_pass_data._render_pass, &render_pass_data._render_pass_data_name);
    for pipeline_data in render_pass_data._pipeline_data_map.values() {
        destroy_pipeline_data(device, &mut pipeline_data.borrow_mut());
    }
}


pub fn create_render_pass(
    device: &Device,
    render_pass_data_create_info: &RenderPassDataCreateInfo
) -> vk::RenderPass {
    let create_image_attachment = | attachment_description: &ImageAttachmentDescription | -> vk::AttachmentDescription {
        vk::AttachmentDescription {
            format: attachment_description._attachment_image_format,
            samples: attachment_description._attachment_image_samples,
            load_op: attachment_description._attachment_load_operation,
            store_op: attachment_description._attachment_store_operation,
            stencil_load_op: attachment_description._attachment_stencil_load_operation,
            stencil_store_op: attachment_description._attachment_stencil_store_operation,
            initial_layout: attachment_description._attachment_initial_layout,
            final_layout: attachment_description._attachment_final_layout,
            ..Default::default()
        }
    };
    let create_image_attachment_reference = | attachment_description: &ImageAttachmentDescription, index: u32 | -> vk::AttachmentReference {
        vk::AttachmentReference {
            attachment: index,
            layout: attachment_description._attachment_reference_layout,
        }
    };
    let color_attachment_descriptions = &render_pass_data_create_info._color_attachment_descriptions;
    let depth_attachment_descriptions = &render_pass_data_create_info._depth_attachment_descriptions;
    let resolve_attachment_descriptions = &render_pass_data_create_info._resolve_attachment_descriptions;
    let mut attachment_descriptions: Vec<ImageAttachmentDescription> = color_attachment_descriptions.clone();
    attachment_descriptions.extend(depth_attachment_descriptions.clone());
    attachment_descriptions.extend(resolve_attachment_descriptions.clone());
    let image_attachments: Vec<vk::AttachmentDescription> = attachment_descriptions
        .iter()
        .map(|attachment_description| create_image_attachment(attachment_description))
        .collect();
    let mut description_offset: u32 = 0;
    let color_attachment_refernces: Vec<vk::AttachmentReference> = color_attachment_descriptions
        .iter()
        .enumerate()
        .map(|(index, ref description)| {
            create_image_attachment_reference(description, index as u32)
        }).collect();
    description_offset += color_attachment_refernces.len() as u32;
    let depth_attachment_refernces: Vec<vk::AttachmentReference> = depth_attachment_descriptions
        .iter()
        .enumerate()
        .map(|(index, ref description)| {
            create_image_attachment_reference(description, description_offset + index as u32)
        }).collect();
    description_offset += depth_attachment_refernces.len() as u32;
    let resolve_attachment_refernces: Vec<vk::AttachmentReference> = resolve_attachment_descriptions
        .iter()
        .enumerate()
        .map(|(index, ref description)| {
            create_image_attachment_reference(description, description_offset + index as u32)
        }).collect();
    let subpasses = [vk::SubpassDescription {
        pipeline_bind_point: vk::PipelineBindPoint::GRAPHICS,
        p_color_attachments: if !color_attachment_refernces.is_empty() { color_attachment_refernces.as_ptr() } else { std::ptr::null() },
        color_attachment_count: color_attachment_refernces.len() as u32,
        p_resolve_attachments: if !resolve_attachment_refernces.is_empty() { resolve_attachment_refernces.as_ptr() } else { std::ptr::null() },
        p_depth_stencil_attachment: if !depth_attachment_refernces.is_empty() { depth_attachment_refernces.as_ptr() } else { std::ptr::null() },
        ..Default::default()
    }];

    let render_pass_create_info = vk::RenderPassCreateInfo {
        p_attachments: image_attachments.as_ptr(),
        attachment_count: image_attachments.len() as u32,
        p_subpasses: subpasses.as_ptr(),
        subpass_count: subpasses.len() as u32,
        p_dependencies: render_pass_data_create_info._subpass_dependencies.as_ptr(),
        dependency_count: render_pass_data_create_info._subpass_dependencies.len() as u32,
        ..Default::default()
    };
    unsafe {
        let render_pass = device.create_render_pass(&render_pass_create_info, None).expect("vkCreatePipelineLayout failed!");
        log::debug!("create_render_pass: {} {:?}", render_pass_data_create_info._render_pass_create_info_name, render_pass);
        render_pass
    }
}

pub fn destroy_render_pass(device: &Device, render_pass: vk::RenderPass, render_pass_name: &String) {
    log::debug!("destroy_render_pass: {} {:?}", render_pass_name, render_pass);
    unsafe {
        device.destroy_render_pass(render_pass, None);
    }
}


pub fn create_pipeline_layout(
    device: &Device,
    push_constant_datas: &[PipelinePushConstantData],
    descriptor_set_layouts: &[vk::DescriptorSetLayout]
) -> vk::PipelineLayout {
    let push_constant_ranges: Vec<vk::PushConstantRange> =
        push_constant_datas.iter().map(|push_constant_data| {
            vk::PushConstantRange {
                stage_flags: push_constant_data._stage_flags,
                offset: push_constant_data._offset,
                size: push_constant_data._push_constant.as_ref().get_size(),
            }
        }).collect();

    let pipeline_create_info = vk::PipelineLayoutCreateInfo {
        p_set_layouts: descriptor_set_layouts.as_ptr(),
        set_layout_count: descriptor_set_layouts.len() as u32,
        p_push_constant_ranges: push_constant_ranges.as_ptr(),
        push_constant_range_count: push_constant_ranges.len() as u32,
        ..Default::default()
    };
    unsafe {
        let pipeline_layout = device.create_pipeline_layout(&pipeline_create_info, None).expect("VkCreatePipelineLayout failed!!");
        log::trace!("    create_pipeline_layout: {:?}", pipeline_layout);
        pipeline_layout
    }
}

pub fn destroy_pipieline_layout(device: &Device, pipeline_layout: vk::PipelineLayout) {
    log::trace!("    destroy_pipieline_layout: {:?}", pipeline_layout);
    unsafe {
        device.destroy_pipeline_layout(pipeline_layout, None);
    }
}

pub fn create_graphics_pipeline_data(
    device: &Device,
    render_pass: vk::RenderPass,
    pipeline_data_create_info: &PipelineDataCreateInfo,
    has_depth_stencil_attachment: bool,
    descriptor_data: &DescriptorData
) -> PipelineData {
    let vertex_shader_create_info = create_shader_stage_create_info(
        device,
        &pipeline_data_create_info._pipeline_vertex_shader_file,
        &pipeline_data_create_info._pipeline_shader_defines,
        vk::ShaderStageFlags::VERTEX
    );
    let fragment_shader_create_info = create_shader_stage_create_info(
        device,
        &pipeline_data_create_info._pipeline_fragment_shader_file,
        &pipeline_data_create_info._pipeline_shader_defines,
        vk::ShaderStageFlags::FRAGMENT
    );
    let descriptor_set_layouts = [ descriptor_data._descriptor_set_layout, ];
    let shader_stage_infos = vec![vertex_shader_create_info, fragment_shader_create_info];
    let pipeline_layout = create_pipeline_layout(
        device,
        &pipeline_data_create_info._push_constant_datas,
        &descriptor_set_layouts
    );
    let vertex_input_state_info = vk::PipelineVertexInputStateCreateInfo {
        vertex_binding_description_count: pipeline_data_create_info._vertex_input_bind_descriptions.len() as u32,
        p_vertex_binding_descriptions: pipeline_data_create_info._vertex_input_bind_descriptions.as_ptr(),
        vertex_attribute_description_count: pipeline_data_create_info._vertex_input_attribute_descriptions.len() as u32,
        p_vertex_attribute_descriptions: pipeline_data_create_info._vertex_input_attribute_descriptions.as_ptr(),
        ..Default::default()
    };
    let input_assembly = vk::PipelineInputAssemblyStateCreateInfo {
        topology: vk::PrimitiveTopology::TRIANGLE_LIST,
        primitive_restart_enable: 0,
        ..Default::default()
    };
    let dynamic_state = vk::PipelineDynamicStateCreateInfo {
        dynamic_state_count: pipeline_data_create_info._pipeline_dynamic_states.len() as u32,
        p_dynamic_states: pipeline_data_create_info._pipeline_dynamic_states.as_ptr(),
        ..Default::default()
    };
    let viewports = [pipeline_data_create_info._pipeline_viewport];
    let scissors = [pipeline_data_create_info._pipeline_scissor_rect];
    let viewport_state = vk::PipelineViewportStateCreateInfo {
        viewport_count: viewports.len() as u32,
        p_viewports: viewports.as_ptr(),
        scissor_count: scissors.len() as u32,
        p_scissors: scissors.as_ptr(),
        ..Default::default()
    };
    let depth_bias_enable: bool =
            0.0 != pipeline_data_create_info._pipeline_depth_bias_constant_factor ||
            0.0 != pipeline_data_create_info._pipeline_depth_bias_clamp ||
            0.0 != pipeline_data_create_info._pipeline_depth_bias_slope_factor;
    let rasterizer = vk::PipelineRasterizationStateCreateInfo {
        depth_clamp_enable: 0,
        rasterizer_discard_enable: 0,
        polygon_mode: pipeline_data_create_info._pipeline_polygon_mode,
        cull_mode: pipeline_data_create_info._pipeline_cull_mode,
        front_face: pipeline_data_create_info._pipeline_front_face,
        depth_bias_enable: if depth_bias_enable { 1 } else { 0 },
        depth_bias_constant_factor: pipeline_data_create_info._pipeline_depth_bias_constant_factor,
        depth_bias_clamp: pipeline_data_create_info._pipeline_depth_bias_clamp,
        depth_bias_slope_factor: pipeline_data_create_info._pipeline_depth_bias_slope_factor,
        line_width: pipeline_data_create_info._pipeline_line_width,
        ..Default::default()
    };
    let multi_sampling = vk::PipelineMultisampleStateCreateInfo {
        sample_shading_enable: 0,
        rasterization_samples: pipeline_data_create_info._pipeline_sample_count,
        min_sample_shading: 1.0,
        alpha_to_coverage_enable: 0,
        alpha_to_one_enable: 0,
        ..Default::default()
    };
    let blend_constants = [0.0, 0.0, 0.0, 0.0];
    let color_blending = vk::PipelineColorBlendStateCreateInfo {
        logic_op_enable: 0,
        logic_op: vk::LogicOp::COPY,
        attachment_count: pipeline_data_create_info._pipeline_color_blend_modes.len() as u32,
        p_attachments: pipeline_data_create_info._pipeline_color_blend_modes.as_ptr(),
        blend_constants,
        ..Default::default()
    };
    let depth_stencil_state_create_info = &pipeline_data_create_info._depth_stencil_state_create_info;
    let depth_stencil_state = if has_depth_stencil_attachment {
        vk::PipelineDepthStencilStateCreateInfo {
            depth_test_enable: depth_stencil_state_create_info._depth_test_enable.into(),
            depth_write_enable: depth_stencil_state_create_info._depth_write_enable.into(),
            depth_compare_op: depth_stencil_state_create_info._depth_compare_op,
            depth_bounds_test_enable: 0,
            min_depth_bounds: 0.0,
            max_depth_bounds: 1.0,
            stencil_test_enable: depth_stencil_state_create_info._stencil_test_enable.into(),
            front: vk::StencilOpState {
                fail_op: depth_stencil_state_create_info._front_fail_op,
                pass_op: depth_stencil_state_create_info._front_pass_op,
                depth_fail_op: depth_stencil_state_create_info._front_depth_fail_op,
                compare_op: depth_stencil_state_create_info._front_compare_op,
                compare_mask: depth_stencil_state_create_info._front_compare_mask,
                write_mask: depth_stencil_state_create_info._front_write_mask,
                reference: depth_stencil_state_create_info._front_reference,
            },
            back: vk::StencilOpState {
                fail_op: depth_stencil_state_create_info._back_fail_op,
                pass_op: depth_stencil_state_create_info._back_pass_op,
                depth_fail_op: depth_stencil_state_create_info._back_depth_fail_op,
                compare_op: depth_stencil_state_create_info._back_compare_op,
                compare_mask: depth_stencil_state_create_info._back_compare_mask,
                write_mask: depth_stencil_state_create_info._back_write_mask,
                reference: depth_stencil_state_create_info._back_reference,
            },
            ..Default::default()
        }
    } else {
        vk::PipelineDepthStencilStateCreateInfo::default()
    };
    let grphics_pipeline_create_info = [vk::GraphicsPipelineCreateInfo {
        stage_count: shader_stage_infos.len() as u32,
        p_stages: shader_stage_infos.as_ptr(),
        p_vertex_input_state: &vertex_input_state_info,
        p_input_assembly_state: &input_assembly,
        p_viewport_state: &viewport_state,
        p_rasterization_state: &rasterizer,
        p_multisample_state: &multi_sampling,
        p_depth_stencil_state: &depth_stencil_state,
        p_color_blend_state: &color_blending,
        p_dynamic_state: &dynamic_state,
        layout: pipeline_layout,
        render_pass,
        subpass: 0,
        base_pipeline_handle: vk::Pipeline::null(),
        base_pipeline_index: -1,
        ..Default::default()
    }];

    unsafe {
        let graphics_pipelines = device.create_graphics_pipelines(
            vk::PipelineCache::null(),
            &grphics_pipeline_create_info,
            None
        ).expect("vkCreateGraphicsPipelines failed!");

        log::trace!("    create_graphics_pipeline_data: {} ({:?})", pipeline_data_create_info._pipeline_data_create_info_name, graphics_pipelines);
        log::trace!("    shaderDefines: {:?}", pipeline_data_create_info._pipeline_shader_defines);
        log::trace!("    vertexShader: {:#X} {:?}", vertex_shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_vertex_shader_file);
        log::trace!("    fragmentShader: {:#X} {:?}", fragment_shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_fragment_shader_file);

        PipelineData {
            _pipeline_data_name: pipeline_data_create_info._pipeline_data_create_info_name.clone(),
            _vertex_shader_create_info: vertex_shader_create_info,
            _fragment_shader_create_info: fragment_shader_create_info,
            _pipeline: graphics_pipelines[0],
            _pipeline_layout: pipeline_layout,
            _pipeline_bind_point: pipeline_data_create_info._pipeline_bind_point,
            _pipeline_dynamic_states: pipeline_data_create_info._pipeline_dynamic_states.clone(),
            _descriptor_data: descriptor_data.clone(),
            _push_constant_datas: pipeline_data_create_info._push_constant_datas.clone(),
            ..Default::default()
        }
    }
}

pub fn create_compute_pipeline_data(
    device: &Device,
    pipeline_data_create_info: &PipelineDataCreateInfo,
    descriptor_data: &DescriptorData
) -> PipelineData {
    let shader_create_info = create_shader_stage_create_info(
        device,
        &pipeline_data_create_info._pipeline_compute_shader_file,
        &pipeline_data_create_info._pipeline_shader_defines,
        vk::ShaderStageFlags::COMPUTE
    );
    let descriptor_set_layouts = [ descriptor_data._descriptor_set_layout, ];
    let pipeline_layout = create_pipeline_layout(
        device,
        &pipeline_data_create_info._push_constant_datas,
        &descriptor_set_layouts
    );
    let pipeline_create_info = [vk::ComputePipelineCreateInfo {
        flags: pipeline_data_create_info._pipeline_create_flags,
        stage: shader_create_info,
        layout: pipeline_layout,
        base_pipeline_handle: vk::Pipeline::null(),
        base_pipeline_index: -1,
        ..Default::default()
    }];

    unsafe {
        let pipelines = device.create_compute_pipelines(
            vk::PipelineCache::null(),
            &pipeline_create_info,
            None
        ).expect("vkCreateComputePipelines failed!");

        log::trace!("    create_compute_pipeline_data: {} ({:?})", pipeline_data_create_info._pipeline_data_create_info_name, pipelines);
        log::trace!("    shaderDefines: {:?}", pipeline_data_create_info._pipeline_shader_defines);
        log::trace!("    computeShader: {:#X} {:?}", shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_compute_shader_file);

        PipelineData {
            _pipeline_data_name: pipeline_data_create_info._pipeline_data_create_info_name.clone(),
            _compute_shader_create_info: shader_create_info,
            _pipeline: pipelines[0],
            _pipeline_layout: pipeline_layout,
            _pipeline_bind_point: pipeline_data_create_info._pipeline_bind_point,
            _pipeline_dynamic_states: pipeline_data_create_info._pipeline_dynamic_states.clone(),
            _descriptor_data: descriptor_data.clone(),
            _push_constant_datas: pipeline_data_create_info._push_constant_datas.clone(),
            ..Default::default()
        }
    }
}

pub fn create_ray_tracing_pipeline_data(
    device: &Device,
    command_pool: vk::CommandPool,
    command_queue: vk::Queue,
    device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    ray_tracing: &RayTracing,
    ray_tracing_properties: &vk::PhysicalDeviceRayTracingPropertiesNV,
    pipeline_data_create_info: &PipelineDataCreateInfo,
    descriptor_data: &DescriptorData
) -> PipelineData {
    let shader_groups = vec![
        // ray generation
        vk::RayTracingShaderGroupCreateInfoNV {
            ty: vk::RayTracingShaderGroupTypeNV::GENERAL,
            general_shader: 0,
            closest_hit_shader: vk::SHADER_UNUSED_NV,
            any_hit_shader: vk::SHADER_UNUSED_NV,
            intersection_shader: vk::SHADER_UNUSED_NV,
            ..Default::default()
        },
        // ray closet-hit
        vk::RayTracingShaderGroupCreateInfoNV {
            ty: vk::RayTracingShaderGroupTypeNV::TRIANGLES_HIT_GROUP,
            general_shader: vk::SHADER_UNUSED_NV,
            closest_hit_shader: 1,
            any_hit_shader: vk::SHADER_UNUSED_NV,
            intersection_shader: vk::SHADER_UNUSED_NV,
            ..Default::default()
        },
        // ray miss
        vk::RayTracingShaderGroupCreateInfoNV {
            ty: vk::RayTracingShaderGroupTypeNV::GENERAL,
            general_shader: 2,
            closest_hit_shader: vk::SHADER_UNUSED_NV,
            any_hit_shader: vk::SHADER_UNUSED_NV,
            intersection_shader: vk::SHADER_UNUSED_NV,
            ..Default::default()
        }
    ];
    let shader_group_count: u32 = shader_groups.len() as u32;

    let shader_create_infos = [
        create_shader_stage_create_info(
            device,
            &pipeline_data_create_info._pipeline_ray_generation_shader_file,
            &pipeline_data_create_info._pipeline_shader_defines,
            vk::ShaderStageFlags::RAYGEN_KHR
        ),
        create_shader_stage_create_info(
            device,
            &pipeline_data_create_info._pipeline_ray_closet_hit_shader_file,
            &pipeline_data_create_info._pipeline_shader_defines,
            vk::ShaderStageFlags::CLOSEST_HIT_KHR
        ),
        create_shader_stage_create_info(
            device,
            &pipeline_data_create_info._pipeline_ray_miss_shader_file,
            &pipeline_data_create_info._pipeline_shader_defines,
            vk::ShaderStageFlags::MISS_KHR
        )
    ];
    let descriptor_set_layouts = [ descriptor_data._descriptor_set_layout, ];
    let pipeline_layout = create_pipeline_layout(
        device,
        &pipeline_data_create_info._push_constant_datas,
        &descriptor_set_layouts
    );
    let pipeline_create_info = [
        vk::RayTracingPipelineCreateInfoNV {
            stage_count: shader_create_infos.len() as u32,
            p_stages: shader_create_infos.as_ptr(),
            group_count: shader_group_count,
            p_groups: shader_groups.as_ptr(),
            max_recursion_depth: 1,
            layout: pipeline_layout,
            ..Default::default()
        }
    ];

    unsafe {
        let pipeline = ray_tracing.create_ray_tracing_pipelines(vk::PipelineCache::null(), &pipeline_create_info, None,).expect("create_ray_tracing_pipelines failed!")[0];

        log::trace!("    create_ray_tracing_pipeline_data: {} ({:?})", pipeline_data_create_info._pipeline_data_create_info_name, pipeline);
        log::trace!("    shader defines: {:?}", pipeline_data_create_info._pipeline_shader_defines);
        for shader_create_info in shader_create_infos {
            log::trace!("    ray generation shader: {:#X} {:?}", shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_ray_generation_shader_file);
            log::trace!("    ray closet-hit shader: {:#X} {:?}", shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_ray_closet_hit_shader_file);
            log::trace!("    ray miss shader: {:#X} {:?}", shader_create_info.module.as_raw(), pipeline_data_create_info._pipeline_ray_miss_shader_file);
        }

        // create shader binding table
        let table_size = (ray_tracing_properties.shader_group_handle_size * shader_group_count) as u64;
        let mut table_data: Vec<u8> = vec![0u8; table_size as usize];
        ray_tracing.get_ray_tracing_shader_group_handles(pipeline, 0, shader_group_count, &mut table_data).expect("get_ray_tracing_shader_group_handles failed!");

        let shader_binding_table = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::TRANSFER_SRC,
            &table_data
        );

        log::info!(">>> TEST CODE: create_ray_tracing_pipeline_data");
        log::info!("    create_ray_tracing_pipeline_data.");
        log::info!("    CEHCK :: do i need present_queue or graphics queue ??");
        PipelineData {
            _pipeline_data_name: pipeline_data_create_info._pipeline_data_create_info_name.clone(),
            _ray_tracing_shader_create_infos: shader_create_infos,
            _pipeline: pipeline,
            _pipeline_layout: pipeline_layout,
            _pipeline_bind_point: pipeline_data_create_info._pipeline_bind_point,
            _pipeline_dynamic_states: pipeline_data_create_info._pipeline_dynamic_states.clone(),
            _descriptor_data: descriptor_data.clone(),
            _shader_binding_table: Some(shader_binding_table),
            _push_constant_datas: pipeline_data_create_info._push_constant_datas.clone(),
            ..Default::default()
        }
    }
}

pub fn destroy_pipeline_data(device: &Device, pipeline_data: &mut PipelineData) {
    log::debug!("    destroy_pipeline_data: {}, pipeline: {:?}, pipeline_layout: {:?}", pipeline_data._pipeline_data_name,pipeline_data._pipeline, pipeline_data._pipeline_layout);
    unsafe {
        device.destroy_pipeline(pipeline_data._pipeline, None);
        device.destroy_pipeline_layout(pipeline_data._pipeline_layout, None);
    }
    destroy_shader_stage_create_info(device, &pipeline_data._compute_shader_create_info);
    destroy_shader_stage_create_info(device, &pipeline_data._vertex_shader_create_info);
    destroy_shader_stage_create_info(device, &pipeline_data._fragment_shader_create_info);
    for shader_create_info in pipeline_data._ray_tracing_shader_create_infos {
        destroy_shader_stage_create_info(device, &shader_create_info);
    }
    if pipeline_data._shader_binding_table.is_some() {
        buffer::destroy_buffer_data(device, &pipeline_data._shader_binding_table.unwrap());
        pipeline_data._shader_binding_table = None;
    }
}