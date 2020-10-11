use std::mem;
use std::collections::HashMap;
use std::path::{
    Path,
    PathBuf,
};

use nalgebra::{
    Matrix4
};
use ash::{
    vk,
    Device,
};
use ash::version::{
    DeviceV1_0
};

use crate::vulkan_context::descriptor::{
    DescriptorDataCreateInfo,
    DescriptorData,
};
use crate::vulkan_context::framebuffer::{
    FramebufferDataCreateInfo,
    FramebufferData,
};
use crate::vulkan_context::geometry_buffer::{
    VertexData
};
use crate::vulkan_context::push_constant::{
    PushConstantInterface,
    PushConstants_StaticRenderObject,
};


pub struct RenderPassPipelineDataName {
    _render_pass_data_name: String,
    _pipeline_data_name: String,
}

#[derive(Clone)]
pub struct RenderPassDataCreateInfo  {
    pub _render_pass_create_info_name: String,
    pub _render_pass_frame_buffer_create_info: FramebufferDataCreateInfo,
    pub _color_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _depth_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _resolve_attachment_descriptions: Vec<ImageAttachmentDescription>,
    pub _subpass_dependencies: Vec<vk::SubpassDependency>,
    pub _pipeline_data_create_infos: Vec<PipelineDataCreateInfo>
}

#[derive(Clone, Debug)]
pub struct PipelineDataCreateInfo {
    pub _pipeline_data_create_info_name: String,
    pub _pipeline_vertex_shader_file: PathBuf,
    pub _pipeline_fragment_shader_file: PathBuf,
    pub _pipeline_shader_defines: Vec<String>,
    pub _pipeline_dynamic_states: Vec<vk::DynamicState>,
    pub _pipeline_sample_count: vk::SampleCountFlags,
    pub _pipeline_polygon_mode: vk::PolygonMode,
    pub _pipeline_cull_mode: vk::CullModeFlags,
    pub _pipeline_front_face: vk::FrontFace,
    pub _pipeline_viewport: vk::Viewport,
    pub _pipeline_scissor_rect: vk::Rect2D,
    pub _pipeline_color_blend_modes: Vec<vk::PipelineColorBlendAttachmentState>,
    pub _depth_stencil_state_create_info: DepthStencilStateCreateInfo,
    pub _descriptor_data_create_info_list: Vec<DescriptorDataCreateInfo>,
}

#[derive(Clone, Debug)]
pub struct DepthStencilStateCreateInfo {
    pub _depth_test_enable: vk::Bool32,
    pub _depth_write_enable: vk::Bool32,
    pub _depth_compare_op: vk::CompareOp,
    pub _stencil_test_enable: vk::Bool32,
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
            _depth_test_enable: vk::TRUE,
            _depth_write_enable: vk::TRUE,
            _depth_compare_op: vk::CompareOp::LESS,
            _stencil_test_enable: vk::TRUE,
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

type PipelineDataMap = HashMap<String, PipelineData>;

#[derive(Clone, Debug)]
pub struct PipelineData {
    pub _pipeline_data_name: String,
    pub _vertex_shader_create_info: vk::PipelineShaderStageCreateInfo,
    pub _fragment_shader_create_info: vk::PipelineShaderStageCreateInfo,
    pub _pipeline: vk::Pipeline,
    pub _pipeline_layout: vk::PipelineLayout,
    pub _pipeline_dynamic_states: Vec<vk::DynamicState>,
    pub _descriptor_data: DescriptorData,
}

#[derive(Clone, Debug)]
pub struct RenderPassData {
    pub _render_pass_data_name: String,
    pub _render_pass: vk::RenderPass,
    pub _render_pass_framebuffer_name: String,
    pub _default_pipeline_data: PipelineData,
    pub _pipeline_data_map: PipelineDataMap,
}


pub fn create_render_pass_data(
    device: &Device,
    render_pass_data_create_info: RenderPassDataCreateInfo,
    descriptor_datas: Vec<DescriptorData>
) -> RenderPassData {
    let render_pass = create_render_pass(&device, &reder_pass_data_create_info);
    let count = _pipeline_data_create_infos.len();
    for i in 0..count {
        let pipeline_data_create_info = &piple_data_create_infos[i];
        let descriptor_data = &descriptor_datas[i];
        let graphics_pipeline_data = create_graphics_pipeline_data(device, render_pass, &pipeline_data_create_info, &descriptor_data)'
    }
    pipelineDataList <- forM (zip _pipelineDataCreateInfos descriptorDatas) $ \(pipelineDataCreateInfo, descriptorData) -> do
    graphicsPipelineData <- createGraphicsPipelineData device renderPass pipelineDataCreateInfo descriptorData
    return graphicsPipelineData
    pipelineDataMap <- HashTable.new
    forM_ pipelineDataList $ \pipelineData ->
    HashTable.insert pipelineDataMap (_pipelineDataName pipelineData) pipelineData
    log::info!("CreateRenderPassData : " ++ (Text.unpack _renderPassCreateInfoName)
    return RenderPassData
        { _renderPassDataName = _renderPassCreateInfoName
        , _renderPass = renderPass
        , _renderPassFramebufferName = (_frameBufferName _renderPassFramebufferCreateInfo)
        , _defaultPipelineData = pipelineDataList !! 0
        , _pipelineDataMap = pipelineDataMap
        }
}


destroyRenderPassData :: vk::Device -> RenderPassData -> IO ()
destroyRenderPassData device renderPassData@RenderPassData {..} = do
    logInfo "DestroyRenderPassData"
    clearHashTable _pipelineDataMap (\(k, v) -> destroyPipelineData device v)
    destroyRenderPass device _renderPass _renderPassDataName


pub fn create_render_pass(
    deivce: &Device,
    render_pass_data_create_info: &RenderPassDataCreateInfo
) -> vk::RenderPass {
    let create_image_attachment = | attachment_description: &ImageAttachmentDescription | -> vk::AttachmentDescription {
        vk::AttachmentDescription::builder()
            .format(attachment_description._attachment_image_format)
            .samples(attachment_description._attachment_image_samples)
            .load_op(attachment_description._attachment_load_operation)
            .store_op(attachment_description._attachment_store_operation)
            .stencil_load_op(attachment_description._attachment_stencil_load_operation)
            .stencil_store_op(attachment_description._attachment_stencil_store_operation)
            .initial_layout(attachment_description._attachment_initial_layout)
            .final_layout(attachment_description._attachment_final_layout)
            .build()
    };
    let create_image_attachment_reference = | attachment_description: &ImageAttachmentDescription, index: u32 | -> vk::AttachmentReference {
        vk::AttachmentReference::builder()
            .attachment(index)
            .layout(attachment_description._attachment_reference_layout)
            .build()
    };
    let color_attachment_description = &render_pass_data_create_info._color_attachment_descriptions;
    let depth_attachment_description = &render_pass_data_create_info._depth_attachment_descriptions;
    let resolve_attachment_description = &render_pass_data_create_info._resolve_attachment_descriptions;
    let mut attachment_descriptions: Vec<ImageAttachmentDescription> = color_attachment_descriptions.clone();
    attachment_descriptions.extend(depth_attachment_descriptions.clone());
    attachment_descriptions.extend(resolve_attachment_descriptions.clone());
    let image_attachments = attachment_descriptions
        .iter()
        .map(|attachment_description| create_image_attachment(attachment_description))
        .collect();
    let mut description_offset: u32 = 0;
    let color_attachment_refernces: Vec<vk::AttachmentReference> = color_attachment_description
        .iter()
        .enumerate()
        .map(|index, ref description| {
            create_image_attachment_reference(description, index as u32)
        }).collect();
    description_offset += color_attachment_refernces.len();
    let depth_attachment_refernces: Vec<vk::AttachmentReference> = depth_attachment_description
        .iter()
        .enumerate()
        .map(|index, ref description| {
            create_image_attachment_reference(description, description_offset + index as u32)
        }).collect();
    description_offset += depth_attachment_refernces.len();
    let resolve_attachment_refernces: Vec<vk::AttachmentReference> = resolve_attachment_description
        .iter()
        .enumerate()
        .map(|index, ref description| {
            create_image_attachment_reference(description, description_offset + index as u32)
        }).collect();
    let subpasses: [vk::SubpassDescription; 1] = [vk::SubpassDescription::builder()
        .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
        .color_attachments(&color_attachment_refernces)
        .depth_stencil_attachment(if false == depth_attachment_refernces.is_empty() {
            &depth_attachment_refernces[0]
        } else {
            std::ptr::null()
        })
        .resolve_attachments(&resolve_attachment_refernces)
        .build()
    ];
    let render_pass_create_info = vk::RenderPassCreateInfo::builder()
        .attachments(&image_attachments)
        .subpasses(&subpasses)
        .dependenciese(&render_pass_data_create_info._subpass_dependencies)
    unsafe {
        let render_pass = deivce.create_render_pass(&render_pass_create_info, None).expect("vkCreatePipelineLayout failed!");
        log::info!("Create RenderPass: {} {}", render_pass_data_create_info._render_pass_create_info_name, render_pass);
        return renderPass;
    }
}

pub fn destroy_render_pass(device: &Device, render_pass: vk::RenderPass, render_pass_name: &String) {
    log::info!("Destroy RenderPass: {} {}", render_pass_name, render_pass);
    unsafe {
        device.destroy_render_pass(render_pass, None);
    }
}


pub fn create_pipeline_layout(
    device: &Device,
    push_constant_ranges: &[vk::PushConstantRange],
    descriptor_set_layouts: &[vk::DescriptorSetLayout]
) -> vk::PipelineLayout {
    let pipeline_create_info = vk::PipelineLayoutCreateInfo::builder()
        .set_layouts(descriptor_set_layouts)
        .push_constant_ranges(push_constant_ranges)
        .build();
    unsafe {
        let pipeline_layout = device.create_pipeline_layout(&pipeline_create_info, None).expect("VkCreatePipelineLayout failed!!");
        log::info!("Create PipelineLayout: {}", pipeline_layout);
        pipeline_layout
    }
}

pub fn destroy_pipieline_layout(device: &Devuce, pipeline_layout: vk::PipelineLayout) {
    log::info!("Destroy PipelineLayout, {}", pipeline_layout);
    unsafe {
        device.destroy_pipeline_layout(pipeline_layout, None);
    }
}

pub fn create_grahpics_pipeline_data<T: PushConstantInterface>(
    device: &Device,
    render_pass: vk::RenderPass,
    pipeline_data_create_info: &PipelineDataCreateInfo,
    default_push_constant_data: &T,
    descriptor_data: &DescriptorData
) -> PipelineData {
    let vertex_shader_create_info = create_shader_stage_creat_info(
        device,
        &pipeline_data_create_info._pipeline_vertex_shader_file,
        &pipeline_data_create_info._pipeline_shader_defines,
        vk::ShaderStageFlags::VERTEX
    );
    let fragment_shader_create_info = create_shader_stage_creat_info(
        device,
        &pipeline_data_create_info._pipeline_fragment_shader_file,
        &pipeline_data_create_info._pipeline_shader_defines,
        vk::ShaderStageFlags::FRAGMENT
    );
    let depth_stencil_state_create_info = &pipeline_data_create_info._depth_stencil_state_create_info;
    let push_constant_ranges = [vk::PushConstantRange {
        stage_flags: vk::ShaderStageFlags::ALL
        offset: 0,
        size: mem::size_of::<T>() as u32,
    }];
    let descriptor_set_layouts = &[descriptor_data._descriptor_set_layout];
    let shader_stage_infos = vec![vertex_shader_create_info, fragment_shader_create_info];
    // let descriptorSetCount = Constants.swapChainImageCount
    let pipeline_layout = create_pipeline_layout(
        device,
        &push_constant_ranges,
        &descriptor_set_layouts
    );
    let vertex_input_bind_descriptions = [VertexData.get_vertex_input_binding_description()];
    let vertex_input_attribute_descriptions = [VertexData.create_vertex_input_attribute_descriptions()];
    let vertex_input_info = vk::PipelineVertexInputStateCreateInfo::builder()
        .vertex_binding_descriptions(&vertex_input_bind_descriptions)
        .vertex_attribute_descriptions(&vertex_input_attribute_descriptions)
        .build();
    let input_assembly = vk::PipelineInputAssemblyStateCreateInfo::builder()
        .topology(vk::PrimitiveTopology::TRIANGLE_LIST)
        .primitive_restart_enable(false)
        .build();
    let dynamic_state = vk::PipelineDynamicStateCreateInfo::builder()
        .dynamic_states(&pipeline_data_create_info._pipeline_dynamic_states)
    let viewports = [pipeline_data_create_info._pipeline_viewport];
    let scissors = [pipeline_data_create_info._pipeline_scissor_rect];
    let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
        .viewports(&viewports)
        .scissors(&scissors)
        .build();
    let rasterizer = vk::PipelineRasterizationStateCreateInfo::builder()
        .depth_clamp_enable(false)
        .rasterizer_discard_enable(false)
        .polygon_mode(pipeline_data_create_info._pipeline_polygon_mode)
        .cull_mode(pipeline_data_create_info._pipeline_cull_mode)
        .front_face(pipeline_data_create_info._pipeline_front_face)
        .depth_bias_enable(false)
        .depth_bias_constant_factor(0.0)
        .depth_bias_clamp(0.0)
        .depth_bias_slope_factor(0.0)
        .line_width(1.0)
        .build();
    let sample_mask: [vk::SampleMask] = [];
    let multi_sampling = vk::PipelineMultisampleStateCreateInfo::builder()
        .sample_shading_enable(false)
        .rasterization_samples(pipeline_data_create_info._pipeline_sample_count)
        .min_sample_shading(1.0)
        .sample_mask(&[])
        .alpha_to_coverage_enable(false)
        .alpha_to_one_enable(false)
        .build();
    let blend_constants = [0.0, 0.0, 0.0, 0.0];
    let color_blending = vk::PipelineColorBlendStateCreateInfo::builder()
        .logic_op_enable(false)
        .logic_op(vk::LogicOp::COPY)
        .attachments(&pipeline_data_create_info._pipeline_color_blend_modes)
        .blend_constants(blend_constants)
        .build();
    let depth_stencil_sate_create_info = &pipeline_data_create_info._depth_stencil_state_create_info;
    let depth_stencil_state = vk::PipelineDepthStencilStateCreateInfo::builder()
        .depth_test_enable(depth_stencil_sate_create_info._depth_test_enable as bool)
        .depth_write_enable(depth_stencil_sate_create_info._depth_write_enable as bool)
        .depth_compare_op(depth_stencil_sate_create_info._depth_compare_op)
        .depth_bounds_test_enable(false)
        .min_depth_bounds(0.0)
        .max_depth_bounds(1.0)
        .stencil_test_enable(depth_stencil_state_create_info._stencil_test_enable)
        .front(vk::StencilOpState {
            fail_op: depth_stencil_state_create_info._front_fail_op,
            pass_op: depth_stencil_state_create_info._front_pass_op,
            depth_fail_op: depth_stencil_state_create_info._front_depth_fail_op,
            compare_op: depth_stencil_state_create_info._front_compare_op,
            compare_mask: depth_stencil_state_create_info._front_compare_mask,
            write_mask: depth_stencil_state_create_info._front_write_mask,
            reference: depth_stencil_state_create_info._front_reference,
        })
        .back(vk::StencilOpState {
            fail_op: depth_stencil_state_create_info._back_fail_op,
            pass_op: depth_stencil_state_create_info._back_pass_op,
            depth_fail_op: depth_stencil_state_create_info._back_depth_fail_op,
            compare_op: depth_stencil_state_create_info._back_compare_op,
            compare_mask: depth_stencil_state_create_info._back_compare_mask,
            write_mask: depth_stencil_state_create_info._back_write_mask,
            reference: depth_stencil_state_create_info._back_reference,
        })
        .build();
    let grphics_pipeline_create_info = vk::GraphicsPipelineCreateInfo::builder()
        .stages(&shader_stage_infos)

        graphicsPipelineCreateInfo = createvk:: @vk::GraphicsPipelineCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"stageCount" (fromIntegral shaderStageInfoCount)
            &* setListRef @"pStages" shaderStageInfos
            &* setvk::Ref @"pVertexInputState" vertexInputInfo
            &* setvk::Ref @"pInputAssemblyState" inputAssembly
            &* set @"pTessellationState" VK_NULL
            &* setvk::Ref @"pViewportState" viewPortState
            &* setvk::Ref @"pRasterizationState" rasterizer
            &* setvk::Ref @"pMultisampleState" multisampling
            &* setvk::Ref @"pDepthStencilState" depthStencilState
            &* setvk::Ref @"pColorBlendState" colorBlending
            &* case _pipelineDynamicStateList of
                [] -> set @"pDynamicState" VK_NULL
                otherwise -> setvk::Ref @"pDynamicState" dynamicState
            &* set @"layout" pipelineLayout
            &* set @"renderPass" renderPass
            &* set @"subpass" 0
            &* set @"basePipelineHandle" VK_NULL_HANDLE
            &* set @"basePipelineIndex" (-1)

    graphicsPipeline <- alloca $ \graphicsPipelinePtr -> do
        withPtr graphicsPipelineCreateInfo $ \graphicsPipelineCreateInfoPtr -> do
            result <- vkCreateGraphicsPipelines device VK_NULL_HANDLE 1 graphicsPipelineCreateInfoPtr VK_NULL graphicsPipelinePtr
            validationVK result "vkCreateGraphicsPipelines failed!"
        peek graphicsPipelinePtr

    log::info!("createGraphicsPipeline : " ++ Text.unpack _pipelineDataCreateInfoName ++ " (" ++ show graphicsPipeline ++ ")"
    logTrivialInfo $ "    shaderDefines : " ++ show _pipelineShaderDefines
    logTrivialInfo $ "    vertexShader : " ++ show _pipelineVertexShaderFile
    logTrivialInfo $ "    fragmentShader : " ++ show _pipelineFragmentShaderFile

    return PipelineData
        { _pipelineDataName = _pipelineDataCreateInfoName
        , _vertexShaderCreateInfo = vertexShaderCreateInfo
        , _fragmentShaderCreateInfo = fragmentShaderCreateInfo
        , _pipeline = graphicsPipeline
        , _pipelineLayout = pipelineLayout
        , _pipelineDynamicStates = _pipelineDynamicStateList
        , _descriptorData = descriptorData
        }
}

destroyPipelineData :: vk::Device -> PipelineData -> IO ()
destroyPipelineData device pipelineData@PipelineData {..} = do
    log::info!("Destroy GraphicsPipeline : " ++ Text.unpack _pipelineDataName ++ "pipeline " ++ show _pipeline ++ ", pipelineLayout" ++ show _pipelineLayout
    vkDestroyPipeline device _pipeline VK_NULL
    destroyPipelineLayout device _pipelineLayout
    destroyShaderStageCreateInfo device _vertexShaderCreateInfo
    destroyShaderStageCreateInfo device _fragmentShaderCreateInfo
