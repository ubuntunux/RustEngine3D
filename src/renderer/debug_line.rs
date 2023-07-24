use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4 };
use ash::{ vk, Device };
use ash::extensions::ext::DebugUtils;

use crate::constants;
use crate::renderer::renderer_context::RendererContext;
use crate::resource::resource::EngineResources;
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::geometry_buffer::{ self, VertexDataBase };
use crate::vulkan_context::vulkan_context::get_color32;

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct DebugLineVertexData {
    pub _position: Vector4<f32>,
}

impl Default for DebugLineVertexData {
    fn default() -> DebugLineVertexData {
        DebugLineVertexData {
            _position: Vector4::zeros()
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DebugLineInstanceData {
    pub _positions0: Vector3<f32>,
    pub _color: u32,
    pub _positions1: Vector3<f32>,
    pub _is_debug_line_3d: u32
}

impl Default for DebugLineInstanceData {
    fn default() -> DebugLineInstanceData {
        DebugLineInstanceData {
            _positions0: Vector3::zeros(),
            _color: get_color32(255, 255, 255, 255),
            _positions1: Vector3::zeros(),
            _is_debug_line_3d: 0
        }
    }
}

pub struct DebugLineManager {
    pub _show: bool,
    pub _debug_line_vertex_buffer: BufferData,
    pub _debug_line_index_buffer: BufferData,
    pub _debug_line_index_count: u32,
    pub _debug_line_instance_datas: Vec<DebugLineInstanceData>,
}

// Implementations
impl DebugLineVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32A32_SFLOAT;
}

impl VertexDataBase for DebugLineVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        geometry_buffer::add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, 0, DebugLineVertexData::POSITION);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![
            vk::VertexInputBindingDescription {
                binding: 0,
                stride: std::mem::size_of::<DebugLineVertexData>() as u32,
                input_rate: vk::VertexInputRate::VERTEX
            }
        ]
    }
}

impl DebugLineManager {
    pub fn create_debug_line_manager() -> DebugLineManager {
        log::info!("create_debug_line_manager");
        DebugLineManager {
            _show: true,
            _debug_line_vertex_buffer: BufferData::default(),
            _debug_line_index_buffer: BufferData::default(),
            _debug_line_index_count: 0,
            _debug_line_instance_datas: Vec::new(),
        }
    }

    pub fn initialize_debug_line_manager(&mut self, renderer_context: &RendererContext) {
        self.create_debug_line_vertex_data(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
            renderer_context.get_command_pool(),
            renderer_context.get_graphics_queue(),
            renderer_context.get_device_memory_properties()
        );
    }

    pub fn destroy_debug_line_manager(&mut self, device: &Device) {
        log::info!("destroy_debug_line_manager");
        buffer::destroy_buffer_data(device, &self._debug_line_vertex_buffer);
        buffer::destroy_buffer_data(device, &self._debug_line_index_buffer);
    }

    pub fn create_debug_line_vertex_data(
        &mut self,
        device: &Device,
        debug_utils: &DebugUtils,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties
    ) {
        log::debug!("create_debug_line_vertex_data");
        let positions: Vec<Vector4<f32>> = vec![Vector4::new(0.0, 0.0, 0.0, 0.0), Vector4::new(1.0, 0.0, 0.0, 0.0)];
        let vertex_datas = positions.iter().map(|position| DebugLineVertexData { _position: (*position).clone() as Vector4<f32> }).collect();
        let indices: Vec<u32> = vec![0, 1];

        self._debug_line_vertex_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils,
            "debug_line_vertex_buffer",
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &vertex_datas,
        );

        self._debug_line_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils,
            "debug_line_index_buffer",
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices
        );
        self._debug_line_index_count = indices.len() as u32;
    }

    pub fn add_debug_line_2d(&mut self, position0: &Vector3<f32>, position1: &Vector3<f32>, color: u32) {
        self._debug_line_instance_datas.push(
            DebugLineInstanceData {
                _positions0: position0.clone(),
                _color: color,
                _positions1: position1.clone(),
                _is_debug_line_3d: 0
            }
        );
    }

    pub fn add_debug_line_3d(&mut self, position0: &Vector3<f32>, position1: &Vector3<f32>, color: u32) {
        self._debug_line_instance_datas.push(
            DebugLineInstanceData {
                _positions0: position0.clone(),
                _color: color,
                _positions1: position1.clone(),
                _is_debug_line_3d: 1
            }
        );
    }

    pub fn render_debug_line(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources
    ) {
        // Test Debugline
        // let t = renderer_context._renderer_data._scene_constants._time;
        // self.add_debug_line_3d(
        //     &Vector3::new(5.0, 12.0, 5.0),
        //     &Vector3::new(5.0 + t.sin() * 10.0, 12.0, 5.0 + t.cos() * 10.0),
        //     get_color32(0, 0, 255, 255)
        // );

        if self._show && 0 < self._debug_line_instance_datas.len() {
            let material_instance_data = engine_resources.get_material_instance_data("common/render_debug_line").borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let render_pass_data = &pipeline_binding_data.get_render_pass_data().borrow();
            let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
            let render_debug_line_descriptor_sets = Some(&pipeline_binding_data._descriptor_sets);
            let none_framebuffer_data = None;

            // upload storage buffer
            let debug_line_count = constants::MAX_DEBUG_LINE_INSTANCE_COUNT.min(self._debug_line_instance_datas.len()) as u32;
            let upload_data = &self._debug_line_instance_datas;
            let shader_buffer = renderer_context.get_shader_buffer_data_from_str("DebugLineInstanceDataBuffer");
            renderer_context.upload_shader_buffer_datas(command_buffer, swapchain_index, shader_buffer, upload_data);
            self._debug_line_instance_datas.clear();

            // render
            renderer_context.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, none_framebuffer_data);
            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, render_debug_line_descriptor_sets);
            renderer_context.draw_indexed(
                command_buffer,
                &[self._debug_line_vertex_buffer._buffer],
                &[],
                debug_line_count,
                self._debug_line_index_buffer._buffer,
                self._debug_line_index_count,
            );
            renderer_context.end_render_pass(command_buffer);
        }
    }

    pub fn update(&self) {

    }
}