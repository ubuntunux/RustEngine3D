use ash::{Device, vk};
use ash::ext;
use nalgebra::{Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};

use crate::constants;
use crate::renderer::renderer_context::RendererContext;
use crate::resource::resource::EngineResources;
use crate::scene::bounding_box::calc_bounding_box;
use crate::vulkan_context::buffer;
use crate::vulkan_context::geometry_buffer::{self, GeometryData, VertexDataBase};
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
            _position: Vector4::zeros(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DebugLineInstanceData {
    pub _positions0: Vector3<f32>,
    pub _color: u32,
    pub _positions1: Vector3<f32>,
    pub _is_debug_line_3d: u32,
}

impl Default for DebugLineInstanceData {
    fn default() -> DebugLineInstanceData {
        DebugLineInstanceData {
            _positions0: Vector3::zeros(),
            _color: get_color32(255, 255, 255, 255),
            _positions1: Vector3::zeros(),
            _is_debug_line_3d: 0,
        }
    }
}

pub struct DebugLineManager {
    pub _show: bool,
    pub _debug_line_geometry_data: GeometryData,
    pub _debug_line_instance_data_list: Vec<DebugLineInstanceData>,
}

// Implementations
impl DebugLineVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32A32_SFLOAT;
}

impl VertexDataBase for DebugLineVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions =
            Vec::<vk::VertexInputAttributeDescription>::new();
        geometry_buffer::add_vertex_input_attribute_description(
            &mut vertex_input_attribute_descriptions,
            0,
            DebugLineVertexData::POSITION,
        );
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![vk::VertexInputBindingDescription {
            binding: 0,
            stride: std::mem::size_of::<DebugLineVertexData>() as u32,
            input_rate: vk::VertexInputRate::VERTEX,
        }]
    }
}

impl DebugLineManager {
    pub fn create_debug_line_manager() -> Box<DebugLineManager> {
        log::info!("create_debug_line_manager");
        Box::new(DebugLineManager {
            _show: true,
            _debug_line_geometry_data: GeometryData::default(),
            _debug_line_instance_data_list: Vec::new(),
        })
    }

    pub fn initialize_debug_line_manager(&mut self, renderer_context: &RendererContext) {
        self._debug_line_geometry_data = DebugLineManager::create_debug_line_vertex_data(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
            renderer_context.get_command_pool(),
            renderer_context.get_graphics_queue(),
            renderer_context.get_device_memory_properties(),
        );
    }

    pub fn destroy_debug_line_manager(&mut self, device: &Device) {
        log::info!("destroy_debug_line_manager");
        geometry_buffer::destroy_geometry_data(device, &self._debug_line_geometry_data);
    }

    pub fn create_debug_line_vertex_data(
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    ) -> GeometryData {
        log::debug!("create_debug_line_vertex_data");
        let positions: Vec<Vector4<f32>> = vec![
            Vector4::new(0.0, 0.0, 0.0, 0.0),
            Vector4::new(1.0, 0.0, 0.0, 0.0),
        ];
        let vertex_data_list = positions
            .iter()
            .map(|position| DebugLineVertexData {
                _position: (*position).clone() as Vector4<f32>,
            })
            .collect();
        let indices: Vec<u32> = vec![0, 1];

        let debug_line_vertex_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils_device,
            "debug_line_vertex_buffer",
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &vertex_data_list,
        );

        let debug_line_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils_device,
            "debug_line_index_buffer",
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices,
        );

        GeometryData {
            _geometry_name: String::from("debug_line"),
            _vertex_buffer_data: debug_line_vertex_buffer,
            _index_buffer_data: debug_line_index_buffer,
            _vertex_index_count: indices.len() as u32,
            _geometry_bounding_box: calc_bounding_box(&positions),
        }
    }

    // screen size coordinate
    pub fn add_debug_line_2d(
        &mut self,
        position0: &Vector2<f32>,
        position1: &Vector2<f32>,
        color: u32,
    ) {
        self._debug_line_instance_data_list
            .push(DebugLineInstanceData {
                _positions0: Vector3::new(position0.x, position0.y, 0.0),
                _color: color,
                _positions1: Vector3::new(position1.x, position1.y, 0.0),
                _is_debug_line_3d: 0,
            });
    }

    pub fn add_debug_line_3d(
        &mut self,
        position0: &Vector3<f32>,
        position1: &Vector3<f32>,
        color: u32,
    ) {
        self._debug_line_instance_data_list
            .push(DebugLineInstanceData {
                _positions0: position0.clone(),
                _color: color,
                _positions1: position1.clone(),
                _is_debug_line_3d: 1,
            });
    }

    pub fn render_debug_line(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources,
    ) {
        // Test DebugLine
        // {
        //     let t = renderer_context._renderer_data._scene_constants._time;
        //     self.add_debug_line_3d(
        //         &Vector3::new(5.0, 12.0, 5.0),
        //         &Vector3::new(5.0 + t.sin() * 10.0, 12.0, 5.0 + t.cos() * 10.0),
        //         get_color32(0, 0, 255, 255),
        //     );
        // }

        if self._show && 0 < self._debug_line_instance_data_list.len() {
            let material_instance_data = engine_resources
                .get_material_instance_data("common/render_debug_line")
                .borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let render_debug_line_descriptor_sets = Some(&pipeline_binding_data._descriptor_sets);

            // upload storage buffer
            let debug_line_count = constants::MAX_DEBUG_LINE_INSTANCE_COUNT
                .min(self._debug_line_instance_data_list.len())
                as u32;
            renderer_context.upload_shader_buffer_data_list(
                command_buffer,
                swapchain_index,
                renderer_context.get_shader_buffer_data_from_str("DebugLineInstanceDataBuffer"),
                &self._debug_line_instance_data_list,
            );
            self._debug_line_instance_data_list.clear();

            // render
            renderer_context.render_render_pass_pipeline_instanced(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                &self._debug_line_geometry_data,
                &[],
                debug_line_count,
                None,
                render_debug_line_descriptor_sets,
                None,
            );
        }
    }

    pub fn update_debug_line(&self) {}
}
