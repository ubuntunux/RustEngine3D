use std::cmp::max;
use std::path::{ PathBuf };

use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4 };
use ash::{ vk, Device };

use crate::resource::Resources;
use crate::renderer::RendererData;
use crate::utilities::system::{ newRcRefCell, RcRefCell };
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::geometry_buffer::{ self, VertexData };

const MAX_FONT_INSTANCE_COUNT: u32 = 1024;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct FontDataCreateInfo {
    pub _font_data_name: String,
    pub _range_min: u32,
    pub _range_max: u32,
    pub _text_count: u32,
    pub _count_of_side: u32,
    pub _font_size: f32,
    pub _texture_file_path: PathBuf,
}

pub struct FontData {
    pub _font_data_name: String,
    pub _range_min: u32,
    pub _range_max: u32,
    pub _text_count: u32,
    pub _count_of_side: u32,
    pub _font_size: f32,
    pub _texture: RcRefCell<TextureData>,
}

impl Default for FontData {
    fn default() -> FontData {
        FontData {
            _font_data_name: String::new(),
            _range_min: 0,
            _range_max: 0,
            _text_count: 0,
            _count_of_side: 0,
            _font_size: 0.0,
            _texture: newRcRefCell(TextureData::default()),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct FontVertexData {
    pub _position: Vector3<f32>,
}

impl Default for FontVertexData {
    fn default() -> FontVertexData {
        FontVertexData {
            _position: Vector3::zeros()
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct FontInstanceData {
    pub _font_infos: Vector4<f32>,
}

impl Default for FontInstanceData {
    fn default() -> FontInstanceData {
        FontInstanceData {
            _font_infos: Vector4::zeros()
        }
    }
}

pub struct TextRenderData {
    pub _text: String,
    pub _column: i32,
    pub _row: i32,
    pub _font_size: u32,
    pub _width: f32,
    pub _height: f32,
    pub _initial_column: i32,
    pub _initial_row: i32,
    pub _font_data: RcRefCell<FontData>,
    pub _render_count: u32,
    pub _render_instance_data: Vec<FontInstanceData>,
    pub _font_mesh_instance_buffer: BufferData,
}

impl Default for TextRenderData {
    fn default() -> TextRenderData {
        TextRenderData {
            _text: String::new(),
            _column: 0,
            _row: 0,
            _font_size: 0,
            _width: 0.0,
            _height: 0.0,
            _initial_column: 0,
            _initial_row: 0,
            _font_data: newRcRefCell(FontData::default()),
            _render_count: 0,
            _render_instance_data: Vec::new(),
            _font_mesh_instance_buffer: BufferData::default(),
        }
    }
}

pub struct FontManager {
    pub _ascii: RcRefCell<FontData>,
    pub _show: bool,
    pub _logs: Vec<String>,
    pub _text_render_data: TextRenderData,
    pub _font_mesh_vertex_buffer: BufferData,
    pub _font_mesh_instance_buffer: BufferData,
    pub _font_mesh_index_buffer: BufferData,
}


impl FontVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
    const FONT_INFOS: vk::Format = vk::Format::R32G32B32A32_SFLOAT;
}

impl VertexData for FontVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        geometry_buffer::add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, FontVertexData::POSITION);
        geometry_buffer::add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, FontVertexData::FONT_INFOS);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![
            vk::VertexInputBindingDescription {
                binding: 0,
                stride: std::mem::size_of::<FontVertexData>() as u32,
                input_rate: vk::VertexInputRate::VERTEX
            },
            vk::VertexInputBindingDescription {
                binding: 1,
                stride: std::mem::size_of::<FontInstanceData>() as u32,
                input_rate: vk::VertexInputRate::INSTANCE
            },
        ]
    }
}

impl TextRenderData {
    pub fn create_text_render_data(font_data: &RcRefCell<FontData>) -> TextRenderData {
        TextRenderData {
            _text: String::new(),
            _column: 0,
            _row: 0,
            _font_size: 10,
            _width: 10.0,
            _height: 0.0,
            _initial_column: 0,
            _initial_row: 0,
            _font_data: font_data.clone(),
            _render_count: 0,
            _render_instance_data: Vec::new(),
            _font_mesh_instance_buffer: BufferData::default(),
        }
    }

    pub fn destroy_text_render_data(&mut self) {

    }

    pub fn get_text(&self) -> &String {
        &self._text
    }

    pub fn set_text_inner(&mut self, text: String) {
        self._text = text;

        let font_data = &self._font_data.borrow();
        let range_min = font_data._range_min;
        let count_of_side = font_data._count_of_side;
        let ratio = 1.0 / font_data._count_of_side as f32;
        let text_count = self._text.len();

        if self._render_instance_data.len() < text_count {
            self._render_instance_data.resize(text_count, FontInstanceData::default());
        }

        let mut render_index: u32 = 0;
        let mut max_column: i32 = self._initial_column;
        let mut column: i32 = self._initial_column;
        let mut row: i32 = self._initial_row;
        for c in self._text.as_bytes().iter() {
            let ch = (*c) as char;
            if '\n' == ch {
                column = self._initial_column;
                row += 1;
            } else if '\t' == ch {
                column += 1;
            } else if ' ' == ch {
                column += 1;
            } else {
                let index: u32 = max(0, (*c) as i32 - range_min as i32) as u32;
                let texcoord_x = (index % count_of_side) as f32 * ratio;
                let texcoord_y = (count_of_side as i32 - 1 - (index as f32 * ratio) as i32) as f32 * ratio;
                self._render_instance_data[render_index as usize] = FontInstanceData {
                    _font_infos: Vector4::new(column as f32, row as f32, texcoord_x, texcoord_y),
                };
                render_index += 1;
                column += 1;
            }
            max_column = max(max_column, column);
        }
        row += 1;

        self._column = max_column - self._initial_column;
        self._row = row - self._initial_row;
        self._width = self._column as f32 * self._font_size as f32;
        self._height = self._row as f32 * self._font_size as f32;
        self._render_count = render_index;
    }

    pub fn set_text_render_data(
        &mut self,
        text: String,
        font_data: &RcRefCell<FontData>,
        font_size: u32,
        initial_column: i32,
        initial_row: i32,
        skip_check: bool
    ) -> bool {
        if !skip_check && text == self._text {
            return false;
        }

        self._font_data = font_data.clone();
        self._font_size = font_size;
        self._initial_column = initial_column;
        self._initial_row = initial_row;
        self.set_text_inner(text);
        true
    }
}

impl FontManager {
    pub fn create_font_manager() -> FontManager {
        log::info!("create_font_manager");
        FontManager {
            _ascii: newRcRefCell(FontData::default()),
            _show: true,
            _logs: Vec::new(),
            _text_render_data: TextRenderData::default(),
            _font_mesh_vertex_buffer: BufferData::default(),
            _font_mesh_instance_buffer: BufferData::default(),
            _font_mesh_index_buffer: BufferData::default(),
        }
    }

    pub fn initialize_font_manager(&mut self, renderer_data: &RendererData, resources: &Resources) {
        let ascii_font_data = resources.get_font_data("NanumBarunGothic_Basic_Latin");
        self._ascii = ascii_font_data.clone();
        self._text_render_data = TextRenderData::create_text_render_data(&ascii_font_data);
        self.create_font_vertex_data(renderer_data.get_device(), renderer_data.get_command_pool(), renderer_data.get_graphics_queue(), renderer_data.get_device_memory_properties());
    }

    pub fn destroy_font_manager(&mut self, device: &Device) {
        log::info!("destroy_font_manager");
        self._text_render_data.destroy_text_render_data();
        buffer::destroy_buffer_data(device, &self._font_mesh_vertex_buffer);
        buffer::destroy_buffer_data(device, &self._font_mesh_instance_buffer);
        buffer::destroy_buffer_data(device, &self._font_mesh_index_buffer);
    }

    pub fn create_font_vertex_data(
        &mut self,
        device: &Device,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties
    ) {
        log::info!("create_font_vertex_data");
        let positions: Vec<Vector3<f32>> = vec![Vector3::new(-1.0, -1.0, 0.0), Vector3::new(1.0, -1.0, 0.0), Vector3::new(1.0, 1.0, 0.0), Vector3::new(-1.0, 1.0, 0.0)];
        let vertex_datas = positions.iter().map(|position| FontVertexData { _position: (*position).clone() as Vector3<f32> }).collect();
        let indices: Vec<u32> = vec![0, 3, 2, 2, 1, 0];

        self._font_mesh_vertex_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &vertex_datas,
        );

        let instance_buffer_size = (std::mem::size_of::<FontInstanceData>() * MAX_FONT_INSTANCE_COUNT as usize) as vk::DeviceSize;
        let instance_buffer_usage_flags = vk::BufferUsageFlags::VERTEX_BUFFER | vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST;
        let instance_buffer_memory_property_flags = vk::MemoryPropertyFlags::DEVICE_LOCAL | vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
        self._font_mesh_instance_buffer = buffer::create_buffer_data(
            device,
            device_memory_properties,
            instance_buffer_size,
            instance_buffer_usage_flags,
            instance_buffer_memory_property_flags,
        );

        self._font_mesh_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices
        );
    }

    pub fn clear_logs(&mut self) {
        self._logs.clear();
        self._text_render_data.set_text_render_data(String::from(""), &self._ascii, 12, 0, 0, false);
    }

    pub fn toggle(&mut self) {
        self._show = !self._show;
    }

    pub fn log(&mut self, text: String) {
        if self._show {
            self._logs.push(text);
        }
    }

    pub fn render_log(&mut self, renderer_data: &RendererData, canvas_width: u32, canvas_height: u32) {
        if self._show && 0 < self._logs.len() {
            let text = self._logs.join("\n");
            self._logs.clear();
            self._text_render_data.set_text_render_data(text, &self._ascii, 12, 0, 0, true);
            let offset_x = 0;
            let offset_y = (canvas_height - self._text_render_data._font_size) as i32;

            //     self.font_shader.bind_uniform_data("texture_font", text_render_data.font_data.texture)
            //     self.font_shader.bind_uniform_data("font_size", text_render_data.font_size)
            //     self.font_shader.bind_uniform_data("offset", (offset_x, offset_y))
            //     self.font_shader.bind_uniform_data("inv_canvas_size", (1.0 / canvas_width, 1.0 / canvas_height))
            //     self.font_shader.bind_uniform_data("count_of_side", text_render_data.font_data.count_of_side)
            //     self.postprocess.draw_elements_instanced(text_render_data.render_count, self.font_instance_buffer, [text_render_data.render_queue, ])
        }
    }
}