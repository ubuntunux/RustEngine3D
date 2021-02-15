use std::cmp::max;
use std::path::{ PathBuf };

use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector2, Vector3 };
use ash::{ vk, Device };

use crate::resource::Resources;
use crate::renderer::renderer::{ RendererData };
use crate::renderer::shader_buffer_datas::ShaderBufferDataType;
use crate::renderer::utility;
use crate::utilities::system::{ newRcRefCell, RcRefCell };
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::descriptor::DescriptorResourceInfo;
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::geometry_buffer::{ self, VertexData };
use crate::vulkan_context::vulkan_context::SwapchainArray;

// MAX_FONT_INSTANCE_COUNT must match with render_font_common.glsl
pub const MAX_FONT_INSTANCE_COUNT: u32 = 1024;

pub const USE_DISTANCE_FIELD: bool = false;
pub const FONT_SIZE: u32 = 20;
pub const FONT_PADDING: u32 = 1;

pub struct RenderTextInfo {
    pub _render_font_size: u32,
    pub _initial_column: i32,
    pub _initial_row: i32,
    pub _render_text_offset: Vector2<f32>
}

impl Default for RenderTextInfo {
    fn default() -> RenderTextInfo {
        RenderTextInfo {
            _render_font_size: 12,
            _initial_column: 0,
            _initial_row: 0,
            _render_text_offset: Vector2::zeros(),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderFont {
    pub _inv_canvas_size: Vector2<f32>,
    pub _offset: Vector2<f32>,
    pub _font_size: Vector2<f32>,
    pub _count_of_side: f32,
    pub _reserved0: u32,
}


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
    pub _font_size: Vector2<f32>,
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
            _font_size: Vector2::zeros(),
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

#[derive(Debug, Clone, Copy)]
pub struct FontInstanceData {
    pub _font_texcoord: Vector2<f32>,
    pub _font_column: f32,
    pub _font_row: f32,
}

impl Default for FontInstanceData {
    fn default() -> FontInstanceData {
        FontInstanceData {
            _font_texcoord: Vector2::zeros(),
            _font_column: 0.0,
            _font_row: 0.0,
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
    pub _font_instance_datas: [FontInstanceData; MAX_FONT_INSTANCE_COUNT as usize],
    pub _render_font_descriptor_sets: SwapchainArray<vk::DescriptorSet>,
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
            _font_instance_datas: [FontInstanceData::default(); MAX_FONT_INSTANCE_COUNT as usize],
            _render_font_descriptor_sets: SwapchainArray::new(),
        }
    }
}

pub struct FontManager {
    pub _ascii: RcRefCell<FontData>,
    pub _show: bool,
    pub _logs: Vec<String>,
    pub _text_render_data: TextRenderData,
    pub _font_mesh_vertex_buffer: BufferData,
    pub _font_mesh_index_buffer: BufferData,
    pub _font_mesh_index_count: u32,
}


impl FontVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
}

impl VertexData for FontVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        geometry_buffer::add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, FontVertexData::POSITION);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![
            vk::VertexInputBindingDescription {
                binding: 0,
                stride: std::mem::size_of::<FontVertexData>() as u32,
                input_rate: vk::VertexInputRate::VERTEX
            },
        ]
    }
}

impl TextRenderData {
    pub fn create_text_render_data(device: &Device, resources: &Resources, font_data: &RcRefCell<FontData>) -> TextRenderData {
        let mut text_render_data = TextRenderData {
            _font_data: font_data.clone(),
            ..Default::default()
        };
        text_render_data.create_texture_render_data_descriptor_sets(device, resources);
        text_render_data
    }

    pub fn create_texture_render_data_descriptor_sets(&mut self, device: &Device, resources: &Resources) {
        let material_instance = resources.get_material_instance_data("render_font").borrow();
        let render_font_pipeline_binding_data = material_instance.get_default_pipeline_binding_data();
        let font_texture_image_info = DescriptorResourceInfo::DescriptorImageInfo(self._font_data.borrow()._texture.borrow().get_default_image_info().clone());
        self._render_font_descriptor_sets = utility::create_descriptor_sets(
            device,
            render_font_pipeline_binding_data,
            &[ (0, utility::create_swapchain_array(font_texture_image_info.clone())) ]
        );
    }

    pub fn destroy_text_render_data(&mut self) {
        self.destroy_text_render_data_descriptor_sets();
    }

    pub fn destroy_text_render_data_descriptor_sets(&mut self) {
        self._render_font_descriptor_sets.clear();
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
        let mut render_index: u32 = 0;
        let mut max_column: i32 = self._initial_column;
        let mut column: i32 = self._initial_column;
        let mut row: i32 = self._initial_row;
        for c in self._text.as_bytes().iter() {
            let ch = (*c) as char;
            if '\n' == ch {
                column = self._initial_column;
                row -= 1;
            } else if '\t' == ch {
                column += 1;
            } else if ' ' == ch {
                column += 1;
            } else {
                let index: u32 = max(0, (*c) as i32 - range_min as i32) as u32;
                let texcoord_x = (index % count_of_side) as f32 * ratio;
                let texcoord_y = (index / count_of_side) as f32 * ratio;
                let font_instance_data = &mut self._font_instance_datas[render_index as usize];
                font_instance_data._font_column = column as f32;
                font_instance_data._font_row = row as f32;
                font_instance_data._font_texcoord.x = texcoord_x;
                font_instance_data._font_texcoord.y = texcoord_y;
                render_index += 1;
                column += 1;
            }
            max_column = max(max_column, column);
        }
        row += 1;

        self._column = max_column - self._initial_column;
        self._row = row - self._initial_row;
        self._width = self._column as f32 * font_data._font_size.x;
        self._height = self._row as f32 * font_data._font_size.y;
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
            _font_mesh_index_buffer: BufferData::default(),
            _font_mesh_index_count: 0,
        }
    }

    pub fn initialize_font_manager(&mut self, renderer_data: &RendererData, resources: &Resources) {
        let ascii_font_data = resources.get_font_data("NanumGothic_Coding_Basic_Latin");
        self._ascii = ascii_font_data.clone();
        self._text_render_data = TextRenderData::create_text_render_data(renderer_data.get_device(), resources, &ascii_font_data);
        self.create_font_vertex_data(renderer_data.get_device(), renderer_data.get_command_pool(), renderer_data.get_graphics_queue(), renderer_data.get_device_memory_properties());
    }

    pub fn create_font_descriptor_sets(&mut self, renderer_data: &RendererData, resources: &Resources) {
        self._text_render_data.create_texture_render_data_descriptor_sets(renderer_data.get_device(), resources);
    }

    pub fn destroy_font_descriptor_sets(&mut self) {
        self._text_render_data.destroy_text_render_data_descriptor_sets();
    }

    pub fn destroy_font_manager(&mut self, device: &Device) {
        log::info!("destroy_font_manager");
        self._text_render_data.destroy_text_render_data();
        buffer::destroy_buffer_data(device, &self._font_mesh_vertex_buffer);
        buffer::destroy_buffer_data(device, &self._font_mesh_index_buffer);
    }

    pub fn create_font_vertex_data(
        &mut self,
        device: &Device,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties
    ) {
        log::debug!("create_font_vertex_data");
        let positions: Vec<Vector3<f32>> = vec![Vector3::new(-0.5, -0.5, 0.0), Vector3::new(0.5, -0.5, 0.0), Vector3::new(0.5, 0.5, 0.0), Vector3::new(-0.5, 0.5, 0.0)];
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

        self._font_mesh_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices
        );
        self._font_mesh_index_count = indices.len() as u32;
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

    pub fn render_text(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_data: &RendererData,
        resources: &Resources,
        render_text_info: &RenderTextInfo
    ) {
        if self._show && 0 < self._logs.len() {
            let text = self._logs.join("\n");
            self._logs.clear();

            let skip_check = true;
            self._text_render_data.set_text_render_data(
                text,
                &self._ascii,
                render_text_info._render_font_size,
                render_text_info._initial_column,
                render_text_info._initial_row,
                skip_check
            );
            let font_data = self._ascii.borrow();
            let framebuffer_data = resources.get_framebuffer_data("render_font").borrow();
            let material_instance_data = resources.get_material_instance_data("render_font").borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let render_pass_data = &pipeline_binding_data.get_render_pass_data().borrow();
            let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
            let none_framebuffer_data = None;
            let render_font_descriptor_sets = Some(&self._text_render_data._render_font_descriptor_sets);
            let font_width_ratio = font_data._font_size.x / font_data._font_size.y;
            let font_size = Vector2::new(render_text_info._render_font_size as f32 * font_width_ratio, render_text_info._render_font_size as f32);
            let push_constant_data = PushConstant_RenderFont {
                _offset: render_text_info._render_text_offset.into(),
                _inv_canvas_size: Vector2::new(1.0 / framebuffer_data._framebuffer_info._framebuffer_width as f32, 1.0 / framebuffer_data._framebuffer_info._framebuffer_height as f32),
                _font_size: font_size,
                _count_of_side: font_data._count_of_side as f32,
                _reserved0: 0,
            };

            // upload storage buffer
            let text_count = self._text_render_data._render_count;
            let upload_data = &self._text_render_data._font_instance_datas[0..text_count as usize];
            renderer_data.upload_shader_buffer_datas(command_buffer, swapchain_index, ShaderBufferDataType::FontInstanceDataBuffer, upload_data);

            // render text
            renderer_data.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, none_framebuffer_data);
            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, render_font_descriptor_sets);
            renderer_data.upload_push_constant_data(command_buffer, pipeline_data, &push_constant_data);
            renderer_data.draw_elements(
                command_buffer,
                &[self._font_mesh_vertex_buffer._buffer],
                &[],
                text_count,
                self._font_mesh_index_buffer._buffer,
                self._font_mesh_index_count,
            );
            renderer_data.end_render_pass(command_buffer);
        }
    }

    pub fn update(&self) {

    }
}