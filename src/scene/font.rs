use std::cmp::max;

use ash::ext;
use ash::{Device, vk};
use nalgebra::{Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};

use crate::constants;
use crate::core::engine_service_locator::{get_engine_resources, get_renderer_context};
use crate::render_pass::common::render_font;
use crate::renderer::push_constants::{PushConstant, PushConstantName};
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::utility;
use crate::scene::bounding_box::BoundingBox;
use crate::utilities::system::{RcRefCell, newRcRefCell};
use crate::vulkan_context::buffer;
use crate::vulkan_context::descriptor::DescriptorResourceInfo;
use crate::vulkan_context::geometry_buffer::{self, GeometryData, VertexDataBase};
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::SwapchainArray;

pub const UI_RENDER_FONT_PADDING_RATIO: f32 = 0.7;
pub const USE_DISTANCE_FIELD: bool = true;
pub const FONT_SIZE: u32 = 80;
pub const FONT_PADDING: u32 = 1;
pub const KOREAN_FONT_CELL_SIZE: u32 = 64;
pub const KOREAN_FONT_RENDER_SIZE: f32 = 48.0;

#[repr(C)]
pub struct RenderTextInfo {
    pub _render_font_size: u32,
    pub _initial_column: i32,
    pub _initial_row: i32,
    pub _render_text_offset: Vector2<f32>,
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

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_RenderFont {
    pub _inv_canvas_size: Vector2<f32>,
    pub _offset: Vector2<f32>,
    pub _font_size: Vector2<f32>,
    pub _count_of_side: f32,
    pub _reserved0: u32,
}

impl PushConstantName for PushConstant_RenderFont {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderFont"
    }
}

impl PushConstant for PushConstant_RenderFont {}

#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default, PartialEq)]
#[serde(default)]
pub struct GlyphMetric {
    pub _uv_min: Vector2<f32>,
    pub _uv_max: Vector2<f32>,
    pub _plane_bounds: Vector4<f32>,
    pub _advance_x: f32,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct FontDataCreateInfo {
    pub _font_data_name: String,
    pub _range_min: u32,
    pub _range_max: u32,
    pub _text_count: u32,
    pub _count_of_side: u32,
    pub _font_size: f32,
    pub _chars: String,
    pub _glyphs: Vec<GlyphMetric>,
}

impl Default for FontDataCreateInfo {
    fn default() -> FontDataCreateInfo {
        FontDataCreateInfo {
            _font_data_name: String::new(),
            _range_min: 0,
            _range_max: 0,
            _text_count: 0,
            _count_of_side: 0,
            _font_size: 10.0,
            _chars: String::new(),
            _glyphs: Vec::new(),
        }
    }
}

pub struct FontData {
    pub _font_data_name: String,
    pub _range_min: u32,
    pub _range_max: u32,
    pub _text_count: u32,
    pub _count_of_side: u32,
    pub _font_size: Vector2<f32>,
    pub _texture: RcRefCell<TextureData>,
    pub _char_to_index: Vec<i32>,
    pub _glyphs: Vec<GlyphMetric>,
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
            _char_to_index: Vec::new(),
            _glyphs: Vec::new(),
        }
    }
}

impl FontData {
    pub fn is_cjk_font(&self) -> bool {
        self._font_data_name.contains("Korean")
            || self._font_data_name.contains("CJK")
            || (self._font_size.y > 0.0
                && (self._font_size.x / self._font_size.y) >= 0.7
                && !self._char_to_index.is_empty())
    }

    pub fn is_fullwidth(ch: char) -> bool {
        let cp = ch as u32;
        (0xAC00..=0xD7AF).contains(&cp)
            || (0x1100..=0x11FF).contains(&cp)
            || (0x3130..=0x318F).contains(&cp)
            || (0x4E00..=0x9FFF).contains(&cp)
            || (0x3000..=0x303F).contains(&cp)
            || (0xFF01..=0xFF60).contains(&cp)
            || (0xFFE0..=0xFFE6).contains(&cp)
    }

    pub fn get_glyph_index(&self, ch: char) -> Option<u32> {
        let code = ch as usize;
        if !self._char_to_index.is_empty() {
            if code < self._char_to_index.len() {
                let idx = self._char_to_index[code];
                if idx >= 0 {
                    return Some(idx as u32);
                }
            }
            None
        } else if code >= self._range_min as usize && code <= self._range_max as usize {
            Some((code - self._range_min as usize) as u32)
        } else {
            None
        }
    }

    pub fn get_glyph_metric(&self, ch: char) -> Option<&GlyphMetric> {
        let index = self.get_glyph_index(ch)? as usize;
        self._glyphs.get(index)
    }

    pub fn has_glyph_metrics(&self) -> bool {
        !self._glyphs.is_empty()
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct FontVertexData {
    pub _position: Vector3<f32>,
}

impl Default for FontVertexData {
    fn default() -> FontVertexData {
        FontVertexData {
            _position: Vector3::zeros(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FontInstanceData {
    pub _font_render_area: Vector4<f32>,
    pub _font_texcoord: Vector4<f32>,
}

impl Default for FontInstanceData {
    fn default() -> FontInstanceData {
        FontInstanceData {
            _font_render_area: Vector4::zeros(),
            _font_texcoord: Vector4::zeros(),
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
    pub _font_instance_data_list: Vec<FontInstanceData>,
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
            _font_instance_data_list: Vec::new(),
            _render_font_descriptor_sets: SwapchainArray::new(),
        }
    }
}

pub struct FontManager {
    pub _ascii: RcRefCell<FontData>,
    pub _show: bool,
    pub _logs: Vec<String>,
    pub _text_render_data: TextRenderData,
    pub _quad_mesh: GeometryData,
}

impl FontVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
}

impl VertexDataBase for FontVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        geometry_buffer::add_vertex_input_attribute_description(
            &mut vertex_input_attribute_descriptions,
            binding,
            FontVertexData::POSITION,
        );
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![vk::VertexInputBindingDescription {
            binding: 0,
            stride: size_of::<FontVertexData>() as u32,
            input_rate: vk::VertexInputRate::VERTEX,
        }]
    }
}

impl TextRenderData {
    pub fn create_text_render_data(
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        font_data: &RcRefCell<FontData>,
    ) -> TextRenderData {
        let mut text_render_data = TextRenderData {
            _font_data: font_data.clone(),
            ..Default::default()
        };
        text_render_data
            ._font_instance_data_list
            .resize(constants::MAX_FONT_INSTANCE_COUNT, FontInstanceData::default());
        text_render_data.create_texture_render_data_descriptor_sets(device, debug_utils_device);
        text_render_data
    }

    pub fn create_texture_render_data_descriptor_sets(
        &mut self,
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
    ) {
        let engine_resources = get_engine_resources();
        let material_instance = engine_resources.get_material_instance_data("ui/render_font").borrow();
        let render_font_pipeline_binding_data = material_instance.get_default_pipeline_binding_data();
        let font_texture_image_info = DescriptorResourceInfo::DescriptorImageInfo(
            self._font_data.borrow()._texture.borrow().get_default_image_info().clone(),
        );
        self._render_font_descriptor_sets = utility::create_descriptor_sets_by_semantic(
            device,
            debug_utils_device,
            render_font_pipeline_binding_data,
            &[(
                render_font::SEMANTIC_TEXTURE_FONT.to_string(),
                utility::create_swapchain_array(font_texture_image_info.clone()),
            )],
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

        let font_data = self._font_data.borrow();

        // Path A: Standard Typography Glyph Metrics Pipeline
        if font_data.has_glyph_metrics() {
            let em_scale = self._font_size as f32 * UI_RENDER_FONT_PADDING_RATIO;
            let default_advance = 0.5 * em_scale;
            let initial_x = self._initial_column as f32 * default_advance;
            let initial_y = self._initial_row as f32 * em_scale;
            let mut cursor_x = initial_x;
            let mut cursor_y = initial_y;
            let mut max_width = 0.0f32;
            let mut render_index: u32 = 0;
            let mut row: i32 = self._initial_row;

            for ch in self._text.chars() {
                if '\n' == ch {
                    max_width = max_width.max(cursor_x - initial_x);
                    cursor_x = initial_x;
                    cursor_y += em_scale;
                    row += 1;
                } else if '\t' == ch {
                    cursor_x += default_advance * 4.0;
                } else if ' ' == ch {
                    let adv = font_data
                        .get_glyph_metric(' ')
                        .map(|m| m._advance_x * em_scale)
                        .unwrap_or(default_advance);
                    cursor_x += adv;
                } else {
                    if let Some(metric) = font_data.get_glyph_metric(ch) {
                        let x1 = cursor_x + metric._plane_bounds.x * em_scale;
                        let y1 = cursor_y + metric._plane_bounds.y * em_scale;
                        let x2 = cursor_x + metric._plane_bounds.z * em_scale;
                        let y2 = cursor_y + metric._plane_bounds.w * em_scale;

                        if (render_index as usize) < self._font_instance_data_list.len() {
                            let font_instance_data = &mut self._font_instance_data_list[render_index as usize];
                            font_instance_data._font_render_area = Vector4::new(x1, y1, x2, y2);
                            font_instance_data._font_texcoord = Vector4::new(
                                metric._uv_min.x,
                                metric._uv_min.y,
                                metric._uv_max.x,
                                metric._uv_max.y,
                            );
                            render_index += 1;
                        }
                        cursor_x += metric._advance_x * em_scale;
                    } else {
                        cursor_x += default_advance;
                    }
                }
                max_width = max_width.max(cursor_x - initial_x);
            }
            row += 1;

            self._width = max_width;
            self._height = (row - self._initial_row) as f32 * em_scale;
            self._column = if default_advance > 0.0 {
                (self._width / default_advance).round() as i32
            } else {
                0
            };
            self._row = row - self._initial_row;
            self._render_count = render_index;
            return;
        }

        // Fallback: Uniform Grid-based calculation
        let count_of_side = font_data._count_of_side as f32;
        let inv_count_of_side = 1.0 / count_of_side;
        let font_size_ratio = self._font_size as f32 / font_data._font_size.y;
        let font_size: Vector2<f32> = &font_data._font_size * font_size_ratio * UI_RENDER_FONT_PADDING_RATIO;
        let is_cjk_font = font_data.is_cjk_font();
        let char_step_x = if is_cjk_font { font_size.x * 0.5 } else { font_size.x };
        let mut render_index: u32 = 0;
        let mut max_column: i32 = self._initial_column;
        let mut column: i32 = self._initial_column;
        let mut row: i32 = self._initial_row;
        for ch in self._text.chars() {
            if '\n' == ch {
                column = self._initial_column;
                row += 1;
            } else if '\t' == ch {
                column += 4;
            } else if ' ' == ch {
                column += 1;
            } else {
                let char_is_full = is_cjk_font && FontData::is_fullwidth(ch);
                let char_width = if char_is_full { char_step_x * 2.0 } else { char_step_x };

                if let Some(index) = font_data.get_glyph_index(ch) {
                    let texcoord_x = (index % font_data._count_of_side) as f32 * inv_count_of_side;
                    let texcoord_y = (index / font_data._count_of_side) as f32 * inv_count_of_side;

                    let x1 = column as f32 * char_step_x;
                    let y1 = row as f32 * font_size.y;
                    let x2 = x1 + char_width;
                    let y2 = y1 + font_size.y;

                    if (render_index as usize) < self._font_instance_data_list.len() {
                        let font_instance_data = &mut self._font_instance_data_list[render_index as usize];
                        font_instance_data._font_render_area = Vector4::new(x1, y1, x2, y2);
                        if is_cjk_font && !char_is_full {
                            let half_u = inv_count_of_side * 0.25;
                            font_instance_data._font_texcoord.x = texcoord_x + half_u;
                            font_instance_data._font_texcoord.y = texcoord_y;
                            font_instance_data._font_texcoord.z = texcoord_x + inv_count_of_side - half_u;
                            font_instance_data._font_texcoord.w = texcoord_y + inv_count_of_side;
                        } else {
                            font_instance_data._font_texcoord.x = texcoord_x;
                            font_instance_data._font_texcoord.y = texcoord_y;
                            font_instance_data._font_texcoord.z = texcoord_x + inv_count_of_side;
                            font_instance_data._font_texcoord.w = texcoord_y + inv_count_of_side;
                        }
                        render_index += 1;
                    }
                }
                column += if char_is_full { 2 } else { 1 };
            }
            max_column = max(max_column, column);
        }
        row += 1;

        self._column = max_column - self._initial_column;
        self._row = row - self._initial_row;
        self._width = self._column as f32 * char_step_x;
        self._height = self._row as f32 * font_size.y;
        self._render_count = render_index;
    }

    pub fn set_text_render_data(
        &mut self,
        text: String,
        font_data: &RcRefCell<FontData>,
        font_size: u32,
        initial_column: i32,
        initial_row: i32,
        skip_check: bool,
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
    pub fn create_font_manager() -> Box<FontManager> {
        log::info!("create_font_manager");
        Box::new(FontManager {
            _ascii: newRcRefCell(FontData::default()),
            _show: true,
            _logs: Vec::new(),
            _text_render_data: TextRenderData::default(),
            _quad_mesh: GeometryData::default(),
        })
    }

    pub fn initialize_font_manager(&mut self) {
        let renderer_context = get_renderer_context();
        let engine_resources = get_engine_resources();
        let ascii_font_data = engine_resources.get_default_font_data();
        self._ascii = ascii_font_data.clone();
        self._text_render_data = TextRenderData::create_text_render_data(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
            &ascii_font_data,
        );
        self._quad_mesh = self.create_font_vertex_data(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
            renderer_context.get_command_pool(),
            renderer_context.get_graphics_queue(),
            renderer_context.get_device_memory_properties(),
        );
    }

    pub fn create_font_descriptor_sets(&mut self, renderer_context: &RendererContext) {
        self._text_render_data.create_texture_render_data_descriptor_sets(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
        );
    }

    pub fn destroy_font_descriptor_sets(&mut self) {
        self._text_render_data.destroy_text_render_data_descriptor_sets();
    }

    pub fn set_font_data(&mut self, font_data: &RcRefCell<FontData>) {
        self._ascii = font_data.clone();
        let renderer_context = get_renderer_context();
        renderer_context.device_wait_idle();
        self._text_render_data.destroy_text_render_data_descriptor_sets();
        self._text_render_data._font_data = font_data.clone();
        self._text_render_data.create_texture_render_data_descriptor_sets(
            renderer_context.get_device(),
            renderer_context.get_debug_utils(),
        );
    }

    pub fn destroy_font_manager(&mut self, device: &Device) {
        log::info!("destroy_font_manager");
        self._text_render_data.destroy_text_render_data();
        geometry_buffer::destroy_geometry_data(device, &self._quad_mesh);
    }

    pub fn create_font_vertex_data(
        &mut self,
        device: &Device,
        debug_utils_device: &ext::debug_utils::Device,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties,
    ) -> GeometryData {
        log::debug!("create_font_vertex_data");

        let positions: Vec<Vector3<f32>> = vec![
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
            Vector3::new(0.0, 1.0, 0.0),
        ];

        let vertex_data_list = positions
            .iter()
            .map(|position| FontVertexData {
                _position: (*position).clone() as Vector3<f32>,
            })
            .collect();
        let indices: Vec<u32> = vec![0, 3, 2, 2, 1, 0];

        let font_mesh_vertex_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils_device,
            "font_vertex_buffer",
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &vertex_data_list,
        );

        let font_mesh_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            debug_utils_device,
            "font_index_buffer",
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices,
        );

        GeometryData {
            _geometry_name: String::from("font_mesh"),
            _vertex_buffer_data: font_mesh_vertex_buffer,
            _index_buffer_data: font_mesh_index_buffer,
            _vertex_index_count: indices.len() as u32,
            _geometry_bounding_box: BoundingBox::calc_bounding_box(&positions),
        }
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
        renderer_context: &RendererContext,
        render_text_info: &RenderTextInfo,
    ) {
        let engine_resources = get_engine_resources();
        if self._show && 0 < self._logs.len() {
            let text = self._logs.join("\n");

            let skip_check = true;
            self._text_render_data.set_text_render_data(
                text,
                &self._ascii,
                render_text_info._render_font_size,
                render_text_info._initial_column,
                render_text_info._initial_row,
                skip_check,
            );
            let font_data = self._ascii.borrow();
            let framebuffer_data = engine_resources.get_framebuffer_data("render_font").borrow();
            let material_instance_data = engine_resources.get_material_instance_data("ui/render_font").borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let render_font_descriptor_sets = Some(&self._text_render_data._render_font_descriptor_sets);
            let font_width_ratio = font_data._font_size.x / font_data._font_size.y;
            let font_size = Vector2::new(
                render_text_info._render_font_size as f32 * font_width_ratio,
                render_text_info._render_font_size as f32,
            );
            let push_constant_data = PushConstant_RenderFont {
                _offset: render_text_info._render_text_offset.into(),
                _inv_canvas_size: Vector2::new(
                    1.0 / framebuffer_data._framebuffer_info._framebuffer_width as f32,
                    1.0 / framebuffer_data._framebuffer_info._framebuffer_height as f32,
                ),
                _font_size: font_size,
                _count_of_side: font_data._count_of_side as f32,
                _reserved0: 0,
            };

            // upload storage buffer
            let text_count = self._text_render_data._render_count;
            let upload_data = &self._text_render_data._font_instance_data_list[0..text_count as usize];
            let shader_buffer = renderer_context.get_shader_buffer_data_from_str("FontInstanceDataBuffer");
            renderer_context.upload_shader_buffer_data_list(
                command_buffer,
                swapchain_index,
                shader_buffer,
                upload_data,
            );

            // render text
            renderer_context.render_render_pass_pipeline_instanced(
                command_buffer,
                swapchain_index,
                pipeline_binding_data,
                &self._quad_mesh,
                &[],
                text_count,
                None,
                render_font_descriptor_sets,
                Some(&push_constant_data),
            );
        }
    }

    pub fn update_font_manager(&self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_render_data_cjk_spacing_and_uv_crop() {
        let chars_str = " A가";
        let mut char_to_index = Vec::new();
        let mut max_cp = 0;
        for ch in chars_str.chars() {
            max_cp = max_cp.max(ch as usize);
        }
        char_to_index.resize(max_cp + 1, -1);
        for (idx, ch) in chars_str.chars().enumerate() {
            char_to_index[ch as usize] = idx as i32;
        }

        let font_data = newRcRefCell(FontData {
            _font_data_name: String::from("D2Coding-Regular_Korean"),
            _font_size: Vector2::new(64.0, 64.0),
            _count_of_side: 10,
            _char_to_index: char_to_index,
            ..Default::default()
        });

        let mut text_render_data = TextRenderData {
            _font_data: font_data.clone(),
            _font_instance_data_list: vec![FontInstanceData::default(); 10],
            ..Default::default()
        };

        let font_size = 20u32;
        text_render_data.set_text_render_data("A가".to_string(), &font_data, font_size, 0, 0, true);

        assert_eq!(text_render_data._render_count, 2);

        let inv_count_of_side = 1.0 / 10.0;
        let expected_font_size = 20.0 * UI_RENDER_FONT_PADDING_RATIO;
        let char_step_x = expected_font_size * 0.5;

        // 'A' is halfwidth
        let a_inst = &text_render_data._font_instance_data_list[0];
        assert_eq!(a_inst._font_render_area.x, 0.0);
        assert_eq!(a_inst._font_render_area.z, char_step_x);
        assert_eq!(
            a_inst._font_render_area.w - a_inst._font_render_area.y,
            expected_font_size
        );

        // UV crop for 'A': index 1 in 10-count atlas
        let texcoord_x = 1.0 * inv_count_of_side;
        let half_u = inv_count_of_side * 0.25;
        assert_eq!(a_inst._font_texcoord.x, texcoord_x + half_u);
        assert_eq!(a_inst._font_texcoord.z, texcoord_x + inv_count_of_side - half_u);

        // '가' is fullwidth, placed after 'A' (column 1 -> x = 1 * char_step_x)
        let ga_inst = &text_render_data._font_instance_data_list[1];
        assert_eq!(ga_inst._font_render_area.x, char_step_x);
        assert_eq!(ga_inst._font_render_area.z, char_step_x + char_step_x * 2.0);

        // UV for '가': index 2 in 10-count atlas, no crop
        let ga_texcoord_x = 2.0 * inv_count_of_side;
        assert_eq!(ga_inst._font_texcoord.x, ga_texcoord_x);
        assert_eq!(ga_inst._font_texcoord.z, ga_texcoord_x + inv_count_of_side);
    }

    #[test]
    fn test_text_render_data_glyph_metrics_pipeline() {
        let chars_str = " A가";
        let mut char_to_index = Vec::new();
        let mut max_cp = 0;
        for ch in chars_str.chars() {
            max_cp = max_cp.max(ch as usize);
        }
        char_to_index.resize(max_cp + 1, -1);
        for (idx, ch) in chars_str.chars().enumerate() {
            char_to_index[ch as usize] = idx as i32;
        }

        let glyphs = vec![
            GlyphMetric {
                _uv_min: Vector2::new(0.0, 0.0),
                _uv_max: Vector2::new(0.1, 0.1),
                _plane_bounds: Vector4::new(0.0, 0.0, 0.0, 1.0),
                _advance_x: 0.5,
            },
            GlyphMetric {
                _uv_min: Vector2::new(0.125, 0.0),
                _uv_max: Vector2::new(0.175, 0.1),
                _plane_bounds: Vector4::new(-0.0833, -0.1667, 0.5833, 1.1667),
                _advance_x: 0.5,
            },
            GlyphMetric {
                _uv_min: Vector2::new(0.2, 0.0),
                _uv_max: Vector2::new(0.3, 0.1),
                _plane_bounds: Vector4::new(-0.1667, -0.1667, 1.1667, 1.1667),
                _advance_x: 1.0,
            },
        ];

        let font_data = newRcRefCell(FontData {
            _font_data_name: String::from("D2Coding-Regular_Korean"),
            _font_size: Vector2::new(64.0, 64.0),
            _count_of_side: 10,
            _char_to_index: char_to_index,
            _glyphs: glyphs,
            ..Default::default()
        });

        let mut text_render_data = TextRenderData {
            _font_data: font_data.clone(),
            _font_instance_data_list: vec![FontInstanceData::default(); 10],
            ..Default::default()
        };

        let font_size = 20u32;
        text_render_data.set_text_render_data("A가".to_string(), &font_data, font_size, 0, 0, true);

        assert_eq!(text_render_data._render_count, 2);

        let em_scale = 20.0 * UI_RENDER_FONT_PADDING_RATIO;
        let a_inst = &text_render_data._font_instance_data_list[0];
        let ga_inst = &text_render_data._font_instance_data_list[1];

        // 'A' cursor starts at 0.0, advance is 0.5 * em_scale
        assert!((a_inst._font_render_area.x - (-0.0833 * em_scale)).abs() < 1e-4);
        assert!((a_inst._font_render_area.z - (0.5833 * em_scale)).abs() < 1e-4);
        assert!((a_inst._font_texcoord.x - 0.125).abs() < 1e-4);
        assert!((a_inst._font_texcoord.z - 0.175).abs() < 1e-4);

        // '가' cursor starts at advance of 'A' = 0.5 * em_scale
        let cursor_ga = 0.5 * em_scale;
        assert!((ga_inst._font_render_area.x - (cursor_ga + -0.1667 * em_scale)).abs() < 1e-4);
        assert!((ga_inst._font_render_area.z - (cursor_ga + 1.1667 * em_scale)).abs() < 1e-4);
        assert!((ga_inst._font_texcoord.x - 0.2).abs() < 1e-4);
        assert!((ga_inst._font_texcoord.z - 0.3).abs() < 1e-4);

        // Total width = 0.5 (A) + 1.0 (가) = 1.5 * em_scale
        assert!((text_render_data._width - 1.5 * em_scale).abs() < 1e-4);
    }
}
