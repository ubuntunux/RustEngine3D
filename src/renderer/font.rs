use std::cmp::max;
use std::path::{ PathBuf };

use serde::{ Serialize, Deserialize };
use nalgebra::Vector4;

use crate::vulkan_context::texture::TextureData;
use crate::utilities::system::RcRefCell;
use crate::renderer::RendererData;
use crate::resource::Resources;

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
    pub _render_queue: Vec<Vector4<f32>>,
}

pub struct FontManager {
    pub _ascii: RcRefCell<FontData>,
    pub _show: bool,
    pub _logs: Vec<String>,
    pub _text_render_data: TextRenderData,
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
            _render_queue: Vec::new(),
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

        if self._render_queue.len() < text_count {
            self._render_queue.resize(text_count, Vector4::zeros());
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
                self._render_queue[render_index as usize] = Vector4::new(column as f32, row as f32, texcoord_x, texcoord_y);
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
    pub fn create_font_manager(resources: &Resources) -> FontManager {
        let ascii_font_data = resources.get_font_data("NanumBarunGothic_Basic_Latin");
        FontManager {
            _ascii: ascii_font_data.clone(),
            _show: true,
            _logs: Vec::new(),
            _text_render_data: TextRenderData::create_text_render_data(ascii_font_data)
        }
    }

    pub fn destroy_font_manager(&mut self) {
        self._text_render_data.destroy_text_render_data();
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
            renderer_data.render_text(&self._text_render_data, offset_x, offset_y, canvas_width, canvas_height);
        }
    }
}