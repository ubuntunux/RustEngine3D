use std::boxed::Box;
use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector2, Vector3, Vector4, Matrix4 };
use ash::{ vk, Device };

use crate::resource::Resources;
use crate::renderer::font::FontData;
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::renderer::{ RendererData };
use crate::renderer::shader_buffer_datas::ShaderBufferDataType;
use crate::renderer::transform_object::TransformObjectData;
use crate::utilities::system::{ self, RcRefCell };
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::geometry_buffer::{ self, VertexData };
use crate::vulkan_context::render_pass::{ PipelineData };
use crate::vulkan_context::vulkan_context::{ get_color32 };

// must match with render_ui_common.glsl
pub const MAX_UI_INSTANCE_COUNT: u32 = 1024;
pub const UI_RENDER_FLAG_NONE: u32 = 0;
pub const UI_RENDER_FLAG_RENDER_TEXT: u32 = 1 << 0;
pub const UI_RENDER_FLAG_RENDER_TEXTURE: u32 = 1 << 1;
pub const UI_INDEX_LEFT: usize = 0; // x
pub const UI_INDEX_TOP: usize = 1; // y
pub const UI_INDEX_RIGHT: usize = 2; // z
pub const UI_INDEX_BOTTOM: usize = 3; // w

pub const DEFAILT_HORIZONTAL_ALIGN: HorizontalAlign = HorizontalAlign::LEFT;
pub const DEFAILT_VERTICAL_ALIGN: VerticalAlign = VerticalAlign::TOP;

// |--ui-size----------------------------------------------------------------------------|
// |--margin--|--border--|--padding--|--contents-size--|--padding--|--border--|--margin--|
//            |--render-size--------------------------------------------------|

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderUI {
    pub _inv_canvas_size: Vector2<f32>,
    pub _instance_id_offset: u32,
    pub _reserved0: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum UIWidgetTypes {
    Default
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum HorizontalAlign {
    LEFT,
    CENTER,
    RIGHT,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum VerticalAlign {
    BOTTOM,
    CENTER,
    TOP,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Orientation {
    HORIZONTAL,
    VERTICAL,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum UILayoutType {
    FloatLayout,
    BoxLayout,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum UILayoutState {
    Unknown,
    Complete,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct UIVertexData {
    pub _position: Vector3<f32>,
}

impl Default for UIVertexData {
    fn default() -> UIVertexData {
        UIVertexData {
            _position: Vector3::zeros()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UIRenderGroupData {
    pub _accumulated_render_count: u32,
    pub _material_instance: *const MaterialInstanceData
}

#[derive(Debug, Clone, Copy)]
pub struct UIRenderData {
    pub _ui_texcoord: Vector4<f32>,
    pub _ui_render_area: Vector4<f32>,
    pub _ui_parent_render_area: Vector4<f32>,
    pub _ui_color: u32,
    pub _ui_round: f32,
    pub _ui_border: f32,
    pub _ui_border_color: u32,
    pub _ui_render_flags: u32,
    pub _reserved0: u32,
    pub _reserved1: u32,
    pub _reserved2: u32,
}

impl Default for UIRenderData {
    fn default() -> UIRenderData {
        UIRenderData {
            _ui_texcoord: Vector4::new(0.0, 0.0, 1.0, 1.0),
            _ui_render_area: Vector4::zeros(),
            _ui_parent_render_area: Vector4::zeros(),
            _ui_color: 0xFFFFFFFF,
            _ui_round: 0.0,
            _ui_border: 0.0,
            _ui_border_color: 0x00000000,
            _ui_render_flags: UI_RENDER_FLAG_NONE,
            _reserved0: 0,
            _reserved1: 0,
            _reserved2: 0,
        }
    }
}

pub struct UIComponentData {
    pub _layout_type: UILayoutType,
    pub _layout_orientation: Orientation,
    pub _pos: Vector2<f32>,
    pub _size: Vector2<f32>,
    pub _halign: HorizontalAlign,
    pub _valign: VerticalAlign,
    pub _pos_hint_x: Option<f32>,
    pub _pos_hint_y: Option<f32>,
    pub _size_hint_x: Option<f32>,
    pub _size_hint_y: Option<f32>,
    pub _padding: Vector4<f32>,
    pub _margine: Vector4<f32>,
    pub _texcoord: Vector4<f32>,
    pub _dragable: bool,
    pub _touchable: bool,
    pub _scroll_x: bool,
    pub _scroll_y: bool,
    pub _expandable_x: bool,
    pub _expandable_y: bool,
    pub _resizable_x: bool,
    pub _resizable_y: bool,
    pub _color: u32,
    pub _pressed_color: u32,
    pub _round: f32,
    pub _border: f32,
    pub _border_color: u32,
    pub _font_size: f32,
    pub _font_color: u32,
    pub _material_instance: Option<RcRefCell<MaterialInstanceData>>,
}

pub struct UIComponentInstance {
    pub _ui_component_data: UIComponentData,
    pub _owner_widget: Option<*mut dyn Widget>,
    pub _parent: Option<*mut UIComponentInstance>,
    pub _children: Vec<*mut UIComponentInstance>,
    pub _changed_layout: bool,
    pub _changed_render_data: bool,
    pub _changed_text: bool,
    pub _render_ui_index: u32,
    pub _transform: TransformObjectData,
    pub _world_to_local_matrix: Matrix4<f32>,
    pub _local_to_world_matrix: Matrix4<f32>,
    pub _spaces: Vector4<f32>, // margine + border + padding
    pub _contents_area: Vector4<f32>, // (x,y) ~ ((x,y) + contents_size)
    pub _contents_size: Vector2<f32>, // text column size, text row size
    pub _required_contents_area: Vector4<f32>,
    pub _required_contents_size: Vector2<f32>,
    pub _text_counts: Vec<usize>, // text column count, text row count
    pub _render_area: Vector4<f32>, // border + size
    pub _ui_area: Vector4<f32>, // (x,y) ~ ((x,y) + _ui_size)
    pub _ui_size: Vector2<f32>, // margie + border + padding + size
    pub _ui_layout_state: UILayoutState,
    pub _visible: bool,
    pub _touched: bool,
    pub _touched_offset: Vector2<f32>,
    pub _text: String,
    pub _render_text_count: u32,
    pub _callback_touch_down: Option<Box<fn()>>,
    pub _callback_touch_move: Option<Box<fn()>>,
    pub _callback_touch_up: Option<Box<fn()>>,
}

pub struct WidgetDefault {
    pub _ui_widget_type: UIWidgetTypes,
    pub _ui_component: UIComponentInstance,
    pub _parent: Option<*mut dyn Widget>,
    pub _widgets: Vec<*mut dyn Widget>,
}

pub struct UIManager {
    pub _root: Box<dyn Widget>,
    pub _ui_mesh_vertex_buffer: BufferData,
    pub _ui_mesh_index_buffer: BufferData,
    pub _ui_mesh_index_count: u32,
    pub _font_data: RcRefCell<FontData>,
    pub _ui_render_datas: [UIRenderData; MAX_UI_INSTANCE_COUNT as usize],
    pub _render_ui_count: u32,
    pub _render_ui_group: Vec<UIRenderGroupData>,
    pub _default_render_ui_material: Option<RcRefCell<MaterialInstanceData>>,
}

//////////////////////////////////////////
// interfaces
/////////////////////////////////////////

impl Default for UIComponentData {
    fn default() -> UIComponentData {
        unsafe {
            UIComponentData {
                _layout_type: UILayoutType::BoxLayout,
                _layout_orientation: Orientation::HORIZONTAL,
                _pos: Vector2::new(0.0, 0.0),
                _size: Vector2::new(100.0, 100.0),
                _halign: DEFAILT_HORIZONTAL_ALIGN,
                _valign: DEFAILT_VERTICAL_ALIGN,
                _pos_hint_x: None,
                _pos_hint_y: None,
                _size_hint_x: None,
                _size_hint_y: None,
                _padding: Vector4::zeros(),
                _margine: Vector4::zeros(),
                _round: 0.0,
                _border: 0.0,
                _border_color: get_color32(0, 0, 0, 255),
                _texcoord: Vector4::new(0.0, 0.0, 1.0, 1.0),
                _dragable: false,
                _touchable: false,
                _scroll_x: false,
                _scroll_y: false,
                _expandable_x: true,
                _expandable_y: true,
                _resizable_x: false,
                _resizable_y: false,
                _color: get_color32(255, 255, 255, 255),
                _pressed_color: get_color32(128, 128, 255, 255),
                _font_size: 20.0,
                _font_color: get_color32(0, 0, 0, 255),
                _material_instance: None,
            }
        }
    }
}

impl UIComponentInstance {
    pub fn create_ui_component() -> UIComponentInstance {
        UIComponentInstance {
            _ui_component_data: UIComponentData::default(),
            _owner_widget: None,
            _parent: None,
            _children: Vec::new(),
            _changed_layout: true,
            _changed_render_data: true,
            _changed_text: false,
            _render_ui_index: std::u32::MAX,
            _transform: TransformObjectData::new_transform_object_data(),
            _world_to_local_matrix: Matrix4::identity(),
            _local_to_world_matrix: Matrix4::identity(),
            _spaces: Vector4::zeros(),
            _contents_area: Vector4::zeros(),
            _contents_size: Vector2::zeros(),
            _required_contents_area: Vector4::zeros(),
            _required_contents_size: Vector2::zeros(),
            _render_area: Vector4::zeros(),
            _ui_area: Vector4::zeros(),
            _ui_size: Vector2::zeros(),
            _ui_layout_state: UILayoutState::Unknown,
            _visible: true,
            _touched: false,
            _touched_offset: Vector2::zeros(),
            _text: String::new(),
            _text_counts: Vec::new(),
            _render_text_count: 0,
            _callback_touch_down: None,
            _callback_touch_move: None,
            _callback_touch_up: None,
        }
    }

    pub fn check_collide(&self, x: f32, y: f32) -> bool {
        let collide_pos = &self._world_to_local_matrix * &Vector4::new(x, y, 0.0, 1.0);
        let pos = self._transform.get_position();
        let half_size = &self._ui_component_data._size * 0.5;
        (pos.x - half_size.x) < collide_pos.x && collide_pos.x < (pos.x + half_size.x) && (pos.y - half_size.y) < collide_pos.y && collide_pos.y < (pos.y + half_size.y)
    }

    pub fn on_touch_down(&mut self, x: f32, y: f32) {
        self._touched = true;
        if self._ui_component_data._dragable {
            let pos = self._transform.get_position();
            let touched_pos = &self._world_to_local_matrix * &Vector4::new(x, y, 0.0, 1.0);
            self._touched_offset.x = touched_pos.x - pos.x;
            self._touched_offset.y = touched_pos.x - pos.y;
            if let Some(callback_touch_down) = self._callback_touch_down.as_ref() {
                callback_touch_down();
            }
        }
    }

    pub fn on_touch_move(&mut self, _x: f32, _y: f32) {
        if self._touched {
        //     if self._dragable:
        //         self._x = x + self._touch_offset_x
        //         self._y = y + self._touch_offset_y
        //
        //         if self._callback_touch_move is not None:
        //             self._callback_touch_move(self, x, y)
        }
    }

    pub fn on_touch_up(&mut self, _x: f32, _y: f32) {
        if self._touched {
            self._touched = false;
            // if self._dragable:
            //     self._x = x + self._touch_offset_x
            //     self._y = y + self._touch_offset_y
            //
            //     if self._callback_touch_up is not None:
            //         self._callback_touch_up(self, x, y)
        }
    }
    pub fn get_pivot(&self) -> &Vector3<f32> {
        self._transform.get_position()
    }
    pub fn get_spaces(&self) -> &Vector4<f32> {
        &self._spaces
    }
    pub fn get_layout_type(&self) -> UILayoutType {
        self._ui_component_data._layout_type
    }
    pub fn set_layout_type(&mut self, layout_type: UILayoutType) { self._ui_component_data._layout_type = layout_type; }
    pub fn get_layout_orientation(&self) -> Orientation { self._ui_component_data._layout_orientation }
    pub fn set_layout_orientation(&mut self, layout_orientation: Orientation) { self._ui_component_data._layout_orientation = layout_orientation; }
    pub fn get_pos_x(&self) -> f32 { self._ui_component_data._pos.x }
    pub fn get_pos_y(&self) -> f32 { self._ui_component_data._pos.y }
    pub fn get_pos(&self) -> &Vector2<f32> { &self._ui_component_data._pos }
    pub fn set_pos(&mut self, x: f32, y: f32) {
        self.set_pos_x(x);
        self.set_pos_y(y);
    }
    pub fn set_pos_x(&mut self, x: f32) {
        if x != self._ui_component_data._pos.x || self._ui_component_data._pos_hint_x.is_some() {
            self._ui_component_data._pos_hint_x = None;
            self._ui_component_data._pos.x = x;
            self._changed_layout = true;
        }
    }
    pub fn set_pos_y(&mut self, y: f32) {
        if y != self._ui_component_data._pos.y || self._ui_component_data._pos_hint_y.is_some() {
            self._ui_component_data._pos_hint_y = None;
            self._ui_component_data._pos.y = y;
            self._changed_layout = true;
        }
    }
    pub fn get_pos_hint_x(&self) -> Option<f32> {
        self._ui_component_data._pos_hint_x
    }
    pub fn get_pos_hint_y(&self) -> Option<f32> {
        self._ui_component_data._pos_hint_y
    }
    pub fn set_pos_hint_x(&mut self, pos_hint_x: Option<f32>) {
        if pos_hint_x != self._ui_component_data._pos_hint_x {
            self._ui_component_data._pos_hint_x = pos_hint_x;
            self._changed_layout = true;
        }
    }
    pub fn set_pos_hint_y(&mut self, pos_hint_y: Option<f32>) {
        if pos_hint_y != self._ui_component_data._pos_hint_y {
            self._ui_component_data._pos_hint_y = pos_hint_y;
            self._changed_layout = true;
        }
    }
    pub fn get_size(&self) -> &Vector2<f32> {
        &self._ui_component_data._size
    }
    pub fn get_size_x(&self) -> f32 { self._ui_component_data._size.x }
    pub fn get_size_y(&self) -> f32 { self._ui_component_data._size.y }
    pub fn set_size(&mut self, size_x: f32, size_y: f32) {
        self.set_size_x(size_x);
        self.set_size_y(size_y);
    }
    pub fn set_size_x(&mut self, size_x: f32) {
        if size_x != self._ui_component_data._size.x || self._ui_component_data._size_hint_x.is_some() {
            self._ui_component_data._size_hint_x = None;
            self._ui_component_data._size.x = size_x;
            self._changed_layout = true;
        }
    }
    pub fn set_size_y(&mut self, size_y: f32) {
        if size_y != self._ui_component_data._size.y || self._ui_component_data._size_hint_y.is_some() {
            self._ui_component_data._size_hint_y = None;
            self._ui_component_data._size.y = size_y;
            self._changed_layout = true;
        }
    }
    pub fn get_size_hint_x(&self) -> Option<f32> { self._ui_component_data._size_hint_x }
    pub fn get_size_hint_y(&self) -> Option<f32> { self._ui_component_data._size_hint_y }
    pub fn set_size_hint_x(&mut self, size_hint_x: Option<f32>) {
        if size_hint_x != self._ui_component_data._size_hint_x {
            self._ui_component_data._size_hint_x = size_hint_x;
            self._changed_layout = true;
        }
    }
    pub fn set_size_hint_y(&mut self, size_hint_y: Option<f32>) {
        if size_hint_y != self._ui_component_data._size_hint_y {
            self._ui_component_data._size_hint_y = size_hint_y;
            self._changed_layout = true;
        }
    }
    pub fn set_margine(&mut self, margine: f32) { self.set_margines(Vector4::new(margine, margine, margine, margine)); }
    pub fn set_margines(&mut self, margine: Vector4<f32>) {
        if margine != self._ui_component_data._margine {
            self._ui_component_data._margine = margine;
            self._changed_layout = true;
        }
    }
    fn set_margine_inner(&mut self, index: usize, margine: f32) {
        if margine != self._ui_component_data._margine[index] {
            self._ui_component_data._margine[index] = margine;
            self._changed_layout = true;
        }
    }
    pub fn get_margine(&self) -> &Vector4<f32> { &self._ui_component_data._margine }
    pub fn get_margine_left(&self) -> f32 { self._ui_component_data._margine.x }
    pub fn get_margine_top(&self) -> f32 { self._ui_component_data._margine.y }
    pub fn get_margine_right(&self) -> f32 { self._ui_component_data._margine.z }
    pub fn get_margine_bottom(&self) -> f32 { self._ui_component_data._margine.w }
    pub fn set_margine_left(&mut self, margine: f32) { self.set_margine_inner(UI_INDEX_LEFT, margine); }
    pub fn set_margine_top(&mut self, margine: f32) { self.set_margine_inner(UI_INDEX_TOP, margine); }
    pub fn set_margine_right(&mut self, margine: f32) { self.set_margine_inner(UI_INDEX_RIGHT, margine); }
    pub fn set_margine_bottom(&mut self, margine: f32) { self.set_margine_inner(UI_INDEX_BOTTOM, margine); }

    pub fn set_padding(&mut self, padding: f32) { self.set_paddings(Vector4::new(padding, padding, padding, padding)); }
    pub fn set_paddings(&mut self, padding: Vector4<f32>) {
        if padding != self._ui_component_data._padding {
            self._ui_component_data._padding = padding;
            self._changed_layout = true;
        }
    }
    fn set_padding_inner(&mut self, index: usize, padding: f32) {
        if padding != self._ui_component_data._padding[index] {
            self._ui_component_data._padding[index] = padding;
            self._changed_layout = true;
        }
    }
    pub fn get_padding(&self) -> &Vector4<f32> { &self._ui_component_data._padding }
    pub fn get_padding_left(&self) -> f32 { self._ui_component_data._padding.x }
    pub fn get_padding_top(&self) -> f32 { self._ui_component_data._padding.y }
    pub fn get_padding_right(&self) -> f32 { self._ui_component_data._padding.z }
    pub fn get_padding_bottom(&self) -> f32 { self._ui_component_data._padding.w }
    pub fn set_padding_left(&mut self, padding: f32) { self.set_padding_inner(UI_INDEX_LEFT, padding); }
    pub fn set_padding_top(&mut self, padding: f32) { self.set_padding_inner(UI_INDEX_TOP, padding); }
    pub fn set_padding_right(&mut self, padding: f32) { self.set_padding_inner(UI_INDEX_RIGHT, padding); }
    pub fn set_padding_bottom(&mut self, padding: f32) { self.set_padding_inner(UI_INDEX_BOTTOM, padding); }
    pub fn get_halign(&self) -> HorizontalAlign { self._ui_component_data._halign }
    pub fn set_halign(&mut self, halign: HorizontalAlign) {
        if halign != self._ui_component_data._halign {
            self._ui_component_data._halign = halign;
            self._changed_layout = true;
        }
    }
    pub fn get_valign(&self) -> VerticalAlign { self._ui_component_data._valign }
    pub fn set_valign(&mut self, valign: VerticalAlign) {
        if valign != self._ui_component_data._valign {
            self._ui_component_data._valign = valign;
            self._changed_layout = true;
        }
    }
    pub fn get_changed_layout(&self) -> bool { self._changed_layout }
    pub fn get_owner_widget(&self) -> &Option<*mut dyn Widget> { &self._owner_widget }
    pub fn get_parent(&self) -> &Option<*mut UIComponentInstance> { &self._parent }
    pub fn set_parent(&mut self, parent: Option<*mut UIComponentInstance>) {
        if self._parent.is_some() {
            panic!("Widget already has parent");
        }
        self._parent = parent;
    }
    pub fn clear_children(&mut self) {
        for child in self._children.iter() {
            unsafe {
                child.as_mut().unwrap().clear_children();
            }
            // if self._viewport_manager.focused_widget is widget:
            //     self._viewport_manager.focused_widget = None
        }
        self._children.clear();
        self._changed_layout = true;
    }
    pub fn get_visible(&self) -> bool { self._visible }
    pub fn set_visible(&mut self, visible: bool) {
        self._visible = visible;
        self._changed_render_data = true;
    }
    pub fn get_color(&self) -> u32 { self._ui_component_data._color }
    pub fn set_color(&mut self, color: u32) {
        if color != self._ui_component_data._color {
            self._ui_component_data._color = color;
            self._changed_render_data = true;
        }
    }
    pub fn get_pressed_color(&self) -> u32 { self._ui_component_data._pressed_color }
    pub fn set_pressed_color(&mut self, color: u32) {
        if color != self._ui_component_data._pressed_color {
            self._ui_component_data._pressed_color = color;
            self._changed_render_data = true;
        }
    }
    pub fn get_border_color(&self) -> u32 { self._ui_component_data._border_color }
    pub fn set_border_color(&mut self, color: u32) {
        if color != self._ui_component_data._border_color {
            self._ui_component_data._border_color = color;
            self._changed_render_data = true;
        }
    }
    pub fn get_font_size(&self) -> f32 { self._ui_component_data._font_size }
    pub fn set_font_size(&mut self, font_size: f32) {
        if font_size != self._ui_component_data._font_size {
            self._ui_component_data._font_size = font_size;
            self._changed_layout = true;
        }
    }
    pub fn get_font_color(&self) -> u32 { self._ui_component_data._font_color }
    pub fn set_font_color(&mut self, color: u32) {
        if color != self._ui_component_data._font_color {
            self._ui_component_data._font_color = color;
            self._changed_render_data = true;
        }
    }
    pub fn get_texcoord(&self) -> &Vector4<f32> { &self._ui_component_data._texcoord }
    pub fn get_material_instance(&self) -> &Option<RcRefCell<MaterialInstanceData>> { &self._ui_component_data._material_instance }
    pub fn set_material_instance(&mut self, material_instance: &RcRefCell<MaterialInstanceData>) {
        self._ui_component_data._material_instance = Some(material_instance.clone());
    }
    pub fn get_round(&self) -> f32 { self._ui_component_data._round }
    pub fn set_round(&mut self, round: f32) {
        if round != self._ui_component_data._round {
            self._ui_component_data._round = round;
            self._changed_render_data = true;
        }
    }
    pub fn get_border(&self) -> f32 { self._ui_component_data._border }
    pub fn set_border(&mut self, border: f32) {
        if border != self._ui_component_data._border {
            self._ui_component_data._border = border;
            self._changed_render_data = true;
        }
    }
    pub fn get_dragable(&self) -> bool { self._ui_component_data._dragable }
    pub fn get_touchable(&self) -> bool { self._ui_component_data._touchable }
    pub fn get_scroll_x(&self) -> bool { self._ui_component_data._scroll_x }
    pub fn get_scroll_y(&self) -> bool { self._ui_component_data._scroll_y }
    pub fn get_expandable_x(&self) -> bool { self._ui_component_data._expandable_x }
    pub fn get_expandable_y(&self) -> bool { self._ui_component_data._expandable_y }
    pub fn get_resizable_x(&self) -> bool { self._ui_component_data._resizable_x }
    pub fn get_resizable_y(&self) -> bool { self._ui_component_data._resizable_y }
    pub fn add_ui_component(&mut self, child: *mut UIComponentInstance) {
        unsafe {
            if child.as_ref().unwrap().get_parent().is_some() {
                panic!("Widget already has parent");
            }

            if false == self._children.contains(&child) {
                self._children.push(child);
                child.as_mut().unwrap().set_parent(Some(self));
                self._changed_layout = true;
            }
        }
    }

    pub fn remove_ui_component(&mut self, child: *mut UIComponentInstance) {
        unsafe {
            if let Some(index) = self._children.iter().position(|x| *x == child) {
                // if self._viewport_manager.focused_widget is widget {
                //     self._viewport_manager.focused_widget = None
                // }
                self._children.remove(index);
                child.as_mut().unwrap().set_parent(None);
                self._changed_layout = true;
            }
        }
    }

    pub fn set_text(&mut self, text: String) {
        if text != self._text {
            self._text = text;
            self._changed_text = true;
            self._changed_layout = true;
        }
    }

    pub fn compute_text_contents_size(&mut self, font_data: &FontData, contents_size: &mut Vector2<f32>) {
        self._text_counts.clear();

        if false == self._text.is_empty() {
            let font_size_ratio = self.get_font_size() / font_data._font_size.y;
            let font_size = &font_data._font_size * font_size_ratio;
            let mut column_count: usize = 0;
            let mut row_count: usize = 0;
            let mut max_column_count: usize = 0;
            for c in self._text.as_bytes().iter() {
                let ch = (*c) as char;
                if '\n' == ch {
                    self._text_counts.push(column_count);
                    max_column_count = max_column_count.max(column_count);
                    column_count = 0;
                    row_count += 1;
                } else if '\t' == ch {
                    column_count += 4;
                } else if ' ' == ch {
                    column_count += 1;
                } else {
                    column_count += 1;
                }
            }

            self._text_counts.push(column_count);
            max_column_count = max_column_count.max(column_count);
            row_count += 1;
            contents_size.x = max_column_count as f32 * font_size.x;
            contents_size.y = row_count as f32 * font_size.y;
        }
    }

    pub fn collect_ui_font_render_data(
        &mut self,
        font_data: &FontData,
        render_ui_count: u32,
        _render_ui_group: &mut Vec<UIRenderGroupData>,
        _prev_render_group_data: &mut UIRenderGroupData,
        render_ui_instance_datas: &mut [UIRenderData],
    ) {
        let mut render_ui_index = render_ui_count;

        let mut ui_render_area: Vector4<f32> = Vector4::zeros();
        let mut render_pos: Vector2<f32> = Vector2::zeros();

        let count_of_side: u32 = font_data._count_of_side;
        let inv_count_of_side: f32 = 1.0 / font_data._count_of_side as f32;
        let font_size_ratio: f32 = self.get_font_size() / font_data._font_size.y;
        let font_size: Vector2<f32> = &font_data._font_size * font_size_ratio;
        let mut column: i32 = 0;
        let mut row: i32 = 0;
        self._render_text_count = 0;

        for c in self._text.as_bytes().iter() {
            let ch = (*c) as char;
            if '\n' == ch {
                column = 0;
                row += 1;
            } else if '\t' == ch {
                column += 4;
            } else if ' ' == ch {
                column += 1;
            } else {
                let index: u32 = 0i32.max((*c) as i32 - font_data._range_min as i32) as u32;
                let texcoord_x = (index % count_of_side) as f32 * inv_count_of_side;
                let texcoord_y = (index / count_of_side) as f32 * inv_count_of_side;

                render_pos.x = self._contents_area.x;
                render_pos.y = self._contents_area.y;
                render_pos.x += column as f32 * font_size.x;
                render_pos.y += row as f32 * font_size.y;

                ui_render_area.x = render_pos.x;
                ui_render_area.y = render_pos.y;
                ui_render_area.z = render_pos.x + font_size.x;
                ui_render_area.w = render_pos.y + font_size.y;

                if self._contents_area.x < ui_render_area.z && self._contents_area.y < ui_render_area.w &&
                    ui_render_area.x < self._contents_area.z && ui_render_area.y < self._contents_area.w {
                    let render_ui_instance_data = &mut render_ui_instance_datas[render_ui_index as usize];
                    render_ui_instance_data._ui_texcoord.x = texcoord_x;
                    render_ui_instance_data._ui_texcoord.y = texcoord_y;
                    render_ui_instance_data._ui_texcoord.z = texcoord_x + inv_count_of_side;
                    render_ui_instance_data._ui_texcoord.w = texcoord_y + inv_count_of_side;
                    render_ui_instance_data._ui_render_area = ui_render_area.clone() as Vector4<f32>;
                    render_ui_instance_data._ui_parent_render_area = self._contents_area.clone() as Vector4<f32>;
                    render_ui_instance_data._ui_color = self.get_font_color();
                    render_ui_instance_data._ui_round = 0.0;
                    render_ui_instance_data._ui_border = 0.0;
                    render_ui_instance_data._ui_border_color = 0;
                    render_ui_instance_data._ui_render_flags = UI_RENDER_FLAG_RENDER_TEXT;
                    render_ui_index += 1;
                    self._render_text_count += 1;
                }
                column += 1;
            }
        }
    }

    pub fn collect_ui_render_data(
        &mut self,
        font_data: &FontData,
        render_ui_count: &mut u32,
        render_ui_group: &mut Vec<UIRenderGroupData>,
        prev_render_group_data: &mut UIRenderGroupData,
        render_ui_instance_datas: &mut [UIRenderData]
    ) {
        if self._visible {
            let render_ui_index = *render_ui_count;
            *render_ui_count += 1;

            let need_to_collect_render_data = self._changed_render_data || render_ui_index != self._render_ui_index;

            // collect ui render data
            if need_to_collect_render_data {
                let render_ui_instance_data = &mut render_ui_instance_datas[render_ui_index as usize];
                render_ui_instance_data._ui_render_area.clone_from(&self._render_area);
                if self._parent.is_some() {
                    unsafe {
                        render_ui_instance_data._ui_parent_render_area = (*self._parent.unwrap())._contents_area.clone() as Vector4<f32>;
                    }
                } else {
                    render_ui_instance_data._ui_parent_render_area = render_ui_instance_data._ui_render_area.clone() as Vector4<f32>;
                }

                render_ui_instance_data._ui_color = self.get_color();
                render_ui_instance_data._ui_round = self.get_round();
                render_ui_instance_data._ui_border = self.get_border();
                render_ui_instance_data._ui_border_color = self.get_border_color();
                render_ui_instance_data._ui_render_flags = UI_RENDER_FLAG_NONE;
                render_ui_instance_data._ui_texcoord.clone_from(&self._ui_component_data._texcoord);

                self._render_ui_index = render_ui_index;
                self._changed_render_data = false;
            }

            // add_ui_render_group_data
            if 0 < render_ui_index {
                let material_instance = match self.get_material_instance() {
                    Some(material_instance) => material_instance.as_ptr() as *const MaterialInstanceData,
                    None => std::ptr::null()
                };

                if prev_render_group_data._material_instance != material_instance {
                    UIRenderGroupData::add_ui_render_group_data(render_ui_group, render_ui_index, prev_render_group_data, material_instance);
                }
            }

            // collect font render data
            if need_to_collect_render_data || self._changed_text {
                if self._text.is_empty() {
                    self._render_text_count = 0;
                } else {
                    self.collect_ui_font_render_data(
                        font_data,
                        *render_ui_count,
                        render_ui_group,
                        prev_render_group_data,
                        render_ui_instance_datas,
                    );
                }
                self._changed_text = false;
            }
            *render_ui_count += self._render_text_count;
        }

        unsafe {
            for ui_component in self._children.iter() {
                ui_component.as_mut().unwrap().collect_ui_render_data(
                    font_data,
                    render_ui_count,
                    render_ui_group,
                    prev_render_group_data,
                    render_ui_instance_datas,
                );
            }
        }
    }

    fn update_layout_size(&mut self, parent_contents_size: &Vector2<f32>) {
        let border = self.get_border();
        let spaces = self.get_margine() + &Vector4::new(border, border, border, border) + self.get_padding();
        let size_hint_x = self.get_size_hint_x();
        let size_hint_y = self.get_size_hint_y();
        let mut ui_size: Vector2<f32> = self.get_size().clone() as Vector2<f32>;
        if size_hint_x.is_some() {
            ui_size.x = parent_contents_size.x * size_hint_x.unwrap();
        }
        if size_hint_y.is_some() {
            ui_size.y = parent_contents_size.y * size_hint_y.unwrap();
        }
        self._spaces.clone_from(&spaces);
        self._ui_size.clone_from(&ui_size);
        self._contents_size.x = 0f32.max(ui_size.x - spaces.x - spaces.x);
        self._contents_size.y = 0f32.max(ui_size.y - spaces.y - spaces.w);
    }

    fn update_layout_area(
        &mut self,
        parent_layout_type: UILayoutType,
        parent_layout_orientation: Orientation,
        parent_halign: HorizontalAlign,
        parent_valign: VerticalAlign,
        parent_contents_area: &Vector4<f32>,
        parent_contents_size: &Vector2<f32>,
        required_contents_size: &Vector2<f32>,
        ui_area_pos: &mut Vector2<f32>
    ) {
        // update ui_area.xy
        match parent_layout_type {
            UILayoutType::FloatLayout => {
                let pos_hint_x = self.get_pos_hint_x();
                let pos_hint_y = self.get_pos_hint_y();
                if pos_hint_x.is_some() {
                    self._ui_area.x = parent_contents_area.x + parent_contents_size.x * pos_hint_x.unwrap();
                } else {
                    self._ui_area.x = parent_contents_area.x + self.get_pos_x();
                }
                if pos_hint_y.is_some() {
                    self._ui_area.y = parent_contents_area.y + parent_contents_size.y * pos_hint_y.unwrap();
                } else {
                    self._ui_area.y = parent_contents_area.y + self.get_pos_y();
                }
            },
            UILayoutType::BoxLayout => {
                match parent_layout_orientation {
                    Orientation::HORIZONTAL => {
                        self._ui_area.x = ui_area_pos.x;
                        match parent_valign {
                            VerticalAlign::TOP => self._ui_area.y = ui_area_pos.y,
                            VerticalAlign::CENTER => {
                                self._ui_area.y = ui_area_pos.y + (required_contents_size.y - self._ui_size.y) * 0.5;
                            },
                            VerticalAlign::BOTTOM => self._ui_area.y = ui_area_pos.y + (required_contents_size.y - self._ui_size.y),
                        }
                        ui_area_pos.x += self._ui_size.x;
                    },
                    Orientation::VERTICAL => {
                        self._ui_area.y = ui_area_pos.y;
                        match parent_halign {
                            HorizontalAlign::LEFT => self._ui_area.x = ui_area_pos.x,
                            HorizontalAlign::CENTER => self._ui_area.x = ui_area_pos.x + (required_contents_size.x - self._ui_size.x) * 0.5,
                            HorizontalAlign::RIGHT => self._ui_area.x = ui_area_pos.x + (required_contents_size.x - self._ui_size.x),
                        }
                        ui_area_pos.y += self._ui_size.y;
                    },
                }
            }
        }

        // update ui area
        self._ui_area.z = self._ui_area.x + self._ui_size.x;
        self._ui_area.w = self._ui_area.y + self._ui_size.y;

        self._render_area.x = self._ui_area.x + self.get_margine_left();
        self._render_area.y = self._ui_area.y + self.get_margine_top();
        self._render_area.z = self._ui_area.z - self.get_margine_right();
        self._render_area.w = self._ui_area.w - self.get_margine_bottom();

        self._contents_area.x = self._ui_area.x + self._spaces.x;
        self._contents_area.y = self._ui_area.y + self._spaces.y;
        self._contents_area.z = self._ui_area.z - self._spaces.z;
        self._contents_area.w = self._ui_area.w - self._spaces.w;

        let pivot = Vector3::new(
            (self._render_area.x + self._render_area.z) * 0.5,
            (self._render_area.y + self._render_area.w) * 0.5,
            0.0
        );
        self._transform.set_position(&pivot);
    }

    fn update_layout(&mut self, font_data: &FontData) {
        unsafe {
            let mut changed_layout: bool = self._changed_layout;
            let mut required_contents_area = self._contents_area.clone() as Vector4<f32>;
            let mut required_contents_size = Vector2::<f32>::zeros();

            let layout_type = self.get_layout_type();
            let layout_orientation = self.get_layout_orientation();
            let halign = self.get_halign();
            let valign = self.get_valign();
            let contents_area = &self._contents_area;
            let contents_size = &self._contents_size;

            // preupdate layout size
            for child in self._children.iter() {
                let child_ui_instance = child.as_mut().unwrap();
                changed_layout |= child_ui_instance._changed_layout;
                if changed_layout {
                    // update_layout_size
                    child_ui_instance.update_layout_size(&self._contents_size);

                    // accumulate required_contents_size
                    if UILayoutType::BoxLayout == layout_type {
                        match layout_orientation {
                            Orientation::HORIZONTAL => {
                                required_contents_size.x += child_ui_instance._ui_size.x;
                                required_contents_size.y = required_contents_size.y.max(child_ui_instance._ui_size.y);
                            },
                            Orientation::VERTICAL => {
                                required_contents_size.x = required_contents_size.x.max(child_ui_instance._ui_size.x);
                                required_contents_size.y += child_ui_instance._ui_size.y;
                            },
                        }
                    }
                }
            }

            if changed_layout {
                // calculate child_ui_pos
                let mut child_ui_pos = Vector2::<f32>::new(contents_area.x, contents_area.y); // (left, top)
                if UILayoutType::BoxLayout == layout_type {
                    match halign {
                        HorizontalAlign::LEFT => {},
                        HorizontalAlign::CENTER => child_ui_pos.x += (contents_size.x - required_contents_size.x) * 0.5,
                        HorizontalAlign::RIGHT => child_ui_pos.x += contents_size.x - required_contents_size.x,
                    }
                    match valign {
                        VerticalAlign::TOP => {},
                        VerticalAlign::CENTER => child_ui_pos.y += (contents_size.y - required_contents_size.y) * 0.5,
                        VerticalAlign::BOTTOM => child_ui_pos.y += contents_size.y - required_contents_size.y,
                    }
                }

                // update children layout area
                for child in self._children.iter() {
                    let child_ui_instance = child.as_mut().unwrap();
                    child_ui_instance.update_layout_area(layout_type, layout_orientation, halign, valign, contents_area, contents_size, &required_contents_size, &mut child_ui_pos);
                    child_ui_instance.update_layout(font_data);
                    required_contents_area.x = required_contents_area.x.min(child_ui_instance._ui_area.x);
                    required_contents_area.y = required_contents_area.y.min(child_ui_instance._ui_area.y);
                    required_contents_area.z = required_contents_area.z.min(child_ui_instance._ui_area.z);
                    required_contents_area.w = required_contents_area.w.min(child_ui_instance._ui_area.w);
                }

                // update contents area
                let mut text_contents_size = Vector2::<f32>::zeros();
                if self._changed_text {
                    self.compute_text_contents_size(font_data, &mut text_contents_size);
                }

                required_contents_size.x = 0f32.max(required_contents_area.z - required_contents_area.x);
                required_contents_size.y = 0f32.max(required_contents_area.w - required_contents_area.y);
                self._required_contents_area = required_contents_area;
                self._required_contents_size = required_contents_size;

                // complete
                self._changed_render_data = true;
                self._changed_layout = false;
                self._ui_layout_state = UILayoutState::Complete;
            }
        }
    }

    fn update(&mut self, _delta_time: f64, _touch_event: bool) {
        //     for widget in self._widgets:
        //         touch_event = widget.update(dt, touch_event)
        //
        //     if not touch_event and self._touchable:
        //         down_left, down_middle, down_right = self._core_manager.get_mouse_down()
        //         pressed_left, pressed_middle, pressed_right = self._core_manager.get_mouse_pressed()
        //         mouse_x, mouse_y = self._core_manager.get_mouse_pos()
        //
        //         if self._touched:
        //             if pressed_left:
        //                 self._on_touch_move(mouse_x, mouse_y)
        //             else:
        //                 self._on_touch_up(mouse_x, mouse_y)
        //                 if not self._has_cursor:
        //                     self._viewport_manager.focused_widget = None
        //
        //         elif down_left:
        //             if self._collide(mouse_x, mouse_y):
        //                 self._viewport_manager.focused_widget = self
        //                 self._on_touch_down(mouse_x, mouse_y)
        //             elif self._has_cursor:
        //                 self._viewport_manager.focused_widget = None
        //
        //     return self._touched or touch_event
    }
    fn render(&self) {
        //     if 0.0 <= self._opacity and self._visible:
        //         render_widget_program.use_program()
        //         render_widget_program.bind_material_instance()
        //
        //         if self._pressed:
        //             render_widget_program.bind_uniform_data("color", self._pressed_color)
        //         else:
        //             render_widget_program.bind_uniform_data("color", self._color)
        //
        //         render_widget_program.bind_uniform_data("pos_size", [self._world_x, self._world_y, self._width, self._height])
        //         render_widget_program.bind_uniform_data("texcoord", self._texcoord)
        //         render_widget_program.bind_uniform_data("opacity", self._opacity)
        //
        //         if self._texture is not None:
        //             render_widget_program.bind_uniform_data("texture_diffuse", self._texture)
        //             render_widget_program.bind_uniform_data("is_render_diffuse", True)
        //         else:
        //             render_widget_program.bind_uniform_data("is_render_diffuse", False)
        //
        //         mesh.draw_elements()
        //
        //     if self._visible:
        //         if isinstance(self, Label) and self._text_render_data is not None:
        //             self._core_manager.renderer.render_text(self._text_render_data,
        //                                                    self._world_x,
        //                                                    self._world_y,
        //                                                    self._root.width,
        //                                                    self._root.height)
        //         for widget in self._widgets:
        //             widget.render(last_program, render_widget_program, mesh)
    }
}

pub trait Widget {
    fn get_ui_widget_type(&self) -> UIWidgetTypes;
    fn has_cursor(&self) -> bool;
    fn get_ui_component(&self) -> &UIComponentInstance;
    fn get_ui_component_mut(&mut self) -> *mut UIComponentInstance;
    fn get_changed_layout(&self) -> bool;
    fn clear_parent(&mut self);
    fn set_parent(&mut self, widget: *mut dyn Widget);
    fn add_widget(&mut self, widget: *mut dyn Widget);
    fn remove_widget(&mut self, widget: *mut dyn Widget);
    fn clear_widgets(&mut self);
}

impl WidgetDefault {
    fn create_widget() -> Box<dyn Widget> {
        let mut widget = Box::new(WidgetDefault {
            _ui_widget_type: UIWidgetTypes::Default,
            _ui_component: UIComponentInstance::create_ui_component(),
            _parent: None,
            _widgets: Vec::new(),
        });
        widget._ui_component._owner_widget = Some(&mut (*widget));
        widget
    }
}

impl Widget for WidgetDefault {
    fn get_ui_widget_type(&self) -> UIWidgetTypes {
        self._ui_widget_type
    }
    fn has_cursor(&self) -> bool { false }
    fn get_ui_component(&self) -> &UIComponentInstance { &self._ui_component }
    fn get_ui_component_mut(&mut self) -> *mut UIComponentInstance { &mut self._ui_component }
    fn get_changed_layout(&self) -> bool { self._ui_component._changed_layout }
    fn clear_parent(&mut self) {
        self._parent = None;
        self._ui_component._parent = None;
    }
    fn set_parent(&mut self, widget: *mut dyn Widget) {
        unsafe {
            if self._parent.is_some() {
                (*self._parent.unwrap()).remove_widget(self);
            }
            self._parent = Some(widget);
            self._ui_component._parent = Some(widget.as_mut().unwrap().get_ui_component_mut());
        }
    }
    fn add_widget(&mut self, widget: *mut dyn Widget) {
        unsafe {
            widget.as_mut().unwrap().set_parent(self);
            self._widgets.push(widget);
            self._ui_component._children.push(widget.as_mut().unwrap().get_ui_component_mut());
        }
    }
    fn remove_widget(&mut self, widget: *mut dyn Widget) {
        for (i, child_widget) in self._widgets.iter().enumerate() {
            if *child_widget == widget {
                unsafe {
                    (*widget).clear_parent();
                    (*widget).clear_widgets();
                    drop(Box::from_raw(widget));
                }
                self._widgets.remove(i);
                self._ui_component._children.remove(i);
                return;
            }
        }
    }
    fn clear_widgets(&mut self) {
        unsafe {
            for child_widget in self._widgets.iter_mut() {
                (**child_widget).clear_widgets();
                drop(Box::from_raw(*child_widget));
            }
        }
        self._parent = None;
        self._widgets.clear();
        self._ui_component._parent = None;
        self._ui_component._children.clear();
    }
}

impl UIVertexData {
    const POSITION: vk::Format = vk::Format::R32G32B32_SFLOAT;
}

impl VertexData for UIVertexData {
    fn create_vertex_input_attribute_descriptions() -> Vec<vk::VertexInputAttributeDescription> {
        let mut vertex_input_attribute_descriptions = Vec::<vk::VertexInputAttributeDescription>::new();
        let binding = 0u32;
        geometry_buffer::add_vertex_input_attribute_description(&mut vertex_input_attribute_descriptions, binding, UIVertexData::POSITION);
        vertex_input_attribute_descriptions
    }

    fn get_vertex_input_binding_descriptions() -> Vec<vk::VertexInputBindingDescription> {
        vec![
            vk::VertexInputBindingDescription {
                binding: 0,
                stride: std::mem::size_of::<UIVertexData>() as u32,
                input_rate: vk::VertexInputRate::VERTEX
            },
        ]
    }
}

impl UIRenderGroupData {
    pub fn add_ui_render_group_data(
        render_ui_group: &mut Vec<UIRenderGroupData>,
        render_ui_count: u32,
        prev_render_group_data: &mut UIRenderGroupData,
        current_material_instance: *const MaterialInstanceData,
    ) {
        prev_render_group_data._accumulated_render_count = render_ui_count;
        render_ui_group.push(*prev_render_group_data);
        prev_render_group_data._material_instance = current_material_instance;
    }
}


impl UIManager {
    pub fn create_ui_manager() -> UIManager {
        log::info!("create_ui_manager");
        unsafe {
            let ui_manager = UIManager {
                _root: Box::from_raw(UIManager::create_widget(UIWidgetTypes::Default)),
                _ui_mesh_vertex_buffer: BufferData::default(),
                _ui_mesh_index_buffer: BufferData::default(),
                _ui_mesh_index_count: 0,
                _font_data: system::newRcRefCell(FontData::default()),
                _ui_render_datas: [UIRenderData::default(); MAX_UI_INSTANCE_COUNT as usize],
                _render_ui_count: 0,
                _render_ui_group: Vec::new(),
                _default_render_ui_material: None,
            };
            ui_manager
        }
    }

    pub fn initialize_ui_manager(&mut self, renderer_data: &RendererData, resources: &Resources) {
        let font_data = resources.get_default_font_data();
        self._font_data = font_data.clone();
        self.create_ui_vertex_data(renderer_data.get_device(), renderer_data.get_command_pool(), renderer_data.get_graphics_queue(), renderer_data.get_device_memory_properties());
        self._default_render_ui_material = Some(resources.get_material_instance_data("render_ui").clone());
    }

    pub fn create_ui_descriptor_sets(&mut self, _renderer_data: &RendererData, _resources: &Resources) {
    }

    pub fn destroy_ui_descriptor_sets(&mut self) {
    }

    pub fn destroy_ui_manager(&mut self, device: &Device) {
        log::info!("destroy_ui_manager");
        self._root.clear_widgets();
        drop(&self._root);
        buffer::destroy_buffer_data(device, &self._ui_mesh_vertex_buffer);
        buffer::destroy_buffer_data(device, &self._ui_mesh_index_buffer);
    }

    pub fn create_ui_vertex_data(
        &mut self,
        device: &Device,
        command_pool: vk::CommandPool,
        command_queue: vk::Queue,
        device_memory_properties: &vk::PhysicalDeviceMemoryProperties
    ) {
        log::debug!("create_ui_vertex_data");
        let positions: Vec<Vector3<f32>> = vec![Vector3::new(0.0, 0.0, 0.0), Vector3::new(1.0, 0.0, 0.0), Vector3::new(1.0, 1.0, 0.0), Vector3::new(0.0, 1.0, 0.0)];
        let vertex_datas = positions.iter().map(|position| UIVertexData { _position: (*position).clone() as Vector3<f32> }).collect();
        let indices: Vec<u32> = vec![0, 3, 2, 2, 1, 0];

        self._ui_mesh_vertex_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::VERTEX_BUFFER,
            &vertex_datas,
        );

        self._ui_mesh_index_buffer = buffer::create_buffer_data_with_uploads(
            device,
            command_pool,
            command_queue,
            device_memory_properties,
            vk::BufferUsageFlags::INDEX_BUFFER,
            &indices
        );
        self._ui_mesh_index_count = indices.len() as u32;
    }

    pub fn create_widget(widget_type: UIWidgetTypes) -> *mut dyn Widget {
        Box::into_raw(match widget_type {
            UIWidgetTypes::Default => WidgetDefault::create_widget(),
        })
    }

    pub fn collect_ui_render_data(&mut self) {
        let font_data = self._font_data.borrow();
        let mut render_ui_count: u32 = 0;
        let mut render_ui_group: Vec<UIRenderGroupData> = Vec::new();
        let mut prev_render_group_data = UIRenderGroupData {
            _accumulated_render_count: 0,
            _material_instance: std::ptr::null(),
        };
        unsafe {
            self._root.get_ui_component_mut().as_mut().unwrap().collect_ui_render_data(
                &font_data,
                &mut render_ui_count,
                &mut render_ui_group,
                &mut prev_render_group_data,
                &mut self._ui_render_datas,
            );

            // last render count
            if 0 < render_ui_count {
                UIRenderGroupData::add_ui_render_group_data(&mut render_ui_group, render_ui_count, &mut prev_render_group_data, std::ptr::null());
            }
        }
        self._render_ui_count = render_ui_count;
        self._render_ui_group = render_ui_group;
    }

    pub fn render_ui(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_data: &RendererData,
        resources: &Resources
    ) {
        if 0 < self._render_ui_count {
            let framebuffer_data = resources.get_framebuffer_data("render_ui").borrow();
            let mut push_constant_data = PushConstant_RenderUI {
                _inv_canvas_size: Vector2::new(1.0 / framebuffer_data._framebuffer_info._framebuffer_width as f32, 1.0 / framebuffer_data._framebuffer_info._framebuffer_height as f32),
                _instance_id_offset: 0,
                _reserved0: 0,
            };

            // upload storage buffer
            let upload_data = &self._ui_render_datas[0..self._render_ui_count as usize];
            renderer_data.upload_shader_buffer_datas(command_buffer, swapchain_index, ShaderBufferDataType::UIRenderDataBuffer, upload_data);

            // render ui
            let mut prev_material_instance_data: *const MaterialInstanceData = std::ptr::null();
            let mut prev_pipeline_data: *const PipelineData = std::ptr::null();
            let mut prev_pipeline_binding_data: *const PipelineBindingData = std::ptr::null();
            for render_group_data in self._render_ui_group.iter() {
                unsafe {
                    let render_count = render_group_data._accumulated_render_count - push_constant_data._instance_id_offset;
                    let material_instance_data = if render_group_data._material_instance.is_null() {
                        self._default_render_ui_material.as_ref().unwrap().as_ptr()
                    } else {
                        render_group_data._material_instance
                    };

                    if prev_material_instance_data != material_instance_data {
                        let pipeline_binding_data: &PipelineBindingData = (*material_instance_data).get_default_pipeline_binding_data();
                        let render_pass_data = pipeline_binding_data.get_render_pass_data().borrow();
                        let pipeline_data = pipeline_binding_data.get_pipeline_data();
                        let pipeline_data_ptr: *const PipelineData = pipeline_data.as_ptr();
                        let pipeline_data: &PipelineData = &pipeline_data.borrow();

                        if prev_pipeline_data != pipeline_data_ptr {
                            if false == prev_pipeline_data.is_null() {
                                renderer_data.end_render_pass(command_buffer);
                            }
                            renderer_data.begin_render_pass_pipeline(command_buffer, swapchain_index, &render_pass_data, pipeline_data, None);
                            prev_pipeline_data = pipeline_data_ptr;
                        }

                        if prev_pipeline_binding_data != pipeline_binding_data {
                            prev_pipeline_binding_data = pipeline_binding_data;
                            let render_ui_descriptor_sets = Some(&pipeline_binding_data._descriptor_sets);
                            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, &(*pipeline_binding_data), render_ui_descriptor_sets);
                        }
                        prev_material_instance_data = material_instance_data;
                    }

                    renderer_data.upload_push_constant_data(command_buffer, &(*prev_pipeline_data), &push_constant_data);
                    renderer_data.draw_elements(
                        command_buffer,
                        &[self._ui_mesh_vertex_buffer._buffer],
                        &[],
                        render_count,
                        self._ui_mesh_index_buffer._buffer,
                        self._ui_mesh_index_count,
                    );
                    push_constant_data._instance_id_offset = render_group_data._accumulated_render_count;
                }
            }
            renderer_data.end_render_pass(command_buffer);
        }
    }

    pub fn update(&mut self, window_size: (u32, u32), delta_time: f64, resources: &Resources) {
        static mut test: bool = true;
        unsafe {
            if test {
                let ui_component = &mut self._root.get_ui_component_mut().as_mut().unwrap();
                ui_component.set_layout_type(UILayoutType::BoxLayout);
                ui_component.set_layout_orientation(Orientation::HORIZONTAL);
                ui_component.set_halign(HorizontalAlign::RIGHT);
                ui_component.set_valign(VerticalAlign::BOTTOM);
                ui_component.set_pos(200.0, 200.0);
                ui_component.set_size_x(400.0);
                ui_component.set_size_y(300.0);
                ui_component.set_color(get_color32(255, 255, 0, 255));
                ui_component.set_font_color(get_color32(0, 0, 0, 255));
                ui_component.set_border_color(get_color32(0, 0, 255, 255));
                ui_component.set_margine(5.0);
                ui_component.set_padding(5.0);
                ui_component.set_round(10.0);
                ui_component.set_border(5.0);
                ui_component.set_font_size(20.0);
                ui_component.set_text(String::from("Text ui\nNext line\tTab\n\tOver text"));

                let btn = UIManager::create_widget(UIWidgetTypes::Default);
                let ui_component = &mut btn.as_mut().unwrap().get_ui_component_mut().as_mut().unwrap();
                ui_component.set_pos(25.0, 25.0);
                ui_component.set_size(200.0, 100.0);
                ui_component.set_color(get_color32(255, 255, 255, 255));
                ui_component.set_font_color(get_color32(0, 0, 0, 255));
                ui_component.set_border_color(get_color32(255, 0, 0, 255));
                ui_component.set_margine(5.0);
                ui_component.set_round(10.0);
                ui_component.set_border(5.0);
                ui_component.set_text(String::from("Child\nChild Test"));
                ui_component.set_material_instance(&resources.get_material_instance_data("render_ui_test"));
                self._root.add_widget(btn);

                let btn2 = UIManager::create_widget(UIWidgetTypes::Default);
                let ui_component = &mut btn2.as_mut().unwrap().get_ui_component_mut().as_mut().unwrap();
                ui_component.set_pos(0.0, 0.0);
                ui_component.set_size(100.0, 50.0);
                ui_component.set_color(get_color32(255, 128, 128, 255));
                ui_component.set_font_color(get_color32(255, 255, 255, 255));
                ui_component.set_border_color(get_color32(0, 0, 0, 255));
                ui_component.set_margine(5.0);
                ui_component.set_round(10.0);
                ui_component.set_border(5.0);

                ui_component.set_text(String::from("Btn2\nBtn2 Test"));
                self._root.add_widget(btn2);

                test = false;
            }

            let ui_component = &mut self._root.get_ui_component_mut().as_mut().unwrap();
            let touch_evemt: bool = false;
            let contents_area = Vector4::new(0.0, 0.0, window_size.0 as f32, window_size.1 as f32);
            let contents_size = Vector2::new(window_size.0 as f32, window_size.1 as f32);
            let required_contents_size = Vector2::<f32>::zeros();
            let mut child_ui_pos = Vector2::<f32>::zeros();

            if ui_component.get_changed_layout() {
                ui_component.update_layout_size(&contents_size);
                ui_component.update_layout_area(
                    UILayoutType::FloatLayout,
                    Orientation::HORIZONTAL,
                    DEFAILT_HORIZONTAL_ALIGN,
                    DEFAILT_VERTICAL_ALIGN,
                    &contents_area,
                    &contents_size,
                    &required_contents_size,
                    &mut child_ui_pos
                );
            }
            ui_component.update_layout(&self._font_data.borrow());
            ui_component.update(delta_time, touch_evemt);

            self.collect_ui_render_data();
        }
    }
}