use std::os::raw::c_void;
use std::rc::Rc;

use bitflags::bitflags;
use nalgebra::{Matrix4, Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};

use crate::scene::font::FontData;
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::system::RcRefCell;
use crate::vulkan_context::buffer::BufferData;

pub type CallbackTouchEvent = fn(
    ui_component: &mut UIComponentInstance,
    touched_pos: &Vector2<f32>,
    touched_pos_delta: &Vector2<f32>,
) -> bool;

pub const UI_RENDER_FONT_PADDING_RATIO: f32 = 0.7;

pub const UI_RENDER_FLAG_NONE: u32 = 0;
pub const UI_RENDER_FLAG_RENDER_TEXT: u32 = 1 << 0;
pub const UI_RENDER_FLAG_RENDER_TEXTURE: u32 = 1 << 1;
pub const UI_RENDER_FLAG_TOUCHED: u32 = 1 << 2;

pub const UI_INDEX_LEFT: usize = 0; // x
pub const UI_INDEX_TOP: usize = 1; // y
pub const UI_INDEX_RIGHT: usize = 2; // z
pub const UI_INDEX_BOTTOM: usize = 3; // w

pub const DEFAULT_HORIZONTAL_ALIGN: HorizontalAlign = HorizontalAlign::LEFT;
pub const DEFAULT_VERTICAL_ALIGN: VerticalAlign = VerticalAlign::TOP;

// |--ui-size----------------------------------------------------------------------------|
// |--margin--|--border--|--padding--|--contents-size--|--padding--|--border--|--margin--|
//            |--render-size--------------------------------------------------|

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct PushConstant_RenderUI {
    pub _inv_canvas_size: Vector2<f32>,
    pub _instance_id_offset: u32,
    pub _reserved0: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum UIWidgetTypes {
    Default,
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

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    pub struct UICornerFlags: i32 {
        const NONE = 0;
        const LEFT = 1 << 0;
        const RIGHT = 1 << 1;
        const BOTTOM = 1 << 2;
        const TOP = 1 << 3;
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct UIVertexData {
    pub _position: Vector3<f32>,
}

#[derive(Debug, Clone, Copy)]
pub struct UIRenderGroupData {
    pub _accumulated_render_count: u32,
    pub _material_instance: *const MaterialInstanceData,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct UIRenderData {
    pub _ui_texcoord: Vector4<f32>,
    pub _ui_render_area: Vector4<f32>,
    pub _ui_renderable_area: Vector4<f32>,
    pub _ui_color: u32,
    pub _ui_round: f32,
    pub _ui_border: f32,
    pub _ui_border_color: u32,
    pub _ui_render_flags: u32,
    pub _ui_opacity: f32,
    pub _reserved0: u32,
    pub _reserved1: u32,
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
    pub _margin: Vector4<f32>,
    pub _texcoord: Vector4<f32>,
    pub _draggable: bool,
    pub _touchable: bool,
    pub _has_cursor: bool,
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
    pub _owner_widget: *const dyn Widget,
    pub _parent: *const UIComponentInstance,
    pub _children: Vec<*const UIComponentInstance>,
    pub _changed_layout: bool,
    pub _changed_child_layout: bool,
    pub _changed_deep_child_layout: bool,
    pub _changed_render_data: bool,
    pub _changed_text: bool,
    pub _render_ui_index: u32,
    pub _transform: TransformObjectData,
    pub _world_to_local_matrix: Matrix4<f32>,
    pub _local_to_world_matrix: Matrix4<f32>,
    pub _spaces: Vector4<f32>, // margin + border + padding
    pub _contents_area: Vector4<f32>,
    pub _contents_area_size: Vector2<f32>, // ui_size - spaces
    pub _text_contents_size: Vector2<f32>, // just text contents size
    pub _required_contents_area: Vector4<f32>,
    pub _required_contents_size: Vector2<f32>, // required size of text_contents_size and sum of uisize of children
    pub _text_counts: Vec<usize>,              // text column count, text row count
    pub _render_area: Vector4<f32>,            // border + _ui_component_data._size
    pub _renderable_area: Vector4<f32>,        // inherit _render_area
    pub _ui_area: Vector4<f32>,
    pub _ui_size: Vector2<f32>, // margie + border + padding + _ui_component_data._size
    pub _ui_layout_state: UILayoutState,
    pub _opacity: f32,
    pub _renderable: bool,
    pub _visible: bool, // hierarchical visible flag
    pub _touched: bool,
    pub _touch_start_pos: Vector2<f32>,
    pub _touch_corner_flags: UICornerFlags,
    pub _text: String,
    pub _render_text_count: u32,
    pub _callback_touch_down: *const CallbackTouchEvent,
    pub _callback_touch_move: *const CallbackTouchEvent,
    pub _callback_touch_up: *const CallbackTouchEvent,
    pub _user_data: *const c_void,
}

pub trait Widget {
    fn get_ui_widget_name(&self) -> &String;
    fn get_ui_widget_type(&self) -> UIWidgetTypes;
    fn has_cursor(&self) -> bool;
    fn get_ui_component(&self) -> &UIComponentInstance;
    fn get_ui_component_mut(&mut self) -> &mut UIComponentInstance;
    fn get_changed_layout(&self) -> bool;
    fn clear_parent(&mut self);
    fn has_parent(&self) -> bool;
    fn get_parent(&self) -> &dyn Widget;
    fn set_parent(&mut self, widget: *const dyn Widget);
    fn add_widget(&mut self, widget: &Rc<dyn Widget>);
    fn remove_widget(&mut self, widget: *const dyn Widget);
    fn clear_widgets(&mut self);
}

pub struct WidgetDefault {
    pub _ui_widget_name: String,
    pub _ui_widget_type: UIWidgetTypes,
    pub _ui_component: UIComponentInstance,
    pub _parent: *const dyn Widget,
    pub _widgets: Vec<Rc<dyn Widget>>,
}

pub struct UIManager {
    pub _root: Rc<dyn Widget>,
    pub _window_size: Vector2<i32>,
    pub _ui_mesh_vertex_buffer: BufferData,
    pub _ui_mesh_index_buffer: BufferData,
    pub _ui_mesh_index_count: u32,
    pub _font_data: RcRefCell<FontData>,
    pub _ui_render_data_list: Vec<UIRenderData>,
    pub _render_ui_count: u32,
    pub _render_ui_group: Vec<UIRenderGroupData>,
    pub _default_render_ui_material: Option<RcRefCell<MaterialInstanceData>>,
    pub _ui_world_axis: Option<UIWorldAxis>,
}

pub struct UIWorldAxis {
    pub _widget_axis_x: Rc<dyn Widget>,
    pub _widget_axis_y: Rc<dyn Widget>,
    pub _widget_axis_z: Rc<dyn Widget>,
}
