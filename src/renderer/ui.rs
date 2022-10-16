use std::os::raw::c_void;
use std::rc::Rc;

use bitflags::bitflags;
use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector2, Vector3, Vector4, Matrix4 };
use ash::{ vk, Device };

use crate::constants;
use crate::application::application::TimeData;
use crate::application::input::{
    KeyboardInputData,
    MouseMoveData,
    MouseInputData,
};
use crate::resource::resource::EngineResources;
use crate::renderer::font::FontData;
use crate::renderer::material_instance::{ PipelineBindingData, MaterialInstanceData };
use crate::renderer::push_constants::{PushConstant, PushConstantName};
use crate::renderer::renderer_context::{ RendererContext };
use crate::renderer::transform_object::TransformObjectData;
use crate::utilities::system::{ self, RcRefCell, ptr_as_ref, ptr_as_mut };
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::geometry_buffer::{ self, VertexData };
use crate::vulkan_context::render_pass::{ PipelineData };
use crate::vulkan_context::vulkan_context::{ get_color32 };

pub type CallbackTouchEvent = fn(ui_component: &mut UIComponentInstance, touched_pos: &Vector2<f32>, touched_pos_delta: &Vector2<f32>) -> bool;

pub const UI_RENDER_FONT_PADDING_RATIO: f32 = 0.7;

pub const UI_RENDER_FLAG_NONE: u32 = 0;
pub const UI_RENDER_FLAG_RENDER_TEXT: u32 = 1 << 0;
pub const UI_RENDER_FLAG_RENDER_TEXTURE: u32 = 1 << 1;
pub const UI_RENDER_FLAG_TOUCHED: u32 = 1 << 2;

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
#[derive(Debug, Clone, Default)]
pub struct PushConstant_RenderUI {
    pub _inv_canvas_size: Vector2<f32>,
    pub _instance_id_offset: u32,
    pub _reserved0: u32,
}

impl PushConstantName for PushConstant_RenderUI {
    fn get_push_constant_name(&self) -> &str {
        "PushConstant_RenderUI"
    }
}

impl PushConstant for PushConstant_RenderUI {
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

bitflags! {
    pub struct UICornerFlags: i32 {
        const NONE = 0;
        const LEFT = 1 << 0;
        const RIGHT = 1 << 1;
        const BOTTOM = 1 << 2;
        const TOP = 1 << 3;
    }
}

impl Default for UICornerFlags {
    fn default() -> UICornerFlags {
        UICornerFlags::NONE
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
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

impl Default for UIRenderData {
    fn default() -> UIRenderData {
        UIRenderData {
            _ui_texcoord: Vector4::new(0.0, 0.0, 1.0, 1.0),
            _ui_render_area: Vector4::zeros(),
            _ui_renderable_area: Vector4::zeros(),
            _ui_color: 0xFFFFFFFF,
            _ui_round: 0.0,
            _ui_border: 0.0,
            _ui_border_color: 0x00000000,
            _ui_render_flags: UI_RENDER_FLAG_NONE,
            _ui_opacity: 1.0,
            _reserved0: 0,
            _reserved1: 0,
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
    pub _spaces: Vector4<f32>, // margine + border + padding
    pub _contents_area: Vector4<f32>,
    pub _contents_area_size: Vector2<f32>, // ui_size - spaces
    pub _text_contents_size: Vector2<f32>, // just text contents size
    pub _required_contents_area: Vector4<f32>,
    pub _required_contents_size: Vector2<f32>, // required size of text_contents_size and sum of uisize of children
    pub _text_counts: Vec<usize>, // text column count, text row count
    pub _render_area: Vector4<f32>, // border + _ui_component_data._size
    pub _renderable_area: Vector4<f32>, // inherit _render_area
    pub _ui_area: Vector4<f32>,
    pub _ui_size: Vector2<f32>, // margie + border + padding + _ui_component_data._size
    pub _ui_layout_state: UILayoutState,
    pub _opacity: f32,
    pub _renderable: bool,
    pub _visible: bool, // hierachycal visible flag
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

pub struct WidgetDefault {
    pub _ui_widget_name: String,
    pub _ui_widget_type: UIWidgetTypes,
    pub _ui_component: UIComponentInstance,
    pub _parent: *const dyn Widget,
    pub _widgets: Vec<Rc<dyn Widget>>,
}

pub trait ProjectUIManagerBase {
    fn get_ui_manager(&self) -> &UIManager;
    fn get_ui_manager_mut(&self) -> &mut UIManager;
    fn get_root_widget(&self) -> &dyn Widget;
    fn get_root_widget_mut(&self) -> &mut dyn Widget;
    fn initialize_project_ui_manager(&mut self, ui_manager: &UIManager);
    fn build_ui(&mut self, renderer_context: &RendererContext, engine_resources: &EngineResources);
}

pub struct UIManager {
    pub _project_ui_manager: *const dyn ProjectUIManagerBase,
    pub _root: Rc<dyn Widget>,
    pub _window_size: Vector2<i32>,
    pub _ui_mesh_vertex_buffer: BufferData,
    pub _ui_mesh_index_buffer: BufferData,
    pub _ui_mesh_index_count: u32,
    pub _font_data: RcRefCell<FontData>,
    pub _ui_render_datas: Vec<UIRenderData>,
    pub _render_ui_count: u32,
    pub _render_ui_group: Vec<UIRenderGroupData>,
    pub _default_render_ui_material: Option<RcRefCell<MaterialInstanceData>>,
}

//////////////////////////////////////////
// interfaces
/////////////////////////////////////////

impl Default for UIComponentData {
    fn default() -> UIComponentData {
        UIComponentData {
            _layout_type: UILayoutType::FloatLayout,
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
            _has_cursor: false,
            _scroll_x: false,
            _scroll_y: false,
            _expandable_x: false,
            _expandable_y: false,
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

impl UIComponentInstance {
    pub fn create_ui_component() -> UIComponentInstance {
        UIComponentInstance {
            _ui_component_data: UIComponentData::default(),
            _owner_widget: std::ptr::null() as *const WidgetDefault,
            _parent: std::ptr::null(),
            _children: Vec::new(),
            _changed_layout: true,
            _changed_child_layout: false,
            _changed_deep_child_layout: false,
            _changed_render_data: true,
            _changed_text: false,
            _render_ui_index: std::u32::MAX,
            _transform: TransformObjectData::new_transform_object_data(),
            _world_to_local_matrix: Matrix4::identity(),
            _local_to_world_matrix: Matrix4::identity(),
            _spaces: Vector4::zeros(),
            _contents_area: Vector4::zeros(),
            _contents_area_size: Vector2::zeros(),
            _text_contents_size: Vector2::zeros(),
            _required_contents_area: Vector4::zeros(),
            _required_contents_size: Vector2::zeros(),
            _render_area: Vector4::zeros(),
            _renderable_area: Vector4::zeros(),
            _ui_area: Vector4::zeros(),
            _ui_size: Vector2::zeros(),
            _ui_layout_state: UILayoutState::Unknown,
            _opacity: 1.0,
            _renderable: true,
            _visible: true,
            _touched: false,
            _touch_start_pos: Vector2::zeros(),
            _touch_corner_flags: UICornerFlags::NONE,
            _text: String::new(),
            _text_counts: Vec::new(),
            _render_text_count: 0,
            _callback_touch_down: std::ptr::null(),
            _callback_touch_move: std::ptr::null(),
            _callback_touch_up: std::ptr::null(),
            _user_data: std::ptr::null(),
        }
    }

    pub fn get_owner_widget(&self) -> &dyn Widget { ptr_as_ref(self._owner_widget) }
    pub fn has_parent(&self) -> bool { false == self._parent.is_null() }
    pub fn get_parent(&self) -> &UIComponentInstance { ptr_as_ref(self._parent) }
    pub fn set_parent(&mut self, parent: *const UIComponentInstance) {
        if false == self._parent.is_null() {
            panic!("Widget already has parent");
        }
        self._parent = parent;
    }
    pub fn clear_children(&mut self) {
        for child in self._children.iter() {
            ptr_as_mut(*child).clear_children();
        }
        self._children.clear();
        self.set_changed_layout(true);
    }
    pub fn check_collide(&self, touched_pos: &Vector2<f32>) -> bool {
        self._renderable_area.x <= touched_pos.x && touched_pos.x < self._renderable_area.z && self._renderable_area.y <= touched_pos.y && touched_pos.y < self._renderable_area.w
    }

    pub fn on_touch_down(&mut self, touched_pos: &Vector2<f32>, touched_pos_delta: &Vector2<f32>) {
        if self.get_touchable() || self.get_dragable() {
            let mut touched: bool = true;
            self._touch_start_pos.x = touched_pos.x;
            self._touch_start_pos.y = touched_pos.y;
            let touched_offset_x = self.get_pos_x() - touched_pos.x;
            let touched_offset_y = self.get_pos_y() - touched_pos.y;

            let check_thickness: f32 = 10.0;
            self._touch_corner_flags = UICornerFlags::NONE;
            if touched_offset_x.abs() <= check_thickness {
                self._touch_corner_flags.insert(UICornerFlags::LEFT);
            } else if (touched_offset_x + self.get_size_x()).abs() <= check_thickness {
                self._touch_corner_flags.insert(UICornerFlags::RIGHT);
            }

            if touched_offset_y.abs() <= check_thickness {
                self._touch_corner_flags.insert(UICornerFlags::TOP);
            } else if (touched_offset_y + self.get_size_y()).abs() <= check_thickness {
                self._touch_corner_flags.insert(UICornerFlags::BOTTOM);
            }

            if false == self._callback_touch_down.is_null() {
                touched = ptr_as_ref(self._callback_touch_down)(self, touched_pos, touched_pos_delta);
            }
            self._changed_render_data = true;
            self._touched = touched;
        }
    }

    pub fn on_touch_move(&mut self, touched_pos: &Vector2<f32>, touched_pos_delta: &Vector2<f32>) {
        if self._touched {
            if self.get_touchable() || self.get_dragable() {
                if self.get_dragable() {
                    if self._touch_corner_flags == UICornerFlags::NONE {
                        self.set_pos(self.get_pos().x + touched_pos_delta.x, self.get_pos().y + touched_pos_delta.y);
                    }
                }

                if self.get_resizable_x() {
                    if self._touch_corner_flags.contains(UICornerFlags::LEFT) {
                        let size_x = 0f32.max(self.get_size_x() - touched_pos_delta.x);
                        self.set_size_x(size_x);
                        if 0.0 < size_x {
                            self.set_pos_x(self.get_pos_x() + touched_pos_delta.x);
                        }
                    } else if self._touch_corner_flags.contains(UICornerFlags::RIGHT) {
                        self.set_size_x(0f32.max(self.get_size_x() + touched_pos_delta.x));
                    }
                }

                if self.get_resizable_y() {
                    if self._touch_corner_flags.contains(UICornerFlags::TOP) {
                        let size_y = 0f32.max(self.get_size_y() - touched_pos_delta.y);
                        self.set_size_y(size_y);
                        if 0.0 < size_y {
                            self.set_pos_y(self.get_pos_y() + touched_pos_delta.y);
                        }
                    } else if self._touch_corner_flags.contains(UICornerFlags::BOTTOM) {
                        self.set_size_y(0f32.max(self.get_size_y() + touched_pos_delta.y));
                    }
                }

                if false == self._callback_touch_move.is_null() {
                    ptr_as_ref(self._callback_touch_move)(self, touched_pos, touched_pos_delta);
                }
                self._changed_render_data = true;
            }
        }
    }

    pub fn on_touch_up(&mut self, touched_pos: &Vector2<f32>, touched_pos_delta: &Vector2<f32>) {
        if self._touched {
            if self.get_touchable() || self.get_dragable() {
                self._touch_corner_flags = UICornerFlags::NONE;

                if self.get_dragable() {
                    if self._touch_corner_flags == UICornerFlags::NONE {
                        self.set_pos(self.get_pos().x + touched_pos_delta.x, self.get_pos().y + touched_pos_delta.y);
                    }
                }

                if false == self._callback_touch_up.is_null() {
                    ptr_as_ref(self._callback_touch_up)(self, touched_pos, touched_pos_delta);
                }
                self._changed_render_data = true;
            }
            self._touched = false;
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
            self.set_changed_layout(true);
        }
    }
    pub fn set_pos_y(&mut self, y: f32) {
        if y != self._ui_component_data._pos.y || self._ui_component_data._pos_hint_y.is_some() {
            self._ui_component_data._pos_hint_y = None;
            self._ui_component_data._pos.y = y;
            self.set_changed_layout(true);
        }
    }
    pub fn get_center_x(&self) -> f32 { self._ui_component_data._pos.x + self._ui_component_data._size.x * 0.5 }
    pub fn get_center_y(&self) -> f32 { self._ui_component_data._pos.y + self._ui_component_data._size.y + 0.5 }
    pub fn get_center(&self) -> Vector2<f32> { &self._ui_component_data._pos + &self._ui_component_data._size * 0.5 }
    pub fn set_center(&mut self, x: f32, y: f32) {
        self.set_center_x(x);
        self.set_center_y(y);
    }
    pub fn set_center_x(&mut self, x: f32) {
        if x != self._ui_component_data._pos.x || self._ui_component_data._pos_hint_x.is_some() {
            self._ui_component_data._pos_hint_x = None;
            self._ui_component_data._pos.x = x - self._ui_component_data._size.x * 0.5;
            self.set_changed_layout(true);
        }
    }
    pub fn set_center_y(&mut self, y: f32) {
        if y != self._ui_component_data._pos.y || self._ui_component_data._pos_hint_y.is_some() {
            self._ui_component_data._pos_hint_y = None;
            self._ui_component_data._pos.y = y - self._ui_component_data._size.y * 0.5;
            self.set_changed_layout(true);
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
            self.set_changed_layout(true);
        }
    }
    pub fn set_pos_hint_y(&mut self, pos_hint_y: Option<f32>) {
        if pos_hint_y != self._ui_component_data._pos_hint_y {
            self._ui_component_data._pos_hint_y = pos_hint_y;
            self.set_changed_layout(true);
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
            self.set_changed_layout(true);
        }
    }
    pub fn set_size_y(&mut self, size_y: f32) {
        if size_y != self._ui_component_data._size.y || self._ui_component_data._size_hint_y.is_some() {
            self._ui_component_data._size_hint_y = None;
            self._ui_component_data._size.y = size_y;
            self.set_changed_layout(true);
        }
    }
    pub fn get_size_hint_x(&self) -> Option<f32> { self._ui_component_data._size_hint_x }
    pub fn get_size_hint_y(&self) -> Option<f32> { self._ui_component_data._size_hint_y }
    pub fn set_size_hint_x(&mut self, size_hint_x: Option<f32>) {
        if size_hint_x != self._ui_component_data._size_hint_x {
            self._ui_component_data._size_hint_x = size_hint_x;
            self.set_changed_layout(true);
        }
    }
    pub fn set_size_hint_y(&mut self, size_hint_y: Option<f32>) {
        if size_hint_y != self._ui_component_data._size_hint_y {
            self._ui_component_data._size_hint_y = size_hint_y;
            self.set_changed_layout(true);
        }
    }
    pub fn set_margine(&mut self, margine: f32) { self.set_margines(Vector4::new(margine, margine, margine, margine)); }
    pub fn set_margines(&mut self, margine: Vector4<f32>) {
        if margine != self._ui_component_data._margine {
            self._ui_component_data._margine = margine;
            self.set_changed_layout(true);
        }
    }
    fn set_margine_inner(&mut self, index: usize, margine: f32) {
        if margine != self._ui_component_data._margine[index] {
            self._ui_component_data._margine[index] = margine;
            self.set_changed_layout(true);
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
            self.set_changed_layout(true);
        }
    }
    fn set_padding_inner(&mut self, index: usize, padding: f32) {
        if padding != self._ui_component_data._padding[index] {
            self._ui_component_data._padding[index] = padding;
            self.set_changed_layout(true);
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
            self.set_changed_layout(true);
        }
    }
    pub fn get_valign(&self) -> VerticalAlign { self._ui_component_data._valign }
    pub fn set_valign(&mut self, valign: VerticalAlign) {
        if valign != self._ui_component_data._valign {
            self._ui_component_data._valign = valign;
            self.set_changed_layout(true);
        }
    }
    pub fn get_changed_layout(&self) -> bool { self._changed_layout }
    pub fn set_changed_layout(&mut self, changed_layout: bool) { self._changed_layout = changed_layout; }
    pub fn get_changed_child_layout(&self) -> bool { self._changed_child_layout }
    pub fn set_changed_child_layout(&mut self, changed_child_layout: bool) { self._changed_child_layout = changed_child_layout; }
    pub fn get_changed_deep_child_layout(&self) -> bool { self._changed_deep_child_layout }
    pub fn set_changed_deep_child_layout(&mut self, changed_deep_child_layout: bool) { self._changed_deep_child_layout = changed_deep_child_layout; }
    pub fn get_renderable(&self) -> bool { self._renderable }
    pub fn set_renderable(&mut self, renderable: bool) {
        self._renderable = renderable;
        self._changed_render_data = true;
    }
    pub fn get_visible(&self) -> bool { self._visible }
    pub fn set_visible(&mut self, visible: bool) {
        self._visible = visible;
        self._changed_render_data = true;
    }
    pub fn get_opacity(&self) -> f32 { self._opacity }
    pub fn set_opacity(&mut self, opacity: f32) {
        self._opacity = opacity;
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
            self.set_changed_layout(true);
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
    pub fn set_callback_touch_down(&mut self, callback_touch_down: *const CallbackTouchEvent) {
        self._callback_touch_down = callback_touch_down;
    }
    pub fn set_callback_touch_move(&mut self, callback_touch_move: *const CallbackTouchEvent) {
        self._callback_touch_move = callback_touch_move;
    }
    pub fn set_callback_touch_up(&mut self, callback_touch_up: *const CallbackTouchEvent) {
        self._callback_touch_up = callback_touch_up;
    }
    pub fn get_user_data(&self) -> *const c_void {
        self._user_data
    }
    pub fn set_user_data(&mut self, user_data: *const c_void) {
        self._user_data = user_data;
    }
    pub fn get_touched(&self) -> bool { self._touched }
    pub fn get_touch_start_pos(&self) -> &Vector2<f32> { &self._touch_start_pos }
    pub fn get_touch_corner_flags(&self) -> UICornerFlags { self._touch_corner_flags }
    pub fn get_dragable(&self) -> bool { self._ui_component_data._dragable }
    pub fn set_dragable(&mut self, dragable: bool) { self._ui_component_data._dragable = dragable; }
    pub fn get_touchable(&self) -> bool { self._ui_component_data._touchable }
    pub fn set_touchable(&mut self, touchable: bool) { self._ui_component_data._touchable = touchable; }
    pub fn get_has_cursor(&self) -> bool { self._ui_component_data._has_cursor }
    pub fn set_has_cursor(&mut self, has_cursor: bool) { self._ui_component_data._has_cursor = has_cursor; }
    pub fn get_scroll_x(&self) -> bool { self._ui_component_data._scroll_x }
    pub fn get_scroll_y(&self) -> bool { self._ui_component_data._scroll_y }
    pub fn get_expandable(&self) -> (bool, bool) { (self._ui_component_data._expandable_x, self._ui_component_data._expandable_y) }
    pub fn set_expandable(&mut self, expandable: bool) {
        self._ui_component_data._expandable_x = expandable;
        self._ui_component_data._expandable_y = expandable;
        self._changed_layout = true;
    }
    pub fn get_expandable_x(&self) -> bool { self._ui_component_data._expandable_x }
    pub fn set_expandable_x(&mut self, expandable: bool) {
        self._ui_component_data._expandable_x = expandable;
        self._changed_layout = true;
    }
    pub fn get_expandable_y(&self) -> bool { self._ui_component_data._expandable_y }
    pub fn set_expandable_y(&mut self, expandable: bool) {
        self._ui_component_data._expandable_y = expandable;
        self._changed_layout = true;
    }
    pub fn get_resizable(&self) -> (bool, bool) { (self._ui_component_data._resizable_x, self._ui_component_data._resizable_y) }
    pub fn set_resizable(&mut self, resizable: bool) {
        self._ui_component_data._resizable_x = resizable;
        self._ui_component_data._resizable_y = resizable;
        self._changed_layout = true;
    }
    pub fn get_resizable_x(&self) -> bool { self._ui_component_data._resizable_x }
    pub fn set_resizable_x(&mut self, resizable: bool) { self._ui_component_data._resizable_x = resizable; }
    pub fn get_resizable_y(&self) -> bool { self._ui_component_data._resizable_y }
    pub fn set_resizable_y(&mut self, resizable: bool) { self._ui_component_data._resizable_y = resizable; }
    pub fn add_ui_component(&mut self, child_ptr: *const UIComponentInstance) {
        let child = ptr_as_mut(child_ptr);
        if child.has_parent() {
            panic!("Widget already has parent");
        }

        if false == self._children.contains(&child_ptr) {
            self._children.push(child_ptr);
            child.set_parent(self);
            self.set_changed_layout(true);
        }
    }

    pub fn remove_ui_component(&mut self, child: *const UIComponentInstance) {
        if let Some(index) = self._children.iter().position(|x| *x == child) {
            // if self._viewport_manager.focused_widget is widget {
            //     self._viewport_manager.focused_widget = None
            // }
            self._children.remove(index);
            ptr_as_mut(child).set_parent(std::ptr::null());
            self.set_changed_layout(true);
        }
    }

    pub fn set_text(&mut self, text: &str) {
        if text != self._text {
            self._text = String::from(text);
            self._changed_text = true;
            self.set_changed_layout(true);
        }
    }

    pub fn compute_text_contents_size(&mut self, font_data: &FontData) -> Vector2<f32>{
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

            return Vector2::new(max_column_count as f32 * font_size.x * UI_RENDER_FONT_PADDING_RATIO, row_count as f32 * font_size.y * UI_RENDER_FONT_PADDING_RATIO);
        }
        Vector2::zeros()
    }

    pub fn collect_ui_font_render_data(
        &mut self,
        font_data: &FontData,
        render_ui_count: u32,
        _render_ui_group: &mut Vec<UIRenderGroupData>,
        _prev_render_group_data: &mut UIRenderGroupData,
        render_ui_instance_datas: &mut [UIRenderData],
        opacity: f32
    ) {
        let mut render_ui_index = render_ui_count;

        let count_of_side: u32 = font_data._count_of_side;
        let inv_count_of_side: f32 = 1.0 / font_data._count_of_side as f32;
        let font_size_ratio: f32 = self.get_font_size() / font_data._font_size.y;
        let font_size: Vector2<f32> = &font_data._font_size * font_size_ratio * UI_RENDER_FONT_PADDING_RATIO;
        let mut column: i32 = 0;
        let mut row: i32 = 0;
        self._render_text_count = 0;

        let text_renderable_area = Vector4::new(
            self._contents_area.x.max(self._renderable_area.x),
            self._contents_area.y.max(self._renderable_area.y),
            self._contents_area.z.min(self._renderable_area.z),
            self._contents_area.w.min(self._renderable_area.w)
        );

        // text_render_size_x
        let get_text_render_area_x = | halign: HorizontalAlign, contents_area_size_x: f32, contents_area_x: f32, column_count: usize | -> f32 {
            let text_render_size_x = column_count as f32 * font_size.x;
            match halign {
                HorizontalAlign::LEFT => contents_area_x,
                HorizontalAlign::CENTER => contents_area_x + (contents_area_size_x - text_render_size_x) * 0.5,
                HorizontalAlign::RIGHT => contents_area_x + contents_area_size_x - text_render_size_x,
            }
        };
        let mut text_render_area_x: f32 = get_text_render_area_x(self.get_halign(), self._contents_area_size.x, self._contents_area.x, self._text_counts[0]);

        // text_render_area_y
        let row_count = self._text_counts.len();
        let text_render_size_y = row_count as f32 * font_size.y;
        let text_render_area_y: f32 = match self.get_valign() {
            VerticalAlign::TOP => self._contents_area.y,
            VerticalAlign::CENTER => self._contents_area.y + (self._contents_area_size.y - text_render_size_y) * 0.5,
            VerticalAlign::BOTTOM => self._contents_area.y + self._contents_area_size.y - text_render_size_y,
        };

        let mut ui_render_area: Vector4<f32> = Vector4::zeros();
        for c in self._text.as_bytes().iter() {
            let ch = (*c) as char;
            if '\n' == ch {
                column = 0;
                row += 1;
                text_render_area_x = get_text_render_area_x(self.get_halign(), self._contents_area_size.x, self._contents_area.x, self._text_counts[row as usize]);
            } else if '\t' == ch {
                column += 4;
            } else if ' ' == ch {
                column += 1;
            } else {
                let index: u32 = 0i32.max((*c) as i32 - font_data._range_min as i32) as u32;
                let texcoord_x = (index % count_of_side) as f32 * inv_count_of_side;
                let texcoord_y = (index / count_of_side) as f32 * inv_count_of_side;

                ui_render_area.x = text_render_area_x + column as f32 * font_size.x;
                ui_render_area.y = text_render_area_y + row as f32 * font_size.y;
                ui_render_area.z = ui_render_area.x + font_size.x;
                ui_render_area.w = ui_render_area.y + font_size.y;

                if self._contents_area.x < ui_render_area.z && self._contents_area.y < ui_render_area.w &&
                    ui_render_area.x < self._contents_area.z && ui_render_area.y < self._contents_area.w {
                    let render_ui_instance_data = &mut render_ui_instance_datas[render_ui_index as usize];
                    render_ui_instance_data._ui_texcoord.x = texcoord_x;
                    render_ui_instance_data._ui_texcoord.y = texcoord_y;
                    render_ui_instance_data._ui_texcoord.z = texcoord_x + inv_count_of_side;
                    render_ui_instance_data._ui_texcoord.w = texcoord_y + inv_count_of_side;
                    render_ui_instance_data._ui_render_area = ui_render_area.clone() as Vector4<f32>;
                    render_ui_instance_data._ui_renderable_area.clone_from(&text_renderable_area);
                    render_ui_instance_data._ui_color = self.get_font_color();
                    render_ui_instance_data._ui_round = 0.0;
                    render_ui_instance_data._ui_border = 0.0;
                    render_ui_instance_data._ui_border_color = 0;
                    render_ui_instance_data._ui_opacity = 1.0;
                    render_ui_instance_data._ui_render_flags = UI_RENDER_FLAG_RENDER_TEXT;
                    render_ui_instance_data._ui_opacity = opacity;
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
        render_ui_instance_datas: &mut [UIRenderData],
        mut need_to_collect_render_data: bool,
        mut opacity: f32
    ) {
        if self._visible && self._renderable {
            let render_ui_index = *render_ui_count;
            *render_ui_count += 1;

            need_to_collect_render_data = need_to_collect_render_data || self._changed_render_data || render_ui_index != self._render_ui_index;

            // collect ui render data
            if need_to_collect_render_data {
                opacity *= self.get_opacity();

                let render_ui_instance_data = &mut render_ui_instance_datas[render_ui_index as usize];
                render_ui_instance_data._ui_render_area.clone_from(&self._render_area);
                render_ui_instance_data._ui_renderable_area.clone_from(&self._renderable_area);
                render_ui_instance_data._ui_opacity = opacity;
                render_ui_instance_data._ui_color = self.get_color();
                render_ui_instance_data._ui_round = self.get_round();
                render_ui_instance_data._ui_border = self.get_border();
                render_ui_instance_data._ui_border_color = self.get_border_color();
                render_ui_instance_data._ui_render_flags = UI_RENDER_FLAG_NONE;
                render_ui_instance_data._ui_texcoord.clone_from(&self._ui_component_data._texcoord);
                if self.get_material_instance().is_some() {
                    render_ui_instance_data._ui_render_flags |= UI_RENDER_FLAG_RENDER_TEXTURE;
                }
                if self._touched {
                    render_ui_instance_data._ui_render_flags |= UI_RENDER_FLAG_TOUCHED;
                }
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
                        opacity,
                    );
                }
                self._changed_text = false;
            }
            *render_ui_count += self._render_text_count;
        }

        if self._visible {
            for child_ui_component in self._children.iter() {
                ptr_as_mut(*child_ui_component).collect_ui_render_data(
                    font_data,
                    render_ui_count,
                    render_ui_group,
                    prev_render_group_data,
                    render_ui_instance_datas,
                    need_to_collect_render_data,
                    opacity
                );
            }
        }
    }

    fn update_layout_size(&mut self, mut inherit_changed_layout: bool, parent_contents_size: &Vector2<f32>, font_data: &FontData) {
        if inherit_changed_layout || self._changed_layout {
            inherit_changed_layout = true;

            let border = self.get_border();
            let spaces = self.get_margine() + self.get_padding() + &Vector4::new(border, border, border, border);
            let size_hint_x = self.get_size_hint_x();
            let size_hint_y = self.get_size_hint_y();
            let mut ui_size: Vector2<f32> = self.get_size().clone() as Vector2<f32>;
            if size_hint_x.is_some() {
                ui_size.x = parent_contents_size.x * size_hint_x.unwrap();
            }
            if size_hint_y.is_some() {
                ui_size.y = parent_contents_size.y * size_hint_y.unwrap();
            }

            // update contents area
            if self._changed_text {
                self._text_contents_size = self.compute_text_contents_size(font_data);
                self._changed_text = false;
            }

            // expandable
            if self.get_expandable_x() {
                ui_size.x = ui_size.x.max(self._text_contents_size.x + spaces.x + spaces.z);
            }
            if self.get_expandable_y() {
                ui_size.y = ui_size.y.max(self._text_contents_size.y + spaces.y + spaces.w);
            }

            self._spaces.clone_from(&spaces);
            self._ui_size.clone_from(&ui_size);
            self._contents_area_size.x = ui_size.x - spaces.x - spaces.z;
            self._contents_area_size.y = ui_size.y - spaces.y - spaces.w;
        }

        if inherit_changed_layout || self._changed_deep_child_layout {
            // update required contents size
            let mut required_contents_size = Vector2::<f32>::zeros();
            for child in self._children.iter() {
                let child_ui_instance = ptr_as_mut(*child);
                child_ui_instance.update_layout_size(inherit_changed_layout, &self._contents_area_size, font_data);

                // accumulate required_contents_size
                if UILayoutType::BoxLayout == self.get_layout_type() {
                    match self.get_layout_orientation() {
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
                self._required_contents_size = required_contents_size;
            }

            // update eexpandable size
            if self.get_expandable_x() && self._contents_area_size.x < self._required_contents_size.x {
                self._contents_area_size.x = self._contents_area_size.x.max(self._required_contents_size.x);
                self._ui_size.x = self._ui_size.x.max(self._required_contents_size.x + self._spaces.x + self._spaces.z);
            }
            if self.get_expandable_y() && self._contents_area_size.y < self._required_contents_size.y {
                self._contents_area_size.y = self._contents_area_size.y.max(self._required_contents_size.y);
                self._ui_size.y = self._ui_size.y.max(self._required_contents_size.y + self._spaces.y + self._spaces.w);
            }
        }
    }

    fn update_layout_area(
        &mut self,
        parent_layout_type: UILayoutType,
        parent_layout_orientation: Orientation,
        parent_halign: HorizontalAlign,
        parent_valign: VerticalAlign,
        parent_contents_area: &Vector4<f32>,
        parent_contents_area_size: &Vector2<f32>,
        required_contents_size: &Vector2<f32>,
        parent_renderable_area: &Vector4<f32>,
        ui_area_pos: &mut Vector2<f32>,
        _update_depth: u32,
    ) {
        match parent_layout_type {
            UILayoutType::FloatLayout => {
                let pos_hint_x = self.get_pos_hint_x();
                let pos_hint_y = self.get_pos_hint_y();
                if pos_hint_x.is_some() {
                    self._ui_area.x = parent_contents_area.x + parent_contents_area_size.x * pos_hint_x.unwrap();
                } else {
                    self._ui_area.x = parent_contents_area.x + self.get_pos_x();
                }
                if pos_hint_y.is_some() {
                    self._ui_area.y = parent_contents_area.y + parent_contents_area_size.y * pos_hint_y.unwrap();
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
                    },
                    Orientation::VERTICAL => {
                        self._ui_area.y = ui_area_pos.y;
                        match parent_halign {
                            HorizontalAlign::LEFT => self._ui_area.x = ui_area_pos.x,
                            HorizontalAlign::CENTER => self._ui_area.x = ui_area_pos.x + (required_contents_size.x - self._ui_size.x) * 0.5,
                            HorizontalAlign::RIGHT => self._ui_area.x = ui_area_pos.x + (required_contents_size.x - self._ui_size.x),
                        }
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

        self._renderable_area.x = self._render_area.x.max(parent_renderable_area.x);
        self._renderable_area.y = self._render_area.y.max(parent_renderable_area.y);
        self._renderable_area.z = self._render_area.z.min(parent_renderable_area.z);
        self._renderable_area.w = self._render_area.w.min(parent_renderable_area.w);

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

    fn update_layout(&mut self, mut inherit_changed_layout: bool, update_depth: u32, font_data: &FontData) {
        inherit_changed_layout = inherit_changed_layout || self._changed_layout;
        if inherit_changed_layout || self._changed_deep_child_layout {
            if inherit_changed_layout || self._changed_child_layout {
                let inherit_renderable_area = Vector4::new(
                    self._contents_area.x.max(self._renderable_area.x),
                    self._contents_area.y.max(self._renderable_area.y),
                    self._contents_area.z.min(self._renderable_area.z),
                    self._contents_area.w.min(self._renderable_area.w)
                );

                // calculate child_ui_pos
                let mut child_ui_pos = Vector2::<f32>::new(self._contents_area.x, self._contents_area.y);
                if UILayoutType::BoxLayout == self.get_layout_type() {
                    match self.get_halign() {
                        HorizontalAlign::LEFT => {},
                        HorizontalAlign::CENTER => child_ui_pos.x += (self._contents_area_size.x - self._required_contents_size.x) * 0.5,
                        HorizontalAlign::RIGHT => child_ui_pos.x += self._contents_area_size.x - self._required_contents_size.x,
                    }
                    match self.get_valign() {
                        VerticalAlign::TOP => {},
                        VerticalAlign::CENTER => child_ui_pos.y += (self._contents_area_size.y - self._required_contents_size.y) * 0.5,
                        VerticalAlign::BOTTOM => child_ui_pos.y += self._contents_area_size.y - self._required_contents_size.y,
                    }
                }

                // update children ui area
                for child in self._children.iter() {
                    let child_ui_instance = ptr_as_mut(*child);
                    child_ui_instance.update_layout_area(
                        self.get_layout_type(),
                        self.get_layout_orientation(),
                        self.get_halign(),
                        self.get_valign(),
                        &self._contents_area,
                        &self._contents_area_size,
                        &self._required_contents_size,
                        &inherit_renderable_area,
                        &mut child_ui_pos,
                        update_depth + 1
                    );

                    // update child_ui_pos
                    if UILayoutType::BoxLayout == self.get_layout_type() {
                        match self.get_layout_orientation() {
                            Orientation::HORIZONTAL => {
                                child_ui_pos.x += child_ui_instance._ui_size.x;
                            },
                            Orientation::VERTICAL => {
                                child_ui_pos.y += child_ui_instance._ui_size.y;
                            },
                        }
                    }
                }
            }

            // recursive update_layout
            for child in self._children.iter() {
                ptr_as_mut(*child).update_layout(inherit_changed_layout, update_depth + 1, font_data);
            }
        }

        // complete
        self._changed_render_data = true;
        self.set_changed_layout(false);
        self.set_changed_child_layout(false);
        self.set_changed_deep_child_layout(false);
        self._ui_layout_state = UILayoutState::Complete;
    }

    fn update_ui_component(
        &mut self,
        delta_time: f64,
        window_size: &Vector2<i32>,
        time_data: &TimeData,
        keyboard_input_data: &KeyboardInputData,
        mouse_pos: &Vector2<f32>,
        mouse_pos_delta: &Vector2<f32>,
        mouse_moved: bool,
        mouse_input_data: &MouseInputData,
        touch_event: &mut bool,
    ) {
        let mut child_index: isize = self._children.len() as isize - 1;
        while 0 <= child_index {
            let child_ui_instance = ptr_as_mut(self._children[child_index as usize]);
            child_ui_instance.update_ui_component(
                delta_time,
                window_size,
                time_data,
                keyboard_input_data,
                mouse_pos,
                mouse_pos_delta,
                mouse_moved,
                mouse_input_data,
                touch_event,
            );
            if child_ui_instance.get_changed_layout() {
                self._changed_child_layout = true;
            }
            if child_ui_instance.get_changed_layout() || child_ui_instance.get_changed_deep_child_layout() {
                self._changed_deep_child_layout = true;
            }
            child_index -= 1;
        }

        if false == *touch_event && self.get_touchable() {
            if self._touched {
                if mouse_input_data._btn_l_hold {
                    if mouse_moved {
                        self.on_touch_move(mouse_pos, mouse_pos_delta);
                    }
                } else {
                    self.on_touch_up(mouse_pos, mouse_pos_delta);
                    if false == self.get_has_cursor() {
                        //self._viewport_manager.focused_widget = None;
                    }
                }
            }
            else if mouse_input_data._btn_l_pressed {
                if self.check_collide(mouse_pos) {
                    //self._viewport_manager.focused_widget = self
                    self.on_touch_down(mouse_pos, mouse_pos_delta);
                } else if self.get_has_cursor() {
                    //self._viewport_manager.focused_widget = None
                }
            }
        }

        if self._touched {
            *touch_event = true;
        }
    }
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

impl WidgetDefault {
    fn create_widget(widget_name: &str) -> Rc<dyn Widget> {
        let widget = Rc::new(WidgetDefault {
            _ui_widget_name: String::from(widget_name),
            _ui_widget_type: UIWidgetTypes::Default,
            _ui_component: UIComponentInstance::create_ui_component(),
            _parent: std::ptr::null() as *const WidgetDefault,
            _widgets: Vec::new(),
        });
        ptr_as_mut(widget.as_ref())._ui_component._owner_widget = widget.as_ref();
        widget
    }
}

impl Widget for WidgetDefault {
    fn get_ui_widget_name(&self) -> &String { &self._ui_widget_name }
    fn get_ui_widget_type(&self) -> UIWidgetTypes {
        self._ui_widget_type
    }
    fn has_cursor(&self) -> bool { false }
    fn get_ui_component(&self) -> &UIComponentInstance { &self._ui_component }
    fn get_ui_component_mut(&mut self) -> &mut UIComponentInstance { &mut self._ui_component }
    fn get_changed_layout(&self) -> bool { self._ui_component._changed_layout }
    fn clear_parent(&mut self) {
        self._parent = std::ptr::null() as *const WidgetDefault;
        self._ui_component._parent = std::ptr::null();
    }
    fn has_parent(&self) -> bool { false == self._parent.is_null() }
    fn get_parent(&self) -> &dyn Widget { ptr_as_ref(self._parent) }
    fn set_parent(&mut self, widget: *const dyn Widget) {
        if self.has_parent() {
            ptr_as_mut(self._parent).remove_widget(self);
        }
        self._parent = widget;
        self._ui_component._parent = ptr_as_ref(widget).get_ui_component();
        self._ui_component.set_changed_child_layout(true);
        self._ui_component.set_changed_layout(true);
    }
    fn add_widget(&mut self, widget: &Rc<dyn Widget>) {
        let widget_instance = ptr_as_mut(widget.as_ref());
        widget_instance.set_parent(self);
        self._widgets.push(widget.clone());
        self._ui_component._children.push(widget_instance.get_ui_component());
        self._ui_component.set_changed_child_layout(true);
        self._ui_component.set_changed_layout(true);
    }
    fn remove_widget(&mut self, widget: *const dyn Widget) {
        for (i, child_widget) in self._widgets.iter().enumerate() {
            if child_widget.as_ref() as *const dyn Widget == widget {
                let widget_instance = ptr_as_mut(widget);
                widget_instance.clear_parent();
                widget_instance.clear_widgets();
                self._widgets.remove(i);
                self._ui_component._children.remove(i);
                self._ui_component.set_changed_child_layout(true);
                self._ui_component.set_changed_layout(true);
                return;
            }
        }
    }
    fn clear_widgets(&mut self) {
        for child_widget in self._widgets.iter() {
            ptr_as_mut(child_widget.as_ref()).clear_widgets();
        }
        self._parent = std::ptr::null() as *const WidgetDefault;
        self._widgets.clear();
        self._ui_component._parent = std::ptr::null();
        self._ui_component._children.clear();
        self._ui_component.set_changed_child_layout(true);
        self._ui_component.set_changed_layout(true);
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
    pub fn get_root_ptr(&self) -> *const dyn Widget { self._root.as_ref() }
    pub fn create_ui_manager(project_ui_manager: *const dyn ProjectUIManagerBase) -> UIManager {
        log::info!("create_ui_manager");
        unsafe {
            let mut ui_manager = UIManager {
                _project_ui_manager: project_ui_manager,
                _root: UIManager::create_widget("root", UIWidgetTypes::Default),
                _window_size: Vector2::zeros(),
                _ui_mesh_vertex_buffer: BufferData::default(),
                _ui_mesh_index_buffer: BufferData::default(),
                _ui_mesh_index_count: 0,
                _font_data: system::newRcRefCell(FontData::default()),
                _ui_render_datas: Vec::new(),
                _render_ui_count: 0,
                _render_ui_group: Vec::new(),
                _default_render_ui_material: None,
            };
            ui_manager._ui_render_datas.resize(constants::MAX_UI_INSTANCE_COUNT, UIRenderData::default());
            let ui_component = ptr_as_mut(ui_manager._root.as_ref()).get_ui_component_mut();
            ui_component.set_layout_type(UILayoutType::FloatLayout);
            ui_component.set_size_hint_x(Some(1.0));
            ui_component.set_size_hint_y(Some(1.0));
            ui_component.set_renderable(false);
            ui_manager
        }
    }

    pub fn initialize_ui_manager(&mut self, renderer_context: &RendererContext, engine_resources: &EngineResources) {
        self._font_data = engine_resources.get_default_font_data().clone();
        self.create_ui_vertex_data(renderer_context.get_device(), renderer_context.get_command_pool(), renderer_context.get_graphics_queue(), renderer_context.get_device_memory_properties());
        self.create_ui_graphics_data(engine_resources);
        self.get_project_ui_manager_mut().initialize_project_ui_manager(&self);
        self.get_project_ui_manager_mut().build_ui(renderer_context, engine_resources);
    }

    pub fn create_ui_graphics_data(&mut self, engine_resources: &EngineResources) {
        self._default_render_ui_material = Some(engine_resources.get_material_instance_data("ui/render_ui").clone());
    }

    pub fn destroy_ui_graphics_data(&mut self) {
        self._default_render_ui_material = None;
    }

    pub fn get_project_ui_manager(&self) -> &dyn ProjectUIManagerBase {
        unsafe { &*self._project_ui_manager }
    }

    pub fn get_project_ui_manager_mut(&self) -> &mut dyn ProjectUIManagerBase {
        unsafe { &mut *(self._project_ui_manager as *mut dyn ProjectUIManagerBase) }
    }

    pub fn destroy_ui_manager(&mut self, device: &Device) {
        log::info!("destroy_ui_manager");
        ptr_as_mut(self._root.as_ref()).clear_widgets();
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

    pub fn create_widget(widget_name: &str, widget_type: UIWidgetTypes) -> Rc<dyn Widget> {
        match widget_type {
            UIWidgetTypes::Default => WidgetDefault::create_widget(widget_name),
        }
    }

    pub fn render_ui(
        &mut self,
        command_buffer: vk::CommandBuffer,
        swapchain_index: u32,
        renderer_context: &RendererContext,
        engine_resources: &EngineResources
    ) {
        if 0 < self._render_ui_count {
            let framebuffer_data = engine_resources.get_framebuffer_data("render_ui").borrow();
            let mut push_constant_data = PushConstant_RenderUI {
                _inv_canvas_size: Vector2::new(1.0 / framebuffer_data._framebuffer_info._framebuffer_width as f32, 1.0 / framebuffer_data._framebuffer_info._framebuffer_height as f32),
                _instance_id_offset: 0,
                _reserved0: 0,
            };

            // upload storage buffer
            let upload_data = &self._ui_render_datas[0..self._render_ui_count as usize];
            let shader_buffer_data = renderer_context.get_shader_buffer_data_from_str("UIRenderDataBuffer");
            renderer_context.upload_shader_buffer_datas(command_buffer, swapchain_index, shader_buffer_data, upload_data);

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
                                renderer_context.end_render_pass(command_buffer);
                            }
                            renderer_context.begin_render_pass_pipeline(command_buffer, swapchain_index, &render_pass_data, pipeline_data, None);
                            prev_pipeline_data = pipeline_data_ptr;
                        }

                        if prev_pipeline_binding_data != pipeline_binding_data {
                            prev_pipeline_binding_data = pipeline_binding_data;
                            let render_ui_descriptor_sets = Some(&pipeline_binding_data._descriptor_sets);
                            renderer_context.bind_descriptor_sets(command_buffer, swapchain_index, &(*pipeline_binding_data), render_ui_descriptor_sets);
                        }
                        prev_material_instance_data = material_instance_data;
                    }

                    renderer_context.upload_push_constant_data(command_buffer, &(*prev_pipeline_data), &push_constant_data);
                    renderer_context.draw_indexed(
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
            renderer_context.end_render_pass(command_buffer);
        }
    }

    pub fn update(
        &mut self,
        delta_time: f64,
        window_size: &Vector2<i32>,
        time_data: &TimeData,
        keyboard_input_data: &KeyboardInputData,
        mouse_move_data: &MouseMoveData,
        mouse_input_data: &MouseInputData,
        _engine_resources: &EngineResources
    ) {
        let root_ui_component = ptr_as_mut(self._root.as_ref()).get_ui_component_mut();
        if *window_size != self._window_size {
            self._window_size = window_size.clone() as Vector2<i32>;
            root_ui_component.set_changed_layout(true);
        }

        // update ui component
        let mut touch_event: bool = false;
        let mouse_pos: Vector2<f32> = Vector2::new(mouse_move_data._mouse_pos.x as f32, mouse_move_data._mouse_pos.y as f32);
        let mouse_pos_delta: Vector2<f32> = Vector2::new(mouse_move_data._mouse_pos_delta.x as f32, mouse_move_data._mouse_pos_delta.y as f32);
        let mouse_moved: bool = 0 != mouse_move_data._mouse_pos_delta.x || 0 != mouse_move_data._mouse_pos_delta.y;
        root_ui_component.update_ui_component(
            delta_time,
            window_size,
            time_data,
            keyboard_input_data,
            &mouse_pos,
            &mouse_pos_delta,
            mouse_moved,
            mouse_input_data,
            &mut touch_event,
        );

        // updatge ui layout
        let contents_area = Vector4::new(0.0, 0.0, window_size.x as f32, window_size.y as f32);
        let contents_area_size = Vector2::new(window_size.x as f32, window_size.y as f32);
        let required_contents_size = Vector2::<f32>::zeros();
        let mut child_ui_pos = Vector2::<f32>::zeros();
        let inherit_changed_layout: bool = false;
        let update_depth: u32 = 0;

        if root_ui_component.get_changed_layout() || root_ui_component.get_changed_deep_child_layout() {
            root_ui_component.update_layout_size(inherit_changed_layout, &contents_area_size, &self._font_data.borrow());
            root_ui_component.update_layout_area(
                UILayoutType::FloatLayout,
                Orientation::HORIZONTAL,
                DEFAILT_HORIZONTAL_ALIGN,
                DEFAILT_VERTICAL_ALIGN,
                &contents_area,
                &contents_area_size,
                &required_contents_size,
                &contents_area,
                &mut child_ui_pos,
                update_depth
            );
            root_ui_component.update_layout(inherit_changed_layout, update_depth, &self._font_data.borrow());
        }

        // collect_ui_render_data
        let font_data = self._font_data.borrow();
        let need_to_collect_render_data: bool = false;
        let opacity: f32 = 1.0;
        let mut render_ui_count: u32 = 0;
        let mut render_ui_group: Vec<UIRenderGroupData> = Vec::new();
        let mut prev_render_group_data = UIRenderGroupData {
            _accumulated_render_count: 0,
            _material_instance: std::ptr::null(),
        };

        root_ui_component.collect_ui_render_data(
            &font_data,
            &mut render_ui_count,
            &mut render_ui_group,
            &mut prev_render_group_data,
            &mut self._ui_render_datas,
            need_to_collect_render_data,
            opacity
        );

        // last render count
        if 0 < render_ui_count {
            UIRenderGroupData::add_ui_render_group_data(&mut render_ui_group, render_ui_count, &mut prev_render_group_data, std::ptr::null());
        }
        self._render_ui_count = render_ui_count;
        self._render_ui_group = render_ui_group;
    }
}