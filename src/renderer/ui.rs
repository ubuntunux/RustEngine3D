use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector2, Vector3, Vector4, Matrix4 };
use ash::{ vk, Device };

use crate::resource::Resources;
use crate::renderer::renderer::{ RendererData };
use crate::renderer::shader_buffer_datas::ShaderBufferDataType;
use crate::renderer::transform_object::TransformObjectData;
use crate::utilities::system::{ RcRefCell };
use crate::vulkan_context::buffer::{ self, BufferData };
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::geometry_buffer::{ self, VertexData };
use crate::vulkan_context::vulkan_context::{ get_color32 };

// MAX_UI_INSTANCE_COUNT must match with render_ui_common.glsl
pub const MAX_UI_INSTANCE_COUNT: u32 = 1024;
pub const DEFAILT_HORIZONTAL_ALIGN: HorizontalAlign = HorizontalAlign::LEFT;
pub const DEFAILT_VERTICAL_ALIGN: VerticalAlign = VerticalAlign::TOP;

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
pub struct UIInstanceData {
    pub _ui_texcoord: Vector4<f32>,
    pub _ui_pos: Vector2<f32>,
    pub _ui_size: Vector2<f32>,
    pub _ui_color: u32,
    pub _ui_round: f32,
    pub _ui_border: f32,
    pub _ui_border_color: u32,
}

impl Default for UIInstanceData {
    fn default() -> UIInstanceData {
        UIInstanceData {
            _ui_texcoord: Vector4::new(0.0, 0.0, 1.0, 1.0),
            _ui_pos: Vector2::zeros(),
            _ui_size: Vector2::zeros(),
            _ui_color: 0xFFFFFFFF,
            _ui_round: 0.0,
            _ui_border: 0.0,
            _ui_border_color: 0x00000000,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub struct PushConstant_RenderUI {
    pub _inv_canvas_size: Vector2<f32>,
    pub _reserved0: u32,
    pub _reserved1: u32,
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
    Float,
    Boxlayout,
}

pub struct UIComponent {
    pub _owner_widget: Option<*mut dyn Widget>,
    pub _parent: Option<*mut UIComponent>,
    pub _children: Vec<*mut UIComponent>,
    pub _layout_type: UILayoutType,
    pub _layout_orientation: Orientation,
    pub _changed_layout: bool,
    pub _updated_layout: bool,
    pub _render_ui_index: u32,
    pub _transform: TransformObjectData,
    pub _world_to_local_matrix: Matrix4<f32>,
    pub _local_to_world_matrix: Matrix4<f32>,
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
    pub _visible: bool,
    pub _touched: bool,
    pub _dragable: bool,
    pub _touchable: bool,
    pub _touched_offset: Vector2<f32>,
    pub _color: u32,
    pub _pressed_color: u32,
    pub _round: f32,
    pub _border: f32,
    pub _border_color: u32,
    pub _texture: Option<RcRefCell<TextureData>>,
    pub _callback_touch_down: Option<Box<fn()>>,
    pub _callback_touch_move: Option<Box<fn()>>,
    pub _callback_touch_up: Option<Box<fn()>>,
}

pub struct WidgetDefault {
    pub _ui_component: UIComponent,
    pub _parent: Option<*mut dyn Widget>,
    pub _widgets: Vec<*mut dyn Widget>,
}

pub struct UIManager {
    pub _root: WidgetDefault,
    pub _ui_mesh_vertex_buffer: BufferData,
    pub _ui_mesh_index_buffer: BufferData,
    pub _ui_mesh_index_count: u32,
    pub _render_ui_instance_datas: [UIInstanceData; MAX_UI_INSTANCE_COUNT as usize],
    pub _render_ui_count: u32,
}

// interfaces

impl UIComponent {
    pub fn create_ui_component() -> UIComponent {
        UIComponent {
            _owner_widget: None,
            _parent: None,
            _children: Vec::new(),
            _changed_layout: true,
            _updated_layout: true,
            _render_ui_index: std::u32::MAX,
            _layout_type: UILayoutType::Boxlayout,
            _layout_orientation: Orientation::HORIZONTAL,
            _transform: TransformObjectData::new_transform_object_data(),
            _world_to_local_matrix: Matrix4::identity(),
            _local_to_world_matrix: Matrix4::identity(),
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
            _visible: true,
            _touched: false,
            _dragable: false,
            _touchable: false,
            _touched_offset: Vector2::zeros(),
            _color: get_color32(255, 255, 255, 255),
            _pressed_color: get_color32(255, 255, 255, 255),
            _texture: None,
            _callback_touch_down: None,
            _callback_touch_move: None,
            _callback_touch_up: None,
        }
    }

    pub fn check_collide(&self, x: f32, y: f32) -> bool {
        let collide_pos = &self._world_to_local_matrix * &Vector4::new(x, y, 0.0, 1.0);
        let pos = self._transform.get_position();
        let half_size = &self._size * 0.5;
        (pos.x - half_size.x) < collide_pos.x && collide_pos.x < (pos.x + half_size.x) && (pos.y - half_size.y) < collide_pos.y && collide_pos.y < (pos.y + half_size.y)
    }

    pub fn on_touch_down(&mut self, x: f32, y: f32) {
        self._touched = true;
        if self._dragable {
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

    pub fn set_pos(&mut self, x: f32, y: f32) {
        self.set_pos_x(x);
        self.set_pos_y(y);
    }

    pub fn set_pos_x(&mut self, x: f32) {
        if x != self._pos.x || self._pos_hint_x.is_some() {
            self._pos_hint_x = None;
            self._pos.x = x;
            self._changed_layout = true;
        }
    }

    pub fn set_pos_y(&mut self, y: f32) {
        if y != self._pos.y || self._pos_hint_y.is_some() {
            self._pos_hint_y = None;
            self._pos.y = y;
            self._changed_layout = true;
        }
    }

    pub fn set_pos_hint_x(&mut self, pos_hint_x: Option<f32>) {
        if pos_hint_x != self._pos_hint_x {
            self._pos_hint_x = pos_hint_x;
            self._changed_layout = true;
        }
    }

    pub fn set_pos_hint_y(&mut self, pos_hint_y: Option<f32>) {
        if pos_hint_y != self._pos_hint_y {
            self._pos_hint_y = pos_hint_y;
            self._changed_layout = true;
        }
    }

    pub fn set_size(&mut self, size_x: f32, size_y: f32) {
        self.set_size_x(size_x);
        self.set_size_y(size_y);
    }

    pub fn set_size_x(&mut self, size_x: f32) {
        if size_x != self._size.x || self._size_hint_x.is_some() {
            self._size_hint_x = None;
            self._size.x = size_x;
            self._changed_layout = true;
        }
    }

    pub fn set_size_y(&mut self, size_y: f32) {
        if size_y != self._size.y || self._size_hint_y.is_some() {
            self._size_hint_y = None;
            self._size.y = size_y;
            self._changed_layout = true;
        }
    }

    pub fn set_size_hint_x(&mut self, size_hint_x: Option<f32>) {
        if size_hint_x != self._size_hint_x {
            self._size_hint_x = size_hint_x;
            self._changed_layout = true;
        }
    }

    pub fn set_size_hint_y(&mut self, size_hint_y: Option<f32>) {
        if size_hint_y != self._size_hint_y {
            self._size_hint_y = size_hint_y;
            self._changed_layout = true;
        }
    }

    pub fn set_margine(&mut self, margine: Vector4<f32>) {
        if margine != self._margine {
            self._margine = margine;
            self._changed_layout = true;
        }
    }
    fn set_margine_inner(&mut self, index: usize, margine: f32) {
        if margine != self._margine[index] {
            self._margine[index] = margine;
            self._changed_layout = true;
        }
    }
    pub fn get_margine_left(&mut self) -> f32 { self._margine[0] }
    pub fn get_margine_right(&mut self) -> f32 { self._margine[1] }
    pub fn get_margine_top(&mut self) -> f32 { self._margine[2] }
    pub fn get_margine_bottom(&mut self) -> f32 { self._margine[3] }
    pub fn set_margine_left(&mut self, margine: f32) { self.set_margine_inner(0, margine); }
    pub fn set_margine_right(&mut self, margine: f32) { self.set_margine_inner(1, margine); }
    pub fn set_margine_top(&mut self, margine: f32) { self.set_margine_inner(2, margine); }
    pub fn set_margine_bottom(&mut self, margine: f32) { self.set_margine_inner(3, margine); }

    pub fn set_padding(&mut self, padding: Vector4<f32>) {
        if padding != self._padding {
            self._padding = padding;
            self._changed_layout = true;
        }
    }
    fn set_padding_inner(&mut self, index: usize, padding: f32) {
        if padding != self._padding[index] {
            self._padding[index] = padding;
            self._changed_layout = true;
        }
    }
    pub fn get_padding_left(&mut self) -> f32 { self._padding[0] }
    pub fn get_padding_right(&mut self) -> f32 { self._padding[1] }
    pub fn get_padding_top(&mut self) -> f32 { self._padding[2] }
    pub fn get_padding_bottom(&mut self) -> f32 { self._padding[3] }
    pub fn set_padding_left(&mut self, padding: f32) { self.set_padding_inner(0, padding); }
    pub fn set_padding_right(&mut self, padding: f32) { self.set_padding_inner(1, padding); }
    pub fn set_padding_top(&mut self, padding: f32) { self.set_padding_inner(2, padding); }
    pub fn set_padding_bottom(&mut self, padding: f32) { self.set_padding_inner(3, padding); }

    pub fn set_halign(&mut self, halign: HorizontalAlign) {
        if halign != self._halign {
            self._halign = halign;
            self._changed_layout = true;
        }
    }

    pub fn set_valign(&mut self, valign: VerticalAlign) {
        if valign != self._valign {
            self._valign = valign;
            self._changed_layout = true;
        }
    }

    pub fn get_changed_layout(&self) -> bool { self._changed_layout }
    pub fn get_owner_widget(&self) -> &Option<*mut dyn Widget> { &self._owner_widget }
    pub fn set_owner_widget(&mut self, owner: Option<*mut dyn Widget>) {
        if self._owner_widget.is_some() {
            panic!("Widget already has owner widget");
        }
        self._owner_widget = owner;
    }
    pub fn get_parent(&self) -> &Option<*mut UIComponent> { &self._parent }
    pub fn set_parent(&mut self, parent: Option<*mut UIComponent>) {
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
    pub fn get_visible(&mut self) -> bool { self._visible }
    pub fn set_visible(&mut self, visible: bool) {
        self._visible = visible;
        self._updated_layout = true;
    }
    pub fn set_color(&mut self, color: u32) {
        if self._color != color {
            self._color = color;
            self._updated_layout = true;
        }
    }
    pub fn set_round(&mut self, round: f32) {
        if round != self._round {
            self._round = round;
            self._updated_layout = true;
        }
    }
    pub fn set_border(&mut self, border: f32) {
        if border != self._border {
            self._border = border;
            self._updated_layout = true;
        }
    }

    pub fn add_ui_component(&mut self, child: *mut UIComponent) {
        unsafe {
            if child.as_ref().unwrap().get_parent().is_some() {
                panic!("Widget already has parent");
            }

            if false == self._children.contains(&child) {
                self._children.push(child);
                child.as_mut().unwrap().set_parent(Some(self));
                let recursive = true;
                let changed_layout = true;
                self.update_layout(changed_layout, recursive);
            }
        }
    }

    pub fn remove_ui_component(&mut self, child: *mut UIComponent) {
        unsafe {
            if let Some(index) = self._children.iter().position(|x| *x == child) {
                // if self._viewport_manager.focused_widget is widget {
                //     self._viewport_manager.focused_widget = None
                // }
                self._children.remove(index);
                child.as_mut().unwrap().set_parent(None);
                let recursive = true;
                let changed_layout = true;
                self.update_layout(changed_layout, recursive);
            }
        }
    }

    pub fn collect_ui_render_data(&mut self, render_ui_count: &mut u32, render_ui_instance_datas: &mut [UIInstanceData]) {
        if self._visible {
            let render_ui_index = *render_ui_count;
            *render_ui_count += 1;

            if self._updated_layout || render_ui_index != self._render_ui_index {
                let pos = self._transform.get_position();
                let render_ui_instance_data = &mut render_ui_instance_datas[render_ui_index as usize];
                render_ui_instance_data._ui_texcoord = self._texcoord.into();
                render_ui_instance_data._ui_pos.x = pos.x;
                render_ui_instance_data._ui_pos.y = pos.y;
                render_ui_instance_data._ui_size.x = self._size.x;
                render_ui_instance_data._ui_size.y = self._size.y;
                render_ui_instance_data._ui_color = self._color;
                render_ui_instance_data._ui_round = self._round;
                render_ui_instance_data._ui_border = self._border;
                render_ui_instance_data._ui_border_color = self._border_color;

                self._render_ui_index = render_ui_index;
                self._updated_layout = false;
            }
        }

        for ui_component in self._children.iter() {
            unsafe {
                ui_component.as_mut().unwrap().collect_ui_render_data(render_ui_count, render_ui_instance_datas);
            }
        }
    }

    fn update_layout(&mut self, changed_layout: bool, recursive: bool) {
        unsafe {
            let changed_layout = self._changed_layout || changed_layout;
            if changed_layout {
                let parent: *mut UIComponent = if self._parent.is_some() { *self._parent.as_ref().unwrap() } else { std::ptr::null_mut() };
                let halign: HorizontalAlign = if parent.is_null() { DEFAILT_HORIZONTAL_ALIGN } else { parent.as_ref().unwrap()._halign };
                let valign: VerticalAlign = if parent.is_null() { DEFAILT_VERTICAL_ALIGN } else { parent.as_ref().unwrap()._valign };
                let half_size = &self._size * 0.5;
                let mut pos = Vector3::new(self._pos.x, self._pos.y, 0.0);
                pos.x += self.get_margine_left();
                pos.y += self.get_margine_top();
                match halign {
                    HorizontalAlign::LEFT => pos.x += half_size.x,
                    HorizontalAlign::CENTER => (),
                    HorizontalAlign::RIGHT => pos.x -= half_size.x,
                }
                match valign {
                    VerticalAlign::TOP => pos.y += half_size.y,
                    VerticalAlign::CENTER => (),
                    VerticalAlign::BOTTOM => pos.y -= half_size.y,
                }

                self._transform.set_position(&pos);

                self._updated_layout = true;
                self._changed_layout = false;
            }

            if recursive {
                for child in self._children.iter() {
                    child.as_mut().unwrap().update_layout(changed_layout, recursive);
                }
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
    fn has_cursor(&self) -> bool;
    fn get_ui_component(&self) -> &UIComponent;
    fn get_changed_layout(&self) -> bool;
}

impl WidgetDefault {
    pub fn create_widget() -> WidgetDefault {
        let mut widget = WidgetDefault {
            _ui_component: UIComponent::create_ui_component(),
            _parent: None,
            _widgets: Vec::new(),
        };
        widget._ui_component._owner_widget = Some(&mut widget);
        widget
    }
}

impl Widget for WidgetDefault {
    fn has_cursor(&self) -> bool { false }
    fn get_ui_component(&self) -> &UIComponent { &self._ui_component }
    fn get_changed_layout(&self) -> bool { self._ui_component._changed_layout }
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

impl UIManager {
    pub fn create_ui_manager() -> UIManager {
        log::info!("create_ui_manager");
        UIManager {
            _root: WidgetDefault::create_widget(),
            _ui_mesh_vertex_buffer: BufferData::default(),
            _ui_mesh_index_buffer: BufferData::default(),
            _ui_mesh_index_count: 0,
            _render_ui_instance_datas: [UIInstanceData::default(); MAX_UI_INSTANCE_COUNT as usize],
            _render_ui_count: 0,
        }
    }

    pub fn initialize_ui_manager(&mut self, renderer_data: &RendererData, _resources: &Resources) {
        self.create_ui_vertex_data(renderer_data.get_device(), renderer_data.get_command_pool(), renderer_data.get_graphics_queue(), renderer_data.get_device_memory_properties());
    }

    pub fn create_ui_descriptor_sets(&mut self, _renderer_data: &RendererData, _resources: &Resources) {
    }

    pub fn destroy_ui_descriptor_sets(&mut self) {
    }

    pub fn destroy_ui_manager(&mut self, device: &Device) {
        log::info!("destroy_ui_manager");
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
        let positions: Vec<Vector3<f32>> = vec![Vector3::new(-0.5, -0.5, 0.0), Vector3::new(0.5, -0.5, 0.0), Vector3::new(0.5, 0.5, 0.0), Vector3::new(-0.5, 0.5, 0.0)];
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

    pub fn collect_ui_render_data(&mut self) {
        let mut render_ui_count: u32 = 0;
        self._root._ui_component.collect_ui_render_data(&mut render_ui_count, &mut self._render_ui_instance_datas);
        self._render_ui_count = render_ui_count;
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
            let material_instance_data = resources.get_material_instance_data("render_ui").borrow();
            let pipeline_binding_data = material_instance_data.get_default_pipeline_binding_data();
            let render_pass_data = &pipeline_binding_data.get_render_pass_data().borrow();
            let pipeline_data = &pipeline_binding_data.get_pipeline_data().borrow();
            let render_ui_framebuffer_data = None;
            let render_ui_descriptor_sets = None;
            let push_constant_data = PushConstant_RenderUI {
                _inv_canvas_size: Vector2::new(1.0 / framebuffer_data._framebuffer_info._framebuffer_width as f32, 1.0 / framebuffer_data._framebuffer_info._framebuffer_height as f32),
                _reserved0: 0,
                _reserved1: 0,
            };

            // upload storage buffer
            let upload_data = &self._render_ui_instance_datas[0..self._render_ui_count as usize];
            renderer_data.upload_shader_buffer_datas(command_buffer, swapchain_index, ShaderBufferDataType::UIInstanceDataBuffer, upload_data);

            // render ui
            renderer_data.begin_render_pass_pipeline(command_buffer, swapchain_index, render_pass_data, pipeline_data, render_ui_framebuffer_data);
            renderer_data.bind_descriptor_sets(command_buffer, swapchain_index, pipeline_binding_data, render_ui_descriptor_sets);
            renderer_data.upload_push_constant_data(command_buffer, pipeline_data, &push_constant_data);
            renderer_data.draw_elements(
                command_buffer,
                &[self._ui_mesh_vertex_buffer._buffer],
                &[],
                self._render_ui_count,
                self._ui_mesh_index_buffer._buffer,
                self._ui_mesh_index_count,
            );
            renderer_data.end_render_pass(command_buffer);
        }
    }

    pub fn update(&mut self, delta_time: f64) {
        let changed_layout: bool = false;
        let recursive: bool = true;
        let touch_evemt: bool = false;

        self._root._ui_component.set_margine(Vector4::new(100.0, 100.0, 100.0, 100.0));
        self._root._ui_component.set_pos(50.0, 50.0);
        self._root._ui_component.set_size(200.0, 100.0);
        self._root._ui_component.set_color(get_color32(255, 255, 0, 255));
        self._root._ui_component.set_round(5.0);
        self._root._ui_component.set_border(2.0);

        self._root._ui_component.update_layout(changed_layout, recursive);
        self._root._ui_component.update(delta_time, touch_evemt);
        self.collect_ui_render_data();
    }
}