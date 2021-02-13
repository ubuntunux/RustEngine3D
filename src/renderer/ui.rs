use nalgebra::{ Vector2, Vector4, Matrix4 };

use crate::utilities::system::{ RcRefCell };
use crate::renderer::transform_object::TransformObjectData;
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::get_color32;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum HorizontalAlign {
    LEFT,
    CENTER,
    RIGHT,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum VerticalAlign {
    BOTTOM,
    CENTER,
    TOP,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Orientation {
    HORIZONTAL,
    VERTICAL,
}

pub struct UIComponent {
    pub _owner_widget: Option<*mut dyn Widget>,
    pub _parent: Option<*mut UIComponent>,
    pub _children: Vec<*mut UIComponent>,
    pub _changed_layout: bool,
    pub _transform: TransformObjectData,
    pub _world_to_local_matrix: Matrix4<f32>,
    pub _local_to_world_matrix: Matrix4<f32>,
    pub _size: Vector2<f32>,
    pub _hpivot: HorizontalAlign,
    pub _vpivot: VerticalAlign,
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

// interfaces

impl UIComponent {
    pub fn create_ui_component() -> UIComponent {
        UIComponent {
            _owner_widget: None,
            _parent: None,
            _children: Vec::new(),
            _changed_layout: false,
            _transform: TransformObjectData::new_transform_object_data(),
            _world_to_local_matrix: Matrix4::identity(),
            _local_to_world_matrix: Matrix4::identity(),
            _size: Vector2::new(100.0, 100.0),
            _hpivot: HorizontalAlign::LEFT,
            _vpivot: VerticalAlign::TOP,
            _halign: HorizontalAlign::LEFT,
            _valign: VerticalAlign::TOP,
            _pos_hint_x: None,
            _pos_hint_y: None,
            _size_hint_x: None,
            _size_hint_y: None,
            _padding: Vector4::zeros(),
            _margine: Vector4::zeros(),
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

    pub fn set_text(&mut self, text: String, font_size: u32, halign: HorizontalAlign, valign: VerticalAlign) {
        // if self.label is None:
        //     self.label = Label(halign=halign, valign=valign)
        //     self.add_widget(self.label)
        // self.label.set_text(text, font_size, halign=halign, valign=valign)
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

    pub fn on_touch_move(&mut self, x: f32, y: f32) {
        if self._touched {
        //     if self._dragable:
        //         self._x = x + self._touch_offset_x
        //         self._y = y + self._touch_offset_y
        //
        //         if self._callback_touch_move is not None:
        //             self._callback_touch_move(self, x, y)
        }
    }

    pub fn on_touch_up(&mut self, x: f32, y: f32) {
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

    pub fn set_pos_x(&mut self, x: f32) {
        if x != self._transform._position.x {
            self._pos_hint_x = None;
            self._transform._position.x = x;
            self._changed_layout = true;
        }
    }

    pub fn set_pos_y(&mut self, y: f32) {
        if y != self._transform._position.y {
            self._pos_hint_y = None;
            self._transform._position.y = y;
            self._changed_layout = true;
        }
    }

    pub fn set_pos_hint_x(&mut self, pos_hint_x: Option<f32>) {
        if pos_hint_x.is_some() && pos_hint_x != self._pos_hint_x {
            self._changed_layout = true;
        }
        self._pos_hint_x = pos_hint_x;
    }

    pub fn set_pos_hint_y(&mut self, pos_hint_y: Option<f32>) {
        if pos_hint_y.is_some() && pos_hint_y != self._pos_hint_y {
            self._changed_layout = true;
        }
        self._pos_hint_y = pos_hint_y;
    }

    pub fn set_size_x(&mut self, size_x: f32) {
        if size_x != self._size.x {
            self._size_hint_x = None;
            self._size.x = size_x;
            self._changed_layout = true;
        }
    }

    pub fn set_size_y(&mut self, size_y: f32) {
        if size_y != self._size.y {
            self._size_hint_y = None;
            self._size.y = size_y;
            self._changed_layout = true;
        }
    }

    pub fn set_size_hint_x(&mut self, size_hint_x: Option<f32>) {
        if size_hint_x.is_some() && size_hint_x != self._size_hint_x {
            self._changed_layout = true;
        }
        self._size_hint_x = size_hint_x;
    }

    pub fn set_size_hint_y(&mut self, size_hint_y: Option<f32>) {
        if size_hint_y.is_some() && size_hint_y != self._size_hint_y {
            self._changed_layout = true;
        }
        self._size_hint_y = size_hint_y;
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
    pub fn get_margine_left(&mut self) { self._margine[0]; }
    pub fn get_margine_right(&mut self) { self._margine[1]; }
    pub fn get_margine_top(&mut self) { self._margine[2]; }
    pub fn get_margine_bottom(&mut self) { self._margine[3]; }
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
    pub fn get_padding_left(&mut self) { self._padding[0]; }
    pub fn get_padding_right(&mut self) { self._padding[1]; }
    pub fn get_padding_top(&mut self) { self._padding[2]; }
    pub fn get_padding_bottom(&mut self) { self._padding[3]; }
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

    fn get_changed_layout(&self) -> bool { self._changed_layout }
    fn get_owner_widget(&self) -> &Option<*mut dyn Widget> { &self._owner_widget }
    fn set_owner_widget(&mut self, owner: Option<*mut dyn Widget>) {
        if self._owner_widget.is_some() {
            panic!("Widget already has owner widget");
        }
        self._owner_widget = owner;
    }
    fn get_parent(&self) -> &Option<*mut UIComponent> { &self._parent }
    fn set_parent(&mut self, parent: Option<*mut UIComponent>) {
        if self._parent.is_some() {
            panic!("Widget already has parent");
        }
        self._parent = parent;
    }
    fn clear_children(&mut self) {
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
    fn add_ui_component(&mut self, child: *mut UIComponent) {
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
    fn remove_ui_component(&mut self, child: *mut UIComponent) {
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
    fn update_layout(&mut self, changed_layout: bool, recursive: bool) {
        unsafe {
            let changed_layout = self._changed_layout || changed_layout;
            if changed_layout {

                self._changed_layout = false;
            }

            if recursive {
                for child in self._children.iter() {
                    child.as_mut().unwrap().update_layout(changed_layout, recursive);
                }
            }
        }
    }
    fn update(&mut self, delta_time: f32, touch_event: bool) {
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