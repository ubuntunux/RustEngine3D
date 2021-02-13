
use nalgebra::{ Vector4 };

use crate::utilities::system::RcRefCell;
use crate::vulkan_context::texture::TextureData;
use crate::vulkan_context::vulkan_context::get_color32;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum HorizontalAlign {
    NONE,
    CENTER,
    LEFT,
    RIGHT,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum VerticalAlign {
    NONE,
    CENTER,
    TOP,
    BOTTOM,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Orientation {
    HORIZONTAL,
    VERTICAL,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UIComponentState {
    NONE,
    VISIBLE,
    TOUCHED,
}

pub struct UIComponent {
    pub _parent: Option<RcRefCell<dyn Widget>>,
    pub _changed_layout: bool,
    pub _x: f32,
    pub _y: f32,
    pub _center_x: f32,
    pub _center_y: f32,
    pub _width: f32,
    pub _height: f32,
    pub _halign: HorizontalAlign,
    pub _valign: VerticalAlign,
    pub _pos_hint_x: Option<f32>,
    pub _pos_hint_y: Option<f32>,
    pub _size_hint_x: Option<f32>,
    pub _size_hint_y: Option<f32>,
    pub _padding: Vector4<f32>,
    pub _margine: Vector4<f32>,
    pub _spacing: f32,
    pub _texcoord: Vector4<f32>,
    pub _opacity: f32,
    pub _state: UIComponentState,
    pub _dragable: bool,
    pub _touchable: bool,
    pub _touch_offset_x: f32,
    pub _touch_offset_y: f32,
    pub _color: u32,
    pub _pressed_color: u32,
    pub _texture: Option<RcRefCell<TextureData>>,
    pub _callback_touch_down: Option<Box<fn()>>,
    pub _callback_touch_move: Option<Box<fn()>>,
    pub _callback_touch_up: Option<Box<fn()>>,
}

impl UIComponent {
    pub fn create_ui_component() -> UIComponent {
        UIComponent {
            _parent: None,
            _changed_layout: false,
            _x: 0.0,
            _y: 0.0,
            _center_x: 0.0,
            _center_y: 0.0,
            _width: 100.0,
            _height: 100.0,
            _halign: HorizontalAlign::NONE,
            _valign: VerticalAlign::NONE,
            _pos_hint_x: None,
            _pos_hint_y: None,
            _size_hint_x: None,
            _size_hint_y: None,
            _padding: Vector4::zeros(),
            _margine: Vector4::zeros(),
            _spacing: 0.0,
            _texcoord: Vector4::new(0.0, 0.0, 1.0, 1.0),
            _opacity: 1.0,
            _state: UIComponentState::NONE,
            _dragable: false,
            _touchable: false,
            _touch_offset_x: 0.0,
            _touch_offset_y: 0.0,
            _color: get_color32(255, 255, 255, 255),
            _pressed_color: get_color32(255, 255, 255, 255),
            _texture: None,
            _callback_touch_down: None,
            _callback_touch_move: None,
            _callback_touch_up: None,
        }
    }
}

pub struct WidgetDefault {
    pub _ui_component: UIComponent,
    pub _parent: Option<RcRefCell<dyn Widget>>,
    pub _children: Vec<RcRefCell<dyn Widget>>,
}

pub trait Widget {
    fn has_cursor(&self) -> bool;
    fn get_ui_component(&self) -> &UIComponent;
}

impl Widget for WidgetDefault {
    fn has_cursor(&self) -> bool { false }
    fn get_ui_component(&self) -> &UIComponent { &self._ui_component }
}



/*
class Widget:
    core_manager = None
    viewport_manager = None
    root = None
    haligns = (Align.LEFT, Align.CENTER, Align.RIGHT)
    valigns = (Align.TOP, Align.CENTER, Align.BOTTOM)
    orientations = (Orientation.HORIZONTAL, Orientation.VERTICAL)
    has_cursor = False

    def __init__(self, **kwargs):
        self.changed_layout = True
        self.parent = None
        self.widgets = []

        self._x = 0.0
        self._y = 0.0
        self._width = 100.0
        self._height = 100.0
        self._halign = ''
        self._valign = ''
        self._pos_hint_x = None
        self._pos_hint_y = None
        self._size_hint_x = None
        self._size_hint_y = None
        self._padding_x = 0.0
        self._padding_y = 0.0
        self._spacing = 0.0
        self._color = np.array(kwargs.get('color', [0.0, 0.0, 0.0, 0.0]), np.float32)
        self._pressed_color = np.array(kwargs.get('pressed_color', [0.0, 0.0, 0.0, 0.0]), np.float32)

        self.name = kwargs.get('name', '')
        self.x = kwargs.get('x', 0.0)
        self.y = kwargs.get('y', 0.0)
        self.width = kwargs.get('width', 100.0)
        self.height = kwargs.get('height', 100.0)
        self.halign = kwargs.get('halign', Align.NONE)
        self.valign = kwargs.get('valign', Align.NONE)
        self.pos_hint_x = kwargs.get('pos_hint_x')
        self.pos_hint_y = kwargs.get('pos_hint_y')
        self.size_hint_x = kwargs.get('size_hint_x')
        self.size_hint_y = kwargs.get('size_hint_y')
        self.padding_x = kwargs.get('padding_x', 0.0)
        self.padding_y = kwargs.get('padding_y', 0.0)
        self.spacing = kwargs.get('spacing', 0.0)
        self.texcoord = np.array(kwargs.get('texcoord', [0.0, 0.0, 1.0, 1.0]), np.float32)
        self.dragable = kwargs.get('dragable', False)
        self.touchable = kwargs.get('touchable', False) or self.dragable
        self.texture = kwargs.get('texture')
        self.opacity = kwargs.get('opacity', 1.0)
        self.visible = kwargs.get('visible', True)

        self.center_x = 0.0
        self.center_y = 0.0
        self.world_x = 0.0
        self.world_y = 0.0
        self.world_center_x = 0.0
        self.world_center_y = 0.0
        self.touch_offset_x = 0.0
        self.touch_offset_y = 0.0
        self.total_size_hint_x = 1.0
        self.total_size_hint_y = 1.0

        self.touched = False
        self.pressed = False

        self.callback_touch_down = None
        self.callback_touch_move = None
        self.callback_touch_up = None

        self.label = None
        text = kwargs.get('text', '')
        font_size = kwargs.get('font_size', 10)

        if text:
            self.set_text(text, font_size)

    @property
    def text(self):
        return self.label.text if self.label is not None else ''

    @text.setter
    def text(self, text):
        if self.label is not None:
            self.label.text = text
        else:
            self.set_text(text)

    def set_text(self, text, font_size=10, halign=Align.LEFT, valign=Align.BOTTOM):
        if self.label is None:
            self.label = Label(halign=halign, valign=valign)
            self.add_widget(self.label)
        self.label.set_text(text, font_size, halign=halign, valign=valign)

    def collide(self, x, y):
        return self.world_x <= x < (self.world_x + self.width) and self.world_y <= y < (self.world_y + self.height)

    def bind(self, **kwargs):
        for key in kwargs:
            if self.on_touch_down.__name__ == key:
                self.callback_touch_down = kwargs[key]
            elif self.on_touch_move.__name__ == key:
                self.callback_touch_move = kwargs[key]
            elif self.on_touch_up.__name__ == key:
                self.callback_touch_up = kwargs[key]

    def on_touch_down(self, x, y):
        self.touched = True

        if self.dragable:
            self.touch_offset_x = self.x - x
            self.touch_offset_y = self.y - y

            if self.callback_touch_down is not None:
                self.callback_touch_down(self, x, y)

    def on_touch_move(self, x, y):
        if self.touched:
            if self.dragable:
                self.x = x + self.touch_offset_x
                self.y = y + self.touch_offset_y

                if self.callback_touch_move is not None:
                    self.callback_touch_move(self, x, y)

    def on_touch_up(self, x, y):
        if self.touched:
            self.touched = False
            if self.dragable:
                self.x = x + self.touch_offset_x
                self.y = y + self.touch_offset_y

                if self.callback_touch_up is not None:
                    self.callback_touch_up(self, x, y)

    @property
    def color(self):
        return self._color

    @color.setter
    def color(self, color):
        self._color[...] = color

    @property
    def pressed_color(self):
        return self._pressed_color

    @pressed_color.setter
    def pressed_color(self, color):
        self._pressed_color[...] = color

    @property
    def pressed_opacity(self):
        return self._pressed_color[3]

    @pressed_opacity.setter
    def pressed_opacity(self, opacity):
        self._pressed_color[3] = opacity

    @property
    def x(self):
        return self._x

    @x.setter
    def x(self, x):
        if self._x != x:
            self.changed_layout = True
            self.pos_hint_x = None
            self._x = x

    @property
    def y(self):
        return self._y

    @y.setter
    def y(self, y):
        if self._y != y:
            self.changed_layout = True
            self.pos_hint_y = None
            self._y = y

    @property
    def pos_hint_x(self):
        return self._pos_hint_x

    @pos_hint_x.setter
    def pos_hint_x(self, pos_hint_x):
        if pos_hint_x is not None and self._pos_hint_x != pos_hint_x:
            self.changed_layout = True
        self._pos_hint_x = pos_hint_x

    @property
    def pos_hint_y(self):
        return self._pos_hint_y

    @pos_hint_y.setter
    def pos_hint_y(self, pos_hint_y):
        if pos_hint_y is not None and self._pos_hint_y != pos_hint_y:
            self.changed_layout = True
        self._pos_hint_y = pos_hint_y

    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, width):
        if self._width != width:
            self.changed_layout = True
            self.size_hint_x = None
            self._width = width

    @property
    def height(self):
        return self._height

    @height.setter
    def height(self, height):
        if self._height != height:
            self.changed_layout = True
            self.size_hint_y = None
            self._height = height

    @property
    def size_hint_x(self):
        return self._size_hint_x

    @size_hint_x.setter
    def size_hint_x(self, size_hint_x):
        if size_hint_x is not None and self._size_hint_x != size_hint_x:
            self.changed_layout = True
        self._size_hint_x = size_hint_x

    @property
    def size_hint_y(self):
        return self._size_hint_y

    @size_hint_y.setter
    def size_hint_y(self, size_hint_y):
        if size_hint_y is not None and self._size_hint_y != size_hint_y:
            self.changed_layout = True
        self._size_hint_y = size_hint_y

    @property
    def padding_x(self):
        return self._padding_x

    @padding_x.setter
    def padding_x(self, padding_x):
        if padding_x != self._padding_x:
            self._padding_x = padding_x
            self.changed_layout = True

    @property
    def padding_y(self):
        return self._padding_y

    @padding_y.setter
    def padding_y(self, padding_y):
        if padding_y != self._padding_y:
            self._padding_y = padding_y
            self.changed_layout = True

    @property
    def spacing(self):
        return self._spacing

    @spacing.setter
    def spacing(self, spacing):
        if spacing != self._spacing:
            self._spacing = spacing
            self.changed_layout = True

    @property
    def halign(self):
        return self._halign

    @halign.setter
    def halign(self, halign):
        if halign in self.haligns and halign != self._halign:
            self.changed_layout = True
            self.pos_hint_x = None
            self._halign = halign

    @property
    def valign(self):
        return self._valign

    @valign.setter
    def valign(self, valign):
        if valign in self.valigns and valign != self._valign:
            self.changed_layout = True
            self.pos_hint_y = None
            self._valign = valign

    def bind_texture(self, texture):
        self.texture = texture

    def clear_widgets(self):
        for widget in self.widgets:
            widget.clear_widgets()

            if self.viewport_manager.focused_widget is widget:
                self.viewport_manager.focused_widget = None

        self.widgets = []

    def add_widget(self, widget):
        if widget.parent is not None:
            raise AttributeError("Widget already has parent.")

        if widget not in self.widgets:
            self.widgets.append(widget)
            widget.parent = self
            self.update_layout(changed_layout=True)

    def remove_widget(self, widget):
        if widget in self.widgets:
            if self.viewport_manager.focused_widget is widget:
                self.viewport_manager.focused_widget = None

            self.widgets.remove(widget)
            widget.parent = None
            self.update_layout(changed_layout=True)

    def update_layout(self, changed_layout=False, recursive=True):
        changed_layout = self.changed_layout or changed_layout

        if changed_layout:
            if self.parent is not None:
                # NOTE : If you set the value to x instead of __x, the value of __size_hint_x will be none by @__x.setter.
                if self._halign:
                    if Align.LEFT == self._halign:
                        self._x = self.parent.padding_x
                    elif Align.RIGHT == self._halign:
                        self._x = self.parent.width - self._width - self.parent.padding_x
                    else:
                        self._x = (self.parent.width - self._width) * 0.5
                elif self._pos_hint_x is not None:
                    self._x = self.parent.padding_x + self._pos_hint_x * (self.parent.width - self.parent.padding_x * 2.0)

                if self._valign:
                    if Align.BOTTOM == self._valign:
                        self._y = self.parent.padding_y
                    elif Align.TOP == self._valign:
                        self._y = self.parent.height - self._height - self.parent.padding_y
                    else:
                        self._y = (self.parent.height - self._height) * 0.5
                elif self._pos_hint_y is not None:
                    self._y = self._pos_hint_y * self.parent.height

                if self._size_hint_x is not None:
                    self._width = (self.parent.width - self.parent.padding_x * 2.0) * self._size_hint_x / self.parent.total_size_hint_x

                if self._size_hint_y is not None:
                    self._height = (self.parent.height - self.parent.padding_y * 2.0) * self._size_hint_y / self.parent.total_size_hint_y

            self.center_x = self._x + self._width / 2
            self.center_y = self._y + self._height / 2
            self.world_x = self._x
            self.world_y = self._y

            if self.parent is not None:
                self.world_x += self.parent.world_x
                self.world_y += self.parent.world_y
                self.world_center_x = self.center_x + self.parent.world_x
                self.world_center_y = self.center_y + self.parent.world_y

            self.changed_layout = False

        if recursive:
            for widget in self.widgets:
                widget.update_layout(changed_layout=changed_layout)

    def update(self, dt, touch_event=False):
        for widget in self.widgets:
            touch_event = widget.update(dt, touch_event)

        if not touch_event and self.touchable:
            down_left, down_middle, down_right = self.core_manager.get_mouse_down()
            pressed_left, pressed_middle, pressed_right = self.core_manager.get_mouse_pressed()
            mouse_x, mouse_y = self.core_manager.get_mouse_pos()

            if self.touched:
                if pressed_left:
                    self.on_touch_move(mouse_x, mouse_y)
                else:
                    self.on_touch_up(mouse_x, mouse_y)
                    if not self.has_cursor:
                        self.viewport_manager.focused_widget = None

            elif down_left:
                if self.collide(mouse_x, mouse_y):
                    self.viewport_manager.focused_widget = self
                    self.on_touch_down(mouse_x, mouse_y)
                elif self.has_cursor:
                    self.viewport_manager.focused_widget = None

        return self.touched or touch_event

    def render(self, last_program, render_widget_program, mesh):
        if 0.0 <= self.opacity and self.visible:
            render_widget_program.use_program()
            render_widget_program.bind_material_instance()

            if self.pressed:
                render_widget_program.bind_uniform_data("color", self.pressed_color)
            else:
                render_widget_program.bind_uniform_data("color", self.color)

            render_widget_program.bind_uniform_data("pos_size", [self.world_x, self.world_y, self.width, self.height])
            render_widget_program.bind_uniform_data("texcoord", self.texcoord)
            render_widget_program.bind_uniform_data("opacity", self.opacity)

            if self.texture is not None:
                render_widget_program.bind_uniform_data("texture_diffuse", self.texture)
                render_widget_program.bind_uniform_data("is_render_diffuse", True)
            else:
                render_widget_program.bind_uniform_data("is_render_diffuse", False)

            mesh.draw_elements()

        if self.visible:
            if isinstance(self, Label) and self.text_render_data is not None:
                self.core_manager.renderer.render_text(self.text_render_data,
                                                       self.world_x,
                                                       self.world_y,
                                                       self.root.width,
                                                       self.root.height)
            for widget in self.widgets:
                widget.render(last_program, render_widget_program, mesh)
*/