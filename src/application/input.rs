use std::collections::HashMap;
use nalgebra::{ Vector2 };

pub type inputKey = u32;
pub type KeyMap = HashMap<inputKey, bool>;

#[derive(Clone)]
pub struct KeyboardInputData {
    _keyboard_down: bool,
    _keyboard_pressed: bool,
    _keyboard_up: bool,
    _key_pressed_map: KeyMap,
    _key_released_map: KeyMap,
    _modifier_keys_shift: bool,
    _modifier_keys_control: bool,
    _modifier_keys_alt: bool,
    _modifier_keys_super: bool,
}

#[derive(Clone)]
pub struct MouseMoveData {
    _mouse_pos: Vector2<i32>,
    _mouse_pos_prev: Vector2<i32>,
    _mouse_pos_delta: Vector2<i32>,
    _scroll_xoffset: f32,
    _scroll_yoffset: f32
}

#[derive(Clone)]
pub struct MouseInputData {
    _btn_l_down: bool,
    _btn_m_down: bool,
    _btn_r_down: bool,
    _btn_l_up: bool,
    _btn_m_up: bool,
    _btn_r_up: bool
}

pub fn create_keyboard_input_data() -> Box<KeyboardInputData> {
    Box::new(KeyboardInputData {
        _keyboard_down: false,
        _keyboard_pressed: false,
        _keyboard_up: false,
        _key_pressed_map: KeyMap::new(),
        _key_released_map: KeyMap::new(),
        _modifier_keys_shift: false,
        _modifier_keys_control: false,
        _modifier_keys_alt: false,
        _modifier_keys_super: false,
    })
}

pub fn create_mouse_move_data((width, height): (u32, u32)) -> Box<MouseMoveData> {
    let mouse_pos = Vector2::new(width as i32, height as i32);
    Box::new(MouseMoveData {
        _mouse_pos: mouse_pos.clone(),
        _mouse_pos_prev: mouse_pos.clone(),
        _mouse_pos_delta: Vector2::new(0, 0),
        _scroll_xoffset: 0.0,
        _scroll_yoffset: 0.0,
    })
}

pub fn create_mouse_input_data() -> Box<MouseInputData> {
    Box::new(MouseInputData {
        _btn_l_down: false,
        _btn_m_down: false,
        _btn_r_down: false,
        _btn_l_up: false,
        _btn_m_up: false,
        _btn_r_up: false,
    })
}

impl KeyboardInputData {
    pub fn get_key_pressed(&self, key: &inputKey) -> bool {
        match self._key_pressed_map.get(key) {
            Some(a) => *a,
            _ => false
        }
    }

    pub fn get_key_released(&self, key: &inputKey) -> bool {
        match self._key_released_map.get(key) {
            Some(a) => *a,
            _ => false
        }
    }
}