use std::collections::HashMap;

use winit::event::{
    VirtualKeyCode,
};
use nalgebra::{ Vector2 };

pub type KeyMap = HashMap<VirtualKeyCode, bool>;

#[derive(Clone)]
pub struct KeyboardInputData {
    pub _keyboard_down: bool,
    pub _keyboard_pressed: bool,
    pub _keyboard_up: bool,
    pub _key_pressed_map: KeyMap,
    pub _key_hold_map: KeyMap,
    pub _key_released_map: KeyMap,
    pub _modifier_keys_shift: bool,
    pub _modifier_keys_control: bool,
    pub _modifier_keys_alt: bool,
    pub _modifier_keys_super: bool,
}

#[derive(Clone)]
pub struct MouseMoveData {
    pub _mouse_pos: Vector2<i32>,
    pub _mouse_pos_prev: Vector2<i32>,
    pub _mouse_pos_delta: Vector2<i32>,
    pub _scroll_xoffset: f32,
    pub _scroll_yoffset: f32
}

#[derive(Clone)]
pub struct MouseInputData {
    pub _btn_l_down: bool,
    pub _btn_m_down: bool,
    pub _btn_r_down: bool,
    pub _btn_l_up: bool,
    pub _btn_m_up: bool,
    pub _btn_r_up: bool
}

pub fn create_keyboard_input_data() -> Box<KeyboardInputData> {
    Box::new(KeyboardInputData {
        _keyboard_down: false,
        _keyboard_pressed: false,
        _keyboard_up: false,
        _key_pressed_map: KeyMap::new(),
        _key_hold_map: KeyMap::new(),
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
    pub fn get_key_hold(&self, key: VirtualKeyCode) -> bool {
        match self._key_hold_map.get(&key) {
            Some(a) => *a,
            _ => false
        }
    }

    pub fn set_key_hold(&mut self, key: VirtualKeyCode, hold: bool) {
        self._key_hold_map.insert(key, hold);
    }

    pub fn clear_key_hold(&mut self) {
        self._key_hold_map.clear();
    }

    pub fn get_key_pressed(&self, key: VirtualKeyCode) -> bool {
        match self._key_pressed_map.get(&key) {
            Some(a) => *a,
            _ => false
        }
    }

    pub fn set_key_pressed(&mut self, key: VirtualKeyCode) {
        self._key_pressed_map.insert(key, true);
        self.set_key_hold(key, true);
    }

    pub fn clear_key_pressed(&mut self) {
        self._key_pressed_map.clear();
    }

    pub fn get_key_released(&self, key: VirtualKeyCode) -> bool {
        match self._key_released_map.get(&key) {
            Some(a) => *a,
            _ => false
        }
    }

    pub fn set_key_released(&mut self, key: VirtualKeyCode) {
        self._key_released_map.insert(key, true);
        self.set_key_hold(key, false);
    }

    pub fn clear_key_released(&mut self) {
        self._key_released_map.clear();
    }
}