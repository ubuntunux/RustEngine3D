use std::collections::HashMap;

use winit::event::{
    VirtualKeyCode,
};
use nalgebra::{ Vector2 };

pub type KeyMap = HashMap<VirtualKeyCode, bool>;

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct MouseMoveData {
    pub _mouse_pos: Vector2<i32>,
    pub _mouse_pos_delta: Vector2<i32>,
    pub _scroll_xoffset: f32,
    pub _scroll_yoffset: f32
}

#[derive(Clone, Debug)]
pub struct MouseInputData {
    pub _btn_l_pressed: bool,
    pub _btn_m_pressed: bool,
    pub _btn_r_pressed: bool,
    pub _btn_l_hold: bool,
    pub _btn_m_hold: bool,
    pub _btn_r_hold: bool,
    pub _btn_l_released: bool,
    pub _btn_m_released: bool,
    pub _btn_r_released: bool
}

impl Default for MouseInputData {
    fn default() -> MouseInputData {
        MouseInputData {
            _btn_l_pressed: false,
            _btn_m_pressed: false,
            _btn_r_pressed: false,
            _btn_l_hold: false,
            _btn_m_hold: false,
            _btn_r_hold: false,
            _btn_l_released: false,
            _btn_m_released: false,
            _btn_r_released: false,
        }
    }
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
        _mouse_pos_delta: Vector2::new(0, 0),
        _scroll_xoffset: 0.0,
        _scroll_yoffset: 0.0,
    })
}

pub fn create_mouse_input_data() -> Box<MouseInputData> {
    Box::new(MouseInputData::default())
}

impl MouseMoveData {
    pub fn clear_mouse_move_delta(&mut self) {
        self._mouse_pos_delta.x = 0;
        self._mouse_pos_delta.y = 0;
    }

    pub fn update_mouse_move(&mut self, position: &(i32, i32)) {
        self._mouse_pos_delta.x += position.0 - self._mouse_pos.x;
        self._mouse_pos_delta.y += position.1 - self._mouse_pos.y;
        self._mouse_pos.x = position.0;
        self._mouse_pos.y = position.1;
    }
}

impl MouseInputData {
    pub fn clear_mouse_input(&mut self) {
        self._btn_l_pressed = false;
        self._btn_m_pressed = false;
        self._btn_r_pressed = false;
        self._btn_l_released = false;
        self._btn_m_released = false;
        self._btn_r_released = false;
    }

    pub fn btn_l_pressed(&mut self, pressed: bool) {
        self._btn_l_pressed = pressed;
        self._btn_l_hold = pressed;
        self._btn_l_released = !pressed;
    }

    pub fn btn_r_pressed(&mut self, pressed: bool) {
        self._btn_r_pressed = pressed;
        self._btn_r_hold = pressed;
        self._btn_r_released = !pressed;
    }

    pub fn btn_m_pressed(&mut self, pressed: bool) {
        self._btn_m_pressed = pressed;
        self._btn_m_hold = pressed;
        self._btn_m_released = !pressed;
    }
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