use std::collections::HashMap;

use crate::constants::JOYSTICK_SENSOR_DEAD_ZONE;
use nalgebra::Vector2;
use sdl2::controller::{Axis, Button, GameController};
use sdl2::Sdl;
use winit::keyboard::KeyCode;

pub type KeyMap = HashMap<KeyCode, bool>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ButtonState {
    None,
    Pressed,
    Hold,
    Released,
}

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
    pub _scroll_delta: Vector2<i32>,
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
    pub _btn_r_released: bool,
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

pub struct JoystickInputData {
    pub _stick_left_direction: Vector2<i16>,
    pub _stick_right_direction: Vector2<i16>,
    pub _btn_left_stick: ButtonState,
    pub _btn_right_stick: ButtonState,
    pub _btn_left: ButtonState,
    pub _btn_right: ButtonState,
    pub _btn_up: ButtonState,
    pub _btn_down: ButtonState,
    pub _btn_a: ButtonState,
    pub _btn_b: ButtonState,
    pub _btn_x: ButtonState,
    pub _btn_y: ButtonState,
    pub _btn_left_shoulder: ButtonState,
    pub _btn_left_trigger: ButtonState,
    pub _btn_left_trigger_value: u16,
    pub _btn_right_shoulder: ButtonState,
    pub _btn_right_trigger: ButtonState,
    pub _btn_right_trigger_value: u16,
    pub _btn_back: ButtonState,
    pub _btn_start: ButtonState,
    pub _btn_guide: ButtonState,
    pub _game_controller: Option<GameController>,
}

// Implementation
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

pub fn create_mouse_move_data(width: i32, height: i32) -> Box<MouseMoveData> {
    let mouse_pos = Vector2::new(width, height);
    Box::new(MouseMoveData {
        _mouse_pos: mouse_pos.clone(),
        _mouse_pos_delta: Vector2::new(0, 0),
        _scroll_delta: Vector2::new(0, 0),
    })
}

pub fn create_mouse_input_data() -> Box<MouseInputData> {
    Box::new(MouseInputData::default())
}

impl MouseMoveData {
    pub fn clear_mouse_move_delta(&mut self) {
        self._mouse_pos_delta.x = 0;
        self._mouse_pos_delta.y = 0;
        self._scroll_delta.x = 0;
        self._scroll_delta.y = 0;
    }

    pub fn clamp_mouse_pos(pos: i32, limit_pos: i32) -> i32 {
        if pos < 0 {
            0
        } else if limit_pos <= pos {
            limit_pos - 1
        } else {
            pos
        }
    }

    pub fn update_mouse_pos(&mut self, position: &(i32, i32), window_size: &Vector2<i32>) {
        self._mouse_pos_delta.x += position.0 - self._mouse_pos.x;
        self._mouse_pos_delta.y += position.1 - self._mouse_pos.y;
        self._mouse_pos.x = MouseMoveData::clamp_mouse_pos(position.0, window_size.x);
        self._mouse_pos.y = MouseMoveData::clamp_mouse_pos(position.1, window_size.y);
    }

    pub fn update_mouse_move(&mut self, delta: &(i32, i32), window_size: &Vector2<i32>) {
        self._mouse_pos_delta.x += delta.0;
        self._mouse_pos_delta.y += delta.1;
        self._mouse_pos.x =
            MouseMoveData::clamp_mouse_pos(self._mouse_pos.x + delta.0, window_size.x);
        self._mouse_pos.y =
            MouseMoveData::clamp_mouse_pos(self._mouse_pos.y + delta.1, window_size.y);
    }

    pub fn update_scroll_move(&mut self, delta: &(i32, i32)) {
        self._scroll_delta.x += delta.0;
        self._scroll_delta.y += delta.1;
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
    pub fn is_any_key_hold(&self) -> bool {
        false == self._key_hold_map.is_empty()
    }

    pub fn get_key_hold(&self, key: KeyCode) -> bool {
        match self._key_hold_map.get(&key) {
            Some(a) => *a,
            _ => false,
        }
    }

    pub fn set_key_hold(&mut self, key: KeyCode, hold: bool) {
        if hold {
            self._key_hold_map.insert(key, hold);
        } else {
            self._key_hold_map.remove(&key);
        }
    }

    pub fn clear_key_hold(&mut self) {
        self._key_hold_map.clear();
    }

    pub fn is_any_key_pressed(&self) -> bool {
        false == self._key_pressed_map.is_empty()
    }

    pub fn get_key_pressed(&self, key: KeyCode) -> bool {
        match self._key_pressed_map.get(&key) {
            Some(a) => *a,
            _ => false,
        }
    }

    pub fn set_key_pressed(&mut self, key: KeyCode) {
        if false == self.get_key_hold(key) {
            self._key_pressed_map.insert(key, true);
            self.set_key_hold(key, true);
        }
    }

    pub fn clear_key_pressed(&mut self) {
        self._key_pressed_map.clear();
    }

    pub fn get_key_released(&self, key: KeyCode) -> bool {
        match self._key_released_map.get(&key) {
            Some(a) => *a,
            _ => false,
        }
    }

    pub fn set_key_released(&mut self, key: KeyCode) {
        self._key_released_map.insert(key, true);
        self.set_key_hold(key, false);
    }

    pub fn clear_key_released(&mut self) {
        self._key_released_map.clear();
    }
}

// Joystick Input Data
impl JoystickInputData {
    pub fn create_joystick_input_data(sdl: &Sdl) -> Box<JoystickInputData> {
        let game_controller_subsystem = sdl.game_controller().expect("failed to game_controller.");
        let available_joystick_count = game_controller_subsystem
            .num_joysticks()
            .map_err(|e| format!("can't enumerate joysticks: {}", e))
            .expect("failed to get num joysticks.");
        log::info!("{} joysticks available", available_joystick_count);

        let joystick: Option<GameController> = (0..available_joystick_count).find_map(|id| {
            if false == game_controller_subsystem.is_game_controller(id) {
                log::info!("{} is not a game controller", id);
                return None;
            }

            log::info!("Attempting to open controller {}", id);

            match game_controller_subsystem.open(id) {
                Ok(c) => {
                    // We managed to find and open a game controller,
                    // exit the loop
                    log::info!("Success: opened \"{}\"", c.name());
                    Some(c)
                }
                Err(e) => {
                    log::info!("failed: {:?}", e);
                    None
                }
            }
        });

        if let Some(joystick_controller) = &joystick {
            log::info!("Controller mapping: {}", joystick_controller.mapping());
        }

        Box::new(JoystickInputData {
            _stick_left_direction: Vector2::zeros(),
            _stick_right_direction: Vector2::zeros(),
            _btn_left_stick: ButtonState::None,
            _btn_right_stick: ButtonState::None,
            _btn_left: ButtonState::None,
            _btn_right: ButtonState::None,
            _btn_up: ButtonState::None,
            _btn_down: ButtonState::None,
            _btn_a: ButtonState::None,
            _btn_b: ButtonState::None,
            _btn_x: ButtonState::None,
            _btn_y: ButtonState::None,
            _btn_left_shoulder: ButtonState::None,
            _btn_left_trigger: ButtonState::None,
            _btn_left_trigger_value: 0,
            _btn_right_shoulder: ButtonState::None,
            _btn_right_trigger: ButtonState::None,
            _btn_right_trigger_value: 0,
            _btn_back: ButtonState::None,
            _btn_start: ButtonState::None,
            _btn_guide: ButtonState::None,
            _game_controller: joystick,
        })
    }

    pub fn reset_button_state(button: &mut ButtonState) {
        match button {
            ButtonState::Pressed => *button = ButtonState::Hold,
            ButtonState::Released => *button = ButtonState::None,
            _ => (),
        }
    }

    pub fn update_joystick_button_state(&mut self) {
        JoystickInputData::reset_button_state(&mut self._btn_left_stick);
        JoystickInputData::reset_button_state(&mut self._btn_right_stick);
        JoystickInputData::reset_button_state(&mut self._btn_left);
        JoystickInputData::reset_button_state(&mut self._btn_right);
        JoystickInputData::reset_button_state(&mut self._btn_up);
        JoystickInputData::reset_button_state(&mut self._btn_down);
        JoystickInputData::reset_button_state(&mut self._btn_a);
        JoystickInputData::reset_button_state(&mut self._btn_b);
        JoystickInputData::reset_button_state(&mut self._btn_x);
        JoystickInputData::reset_button_state(&mut self._btn_y);
        JoystickInputData::reset_button_state(&mut self._btn_left_shoulder);
        JoystickInputData::reset_button_state(&mut self._btn_right_shoulder);
        JoystickInputData::reset_button_state(&mut self._btn_left_trigger);
        JoystickInputData::reset_button_state(&mut self._btn_right_trigger);
        JoystickInputData::reset_button_state(&mut self._btn_back);
        JoystickInputData::reset_button_state(&mut self._btn_start);
        JoystickInputData::reset_button_state(&mut self._btn_guide);
    }

    pub fn has_game_controller(&self) -> bool {
        self._game_controller.is_some()
    }

    pub fn set_rumble(
        &mut self,
        low_frequency_rumble: u16,
        high_frequency_rumble: u16,
        duration_ms: u32,
    ) {
        self._game_controller
            .as_mut()
            .unwrap()
            .set_rumble(low_frequency_rumble, high_frequency_rumble, duration_ms)
            .expect("")
    }

    pub fn update_controller_axis_motion(&mut self, axis: Axis, value: i16) {
        if self.has_game_controller() {
            let prev_left_trigger_value = self._btn_left_trigger_value;
            let prev_right_trigger_value = self._btn_right_trigger_value;

            match axis {
                Axis::TriggerLeft => {
                    self._btn_left_trigger_value = (value as u16) * 2; // Trigger axes go from 0 to 32767, so this should be okay
                    self.set_rumble(self._btn_left_trigger_value, self._btn_right_trigger_value, 15000);
                }
                Axis::TriggerRight => {
                    self._btn_right_trigger_value = (value as u16) * 2; // Trigger axes go from 0 to 32767, so this should be okay
                    self.set_rumble(self._btn_left_trigger_value, self._btn_right_trigger_value, 15000);
                }
                Axis::LeftX => {
                    if value < -JOYSTICK_SENSOR_DEAD_ZONE || JOYSTICK_SENSOR_DEAD_ZONE < value {
                        // Axis motion is an absolin the range [-32768, 32767]. Let's simulate a very rough dead zone to ignore spurious events.
                        self._stick_left_direction.x = value;
                    } else {
                        self._stick_left_direction.x = 0;
                    }
                }
                Axis::LeftY => {
                    if value < -JOYSTICK_SENSOR_DEAD_ZONE || JOYSTICK_SENSOR_DEAD_ZONE < value {
                        self._stick_left_direction.y = value;
                    } else {
                        self._stick_left_direction.y = 0;
                    }
                }
                Axis::RightX => {
                    if value < -JOYSTICK_SENSOR_DEAD_ZONE || JOYSTICK_SENSOR_DEAD_ZONE < value {
                        self._stick_right_direction.x = value;
                    } else {
                        self._stick_right_direction.x = 0;
                    }
                }
                Axis::RightY => {
                    if value < -JOYSTICK_SENSOR_DEAD_ZONE || JOYSTICK_SENSOR_DEAD_ZONE < value {
                        self._stick_right_direction.y = value;
                    } else {
                        self._stick_right_direction.y = 0;
                    }
                }
            }
            //_ => log::info!("unknown axis: {:?}, value: {:?}", axis, value)

            // update trigger buttons
            if prev_left_trigger_value == 0 && self._btn_left_trigger_value != 0 {
                self._btn_left_trigger = ButtonState::Pressed;
            } else if prev_left_trigger_value != 0 && self._btn_left_trigger_value == 0 {
                self._btn_left_trigger = ButtonState::Released;
            }

            if prev_right_trigger_value == 0 && self._btn_right_trigger_value != 0 {
                self._btn_right_trigger = ButtonState::Pressed;
            } else if prev_right_trigger_value != 0 && self._btn_right_trigger_value == 0 {
                self._btn_right_trigger = ButtonState::Released;
            }
        }
    }

    pub fn update_controller_button_state(&mut self, button: Button, button_state: ButtonState) {
        match button {
            Button::A => self._btn_a = button_state,
            Button::B => self._btn_b = button_state,
            Button::X => self._btn_x = button_state,
            Button::Y => self._btn_y = button_state,
            Button::Back => self._btn_back = button_state,
            Button::Guide => self._btn_guide = button_state,
            Button::Start => self._btn_start = button_state,
            Button::LeftStick => self._btn_left_stick = button_state,
            Button::RightStick => self._btn_right_stick = button_state,
            Button::LeftShoulder => self._btn_left_shoulder = button_state,
            Button::RightShoulder => self._btn_right_shoulder = button_state,
            Button::DPadUp => self._btn_up = button_state,
            Button::DPadDown => self._btn_down = button_state,
            Button::DPadLeft => self._btn_left = button_state,
            Button::DPadRight => self._btn_right = button_state,
            _ => log::debug!("unknown button: {:?}", button)
        }
    }
}
