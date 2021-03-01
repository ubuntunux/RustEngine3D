use std::cell::RefMut;
use std::time;
use log;

use nalgebra::{
    Vector3,
};
use winit::event::{
    ElementState,
    Event,
    MouseButton,
    MouseScrollDelta,
    Touch,
    TouchPhase,
    VirtualKeyCode,
    WindowEvent
};
use winit::event_loop::{
    ControlFlow,
    EventLoop
};
use winit::dpi;
use winit::window::{ WindowBuilder };

use rust_engine_3d::constants::{ Constants };
use rust_engine_3d::application::application::{ self, ApplicationData, ApplicationBase };
use rust_engine_3d::application::scene_manager::{ self, SceneManagerData };
use rust_engine_3d::application::input;
use rust_engine_3d::resource::resource::{ self, Resources};
use rust_engine_3d::renderer::renderer::{ self, RendererData, CameraCreateInfo };
use rust_engine_3d::renderer::font::FontManager;
use rust_engine_3d::renderer::ui::UIManager;
use rust_engine_3d::utilities::system::{self, RcRefCell, newRcRefCell};
use rust_engine_3d::utilities::logger;

pub struct Application {
}

impl ApplicationBase for Application {
    fn update_event(&mut self, scene_manager_data: &SceneManagerData) {
        let renderer_data: *mut RendererData = scene_manager_data._renderer_data.as_ptr();

        const MOUSE_DELTA_RATIO: f32 = 500.0;
        let delta_time = self._time_data._delta_time;
        let _mouse_pos = &self._mouse_move_data._mouse_pos;
        let mouse_delta_x = self._mouse_move_data._mouse_pos_delta.x as f32 / self._window_size.0 as f32 * MOUSE_DELTA_RATIO;
        let mouse_delta_y = self._mouse_move_data._mouse_pos_delta.y as f32 / self._window_size.1 as f32 * MOUSE_DELTA_RATIO;
        let btn_left: bool = self._mouse_input_data._btn_l_hold;
        let btn_right: bool = self._mouse_input_data._btn_r_hold;
        let _btn_middle: bool = self._mouse_input_data._btn_m_hold;

        let pressed_key_a = self._keyboard_input_data.get_key_hold(VirtualKeyCode::A);
        let pressed_key_d = self._keyboard_input_data.get_key_hold(VirtualKeyCode::D);
        let pressed_key_w = self._keyboard_input_data.get_key_hold(VirtualKeyCode::W);
        let pressed_key_s = self._keyboard_input_data.get_key_hold(VirtualKeyCode::S);
        let pressed_key_q = self._keyboard_input_data.get_key_hold(VirtualKeyCode::Q);
        let pressed_key_e = self._keyboard_input_data.get_key_hold(VirtualKeyCode::E);
        let pressed_key_z = self._keyboard_input_data.get_key_hold(VirtualKeyCode::Z);
        let pressed_key_c = self._keyboard_input_data.get_key_hold(VirtualKeyCode::C);
        let pressed_key_comma = self._keyboard_input_data.get_key_hold(VirtualKeyCode::Comma);
        let pressed_key_period = self._keyboard_input_data.get_key_hold(VirtualKeyCode::Period);
        let released_key_left_bracket = self._keyboard_input_data.get_key_released(VirtualKeyCode::LBracket);
        let released_key_right_bracket = self._keyboard_input_data.get_key_released(VirtualKeyCode::RBracket);
        let released_key_subtract = self._keyboard_input_data.get_key_released(VirtualKeyCode::Minus);
        let released_key_equals = self._keyboard_input_data.get_key_released(VirtualKeyCode::Equals);

        let mut main_camera = scene_manager_data._main_camera.borrow_mut();
        let mut main_light = scene_manager_data._main_light.borrow_mut();
        let modifier_keys_shift = self._keyboard_input_data.get_key_hold(VirtualKeyCode::LShift);
        let camera_move_speed_multiplier = if modifier_keys_shift { 2.0 } else { 1.0 };
        let move_speed: f32 = self._constants._camera_move_speed * camera_move_speed_multiplier * delta_time as f32;
        let pan_speed = self._constants._camera_pan_speed * camera_move_speed_multiplier;
        let _rotation_speed = self._constants._camera_rotation_speed;

        unsafe {
            if released_key_left_bracket {
                (*renderer_data).prev_debug_render_target();
            } else if released_key_right_bracket {
                (*renderer_data).next_debug_render_target();
            }

            if released_key_subtract {
                (*renderer_data).prev_debug_render_target_miplevel();
            } else if released_key_equals {
                (*renderer_data).next_debug_render_target_miplevel();
            }
        }

        #[cfg(target_os = "android")]
            let rotation_speed = 0.02 * delta_time as f32;
        #[cfg(not(target_os = "android"))]
            let rotation_speed = delta_time as f32;

        if pressed_key_comma {
            main_light._transform_object.rotation_pitch(rotation_speed);
        } else if pressed_key_period {
            main_light._transform_object.rotation_pitch(-rotation_speed);
        }

        // when (0.0 /= scroll_yoffset) $
        //     writeIORef _cameraMoveSpeed modifiedCameraMoveSpeed

        if btn_left && btn_right {
            main_camera._transform_object.move_left(-pan_speed * mouse_delta_x as f32);
            main_camera._transform_object.move_up(pan_speed * mouse_delta_y as f32);
        }
        else if btn_right {
            main_camera._transform_object.rotation_pitch(-rotation_speed * mouse_delta_y as f32);
            main_camera._transform_object.rotation_yaw(-rotation_speed * mouse_delta_x as f32);
        }

        if pressed_key_z {
            main_camera._transform_object.rotation_roll(-rotation_speed * delta_time as f32);
        }
        else if pressed_key_c {
            main_camera._transform_object.rotation_roll(rotation_speed * delta_time as f32);
        }

        if pressed_key_w {
            main_camera._transform_object.move_front(-move_speed);
        }
        else if pressed_key_s {
            main_camera._transform_object.move_front(move_speed);
        }

        if pressed_key_a {
            main_camera._transform_object.move_left(-move_speed);
        }
        else if pressed_key_d {
            main_camera._transform_object.move_left(move_speed);
        }

        if pressed_key_q {
            main_camera._transform_object.move_up(-move_speed);
        }
        else if pressed_key_e {
            main_camera._transform_object.move_up(move_speed);
        }
    }

    fn terminate_applicateion() {
    }
}