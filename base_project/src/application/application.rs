use log::LevelFilter;

use ash::vk;
use winit::event::VirtualKeyCode;
use rust_engine_3d::constants;
use rust_engine_3d::application::application::{self, ApplicationBase, ApplicationData};
use rust_engine_3d::resource::resource::Resources;

use crate::application_constants;
use crate::application::scene_manager::SceneManager;
use crate::renderer::renderer::Renderer;
use crate::renderer::ui::UIManager;
use crate::renderer::effect::EffectManager;


pub struct Application {
    pub _application_data: *const ApplicationData,
    pub _resources: *const Resources,
    pub _renderer: Box<Renderer>,
    pub _scene_manager: Box<SceneManager>,
    pub _effect_manager: Box<EffectManager>,
    pub _ui_manager: Box<UIManager>,
}

impl ApplicationBase for Application {
    fn initialize_application(&mut self, application_data: &ApplicationData) {
        self._application_data = application_data;
    }

    fn update_event(&self) {
        let application_data = self.get_application_data();
        let time_data = &application_data._time_data;
        let mouse_move_data = &application_data._mouse_move_data;
        let mouse_input_data = &application_data._mouse_input_data;
        let keyboard_input_data = &application_data._keyboard_input_data;

        const MOUSE_DELTA_RATIO: f32 = 500.0;
        let delta_time = time_data._delta_time;
        let _mouse_pos = &mouse_move_data._mouse_pos;
        let mouse_delta_x = mouse_move_data._mouse_pos_delta.x as f32 / application_data._window_size.0 as f32 * MOUSE_DELTA_RATIO;
        let mouse_delta_y = mouse_move_data._mouse_pos_delta.y as f32 / application_data._window_size.1 as f32 * MOUSE_DELTA_RATIO;
        let btn_left: bool = mouse_input_data._btn_l_hold;
        let btn_right: bool = mouse_input_data._btn_r_hold;
        let _btn_middle: bool = mouse_input_data._btn_m_hold;

        let pressed_key_a = keyboard_input_data.get_key_hold(VirtualKeyCode::A);
        let pressed_key_d = keyboard_input_data.get_key_hold(VirtualKeyCode::D);
        let pressed_key_w = keyboard_input_data.get_key_hold(VirtualKeyCode::W);
        let pressed_key_s = keyboard_input_data.get_key_hold(VirtualKeyCode::S);
        let pressed_key_q = keyboard_input_data.get_key_hold(VirtualKeyCode::Q);
        let pressed_key_e = keyboard_input_data.get_key_hold(VirtualKeyCode::E);
        let pressed_key_z = keyboard_input_data.get_key_hold(VirtualKeyCode::Z);
        let pressed_key_c = keyboard_input_data.get_key_hold(VirtualKeyCode::C);
        let pressed_key_comma = keyboard_input_data.get_key_hold(VirtualKeyCode::Comma);
        let pressed_key_period = keyboard_input_data.get_key_hold(VirtualKeyCode::Period);
        let released_key_left_bracket = keyboard_input_data.get_key_released(VirtualKeyCode::LBracket);
        let released_key_right_bracket = keyboard_input_data.get_key_released(VirtualKeyCode::RBracket);
        let released_key_subtract = keyboard_input_data.get_key_released(VirtualKeyCode::Minus);
        let released_key_equals = keyboard_input_data.get_key_released(VirtualKeyCode::Equals);

        let mut main_camera = self.get_scene_manager()._main_camera.borrow_mut();
        let mut main_light = self.get_scene_manager()._main_light.borrow_mut();
        let modifier_keys_shift = keyboard_input_data.get_key_hold(VirtualKeyCode::LShift);
        let camera_move_speed_multiplier = if modifier_keys_shift { 2.0 } else { 1.0 };
        let move_speed: f32 = application_constants::CAMERA_MOVE_SPEED * camera_move_speed_multiplier * delta_time as f32;
        let pan_speed = application_constants::CAMERA_PAN_SPEED * camera_move_speed_multiplier;
        let _rotation_speed = application_constants::CAMERA_ROTATION_SPEED;

        if released_key_left_bracket {
            self.get_renderer_mut().prev_debug_render_target();
        } else if released_key_right_bracket {
            self.get_renderer_mut().next_debug_render_target();
        }

        if released_key_subtract {
            self.get_renderer_mut().prev_debug_render_target_miplevel();
        } else if released_key_equals {
            self.get_renderer_mut().next_debug_render_target_miplevel();
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

    fn terminate_applicateion(&mut self) {
    }
}

impl Application {
    pub fn get_application_data(&self) -> &ApplicationData {
        unsafe { &*self._application_data }
    }
    pub fn get_application_data_mut(&self) -> &mut ApplicationData {
        unsafe { &mut *(self._application_data as *mut ApplicationData) }
    }
    pub fn get_effect_manager(&self) -> &EffectManager {
        &self._effect_manager
    }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager {
        unsafe { &mut *((self._effect_manager.as_ref() as *const EffectManager) as *mut EffectManager) }
    }
    pub fn get_scene_manager(&self) -> &SceneManager {
        &self._scene_manager
    }
    pub fn get_scene_manager_mut(&self) -> &mut SceneManager {
        unsafe { &mut *((self._scene_manager.as_ref() as *const SceneManager) as *mut SceneManager) }
    }
    pub fn get_renderer(&self) -> &Renderer {
        &self._renderer
    }
    pub fn get_renderer_mut(&self) -> &mut Renderer {
        unsafe { &mut *((self._renderer.as_ref() as *const Renderer) as *mut Renderer) }
    }
    pub fn get_ui_manager(&self) -> &UIManager {
        &self._ui_manager
    }
    pub fn get_ui_manager_mut(&self) -> &mut UIManager {
        unsafe { &mut *((self._ui_manager.as_ref() as *const UIManager) as *mut UIManager) }
    }
}

pub fn run_application() {
    let vulkan_api_version: u32;
    let enable_immediate_mode: bool;
    let enable_validation_layer: bool;
    let is_concurrent_mode: bool;

    #[cfg(target_os = "android")]
    {
        vulkan_api_version = vk::make_version(1, 0, 0);
        enable_immediate_mode = false;
        enable_validation_layer = false;
        is_concurrent_mode = false;
    }
    #[cfg(not(target_os = "android"))]
    {
        vulkan_api_version = vk::make_version(1, 2, 0);
        enable_immediate_mode = true;
        enable_validation_layer = true;
        is_concurrent_mode = true;
    }

    unsafe {
        constants::VULKAN_API_VERSION = vulkan_api_version;
        constants::DEBUG_MESSAGE_LEVEL = vk::DebugUtilsMessageSeverityFlagsEXT::WARNING;
        constants::VULKAN_LAYERS = vec!["VK_LAYER_LUNARG_standard_validation".to_string()];
        constants::REQUIRE_DEVICE_EXTENSIONS = vec!["VK_KHR_swapchain".to_string()];
        constants::ENABLE_IMMEDIATE_MODE = enable_immediate_mode;
        constants::ENABLE_VALIDATION_LAYER = enable_validation_layer;
        constants::IS_CONCURRENT_MODE = is_concurrent_mode;
        constants::METER_PER_UNIT = 1.0;
        constants::NEAR = 0.1;
        constants::FAR = 2000.0;
        constants::FOV = 60.0;
        // effect
        constants::MAX_EMITTER_COUNT = 1024;
        constants::MAX_PARTICLE_COUNT = 262144;
    }

    // create
    let renderer = Renderer::create_renderer_data();
    let scene_manager = SceneManager::create_scene_manager();
    let effect_manager = EffectManager::create_effect_manager();
    let ui_manager = UIManager::create_ui_manager();

    // initialize
    let application = Application {
        _application_data: std::ptr::null(),
        _resources: std::ptr::null(),
        _renderer: renderer,
        _scene_manager: scene_manager,
        _effect_manager: effect_manager,
        _ui_manager: ui_manager,
    };
    application::run_application(
        LevelFilter::Info,
        &application,
        application.get_scene_manager(),
        application.get_effect_manager(),
        application.get_renderer(),
        application.get_ui_manager()
    );
}