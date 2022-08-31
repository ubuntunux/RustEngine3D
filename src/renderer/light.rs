use nalgebra::{Vector3, Vector4, Matrix4};
use serde::{ Serialize, Deserialize };

use crate::renderer::transform_object::TransformObjectData;
use crate::utilities::math::{
    get_clip_space_matrix,
    orthogonal,
};
use crate::constants;

// scene_constants.glsl - struct LIGHT_CONSTANTS
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct LightConstants {
    pub _shadow_view_projection: Matrix4<f32>,
    pub _light_position: Vector3<f32>,
    pub _shadow_samples: i32,
    pub _light_direction: Vector3<f32>,
    pub _dummy0: i32,
    pub _light_color: Vector3<f32>,
    pub _dummy1: i32,
}

impl Default for LightConstants {
    fn default() -> LightConstants {
        unsafe {
            LightConstants {
                _shadow_view_projection: Matrix4::identity(),
                _light_position: Vector3::zeros(),
                _shadow_samples: constants::SHADOW_SAMPLES,
                _light_direction: Vector3::new(-std::f32::consts::PI * 0.5, 0.0, 0.0),
                _dummy0: 0,
                _light_color: Vector3::new(1.0, 1.0, 1.0),
                _dummy1: 0,
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct DirectionalLightCreateInfo {
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _light_constants: LightConstants,
    pub _shadow_dimensions: Vector4<f32>,
    pub _shadow_update_distance: f32,
}

impl Default for DirectionalLightCreateInfo {
    fn default() -> DirectionalLightCreateInfo {
        unsafe {
            DirectionalLightCreateInfo {
                _position: Vector3::zeros(),
                _rotation: Vector3::new(std::f32::consts::PI * -0.5, 0.0, 0.0),
                _light_constants: LightConstants::default(),
                _shadow_update_distance: constants::SHADOW_UPDATE_DISTANCE,
                _shadow_dimensions: Vector4::new(
                    constants::SHADOW_DISTANCE,
                    constants::SHADOW_DISTANCE,
                    -constants::SHADOW_DEPTH,
                    constants::SHADOW_DEPTH
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct DirectionalLightData {
    pub _light_name: String,
    pub _light_constants: LightConstants,
    pub _light_shadow_projection: Matrix4<f32>,
    pub _transform_object: TransformObjectData,
    pub _updated_light_data: bool,
    pub _need_to_redraw_shadow: bool,
    pub _shadow_update_distance: f32,
}

impl DirectionalLightData {
    pub fn create_light_data(light_name: &String, light_create_info: &DirectionalLightCreateInfo) -> DirectionalLightData {
        log::debug!("create_light_data: {}", light_name);
        let mut light_data = DirectionalLightData {
            _light_name: light_name.clone(),
            _light_constants: light_create_info._light_constants.clone(),
            _light_shadow_projection: Matrix4::identity(),
            _transform_object: TransformObjectData::new_transform_object_data(),
            _updated_light_data: true,
            _need_to_redraw_shadow: true,
            _shadow_update_distance: light_create_info._shadow_update_distance,
        };
        light_data._transform_object.set_position(&light_create_info._position);
        light_data._transform_object.set_rotation(&light_create_info._rotation);
        light_data.update_shadow_orthogonal(&light_create_info._shadow_dimensions);
        light_data.update_light_data(&Vector3::zeros());
        light_data
    }

    pub fn get_light_constants(&self) -> &LightConstants { &self._light_constants }
    pub fn get_light_position(&self) -> &Vector3<f32> { self._transform_object.get_position() }
    pub fn get_light_direction(&self) -> &Vector3<f32> { self._transform_object.get_front() }
    pub fn get_light_color(&self) -> &Vector3<f32> { &self._light_constants._light_color }
    pub fn get_light_shadow_samples(&self) -> i32 { self._light_constants._shadow_samples }
    pub fn get_shadow_view_projection(&self) -> &Matrix4<f32> { &self._light_constants._shadow_view_projection }
    pub fn get_need_to_redraw_shadow_and_reset(&mut self) -> bool {
        let need_to_redraw_shadow = self._need_to_redraw_shadow;
        self._need_to_redraw_shadow = false;
        need_to_redraw_shadow
    }

    pub fn update_shadow_orthogonal(&mut self, shadow_dimensions: &Vector4<f32>) {
        let width = shadow_dimensions.x;
        let height = shadow_dimensions.y;
        let near = shadow_dimensions.z;
        let far = shadow_dimensions.w;
        self._light_shadow_projection = get_clip_space_matrix() * orthogonal(-width, width, -height, height, near, far);
        self._updated_light_data = true;
    }

    pub fn update_light_data(&mut self, view_position: &Vector3<f32>) {
        let delta: Vector3<f32> = (self._transform_object.get_position() - view_position).abs();
        if self._shadow_update_distance < delta.max() {
            self._transform_object.set_position(&view_position);
        }

        let updated_transform = self._transform_object.update_transform_object();
        if self._updated_light_data || updated_transform {
            self._light_constants._shadow_view_projection = &self._light_shadow_projection * self._transform_object.get_inverse_matrix();
            self._light_constants._light_direction = self.get_light_direction().clone() as Vector3<f32>;
            self._need_to_redraw_shadow = true;
        }
        self._updated_light_data = false;
    }
}