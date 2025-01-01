use nalgebra::{Matrix4, Vector3, Vector4};
use serde::{Deserialize, Serialize};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::transform_object::TransformObjectData;

// scene_constants.glsl - struct LIGHT_DATA
#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct LightData {
    pub _shadow_view_projection: Matrix4<f32>,
    pub _light_position: Vector3<f32>,
    pub _shadow_samples: i32,
    pub _light_direction: Vector3<f32>,
    pub _reserved0: i32,
    pub _light_color: Vector3<f32>,
    pub _reserved1: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct DirectionalLightCreateInfo {
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _light_data: LightData,
    pub _shadow_dimensions: Vector4<f32>,
    pub _shadow_update_distance: f32,
}

#[derive(Clone, Debug)]
pub struct DirectionalLight {
    pub _object_id: i64,
    pub _light_name: String,
    pub _light_data: LightData,
    pub _light_shadow_projection: Matrix4<f32>,
    pub _transform_object: TransformObjectData,
    pub _updated_light_data: bool,
    pub _need_to_redraw_shadow: bool,
    pub _shadow_update_distance: f32,
}

// scene_constants.glsl - struct POINT_LIGHT_DATA
#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct PointLightData {
    pub _light_position: Vector3<f32>,
    pub _radius: f32,
    pub _light_color: Vector3<f32>,
    pub _reserved0: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct PointLightCreateInfo {
    pub _light_position: Vector3<f32>,
    pub _radius: f32,
    pub _light_color: Vector3<f32>,
}

#[derive(Clone, Debug)]
pub struct PointLight {
    pub _object_id: i64,
    pub _light_name: String,
    pub _light_data: PointLightData,
    pub _bounding_box: BoundingBox
}