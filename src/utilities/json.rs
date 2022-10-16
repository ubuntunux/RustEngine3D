use serde_json;
use nalgebra::{Vector2, Vector3, Vector4};
use crate::renderer::push_constants::PushConstantParameter;

pub fn convert_json_value_to_push_constant_parameter(json_data: &serde_json::Map<String, serde_json::Value>, key: &str) -> PushConstantParameter {
    match json_data.get(key) {
        Some(serde_json::Value::Number(num_value)) => {
            if num_value.is_f64() {
                PushConstantParameter::Float(num_value.as_f64().unwrap() as f32)
            } else {
                PushConstantParameter::Int(num_value.as_i64().unwrap() as i32)
            }
        },
        Some(serde_json::Value::Array(array)) => {
            match array.len() {
                2 => PushConstantParameter::Float2(Vector2::new(
                    array[0].as_f64().unwrap() as f32,
                    array[1].as_f64().unwrap() as f32
                )),
                3 => PushConstantParameter::Float3(Vector3::new(
                    array[0].as_f64().unwrap() as f32,
                    array[1].as_f64().unwrap() as f32,
                    array[2].as_f64().unwrap() as f32,
                )),
                4 => PushConstantParameter::Float4(Vector4::new(
                    array[0].as_f64().unwrap() as f32,
                    array[1].as_f64().unwrap() as f32,
                    array[2].as_f64().unwrap() as f32,
                    array[3].as_f64().unwrap() as f32,
                )),
                _ => PushConstantParameter::None
            }
        }
        _ => PushConstantParameter::None
    }
}