use serde_json;
use nalgebra::{Vector2, Vector3, Vector4};

pub fn get_json_int(json_data: &serde_json::Value, key: &str, dst: &mut i32) {
    if let Some(serde_json::Value::Number(num_value)) = json_data.get(key) {
        *dst = num_value.as_i64().unwrap() as i32;
    }
}

pub fn get_json_float(json_data: &serde_json::Value, key: &str, dst: &mut f32) {
    if let Some(serde_json::Value::Number(num_value)) = json_data.get(key) {
        *dst = num_value.as_f64().unwrap() as f32;
    }
}

pub fn get_json_vector2(json_data: &serde_json::Value, key: &str, dst: &mut Vector2<f32>) {
    if let Some(serde_json::Value::Array(array)) = json_data.get(key) {
        for (i, value) in array.iter().enumerate() {
            if 2 <= i {
                break;
            }

            if let serde_json::Value::Number(num_value) = value {
                dst[i] = num_value.as_f64().unwrap() as f32;
            }
        }
    }
}

pub fn get_json_vector3(json_data: &serde_json::Value, key: &str, dst: &mut Vector3<f32>) {
    if let Some(serde_json::Value::Array(array)) = json_data.get(key) {
        for (i, value) in array.iter().enumerate() {
            if 3 <= i {
                break;
            }

            if let serde_json::Value::Number(num_value) = value {
                dst[i] = num_value.as_f64().unwrap() as f32;
            }
        }
    }
}

pub fn get_json_vector4(json_data: &serde_json::Value, key: &str, dst: &mut Vector4<f32>) {
    if let Some(serde_json::Value::Array(array)) = json_data.get(key) {
        for (i, value) in array.iter().enumerate() {
            if 4 <= i {
                break;
            }

            if let serde_json::Value::Number(num_value) = value {
                dst[i] = num_value.as_f64().unwrap() as f32;
            }
        }
    }
}