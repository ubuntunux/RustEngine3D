use serde::{ Serialize, Deserialize };
use nalgebra;
use nalgebra::{Vector3, Vector4, Matrix4};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct BoundingBox {
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>,
    pub _center: Vector3<f32>,
    pub _size: Vector3<f32>,
    pub _radius: f32
}

impl Default for BoundingBox {
    fn default() -> BoundingBox {
        let min = Vector3::new(-1.0, -1.0, -1.0);
        let max = Vector3::new(1.0, 1.0, 1.0);
        BoundingBox {
            _min: min.clone(),
            _max: max.clone(),
            _center: max * 0.5 + min * 0.5,
            _size: max - min,
            _radius: (max * 0.5 - min * 0.5).norm()
        }
    }
}

pub fn calc_bounding_box(positions: &Vec<Vector3<f32>>) -> BoundingBox {
    if 0 == positions.len() {
        return BoundingBox::default();
    }

    let mut bound_min = Vector3::new(std::f32::MAX, std::f32::MAX, std::f32::MAX) * 0.5;
    let mut bound_max = Vector3::new(std::f32::MIN, std::f32::MIN, std::f32::MIN) * 0.5;
    for position in positions.iter() {
        bound_min[0] = bound_min[0].min(position[0]);
        bound_min[1] = bound_min[1].min(position[1]);
        bound_min[2] = bound_min[2].min(position[2]);
        bound_max[0] = bound_max[0].max(position[0]);
        bound_max[1] = bound_max[1].max(position[1]);
        bound_max[2] = bound_max[2].max(position[2]);
    }

    let center: Vector3<f32> = bound_min * 0.5 + bound_max * 0.5;
    let radius: f32 = (bound_max * 0.5 - bound_min * 0.5).norm();

    BoundingBox {
        _min: bound_min,
        _max: bound_max,
        _center: center,
        _size: (bound_max - bound_min),
        _radius: radius,
    }
}

impl BoundingBox {
    pub fn update_with_matrix(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let bound_min: Vector4<f32> = matrix * Vector4::new(bound_box._min.x, bound_box._min.y, bound_box._min.z, 1.0);
        let bound_max: Vector4<f32> = matrix * Vector4::new(bound_box._max.x, bound_box._max.y, bound_box._max.z, 1.0);
        self._min = Vector3::new(bound_min.x.min(bound_max.x), bound_min.y.min(bound_max.y), bound_min.z.min(bound_max.z));
        self._max = Vector3::new(bound_min.x.max(bound_max.x), bound_min.y.max(bound_max.y), bound_min.z.max(bound_max.z));
        self._center = &self._min * 0.5 + &self._max * 0.5;
        self._size = &self._max - &self._min;
        self._radius = (&self._max * 0.5 - &self._min * 0.5).norm();
    }
}

