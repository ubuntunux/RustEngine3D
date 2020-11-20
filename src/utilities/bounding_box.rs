use serde::{ Serialize, Deserialize };
use nalgebra;
use nalgebra::{ Vector3 };

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BoundingBox {
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>,
    pub _center: Vector3<f32>,
    pub _radius: f32
}

impl Default for BoundingBox {
    fn default() -> BoundingBox {
        let min = Vector3::new(-1.0, -1.0, -1.0);
        let max = Vector3::new(1.0, 1.0, 1.0);
        BoundingBox {
            _min: min.clone(),
            _max: max.clone(),
            _center: (max * 0.5 + min * 0.5),
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
        _radius: radius,
    }
}

