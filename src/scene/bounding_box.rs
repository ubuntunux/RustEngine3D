use std::ops::Index;
use nalgebra;
use nalgebra::{Matrix4, Vector3, Vector4};
use serde::{Deserialize, Serialize};
use crate::utilities::math;

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
#[serde(default)]
pub struct BoundingBox {
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>,
    pub _center: Vector3<f32>,
    pub _size: Vector3<f32>,
    pub _radius: f32,
    pub _transform: Matrix4<f32>
}

impl Default for BoundingBox {
    fn default() -> BoundingBox {
        BoundingBox::create_bounding_box(
            &Vector3::new(-1.0, -1.0, -1.0),
            &Vector3::new(1.0, 1.0, 1.0)
        )
    }
}

impl BoundingBox {
    pub fn create_bounding_box(min: &Vector3<f32>, max: &Vector3<f32>) -> BoundingBox {
        let center = max * 0.5 + min * 0.5;
        let size = max - min;
        BoundingBox {
            _min: min.clone(),
            _max: max.clone(),
            _center: center.clone(),
            _size: size.clone(),
            _radius: size.norm() * 0.5,
            _transform: math::combinate_matrix(
                &center,
                &Matrix4::identity(),
                &(size * 0.5)
            )
        }
    }

    pub fn calc_bounding_box<T: Index<usize, Output = f32>>(positions: &Vec<T>) -> BoundingBox {
        if 0 == positions.len() {
            return BoundingBox::default();
        }

        #[allow(deprecated)]
        let mut bound_min = Vector3::new(f32::MAX, f32::MAX, f32::MAX) * 0.5;
        let mut bound_max = Vector3::new(f32::MIN, f32::MIN, f32::MIN) * 0.5;
        for position in positions.iter() {
            bound_min[0] = bound_min[0].min(position[0]);
            bound_min[1] = bound_min[1].min(position[1]);
            bound_min[2] = bound_min[2].min(position[2]);
            bound_max[0] = bound_max[0].max(position[0]);
            bound_max[1] = bound_max[1].max(position[1]);
            bound_max[2] = bound_max[2].max(position[2]);
        }

        BoundingBox::create_bounding_box(&bound_min, &bound_max)
    }

    pub fn collide_in_radius(&self, pos: &Vector3<f32>) -> bool {
        (self._center - pos).magnitude() < self._radius
    }

    pub fn collide_bound_box(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x && min.x < self._max.x
            && self._min.y < max.y && min.y < self._max.y
            && self._min.z < max.z && min.z < self._max.z
    }

    pub fn collide_bound_box_xy(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x && min.x < self._max.x
            && self._min.y < max.y && min.y < self._max.y
    }

    pub fn collide_bound_box_xz(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x && min.x < self._max.x
            && self._min.z < max.z && min.z < self._max.z
    }

    pub fn collide_bound_box_yz(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.y < max.y && min.y < self._max.y
            && self._min.z < max.z && min.z < self._max.z
    }

    pub fn collide_point(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x && pos.x <= self._max.x
            && self._min.y <= pos.y && pos.y <= self._max.y
            && self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn collide_point_x(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x && pos.x <= self._max.x
    }

    pub fn collide_point_y(&self, pos: &Vector3<f32>) -> bool {
        self._min.y <= pos.y && pos.y <= self._max.y
    }

    pub fn collide_point_z(&self, pos: &Vector3<f32>) -> bool {
        self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn collide_point_xy(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x && pos.x <= self._max.x
            && self._min.y <= pos.y && pos.y <= self._max.y
    }

    pub fn collide_point_xz(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x && pos.x <= self._max.x
            && self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn collide_point_yz(&self, pos: &Vector3<f32>) -> bool {
        self._min.y <= pos.y && pos.y <= self._max.y
            && self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn update_with_matrix(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let pos_min = matrix * Vector4::new(bound_box._min.x, bound_box._min.y, bound_box._min.z, 1.0);
        let pos_max = matrix * Vector4::new(bound_box._max.x, bound_box._max.y, bound_box._max.z, 1.0);
        let min = Vector3::new(
            pos_min.x.min(pos_max.x),
            pos_min.y.min(pos_max.y),
            pos_min.z.min(pos_max.z)
        );
        let max = Vector3::new(
            pos_min.x.max(pos_max.x),
            pos_min.y.max(pos_max.y),
            pos_min.z.max(pos_max.z)
        );
        *self = BoundingBox::create_bounding_box(&min, &max);
    }

    pub fn update_with_matrix_no_rotation(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let location = math::extract_location(&matrix);
        let scale = math::extract_scale(&matrix);
        let matrix = math::combinate_matrix(&location, &Matrix4::identity(), &scale);
        let pos_min = matrix * Vector4::new(bound_box._min.x, bound_box._min.y, bound_box._min.z, 1.0);
        let pos_max = matrix * Vector4::new(bound_box._max.x, bound_box._max.y, bound_box._max.z, 1.0);
        let min = Vector3::new(
            pos_min.x.min(pos_max.x),
            pos_min.y.min(pos_max.y),
            pos_min.z.min(pos_max.z)
        );
        let max = Vector3::new(
            pos_min.x.max(pos_max.x),
            pos_min.y.max(pos_max.y),
            pos_min.z.max(pos_max.z)
        );
        *self = BoundingBox::create_bounding_box(&min, &max);
    }
}
