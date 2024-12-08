use std::ops::Index;
use nalgebra;
use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use crate::utilities::math;

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
        // AABB
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

impl BoundingBox {
    pub fn update_with_matrix(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let bound_box_transform = matrix * &bound_box._transform;
        let bound_box_location = &bound_box_transform.column(3).xyz();
        let row_x = &bound_box_transform.row(0);
        let row_y = &bound_box_transform.row(1);
        let row_z = &bound_box_transform.row(2);
        let row_x = Vector3::new(row_x[0], row_x[1], row_x[2]);
        let row_y = Vector3::new(row_y[0], row_y[1], row_y[2]);
        let row_z = Vector3::new(row_z[0], row_z[1], row_z[2]);
        let row_x_negate = -row_x;
        let row_y_negate = -row_y;
        let row_z_negate = -row_z;
        let min = Vector3::new(
            row_x.min().min(row_x_negate.min()),
            row_y.min().min(row_y_negate.min()),
            row_z.min().min(row_z_negate.min()),
        ) + bound_box_location;
        let max = Vector3::new(
            row_x.max().max(row_x_negate.max()),
            row_y.max().max(row_y_negate.max()),
            row_z.max().max(row_z_negate.max()),
        ) + bound_box_location;
        *self = BoundingBox::create_bounding_box(&min, &max);
    }
}
