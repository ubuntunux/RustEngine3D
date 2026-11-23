use crate::utilities::math;
use nalgebra;
use nalgebra::{Matrix3, Matrix4, Vector3, Vector4};
use serde::{Deserialize, Serialize};
use std::ops::Index;

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct BoundingBox {
    pub _min: Vector3<f32>,
    pub _max: Vector3<f32>,
    pub _radius: f32,
    pub _mag_xz: f32,
    pub _center: Vector3<f32>,
    pub _extents: Vector3<f32>,
    pub _orientation: Matrix3<f32>,
}

impl Default for BoundingBox {
    fn default() -> BoundingBox {
        BoundingBox::create_bounding_box(&Vector3::new(-1.0, -1.0, -1.0), &Vector3::new(1.0, 1.0, 1.0))
    }
}

impl BoundingBox {
    pub fn create_bounding_box(min: &Vector3<f32>, max: &Vector3<f32>) -> BoundingBox {
        let min_v = math::get_min(min, max);
        let max_v = math::get_max(min, max);
        let center = max_v * 0.5 + min_v * 0.5;
        let extents = (max_v - min_v) * 0.5;
        BoundingBox {
            _min: min_v,
            _max: max_v,
            _radius: extents.magnitude(),
            _mag_xz: (extents.x * extents.x + extents.z * extents.z).sqrt(),
            _center: center,
            _extents: extents,
            _orientation: Matrix3::identity(),
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

    pub fn get_center(&self) -> &Vector3<f32> {
        &self._center
    }

    pub fn get_mag_xz(&self) -> f32 {
        self._mag_xz
    }

    pub fn get_orientation(&self) -> &Matrix3<f32> {
        &self._orientation
    }

    pub fn collide_in_radius(&self, pos: &Vector3<f32>) -> bool {
        (self._center - pos).magnitude_squared() < self._radius * self._radius
    }

    pub fn collide_bound_box(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x
            && min.x < self._max.x
            && self._min.y < max.y
            && min.y < self._max.y
            && self._min.z < max.z
            && min.z < self._max.z
    }

    pub fn collide_bound_box_xy(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x && min.x < self._max.x && self._min.y < max.y && min.y < self._max.y
    }

    pub fn collide_bound_box_xz(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.x < max.x && min.x < self._max.x && self._min.z < max.z && min.z < self._max.z
    }

    pub fn collide_bound_box_yz(&self, min: &Vector3<f32>, max: &Vector3<f32>) -> bool {
        self._min.y < max.y && min.y < self._max.y && self._min.z < max.z && min.z < self._max.z
    }

    pub fn collide_point(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x
            && pos.x <= self._max.x
            && self._min.y <= pos.y
            && pos.y <= self._max.y
            && self._min.z <= pos.z
            && pos.z <= self._max.z
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
        self._min.x <= pos.x && pos.x <= self._max.x && self._min.y <= pos.y && pos.y <= self._max.y
    }

    pub fn collide_point_xz(&self, pos: &Vector3<f32>) -> bool {
        self._min.x <= pos.x && pos.x <= self._max.x && self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn collide_point_yz(&self, pos: &Vector3<f32>) -> bool {
        self._min.y <= pos.y && pos.y <= self._max.y && self._min.z <= pos.z && pos.z <= self._max.z
    }

    pub fn update_axis_aligned_bounding_box(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let center = matrix * Vector4::new(bound_box._center.x, bound_box._center.y, bound_box._center.z, 1.0);
        let ex = bound_box._extents.x.abs();
        let ey = bound_box._extents.y.abs();
        let ez = bound_box._extents.z.abs();
        let world_extents = Vector3::new(
            matrix.m11.abs() * ex + matrix.m12.abs() * ey + matrix.m13.abs() * ez,
            matrix.m21.abs() * ex + matrix.m22.abs() * ey + matrix.m23.abs() * ez,
            matrix.m31.abs() * ex + matrix.m32.abs() * ey + matrix.m33.abs() * ez,
        );

        self._center = Vector3::new(center.x, center.y, center.z);
        self._extents = world_extents;
        self._min = self._center - self._extents;
        self._max = self._center + self._extents;
        self._radius = self._extents.magnitude();
        self._mag_xz = (self._extents.x * self._extents.x + self._extents.z * self._extents.z).sqrt();
        self._orientation = math::extract_axes(matrix) * bound_box._orientation;
    }

    pub fn update_oriented_bounding_box(&mut self, bound_box: &BoundingBox, matrix: &Matrix4<f32>) {
        let rotation = math::extract_axes(matrix);
        let scale = math::extract_scale(matrix);
        let center = matrix * Vector4::new(bound_box._center.x, bound_box._center.y, bound_box._center.z, 1.0);

        self._center = Vector3::new(center.x, center.y, center.z);
        self._extents = bound_box._extents.component_mul(&scale);
        self._radius = self._extents.magnitude();
        self._mag_xz = (self._extents.x * self._extents.x + self._extents.z * self._extents.z).sqrt();
        self._orientation = rotation * bound_box._orientation;

        let world_extents = Vector3::new(
            self._orientation.m11.abs() * self._extents.x
                + self._orientation.m12.abs() * self._extents.y
                + self._orientation.m13.abs() * self._extents.z,
            self._orientation.m21.abs() * self._extents.x
                + self._orientation.m22.abs() * self._extents.y
                + self._orientation.m23.abs() * self._extents.z,
            self._orientation.m31.abs() * self._extents.x
                + self._orientation.m32.abs() * self._extents.y
                + self._orientation.m33.abs() * self._extents.z,
        );
        self._min = self._center - world_extents;
        self._max = self._center + world_extents;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_bounding_box_sorts_inverted_min_max() {
        let bb = BoundingBox::create_bounding_box(&Vector3::new(-1.0, -2.0, 5.0), &Vector3::new(1.0, 2.0, -3.0));
        assert_eq!(bb._min, Vector3::new(-1.0, -2.0, -3.0));
        assert_eq!(bb._max, Vector3::new(1.0, 2.0, 5.0));
        assert_eq!(bb._extents, Vector3::new(1.0, 2.0, 4.0));
    }

    #[test]
    fn test_update_aabb_rotated_45_degrees() {
        let local = BoundingBox::create_bounding_box(&Vector3::new(-1.0, 0.0, -1.0), &Vector3::new(1.0, 2.0, 1.0));
        let rot_45 = math::make_rotation_matrix(0.0, std::f32::consts::FRAC_PI_4, 0.0);
        let mut world = BoundingBox::default();
        world.update_axis_aligned_bounding_box(&local, &rot_45);
        let expected_xz = std::f32::consts::SQRT_2;
        assert!((world._extents.x - expected_xz).abs() < 1e-5);
        assert!((world._extents.z - expected_xz).abs() < 1e-5);
        assert!((world._extents.y - 1.0).abs() < 1e-5);
    }

    #[test]
    fn test_update_obb_rotates_and_scales_offset_center() {
        let local = BoundingBox::create_bounding_box(&Vector3::new(0.0, 0.0, 0.0), &Vector3::new(2.0, 2.0, 2.0));
        // center = (1, 1, 1), extents = (1, 1, 1)
        let m = math::make_srt_transform(
            &Vector3::new(10.0, 0.0, 0.0),
            &Vector3::new(0.0, math::HALF_PI, 0.0),
            &Vector3::new(2.0, 2.0, 2.0),
        );
        let mut obb = BoundingBox::default();
        obb.update_oriented_bounding_box(&local, &m);
        // R_y(+90deg) * (2, 2, 2) = (+2, +2, -2) + (10, 0, 0) = (12, 2, -2)
        assert!((obb._center - Vector3::new(12.0, 2.0, -2.0)).norm() < 1e-4);
        assert!((obb._extents - Vector3::new(2.0, 2.0, 2.0)).norm() < 1e-4);
    }
}
