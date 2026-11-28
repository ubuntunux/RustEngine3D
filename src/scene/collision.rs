use crate::scene::bounding_box::BoundingBox;
use crate::utilities::math;
use nalgebra::Vector3;
use serde::{Deserialize, Serialize};

pub type CollisionLayer = u32;
pub type CollisionMask = u32;

pub const COLLISION_LAYER_NONE: CollisionLayer = 0;
pub const COLLISION_LAYER_DEFAULT: CollisionLayer = 1 << 0;
pub const COLLISION_LAYER_ALL: CollisionMask = u32::MAX;

#[repr(i32)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum CollisionType {
    NONE,
    BOX,
    CYLINDER,
    SPHERE,
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct CollisionCreateInfo {
    pub _collision_type: CollisionType,
    pub _location: Vector3<f32>,
    pub _extents: Vector3<f32>,
    pub _collision_layer: CollisionLayer,
    pub _collision_mask: CollisionMask,
}

impl Default for CollisionCreateInfo {
    fn default() -> CollisionCreateInfo {
        CollisionCreateInfo {
            _collision_type: CollisionType::NONE,
            _location: Vector3::zeros(),
            _extents: Vector3::new(0.5, 0.5, 0.5),
            _collision_layer: COLLISION_LAYER_DEFAULT,
            _collision_mask: COLLISION_LAYER_ALL,
        }
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct CollisionData {
    pub _collision_type: CollisionType,
    pub _bounding_box: BoundingBox,
    pub _collision_layer: CollisionLayer,
    pub _collision_mask: CollisionMask,
}

impl Default for CollisionData {
    fn default() -> CollisionData {
        CollisionData {
            _collision_type: CollisionType::NONE,
            _bounding_box: BoundingBox::default(),
            _collision_layer: COLLISION_LAYER_DEFAULT,
            _collision_mask: COLLISION_LAYER_ALL,
        }
    }
}

impl CollisionData {
    pub fn create_collision(collision_info: &CollisionCreateInfo) -> CollisionData {
        let pos_min = collision_info._location - collision_info._extents;
        let pos_max = collision_info._location + collision_info._extents;
        CollisionData {
            _collision_type: collision_info._collision_type,
            _bounding_box: BoundingBox::create_bounding_box(&pos_min, &pos_max),
            _collision_layer: collision_info._collision_layer,
            _collision_mask: collision_info._collision_mask,
        }
    }

    pub fn get_collision_type(&self) -> CollisionType {
        self._collision_type
    }

    pub fn is_valid_collision(&self) -> bool {
        self._collision_type != CollisionType::NONE
    }

    pub fn get_collision_layer(&self) -> CollisionLayer {
        self._collision_layer
    }

    pub fn set_collision_layer(&mut self, layer: CollisionLayer) {
        self._collision_layer = layer;
    }

    pub fn get_collision_mask(&self) -> CollisionMask {
        self._collision_mask
    }

    pub fn set_collision_mask(&mut self, mask: CollisionMask) {
        self._collision_mask = mask;
    }

    pub fn matches_layer_mask(&self, query_mask: CollisionMask) -> bool {
        (self._collision_layer & query_mask) != 0
    }

    pub fn can_collide_with(&self, other: &CollisionData) -> bool {
        (self._collision_layer & other._collision_mask) != 0 && (other._collision_layer & self._collision_mask) != 0
    }

    pub fn collide_box_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }
        let to_point = point - self._bounding_box._center;
        self._bounding_box._orientation.column(0).dot(&to_point).abs() <= self._bounding_box._extents.x
            && self._bounding_box._orientation.column(2).dot(&to_point).abs() <= self._bounding_box._extents.z
    }

    pub fn collide_cylinder_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }

        let to_point = point - self._bounding_box._center;
        let to_point_local = self._bounding_box._orientation.transpose() * to_point;

        let rx = self._bounding_box._extents.x;
        let rz = self._bounding_box._extents.z;

        (to_point_local.x / rx).powi(2) + (to_point_local.z / rz).powi(2) <= 1.0
    }

    pub fn collide_sphere_with_point(&self, point: &Vector3<f32>) -> bool {
        let to_point = point - self._bounding_box._center;
        let to_point_local = self._bounding_box._orientation.transpose() * to_point;

        let rx = self._bounding_box._extents.x;
        let ry = self._bounding_box._extents.y;
        let rz = self._bounding_box._extents.z;

        (to_point_local.x / rx).powi(2) + (to_point_local.y / ry).powi(2) + (to_point_local.z / rz).powi(2) <= 1.0
    }

    pub fn collide_aabb(&self, other: &CollisionData) -> bool {
        self._bounding_box._min.x < other._bounding_box._max.x
            && other._bounding_box._min.x < self._bounding_box._max.x
            && self._bounding_box._min.y < other._bounding_box._max.y
            && other._bounding_box._min.y < self._bounding_box._max.y
            && self._bounding_box._min.z < other._bounding_box._max.z
            && other._bounding_box._min.z < self._bounding_box._max.z
    }

    pub fn collide_point(&self, point: &Vector3<f32>) -> bool {
        match self._collision_type {
            CollisionType::BOX => self.collide_box_with_point(point),
            CollisionType::CYLINDER => self.collide_cylinder_with_point(point),
            CollisionType::SPHERE => self.collide_sphere_with_point(point),
            _ => false,
        }
    }

    pub fn collide_ray(&self, origin: &Vector3<f32>, dir: &Vector3<f32>) -> Option<f32> {
        match self._collision_type {
            CollisionType::BOX => self.collide_box_with_ray(origin, dir),
            CollisionType::CYLINDER => self.collide_cylinder_with_ray(origin, dir),
            CollisionType::SPHERE => self.collide_sphere_with_ray(origin, dir),
            CollisionType::NONE => None,
        }
    }

    pub fn collide_ray_with_mask(
        &self,
        origin: &Vector3<f32>,
        dir: &Vector3<f32>,
        layer_mask: CollisionMask,
    ) -> Option<f32> {
        if !self.matches_layer_mask(layer_mask) {
            return None;
        }
        self.collide_ray(origin, dir)
    }

    pub fn collide_sphere_with_ray(&self, origin: &Vector3<f32>, dir: &Vector3<f32>) -> Option<f32> {
        let rx = self._bounding_box._extents.x;
        let ry = self._bounding_box._extents.y;
        let rz = self._bounding_box._extents.z;
        if rx <= 1e-6 || ry <= 1e-6 || rz <= 1e-6 {
            return None;
        }

        let rot_inv = self._bounding_box._orientation.transpose();
        let o_local = rot_inv * (origin - self._bounding_box._center);
        let d_local = rot_inv * dir;

        let o_unit = Vector3::new(o_local.x / rx, o_local.y / ry, o_local.z / rz);
        let d_unit = Vector3::new(d_local.x / rx, d_local.y / ry, d_local.z / rz);

        let a = d_unit.dot(&d_unit);
        if a <= 1e-12 {
            return None;
        }
        let b = o_unit.dot(&d_unit);
        let c = o_unit.dot(&o_unit) - 1.0;

        let disc = b * b - a * c;
        if disc < 0.0 {
            return None;
        }

        let sqrt_disc = disc.sqrt();
        let t0 = (-b - sqrt_disc) / a;
        let t1 = (-b + sqrt_disc) / a;

        if t1 < 0.0 {
            return None;
        }
        Some(if t0 >= 0.0 { t0 } else { 0.0 })
    }

    pub fn collide_cylinder_with_ray(&self, origin: &Vector3<f32>, dir: &Vector3<f32>) -> Option<f32> {
        let rx = self._bounding_box._extents.x;
        let ry = self._bounding_box._extents.y;
        let rz = self._bounding_box._extents.z;
        if rx <= 1e-6 || ry <= 1e-6 || rz <= 1e-6 {
            return None;
        }

        let rot_inv = self._bounding_box._orientation.transpose();
        let o_local = rot_inv * (origin - self._bounding_box._center);
        let d_local = rot_inv * dir;

        // Intersect ray with local Y slab [-ry, +ry]
        let (t_y_min, t_y_max) = if d_local.y.abs() > 1e-6 {
            let t1 = (-ry - o_local.y) / d_local.y;
            let t2 = (ry - o_local.y) / d_local.y;
            if t1 > t2 { (t2, t1) } else { (t1, t2) }
        } else if o_local.y.abs() > ry {
            return None;
        } else {
            (f32::NEG_INFINITY, f32::INFINITY)
        };

        // Intersect ray with infinite local XZ elliptic cylinder (x/rx)^2 + (z/rz)^2 <= 1
        let ox = o_local.x / rx;
        let oz = o_local.z / rz;
        let dx = d_local.x / rx;
        let dz = d_local.z / rz;

        let a = dx * dx + dz * dz;
        let b = ox * dx + oz * dz;
        let c = ox * ox + oz * oz - 1.0;

        let (t_xz_min, t_xz_max) = if a <= 1e-12 {
            if c <= 0.0 {
                (f32::NEG_INFINITY, f32::INFINITY)
            } else {
                return None;
            }
        } else {
            let disc = b * b - a * c;
            if disc < 0.0 {
                return None;
            }
            let sqrt_disc = disc.sqrt();
            ((-b - sqrt_disc) / a, (-b + sqrt_disc) / a)
        };

        let t_enter = t_y_min.max(t_xz_min).max(0.0);
        let t_exit = t_y_max.min(t_xz_max);
        if t_enter <= t_exit && t_exit >= 0.0 {
            Some(t_enter)
        } else {
            None
        }
    }

    pub fn collide_box_with_ray(&self, origin: &Vector3<f32>, dir: &Vector3<f32>) -> Option<f32> {
        let to_origin = origin - self._bounding_box._center;
        let mut t_min = 0.0f32;
        let mut t_max = f32::MAX;

        let axes = [
            self._bounding_box._orientation.column(0),
            self._bounding_box._orientation.column(1),
            self._bounding_box._orientation.column(2),
        ];
        let extents = [
            self._bounding_box._extents.x,
            self._bounding_box._extents.y,
            self._bounding_box._extents.z,
        ];

        for i in 0..3 {
            let axis = axes[i];
            let e = extents[i];
            let f = axis.dot(dir);
            let s = axis.dot(&to_origin);

            if f.abs() > 1e-6 {
                let t1 = (-e - s) / f;
                let t2 = (e - s) / f;
                let (t1, t2) = if t1 > t2 { (t2, t1) } else { (t1, t2) };

                t_min = t_min.max(t1);
                t_max = t_max.min(t2);

                if t_min > t_max {
                    return None;
                }
                if t_max < 0.0 {
                    return None;
                }
            } else {
                if s.abs() > e {
                    return None;
                }
            }
        }

        if t_min >= 0.0 { Some(t_min) } else { None }
    }

    pub fn collide_collision(&self, other: &CollisionData) -> bool {
        if !self.can_collide_with(other) {
            return false;
        }
        if other._collision_type == CollisionType::BOX {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_box(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_box_with_cylinder(other, self);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_box_with_sphere(other, self);
            }
        } else if other._collision_type == CollisionType::CYLINDER {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_cylinder(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_cylinder_with_cylinder(self, other);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_cylinder_with_sphere(other, self);
            }
        } else if other._collision_type == CollisionType::SPHERE {
            if self._collision_type == CollisionType::BOX {
                return collide_box_with_sphere(self, other);
            } else if self._collision_type == CollisionType::CYLINDER {
                return collide_cylinder_with_sphere(self, other);
            } else if self._collision_type == CollisionType::SPHERE {
                return collide_sphere_with_sphere(self, other);
            }
        }
        false
    }

    pub fn push_by_collide(&self, other: &CollisionData) -> Vector3<f32> {
        let mut push_vec = Vector3::zeros();
        let char_center_xz = Vector3::new(self._bounding_box._center.x, 0.0, self._bounding_box._center.z);
        let block_center_xz = Vector3::new(other._bounding_box._center.x, 0.0, other._bounding_box._center.z);

        match other._collision_type {
            CollisionType::BOX => {
                let to_char = char_center_xz - block_center_xz;
                let box_rot_inv = other._bounding_box._orientation.transpose();
                let local_char = box_rot_inv * to_char;
                let ext = other._bounding_box._extents;

                let is_inside = local_char.x.abs() < ext.x && local_char.z.abs() < ext.z;
                let mut normal_world = Vector3::zeros();
                let mut dist_to_surface = 0.0;

                if is_inside {
                    let d_x_pos = ext.x - local_char.x;
                    let d_x_neg = local_char.x - (-ext.x);
                    let d_z_pos = ext.z - local_char.z;
                    let d_z_neg = local_char.z - (-ext.z);

                    let min_d = d_x_pos.min(d_x_neg).min(d_z_pos).min(d_z_neg);
                    let normal_local: Vector3<f32> = if min_d == d_x_pos {
                        Vector3::new(1.0, 0.0, 0.0)
                    } else if min_d == d_x_neg {
                        Vector3::new(-1.0, 0.0, 0.0)
                    } else if min_d == d_z_pos {
                        Vector3::new(0.0, 0.0, 1.0)
                    } else {
                        Vector3::new(0.0, 0.0, -1.0)
                    };

                    normal_world = other._bounding_box._orientation * normal_local;
                    dist_to_surface = min_d;
                } else {
                    let closest_local = Vector3::new(
                        local_char.x.clamp(-ext.x, ext.x),
                        0.0,
                        local_char.z.clamp(-ext.z, ext.z),
                    );
                    let normal_local = local_char - closest_local;
                    let dist = normal_local.norm();
                    if dist > 1e-6 {
                        normal_world = math::safe_normalize(&(other._bounding_box._orientation * normal_local));
                        dist_to_surface = -dist;
                    }
                }

                if normal_world.magnitude_squared() > 0.0 {
                    let penetration = self._bounding_box._mag_xz * 0.5 + dist_to_surface;
                    if penetration > 0.0 {
                        push_vec = normal_world * penetration;
                    }
                }
            }
            CollisionType::CYLINDER | CollisionType::SPHERE => {
                let diff = char_center_xz - block_center_xz;
                let dist = diff.norm();
                let dir = if dist < 1e-6 {
                    Vector3::new(1.0, 0.0, 0.0)
                } else {
                    diff / dist
                };

                let r_char = self._bounding_box._mag_xz * 0.5;
                let r_block = other._bounding_box._mag_xz * 0.5;

                let penetration = (r_char + r_block) - dist;
                if penetration > 0.0 {
                    push_vec = dir * penetration;
                }
            }
            _ => {}
        }
        push_vec
    }
}

fn get_ellipsoid_radius(dir_local: &Vector3<f32>, extents: &Vector3<f32>) -> f32 {
    let tx = dir_local.x / extents.x;
    let ty = dir_local.y / extents.y;
    let tz = dir_local.z / extents.z;
    let sq = tx * tx + ty * ty + tz * tz;
    if sq < 1e-9 {
        return extents.x.min(extents.y).min(extents.z);
    }
    1.0 / sq.sqrt()
}

fn get_ellipse_radius_xz(dir_local_xz: &Vector3<f32>, extents: &Vector3<f32>) -> f32 {
    let tx = dir_local_xz.x / extents.x;
    let tz = dir_local_xz.z / extents.z;
    let sq = tx * tx + tz * tz;
    if sq < 1e-9 {
        return extents.x.min(extents.z);
    }
    1.0 / sq.sqrt()
}

pub fn collide_box_with_box(a: &CollisionData, b: &CollisionData) -> bool {
    if a._bounding_box._max.y < b._bounding_box._min.y || a._bounding_box._min.y > b._bounding_box._max.y {
        return false;
    }

    let center_dist = b._bounding_box._center - a._bounding_box._center;
    let axes = [
        a._bounding_box._orientation.column(0),
        a._bounding_box._orientation.column(2),
        b._bounding_box._orientation.column(0),
        b._bounding_box._orientation.column(2),
    ];

    for axis in axes.iter() {
        let axis_xz = Vector3::new(axis.x, 0.0, axis.z);
        let axis_len_sq = axis_xz.magnitude_squared();
        if axis_len_sq < 1e-6 {
            continue;
        }
        let axis_norm = math::safe_normalize(&axis_xz);

        let proj_dist = center_dist.dot(&axis_norm).abs();

        let proj_a = (a._bounding_box._orientation.column(0).dot(&axis_norm)).abs() * a._bounding_box._extents.x
            + (a._bounding_box._orientation.column(2).dot(&axis_norm)).abs() * a._bounding_box._extents.z;

        let proj_b = (b._bounding_box._orientation.column(0).dot(&axis_norm)).abs() * b._bounding_box._extents.x
            + (b._bounding_box._orientation.column(2).dot(&axis_norm)).abs() * b._bounding_box._extents.z;

        if proj_dist > proj_a + proj_b {
            return false;
        }
    }
    true
}

pub fn collide_box_with_cylinder(a_box: &CollisionData, b_cylinder: &CollisionData) -> bool {
    if a_box._bounding_box._max.y < b_cylinder._bounding_box._min.y
        || a_box._bounding_box._min.y > b_cylinder._bounding_box._max.y
    {
        return false;
    }

    let to_cyl = b_cylinder._bounding_box._center - a_box._bounding_box._center;
    let box_inv_rot = a_box._bounding_box._orientation.transpose();
    let cyl_pos_local = box_inv_rot * to_cyl;

    let ext = a_box._bounding_box._extents;
    let closest_local = Vector3::new(
        cyl_pos_local.x.clamp(-ext.x, ext.x),
        cyl_pos_local.y.clamp(-ext.y, ext.y),
        cyl_pos_local.z.clamp(-ext.z, ext.z),
    );

    let closest_world = a_box._bounding_box._center + a_box._bounding_box._orientation * closest_local;
    let to_closest = closest_world - b_cylinder._bounding_box._center;
    let cyl_inv_rot = b_cylinder._bounding_box._orientation.transpose();
    let pt_in_cyl = cyl_inv_rot * to_closest;

    let rx = b_cylinder._bounding_box._extents.x;
    let rz = b_cylinder._bounding_box._extents.z;

    let dist_sq = (pt_in_cyl.x / rx).powi(2) + (pt_in_cyl.z / rz).powi(2);
    dist_sq <= 1.0
}

pub fn collide_box_with_sphere(a_box: &CollisionData, b_sphere: &CollisionData) -> bool {
    if a_box._bounding_box._max.y < b_sphere._bounding_box._min.y
        || a_box._bounding_box._min.y > b_sphere._bounding_box._max.y
    {
        return false;
    }

    let to_sphere = b_sphere._bounding_box._center - a_box._bounding_box._center;
    let box_inv_rot = a_box._bounding_box._orientation.transpose();
    let sphere_pos_local = box_inv_rot * to_sphere;

    let ext = a_box._bounding_box._extents;
    let closest_local = Vector3::new(
        sphere_pos_local.x.clamp(-ext.x, ext.x),
        sphere_pos_local.y.clamp(-ext.y, ext.y),
        sphere_pos_local.z.clamp(-ext.z, ext.z),
    );

    let closest_world = a_box._bounding_box._center + a_box._bounding_box._orientation * closest_local;
    let to_closest = closest_world - b_sphere._bounding_box._center;
    let sphere_inv_rot = b_sphere._bounding_box._orientation.transpose();
    let pt_in_sphere = sphere_inv_rot * to_closest;

    let rx = b_sphere._bounding_box._extents.x;
    let ry = b_sphere._bounding_box._extents.y;
    let rz = b_sphere._bounding_box._extents.z;

    let dist_sq = (pt_in_sphere.x / rx).powi(2) + (pt_in_sphere.y / ry).powi(2) + (pt_in_sphere.z / rz).powi(2);
    dist_sq <= 1.0
}

pub fn collide_cylinder_with_cylinder(a: &CollisionData, b: &CollisionData) -> bool {
    if a._bounding_box._max.y < b._bounding_box._min.y || a._bounding_box._min.y > b._bounding_box._max.y {
        return false;
    }

    let diff = b._bounding_box._center - a._bounding_box._center;
    let diff_xz = Vector3::new(diff.x, 0.0, diff.z);
    let dist_sq = diff_xz.magnitude_squared();
    if dist_sq < 1e-9 {
        return true;
    }

    let dir_a = math::safe_normalize(&(a._bounding_box._orientation.transpose() * diff_xz));
    let ra = get_ellipse_radius_xz(&dir_a, &a._bounding_box._extents);

    let dir_b = math::safe_normalize(&(b._bounding_box._orientation.transpose() * diff_xz));
    let rb = get_ellipse_radius_xz(&dir_b, &b._bounding_box._extents);

    let total_r = ra + rb;
    dist_sq <= total_r * total_r
}

pub fn collide_cylinder_with_sphere(a_cyl: &CollisionData, b_sphere: &CollisionData) -> bool {
    if a_cyl._bounding_box._max.y < b_sphere._bounding_box._min.y
        || a_cyl._bounding_box._min.y > b_sphere._bounding_box._max.y
    {
        return false;
    }

    let diff = b_sphere._bounding_box._center - a_cyl._bounding_box._center;
    let diff_xz = Vector3::new(diff.x, 0.0, diff.z);
    let dist_sq = diff_xz.magnitude_squared();
    if dist_sq < 1e-9 {
        return true;
    }

    let dir_a = math::safe_normalize(&(a_cyl._bounding_box._orientation.transpose() * diff_xz));
    let ra = get_ellipse_radius_xz(&dir_a, &a_cyl._bounding_box._extents);

    let dir_b = math::safe_normalize(&(b_sphere._bounding_box._orientation.transpose() * diff_xz));
    let rb = get_ellipse_radius_xz(&dir_b, &b_sphere._bounding_box._extents);

    let total_r = ra + rb;
    dist_sq <= total_r * total_r
}

pub fn collide_sphere_with_sphere(a: &CollisionData, b: &CollisionData) -> bool {
    if a._bounding_box._max.y < b._bounding_box._min.y || a._bounding_box._min.y > b._bounding_box._max.y {
        return false;
    }

    let diff = b._bounding_box._center - a._bounding_box._center;
    let dist_sq = diff.magnitude_squared();
    if dist_sq < 1e-9 {
        return true;
    }

    let dir_a = math::safe_normalize(&(a._bounding_box._orientation.transpose() * diff));
    let ra = get_ellipsoid_radius(&dir_a, &a._bounding_box._extents);

    let dir_b = math::safe_normalize(&(b._bounding_box._orientation.transpose() * diff));
    let rb = get_ellipsoid_radius(&dir_b, &b._bounding_box._extents);

    let total_r = ra + rb;
    dist_sq <= total_r * total_r
}

#[cfg(test)]
mod tests {
    use super::*;
    use nalgebra::Matrix4;

    #[test]
    fn test_raycast_sphere_and_ellipsoid() {
        let info = CollisionCreateInfo {
            _collision_type: CollisionType::SPHERE,
            _location: Vector3::new(0.0, 0.0, 5.0),
            _extents: Vector3::new(1.0, 2.0, 1.0),
            ..Default::default()
        };
        let col = CollisionData::create_collision(&info);

        let hit = col.collide_ray(&Vector3::zeros(), &Vector3::new(0.0, 0.0, 1.0));
        assert!((hit.expect("center ray should hit ellipsoid") - 4.0).abs() < 1e-4);

        let hit_high = col.collide_ray(&Vector3::new(0.0, 1.5, 0.0), &Vector3::new(0.0, 0.0, 1.0));
        assert!(hit_high.is_some());

        let miss = col.collide_ray(&Vector3::new(1.5, 0.0, 0.0), &Vector3::new(0.0, 0.0, 1.0));
        assert!(miss.is_none());

        // Ray starting inside returns 0.0
        let inside = col.collide_ray(&Vector3::new(0.0, 0.0, 5.0), &Vector3::new(0.0, 0.0, 1.0));
        assert_eq!(inside, Some(0.0));
    }

    #[test]
    fn test_raycast_cylinder_side_caps_and_rotated() {
        let info = CollisionCreateInfo {
            _collision_type: CollisionType::CYLINDER,
            _location: Vector3::new(0.0, 2.0, 6.0),
            _extents: Vector3::new(1.0, 2.0, 1.0),
            ..Default::default()
        };
        let mut col = CollisionData::create_collision(&info);

        // Hit cylinder side wall at z = 5.0
        let side_hit = col.collide_ray(&Vector3::new(0.0, 2.0, 0.0), &Vector3::new(0.0, 0.0, 1.0));
        assert!((side_hit.expect("side ray should hit") - 5.0).abs() < 1e-4);

        // Hit top cap from above (y = 10 -> y = 4, dist = 6)
        let cap_hit = col.collide_ray(&Vector3::new(0.0, 10.0, 6.0), &Vector3::new(0.0, -1.0, 0.0));
        assert!((cap_hit.expect("top cap ray should hit") - 6.0).abs() < 1e-4);

        // Miss above cylinder
        let miss_above = col.collide_ray(&Vector3::new(0.0, 5.0, 0.0), &Vector3::new(0.0, 0.0, 1.0));
        assert!(miss_above.is_none());

        // Rotate cylinder 90 deg around Z so its local Y axis lies along X (extents X=1, Y=2 -> rotated X=2, Y=1)
        let rot_z_90 = Matrix4::new_rotation(Vector3::new(0.0, 0.0, std::f32::consts::FRAC_PI_2));
        let transform = Matrix4::new_translation(&Vector3::new(0.0, 2.0, 6.0)) * rot_z_90;
        let local_min = Vector3::new(-1.0, -2.0, -1.0);
        let local_max = Vector3::new(1.0, 2.0, 1.0);
        let local_box = BoundingBox::create_bounding_box(&local_min, &local_max);
        col._bounding_box.update_oriented_bounding_box(&local_box, &transform);

        let rotated_hit = col.collide_ray(&Vector3::new(1.5, 2.0, 0.0), &Vector3::new(0.0, 0.0, 1.0));
        assert!((rotated_hit.expect("rotated cylinder should hit") - 5.0).abs() < 1e-3);
    }

    #[test]
    fn test_collision_layer_and_mask_filtering() {
        const LAYER_WORLD: CollisionLayer = 1 << 0;
        const LAYER_ENEMY: CollisionLayer = 1 << 1;
        const LAYER_TRIGGER: CollisionLayer = 1 << 2;

        let world_box = CollisionData::create_collision(&CollisionCreateInfo {
            _collision_type: CollisionType::BOX,
            _location: Vector3::new(0.0, 0.0, 5.0),
            _extents: Vector3::new(1.0, 1.0, 1.0),
            _collision_layer: LAYER_WORLD,
            _collision_mask: LAYER_WORLD | LAYER_ENEMY,
        });
        let trigger_box = CollisionData::create_collision(&CollisionCreateInfo {
            _collision_type: CollisionType::BOX,
            _location: Vector3::new(0.0, 0.0, 5.0),
            _extents: Vector3::new(1.0, 1.0, 1.0),
            _collision_layer: LAYER_TRIGGER,
            _collision_mask: LAYER_ENEMY,
        });

        assert!(!world_box.can_collide_with(&trigger_box));
        assert!(!world_box.collide_collision(&trigger_box));
        assert!(
            world_box.collide_ray_with_mask(&Vector3::zeros(), &Vector3::new(0.0, 0.0, 1.0), LAYER_WORLD).is_some()
        );
        assert!(
            world_box.collide_ray_with_mask(&Vector3::zeros(), &Vector3::new(0.0, 0.0, 1.0), LAYER_ENEMY).is_none()
        );
    }

    #[test]
    fn test_serde_legacy_collision_defaults() {
        let json = r#"{"_collision_type":"CYLINDER","_location":[1.0,2.0,3.0],"_extents":[0.5,1.0,0.5]}"#;
        let parsed: CollisionCreateInfo = serde_json::from_str(json).expect("legacy json should deserialize");
        assert_eq!(parsed._collision_layer, COLLISION_LAYER_DEFAULT);
        assert_eq!(parsed._collision_mask, COLLISION_LAYER_ALL);
    }
}
