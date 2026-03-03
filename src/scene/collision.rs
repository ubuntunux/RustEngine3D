use nalgebra::{Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::bounding_box::BoundingBox;

#[repr(i32)]
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq)]
pub enum CollisionType {
    NONE,
    BOX,
    CYLINDER,
    SPHERE
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct CollisionCreateInfo {
    pub _collision_type: CollisionType,
    pub _location: Vector3<f32>,
    pub _extents: Vector3<f32>
}

impl Default for CollisionCreateInfo {
    fn default() -> CollisionCreateInfo {
        CollisionCreateInfo {
            _collision_type: CollisionType::NONE,
            _location: Vector3::zeros(),
            _extents: Vector3::new(0.5, 0.5, 0.5)
        }
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct CollisionData {
    pub _collision_type: CollisionType,
    pub _bounding_box: BoundingBox,
}

impl Default for CollisionData {
    fn default() -> CollisionData {
        CollisionData {
            _collision_type: CollisionType::NONE,
            _bounding_box: BoundingBox::default()
        }
    }
}

impl CollisionData {
    pub fn create_collision(collision_info: &CollisionCreateInfo) -> CollisionData {
        let pos_min = collision_info._location - collision_info._extents;
        let pos_max = collision_info._location + collision_info._extents;
        CollisionData {
            _collision_type: collision_info._collision_type,
            _bounding_box: BoundingBox::create_bounding_box(&pos_min, &pos_max)
        }
    }

    pub fn get_collision_type(&self) -> CollisionType {
        self._collision_type
    }

    pub fn is_valid_collision(&self) -> bool {
        self._collision_type != CollisionType::NONE
    }

    pub fn collide_box_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }
        let to_point = point - self._bounding_box._center;
        self._bounding_box._orientation.column(0).dot(&to_point).abs() <= self._bounding_box._extents.x &&
            self._bounding_box._orientation.column(2).dot(&to_point).abs() <= self._bounding_box._extents.z
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
        self._bounding_box._min.x < other._bounding_box._max.x && other._bounding_box._min.x < self._bounding_box._max.x &&
            self._bounding_box._min.y < other._bounding_box._max.y && other._bounding_box._min.y < self._bounding_box._max.y &&
            self._bounding_box._min.z < other._bounding_box._max.z && other._bounding_box._min.z < self._bounding_box._max.z
    }

    pub fn collide_point(&self, point: &Vector3<f32>) -> bool {
        match self._collision_type {
            CollisionType::BOX => self.collide_box_with_point(point),
            CollisionType::CYLINDER => self.collide_cylinder_with_point(point),
            CollisionType::SPHERE => self.collide_sphere_with_point(point),
            _ => false
        }
    }

    pub fn collide_collision(&self, other: &CollisionData) -> bool {
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
}

fn get_ellipsoid_radius(dir_local: &Vector3<f32>, extents: &Vector3<f32>) -> f32 {
    let tx = dir_local.x / extents.x;
    let ty = dir_local.y / extents.y;
    let tz = dir_local.z / extents.z;
    let sq = tx*tx + ty*ty + tz*tz;
    if sq < 1e-9 { return extents.x.min(extents.y).min(extents.z); }
    1.0 / sq.sqrt()
}

fn get_ellipse_radius_xz(dir_local_xz: &Vector3<f32>, extents: &Vector3<f32>) -> f32 {
    let tx = dir_local_xz.x / extents.x;
    let tz = dir_local_xz.z / extents.z;
    let sq = tx*tx + tz*tz;
    if sq < 1e-9 { return extents.x.min(extents.z); }
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
        if axis_len_sq < 1e-6 { continue; }
        let axis_norm = axis_xz.normalize();

        let proj_dist = center_dist.dot(&axis_norm).abs();

        let proj_a = (a._bounding_box._orientation.column(0).dot(&axis_norm)).abs() * a._bounding_box._extents.x +
                     (a._bounding_box._orientation.column(2).dot(&axis_norm)).abs() * a._bounding_box._extents.z;

        let proj_b = (b._bounding_box._orientation.column(0).dot(&axis_norm)).abs() * b._bounding_box._extents.x +
                     (b._bounding_box._orientation.column(2).dot(&axis_norm)).abs() * b._bounding_box._extents.z;

        if proj_dist > proj_a + proj_b {
            return false;
        }
    }
    true
}

pub fn collide_box_with_cylinder(a_box: &CollisionData, b_cylinder: &CollisionData) -> bool {
    if a_box._bounding_box._max.y < b_cylinder._bounding_box._min.y || a_box._bounding_box._min.y > b_cylinder._bounding_box._max.y {
        return false;
    }

    let to_cyl = b_cylinder._bounding_box._center - a_box._bounding_box._center;
    let box_inv_rot = a_box._bounding_box._orientation.transpose();
    let cyl_pos_local = box_inv_rot * to_cyl;

    let ext = a_box._bounding_box._extents;
    let closest_local = Vector3::new(
        cyl_pos_local.x.clamp(-ext.x, ext.x),
        cyl_pos_local.y.clamp(-ext.y, ext.y),
        cyl_pos_local.z.clamp(-ext.z, ext.z)
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
    if a_box._bounding_box._max.y < b_sphere._bounding_box._min.y || a_box._bounding_box._min.y > b_sphere._bounding_box._max.y {
        return false;
    }

    let to_sphere = b_sphere._bounding_box._center - a_box._bounding_box._center;
    let box_inv_rot = a_box._bounding_box._orientation.transpose();
    let sphere_pos_local = box_inv_rot * to_sphere;

    let ext = a_box._bounding_box._extents;
    let closest_local = Vector3::new(
        sphere_pos_local.x.clamp(-ext.x, ext.x),
        sphere_pos_local.y.clamp(-ext.y, ext.y),
        sphere_pos_local.z.clamp(-ext.z, ext.z)
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
    if dist_sq < 1e-9 { return true; }

    let dir_a = (a._bounding_box._orientation.transpose() * diff_xz).normalize();
    let ra = get_ellipse_radius_xz(&dir_a, &a._bounding_box._extents);

    let dir_b = (b._bounding_box._orientation.transpose() * diff_xz).normalize();
    let rb = get_ellipse_radius_xz(&dir_b, &b._bounding_box._extents);

    let total_r = ra + rb;
    dist_sq <= total_r * total_r
}

pub fn collide_cylinder_with_sphere(a_cyl: &CollisionData, b_sphere: &CollisionData) -> bool {
    if a_cyl._bounding_box._max.y < b_sphere._bounding_box._min.y || a_cyl._bounding_box._min.y > b_sphere._bounding_box._max.y {
        return false;
    }

    let diff = b_sphere._bounding_box._center - a_cyl._bounding_box._center;
    let diff_xz = Vector3::new(diff.x, 0.0, diff.z);
    let dist_sq = diff_xz.magnitude_squared();
    if dist_sq < 1e-9 { return true; }

    let dir_a = (a_cyl._bounding_box._orientation.transpose() * diff_xz).normalize();
    let ra = get_ellipse_radius_xz(&dir_a, &a_cyl._bounding_box._extents);

    let dir_b = (b_sphere._bounding_box._orientation.transpose() * diff_xz).normalize();
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
    if dist_sq < 1e-9 { return true; }

    let dir_a = (a._bounding_box._orientation.transpose() * diff).normalize();
    let ra = get_ellipsoid_radius(&dir_a, &a._bounding_box._extents);

    let dir_b = (b._bounding_box._orientation.transpose() * diff).normalize();
    let rb = get_ellipsoid_radius(&dir_b, &b._bounding_box._extents);

    let total_r = ra + rb;
    dist_sq <= total_r * total_r
}
