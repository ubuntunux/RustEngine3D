use crate::scene::bounding_box::BoundingBox;
use crate::utilities::math;
use nalgebra::{Matrix3, Vector3};
use serde::{Deserialize, Serialize};

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
}

impl Default for CollisionCreateInfo {
    fn default() -> CollisionCreateInfo {
        CollisionCreateInfo {
            _collision_type: CollisionType::NONE,
            _location: Vector3::zeros(),
            _extents: Vector3::new(0.5, 0.5, 0.5),
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
            _bounding_box: BoundingBox::default(),
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
        self._bounding_box
            ._orientation
            .column(0)
            .dot(&to_point)
            .abs()
            <= self._bounding_box._extents.x
            && self
            ._bounding_box
            ._orientation
            .column(2)
            .dot(&to_point)
            .abs()
            <= self._bounding_box._extents.z
    }

    pub fn collide_cylinder_with_point(&self, point: &Vector3<f32>) -> bool {
        if self._bounding_box._max.y < point.y || point.y < self._bounding_box._min.y {
            return false;
        }

        let to_point_vec = point - self._bounding_box._center;
        let (to_point_dir_xz, dist_xz) =
            math::make_normalize_with_norm(&Vector3::new(to_point_vec.x, 0.0, to_point_vec.z));

        let d = self
            ._bounding_box
            ._orientation
            .column(0)
            .dot(&to_point_dir_xz);
        let r = math::lerp(
            self._bounding_box._extents.z,
            self._bounding_box._extents.x,
            d,
        );
        dist_xz <= r
    }

    pub fn collide_sphere_with_point(&self, point: &Vector3<f32>) -> bool {
        let sphere_center = self._bounding_box._center;
        let sphere_radius = self._bounding_box._extents.x; // Assuming _extents.x is the radius for a sphere

        let distance_squared = (point - sphere_center).norm_squared();
        distance_squared <= sphere_radius * sphere_radius
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
        if self._collision_type == CollisionType::CYLINDER {
            self.collide_cylinder_with_point(point)
        } else if self._collision_type == CollisionType::SPHERE {
            self.collide_sphere_with_point(point)
        } else {
            self.collide_box_with_point(point)
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

    /// Performs a ray-AABB intersection test using the slab method.
    ///
    /// # Arguments
    ///
    /// * `ray_origin` - The starting point of the ray.
    /// * `ray_direction` - The normalized direction of the ray.
    ///
    /// # Returns
    ///
    /// * `Some(f32)` - The distance to the nearest intersection point, if the ray intersects the AABB.
    /// * `None` - If there is no intersection.
    pub fn ray_vs_aabb(&self, ray_origin: &Vector3<f32>, ray_direction: &Vector3<f32>) -> Option<f32> {
        let min_p = &self._bounding_box._min;
        let max_p = &self._bounding_box._max;

        let mut t_min: f32 = 0.0;
        let mut t_max: f32 = f32::INFINITY;

        for i in 0..3 {
            if ray_direction[i].abs() < f32::EPSILON {
                // Ray is parallel to the slab.
                // If the origin is not within the slab, there is no collision.
                if ray_origin[i] < min_p[i] || ray_origin[i] > max_p[i] {
                    return None;
                }
                // This axis does not contribute to restricting the intersection interval, so we can continue.
            } else {
                let inv_dir = 1.0 / ray_direction[i];
                let mut t1 = (min_p[i] - ray_origin[i]) * inv_dir;
                let mut t2 = (max_p[i] - ray_origin[i]) * inv_dir;

                if t1 > t2 {
                    std::mem::swap(&mut t1, &mut t2);
                }

                t_min = t_min.max(t1);
                t_max = t_max.min(t2);

                if t_min > t_max {
                    return None;
                }
            }
        }

        if t_max < 0.0 {
            // The AABB is behind the ray's origin.
            return None;
        }

        if t_min < 0.0 {
            // Ray origin is inside the box, the nearest intersection is at the origin of the ray.
            Some(0.0)
        } else {
            // Standard intersection, t_min is the nearest intersection point.
            Some(t_min)
        }
    }
}

pub fn collide_box_with_box(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y
        || a._bounding_box._max.y < b._bounding_box._min.y
    {
        return false;
    }

    let a_rotation: &Matrix3<f32> = &a._bounding_box._orientation;
    let b_rotation: &Matrix3<f32> = &b._bounding_box._orientation;

    let a_half_sizes: Vector3<f32> = a._bounding_box._extents;
    let b_half_sizes: Vector3<f32> = b._bounding_box._extents;

    let to_a = a._bounding_box._center - b._bounding_box._center;
    let to_a_vertices: [Vector3<f32>; 4] = [
        to_a + a_rotation.column(0) * a_half_sizes[0] + a_rotation.column(2) * a_half_sizes[2],
        to_a - a_rotation.column(0) * a_half_sizes[0] + a_rotation.column(2) * a_half_sizes[2],
        to_a + a_rotation.column(0) * a_half_sizes[0] - a_rotation.column(2) * a_half_sizes[2],
        to_a - a_rotation.column(0) * a_half_sizes[0] - a_rotation.column(2) * a_half_sizes[2],
    ];
    let to_b_vertices: [Vector3<f32>; 4] = [
        b_rotation.column(0) * b_half_sizes[0] + b_rotation.column(2) * b_half_sizes[2] - to_a,
        -b_rotation.column(0) * b_half_sizes[0] + b_rotation.column(2) * b_half_sizes[2] - to_a,
        b_rotation.column(0) * b_half_sizes[0] - b_rotation.column(2) * b_half_sizes[2] - to_a,
        -b_rotation.column(0) * b_half_sizes[0] - b_rotation.column(2) * b_half_sizes[2] - to_a,
    ];

    for i in [0, 2] {
        let axis = Vector3::new(
            a_rotation.column(i)[0],
            a_rotation.column(i)[1],
            a_rotation.column(i)[2],
        );
        let mut dot_max = f32::MIN;
        let mut dot_min = f32::MAX;
        for to_b_vertex in to_b_vertices {
            let d = axis.dot(&to_b_vertex);
            dot_max = dot_max.max(d);
            dot_min = dot_min.min(d);
        }

        let found_seperate = a_half_sizes[i] < dot_min || dot_max < -a_half_sizes[i];
        if found_seperate {
            return false;
        }
    }

    for i in [0, 2] {
        let axis = Vector3::new(
            b_rotation.column(i)[0],
            b_rotation.column(i)[1],
            b_rotation.column(i)[2],
        );
        let mut dot_max = f32::MIN;
        let mut dot_min = f32::MAX;
        for to_a_vertex in to_a_vertices {
            let d = axis.dot(&to_a_vertex);
            dot_max = dot_max.max(d);
            dot_min = dot_min.min(d);
        }

        let found_seperate = b_half_sizes[i] < dot_min || dot_max < -b_half_sizes[i];
        if found_seperate {
            return false;
        }
    }
    true
}

pub fn collide_box_with_cylinder(a_box: &CollisionData, b_cylinder: &CollisionData) -> bool {
    if b_cylinder._bounding_box._max.y < a_box._bounding_box._min.y
        || a_box._bounding_box._max.y < b_cylinder._bounding_box._min.y
    {
        return false;
    }

    let a_box_axis_x = a_box._bounding_box._orientation.column(0);
    let a_box_axis_z = a_box._bounding_box._orientation.column(2);

    let a_box_half_sizes: Vector3<f32> = a_box._bounding_box._extents;
    let to_a_box =
        math::make_vector_xz(&(a_box._bounding_box._center - b_cylinder._bounding_box._center));

    let b_cylinder_radius_dir = if to_a_box.norm_squared() == 0.0 {
        1.0 // Arbitrary direction, use max radius
    } else {
        b_cylinder
            ._bounding_box
            ._orientation
            .column(0)
            .dot(&to_a_box.normalize())
            .abs()
    };
    let b_cylinder_radius: f32 = math::lerp(
        b_cylinder._bounding_box._extents.z,
        b_cylinder._bounding_box._extents.x,
        b_cylinder_radius_dir,
    );

    let to_box_pos_x0 = to_a_box + a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_x1 = to_a_box - a_box_axis_x * a_box_half_sizes.x;
    let to_box_pos_z0 = to_a_box + a_box_axis_z * a_box_half_sizes.z;
    let to_box_pos_z1 = to_a_box - a_box_axis_z * a_box_half_sizes.z;

    let d_x0 = a_box_axis_x.dot(&to_box_pos_x0);
    let d_x1 = a_box_axis_x.dot(&to_box_pos_x1);
    let distance_x = if d_x0.signum() != d_x1.signum() {
        0.0
    } else {
        d_x0.abs().min(d_x1.abs())
    };

    let d_z0 = a_box_axis_z.dot(&to_box_pos_z0);
    let d_z1 = a_box_axis_z.dot(&to_box_pos_z1);
    let distance_z = if d_z0.signum() != d_z1.signum() {
        0.0
    } else {
        d_z0.abs().min(d_z1.abs())
    };

    (distance_x * distance_x + distance_z * distance_z) <= b_cylinder_radius * b_cylinder_radius
}

pub fn collide_box_with_sphere(a_box: &CollisionData, b_sphere: &CollisionData) -> bool {
    // Sphere's center and radius
    let sphere_center = b_sphere._bounding_box._center;
    let sphere_radius = b_sphere._bounding_box._extents.x; // Assuming _extents.x is the radius for a sphere

    // Box's properties
    let box_center = a_box._bounding_box._center;
    let box_half_extents = a_box._bounding_box._extents;
    let box_orientation = a_box._bounding_box._orientation; // World to local rotation is transpose

    // Transform sphere center to box's local space
    let to_sphere_center_local = box_orientation.transpose() * (sphere_center - box_center);

    // Find the closest point on the box in its local space
    let closest_point_local = Vector3::new(
        to_sphere_center_local.x.max(-box_half_extents.x).min(box_half_extents.x),
        to_sphere_center_local.y.max(-box_half_extents.y).min(box_half_extents.y),
        to_sphere_center_local.z.max(-box_half_extents.z).min(box_half_extents.z),
    );

    // Transform the closest point back to world space
    let closest_point_world = box_orientation * closest_point_local + box_center;

    // Calculate the distance squared from the sphere center to this closest point
    let distance_squared = (sphere_center - closest_point_world).norm_squared();

    // Check for collision
    distance_squared <= sphere_radius * sphere_radius
}

pub fn collide_cylinder_with_cylinder(a: &CollisionData, b: &CollisionData) -> bool {
    if b._bounding_box._max.y < a._bounding_box._min.y
        || a._bounding_box._max.y < b._bounding_box._min.y
    {
        return false;
    }

    let to_a = math::make_vector_xz(&(a._bounding_box._center - b._bounding_box._center));
    let to_a_dir = if to_a.norm_squared() == 0.0 {
        Vector3::new(1.0, 0.0, 0.0) // Arbitrary direction if centers are aligned
    } else {
        to_a.normalize()
    };

    let a_radius_dir = a._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let a_radius: f32 = math::lerp(
        a._bounding_box._extents.z,
        a._bounding_box._extents.x,
        a_radius_dir,
    );
    let b_radius_dir = b._bounding_box._orientation.column(0).dot(&to_a_dir).abs();
    let b_radius: f32 = math::lerp(
        b._bounding_box._extents.z,
        b._bounding_box._extents.x,
        b_radius_dir,
    );
    // Corrected collision check for 2D ellipses
    to_a.norm_squared() <= (a_radius + b_radius) * (a_radius + b_radius)
}

pub fn collide_cylinder_with_sphere(a_cylinder: &CollisionData, b_sphere: &CollisionData) -> bool {
    let cylinder_center = a_cylinder._bounding_box._center;
    let cylinder_half_height = a_cylinder._bounding_box._extents.y;
    let cylinder_orientation = a_cylinder._bounding_box._orientation; // For elliptical base

    let sphere_center = b_sphere._bounding_box._center;
    let sphere_radius = b_sphere._bounding_box._extents.x; // Assuming _extents.x is the radius for a sphere

    // 1. Check Y-axis overlap of bounding boxes
    let cylinder_min_y = cylinder_center.y - cylinder_half_height;
    let cylinder_max_y = cylinder_center.y + cylinder_half_height;
    let sphere_min_y = sphere_center.y - sphere_radius;
    let sphere_max_y = sphere_center.y + sphere_radius;

    if sphere_max_y < cylinder_min_y || cylinder_max_y < sphere_min_y {
        return false; // No Y-axis overlap
    }

    // 2. Find the closest point on the cylinder's central axis (line segment) to the sphere's center
    let closest_y_on_axis = sphere_center.y.max(cylinder_min_y).min(cylinder_max_y);
    let closest_point_on_cylinder_axis = Vector3::new(cylinder_center.x, closest_y_on_axis, cylinder_center.z);

    // 3. Calculate the vector from this closest point to the sphere's center
    let vec_to_sphere_center = sphere_center - closest_point_on_cylinder_axis;

    // 4. Calculate the 2D distance (XZ plane) from the closest point on axis to sphere center
    let dist_xz_squared = vec_to_sphere_center.x * vec_to_sphere_center.x + vec_to_sphere_center.z * vec_to_sphere_center.z;

    // 5. Determine the cylinder's radius at the direction of the sphere's center (in XZ plane)
    let cylinder_effective_radius: f32;
    if dist_xz_squared == 0.0 {
        // Sphere center is directly above/below the closest point on cylinder axis in XZ plane
        // Use max radius for conservative check, or average
        cylinder_effective_radius = a_cylinder._bounding_box._extents.x.max(a_cylinder._bounding_box._extents.z);
    } else {
        let to_sphere_center_xz_norm = Vector3::new(vec_to_sphere_center.x, 0.0, vec_to_sphere_center.z).normalize();
        let d = cylinder_orientation.column(0).dot(&to_sphere_center_xz_norm).abs();
        cylinder_effective_radius = math::lerp(
            a_cylinder._bounding_box._extents.z,
            a_cylinder._bounding_box._extents.x,
            d,
        );
    }

    // 6. Check for collision
    let combined_radius = cylinder_effective_radius + sphere_radius;
    dist_xz_squared <= combined_radius * combined_radius
}

pub fn collide_sphere_with_sphere(a: &CollisionData, b: &CollisionData) -> bool {
    let center_a = a._bounding_box._center;
    let radius_a = a._bounding_box._extents.x; // Assuming _extents.x is the radius for a sphere

    let center_b = b._bounding_box._center;
    let radius_b = b._bounding_box._extents.x; // Assuming _extents.x is the radius for a sphere

    let distance_squared = (center_a - center_b).norm_squared();
    let combined_radius = radius_a + radius_b;

    distance_squared <= combined_radius * combined_radius
}
