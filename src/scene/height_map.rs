use nalgebra::{Vector2, Vector3, Vector4};
use crate::utilities::math;
use crate::scene::bounding_box::BoundingBox;

const HEIGHT_MAP_INVERT_TEXCOORD_Y: bool = true;

#[derive(Clone, Default)]
pub struct HeightMapData {
    _dead_zone: f32,
    _bounding_box: BoundingBox,
    _lod_count: i32,
    _width: Vec<i32>,
    _height: Vec<i32>,
    _normal_map_data: Vec<Vector3<f32>>,
    _height_map_data: Vec<Vec<f32>>,
    _initialized: bool,
}

impl HeightMapData {
    pub fn initialize_height_map_data(
        &mut self,
        bounding_box: &BoundingBox,
        width: i32,
        height: i32,
        normal_map_data: &Vec<Vector4<u8>>,
        height_map_data: &Vec<f32>,
        dead_zone: f32
    ) {
        self._dead_zone = dead_zone;
        self._bounding_box = bounding_box.clone();
        let max_height = bounding_box._extents.y * 2.0;
        let lod_count_x = (width as f32).log2() as i32 + 1;
        let lod_count_y = (height as f32).log2() as i32 + 1;
        self._lod_count = lod_count_x.min(lod_count_y);
        assert!(2 <= self._lod_count, "lod_count must be greater than 2.");

        // generate dimension mips
        self._width.clear();
        self._height.clear();
        for lod in 0..self._lod_count {
            self._width.push(width / 2_i32.pow(lod as u32));
            self._height.push(height / 2_i32.pow(lod as u32));
        }

        // generate base height map
        let min_height = self._dead_zone - self._bounding_box._min.y;
        let mut lod_height_map_data: Vec<f32> = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                let pixel_index = self.convert_to_pixel_index(width, height, x, y);
                let mut height = height_map_data[pixel_index] * max_height;
                if height == 0.0 {
                    height = min_height;
                }
                lod_height_map_data.push(min_height.max(height));
            }
        }
        self._height_map_data.reserve(self._lod_count as usize);
        self._height_map_data.clear();
        self._height_map_data.push(lod_height_map_data);

        // generate height map mips
        for lod in 1..self._lod_count as usize {
            let parent_width = self._width[lod - 1];
            let parent_height = self._height[lod - 1];
            let last_height_map_data = &self._height_map_data.last().unwrap();
            let mut lod_height_map_data: Vec<f32> = Vec::with_capacity(((parent_width / 2) * (parent_height / 2)) as usize);
            for y in (0..parent_height).step_by(2) {
                for x in (0..parent_width).step_by(2) {
                    let tex_coord_0 = self.convert_to_pixel_index(parent_width, parent_height, x, y);
                    let tex_coord_1 = self.convert_to_pixel_index(parent_width, parent_height, x, y + 1);
                    let height_00 = last_height_map_data[tex_coord_0];
                    let height_01 = last_height_map_data[tex_coord_0 + 1];
                    let height_10 = last_height_map_data[tex_coord_1];
                    let height_11 = last_height_map_data[tex_coord_1 + 1];
                    let max_height = height_00.max(height_01.max(height_10.max(height_11)));
                    lod_height_map_data.push(max_height);
                }
            }
            self._height_map_data.push(lod_height_map_data);
        }

        // generate normal map data
        let step_size_x: f32 = self._bounding_box._extents.x * 2.0 / (width - 1) as f32;
        let step_size_z: f32 = self._bounding_box._extents.z * 2.0 / (height - 1) as f32;
        self._normal_map_data.reserve((width * height) as usize);
        self._normal_map_data.clear();
        if normal_map_data.is_empty() {
            for y in 0..height {
                for x in 0..width {
                    let height_r = self._height_map_data[0][(y * width + (x + 1).min(width - 1)) as usize];
                    let height_l = self._height_map_data[0][(y * width + (x - 1).max(0)) as usize];
                    let height_t = self._height_map_data[0][((y + 1).min(height - 1) * width + x) as usize];
                    let height_b = self._height_map_data[0][((y - 1).max(0) * width + x) as usize];

                    let vector_x: Vector3<f32> = Vector3::new(
                        step_size_x * 2.0,
                        height_r - height_l,
                        0.0
                    ).normalize();

                    let vector_z: Vector3<f32> = Vector3::new(
                        0.0,
                        height_b - height_t,
                        step_size_z * 2.0
                    ).normalize();

                    let mut normal_map = if HEIGHT_MAP_INVERT_TEXCOORD_Y {
                        vector_z.cross(&vector_x).normalize()
                    } else {
                        vector_x.cross(&vector_z).normalize()
                    };
                    if height_r == 0.0 || height_l == 0.0 || height_t == 0.0 || height_b == 0.0 {
                        normal_map.y = 0.0;
                        math::safe_normalize_mut(&mut normal_map);
                    }
                    self._normal_map_data.push(normal_map);
                }
            }
        } else {
            for y in 0..height {
                for x in 0..width {
                    let normal_vector = normal_map_data[(y * width + x) as usize];
                    self._normal_map_data.push(Vector3::new(
                        normal_vector.x as f32 * 2.0 - 1.0,
                        normal_vector.y as f32 * 2.0 - 1.0,
                        normal_vector.z as f32 * 2.0 - 1.0
                    ).normalize());
                }
            }
        }
        self._initialized = true;
    }

    pub fn convert_to_pixel_index(&self, width: i32, height: i32, x: i32, y: i32) -> usize {
        (y * width + x) as usize
    }

    pub fn get_bilinear_pixel_pos_infos(&self, texcoord: &Vector2<f32>, lod: usize) -> (Vector4<usize>, Vector2<f32>) {
        let width = self._width[lod];
        let height = self._height[lod];
        let pixel_pos_x: f32 = 0f32.max(1f32.min(texcoord.x)) * (width - 1) as f32;
        let pixel_pos_y: f32 = 0f32.max(1f32.min(texcoord.y)) * (height - 1) as f32;
        let pixel_pos_x_frac: f32 = pixel_pos_x.fract();
        let pixel_pos_y_frac: f32 = pixel_pos_y.fract();
        let pixel_pos_x_min: i32 = pixel_pos_x as i32;
        let pixel_pos_y_min: i32 = pixel_pos_y as i32 * width;
        let pixel_pos_x_max: i32 = pixel_pos_x.ceil() as i32;
        let pixel_pos_y_max: i32 = pixel_pos_y.ceil() as i32 * width;
        let pixel_indices: Vector4<usize> = Vector4::new(
            (pixel_pos_y_min + pixel_pos_x_min) as usize,
            (pixel_pos_y_min + pixel_pos_x_max) as usize,
            (pixel_pos_y_max + pixel_pos_x_min) as usize,
            (pixel_pos_y_max + pixel_pos_x_max) as usize
        );
        (pixel_indices, Vector2::new(pixel_pos_x_frac, pixel_pos_y_frac))
    }

    pub fn get_texcoord(&self, pos: &Vector3<f32>) -> Vector2<f32> {
        Vector2::new(
            (pos.x - &self._bounding_box._min.x) / (self._bounding_box._extents.x * 2.0),
            if HEIGHT_MAP_INVERT_TEXCOORD_Y {
                1.0 - ((pos.z - &self._bounding_box._min.z) / (self._bounding_box._extents.z * 2.0))
            } else {
                (pos.z - &self._bounding_box._min.z) / (self._bounding_box._extents.z * 2.0)
            }
        )
    }

    pub fn get_height_bilinear(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self.get_height_bilinear_by_texcoord(&self.get_texcoord(pos), lod)
    }

    pub fn get_height_point(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self.get_height_point_by_texcoord(&self.get_texcoord(pos), lod)
    }

    pub fn get_height_bilinear_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
        if self._initialized == false {
            return 0.0;
        }

        let lod = lod.min(self._lod_count as usize - 1);
        let (pixel_indices, blend_factors) = self.get_bilinear_pixel_pos_infos(texcoord, lod);
        let height_map_data = &self._height_map_data[lod];
        let height: f32;
        if height_map_data[pixel_indices.x] <= self._dead_zone ||
            height_map_data[pixel_indices.y] <= self._dead_zone ||
            height_map_data[pixel_indices.z] <= self._dead_zone ||
            height_map_data[pixel_indices.w] <= self._dead_zone {
            height = self._dead_zone;
        } else {
            let height_data_0 = math::lerp(height_map_data[pixel_indices.x], height_map_data[pixel_indices.y], blend_factors.x);
            let height_data_1 = math::lerp(height_map_data[pixel_indices.z], height_map_data[pixel_indices.w], blend_factors.x);
            height = self._bounding_box._min.y + math::lerp(height_data_0, height_data_1, blend_factors.y);
        }

        self._dead_zone.max(height)
    }

    pub fn get_height_point_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
        if self._initialized == false {
            return 0.0;
        }

        let lod = lod.min(self._lod_count as usize - 1);
        let width = self._width[lod];
        let height = self._height[lod];
        let pixel_pos_x: i32 = (0f32.max(1f32.min(texcoord.x)) * (width as f32 - 1.0)).round() as i32;
        let pixel_pos_y: i32 = (0f32.max(1f32.min(texcoord.y)) * (height as f32 - 1.0)).round() as i32;
        let pixel_index: usize = (pixel_pos_x + pixel_pos_y * width) as usize;
        let height_map_value = self._bounding_box._min.y + self._height_map_data[lod][pixel_index];
        self._dead_zone.max(height_map_value)
    }

    pub fn get_normal_bilinear_by_texcoord(&self, texcoord: &Vector2<f32>) -> Vector3<f32> {
        if self._initialized == false {
            return Vector3::new(0.0, 1.0, 0.0);
        }

        let lod = 0;
        let (pixel_indices, blend_factors) = self.get_bilinear_pixel_pos_infos(texcoord, lod);
        let height_map_data = &self._height_map_data[lod];
        if height_map_data[pixel_indices.x] <= self._dead_zone {
            return self._normal_map_data[pixel_indices.x];
        } else if height_map_data[pixel_indices.y] <= self._dead_zone {
            return self._normal_map_data[pixel_indices.y];
        } else if height_map_data[pixel_indices.z] <= self._dead_zone {
            return self._normal_map_data[pixel_indices.z];
        } else if height_map_data[pixel_indices.w] <= self._dead_zone {
            return self._normal_map_data[pixel_indices.w];
        }

        let height_data_0: Vector3<f32> = self._normal_map_data[pixel_indices.x].lerp(&self._normal_map_data[pixel_indices.y], blend_factors.x);
        let height_data_1: Vector3<f32> = self._normal_map_data[pixel_indices.z].lerp(&self._normal_map_data[pixel_indices.w], blend_factors.x);
        height_data_0.lerp(&height_data_1, blend_factors.y).normalize()
    }

    pub fn get_normal_point_by_texcoord(&self, texcoord: &Vector2<f32>) -> Vector3<f32> {
        if self._initialized == false {
            return Vector3::new(0.0, 1.0, 0.0);
        }

        let lod = 0;
        let width = self._width[lod];
        let height = self._height[lod];
        let pixel_pos_x: i32 = (0f32.max(1f32.min(texcoord.x)) * (width as f32 - 1.0)).round() as i32;
        let pixel_pos_y: i32 = (0f32.max(1f32.min(texcoord.y)) * (height as f32 - 1.0)).round() as i32;
        let pixel_index: usize = (pixel_pos_x + pixel_pos_y * width) as usize;
        self._normal_map_data[pixel_index]
    }

    pub fn get_normal_point(&self, pos: &Vector3<f32>) -> Vector3<f32> {
        self.get_normal_point_by_texcoord(&self.get_texcoord(pos))
    }

    pub fn get_normal_bilinear(&self, pos: &Vector3<f32>) -> Vector3<f32> {
        self.get_normal_bilinear_by_texcoord(&self.get_texcoord(pos))
    }

    pub fn get_collision_point(&self, start_pos: &Vector3<f32>, move_dir: &Vector3<f32>, mut limit_dist: f32, collision_point: &mut Vector3<f32>) -> bool {
        if self._initialized == false {
            return false;
        }

        let bound_box_width: f32 = self._bounding_box._extents.x * 2.0;
        let bound_box_height: f32 = self._bounding_box._extents.z * 2.0;
        let max_ray_dist: f32 = self._bounding_box._mag_xz;
        if limit_dist < 0.0 || max_ray_dist < limit_dist {
            limit_dist = max_ray_dist;
        }

        let side_x: f32 = move_dir.x.abs() * limit_dist;
        let side_z: f32 = move_dir.z.abs() * limit_dist;
        let inv_lod_x: usize = if 0.0 < side_x { (bound_box_width / side_x).log2().floor() as usize } else { 0 };
        let inv_lod_z: usize = if 0.0 < side_z { (bound_box_height / side_z).log2().floor() as usize } else { 0 };
        let max_lod: usize = if 2 <= self._lod_count { (self._lod_count - 2) as usize } else { 0 };
        let mut lod: usize = max_lod - max_lod.min(inv_lod_x.min(inv_lod_z));

        collision_point.clone_from(start_pos);
        let mut ray_point: Vector3<f32> = start_pos.clone();
        let mut ray_point_prev: Vector3<f32> = start_pos.clone();

        const MIN_STEP: f32 = 0.01;
        let step_x: f32 = (bound_box_width / self._width[lod as usize] as f32) / if move_dir.x != 0.0 { move_dir.x.abs() } else { 1.0 };
        let step_z: f32 = (bound_box_height / self._height[lod as usize] as f32) / if move_dir.z != 0.0 { move_dir.z.abs() } else { 1.0 };
        let mut step: f32 = step_x.min(step_z);

        let mut distance: f32 = 0.0;
        let mut distance_prev: f32 = 0.0;

        let mut collided = false;
        let mut debug_loop_count: i32 = 0;
        let loop_count_limit_x = (max_ray_dist / bound_box_width * self._width[0] as f32) as i32;
        let loop_count_limit_y = (max_ray_dist / bound_box_height * self._height[0] as f32) as i32;
        let limit_loop_count: i32 = loop_count_limit_x.max(loop_count_limit_y);
        while debug_loop_count < limit_loop_count {
            debug_loop_count += 1;

            let height_value = if lod == 0 {
                self.get_height_bilinear(&ray_point, lod)
            } else {
                self.get_height_point(&ray_point, lod)
            };

            if ray_point.y <= height_value {
                collision_point.clone_from(&ray_point_prev);
                collided = true;

                // collided and go to higher lod
                ray_point.clone_from(&ray_point_prev);
                distance = distance_prev;
                step *= 0.5;

                if lod == 0 && step < MIN_STEP {
                    break;
                } else if 0 < lod {
                    lod -= 1;
                }
                continue;
            }

            //
            collided = false;

            // next step
            ray_point_prev.clone_from(&ray_point);
            distance_prev = distance;
            distance += step;
            ray_point = start_pos + move_dir * distance;

            // arrive in goal
            if limit_dist <= distance_prev || ray_point_prev.x < self._bounding_box._min.x || ray_point_prev.z < self._bounding_box._min.z || self._bounding_box._max.x < ray_point_prev.x || self._bounding_box._max.z < ray_point_prev.z {
                break;
            }
        }

        log::info!("\t[{:?}] Finish! collided: {:?}, lod: {:?}, start_pos: {:?}, collision_point: {:?}, move_dir: {:?}, dist: {:?}", debug_loop_count, collided, lod, start_pos, collision_point, move_dir, (start_pos - &*collision_point).norm());
        collided
    }
}