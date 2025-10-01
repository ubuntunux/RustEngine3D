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

                    let mut vector_z: Vector3<f32> = Vector3::new(
                        0.0,
                        height_b - height_t,
                        step_size_z * 2.0
                    ).normalize();

                    if HEIGHT_MAP_INVERT_TEXCOORD_Y {
                        vector_z.z = -vector_z.z;
                    }

                    let mut normal_map = vector_x.cross(&vector_z).normalize();
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
        if HEIGHT_MAP_INVERT_TEXCOORD_Y {
            ((height - 1 - y) * width + x) as usize
        } else {
            (y * width + x) as usize
        }
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
            (pos.z - &self._bounding_box._min.z) / (self._bounding_box._extents.z * 2.0)
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

        let max_lod: usize = (self._lod_count - 2) as usize;
        let bound_box_width: f32 = self._bounding_box._extents.x * 2.0;
        let bound_box_height: f32 = self._bounding_box._extents.z * 2.0;
        let max_ray_dist: f32 = bound_box_width.max(bound_box_height);
        if limit_dist < 0.0 || max_ray_dist < limit_dist {
            limit_dist = max_ray_dist;
        }
        let texcoord_dir: Vector2<f32> = Vector2::new(move_dir.x, move_dir.z).normalize();
        let max_dir: f32 = texcoord_dir.x.abs().max(texcoord_dir.y.abs());
        let max_dist: f32 = max_dir * limit_dist;
        let mut lod: usize = 0.max(max_lod - (max_ray_dist / max_dist).log2().ceil() as usize);

        collision_point.clone_from(start_pos);
        let mut ray_point: Vector3<f32> = start_pos.clone();
        let mut ray_point_prev: Vector3<f32> = start_pos.clone();
        let mut texcoord: Vector2<f32> = self.get_texcoord(&ray_point);
        let mut texcoord_prev: Vector2<f32> = texcoord.clone();
        let mut collision_texcoord: Vector2<f32> = texcoord.clone();
        let goal_texcoord: Vector2<f32> = Vector2::new(
            texcoord.x + texcoord_dir.x * limit_dist / bound_box_width,
            texcoord.y + texcoord_dir.y * limit_dist / bound_box_height
        );
        let mut step: f32 = 1.0 / self._width[lod as usize].max(self._height[lod as usize]) as f32;

        {
            let target_pos = start_pos + move_dir * limit_dist;
            log::info!(">>> start_pos: {:?}, move_dir: {:?}, target_pos: {:?}, height: {:?}", start_pos, move_dir, target_pos, self.get_height_point(&target_pos, 0));
        }

        let mut collided = false;
        let mut debug_loop_count: i32 = 0;
        let limit_loop_count: i32 = max_ray_dist as i32;
        while debug_loop_count < limit_loop_count {
            debug_loop_count += 1;
            let height_value = self.get_height_point_by_texcoord(&texcoord, lod);
            log::info!("\t[{:?}] check height value - lod: {:?}, step: {:?}, texcoord: {:?}, ray_point: {:?}, height: {:?}", debug_loop_count, lod, step, texcoord, ray_point, height_value);

            if ray_point.y <= height_value {
                collision_texcoord.clone_from(&texcoord);
                collision_point.clone_from(&ray_point);
                collided = true;

                if lod == 0 {
                    log::info!("\t[{:?}] Collided - lod: {:?}, step: {:?}, texcoord: {:?}, ray_point: {:?}, height: {:?}", debug_loop_count, lod, step, texcoord, ray_point, height_value);
                    break;
                }

                // collided and go to higher lod
                texcoord.clone_from(&texcoord_prev);
                ray_point.clone_from(&ray_point_prev);
                step *= 0.4;
                lod -= 1;
                log::info!("\t[{:?}] collided and go to higher lod - lod: {:?}, step: {:?}", debug_loop_count, lod, step);
                continue;
            }

            // reset collided
            collided = false;

            // next step
            texcoord_prev.clone_from(&texcoord);
            texcoord += &texcoord_dir * step;
            ray_point_prev.clone_from(&ray_point);
            let ddx: f32 = (((texcoord.x * bound_box_width) + self._bounding_box._min.x - start_pos.x) / texcoord_dir.x).abs();
            ray_point = start_pos + move_dir * ddx;

            // arrive in goal
            if texcoord_dir.dot(&(&goal_texcoord - &texcoord)) < 0.0 {
                texcoord.clone_from(&goal_texcoord);
                log::info!("\t[{:?}] arrive in goal - goal_texcoor: {:?}, ray_point: {:?}", debug_loop_count, goal_texcoord, ray_point);
                break;
            }

            // out of range
            if texcoord.x < 0.0 || texcoord.y < 0.0 || 1.0 < texcoord.x || 1.0 < texcoord.y {
                log::info!("\t[{:?}] out of range", debug_loop_count);
                break;
            }
            log::info!("\t[{:?}] next step - texcoord_prev: {:?}, texcoord: {:?}, ray_point_prev: {:?}, ray_point: {:?}", debug_loop_count, texcoord_prev, texcoord, ray_point_prev, ray_point);
        }

        if collided {
            let height_value = self.get_height_bilinear_by_texcoord(&collision_texcoord, 0);
            collision_point.y = height_value;

            // NOTE: result is smooth but not correct and position spike.
            // let ddy: f32 = ((height_value - start_pos.y) / move_dir.y).abs();
            // *collision_point = start_pos + move_dir * ddy;
        }

        log::info!("\t[{:?}] Finish! collided: {:?}, lod: {:?}, start_pos: {:?}, collision_point: {:?}, move_dir: {:?}, dist: {:?}", debug_loop_count, collided, lod, start_pos, collision_point, move_dir, (start_pos - &*collision_point).norm());
        collided
    }
}