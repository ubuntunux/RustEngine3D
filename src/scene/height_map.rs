use nalgebra::{Vector2, Vector3, Vector4};
use crate::constants::HEIGHT_MAP_INVERT_TEXCOORD_Y;
use crate::utilities::math;
use crate::scene::bounding_box::BoundingBox;

#[derive(Clone, Default)]
pub struct HeightMapData {
    _sea_height: f32,
    _bounding_box: BoundingBox,
    _lod_count: i32,
    _width: Vec<i32>,
    _height: Vec<i32>,
    _normal_map_data: Vec<Vec<Vector3<f32>>>,
    _height_map_data: Vec<Vec<f32>>,
    _initialiezed: bool,
}

impl HeightMapData {
    pub fn initialize_height_map_data(&mut self, bounding_box: &BoundingBox, width: i32, height: i32, normal_map_data: &Vec<Vector4<u8>>, height_map_data: &Vec<f32>, sea_height: f32) {
        self._sea_height = sea_height;
        self._bounding_box = bounding_box.clone();
        let max_height = bounding_box._size.y;
        let lod_count_x = (width as f32).log2() as i32 + 1;
        let lod_count_y = (height as f32).log2() as i32 + 1;
        self._lod_count = lod_count_x.min(lod_count_y);
        assert!(2 <= self._lod_count, "lod_count must be greater than 2.");

        for lod in 0..self._lod_count {
            self._width.push(width / 2_i32.pow(lod as u32));
            self._height.push(height / 2_i32.pow(lod as u32));
        }

        self.generate_normal_mips(width, height, normal_map_data);
        self.generate_hiz_max(width, height, height_map_data, max_height);
        self._initialiezed = true;
    }

    pub fn generate_normal_mips(&mut self, width: i32, height: i32, normal_map_data: &Vec<Vector4<u8>>) {
        let mut lod_normal_map_data: Vec<Vector3<f32>> = Vec::new();
        for y in 0..height {
            for x in 0..width {
                let normal_vector = normal_map_data[(y * width + x) as usize];
                lod_normal_map_data.push(Vector3::new(
                    normal_vector.x as f32 * 2.0 - 1.0,
                    normal_vector.y as f32 * 2.0 - 1.0,
                    normal_vector.z as f32 * 2.0 - 1.0
                ).normalize());
            }
        }
        self._normal_map_data.push(lod_normal_map_data);

        for lod in 1..self._lod_count as usize {
            let parent_width = self._width[lod - 1];
            let parent_height = self._height[lod - 1];
            let mut lod_normal_map_data: Vec<Vector3<f32>> = Vec::new();
            let last_normal_map_data = &self._normal_map_data.last().unwrap();
            for y in (0..parent_height).step_by(2) {
                for x in (0..parent_width).step_by(2) {
                    let tex_coord_0 = (y * parent_width + x) as usize;
                    let tex_coord_1 = ((y + 1) * parent_width + x) as usize;
                    let normal_00 = last_normal_map_data[tex_coord_0];
                    let normal_01 = last_normal_map_data[tex_coord_0 + 1];
                    let normal_10 = last_normal_map_data[tex_coord_1];
                    let normal_11 = last_normal_map_data[tex_coord_1 + 1];
                    lod_normal_map_data.push((normal_00 + normal_01 + normal_10 + normal_11).normalize());
                }
            }
            self._normal_map_data.push(lod_normal_map_data);
        }
    }

    pub fn generate_hiz_max(&mut self, width: i32, height: i32, height_map_data: &Vec<f32>, max_height: f32) {
        let mut lod_height_map_data: Vec<f32> = Vec::new();
        for y in 0..height {
            for x in 0..width {
                lod_height_map_data.push(height_map_data[(y * width + x) as usize] * max_height);
            }
        }
        self._height_map_data.push(lod_height_map_data);

        for lod in 1..self._lod_count as usize {
            let parent_width = self._width[lod - 1];
            let parent_height = self._height[lod - 1];
            let mut lod_height_map_data: Vec<f32> = Vec::new();
            let last_height_map_data = &self._height_map_data.last().unwrap();
            for y in (0..parent_height).step_by(2) {
                for x in (0..parent_width).step_by(2) {
                    let tex_coord_0 = (y * parent_width + x) as usize;
                    let tex_coord_1 = ((y + 1) * parent_width + x) as usize;
                    let height_00 = last_height_map_data[tex_coord_0];
                    let height_01 = last_height_map_data[tex_coord_0 + 1];
                    let height_10 = last_height_map_data[tex_coord_1];
                    let height_11 = last_height_map_data[tex_coord_1 + 1];
                    let min_height = height_00.max(height_01.max(height_10.max(height_11)));
                    lod_height_map_data.push(min_height);
                }
            }
            self._height_map_data.push(lod_height_map_data);
        }
    }

    pub fn get_texcoord(&self, pos: &Vector3<f32>) -> Vector2<f32> {
        let mut texcoord = Vector2::new(
            (pos.x - &self._bounding_box._min.x) / self._bounding_box._size.x,
            (pos.z - &self._bounding_box._min.z) / self._bounding_box._size.z
        );

        if HEIGHT_MAP_INVERT_TEXCOORD_Y {
            texcoord.y = 1.0 - texcoord.y;
        }
        texcoord
    }

    pub fn get_height_bilinear(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self.get_height_bilinear_by_texcoord(&self.get_texcoord(pos), lod)
    }

    pub fn get_height_point(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self.get_height_point_by_texcoord(&self.get_texcoord(pos), lod)
    }

    pub fn get_height_bilinear_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
        if self._initialiezed == false {
            return 0.0;
        }

        let lod = lod.min(self._lod_count as usize - 1);
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
        let pixel_index_00: usize = (pixel_pos_y_min + pixel_pos_x_min) as usize;
        let pixel_index_01: usize = (pixel_pos_y_min + pixel_pos_x_max) as usize;
        let pixel_index_10: usize = (pixel_pos_y_max + pixel_pos_x_min) as usize;
        let pixel_index_11: usize = (pixel_pos_y_max + pixel_pos_x_max) as usize;
        let height_map_data = &self._height_map_data[lod];
        let height_data_0 = math::lerp(height_map_data[pixel_index_00], height_map_data[pixel_index_01], pixel_pos_x_frac);
        let height_data_1 = math::lerp(height_map_data[pixel_index_10], height_map_data[pixel_index_11], pixel_pos_x_frac);
        let height = self._bounding_box._min.y + math::lerp(height_data_0, height_data_1, pixel_pos_y_frac);
        self._sea_height.max(height as f32)
    }

    pub fn get_height_point_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
        if self._initialiezed == false {
            return 0.0;
        }

        let lod = lod.min(self._lod_count as usize - 1);
        let width = self._width[lod];
        let height = self._height[lod];
        let pixel_pos_x: i32 = (0f32.max(1f32.min(texcoord.x)) * (width - 1) as f32) as i32;
        let pixel_pos_y: i32 = (0f32.max(1f32.min(texcoord.y)) * (height - 1) as f32) as i32;
        let pixel_index: usize = (pixel_pos_x + pixel_pos_y * width) as usize;
        let height = self._bounding_box._min.y + self._height_map_data[lod][pixel_index];
        self._sea_height.max(height)
    }

    pub fn get_collision_point(&self, start_pos: &Vector3<f32>, move_dir: &Vector3<f32>, mut limit_dist: f32, collision_point: &mut Vector3<f32>) -> bool {
        if self._initialiezed == false {
            return false;
        }

        let max_size: f32 = self._bounding_box._size.x.max(self._bounding_box._size.z);
        if limit_dist < 0.0 {
            limit_dist = max_size;
        }
        let texcoord_dir: Vector2<f32> = if HEIGHT_MAP_INVERT_TEXCOORD_Y {
            Vector2::new(move_dir.x, -move_dir.z).normalize()
        } else {
            Vector2::new(move_dir.x, move_dir.z).normalize()
        };
        let max_dir: f32 = texcoord_dir.x.abs().max(texcoord_dir.y.abs());
        let max_lod: i32 = self._lod_count - 2;
        let max_dist: f32 = max_dir * limit_dist;
        let mut lod: i32 = 0.max(max_lod - (max_size / max_dist).log2().ceil() as i32);

        collision_point.clone_from(start_pos);
        let mut collision_point_prev: Vector3<f32> = start_pos.clone();
        let mut texcoord: Vector2<f32> = self.get_texcoord(collision_point);
        let mut texcoord_prev: Vector2<f32> = texcoord.clone();
        let goal_texcoord: Vector2<f32> = Vector2::new(
            texcoord.x + texcoord_dir.x * limit_dist / self._bounding_box._size.x,
            texcoord.y + texcoord_dir.y * limit_dist / self._bounding_box._size.z
        );
        let mut step: f32 = 1.0 / self._width[lod as usize].max(self._height[lod as usize]) as f32;
        let mut collided = false;
        let mut debug_loop_count: i32 = 0;
        let limit_loop_count: i32 = max_size as i32;
        while 0 <= lod && debug_loop_count < limit_loop_count {
            debug_loop_count += 1;
            let height_value = self.get_height_point_by_texcoord(&texcoord, lod as usize);
            if collision_point.y <= height_value {
                // collided and go to higher lod
                texcoord.clone_from(&texcoord_prev);
                collision_point.clone_from(&collision_point_prev);
                collided = true;
                step *= 0.4;
                lod -= 1;
                continue;
            }

            // reset
            collided = false;

            // next step
            texcoord_prev.clone_from(&texcoord);
            texcoord += &texcoord_dir * step;
            collision_point_prev.clone_from(&collision_point);
            let ddx: f32 = (((texcoord.x * self._bounding_box._size.x) + self._bounding_box._min.x - start_pos.x) / texcoord_dir.x).abs();
            *collision_point = start_pos + move_dir * ddx;

            // arrive in goal
            if texcoord_dir.dot(&(&goal_texcoord - &texcoord)) < 0.0 {
                texcoord.clone_from(&goal_texcoord);
                break;
            }

            // out of range
            if texcoord.x < 0.0 || texcoord.y < 0.0 || 1.0 < texcoord.x || 1.0 < texcoord.y {
                break;
            }
        }

        if collided {
            let height_value = self.get_height_bilinear_by_texcoord(&texcoord, 0);
            collision_point.y = height_value;

            // NOTE: result is smooth but not correct and position spike.
            // let ddy: f32 = ((height_value - start_pos.y) / dir.y).abs();
            // *collision_point = start_pos + dir * ddy;
        }

        //log::info!("\tFinish!! loop_count: {:?}, collided: {:?}, lod: {:?}, collision_point: {:?}, dist: {:?}", debug_loop_count, collided, lod, collision_point, (start_pos - &*collision_point).norm());
        collided
    }
}