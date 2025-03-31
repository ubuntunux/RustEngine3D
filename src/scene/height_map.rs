use nalgebra::{Vector2, Vector3};
use crate::utilities::math;
use crate::scene::bounding_box::BoundingBox;

#[derive(Clone)]
pub struct HeightMapData {
    _sea_height: f32,
    _bounding_box: BoundingBox,
    _lod_count: i32,
    _width: Vec<i32>,
    _height: Vec<i32>,
    _min_height_map_data: Vec<Vec<f32>>,
}

impl Default for HeightMapData {
    fn default() -> HeightMapData {
        HeightMapData {
            _sea_height: 0.0,
            _bounding_box: BoundingBox::default(),
            _lod_count: 0,
            _width: Vec::new(),
            _height: Vec::new(),
            _min_height_map_data: Vec::new(),
        }
    }
}

impl HeightMapData {
    pub fn initialize_height_map_data(&mut self, bounding_box: &BoundingBox, width: i32, height: i32, height_map_data: Vec<u8>, sea_height: f32) {
        self._sea_height = sea_height;
        self._bounding_box = bounding_box.clone();
        let max_height = bounding_box._size.y;
        let lod_count_x = (width as f32).log2() as i32 + 1;
        let lod_count_y = (height as f32).log2() as i32 + 1;
        self._lod_count = lod_count_x.min(lod_count_y);
        assert!(2 <= self._lod_count, "lod_count must be greater than 2.");

        self._width.push(width);
        self._height.push(height);
        let mut lod_height_map_data: Vec<f32> = Vec::new();
        for y in 0..height {
            for x in 0..width {
                lod_height_map_data.push(height_map_data[(y * width + x) as usize * 4] as f32 / 255.0 * max_height);
            }
        }
        self._min_height_map_data.push(lod_height_map_data);
        self.generate_hiz_max();
    }

    pub fn generate_hiz_max(&mut self) {
        for _ in 1..self._lod_count {
            let width = *self._width.last().unwrap() as i32;
            let height = *self._height.last().unwrap() as i32;
            self._width.push(width / 2);
            self._height.push(height / 2);
            let mut lod_height_map_data: Vec<f32> = Vec::new();
            let last_height_map_data = &self._min_height_map_data.last().unwrap();
            for y in (0..height).step_by(2) {
                for x in (0..width).step_by(2) {
                    let tex_coord_0 = (y * width + x) as usize;
                    let tex_coord_1 = ((y + 1) * width + x) as usize;
                    let height_00 = last_height_map_data[tex_coord_0];
                    let height_01 = last_height_map_data[tex_coord_0 + 1];
                    let height_10 = last_height_map_data[tex_coord_1];
                    let height_11 = last_height_map_data[tex_coord_1 + 1];
                    let min_height = height_00.max(height_01.max(height_10.max(height_11)));
                    lod_height_map_data.push(min_height);
                }
            }
            self._min_height_map_data.push(lod_height_map_data);
        }
    }

    pub fn get_height_bilinear_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
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
        let height_map_data = &self._min_height_map_data[lod];
        let height_data_0 = math::lerp(height_map_data[pixel_index_00], height_map_data[pixel_index_01], pixel_pos_x_frac);
        let height_data_1 = math::lerp(height_map_data[pixel_index_10], height_map_data[pixel_index_11], pixel_pos_x_frac);
        let height = self._bounding_box._min.y + math::lerp(height_data_0, height_data_1, pixel_pos_y_frac);
        self._sea_height.max(height as f32)
    }

    pub fn get_height_bilinear(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        let texcoord: Vector2<f32> = Vector2::new(
            (pos.x - &self._bounding_box._min.x) / self._bounding_box._size.x,
            (pos.z - &self._bounding_box._min.z) / self._bounding_box._size.z
        );
        self.get_height_bilinear_by_texcoord(&texcoord, lod)
    }

    pub fn get_height_point_by_texcoord(&self, texcoord: &Vector2<f32>, lod: usize) -> f32 {
        let lod = lod.min(self._lod_count as usize - 1);
        let width = self._width[lod];
        let height = self._height[lod];
        let pixel_pos_x: i32 = (0f32.max(1f32.min(texcoord.x)) * (width - 1) as f32) as i32;
        let pixel_pos_y: i32 = (0f32.max(1f32.min(texcoord.y)) * (height - 1) as f32) as i32;
        let pixel_index: usize = (pixel_pos_x + pixel_pos_y * width) as usize;
        let height = self._bounding_box._min.y + self._min_height_map_data[lod][pixel_index];
        self._sea_height.max(height as f32)
    }

    pub fn get_height_point(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        let texcoord: Vector2<f32> = Vector2::new(
            (pos.x - &self._bounding_box._min.x) / self._bounding_box._size.x,
            (pos.z - &self._bounding_box._min.z) / self._bounding_box._size.z
        );
        self.get_height_point_by_texcoord(&texcoord, lod)
    }

    pub fn get_collision_point(&self, start_pos: &Vector3<f32>, dir: &Vector3<f32>, mut limit_dist: f32, collision_point: &mut Vector3<f32>) -> bool {
        let max_size: f32 = self._bounding_box._size.x.max(self._bounding_box._size.z);
        if limit_dist < 0.0 {
            limit_dist = max_size;
        }
        let max_dir: f32 = dir.x.abs().max(dir.z.abs());
        let max_lod: i32 = self._lod_count - 2;
        let max_dist: f32 = max_dir * limit_dist;
        let mut lod: i32 = 0.max(max_lod - (max_size / max_dist).log2().ceil() as i32);

        collision_point.clone_from(start_pos);
        let mut collision_point_prev: Vector3<f32> = start_pos.clone_owned();
        let mut texcoord: Vector2<f32> = Vector2::new(
            (collision_point.x - self._bounding_box._min.x) / self._bounding_box._size.x,
            (collision_point.z - self._bounding_box._min.z) / self._bounding_box._size.z
        );
        let mut texcoord_prev: Vector2<f32> = texcoord.clone_owned();
        let goal_texcoord: Vector2<f32> = Vector2::new(
            texcoord.x + dir.x * limit_dist / self._bounding_box._size.x,
            texcoord.y + dir.z * limit_dist / self._bounding_box._size.z
        );
        let dir_xz: Vector2<f32> = Vector2::new(dir.x, dir.z).normalize();
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
            texcoord += &dir_xz * step;
            collision_point_prev.clone_from(&collision_point);
            let ddx: f32 = (((texcoord.x * self._bounding_box._size.x) + self._bounding_box._min.x - start_pos.x) / dir.x).abs();
            *collision_point = start_pos + dir * ddx;

            // arrive in goal
            if dir_xz.dot(&(&goal_texcoord - &texcoord)) < 0.0 {
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