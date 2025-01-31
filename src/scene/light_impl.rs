use nalgebra::{linalg, Matrix4, Vector3, Vector4};
use crate::constants;
use crate::scene::bounding_box::BoundingBox;
use crate::scene::light::{DirectionalLight, DirectionalLightCreateInfo, LightData, PointLight, PointLightCreateInfo, PointLightData};
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::math::orthogonal;

// LightData
impl Default for LightData {
    fn default() -> LightData {
        unsafe {
            LightData {
                _shadow_view_projection: Matrix4::identity(),
                _inv_shadow_view_projection: Matrix4::identity(),
                _light_position: Vector3::zeros(),
                _shadow_samples: constants::SHADOW_SAMPLES,
                _light_direction: Vector3::new(-std::f32::consts::PI * 0.5, 0.0, 0.0),
                _reserved0: 0,
                _light_color: Vector3::new(1.0, 1.0, 1.0),
                _reserved1: 0,
            }
        }
    }
}

// DirectionalLightCreateInfo
impl Default for DirectionalLightCreateInfo {
    fn default() -> DirectionalLightCreateInfo {
        unsafe {
            DirectionalLightCreateInfo {
                _position: Vector3::zeros(),
                _rotation: Vector3::new(std::f32::consts::PI * -0.5, 0.0, 0.0),
                _light_data: LightData::default(),
                _shadow_update_distance: constants::SHADOW_UPDATE_DISTANCE,
                _shadow_dimensions: Vector4::new(
                    constants::SHADOW_DISTANCE,
                    constants::SHADOW_DISTANCE,
                    -constants::SHADOW_DEPTH,
                    constants::SHADOW_DEPTH,
                ),
            }
        }
    }
}

// DirectionalLight
impl DirectionalLight {
    pub fn create_directional_light(
        object_id: i64,
        light_name: &String,
        light_create_info: &DirectionalLightCreateInfo,
    ) -> DirectionalLight {
        log::debug!("    create_directional_light[{}]: {}, {:?}", object_id, light_name, light_create_info);
        let mut light_data = DirectionalLight {
            _object_id: object_id,
            _light_name: light_name.clone(),
            _light_data: light_create_info._light_data.clone(),
            _light_shadow_projection: Matrix4::identity(),
            _transform_object: TransformObjectData::create_transform_object_data(),
            _updated_light_data: true,
            _need_to_redraw_shadow: true,
            _shadow_update_distance: light_create_info._shadow_update_distance,
        };
        light_data
            ._transform_object
            .set_position(&light_create_info._position);
        light_data
            ._transform_object
            .set_rotation(&light_create_info._rotation);
        light_data.update_shadow_orthogonal(&light_create_info._shadow_dimensions);
        light_data.update_light_data(&Vector3::zeros());
        light_data
    }

    pub fn get_light_data(&self) -> &LightData {
        &self._light_data
    }
    pub fn get_light_position(&self) -> &Vector3<f32> {
        self._transform_object.get_position()
    }
    pub fn get_light_direction(&self) -> &Vector3<f32> {
        self._transform_object.get_front()
    }
    pub fn get_light_color(&self) -> &Vector3<f32> {
        &self._light_data._light_color
    }
    pub fn get_light_shadow_samples(&self) -> i32 {
        self._light_data._shadow_samples
    }
    pub fn get_shadow_view_projection(&self) -> &Matrix4<f32> {
        &self._light_data._shadow_view_projection
    }
    pub fn get_inv_shadow_view_projection(&self) -> &Matrix4<f32> {
        &self._light_data._inv_shadow_view_projection
    }
    pub fn get_need_to_redraw_shadow_and_reset(&mut self) -> bool {
        let need_to_redraw_shadow = self._need_to_redraw_shadow;
        self._need_to_redraw_shadow = false;
        need_to_redraw_shadow
    }

    pub fn update_shadow_orthogonal(&mut self, shadow_dimensions: &Vector4<f32>) {
        let width = shadow_dimensions.x;
        let height = shadow_dimensions.y;
        let near = shadow_dimensions.z;
        let far = shadow_dimensions.w;
        self._light_shadow_projection = orthogonal(-width, width, -height, height, near, far);
        self._updated_light_data = true;
    }

    pub fn update_light_data(&mut self, view_position: &Vector3<f32>) {
        let delta: Vector3<f32> = (self._transform_object.get_position() - view_position).abs();
        if self._shadow_update_distance < delta.max() {
            self._transform_object.set_position(&view_position);
        }

        let updated_transform = self._transform_object.update_transform_object();
        if self._updated_light_data || updated_transform {
            self._light_data._shadow_view_projection = &self._light_shadow_projection * self._transform_object.get_inverse_matrix();
            linalg::try_invert_to(self._light_data._shadow_view_projection.into(), &mut self._light_data._inv_shadow_view_projection);
            self._light_data._light_direction = self.get_light_direction().clone() as Vector3<f32>;
            self._need_to_redraw_shadow = true;
        }
        self._updated_light_data = false;
    }
}

// PointLightData
impl Default for PointLightData {
    fn default() -> PointLightData {
        PointLightData {
            _light_position: Vector3::zeros(),
            _radius: 1.0,
            _light_color: Vector3::new(1.0, 1.0, 1.0),
            _reserved0: 0,
        }
    }
}

// PointLightCreateInfo
impl Default for PointLightCreateInfo {
    fn default() -> PointLightCreateInfo {
        PointLightCreateInfo {
            _light_position: Vector3::zeros(),
            _radius: 1.0,
            _light_color: Vector3::new(1.0, 1.0, 1.0)
        }
    }
}

// PointLight
impl PointLight {
    pub fn create_point_light(
        object_id: i64,
        light_name: &String,
        light_create_info: &PointLightCreateInfo,
    ) -> PointLight {
        log::debug!("    create_point_light[{}]: {}, {:?}", object_id, light_name, light_create_info);
        let light_radius_offset = Vector3::new(light_create_info._radius, light_create_info._radius, light_create_info._radius);
        let mut light_data = PointLight {
            _object_id: object_id,
            _light_name: light_name.clone(),
            _light_data: PointLightData {
                _light_position: light_create_info._light_position.clone(),
                _radius: light_create_info._radius,
                _light_color: light_create_info._light_color.clone(),
                _reserved0: 0,
            },
            _bounding_box: BoundingBox::create_bounding_box(
                &(light_create_info._light_position - light_radius_offset),
                &(light_create_info._light_position + light_radius_offset),
            )
        };

        light_data.initialize_point_light();
        light_data
    }

    pub fn initialize_point_light(&mut self) {
        self.update_light_data();
    }

    pub fn get_light_data(&self) -> &PointLightData {
        &self._light_data
    }

    pub fn get_light_position(&self) -> &Vector3<f32> {
        &self._light_data._light_position
    }

    pub fn get_light_color(&self) -> &Vector3<f32> {
        &self._light_data._light_color
    }

    pub fn update_light_data(&mut self) {
    }
}