use crate::constants;
use crate::scene::bounding_box::BoundingBox;
use crate::scene::camera::CameraObjectData;
use crate::scene::light::{
    DirectionalLight, DirectionalLightCreateInfo, LightData, LocalLightProbe, LocalLightProbeCreateInfo,
    LocalLightProbeData, LocalLightingData, PointLight, PointLightCreateInfo, PointLightData, SpotLight,
    SpotLightCreateInfo, SpotLightData,
};
use crate::scene::scene_manager::SceneObjectID;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::math;
use nalgebra::{Matrix4, Vector3, Vector4, linalg};

pub const CSM_ATLAS_RECTS: [Vector4<f32>; constants::CSM_CASCADE_COUNT] = [
    Vector4::new(0.0, 0.0, 0.5, 0.5),
    Vector4::new(0.5, 0.0, 0.5, 0.5),
    Vector4::new(0.0, 0.5, 0.5, 0.5),
    Vector4::new(0.5, 0.5, 0.5, 0.5),
];

// LightData
impl Default for LightData {
    fn default() -> LightData {
        let cfg = constants::get_engine_config();
        let splits = constants::CSM_DEFAULT_SPLIT_DISTANCES;
        LightData {
            _shadow_view_projection: Matrix4::identity(),
            _inv_shadow_view_projection: Matrix4::identity(),
            _light_position: Vector3::zeros(),
            _shadow_samples: cfg._shadow_samples,
            _light_direction: math::get_top_down_view(),
            _active_shadow_cascade: 0,
            _light_color: Vector3::new(1.0, 1.0, 1.0),
            _csm_cascade_count: constants::CSM_CASCADE_COUNT as i32,
            _csm_view_projections: [Matrix4::identity(); constants::CSM_CASCADE_COUNT],
            _inv_csm_view_projections: [Matrix4::identity(); constants::CSM_CASCADE_COUNT],
            _csm_split_distances: Vector4::new(splits[0], splits[1], splits[2], splits[3]),
            _csm_atlas_rects: CSM_ATLAS_RECTS,
        }
    }
}

// DirectionalLightCreateInfo
impl Default for DirectionalLightCreateInfo {
    fn default() -> DirectionalLightCreateInfo {
        let cfg = constants::get_engine_config();
        DirectionalLightCreateInfo {
            _position: Vector3::zeros(),
            _rotation: math::get_top_down_view(),
            _light_data: LightData::default(),
            _shadow_update_distance: cfg._shadow_update_distance,
            _shadow_dimensions: Vector4::new(
                cfg._shadow_distance,
                cfg._shadow_distance,
                -cfg._shadow_depth,
                cfg._shadow_depth,
            ),
        }
    }
}

// DirectionalLight
impl DirectionalLight {
    pub fn create_directional_light(
        object_id: SceneObjectID,
        light_name: &String,
        light_create_info: &DirectionalLightCreateInfo,
    ) -> DirectionalLight {
        log::debug!(
            "    create_directional_light[{:?}]: {}, {:?}",
            object_id,
            light_name,
            light_create_info
        );
        let shadow_depth = light_create_info._shadow_dimensions.w.abs().max(200.0);
        let mut light_data = DirectionalLight {
            _object_id: object_id,
            _light_name: light_name.clone(),
            _light_data: light_create_info._light_data.clone(),
            _light_shadow_projection: Matrix4::identity(),
            _shadow_depth: shadow_depth,
            _csm_split_distances: constants::CSM_DEFAULT_SPLIT_DISTANCES,
            _transform_object: TransformObjectData::create_transform_object_data(),
            _updated_light_data: true,
            _shadow_update_distance: light_create_info._shadow_update_distance,
        };
        light_data._light_data._csm_cascade_count = constants::CSM_CASCADE_COUNT as i32;
        light_data._light_data._csm_atlas_rects = CSM_ATLAS_RECTS;
        light_data._transform_object.set_position(&light_create_info._position);
        light_data._transform_object.set_rotation(&light_create_info._rotation);
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
    pub fn get_csm_view_projections(&self) -> &[Matrix4<f32>; constants::CSM_CASCADE_COUNT] {
        &self._light_data._csm_view_projections
    }

    pub fn update_shadow_orthogonal(&mut self, shadow_dimensions: &Vector4<f32>) {
        let width = shadow_dimensions.x;
        let height = shadow_dimensions.y;
        let near = shadow_dimensions.z;
        let far = shadow_dimensions.w;
        self._shadow_depth = far.abs().max(near.abs()).max(200.0);
        self._light_shadow_projection = math::orthogonal(-width, width, -height, height, near, far);
        self._updated_light_data = true;
    }

    /// Build a texel-grid-snapped orthographic view-projection for a single CSM cascade sphere.
    fn compute_snapped_cascade_vp(
        light_rot: &Matrix4<f32>,
        light_rot_inv: &Matrix4<f32>,
        center_world: &Vector3<f32>,
        radius: f32,
        depth_extent: f32,
        tile_resolution: f32,
    ) -> Matrix4<f32> {
        // Quantize radius slightly upward so frustum rotation never causes sub-texel radius jitter
        let stable_radius = (radius * 4.0).ceil() * 0.25;
        let world_units_per_texel = (2.0 * stable_radius) / tile_resolution.max(1.0);

        let center_light = (light_rot_inv * Vector4::new(center_world.x, center_world.y, center_world.z, 1.0)).xyz();
        let snapped_x = (center_light.x / world_units_per_texel).floor() * world_units_per_texel;
        let snapped_y = (center_light.y / world_units_per_texel).floor() * world_units_per_texel;
        let snapped_light = Vector4::new(snapped_x, snapped_y, center_light.z, 1.0);
        let snapped_world = (light_rot * snapped_light).xyz();

        let translation_inv = Matrix4::new_translation(&(-snapped_world));
        let light_view = light_rot_inv * translation_inv;
        let light_proj = math::orthogonal(
            -stable_radius,
            stable_radius,
            -stable_radius,
            stable_radius,
            -depth_extent,
            depth_extent,
        );
        light_proj * light_view
    }

    /// Extract pure rotation (zero-translation) matrices from the directional light transform.
    fn get_light_rotation_matrices(&self) -> (Matrix4<f32>, Matrix4<f32>) {
        let mut light_rot = *self._transform_object.get_matrix();
        light_rot.set_column(3, &Vector4::new(0.0, 0.0, 0.0, 1.0));
        let mut light_rot_inv = *self._transform_object.get_inverse_matrix();
        light_rot_inv.set_column(3, &Vector4::new(0.0, 0.0, 0.0, 1.0));
        (light_rot, light_rot_inv)
    }

    pub fn update_light_data_with_camera(&mut self, camera: &CameraObjectData) {
        let view_position = *camera._transform_object.get_position();
        let delta: Vector3<f32> = (self._transform_object.get_position() - view_position).abs();
        if self._shadow_update_distance < delta.max() || self._updated_light_data {
            self._transform_object.set_position(&view_position);
        }

        let _updated_transform = self._transform_object.update_transform_object();
        self._light_data._light_direction = self.get_light_direction().clone() as Vector3<f32>;
        self._light_data._light_position = view_position;

        let (light_rot, light_rot_inv) = self.get_light_rotation_matrices();
        let cfg = constants::get_engine_config();
        let shadow_map_size = cfg._shadow_map_size.max(512);
        let active_cascade_count = (cfg._csm_cascade_count as usize).min(constants::CSM_CASCADE_COUNT);
        drop(cfg);

        if active_cascade_count == 0 {
            self._light_data._csm_cascade_count = 0;
            self._light_data._csm_split_distances = Vector4::zeros();
            self._light_data._csm_atlas_rects = CSM_ATLAS_RECTS;
            self._light_data._active_shadow_cascade = 0;
            self._updated_light_data = false;
            return;
        }

        let tile_resolution = (shadow_map_size / constants::CSM_ATLAS_GRID_DIM).max(1) as f32;

        // Unproject the 4 near-plane frustum corners in view space using camera._inv_projection
        // Reverse-Z: near plane is at NDC z = 1.0.
        let near = camera._near.max(0.05);
        let far = camera._far.max(near + 1.0);
        const NDC_CORNERS: [(f32, f32); 4] = [(-1.0, -1.0), (1.0, -1.0), (-1.0, 1.0), (1.0, 1.0)];
        let mut near_corners_view = [Vector3::zeros(); 4];
        for (idx, &(nx, ny)) in NDC_CORNERS.iter().enumerate() {
            let p_h = camera._inv_projection * Vector4::new(nx, ny, 1.0, 1.0);
            let inv_w = if p_h.w.abs() > 1e-6 { 1.0 / p_h.w } else { 1.0 };
            near_corners_view[idx] = p_h.xyz() * inv_w;
        }

        let splits = constants::CSM_SPLIT_PRESETS[active_cascade_count];
        self._csm_split_distances = splits;
        let mut prev_split = near;
        for cascade_idx in 0..active_cascade_count {
            let split_far = splits[cascade_idx].min(far).max(prev_split + 1.0);
            let near_scale = prev_split / near;
            let far_scale = split_far / near;

            let mut corners_world = [Vector3::zeros(); 8];
            let mut center = Vector3::zeros();
            for j in 0..4 {
                let c_near_v = near_corners_view[j] * near_scale;
                let c_far_v = near_corners_view[j] * far_scale;
                let c_near_w = (camera._inv_view * Vector4::new(c_near_v.x, c_near_v.y, c_near_v.z, 1.0)).xyz();
                let c_far_w = (camera._inv_view * Vector4::new(c_far_v.x, c_far_v.y, c_far_v.z, 1.0)).xyz();
                corners_world[j] = c_near_w;
                corners_world[j + 4] = c_far_w;
                center += c_near_w + c_far_w;
            }
            center *= 1.0 / 8.0;

            let mut radius: f32 = 1.0;
            for corner in corners_world.iter() {
                let d = (corner - center).norm();
                if d > radius {
                    radius = d;
                }
            }

            let depth_extent = self._shadow_depth.max(radius * 2.5);
            let vp = Self::compute_snapped_cascade_vp(
                &light_rot,
                &light_rot_inv,
                &center,
                radius,
                depth_extent,
                tile_resolution,
            );
            self._light_data._csm_view_projections[cascade_idx] = vp;
            linalg::try_invert_to(vp, &mut self._light_data._inv_csm_view_projections[cascade_idx]);
            prev_split = split_far;
        }

        self._light_data._csm_split_distances = Vector4::new(splits[0], splits[1], splits[2], splits[3]);
        self._light_data._csm_atlas_rects = CSM_ATLAS_RECTS;
        self._light_data._csm_cascade_count = active_cascade_count as i32;
        self._light_data._active_shadow_cascade = 0;

        // Keep legacy single-matrix fields synced to the best mid-far active cascade for atmosphere/fallback users
        let legacy_idx = (active_cascade_count - 1).min(2);
        self._light_data._shadow_view_projection = self._light_data._csm_view_projections[legacy_idx];
        self._light_data._inv_shadow_view_projection = self._light_data._inv_csm_view_projections[legacy_idx];
        self._updated_light_data = false;
    }

    pub fn update_light_data(&mut self, view_position: &Vector3<f32>) {
        let delta: Vector3<f32> = (self._transform_object.get_position() - view_position).abs();
        if self._shadow_update_distance < delta.max() || self._updated_light_data {
            self._transform_object.set_position(&view_position);
        }

        let updated_transform = self._transform_object.update_transform_object();
        if self._updated_light_data || updated_transform {
            // Preserve exact orthographic projection set via update_shadow_orthogonal
            // (required by CaptureHeightMap terrain readback and standalone directional views)
            let ortho_vp = &self._light_shadow_projection * self._transform_object.get_inverse_matrix();
            self._light_data._shadow_view_projection = ortho_vp;
            linalg::try_invert_to(ortho_vp, &mut self._light_data._inv_shadow_view_projection);
            self._light_data._light_direction = self.get_light_direction().clone() as Vector3<f32>;
            self._light_data._light_position = *view_position;

            for cascade_idx in 0..constants::CSM_CASCADE_COUNT {
                self._light_data._csm_view_projections[cascade_idx] = ortho_vp;
                self._light_data._inv_csm_view_projections[cascade_idx] = self._light_data._inv_shadow_view_projection;
            }
            self._light_data._csm_atlas_rects = CSM_ATLAS_RECTS;
            self._light_data._csm_cascade_count = constants::CSM_CASCADE_COUNT as i32;
            self._light_data._active_shadow_cascade = 0;
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
            _light_color: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}

// PointLight
impl PointLight {
    pub fn create_point_light(
        object_id: SceneObjectID,
        light_name: &String,
        light_create_info: &PointLightCreateInfo,
    ) -> PointLight {
        log::debug!(
            "    create_point_light[{:?}]: {:?}, {:?}",
            object_id,
            light_name,
            light_create_info
        );
        let light_radius_offset = Vector3::new(
            light_create_info._radius,
            light_create_info._radius,
            light_create_info._radius,
        );
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
            ),
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

    pub fn update_light_data(&mut self) {}
}

// SpotLightData
impl Default for SpotLightData {
    fn default() -> SpotLightData {
        SpotLightData {
            _light_position: Vector3::zeros(),
            _radius: 10.0,
            _light_direction: Vector3::new(0.0, -1.0, 0.0),
            _outer_cos_angle: 35.0_f32.to_radians().cos(),
            _light_color: Vector3::new(1.0, 1.0, 1.0),
            _inner_cos_angle: 20.0_f32.to_radians().cos(),
        }
    }
}

// SpotLightCreateInfo
impl Default for SpotLightCreateInfo {
    fn default() -> SpotLightCreateInfo {
        SpotLightCreateInfo {
            _light_position: Vector3::zeros(),
            _light_direction: Vector3::new(0.0, -1.0, 0.0),
            _light_color: Vector3::new(1.0, 1.0, 1.0),
            _radius: 10.0,
            _inner_angle_deg: 20.0,
            _outer_angle_deg: 35.0,
        }
    }
}

// SpotLight
impl SpotLight {
    pub fn create_spot_light(
        object_id: SceneObjectID,
        light_name: &String,
        light_create_info: &SpotLightCreateInfo,
    ) -> SpotLight {
        let dir_norm = light_create_info._light_direction.norm();
        let direction = if dir_norm > 1e-4 {
            light_create_info._light_direction / dir_norm
        } else {
            Vector3::new(0.0, -1.0, 0.0)
        };
        let outer_deg = light_create_info._outer_angle_deg.clamp(1.0, 89.0);
        let inner_deg = light_create_info._inner_angle_deg.clamp(0.5, outer_deg);
        let radius = light_create_info._radius.max(0.1);
        let radius_vec = Vector3::new(radius, radius, radius);

        SpotLight {
            _object_id: object_id,
            _light_name: light_name.clone(),
            _light_data: SpotLightData {
                _light_position: light_create_info._light_position,
                _radius: radius,
                _light_direction: direction,
                _outer_cos_angle: outer_deg.to_radians().cos(),
                _light_color: light_create_info._light_color,
                _inner_cos_angle: inner_deg.to_radians().cos(),
            },
            _bounding_box: BoundingBox::create_bounding_box(
                &(light_create_info._light_position - radius_vec),
                &(light_create_info._light_position + radius_vec),
            ),
        }
    }

    pub fn get_light_data(&self) -> &SpotLightData {
        &self._light_data
    }

    pub fn set_position(&mut self, position: Vector3<f32>) {
        self._light_data._light_position = position;
        let radius_vec = Vector3::new(
            self._light_data._radius,
            self._light_data._radius,
            self._light_data._radius,
        );
        self._bounding_box = BoundingBox::create_bounding_box(&(position - radius_vec), &(position + radius_vec));
    }

    pub fn set_direction(&mut self, direction: Vector3<f32>) {
        let dir_norm = direction.norm();
        if dir_norm > 1e-4 {
            self._light_data._light_direction = direction / dir_norm;
        }
    }

    pub fn set_color(&mut self, color: Vector3<f32>) {
        self._light_data._light_color = color;
    }
}

// LocalLightProbeData
impl Default for LocalLightProbeData {
    fn default() -> LocalLightProbeData {
        LocalLightProbeData {
            _position: Vector3::zeros(),
            _blend_distance: 2.0,
            _half_extents: Vector3::new(5.0, 4.0, 5.0),
            _indoor_factor: 0.85,
            _sky_irradiance: Vector3::new(0.25, 0.22, 0.18),
            _intensity: 1.0,
            _ground_irradiance: Vector3::new(0.10, 0.08, 0.06),
            _priority: 0.0,
            _side_irradiance: Vector3::new(0.18, 0.15, 0.12),
            _specular_tint: 0.35,
        }
    }
}

// LocalLightProbeCreateInfo
impl Default for LocalLightProbeCreateInfo {
    fn default() -> LocalLightProbeCreateInfo {
        LocalLightProbeCreateInfo {
            _position: Vector3::zeros(),
            _half_extents: Vector3::new(5.0, 4.0, 5.0),
            _blend_distance: 2.0,
            _indoor_factor: 0.85,
            _sky_irradiance: Vector3::new(0.25, 0.22, 0.18),
            _ground_irradiance: Vector3::new(0.10, 0.08, 0.06),
            _side_irradiance: Vector3::new(0.18, 0.15, 0.12),
            _intensity: 1.0,
            _priority: 0.0,
            _specular_tint: 0.35,
        }
    }
}

// LocalLightProbe
impl LocalLightProbe {
    pub fn create_local_light_probe(
        object_id: SceneObjectID,
        probe_name: &String,
        create_info: &LocalLightProbeCreateInfo,
    ) -> LocalLightProbe {
        let half_extents = Vector3::new(
            create_info._half_extents.x.max(0.1),
            create_info._half_extents.y.max(0.1),
            create_info._half_extents.z.max(0.1),
        );
        let blend_distance = create_info._blend_distance.max(0.05);
        let outer_extents = half_extents + Vector3::new(blend_distance, blend_distance, blend_distance);
        LocalLightProbe {
            _object_id: object_id,
            _probe_name: probe_name.clone(),
            _probe_data: LocalLightProbeData {
                _position: create_info._position,
                _blend_distance: blend_distance,
                _half_extents: half_extents,
                _indoor_factor: create_info._indoor_factor.clamp(0.0, 1.0),
                _sky_irradiance: create_info._sky_irradiance,
                _intensity: create_info._intensity.max(0.0),
                _ground_irradiance: create_info._ground_irradiance,
                _priority: create_info._priority,
                _side_irradiance: create_info._side_irradiance,
                _specular_tint: create_info._specular_tint.clamp(0.0, 2.0),
            },
            _bounding_box: BoundingBox::create_bounding_box(
                &(create_info._position - outer_extents),
                &(create_info._position + outer_extents),
            ),
        }
    }

    pub fn get_probe_data(&self) -> &LocalLightProbeData {
        &self._probe_data
    }
}

// LocalLightingData
impl Default for LocalLightingData {
    fn default() -> LocalLightingData {
        LocalLightingData {
            _spot_lights: [SpotLightData::default(); constants::MAX_SPOT_LIGHTS],
            _local_light_probes: [LocalLightProbeData::default(); constants::MAX_LOCAL_LIGHT_PROBES],
            _spot_light_count: 0,
            _local_light_probe_count: 0,
            _reserved0: 0,
            _reserved1: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scene::camera::CameraCreateInfo;
    use uuid::Uuid;

    #[test]
    fn test_light_and_local_lighting_std140_layout() {
        assert_eq!(std::mem::size_of::<LightData>(), 768);
        assert_eq!(std::mem::offset_of!(LightData, _active_shadow_cascade), 156);
        assert_eq!(std::mem::offset_of!(LightData, _csm_view_projections), 176);
        assert_eq!(std::mem::offset_of!(LightData, _csm_split_distances), 688);
        assert_eq!(std::mem::offset_of!(LightData, _csm_atlas_rects), 704);

        assert_eq!(std::mem::size_of::<SpotLightData>(), 48);
        assert_eq!(std::mem::size_of::<LocalLightProbeData>(), 80);
        assert_eq!(
            std::mem::size_of::<LocalLightingData>(),
            constants::MAX_SPOT_LIGHTS * 48 + constants::MAX_LOCAL_LIGHT_PROBES * 80 + 16
        );
    }

    #[test]
    fn test_csm_cascades_valid_and_invertible() {
        let mut camera = CameraObjectData::create_camera_object_data(
            Uuid::new_v4(),
            &String::from("test_cam"),
            &CameraCreateInfo {
                position: Vector3::new(10.0, 5.0, -12.0),
                rotation: Vector3::new(-0.2, 0.7, 0.0),
                ..Default::default()
            },
        );
        camera.update_camera_object_data();

        let mut dir_light = DirectionalLight::create_directional_light(
            Uuid::new_v4(),
            &String::from("sun"),
            &DirectionalLightCreateInfo::default(),
        );
        dir_light.update_light_data_with_camera(&camera);

        let ld = dir_light.get_light_data();
        assert_eq!(ld._csm_cascade_count, constants::CSM_CASCADE_COUNT as i32);
        assert!(ld._csm_split_distances.x < ld._csm_split_distances.y);
        assert!(ld._csm_split_distances.y < ld._csm_split_distances.z);
        assert!(ld._csm_split_distances.z < ld._csm_split_distances.w);

        for i in 0..constants::CSM_CASCADE_COUNT {
            let prod = ld._csm_view_projections[i] * ld._inv_csm_view_projections[i];
            for r in 0..4 {
                for c in 0..4 {
                    let expected = if r == c { 1.0 } else { 0.0 };
                    assert!(
                        (prod[(r, c)] - expected).abs() < 1e-2,
                        "Cascade {} VP * InvVP mismatch at ({}, {}): {}",
                        i,
                        r,
                        c,
                        prod[(r, c)]
                    );
                }
            }
        }
    }

    #[test]
    fn test_spot_light_cone_clamping_and_local_probe_bounds() {
        let spot = SpotLight::create_spot_light(
            Uuid::new_v4(),
            &String::from("torch_spot"),
            &SpotLightCreateInfo {
                _light_position: Vector3::new(1.0, 2.0, 3.0),
                _light_direction: Vector3::new(0.0, -2.0, 0.0),
                _inner_angle_deg: 50.0,
                _outer_angle_deg: 30.0, // smaller than inner -> inner clamps to outer
                _radius: 12.0,
                ..Default::default()
            },
        );
        assert!((spot._light_data._light_direction.norm() - 1.0).abs() < 1e-5);
        assert!(spot._light_data._inner_cos_angle >= spot._light_data._outer_cos_angle - 1e-6);

        let probe = LocalLightProbe::create_local_light_probe(
            Uuid::new_v4(),
            &String::from("cave_probe"),
            &LocalLightProbeCreateInfo {
                _position: Vector3::new(0.0, 2.0, 0.0),
                _half_extents: Vector3::new(4.0, 3.0, 4.0),
                _blend_distance: 1.5,
                _indoor_factor: 1.5, // clamps to 1.0
                ..Default::default()
            },
        );
        assert!((probe._probe_data._indoor_factor - 1.0).abs() < 1e-6);
        assert!((probe._bounding_box._extents.x - 5.5).abs() < 1e-5);
    }
}
