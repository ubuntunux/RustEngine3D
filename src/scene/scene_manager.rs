use std::collections::HashMap;
use std::rc::Rc;

use nalgebra::{Matrix4, Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};

use crate::core::engine_core::EngineCore;
use crate::constants;
use crate::constants::MAX_TRANSFORM_COUNT;
use crate::effect::effect_data::{EffectCreateInfo, EffectInstance};
use crate::effect::effect_manager::EffectManager;
use crate::renderer::push_constants::PushConstantParameter;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::{RendererData, RenderObjectType};
use crate::resource::resource::EngineResources;
use crate::scene::camera::{CameraCreateInfo, CameraObjectData};
use crate::scene::light::{DirectionalLightCreateInfo, DirectionalLightData, LightConstants};
use crate::scene::render_element::RenderElementData;
use crate::scene::render_object::{RenderObjectCreateInfo, RenderObjectData};
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{self, newRcRefCell, ptr_as_mut, ptr_as_ref, RcRefCell};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

type CameraObjectMap = HashMap<String, Rc<CameraObjectData>>;
type DirectionalLightObjectMap = HashMap<String, RcRefCell<DirectionalLightData>>;
type EffectIDMap = HashMap<String, i64>;
type RenderObjectMap = HashMap<String, RcRefCell<RenderObjectData>>;

pub trait ProjectSceneManagerBase {
    // TODO
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct SceneDataCreateInfo {
    pub _sea_height: f32,
    pub _cameras: HashMap<String, CameraCreateInfo>,
    pub _directional_lights: HashMap<String, DirectionalLightCreateInfo>,
    pub _effects: HashMap<String, EffectCreateInfo>,
    pub _static_objects: HashMap<String, RenderObjectCreateInfo>,
    pub _skeletal_objects: HashMap<String, RenderObjectCreateInfo>,
}

#[derive(Clone)]
pub struct SceneManager {
    pub _project_scene_manager: *const dyn ProjectSceneManagerBase,
    pub _engine_resources: *const EngineResources,
    pub _renderer_data: *const RendererData,
    pub _effect_manager: *const EffectManager,
    pub _window_size: Vector2<i32>,
    pub _scene_name: String,
    pub _sea_height: f32,
    pub _main_camera: Rc<CameraObjectData>,
    pub _main_light: RcRefCell<DirectionalLightData>,
    pub _capture_height_map: RcRefCell<DirectionalLightData>,
    pub _light_probe_cameras: Vec<RcRefCell<CameraObjectData>>,
    pub _camera_object_map: CameraObjectMap,
    pub _directional_light_object_map: DirectionalLightObjectMap,
    pub _effect_id_map: EffectIDMap,
    pub _static_render_object_map: RenderObjectMap,
    pub _skeletal_render_object_map: RenderObjectMap,
    pub _static_render_elements: Vec<RenderElementData>,
    pub _static_shadow_render_elements: Vec<RenderElementData>,
    pub _skeletal_render_elements: Vec<RenderElementData>,
    pub _skeletal_shadow_render_elements: Vec<RenderElementData>,
    pub _render_element_transform_count: usize,
    pub _render_element_transform_matrices: Vec<Matrix4<f32>>,
}

// Implementation
impl Default for SceneDataCreateInfo {
    fn default() -> SceneDataCreateInfo {
        SceneDataCreateInfo {
            _sea_height: 0.0,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _effects: HashMap::new(),
            _static_objects: HashMap::new(),
            _skeletal_objects: HashMap::new(),
        }
    }
}

impl SceneManager {
    pub fn get_main_camera(&self) -> &CameraObjectData {
        self._main_camera.as_ref()
    }
    pub fn get_main_camera_mut(&self) -> &mut CameraObjectData {
        ptr_as_mut(self._main_camera.as_ref())
    }
    pub fn get_main_light(&self) -> &RcRefCell<DirectionalLightData> {
        &self._main_light
    }
    pub fn get_light_probe_camera(&self, index: usize) -> &RcRefCell<CameraObjectData> {
        &self._light_probe_cameras[index]
    }
    pub fn get_capture_height_map(&self) -> &RcRefCell<DirectionalLightData> {
        &self._capture_height_map
    }
    pub fn get_static_render_elements(&self) -> &Vec<RenderElementData> {
        &self._static_render_elements
    }
    pub fn get_static_shadow_render_elements(&self) -> &Vec<RenderElementData> {
        &self._static_shadow_render_elements
    }
    pub fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData> {
        &self._skeletal_render_elements
    }
    pub fn get_skeletal_shadow_render_elements(&self) -> &Vec<RenderElementData> {
        &self._skeletal_shadow_render_elements
    }
    pub fn get_render_element_transform_count(&self) -> usize {
        self._render_element_transform_count
    }
    pub fn get_render_element_transform_matrices(&self) -> &Vec<Matrix4<f32>> {
        &self._render_element_transform_matrices
    }
    pub fn create_scene_manager(
        project_scene_manager: *const dyn ProjectSceneManagerBase,
    ) -> Box<SceneManager> {
        let default_camera = CameraObjectData::create_camera_object_data(
            &String::from("default_camera"),
            &CameraCreateInfo::default(),
        );
        let light_probe_camera_create_info = CameraCreateInfo {
            fov: 90.0,
            window_size: Vector2::new(
                constants::LIGHT_PROBE_SIZE as i32,
                constants::LIGHT_PROBE_SIZE as i32,
            ),
            enable_jitter: false,
            ..Default::default()
        };
        let light_probe_cameras = vec![
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera0"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera1"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera2"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera3"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera4"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                &String::from("light_probe_camera5"),
                &light_probe_camera_create_info,
            )),
        ];
        let default_light = DirectionalLightData::create_light_data(
            &String::from("default_light"),
            &DirectionalLightCreateInfo::default(),
        );
        let capture_height_map = unsafe {
            DirectionalLightData::create_light_data(
                &String::from("capture_height_map"),
                &DirectionalLightCreateInfo {
                    _rotation: Vector3::new(std::f32::consts::PI * 0.5, 0.0, 0.0),
                    _shadow_dimensions: Vector4::new(
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        constants::CAPTURE_HEIGHT_MAP_DISTANCE,
                        -constants::CAPTURE_HEIGHT_MAP_DEPTH,
                        constants::CAPTURE_HEIGHT_MAP_DEPTH,
                    ),
                    ..Default::default()
                },
            )
        };
        Box::new(SceneManager {
            _project_scene_manager: project_scene_manager,
            _engine_resources: std::ptr::null(),
            _renderer_data: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _window_size: default_camera._window_size.into(),
            _scene_name: String::new(),
            _sea_height: 0.0,
            _main_camera: Rc::new(default_camera),
            _main_light: newRcRefCell(default_light),
            _capture_height_map: newRcRefCell(capture_height_map),
            _light_probe_cameras: light_probe_cameras,
            _camera_object_map: HashMap::new(),
            _directional_light_object_map: HashMap::new(),
            _effect_id_map: HashMap::default(),
            _static_render_object_map: HashMap::new(),
            _skeletal_render_object_map: HashMap::new(),
            _static_render_elements: Vec::new(),
            _static_shadow_render_elements: Vec::new(),
            _skeletal_render_elements: Vec::new(),
            _skeletal_shadow_render_elements: Vec::new(),
            _render_element_transform_count: 0,
            _render_element_transform_matrices: vec![Matrix4::identity(); MAX_TRANSFORM_COUNT],
        })
    }

    pub fn initialize_scene_manager(
        &mut self,
        project_scene_manager: *const dyn ProjectSceneManagerBase,
        renderer_context: &RendererContext,
        effect_manager: &EffectManager,
        engine_resources: &EngineResources,
        window_size: &Vector2<i32>,
    ) {
        self._project_scene_manager = project_scene_manager;
        self._renderer_data = renderer_context.get_renderer_data();
        self._effect_manager = effect_manager;
        self._engine_resources = engine_resources;
        self.resized_window(window_size.x, window_size.y);
    }
    pub fn get_engine_resources(&self) -> &EngineResources {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources {
        ptr_as_mut(self._engine_resources)
    }
    pub fn get_renderer_data(&self) -> &RendererData {
        ptr_as_ref(self._renderer_data)
    }
    pub fn get_renderer_data_mut(&self) -> &mut RendererData {
        ptr_as_mut(self._renderer_data)
    }
    pub fn get_effect_manager(&self) -> &EffectManager {
        ptr_as_ref(self._effect_manager)
    }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager {
        ptr_as_mut(self._effect_manager)
    }
    pub fn set_effect_manager(&mut self, effect_manager: *const EffectManager) {
        self._effect_manager = effect_manager;
    }
    pub fn get_sea_height(&self) -> f32 {
        self._sea_height
    }
    pub fn add_camera_object(
        &mut self,
        object_name: &str,
        camera_create_info: &CameraCreateInfo,
    ) -> Rc<CameraObjectData> {
        let new_object_name = system::generate_unique_name(&self._camera_object_map, object_name);
        let camera_object_data = Rc::new(CameraObjectData::create_camera_object_data(
            &new_object_name,
            camera_create_info,
        ));
        self._camera_object_map
            .insert(new_object_name, camera_object_data.clone());
        camera_object_data
    }
    pub fn add_light_object(
        &mut self,
        object_name: &str,
        light_create_info: &DirectionalLightCreateInfo,
    ) -> RcRefCell<DirectionalLightData> {
        let new_object_name =
            system::generate_unique_name(&self._directional_light_object_map, object_name);
        let light_object_data = newRcRefCell(DirectionalLightData::create_light_data(
            &new_object_name,
            light_create_info,
        ));
        self._directional_light_object_map
            .insert(new_object_name, light_object_data.clone());
        light_object_data
    }

    pub fn add_static_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
    ) -> RcRefCell<RenderObjectData> {
        let model_data = self
            .get_engine_resources()
            .get_model_data(&render_object_create_info._model_data_name);
        let new_object_name =
            system::generate_unique_name(&self._static_render_object_map, &object_name);
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(
            &new_object_name,
            &model_data,
            &render_object_create_info,
        ));
        self._static_render_object_map
            .insert(new_object_name, render_object_data.clone());
        render_object_data
    }

    pub fn add_skeletal_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
    ) -> RcRefCell<RenderObjectData> {
        let model_data = self
            .get_engine_resources()
            .get_model_data(&render_object_create_info._model_data_name);
        let new_object_name =
            system::generate_unique_name(&self._skeletal_render_object_map, &object_name);
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(
            &new_object_name,
            model_data,
            &render_object_create_info,
        ));
        self._skeletal_render_object_map
            .insert(new_object_name, render_object_data.clone());
        render_object_data
    }

    pub fn add_effect(&mut self, object_name: &str, effect_create_info: &EffectCreateInfo) -> i64 {
        let new_object_name = system::generate_unique_name(&self._effect_id_map, &object_name);
        let effect_data = self
            .get_engine_resources()
            .get_effect_data(&effect_create_info._effect_data_name);
        let effect_id = self
            .get_effect_manager_mut()
            .create_effect(effect_create_info, &effect_data);
        self._effect_id_map.insert(new_object_name, effect_id);
        effect_id
    }

    pub fn get_static_render_object(
        &self,
        object_name: &str,
    ) -> Option<&RcRefCell<RenderObjectData>> {
        self._static_render_object_map.get(object_name)
    }

    pub fn remove_static_render_object(&mut self, object_name: &str) {
        self._static_render_object_map.remove(object_name);
    }

    pub fn get_skeletal_render_object(
        &self,
        object_name: &str,
    ) -> Option<&RcRefCell<RenderObjectData>> {
        self._skeletal_render_object_map.get(object_name)
    }

    pub fn remove_skeletal_render_object(&mut self, object_name: &str) {
        self._skeletal_render_object_map.remove(object_name);
    }

    pub fn get_effect(&self, effect_id: i64) -> Option<&RcRefCell<EffectInstance>> {
        self.get_effect_manager().get_effect(effect_id)
    }

    pub fn initialize_light_probe_cameras(&mut self) {
        let pi = std::f32::consts::PI;
        let half_pi = std::f32::consts::PI * 0.5;
        let rotations: [Vector3<f32>; constants::CUBE_LAYER_COUNT] = [
            Vector3::new(0.0, half_pi, 0.0),
            Vector3::new(0.0, -half_pi, 0.0),
            Vector3::new(-half_pi, 0.0, 0.0),
            Vector3::new(half_pi, 0.0, 0.0),
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(0.0, pi, 0.0),
        ];
        for i in 0..constants::CUBE_LAYER_COUNT {
            self._light_probe_cameras[i]
                .borrow_mut()
                ._transform_object
                .set_rotation(&rotations[i]);
            self._light_probe_cameras[i]
                .borrow_mut()
                ._transform_object
                .set_scale(&Vector3::new(1.0, 1.0, 1.0));
            self._light_probe_cameras[i]
                .borrow_mut()
                .update_camera_object_data();
        }
    }
    pub fn view_frustum_culling_geometry(
        camera: &CameraObjectData,
        geometry_bound_box: &BoundingBox,
    ) -> bool {
        let to_geometry = &geometry_bound_box._center - camera.get_camera_position();
        for plane in camera._view_frustum_planes.iter() {
            let d = plane.dot(&to_geometry);
            if geometry_bound_box._radius < d {
                return true;
            }
        }
        false
    }
    pub fn shadow_culling(light: &DirectionalLightData, geometry_bound_box: &BoundingBox) -> bool {
        let shadow_view_projection = light.get_shadow_view_projection();
        let bound_min: Vector4<f32> = shadow_view_projection
            * Vector4::new(
                geometry_bound_box._min.x,
                geometry_bound_box._min.y,
                geometry_bound_box._min.z,
                1.0,
            );
        let bound_max: Vector4<f32> = shadow_view_projection
            * Vector4::new(
                geometry_bound_box._max.x,
                geometry_bound_box._max.y,
                geometry_bound_box._max.z,
                1.0,
            );
        let minimum: Vector3<f32> = Vector3::new(
            bound_min.x.min(bound_max.x),
            bound_min.y.min(bound_max.y),
            bound_min.z.min(bound_max.z),
        );
        let maximum: Vector3<f32> = Vector3::new(
            bound_min.x.max(bound_max.x),
            bound_min.y.max(bound_max.y),
            bound_min.z.max(bound_max.z),
        );
        if maximum.x < -1.0
            || 1.0 < minimum.x
            || maximum.y < -1.0
            || 1.0 < minimum.y
            || maximum.z < -1.0
            || 1.0 < minimum.z
        {
            return true;
        }
        false
    }

    pub fn gather_render_elements(
        render_object_type: RenderObjectType,
        camera: &CameraObjectData,
        light: &DirectionalLightData,
        render_object_map: &RenderObjectMap,
        render_elements: &mut Vec<RenderElementData>,
        render_shadow_elements: &mut Vec<RenderElementData>,
        render_element_transform_offset: &mut usize,
        render_element_transform_matrices: &mut Vec<Matrix4<f32>>,
    ) {
        render_elements.clear();
        render_shadow_elements.clear();

        for (_key, render_object_data_ref) in render_object_map.iter() {
            let render_object_data = render_object_data_ref.borrow();
            let model_data = ptr_as_ref(render_object_data.get_model_data().as_ptr());
            let mesh_data = model_data.get_mesh_data().borrow();
            let geometry_data_list = mesh_data.get_geometry_data_list();
            let material_instance_data_list = model_data.get_material_instance_data_list();
            let is_render = false
                == SceneManager::view_frustum_culling_geometry(
                    camera,
                    &render_object_data._bound_box,
                );
            let is_render_shadow =
                false == SceneManager::shadow_culling(light, &render_object_data._bound_box);

            for index in 0..geometry_data_list.len() {
                let mut transform_offset = *render_element_transform_offset;
                let local_matrix_count = 1usize;
                let local_matrix_prev_count = 1usize;
                let bone_count = render_object_data.get_bone_count();
                // transform matrix offset: _localMatrixPrev + _localMatrix + prev_animation_bone_count + curr_animation_bone_count
                let required_transform_count =
                    local_matrix_count + local_matrix_prev_count + bone_count + bone_count;
                let push_constant_data_list: *const Vec<PipelinePushConstantData> =
                    render_object_data.get_push_constant_data_list(index);
                let render_something: bool = is_render || is_render_shadow;
                if render_something
                    && (transform_offset + required_transform_count) <= MAX_TRANSFORM_COUNT
                {
                    if is_render {
                        render_elements.push(RenderElementData {
                            _render_object: render_object_data_ref.clone(),
                            _geometry_data: geometry_data_list[index].clone(),
                            _material_instance_data: material_instance_data_list[index].clone(),
                            _push_constant_data_list: push_constant_data_list.clone(),
                        });
                    }

                    if is_render_shadow {
                        render_shadow_elements.push(RenderElementData {
                            _render_object: render_object_data_ref.clone(),
                            _geometry_data: geometry_data_list[index].clone(),
                            _material_instance_data: material_instance_data_list[index].clone(),
                            _push_constant_data_list: push_constant_data_list.clone(),
                        });
                    }
                } else {
                    // not visible
                    return;
                }

                // set transform_offset
                let push_constant_data_list_mut = ptr_as_mut(push_constant_data_list);
                for push_constant_data_mut in push_constant_data_list_mut.iter_mut() {
                    push_constant_data_mut
                        ._push_constant
                        .set_push_constant_parameter(
                            "_transform_matrix_offset",
                            &PushConstantParameter::Int(transform_offset as i32),
                        );
                    push_constant_data_mut
                        ._push_constant
                        .set_push_constant_parameter(
                            "_bone_count",
                            &PushConstantParameter::Int(bone_count as i32),
                        );
                }

                // local matrix prev
                render_element_transform_matrices[transform_offset]
                    .copy_from(render_object_data._transform_object.get_prev_matrix());
                transform_offset += local_matrix_prev_count;

                // local matrix
                render_element_transform_matrices[transform_offset]
                    .copy_from(render_object_data._transform_object.get_matrix());
                transform_offset += local_matrix_count;

                if RenderObjectType::Skeletal == render_object_type {
                    // prev animation buffer
                    let prev_animation_buffer: &Vec<Matrix4<f32>> =
                        render_object_data.get_prev_animation_buffer(0);
                    assert_eq!(bone_count, prev_animation_buffer.len());
                    let next_transform_offset: usize = transform_offset + bone_count;
                    render_element_transform_matrices[transform_offset..next_transform_offset]
                        .copy_from_slice(prev_animation_buffer);
                    transform_offset = next_transform_offset;

                    // current animation buffer
                    let animation_buffer: &Vec<Matrix4<f32>> =
                        render_object_data.get_animation_buffer(0);
                    assert_eq!(bone_count, animation_buffer.len());
                    let next_transform_offset: usize = transform_offset + bone_count;
                    render_element_transform_matrices[transform_offset..next_transform_offset]
                        .copy_from_slice(animation_buffer);
                    transform_offset = next_transform_offset;
                }

                // set transform matrix index
                *render_element_transform_offset = transform_offset;
            }
        }
    }

    pub fn resized_window(&mut self, width: i32, height: i32) {
        self._window_size.x = width;
        self._window_size.y = height;
        self.get_main_camera_mut().set_aspect(width, height);
    }

    pub fn create_default_scene_data(&self, scene_data_name: &str) {
        let mut scene_data_create_info = SceneDataCreateInfo {
            _sea_height: 0.0,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _effects: HashMap::new(),
            _static_objects: HashMap::new(),
            _skeletal_objects: HashMap::new(),
        };

        scene_data_create_info._cameras.insert(
            String::from("main_camera"),
            CameraCreateInfo {
                position: Vector3::new(2.0, 2.0, -1.0),
                rotation: Vector3::new(-0.157, 1.3, 0.0),
                ..Default::default()
            },
        );

        let pitch: f32 = std::f32::consts::PI * 0.47;
        scene_data_create_info._directional_lights.insert(
            String::from("main_light"),
            DirectionalLightCreateInfo {
                _position: Vector3::zeros(),
                _rotation: Vector3::new(pitch, 0.0, 0.3),
                _light_constants: LightConstants {
                    _light_direction: Vector3::new(pitch, 0.0, 0.3),
                    ..Default::default()
                },
                ..Default::default()
            },
        );

        scene_data_create_info._static_objects.insert(
            String::from("stage"),
            RenderObjectCreateInfo {
                _model_data_name: String::from("stages/default_stage"),
                _position: Vector3::new(0.0, -20.0, 0.0),
                _scale: Vector3::new(2000.0, 1000.0, 2000.0),
                ..Default::default()
            },
        );
        self.get_engine_resources_mut()
            .save_scene_data(scene_data_name, &scene_data_create_info);
    }

    pub fn open_scene_data(&mut self, scene_data_name: &str) {
        self._scene_name = String::from(scene_data_name);

        self.initialize_light_probe_cameras();

        let engine_resources = ptr_as_ref(self._engine_resources);

        if false == engine_resources.has_scene_data(scene_data_name) {
            self.create_default_scene_data(scene_data_name);
        }

        let scene_data_create_info = engine_resources
            .get_scene_data(scene_data_name)
            .borrow();

        self._sea_height = scene_data_create_info._sea_height;
        self.get_renderer_data_mut()
            ._fft_ocean
            .set_height(scene_data_create_info._sea_height);

        // cameras
        for (index, (object_name, camera_create_info)) in
            scene_data_create_info._cameras.iter().enumerate()
        {
            let camera_create_info = CameraCreateInfo {
                window_size: self._window_size.into(),
                ..camera_create_info.clone()
            };
            let camera_object = self.add_camera_object(object_name, &camera_create_info);
            if 0 == index {
                self._main_camera = camera_object;
            }
        }

        // lights
        for (index, (object_name, light_create_info)) in scene_data_create_info
            ._directional_lights
            .iter()
            .enumerate()
        {
            let light_create_info = DirectionalLightCreateInfo {
                _position: light_create_info._position.clone() as Vector3<f32>,
                _rotation: light_create_info._rotation.clone() as Vector3<f32>,
                _light_constants: LightConstants {
                    _light_position: light_create_info._light_constants._light_position.clone()
                        as Vector3<f32>,
                    _light_direction: light_create_info._light_constants._light_direction.clone()
                        as Vector3<f32>,
                    _light_color: light_create_info._light_constants._light_color.clone()
                        as Vector3<f32>,
                    ..Default::default()
                },
                ..Default::default()
            };
            let light_object = self.add_light_object(object_name, &light_create_info);
            if 0 == index {
                self._main_light = light_object;
            }
        }

        // effects
        for (object_name, effect_create_info) in scene_data_create_info._effects.iter() {
            self.add_effect(object_name, effect_create_info);
        }

        // static objects
        for (object_name, render_object_create_info) in
            scene_data_create_info._static_objects.iter()
        {
            self.add_static_render_object(object_name, render_object_create_info);
        }

        // skeletal objects
        for (object_name, render_object_create_info) in
            scene_data_create_info._skeletal_objects.iter()
        {
            self.add_skeletal_render_object(object_name, render_object_create_info);
        }
    }

    pub fn close_scene_data(&mut self) {
        self._camera_object_map.clear();
        self._directional_light_object_map.clear();
        self._effect_id_map.clear();
        self._static_render_object_map.clear();
        self._skeletal_render_object_map.clear();
        self._static_render_elements.clear();
        self._static_shadow_render_elements.clear();
        self._skeletal_render_elements.clear();
        self._skeletal_shadow_render_elements.clear();
    }

    pub fn save_scene_data(&mut self) {
        let mut scene_data_create_info = SceneDataCreateInfo {
            _sea_height: self._sea_height,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _effects: HashMap::new(),
            _static_objects: HashMap::new(),
            _skeletal_objects: HashMap::new(),
        };

        // cameras
        for camera in self._camera_object_map.values() {
            let camera_create_info = CameraCreateInfo {
                fov: camera._fov,
                near: camera._near,
                far: camera._far,
                position: camera._transform_object.get_position().clone() as Vector3<f32>,
                rotation: camera._transform_object.get_rotation().clone() as Vector3<f32>,
                ..Default::default()
            };
            scene_data_create_info
                ._cameras
                .insert(camera._name.clone(), camera_create_info);
        }
        // lights
        for light_object in self._directional_light_object_map.values() {
            let light = light_object.borrow();
            let light_create_info = DirectionalLightCreateInfo {
                _position: light._transform_object.get_position().clone() as Vector3<f32>,
                _rotation: light._transform_object.get_rotation().clone() as Vector3<f32>,
                ..Default::default()
            };
            scene_data_create_info
                ._directional_lights
                .insert(light._light_name.clone(), light_create_info);
        }
        // effects
        for (effect_name, effect_id) in self._effect_id_map.iter() {
            let effect = self
                .get_effect_manager()
                .get_effect(*effect_id)
                .unwrap()
                .borrow();
            let effect_create_info = EffectCreateInfo {
                _effect_position: effect._effect_transform.get_position().clone() as Vector3<f32>,
                _effect_rotation: effect._effect_transform.get_rotation().clone() as Vector3<f32>,
                _effect_scale: effect._effect_transform.get_scale().clone() as Vector3<f32>,
                _effect_data_name: effect._effect_data.borrow()._effect_data_name.clone(),
            };
            scene_data_create_info
                ._effects
                .insert(effect_name.clone(), effect_create_info);
        }
        // static objects
        for static_object in self._static_render_object_map.values() {
            let object = static_object.borrow();
            let static_object_create_info = RenderObjectCreateInfo {
                _model_data_name: object._model_data.borrow()._model_data_name.clone(),
                _position: object._transform_object.get_position().clone() as Vector3<f32>,
                _rotation: object._transform_object.get_rotation().clone() as Vector3<f32>,
                _scale: object._transform_object.get_scale().clone() as Vector3<f32>,
            };
            scene_data_create_info._static_objects.insert(
                object._render_object_name.clone(),
                static_object_create_info,
            );
        }
        // skeletal objects
        for skeletal_object in self._skeletal_render_object_map.values() {
            let object = skeletal_object.borrow();
            let skeletal_object_create_info = RenderObjectCreateInfo {
                _model_data_name: object._model_data.borrow()._model_data_name.clone(),
                _position: object._transform_object.get_position().clone() as Vector3<f32>,
                _rotation: object._transform_object.get_rotation().clone() as Vector3<f32>,
                _scale: object._transform_object.get_scale().clone() as Vector3<f32>,
            };
            scene_data_create_info._skeletal_objects.insert(
                object._render_object_name.clone(),
                skeletal_object_create_info,
            );
        }

        self.get_engine_resources_mut()
            .save_scene_data(&self._scene_name, &scene_data_create_info);
    }

    pub fn destroy_scene_manager(&mut self) {}

    pub fn update_scene_manager(
        &mut self,
        engine_core: &EngineCore,
        delta_time: f64,
    ) {
        let main_camera = ptr_as_mut(self.get_main_camera());
        main_camera.update_camera_object_data();
        let camera_position = &main_camera.get_camera_position();

        let mut main_light = self._main_light.borrow_mut();
        main_light.update_light_data(camera_position);

        let mut capture_height_map = self._capture_height_map.borrow_mut();
        capture_height_map.update_light_data(camera_position);

        for (_key, render_object_data) in self._static_render_object_map.iter() {
            render_object_data
                .borrow_mut()
                .update_render_object_data(delta_time as f32);
        }

        for (_key, render_object_data) in self._skeletal_render_object_map.iter() {
            render_object_data
                .borrow_mut()
                .update_render_object_data(delta_time as f32);
        }

        // gather render elements
        {
            self._render_element_transform_count = 0;

            SceneManager::gather_render_elements(
                RenderObjectType::Static,
                &main_camera,
                &main_light,
                &self._static_render_object_map,
                &mut self._static_render_elements,
                &mut self._static_shadow_render_elements,
                &mut self._render_element_transform_count,
                &mut self._render_element_transform_matrices,
            );

            SceneManager::gather_render_elements(
                RenderObjectType::Skeletal,
                &main_camera,
                &main_light,
                &self._skeletal_render_object_map,
                &mut self._skeletal_render_elements,
                &mut self._skeletal_shadow_render_elements,
                &mut self._render_element_transform_count,
                &mut self._render_element_transform_matrices,
            );
        }

        // debug text
        let time_data = &engine_core._time_data;
        let font_manager = engine_core.get_font_manager_mut();
        font_manager.log(format!(
            "{:.2}fps / {:.3}ms",
            time_data._average_fps, time_data._average_frame_time
        ));
        font_manager.log(format!(
            "StaticMesh: {:?}, Shadow: {:?}",
            self._static_render_elements.len(),
            self._static_shadow_render_elements.len()
        ));
        font_manager.log(format!(
            "SkeletalMesh: {:?}, Shadow: {:?}",
            self._skeletal_render_elements.len(),
            self._skeletal_shadow_render_elements.len()
        ));
    }
}
