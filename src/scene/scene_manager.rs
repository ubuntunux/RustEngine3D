use std::cmp::Ordering::{Greater, Less};
use std::collections::HashMap;
use std::rc::Rc;

use crate::audio::audio_manager::{AudioInstance, AudioLoop, AudioManager};
use crate::constants::{COLLISION_BLOCK_SIZE, DISTANCE_CULL_FACTOR, INSTANCING_BLOCK_SIZE, MAX_FRAME_COUNT, MAX_POINT_LIGHTS, MAX_TRANSFORM_COUNT};
use crate::effect::effect_data::{EffectCreateInfo, EffectInstance};
use crate::effect::effect_manager::EffectManager;
use crate::renderer::push_constants::PushConstantParameter;
use crate::renderer::renderer_context::RendererContext;
use crate::renderer::renderer_data::{RenderObjectType, RendererData};
use crate::resource::resource::ResourceData::{Audio, AudioBank};
use crate::resource::resource::{EngineResources, ResourceData};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::camera::{CameraCreateInfo, CameraObjectData};
use crate::scene::capture_height_map::CaptureHeightMap;
use crate::scene::collision::CollisionType;
use crate::scene::height_map::HeightMapData;
use crate::scene::light::{
    DirectionalLight, DirectionalLightCreateInfo, LightData, PointLight, PointLightCreateInfo,
    PointLights,
};
use crate::scene::model::ModelData;
use crate::scene::render_element::{RenderElementData, RenderElementInfo};
use crate::scene::render_object::{RenderObjectCreateInfo, RenderObjectData};
use crate::utilities::math;
use crate::utilities::system::{newRcRefCell, ptr_as_mut, ptr_as_ref, RcRefCell};
use crate::{begin_block, constants};
use nalgebra::{Matrix4, Vector, Vector2, Vector3, Vector4};
use serde::{Deserialize, Serialize};

pub type CameraObjectMap = HashMap<SceneObjectID, Rc<CameraObjectData>>;
pub type DirectionalLightObjectMap = HashMap<SceneObjectID, RcRefCell<DirectionalLight>>;
pub type PointLightObjectMap = HashMap<SceneObjectID, RcRefCell<PointLight>>;
pub type RenderObjectInstancingMap<'a> =
    HashMap<RenderObjectInstanceKey<'a>, RcRefCell<RenderObjectData<'a>>>;
pub type RenderObjectMap<'a> = HashMap<SceneObjectID, RcRefCell<RenderObjectData<'a>>>;
pub type RenderObjectCreateInfoMap = HashMap<String, RenderObjectCreateInfo>;
pub type CollisionObjectMap<'a> = HashMap<CollisionObjectKey, Vec<RcRefCell<RenderObjectData<'a>>>>;

#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct SceneObjectID(u64);

#[derive(Copy, Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct CollisionObjectKey {
    pub _indices: Vector3<i32>,
}

impl CollisionObjectKey {
    pub fn new(x: i32, y: i32, z: i32) -> Self {
        CollisionObjectKey {
            _indices: Vector3::new(x, y, z),
        }
    }

    pub fn from_position(position: &Vector3<f32>) -> Self {
        CollisionObjectKey {
            _indices: Vector3::new(
                (position.x / COLLISION_BLOCK_SIZE) as i32,
                (position.y / COLLISION_BLOCK_SIZE) as i32,
                (position.z / COLLISION_BLOCK_SIZE) as i32,
            ),
        }
    }
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct RenderObjectInstanceKey<'a> {
    pub _model_ptr: *const ModelData<'a>,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _is_render_height_map: bool,
    pub _instancing_block_indices: Vector3<i32>,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BoundBoxInstanceData {
    pub _transform: Matrix4<f32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(default)]
pub struct SceneDataCreateInfo {
    pub _sea_height: f32,
    pub _cameras: HashMap<String, CameraCreateInfo>,
    pub _directional_lights: HashMap<String, DirectionalLightCreateInfo>,
    pub _point_lights: HashMap<String, PointLightCreateInfo>,
    pub _effects: HashMap<String, EffectCreateInfo>,
    pub _static_objects: RenderObjectCreateInfoMap,
    pub _skeletal_objects: RenderObjectCreateInfoMap,
}

pub struct SceneManager<'a> {
    pub _engine_resources: *const EngineResources<'a>,
    pub _renderer_data: *const RendererData<'a>,
    pub _audio_manager: *const AudioManager<'a>,
    pub _effect_manager: *const EffectManager<'a>,
    pub _window_size: Vector2<i32>,
    pub _scene_name: String,
    pub _sea_height: f32,
    pub _main_camera: Option<Rc<CameraObjectData>>,
    pub _main_light: Option<RcRefCell<DirectionalLight>>,
    pub _light_probe_cameras: Vec<RcRefCell<CameraObjectData>>,
    pub _camera_object_map: CameraObjectMap,
    pub _capture_height_map: Box<CaptureHeightMap<'a>>,
    pub _directional_light_object_map: DirectionalLightObjectMap,
    pub _point_light_object_map: PointLightObjectMap,
    pub _render_point_lights: PointLights,
    pub _render_point_light_count: i32,
    pub _dynamic_update_object_map: RenderObjectMap<'a>,
    pub _collision_object_map: CollisionObjectMap<'a>,
    pub _collision_object_key_map: HashMap<SceneObjectID, Vec<CollisionObjectKey>>,
    pub _object_id_generator: SceneObjectID,
    pub _static_render_object_instancing_map: RenderObjectInstancingMap<'a>,
    pub _static_render_object_map: RenderObjectMap<'a>,
    pub _skeletal_render_object_map: RenderObjectMap<'a>,
    pub _static_render_elements: Vec<RenderElementData<'a>>,
    pub _static_shadow_render_elements: Vec<RenderElementData<'a>>,
    pub _skeletal_render_elements: Vec<RenderElementData<'a>>,
    pub _skeletal_shadow_render_elements: Vec<RenderElementData<'a>>,
    pub _render_element_transform_count: usize,
    pub _render_element_transform_matrices: Vec<Matrix4<f32>>,
    pub _render_element_transform_offsets: Vec<Vector4<i32>>,
    pub _bound_boxes: Vec<BoundBoxInstanceData>,
    pub _frame_count_for_refresh_light_probe: i32,
}

// Implementation
impl Default for SceneDataCreateInfo {
    fn default() -> SceneDataCreateInfo {
        SceneDataCreateInfo {
            _sea_height: 0.0,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _point_lights: HashMap::new(),
            _effects: HashMap::new(),
            _static_objects: HashMap::new(),
            _skeletal_objects: HashMap::new(),
        }
    }
}

impl<'a> SceneManager<'a> {
    pub fn is_load_complete(&self) -> bool {
        self._capture_height_map.is_capture_height_map_complete()
    }
    pub fn get_main_camera(&self) -> &CameraObjectData {
        self._main_camera.as_ref().unwrap().as_ref()
    }
    pub fn get_main_camera_mut(&self) -> &mut CameraObjectData {
        ptr_as_mut(self._main_camera.as_ref().unwrap().as_ref())
    }
    pub fn get_main_light(&self) -> &RcRefCell<DirectionalLight> {
        &self._main_light.as_ref().unwrap()
    }
    pub fn get_render_point_lights(&self) -> &PointLights {
        &self._render_point_lights
    }
    pub fn get_render_point_light_count(&self) -> i32 {
        self._render_point_light_count
    }
    pub fn get_light_probe_camera(&self, index: usize) -> &RcRefCell<CameraObjectData> {
        &self._light_probe_cameras[index]
    }
    pub fn get_capture_height_map(&self) -> &CaptureHeightMap<'a> {
        self._capture_height_map.as_ref()
    }
    pub fn get_capture_height_map_mut(&self) -> &mut CaptureHeightMap<'a> {
        ptr_as_mut(self._capture_height_map.as_ref())
    }
    pub fn set_start_capture_height_map(&mut self, start: bool) {
        self._capture_height_map.set_start_capture_height_map(start)
    }
    pub fn get_height_map_data(&self) -> &HeightMapData {
        &self._capture_height_map._height_map_data
    }
    pub fn get_height_map_collision_point(
        &self,
        start_pos: &Vector3<f32>,
        dir: &Vector3<f32>,
        limit_dist: f32,
        padding: f32,
        collision_point: &mut Vector3<f32>,
    ) -> bool {
        self._capture_height_map
            ._height_map_data
            .get_collision_point(start_pos, dir, limit_dist, padding, collision_point)
    }
    pub fn get_height_bilinear(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self._capture_height_map
            ._height_map_data
            .get_height_bilinear(pos, lod)
    }
    pub fn get_height_point(&self, pos: &Vector3<f32>, lod: usize) -> f32 {
        self._capture_height_map
            ._height_map_data
            .get_height_point(pos, lod)
    }
    pub fn get_static_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._static_render_elements
    }
    pub fn get_static_shadow_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._static_shadow_render_elements
    }
    pub fn get_skeletal_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._skeletal_render_elements
    }
    pub fn get_skeletal_shadow_render_elements(&self) -> &Vec<RenderElementData<'a>> {
        &self._skeletal_shadow_render_elements
    }
    pub fn get_render_element_transform_count(&self) -> usize {
        self._render_element_transform_count
    }
    pub fn get_render_element_transform_matrices(&self) -> &Vec<Matrix4<f32>> {
        &self._render_element_transform_matrices
    }
    pub fn get_render_element_transform_offsets(&self) -> &Vec<Vector4<i32>> {
        &self._render_element_transform_offsets
    }
    pub fn get_bound_boxes(&self) -> &Vec<BoundBoxInstanceData> {
        &self._bound_boxes
    }
    pub fn create_scene_manager() -> Box<SceneManager<'a>> {
        Box::new(SceneManager {
            _engine_resources: std::ptr::null(),
            _renderer_data: std::ptr::null(),
            _audio_manager: std::ptr::null(),
            _effect_manager: std::ptr::null(),
            _window_size: Vector2::new(1024, 768),
            _scene_name: String::new(),
            _sea_height: 0.0,
            _main_camera: None,
            _main_light: None,
            _light_probe_cameras: Vec::new(),
            _camera_object_map: HashMap::new(),
            _capture_height_map: Box::new(CaptureHeightMap::default()),
            _directional_light_object_map: HashMap::new(),
            _point_light_object_map: HashMap::new(),
            _render_point_lights: PointLights::default(),
            _render_point_light_count: 0,
            _dynamic_update_object_map: HashMap::new(),
            _collision_object_map: HashMap::new(),
            _collision_object_key_map: HashMap::new(),
            _object_id_generator: SceneObjectID(0),
            _static_render_object_instancing_map: HashMap::new(),
            _static_render_object_map: HashMap::new(),
            _skeletal_render_object_map: HashMap::new(),
            _static_render_elements: Vec::new(),
            _static_shadow_render_elements: Vec::new(),
            _skeletal_render_elements: Vec::new(),
            _skeletal_shadow_render_elements: Vec::new(),
            _render_element_transform_count: 0,
            _render_element_transform_matrices: vec![Matrix4::identity(); MAX_TRANSFORM_COUNT],
            _render_element_transform_offsets: vec![Vector4::zeros(); MAX_TRANSFORM_COUNT],
            _bound_boxes: Vec::new(),
            _frame_count_for_refresh_light_probe: 0,
        })
    }

    pub fn initialize_scene_manager(
        &mut self,
        renderer_context: &RendererContext<'a>,
        audio_manager: &AudioManager<'a>,
        effect_manager: &EffectManager<'a>,
        engine_resources: &EngineResources<'a>,
        window_size: &Vector2<i32>,
    ) {
        self._renderer_data = renderer_context.get_renderer_data();
        self._audio_manager = audio_manager;
        self._effect_manager = effect_manager;
        self._engine_resources = engine_resources;

        let default_camera = CameraObjectData::create_camera_object_data(
            self.generate_object_id(),
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
                self.generate_object_id(),
                &String::from("light_probe_camera0"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                self.generate_object_id(),
                &String::from("light_probe_camera1"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                self.generate_object_id(),
                &String::from("light_probe_camera2"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                self.generate_object_id(),
                &String::from("light_probe_camera3"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                self.generate_object_id(),
                &String::from("light_probe_camera4"),
                &light_probe_camera_create_info,
            )),
            newRcRefCell(CameraObjectData::create_camera_object_data(
                self.generate_object_id(),
                &String::from("light_probe_camera5"),
                &light_probe_camera_create_info,
            )),
        ];
        let default_light = DirectionalLight::create_directional_light(
            self.generate_object_id(),
            &String::from("default_light"),
            &DirectionalLightCreateInfo::default(),
        );
        let capture_height_map = Box::new(CaptureHeightMap::create_capture_height_map(
            self.generate_object_id(),
        ));

        // assign
        self._main_camera = Some(Rc::new(default_camera));
        self._main_light = Some(newRcRefCell(default_light));
        self._capture_height_map = capture_height_map;
        self._light_probe_cameras = light_probe_cameras;

        // done
        self.update_window_size(window_size.x, window_size.y);
    }
    pub fn get_engine_resources(&self) -> &EngineResources<'a> {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources<'a> {
        ptr_as_mut(self._engine_resources)
    }
    pub fn get_renderer_data(&self) -> &RendererData<'a> {
        ptr_as_ref(self._renderer_data)
    }
    pub fn get_renderer_data_mut(&self) -> &mut RendererData<'a> {
        ptr_as_mut(self._renderer_data)
    }
    pub fn get_effect_manager(&self) -> &EffectManager<'a> {
        ptr_as_ref(self._effect_manager)
    }
    pub fn get_effect_manager_mut(&self) -> &mut EffectManager<'a> {
        ptr_as_mut(self._effect_manager)
    }
    pub fn get_audio_manager(&self) -> &AudioManager<'a> {
        ptr_as_ref(self._audio_manager)
    }
    pub fn get_audio_manager_mut(&self) -> &mut AudioManager<'a> {
        ptr_as_mut(self._audio_manager)
    }
    pub fn set_effect_manager(&mut self, effect_manager: *const EffectManager<'a>) {
        self._effect_manager = effect_manager;
    }
    pub fn get_sea_height(&self) -> f32 {
        self._sea_height
    }
    pub fn get_dead_zone_height(&self) -> f32 {
        self._sea_height - 5.0
    }
    pub fn generate_object_id(&mut self) -> SceneObjectID {
        let object_id = self._object_id_generator.clone();
        self._object_id_generator = SceneObjectID(self._object_id_generator.0 + 1);
        object_id
    }
    pub fn add_camera_object(
        &mut self,
        object_name: &str,
        camera_create_info: &CameraCreateInfo,
    ) -> Rc<CameraObjectData> {
        let object_id = self.generate_object_id();
        log::info!("camera_create_info: {:?}", camera_create_info);
        let camera_object_data = Rc::new(CameraObjectData::create_camera_object_data(
            object_id,
            &String::from(object_name),
            camera_create_info,
        ));
        self._camera_object_map
            .insert(object_id, camera_object_data.clone());
        camera_object_data
    }
    pub fn add_light_object(
        &mut self,
        object_name: &str,
        light_create_info: &DirectionalLightCreateInfo,
    ) -> RcRefCell<DirectionalLight> {
        let object_id = self.generate_object_id();
        let light_object_data = newRcRefCell(DirectionalLight::create_directional_light(
            object_id,
            &String::from(object_name),
            light_create_info,
        ));
        self._directional_light_object_map
            .insert(object_id, light_object_data.clone());
        light_object_data
    }
    pub fn add_point_light_object(
        &mut self,
        object_name: &str,
        light_create_info: &PointLightCreateInfo,
    ) -> RcRefCell<PointLight> {
        let object_id = self.generate_object_id();
        let light_object_data = newRcRefCell(PointLight::create_point_light(
            object_id,
            &String::from(object_name),
            light_create_info,
        ));
        self._point_light_object_map
            .insert(object_id, light_object_data.clone());
        light_object_data
    }
    pub fn collect_collision_objects(
        &self,
        bound_min: &Vector3<f32>,
        bound_max: &Vector3<f32>,
    ) -> RenderObjectMap<'a> {
        let mut collision_objectm_map: RenderObjectMap<'a> = HashMap::new();
        let key_min = CollisionObjectKey::from_position(&bound_min);
        let key_max = CollisionObjectKey::from_position(&bound_max);
        for z in key_min._indices.z..(key_max._indices.z + 1) {
            for y in key_min._indices.y..(key_max._indices.y + 1) {
                for x in key_min._indices.x..(key_max._indices.x + 1) {
                    if let Some(collision_objects) = self
                        ._collision_object_map
                        .get(&CollisionObjectKey::new(x, y, z))
                    {
                        for collision_object in collision_objects.iter() {
                            collision_objectm_map.insert(
                                collision_object.borrow().get_object_id(),
                                collision_object.clone(),
                            );
                        }
                    }
                }
            }
        }
        collision_objectm_map
    }
    pub fn register_collision_object(&mut self, object: &RcRefCell<RenderObjectData<'a>>) {
        if object.borrow().get_collision_type() != CollisionType::NONE {
            let key_min =
                CollisionObjectKey::from_position(&object.borrow()._collision._bounding_box._min);
            let key_max =
                CollisionObjectKey::from_position(&object.borrow()._collision._bounding_box._max);
            for z in key_min._indices.z..(key_max._indices.z + 1) {
                for y in key_min._indices.y..(key_max._indices.y + 1) {
                    for x in key_min._indices.x..(key_max._indices.x + 1) {
                        let new_key = CollisionObjectKey::new(x, y, z);
                        if let Some(collision_objects) =
                            self._collision_object_map.get_mut(&new_key)
                        {
                            collision_objects.push(object.clone());
                        } else {
                            self._collision_object_map
                                .insert(new_key, vec![object.clone()]);
                        }

                        let scene_object_id = object.borrow().get_object_id();
                        if let Some(collision_object_keys) =
                            self._collision_object_key_map.get_mut(&scene_object_id)
                        {
                            collision_object_keys.push(new_key);
                        } else {
                            self._collision_object_key_map
                                .insert(scene_object_id, vec![new_key]);
                        }
                    }
                }
            }
        }
    }
    pub fn unregister_collision_object(&mut self, object_id: &SceneObjectID) {
        if let Some(keys) = self._collision_object_key_map.remove(&object_id) {
            for key in keys {
                if let Some(collision_objects) = self._collision_object_map.get_mut(&key) {
                    for (i, collision_object) in collision_objects.iter().enumerate() {
                        if collision_object.borrow().get_object_id() == *object_id {
                            collision_objects.remove(i);
                            break;
                        }
                    }
                }
            }
        }
    }
    pub fn get_dynamic_update_objects(&self) -> &RenderObjectMap<'a> {
        &self._dynamic_update_object_map
    }
    pub fn register_dynamic_update_object(&mut self, object: &RcRefCell<RenderObjectData<'a>>) {
        self._dynamic_update_object_map
            .insert(object.borrow().get_object_id(), object.clone());
    }
    pub fn unregister_dynamic_update_object(&mut self, object_id: &SceneObjectID) {
        self._dynamic_update_object_map.remove(&object_id);
    }
    pub fn add_terrain_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
    ) -> RcRefCell<RenderObjectData<'a>> {
        self.add_static_render_object_internal(
            object_name,
            render_object_create_info,
            Some(CollisionType::NONE),
            true,
            false,
        )
    }
    pub fn add_dynamic_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
        collision_type: Option<CollisionType>,
    ) -> RcRefCell<RenderObjectData<'a>> {
        self.add_static_render_object_internal(
            object_name,
            render_object_create_info,
            collision_type,
            false,
            true,
        )
    }
    pub fn add_static_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
        collision_type: Option<CollisionType>,
    ) -> RcRefCell<RenderObjectData<'a>> {
        self.add_static_render_object_internal(
            object_name,
            render_object_create_info,
            collision_type,
            false,
            false,
        )
    }
    pub fn add_static_render_object_internal(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
        custom_collision_type: Option<CollisionType>,
        is_render_height_map: bool,
        is_dynamic_update_object: bool,
    ) -> RcRefCell<RenderObjectData<'a>> {
        let object_id = self.generate_object_id();
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(
            object_id,
            &String::from(object_name),
            self.get_engine_resources()
                .get_model_data(&render_object_create_info._model_data_name),
            &render_object_create_info,
            custom_collision_type,
            is_render_height_map,
        ));

        // register a collision object
        if render_object_data.borrow().get_collision_type() != CollisionType::NONE {
            self.register_collision_object(&render_object_data);
        }

        // register render object
        if is_dynamic_update_object {
            self.register_dynamic_update_object(&render_object_data);
            self._static_render_object_map
                .insert(object_id, render_object_data.clone());
        } else {
            // auto instancing
            let render_object_data_ref = render_object_data.borrow();
            let instance_key = RenderObjectInstanceKey {
                _model_ptr: render_object_data_ref.get_model_data().as_ptr(),
                _is_render_camera: render_object_data_ref.is_render_camera(),
                _is_render_shadow: render_object_data_ref.is_render_shadow(),
                _is_render_height_map: render_object_data_ref.is_render_height_map(),
                _instancing_block_indices: Vector3::new(
                    (render_object_create_info._position.x / INSTANCING_BLOCK_SIZE) as i32,
                    (render_object_create_info._position.y / INSTANCING_BLOCK_SIZE) as i32,
                    (render_object_create_info._position.z / INSTANCING_BLOCK_SIZE) as i32,
                ),
            };

            let instancing_render_object: RcRefCell<RenderObjectData<'a>> =
                if let Some(find_object) =
                    self._static_render_object_instancing_map.get(&instance_key)
                {
                    find_object.clone()
                } else {
                    let new_instancing_object_id = self.generate_object_id();
                    let new_instancing_render_object =
                        newRcRefCell(RenderObjectData::create_render_object_data(
                            new_instancing_object_id,
                            &String::from(object_name),
                            render_object_data_ref.get_model_data(),
                            &RenderObjectCreateInfo {
                                _model_data_name: render_object_create_info
                                    ._model_data_name
                                    .clone(),
                                _position: Vector3::zeros(),
                                _rotation: Vector3::zeros(),
                                _scale: Vector3::new(1.0, 1.0, 1.0),
                            },
                            custom_collision_type,
                            is_render_height_map,
                        ));

                    self._static_render_object_map.insert(
                        new_instancing_object_id,
                        new_instancing_render_object.clone(),
                    );
                    self._static_render_object_instancing_map
                        .insert(instance_key, new_instancing_render_object.clone());
                    new_instancing_render_object
                };
            instancing_render_object
                .borrow_mut()
                .register_instancing_render_object(&render_object_data);
        }

        render_object_data
    }
    pub fn add_skeletal_render_object(
        &mut self,
        object_name: &str,
        render_object_create_info: &RenderObjectCreateInfo,
    ) -> RcRefCell<RenderObjectData<'a>> {
        let object_id = self.generate_object_id();
        let model_data = self
            .get_engine_resources()
            .get_model_data(&render_object_create_info._model_data_name);
        let render_object_data = newRcRefCell(RenderObjectData::create_render_object_data(
            object_id,
            &String::from(object_name),
            model_data,
            &render_object_create_info,
            None,
            false,
        ));
        self.register_dynamic_update_object(&render_object_data);
        self._skeletal_render_object_map
            .insert(object_id, render_object_data.clone());
        render_object_data
    }

    pub fn add_effect(&mut self, object_name: &str, effect_create_info: &EffectCreateInfo) -> i64 {
        let effect_data = self
            .get_engine_resources()
            .get_effect_data(&effect_create_info._effect_data_name);
        self.get_effect_manager_mut()
            .create_effect(object_name, effect_create_info, &effect_data)
    }

    pub fn play_audio_bank(&self, audio_name_bank: &str) -> Option<RcRefCell<AudioInstance>> {
        self.get_audio_manager_mut().play_audio_bank(audio_name_bank, AudioLoop::ONCE, None)
    }

    pub fn play_audio(&self, audio_resource: &ResourceData) -> Option<RcRefCell<AudioInstance>> {
        match audio_resource {
            Audio(audio_data) => {
                self.get_audio_manager_mut().play_audio_data(&audio_data, AudioLoop::ONCE, None)
            }
            AudioBank(audio_bank_data) => self.get_audio_manager_mut().play_audio_bank_data(
                &audio_bank_data,
                AudioLoop::ONCE,
                None,
            ),
            _ => None,
        }
    }

    pub fn play_audio_options(
        &self,
        audio_name_bank: &str,
        audio_loop: AudioLoop,
        volume: Option<f32>,
    ) -> Option<RcRefCell<AudioInstance>> {
        self.get_audio_manager_mut().play_audio_bank(audio_name_bank, audio_loop, volume)
    }

    pub fn stop_audio_instance(&self, audio_instance: &RcRefCell<AudioInstance>) {
        self.get_audio_manager_mut().stop_audio_instance(audio_instance)
    }

    pub fn get_static_render_object_map(&self) -> &RenderObjectMap<'a> {
        &self._static_render_object_map
    }

    pub fn get_static_render_object(
        &self,
        object_id: SceneObjectID,
    ) -> Option<&RcRefCell<RenderObjectData<'a>>> {
        self._static_render_object_map.get(&object_id)
    }

    pub fn remove_static_render_object(&mut self, object_id: SceneObjectID) {
        self._static_render_object_map.remove(&object_id);
        self.unregister_collision_object(&object_id);
        self.unregister_dynamic_update_object(&object_id);
    }

    pub fn get_skeletal_render_object_map(&self) -> &RenderObjectMap<'a> {
        &self._skeletal_render_object_map
    }

    pub fn get_skeletal_render_object(
        &self,
        object_id: SceneObjectID,
    ) -> Option<&RcRefCell<RenderObjectData<'a>>> {
        self._skeletal_render_object_map.get(&object_id)
    }

    pub fn remove_skeletal_render_object(&mut self, object_id: SceneObjectID) {
        self._skeletal_render_object_map.remove(&object_id);
        self.unregister_collision_object(&object_id);
        self.unregister_dynamic_update_object(&object_id);
    }

    pub fn get_effect(&self, effect_id: i64) -> Option<&RcRefCell<EffectInstance<'a>>> {
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
        to_geometry: &Vector3<f32>
    ) -> bool {
        for plane in camera._view_frustum_planes.iter() {
            let d = plane.dot(&to_geometry);
            if geometry_bound_box._radius < d {
                return true;
            }
        }
        false
    }
    pub fn shadow_culling(light: &DirectionalLight, geometry_bound_box: &BoundingBox) -> bool {
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
        light: &DirectionalLight,
        render_object_map: &RenderObjectMap<'a>,
        render_elements: &mut Vec<RenderElementData<'a>>,
        render_shadow_elements: &mut Vec<RenderElementData<'a>>,
        capture_height_map_elements: &mut Vec<RenderElementData<'a>>,
        transform_offset_index: &mut usize,
        transform_offset_index_for_shadow: &mut usize,
        transform_offset_index_for_height_map: &mut usize,
        render_element_transform_count: &mut usize,
        render_element_transform_offsets: &mut Vec<Vector4<i32>>,
        render_element_transform_matrices: &mut Vec<Matrix4<f32>>,
        bound_boxes: &mut Vec<BoundBoxInstanceData>,
        enable_capture_height_map: bool,
        height_map_bounding_box: &mut BoundingBox,
    ) {
        let mut render_element_info_list: Vec<RenderElementInfo<'a>> = Vec::new();
        let mut bound_min = Vector3::new(f32::MAX, f32::MAX, f32::MAX) * 0.5;
        let mut bound_max = Vector3::new(f32::MIN, f32::MIN, f32::MIN) * 0.5;

        // gather render object
        for (_key, render_object_refcell) in render_object_map.iter() {
            let render_object_data = ptr_as_ref(render_object_refcell.as_ptr());

            let to_geometry = &render_object_data._bounding_box._center - camera.get_camera_position();
            let is_distance_culled = render_object_data._is_render_height_map == false && render_object_data._bounding_box._radius * DISTANCE_CULL_FACTOR <= to_geometry.dot(&to_geometry);

            let is_render_camera = render_object_data._is_visible
                && render_object_data._is_render_camera
                && SceneManager::view_frustum_culling_geometry(camera, &render_object_data._bounding_box, &to_geometry) == false;

            let is_render_shadow = render_object_data._is_visible
                && render_object_data._is_render_shadow
                && SceneManager::shadow_culling(light, &render_object_data._bounding_box) == false;

            let is_render_height_map = enable_capture_height_map
                && render_object_data._is_visible
                && render_object_data._is_render_height_map;

            if is_render_height_map {
                bound_min = math::get_min(&bound_min, &render_object_data._bounding_box._min);
                bound_max = math::get_max(&bound_max, &render_object_data._bounding_box._max);
            }

            // render element for bound box
            if unsafe { constants::RENDER_BOUND_BOX } && is_render_camera {
                bound_boxes.push(BoundBoxInstanceData {
                    _transform: math::combinate_matrix2(
                        &render_object_data._bounding_box._center,
                        &render_object_data._bounding_box._orientation,
                        &(render_object_data._bounding_box._extents),
                    ),
                });
            }

            // gather render element infos
            if is_render_camera || is_render_shadow || is_render_height_map {
                let local_matrix_count = if render_object_data.is_instancing_render_object() {
                    render_object_data.get_instancing_render_object_count()
                } else {
                    1
                };
                let bone_count = render_object_data.get_bone_count();
                let required_transform_count = match render_object_type {
                    RenderObjectType::Static => local_matrix_count,
                    RenderObjectType::Skeletal =>
                    // transform matrix offset: localMatrixPrev(1) + localMatrix(1) + prev_animation_bone_count + curr_animation_bone_count
                    {
                        local_matrix_count + local_matrix_count + bone_count + bone_count
                    }
                };

                if (*render_element_transform_count + required_transform_count)
                    <= MAX_TRANSFORM_COUNT
                {
                    let model_data = ptr_as_ref(render_object_data.get_model_data().as_ptr());
                    let mesh_data_refcell = model_data.get_mesh_data();
                    let mesh_data = ptr_as_ref(mesh_data_refcell.as_ptr());
                    let geometry_count = mesh_data.get_geometry_data_count();

                    // gather render element
                    for geometry_index in 0..geometry_count {
                        render_element_info_list.push(RenderElementInfo {
                            _render_object: render_object_refcell.clone(),
                            _mesh_data: mesh_data_refcell.clone(),
                            _transform_offset: *render_element_transform_count,
                            _is_render_camera: is_render_camera && is_distance_culled == false,
                            _is_render_shadow: is_render_shadow && is_distance_culled == false,
                            _is_render_height_map: is_render_height_map,
                            _geometry_index: geometry_index,
                            _geometry_data: mesh_data.get_geometry_data(geometry_index).clone(),
                            _material_instance_data: model_data
                                .get_material_instance_data(geometry_index)
                                .clone(),
                        });
                    }

                    // update render_element_transform_matrices
                    match render_object_type {
                        RenderObjectType::Static => {
                            // local matrix
                            if render_object_data.is_instancing_render_object() {
                                let require_render_element_transform_count: usize =
                                    *render_element_transform_count + local_matrix_count;
                                render_element_transform_matrices[*render_element_transform_count
                                    ..require_render_element_transform_count]
                                    .copy_from_slice(
                                        render_object_data
                                            .get_instancing_render_object_transforms(),
                                    );
                            } else {
                                render_element_transform_matrices[*render_element_transform_count]
                                    .copy_from(&render_object_data._final_transform);
                            }
                            *render_element_transform_count += local_matrix_count;
                        }
                        RenderObjectType::Skeletal => {
                            // local matrix prev
                            render_element_transform_matrices[*render_element_transform_count]
                                .copy_from(&render_object_data._prev_transform);
                            *render_element_transform_count += local_matrix_count;

                            // local matrix
                            render_element_transform_matrices[*render_element_transform_count]
                                .copy_from(&render_object_data._final_transform);
                            *render_element_transform_count += local_matrix_count;

                            if render_object_data.has_animation() {
                                // prev animation buffer
                                let prev_animation_buffer: &Vec<Matrix4<f32>> =
                                    render_object_data.get_prev_animation_buffer();
                                assert_eq!(bone_count, prev_animation_buffer.len());
                                let require_render_element_transform_count: usize =
                                    *render_element_transform_count + bone_count;
                                render_element_transform_matrices[*render_element_transform_count
                                    ..require_render_element_transform_count]
                                    .copy_from_slice(prev_animation_buffer);
                                *render_element_transform_count =
                                    require_render_element_transform_count;

                                // current animation buffer
                                let animation_buffer: &Vec<Matrix4<f32>> =
                                    render_object_data.get_animation_buffer();
                                assert_eq!(bone_count, animation_buffer.len());
                                let require_render_element_transform_count: usize =
                                    *render_element_transform_count + bone_count;
                                render_element_transform_matrices[*render_element_transform_count
                                    ..require_render_element_transform_count]
                                    .copy_from_slice(animation_buffer);
                                *render_element_transform_count =
                                    require_render_element_transform_count;
                            }
                        }
                    }
                }
            }
        }

        // update height map bounding box
        if enable_capture_height_map {
            *height_map_bounding_box = BoundingBox::create_bounding_box(&bound_min, &bound_max);
        }

        // sort render element info list by mesh
        render_element_info_list.sort_by(
            |lhs: &RenderElementInfo<'a>, rhs: &RenderElementInfo<'a>| {
                if lhs._mesh_data.as_ptr() < rhs._mesh_data.as_ptr() {
                    return Less;
                } else if rhs._mesh_data.as_ptr() < lhs._mesh_data.as_ptr() {
                    return Greater;
                }

                if lhs._geometry_data.as_ptr() < rhs._geometry_data.as_ptr() {
                    return Less;
                } else if rhs._geometry_data.as_ptr() < lhs._geometry_data.as_ptr() {
                    return Greater;
                }

                if lhs._material_instance_data.as_ptr() < rhs._material_instance_data.as_ptr() {
                    return Less;
                } else if rhs._material_instance_data.as_ptr()
                    < lhs._material_instance_data.as_ptr()
                {
                    return Greater;
                }
                return Less;
            },
        );

        // build render element data
        let mut render_element_count: usize = 0;
        let mut render_shadow_element_count: usize = 0;
        let mut capture_height_map_element_count: usize = 0;
        let num_render_element_info = render_element_info_list.len();
        for render_element_index in 0..num_render_element_info {
            let render_element_info = &render_element_info_list[render_element_index];
            let render_object_data = ptr_as_ref(render_element_info._render_object.as_ptr());
            let instancing_count = if render_object_data.is_instancing_render_object() {
                render_object_data.get_instancing_render_object_count()
            } else {
                1
            };

            // update transform offset
            for instance_index in 0..instancing_count {
                let transform_offset =
                    (render_element_info._transform_offset + instance_index) as i32;
                if render_element_info._is_render_camera {
                    render_element_transform_offsets[*transform_offset_index].x = transform_offset;
                    *transform_offset_index += 1;
                    render_element_count += 1;
                }

                if render_element_info._is_render_shadow {
                    render_element_transform_offsets[*transform_offset_index_for_shadow].y =
                        transform_offset;
                    *transform_offset_index_for_shadow += 1;
                    render_shadow_element_count += 1;
                }

                if enable_capture_height_map && render_element_info._is_render_height_map {
                    render_element_transform_offsets[*transform_offset_index_for_height_map].z =
                        transform_offset;
                    *transform_offset_index_for_height_map += 1;
                    capture_height_map_element_count += 1;
                }
            }

            let next_render_element_info = &render_element_info_list
                [(num_render_element_info - 1).min(render_element_index + 1)];
            let is_changed = render_element_info._mesh_data.as_ptr()
                != next_render_element_info._mesh_data.as_ptr()
                || render_element_info._geometry_data.as_ptr()
                    != next_render_element_info._geometry_data.as_ptr()
                || render_element_info._material_instance_data.as_ptr()
                    != next_render_element_info._material_instance_data.as_ptr();
            let is_last = render_element_index == (num_render_element_info - 1);

            // update push constants data
            if is_last || is_changed {
                let mut push_constant_data_list = render_object_data
                    .get_push_constant_data_list(render_element_info._geometry_index)
                    .clone();
                for push_constant_data_mut in push_constant_data_list.iter_mut() {
                    push_constant_data_mut
                        ._push_constant
                        .set_push_constant_parameter(
                            "_transform_offset_index",
                            &PushConstantParameter::Int(
                                (*transform_offset_index - render_element_count) as i32,
                            ),
                        );
                    push_constant_data_mut
                        ._push_constant
                        .set_push_constant_parameter(
                            "_bone_count",
                            &PushConstantParameter::Int(render_object_data.get_bone_count() as i32),
                        );
                }

                begin_block!("render element"); {
                    if 0 < render_element_count {
                        render_elements.push(RenderElementData {
                            _geometry_data: render_element_info._geometry_data.clone(),
                            _material_instance_data: render_element_info
                                ._material_instance_data
                                .clone(),
                            _push_constant_data_list: push_constant_data_list.clone(),
                            _num_render_instances: render_element_count as u32,
                        });
                        render_element_count = 0;
                    }
                }

                begin_block!("render shadow element");
                {
                    // update push constants data for shadow
                    for push_constant_data_mut in push_constant_data_list.iter_mut() {
                        push_constant_data_mut
                            ._push_constant
                            .set_push_constant_parameter(
                                "_transform_offset_index",
                                &PushConstantParameter::Int(
                                    (*transform_offset_index_for_shadow
                                        - render_shadow_element_count)
                                        as i32,
                                ),
                            );
                    }

                    // add render shadow element
                    if 0 < render_shadow_element_count {
                        render_shadow_elements.push(RenderElementData {
                            _geometry_data: render_element_info._geometry_data.clone(),
                            _material_instance_data: render_element_info
                                ._material_instance_data
                                .clone(),
                            _push_constant_data_list: push_constant_data_list.clone(),
                            _num_render_instances: render_shadow_element_count as u32,
                        });
                        render_shadow_element_count = 0;
                    }
                }

                begin_block!("capture height map element");
                {
                    // update push constants data for shadow
                    for push_constant_data_mut in push_constant_data_list.iter_mut() {
                        push_constant_data_mut
                            ._push_constant
                            .set_push_constant_parameter(
                                "_transform_offset_index",
                                &PushConstantParameter::Int(
                                    (*transform_offset_index_for_height_map
                                        - capture_height_map_element_count)
                                        as i32,
                                ),
                            );
                    }

                    // add capture height map element
                    if 0 < capture_height_map_element_count {
                        capture_height_map_elements.push(RenderElementData {
                            _geometry_data: render_element_info._geometry_data.clone(),
                            _material_instance_data: render_element_info
                                ._material_instance_data
                                .clone(),
                            _push_constant_data_list: push_constant_data_list.clone(),
                            _num_render_instances: capture_height_map_element_count as u32,
                        });
                        capture_height_map_element_count = 0;
                    }
                }
            }
        }
    }

    pub fn update_window_size(&mut self, width: i32, height: i32) {
        self._window_size.x = width;
        self._window_size.y = height;
        self.get_main_camera_mut().set_aspect(width, height);
    }

    pub fn create_default_scene_data(&self, scene_data_name: &str) {
        let mut scene_data_create_info = SceneDataCreateInfo {
            _sea_height: 0.0,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _point_lights: HashMap::new(),
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
                _light_data: LightData {
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
        let engine_resources = ptr_as_ref(self._engine_resources);
        if false == engine_resources.has_scene_data(scene_data_name) {
            self.create_default_scene_data(scene_data_name);
        }

        let scene_data_create_info = engine_resources.get_scene_data(scene_data_name).borrow();

        self.create_scene_data(&scene_data_create_info);
    }

    pub fn create_scene_data(&mut self, scene_data_create_info: &SceneDataCreateInfo) {
        self.initialize_light_probe_cameras();

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
                self._main_camera = Some(camera_object);
            }
        }

        // directional lights
        for (index, (object_name, light_create_info)) in scene_data_create_info
            ._directional_lights
            .iter()
            .enumerate()
        {
            let light_create_info = DirectionalLightCreateInfo {
                _position: light_create_info._position.clone() as Vector3<f32>,
                _rotation: light_create_info._rotation.clone() as Vector3<f32>,
                _light_data: LightData {
                    _light_position: light_create_info._light_data._light_position.clone(),
                    _light_direction: light_create_info._light_data._light_direction.clone(),
                    _light_color: light_create_info._light_data._light_color.clone(),
                    ..Default::default()
                },
                ..Default::default()
            };
            let light_object = self.add_light_object(object_name, &light_create_info);
            if 0 == index {
                self._main_light = Some(light_object);
            }
        }

        // point lights
        for (object_name, light_create_info) in scene_data_create_info._point_lights.iter() {
            self.add_point_light_object(object_name, light_create_info);
        }

        // effects
        for (object_name, effect_create_info) in scene_data_create_info._effects.iter() {
            self.add_effect(object_name, effect_create_info);
        }

        // static objects
        for (object_name, render_object_create_info) in
            scene_data_create_info._static_objects.iter()
        {
            self.add_static_render_object(object_name, render_object_create_info, None);
        }

        // skeletal objects
        for (object_name, render_object_create_info) in
            scene_data_create_info._skeletal_objects.iter()
        {
            self.add_skeletal_render_object(object_name, render_object_create_info);
        }

        // refresh light probe
        self.reset_frame_count_for_refresh_light_probe();
    }

    pub fn close_scene_data(&mut self) {
        self._capture_height_map.clear_static_render_elements();
        self.get_effect_manager_mut().clear_effects();

        self._main_camera = None;
        self._main_light = None;
        self._camera_object_map.clear();
        self._directional_light_object_map.clear();
        self._point_light_object_map.clear();
        self._render_point_light_count = 0;
        self._dynamic_update_object_map.clear();
        self._collision_object_map.clear();
        self._collision_object_key_map.clear();
        self._static_render_object_instancing_map.clear();
        self._static_render_object_map.clear();
        self._skeletal_render_object_map.clear();
        self._static_render_elements.clear();
        self._static_shadow_render_elements.clear();
        self._skeletal_render_elements.clear();
        self._skeletal_shadow_render_elements.clear();
        self._render_element_transform_count = 0;
        self._bound_boxes.clear();
    }

    pub fn save_scene_data(&mut self) {
        let mut scene_data_create_info = SceneDataCreateInfo {
            _sea_height: self._sea_height,
            _cameras: HashMap::new(),
            _directional_lights: HashMap::new(),
            _point_lights: HashMap::new(),
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
        for (_effect_id, effect) in self.get_effect_manager().get_effects() {
            let effect = effect.borrow();
            let effect_create_info = EffectCreateInfo {
                _effect_position: effect._effect_transform.get_position().clone() as Vector3<f32>,
                _effect_rotation: effect._effect_transform.get_rotation().clone() as Vector3<f32>,
                _effect_scale: effect._effect_transform.get_scale().clone() as Vector3<f32>,
                _effect_data_name: effect._effect_data.borrow()._effect_data_name.clone(),
            };
            scene_data_create_info
                ._effects
                .insert(effect.get_effect_name().clone(), effect_create_info);
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

    pub fn reset_frame_count_for_refresh_light_probe(&mut self) {
        self._frame_count_for_refresh_light_probe = MAX_FRAME_COUNT as i32 + 1;
    }

    pub fn reset_render_light_probe_time(&mut self) {
        self.get_renderer_data_mut().reset_render_light_probe_time();
    }

    pub fn update_scene_manager(&mut self, delta_time: f64) {
        // refresh environment map
        if 0 <= self._frame_count_for_refresh_light_probe {
            self.get_renderer_data_mut().reset_render_light_probe_time();
            self._frame_count_for_refresh_light_probe -= 1;
        }

        let main_camera = ptr_as_mut(self.get_main_camera());
        main_camera.update_camera_object_data();
        let camera_position = &main_camera.get_camera_position();

        let mut main_light = self._main_light.as_ref().unwrap().borrow_mut();
        main_light.update_light_data(camera_position);

        for (_key, update_object_data) in self._dynamic_update_object_map.iter() {
            update_object_data
                .borrow_mut()
                .update_render_object_data(delta_time as f32);
        }

        begin_block!("gather point lights");
        {
            // TODO: cull point light
            self._render_point_light_count = 0;
            for (i, point_light_data) in self._point_light_object_map.values().enumerate() {
                self._render_point_lights._point_light_data[i] =
                    point_light_data.borrow()._light_data.clone();
                self._render_point_light_count += 1;
                if MAX_POINT_LIGHTS <= self._render_point_light_count as usize {
                    break;
                }
            }
        }

        begin_block!("gather render elements");
        {
            self._static_render_elements.clear();
            self._static_shadow_render_elements.clear();
            self._skeletal_render_elements.clear();
            self._skeletal_shadow_render_elements.clear();
            self._capture_height_map._skeletal_render_elements.clear();
            self._render_element_transform_count = 0;
            let mut transform_offset_index: usize = 0;
            let mut transform_offset_index_for_shadow: usize = 0;
            let mut transform_offset_index_for_height_map: usize = 0;

            self._bound_boxes.clear();

            let capture_height_map_fot_static_mesh: bool =
                self._capture_height_map.is_start_capture_height_map();
            let capture_height_map_fot_skeletal_mesh: bool = false;
            let mut height_map_bounding_box_for_static_mesh = BoundingBox::default();
            let mut height_map_bounding_box_for_skeletal_mesh = BoundingBox::default();

            SceneManager::gather_render_elements(
                RenderObjectType::Static,
                &main_camera,
                &main_light,
                &self._static_render_object_map,
                &mut self._static_render_elements,
                &mut self._static_shadow_render_elements,
                &mut self._capture_height_map._static_render_elements,
                &mut transform_offset_index,
                &mut transform_offset_index_for_shadow,
                &mut transform_offset_index_for_height_map,
                &mut self._render_element_transform_count,
                &mut self._render_element_transform_offsets,
                &mut self._render_element_transform_matrices,
                &mut self._bound_boxes,
                capture_height_map_fot_static_mesh,
                &mut height_map_bounding_box_for_static_mesh,
            );

            SceneManager::gather_render_elements(
                RenderObjectType::Skeletal,
                &main_camera,
                &main_light,
                &self._skeletal_render_object_map,
                &mut self._skeletal_render_elements,
                &mut self._skeletal_shadow_render_elements,
                &mut self._capture_height_map._skeletal_render_elements,
                &mut transform_offset_index,
                &mut transform_offset_index_for_shadow,
                &mut transform_offset_index_for_height_map,
                &mut self._render_element_transform_count,
                &mut self._render_element_transform_offsets,
                &mut self._render_element_transform_matrices,
                &mut self._bound_boxes,
                capture_height_map_fot_skeletal_mesh,
                &mut height_map_bounding_box_for_skeletal_mesh,
            );

            if capture_height_map_fot_static_mesh {
                let dead_zone = self.get_dead_zone_height();
                self._capture_height_map
                    .update_capture_height_map(height_map_bounding_box_for_static_mesh, dead_zone);
                if self._capture_height_map.need_to_render_height_map() == false {
                    self._capture_height_map.set_capture_height_map_complete();
                }
                self._capture_height_map.set_start_capture_height_map(false);
            }
        }
    }
}
