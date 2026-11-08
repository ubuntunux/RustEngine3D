use crate::core::engine_service_locator::get_scene_manager;
use crate::ecs::entity::{EntityId, INVALID_ENTITY_ID};
use crate::renderer::push_constants::PushConstantParameter;
use crate::scene::animation::{
    AnimationBuffer, AnimationData, AnimationLayerData, AnimationPlayArgs, AnimationPlayInfo,
    AnimationPlayInfoSaveData, INVALID_BONE_INDEX,
};
use crate::scene::bounding_box::BoundingBox;
use crate::scene::collision::{CollisionData, CollisionType};
use crate::scene::material_instance::MaterialInstanceData;
use crate::scene::mesh::MeshData;
use crate::scene::model::ModelData;
use crate::scene::scene_manager::{RenderObjectMap, SceneObjectID};
use crate::scene::socket::Socket;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::math;
use crate::utilities::system::{RcRefCell, newRcRefCell, ptr_as_mut, ptr_as_ref};
use crate::vulkan_context::render_pass::PipelinePushConstantData;
use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::OnceLock;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

static DUMMY_TRANSFORM: OnceLock<TransformObjectData> = OnceLock::new();

thread_local! {
    static DUMMY_ANIMATION_PLAY_INFO: std::cell::UnsafeCell<AnimationPlayInfo> =
        std::cell::UnsafeCell::new(AnimationPlayInfo::default());
}

fn get_dummy_transform() -> &'static mut TransformObjectData {
    ptr_as_mut(DUMMY_TRANSFORM.get_or_init(TransformObjectData::create_transform_object_data))
}

fn get_dummy_animation_play_info() -> &'static mut AnimationPlayInfo {
    DUMMY_ANIMATION_PLAY_INFO.with(|cell| unsafe { &mut *cell.get() })
}

#[repr(i32)]
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Copy, EnumCountMacro, Default)]
pub enum AnimationLayer {
    #[default]
    BaseLayer,
    ActionLayer,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug, Copy, EnumCountMacro)]
pub enum SceneObjectType {
    Default,
    Collision,
    NoCollision,
    Terrain,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct RenderObjectCreateInfo {
    pub _scene_object_type: SceneObjectType,
    pub _model_data_name: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
}

impl Default for RenderObjectCreateInfo {
    fn default() -> RenderObjectCreateInfo {
        RenderObjectCreateInfo {
            _scene_object_type: SceneObjectType::Default,
            _model_data_name: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
#[serde(default)]
pub struct RenderObjectSaveData {
    pub _render_object_name: String,
    pub _animation_play_infos: Vec<AnimationPlayInfoSaveData>,
    pub _animation_buffer: Option<AnimationBuffer>,
}

#[derive(Clone, Debug)]
pub struct RenderObjectData<'a> {
    pub _scene_object_type: SceneObjectType,
    pub _object_id: SceneObjectID,
    pub _is_ready: bool,
    pub _is_visible: bool,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _is_render_height_map: bool,
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData<'a>>,
    pub _push_constant_data_list_group: Vec<Vec<PipelinePushConstantData>>,
    pub _collision: CollisionData,
    pub _entity_id: EntityId,
    pub _bounding_box: BoundingBox,
    pub _geometry_bound_boxes: Vec<BoundingBox>,
    pub _local_transform: Matrix4<f32>,
    pub _prev_transform: Matrix4<f32>,
    pub _final_transform: Matrix4<f32>,
    pub _instance_objects: RenderObjectMap<'a>,
    pub _instance_transforms: Vec<Matrix4<f32>>,
    pub _animation_play_infos: Vec<AnimationPlayInfo>,
    pub _pending_animations: [Option<Option<(RcRefCell<MeshData>, AnimationPlayArgs)>>; AnimationLayer::COUNT],
    pub _pending_animation_layers: [Option<*const AnimationLayerData>; AnimationLayer::COUNT],
    pub _pending_save_data: Option<RenderObjectSaveData>,
    pub _pending_push_constants: HashMap<String, PushConstantParameter>,
    pub _animation_buffer: Option<AnimationBuffer>,
    pub _bone_count: usize,
    pub _sockets: HashMap<String, RcRefCell<Socket>>,
    pub _material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>>,
}

impl<'a> RenderObjectData<'a> {
    pub fn create_render_object_data(
        object_id: SceneObjectID,
        render_object_name: &String,
        model_data: &RcRefCell<ModelData<'a>>,
        render_object_create_data: &RenderObjectCreateInfo,
        custom_collision_type: Option<CollisionType>,
    ) -> (RenderObjectData<'a>, TransformObjectData) {
        log::debug!("create_render_object_data: {}", render_object_name);
        let mut transform_object_data = TransformObjectData::create_transform_object_data();
        transform_object_data.set_position(&render_object_create_data._position);
        transform_object_data.set_rotation(&render_object_create_data._rotation);
        transform_object_data.set_scale(&render_object_create_data._scale);

        model_data.borrow_mut().update_model_data_resources();
        let model_data_ref = model_data.borrow();
        let material_instance_data_list: Vec<RcRefCell<MaterialInstanceData<'a>>> = model_data_ref
            ._material_instance_data_list
            .iter()
            .map(|material_instance_data| newRcRefCell(material_instance_data.borrow().clone()))
            .collect();
        let push_constant_data_list_group = material_instance_data_list
            .iter()
            .map(|material_instance_data| {
                material_instance_data.borrow().get_default_pipeline_binding_data()._push_constant_data_list.clone()
            })
            .collect();
        let mesh_data = model_data_ref._mesh_data.clone();
        let geometry_bound_boxes = mesh_data
            .borrow()
            ._geometry_data_list
            .iter()
            .map(|geometry_data| geometry_data.borrow()._geometry_bounding_box.clone())
            .collect();

        let mut sockets = HashMap::new();
        for (socket_name, socket_data) in model_data_ref._socket_data_map.iter() {
            let socket = newRcRefCell(Socket {
                _socket_data: socket_data.clone(),
                _transform: Matrix4::identity(),
            });
            sockets.insert(socket_name.clone(), socket);
        }

        let mut render_object_data = RenderObjectData {
            _entity_id: INVALID_ENTITY_ID,
            _scene_object_type: render_object_create_data._scene_object_type,
            _object_id: object_id,
            _is_ready: model_data_ref.is_ready(),
            _is_visible: render_object_create_data._scene_object_type != SceneObjectType::Collision,
            _is_render_camera: model_data_ref.is_render_camera(),
            _is_render_shadow: model_data_ref.is_render_shadow(),
            _is_render_height_map: render_object_create_data._scene_object_type == SceneObjectType::Terrain,
            _render_object_name: render_object_name.clone(),
            _model_data: model_data.clone(),
            _mesh_data: mesh_data.clone(),
            _collision: model_data_ref._collision.clone(),
            _bounding_box: mesh_data.borrow()._bound_box.clone(),
            _geometry_bound_boxes: geometry_bound_boxes,
            _local_transform: model_data_ref._local_transform.clone(),
            _prev_transform: Matrix4::identity(),
            _final_transform: Matrix4::identity(),
            _instance_objects: HashMap::new(),
            _instance_transforms: Vec::new(),
            _push_constant_data_list_group: push_constant_data_list_group,
            _animation_play_infos: Vec::new(),
            _pending_animations: [None, None],
            _pending_animation_layers: [None, None],
            _pending_save_data: None,
            _pending_push_constants: HashMap::new(),
            _animation_buffer: None,
            _bone_count: 0,
            _sockets: sockets,
            _material_instance_data_list: material_instance_data_list,
        };

        if let Some(collision_type) = custom_collision_type {
            render_object_data.set_collision_type(collision_type);
        }

        log::debug!("create_render_object_data: {}", render_object_name);
        (render_object_data, transform_object_data)
    }

    pub fn get_entity_id(&self) -> EntityId {
        self._entity_id
    }

    pub fn set_entity_id(&mut self, entity_id: EntityId) {
        self._entity_id = entity_id;
    }

    pub fn initialize_render_object_data(&mut self) {
        if self._mesh_data.borrow().has_animation_data() || self._mesh_data.borrow().has_skeleton() {
            self.initialize_animation_play_info();
        }

        self.update_render_object_data(0.0);

        // for valid initial velocity
        self._prev_transform = self._final_transform.clone();
    }

    pub fn update_render_object_data_resources(&mut self) {
        let model_data_ref = self._model_data.borrow();
        self._material_instance_data_list = model_data_ref
            ._material_instance_data_list
            .iter()
            .map(|material_instance_data| newRcRefCell(material_instance_data.borrow().clone()))
            .collect();
        self._push_constant_data_list_group = self
            ._material_instance_data_list
            .iter()
            .map(|material_instance_data| {
                material_instance_data.borrow().get_default_pipeline_binding_data()._push_constant_data_list.clone()
            })
            .collect();

        for instance_object in self._instance_objects.values_mut() {
            instance_object.borrow_mut().update_render_object_data_resources();
        }
    }

    pub fn copy_render_object_date(&mut self, src_render_object: &RenderObjectData<'a>) {
        self._is_ready = src_render_object._is_ready;
        self._is_visible = src_render_object._is_visible;
        self._is_render_camera = src_render_object._is_render_camera;
        self._is_render_shadow = src_render_object._is_render_shadow;
        self._is_render_height_map = src_render_object._is_render_height_map;
        self._render_object_name = src_render_object._render_object_name.clone();
        self._local_transform = src_render_object._local_transform;
        self.get_transform_object_data_mut().clone_from(src_render_object.get_transform_object_data());
        self._prev_transform = src_render_object._prev_transform;
        self._final_transform = src_render_object._final_transform;
        self._instance_objects = src_render_object._instance_objects.clone();
        self._instance_transforms = src_render_object._instance_transforms.clone();
        self._animation_play_infos = src_render_object._animation_play_infos.clone();
        self._pending_animations = src_render_object._pending_animations.clone();
        self._pending_animation_layers = src_render_object._pending_animation_layers;
        self._pending_save_data = src_render_object._pending_save_data.clone();
        self._pending_push_constants = src_render_object._pending_push_constants.clone();
        self._animation_buffer = src_render_object._animation_buffer.clone();
    }

    pub fn get_object_id(&self) -> SceneObjectID {
        self._object_id
    }

    pub fn set_object_id(&mut self, object_id: SceneObjectID) {
        self._object_id = object_id;
    }

    pub fn is_ready(&self) -> bool {
        self._is_ready
    }

    pub fn set_ready(&mut self, is_ready: bool) {
        self._is_ready = is_ready;
    }

    pub fn check_and_update_ready_state(&mut self) -> bool {
        if self._is_ready {
            return true;
        }
        self._model_data.borrow_mut().update_model_data_resources();
        if !self._model_data.borrow().is_ready() {
            return false;
        }

        self._is_ready = true;
        let loaded_mesh = self._model_data.borrow()._mesh_data.clone();
        let geometry_bound_boxes: Vec<BoundingBox> = loaded_mesh
            .borrow()
            ._geometry_data_list
            .iter()
            .map(|geometry_data| geometry_data.borrow()._geometry_bounding_box.clone())
            .collect();
        self._mesh_data = loaded_mesh;
        self._geometry_bound_boxes = geometry_bound_boxes;

        self.update_render_object_data_resources();
        let has_skeletal = self._mesh_data.borrow().has_skeleton() || self._mesh_data.borrow().has_animation_data();
        if has_skeletal {
            if self._bone_count == 0 || self._animation_buffer.is_none() || self._animation_play_infos.is_empty() {
                self.initialize_animation_play_info();
            } else {
                self.flush_pending_animations();
            }
        }
        if !self._pending_push_constants.is_empty() {
            let pending_push_constants = std::mem::take(&mut self._pending_push_constants);
            for (key, value) in pending_push_constants.iter() {
                self.set_push_constant_parameter(key, value);
            }
        }
        self.update_all_sockets();

        if self.is_instancing_render_object() {
            let mesh_bound_box = self._mesh_data.borrow()._bound_box.clone();
            let model_collision_box = self._model_data.borrow()._collision._bounding_box.clone();
            let has_valid_collision = self._collision.is_valid_collision();
            let mut is_first = true;
            let mut merged_min = Vector3::zeros();
            let mut merged_max = Vector3::zeros();
            let mut merged_collision_min = Vector3::zeros();
            let mut merged_collision_max = Vector3::zeros();
            self._instance_transforms.clear();
            for instance_object in self._instance_objects.values() {
                let mut child = instance_object.borrow_mut();
                child._is_ready = true;
                child._mesh_data = self._mesh_data.clone();
                child._geometry_bound_boxes = self._geometry_bound_boxes.clone();
                let child_matrix = {
                    let transform = child.get_transform_object_data_mut();
                    transform.update_transform_object();
                    *transform.get_matrix()
                };
                let child_final_transform = child_matrix * child._local_transform;
                child._final_transform = child_final_transform;
                child._prev_transform = child_final_transform;
                child._bounding_box.update_axis_aligned_bounding_box(&mesh_bound_box, &child_final_transform);
                if has_valid_collision {
                    child._collision._bounding_box.update_oriented_bounding_box(&model_collision_box, &child_matrix);
                }
                self._instance_transforms.push(child_final_transform);
                if is_first {
                    merged_min = child._bounding_box._min;
                    merged_max = child._bounding_box._max;
                    merged_collision_min = child._collision._bounding_box._min;
                    merged_collision_max = child._collision._bounding_box._max;
                    is_first = false;
                } else {
                    merged_min = math::get_min(&merged_min, &child._bounding_box._min);
                    merged_max = math::get_max(&merged_max, &child._bounding_box._max);
                    merged_collision_min = math::get_min(&merged_collision_min, &child._collision._bounding_box._min);
                    merged_collision_max = math::get_max(&merged_collision_max, &child._collision._bounding_box._max);
                }
            }
            if !is_first {
                self._bounding_box = BoundingBox::create_bounding_box(&merged_min, &merged_max);
                if has_valid_collision {
                    self._collision._bounding_box =
                        BoundingBox::create_bounding_box(&merged_collision_min, &merged_collision_max);
                }
                let center = self._bounding_box._center;
                let transform = self.get_transform_object_data_mut();
                transform.set_position(&center);
                transform.update_transform_object();
            }
        } else {
            let matrix = {
                let transform = self.get_transform_object_data_mut();
                transform.update_transform_object();
                *transform.get_matrix()
            };
            self._final_transform = matrix * self._local_transform;
            self._prev_transform = self._final_transform;
            let mesh_bound_box = self._mesh_data.borrow()._bound_box.clone();
            self._bounding_box.update_axis_aligned_bounding_box(&mesh_bound_box, &self._final_transform);

            if self._collision.is_valid_collision() {
                let model_collision_box = self._model_data.borrow()._collision._bounding_box.clone();
                self._collision._bounding_box.update_oriented_bounding_box(&model_collision_box, &matrix);
            }

            if self._mesh_data.borrow().has_animation_data() && self._animation_play_infos.is_empty() {
                self.initialize_animation_play_info();
            }

            self.update_all_sockets();
        }

        if crate::resource::resource::LOG_RESOURCE_LIFECYCLE {
            log::info!(
                "[Visibility Gating] RenderObject '{}' is now READY and VISIBLE",
                self._render_object_name
            );
        }
        true
    }

    pub fn is_visible(&self) -> bool {
        self._is_ready && self._is_visible
    }

    pub fn set_visible(&mut self, visible: bool) {
        self._is_visible = visible;
    }

    pub fn is_render_camera(&self) -> bool {
        self._is_render_camera
    }

    pub fn set_render_camera(&mut self, render: bool) {
        self._is_render_camera = render;
    }

    pub fn is_render_shadow(&self) -> bool {
        self._is_render_shadow
    }

    pub fn set_render_shadow(&mut self, render: bool) {
        self._is_render_shadow = render;
    }

    pub fn is_render_height_map(&self) -> bool {
        self._is_render_height_map
    }

    pub fn set_render_height_map(&mut self, render: bool) {
        self._is_render_height_map = render;
    }

    pub fn get_position(&self) -> &Vector3<f32> {
        &self.get_transform_object_data()._position
    }

    pub fn get_bounding_box(&self) -> &BoundingBox {
        &self._bounding_box
    }

    pub fn get_collision(&self) -> &CollisionData {
        &self._collision
    }

    pub fn get_collision_type(&self) -> CollisionType {
        self._collision._collision_type
    }

    pub fn set_collision_type(&mut self, collision_type: CollisionType) {
        self._collision._collision_type = collision_type;
    }

    pub fn debug_bone_names(&self) {
        log::info!("-------------------");
        let animation_data_list = &self._mesh_data.borrow()._animation_data_list;
        for bone in animation_data_list[0]._nodes.iter() {
            log::info!("bone: {:?}", bone._name);
        }
    }

    pub fn is_instancing_render_object(&self) -> bool {
        0 < self._instance_objects.len()
    }

    pub fn get_instancing_render_object_count(&self) -> usize {
        self._instance_objects.len()
    }

    pub fn get_instancing_render_object_transform(&self, index: usize) -> &Matrix4<f32> {
        &self._instance_transforms[index]
    }

    pub fn get_instancing_render_object_transforms(&self) -> &Vec<Matrix4<f32>> {
        &self._instance_transforms
    }

    pub fn register_instancing_render_object(&mut self, render_object_data: &RcRefCell<RenderObjectData<'a>>) {
        let render_object_data_ref = render_object_data.borrow();
        if self._instance_objects.is_empty() {
            self._bounding_box = render_object_data_ref._bounding_box.clone();
            self._collision._bounding_box = render_object_data_ref._collision._bounding_box.clone();
        } else {
            let bounding_box_min = math::get_min(&self._bounding_box._min, &render_object_data_ref._bounding_box._min);
            let bounding_box_max = math::get_max(&self._bounding_box._max, &render_object_data_ref._bounding_box._max);
            let collision_bounding_box_min = math::get_min(
                &self._collision._bounding_box._min,
                &render_object_data_ref._collision._bounding_box._min,
            );
            let collision_bounding_box_max = math::get_max(
                &self._collision._bounding_box._max,
                &render_object_data_ref._collision._bounding_box._max,
            );
            self._bounding_box = BoundingBox::create_bounding_box(&bounding_box_min, &bounding_box_max);
            self._collision._bounding_box =
                BoundingBox::create_bounding_box(&collision_bounding_box_min, &collision_bounding_box_max);
        }

        self.get_transform_object_data_mut().set_position(&self._bounding_box._center);
        self.get_transform_object_data_mut().update_transform_object();

        self._instance_objects.insert(render_object_data_ref.get_object_id(), render_object_data.clone());
        self._instance_transforms.push(render_object_data_ref._final_transform.clone());
    }

    pub fn flush_pending_animations(&mut self) {
        if self._bone_count == 0 || self._animation_play_infos.len() < AnimationLayer::COUNT {
            return;
        }

        for layer_idx in 0..AnimationLayer::COUNT {
            let layer: AnimationLayer = unsafe { std::mem::transmute(layer_idx as i32) };
            if let Some(layer_ptr) = self._pending_animation_layers[layer_idx].take() {
                self.set_animation_layers(layer_ptr, layer);
            }
            if let Some(pending_anim) = self._pending_animations[layer_idx].take() {
                match pending_anim {
                    Some((anim_mesh, anim_args)) => {
                        let anim_bone_count = anim_mesh.borrow().get_bone_count();
                        if anim_bone_count > 0 && anim_bone_count == self._bone_count {
                            self.set_animation(&anim_mesh, &anim_args, layer);
                        } else {
                            self._pending_animations[layer_idx] = Some(Some((anim_mesh, anim_args)));
                        }
                    }
                    None => {
                        self.set_animation_none(layer);
                    }
                }
            }
        }
    }

    pub fn initialize_animation_play_info(&mut self) {
        let bone_count = self._mesh_data.borrow().get_bone_count();
        if bone_count == 0 {
            return;
        }

        if self._bone_count != bone_count || self._animation_buffer.is_none() || self._animation_play_infos.is_empty() {
            self._animation_play_infos.clear();
            let base_layer_index = AnimationLayer::BaseLayer as usize;
            for i in 0..AnimationLayer::COUNT {
                let mesh_data = if base_layer_index == i {
                    Some(self._mesh_data.clone())
                } else {
                    None
                };
                self._animation_play_infos.push(AnimationPlayInfo::create_animation_play_info(
                    unsafe { std::mem::transmute(i as i32) },
                    mesh_data,
                    bone_count,
                ));
            }
            self._animation_buffer = Some(AnimationBuffer::create_animation_buffer(bone_count));
            self._bone_count = bone_count;

            if let Some(save_data) = self._pending_save_data.take() {
                for (info, save_info) in
                    self._animation_play_infos.iter_mut().zip(save_data._animation_play_infos.iter())
                {
                    info.load_animation_play_info_save_data(save_info);
                }
                if let Some(saved_buffer) = save_data._animation_buffer {
                    if saved_buffer._animation_buffer.len() == bone_count
                        && saved_buffer._prev_animation_buffer.len() == bone_count
                    {
                        self._animation_buffer = Some(saved_buffer);
                    }
                }
            }
        }

        self.flush_pending_animations();
    }

    pub fn update_all_sockets(&mut self) {
        if self._sockets.is_empty() {
            return;
        }

        let has_animation = self.has_animation();
        if has_animation && self._animation_buffer.is_some() {
            let base_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::BaseLayer));
            if base_animation.is_valid() {
                let animation_buffer = self._animation_buffer.as_mut().unwrap();
                animation_buffer.update_animation_buffer(base_animation, &mut self._sockets);
                for (_socket_name, socket) in self._sockets.iter() {
                    let mut socket_borrowed = socket.borrow_mut();
                    let local_transform = socket_borrowed._socket_data.borrow()._local_transform;
                    socket_borrowed._transform = self._final_transform * socket_borrowed._transform * local_transform;
                }
                return;
            }
        }

        let mesh_borrow = self._mesh_data.borrow();
        for socket in self._sockets.values() {
            let mut socket_borrowed = socket.borrow_mut();
            let parent_bone_index = socket_borrowed._socket_data.borrow()._parent_bone_index;
            let local_transform = socket_borrowed._socket_data.borrow()._local_transform;

            let bone_transform =
                if parent_bone_index != INVALID_BONE_INDEX && !mesh_borrow._skeleton_data_list.is_empty() {
                    let skeleton = &mesh_borrow._skeleton_data_list[0];
                    if parent_bone_index < skeleton._bone_names.len() {
                        skeleton._transform
                    } else {
                        Matrix4::identity()
                    }
                } else {
                    Matrix4::identity()
                };

            socket_borrowed._transform = self._final_transform * bone_transform * local_transform;
        }
    }

    pub fn notify_mesh_loaded(&mut self, mesh_name: &str, _loaded_mesh: &RcRefCell<MeshData>) {
        let is_my_mesh = self._model_data.borrow()._mesh_name == mesh_name;
        let mut has_matching_pending = false;
        for pending in self._pending_animations.iter().flatten().flatten() {
            if pending.0.borrow()._name == mesh_name {
                has_matching_pending = true;
                break;
            }
        }

        if is_my_mesh || has_matching_pending {
            self.check_and_update_ready_state();
            self.flush_pending_animations();
            self.update_all_sockets();
        }
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_model_data(&self) -> &RcRefCell<ModelData<'a>> {
        &self._model_data
    }

    pub fn get_material_instance_data_list(&self) -> &Vec<RcRefCell<MaterialInstanceData<'a>>> {
        &self._material_instance_data_list
    }

    pub fn get_material_instance_data_list_mut(&mut self) -> &mut Vec<RcRefCell<MaterialInstanceData<'a>>> {
        &mut self._material_instance_data_list
    }

    pub fn get_material_instance_data(&self, index: usize) -> &RcRefCell<MaterialInstanceData<'a>> {
        &self._material_instance_data_list[index]
    }

    pub fn get_push_constant_data_list(&self, model_index: usize) -> &Vec<PipelinePushConstantData> {
        &self._push_constant_data_list_group[model_index]
    }

    pub fn get_push_constant_data_list_mut(&mut self, model_index: usize) -> &mut Vec<PipelinePushConstantData> {
        &mut self._push_constant_data_list_group[model_index]
    }

    pub fn set_push_constant_parameter(&mut self, key: &str, value: &PushConstantParameter) {
        if !self._is_ready {
            self._pending_push_constants.insert(key.to_string(), value.clone());
        }
        for push_constant_data_list in self._push_constant_data_list_group.iter_mut() {
            for push_constant_data in push_constant_data_list.iter_mut() {
                push_constant_data._push_constant.set_push_constant_parameter(key, value);
            }
        }
    }

    #[inline(always)]
    pub fn get_transform_object_data(&self) -> &TransformObjectData {
        if self._entity_id == INVALID_ENTITY_ID || !crate::core::engine_service_locator::is_engine_core_valid() {
            return get_dummy_transform();
        }
        if let Some(transform) = get_scene_manager()._transform_storage.get(self._entity_id) {
            transform
        } else {
            get_dummy_transform()
        }
    }

    #[inline(always)]
    pub fn get_transform_object_data_mut(&self) -> &mut TransformObjectData {
        if self._entity_id == INVALID_ENTITY_ID || !crate::core::engine_service_locator::is_engine_core_valid() {
            return get_dummy_transform();
        }
        if let Some(transform) = ptr_as_mut(get_scene_manager())._transform_storage.get_mut(self._entity_id) {
            transform
        } else {
            get_dummy_transform()
        }
    }

    pub fn has_animation(&self) -> bool {
        self._animation_buffer.is_some() && self._animation_play_infos.len() >= AnimationLayer::COUNT
    }

    pub fn get_bone_count(&self) -> usize {
        self._bone_count
    }

    pub fn get_animation_play_info(&self, layer: AnimationLayer) -> &AnimationPlayInfo {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            &self._animation_play_infos[index]
        } else {
            get_dummy_animation_play_info()
        }
    }

    pub fn get_animation_play_info_mut(&mut self, layer: AnimationLayer) -> &mut AnimationPlayInfo {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            &mut self._animation_play_infos[index]
        } else {
            get_dummy_animation_play_info()
        }
    }

    pub fn get_animation_layers(&self, layer: AnimationLayer) -> *const AnimationLayerData {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            self._animation_play_infos[index]._animation_layers
        } else {
            self._pending_animation_layers[index].unwrap_or(std::ptr::null())
        }
    }

    pub fn set_animation_layers(&mut self, animation_layers: *const AnimationLayerData, layer: AnimationLayer) {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            self._animation_play_infos[index]._animation_layers = animation_layers;
        } else {
            self._pending_animation_layers[index] = Some(animation_layers);
        }
    }

    pub fn clear_animation_layers(&mut self, layer: AnimationLayer) {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            self._animation_play_infos[index]._animation_layers = std::ptr::null();
        } else {
            self._pending_animation_layers[index] = Some(std::ptr::null());
        }
    }

    pub fn set_animation_none(&mut self, layer: AnimationLayer) {
        let index = layer as usize;
        if index < self._animation_play_infos.len() {
            self._animation_play_infos[index]._animation_mesh = None;
        } else {
            self._pending_animations[index] = Some(None);
        }
    }

    pub fn set_animation(
        &mut self,
        animation_mesh: &RcRefCell<MeshData>,
        animation_args: &AnimationPlayArgs,
        layer: AnimationLayer,
    ) {
        let index = layer as usize;
        if index >= self._animation_play_infos.len() || self._bone_count == 0 {
            self._pending_animations[index] = Some(Some((animation_mesh.clone(), animation_args.clone())));
            return;
        }

        let anim_bone_count = animation_mesh.borrow().get_bone_count();
        if anim_bone_count == 0 || anim_bone_count != self._bone_count {
            self._pending_animations[index] = Some(Some((animation_mesh.clone(), animation_args.clone())));
            return;
        }

        let animation_play_info = &mut self.get_animation_play_info_mut(layer);
        let was_valid_animation = animation_play_info.is_valid();
        let prev_animation_mesh_ptr: *mut MeshData = (if was_valid_animation {
            animation_play_info._animation_mesh.as_ref().unwrap().as_ptr()
        } else {
            std::ptr::null()
        }) as *mut MeshData;
        if animation_args._force_animation_setting || animation_mesh.as_ptr() != prev_animation_mesh_ptr {
            animation_play_info.set_animation_play_info(animation_mesh, animation_args, was_valid_animation);
            if was_valid_animation && 0.0 < animation_play_info._animation_blend_time {
                animation_play_info._last_animation_transforms.clone_from(&animation_play_info._animation_transforms);
            }
        }
    }

    pub fn get_prev_animation_buffer(&self) -> &Vec<Matrix4<f32>> {
        &self._animation_buffer.as_ref().unwrap()._prev_animation_buffer
    }

    pub fn get_animation_buffer(&self) -> &Vec<Matrix4<f32>> {
        &self._animation_buffer.as_ref().unwrap()._animation_buffer
    }

    pub fn update_render_object_data(&mut self, delta_time: f32) {
        let became_ready = !self._is_ready && self.check_and_update_ready_state();
        let mut need_to_update_socket = false;

        self._prev_transform = self._final_transform.clone();

        let (matrix, is_updated) = {
            let transform = self.get_transform_object_data_mut();
            let is_updated = transform.update_transform_object();
            (*transform.get_matrix(), is_updated)
        };

        if is_updated {
            self._final_transform = matrix * self._local_transform;
            // update bound box
            self._bounding_box
                .update_axis_aligned_bounding_box(&self._mesh_data.borrow()._bound_box, &self._final_transform);

            // update collision
            if self._collision.is_valid_collision() {
                self._collision
                    ._bounding_box
                    .update_oriented_bounding_box(&self._model_data.borrow()._collision._bounding_box, &matrix);
            }

            for (_socket_name, socket) in self._sockets.iter() {
                let socket_cell: &RcRefCell<Socket> = socket;
                socket_cell.borrow_mut()._transform = Matrix4::identity();
            }
            need_to_update_socket = true;
        }

        if self.has_animation() {
            let mut updated = false;
            for animation_play_info in self._animation_play_infos.iter_mut() {
                if animation_play_info.is_valid() {
                    updated |= animation_play_info.update_animation_frame_time(delta_time);
                }
            }

            if true || updated {
                need_to_update_socket = true;

                for animation_play_info in self._animation_play_infos.iter_mut() {
                    if animation_play_info.is_valid() {
                        // update animation nodes
                        let animation_data_list: &Vec<AnimationData> =
                            &(*animation_play_info)._animation_mesh.as_ref().unwrap().borrow()._animation_data_list;
                        let animation_data = &animation_data_list[animation_play_info._animation_index];
                        for bone_node in animation_data._nodes.iter() {
                            let bone_index = ptr_as_ref(bone_node._bone)._index;
                            animation_play_info._animation_transforms[bone_index] =
                                bone_node.calc_animation_transform(animation_play_info._animation_frame);
                        }

                        // blend last animation
                        if animation_play_info._animation_blend_ratio < 1.0 {
                            for (bone_index, animation_transform) in
                                animation_play_info._animation_transforms.iter_mut().enumerate()
                            {
                                let last_animation_buffer = &animation_play_info._last_animation_transforms[bone_index];
                                *animation_transform = last_animation_buffer
                                    .lerp(animation_transform, animation_play_info._animation_blend_ratio);
                            }
                        }
                    }
                }

                // update additive animation
                let additive_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::ActionLayer));
                if additive_animation.is_valid() {
                    let base_animation = ptr_as_mut(self.get_animation_play_info(AnimationLayer::BaseLayer));
                    base_animation.combine_additive_animation(additive_animation);
                }

                // transform to matrix
                let base_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::BaseLayer));
                let animation_buffer = self._animation_buffer.as_mut().unwrap();
                animation_buffer.swap_animation_buffer();
                animation_buffer.update_animation_buffer(base_animation, &mut self._sockets);
                if became_ready {
                    animation_buffer._prev_animation_buffer.clone_from(&animation_buffer._animation_buffer);
                }
            }
        }

        // update sockets
        if need_to_update_socket {
            for (_socket_name, socket) in self._sockets.iter() {
                let socket_cell: &RcRefCell<Socket> = socket;
                let mut socket_borrowed = socket_cell.borrow_mut();
                let local_transform = socket_borrowed._socket_data.borrow()._local_transform;
                socket_borrowed._transform = self._final_transform * socket_borrowed._transform * local_transform;
            }
        }
    }

    pub fn get_render_object_save_data(&self) -> RenderObjectSaveData {
        if let Some(ref pending) = self._pending_save_data {
            let mut cloned = pending.clone();
            cloned._render_object_name = self._render_object_name.clone();
            return cloned;
        }
        RenderObjectSaveData {
            _render_object_name: self._render_object_name.clone(),
            _animation_play_infos: self
                ._animation_play_infos
                .iter()
                .map(|info| info.get_animation_play_info_save_data())
                .collect(),
            _animation_buffer: self._animation_buffer.clone(),
        }
    }

    pub fn load_render_object_save_data(&mut self, save_data: &RenderObjectSaveData) {
        self._render_object_name.clone_from(&save_data._render_object_name);
        if self._animation_play_infos.len() < AnimationLayer::COUNT {
            self._pending_save_data = Some(save_data.clone());
            return;
        }
        for (info, save_info) in self._animation_play_infos.iter_mut().zip(save_data._animation_play_infos.iter()) {
            info.load_animation_play_info_save_data(save_info);
        }
        if let Some(ref saved_buffer) = save_data._animation_buffer {
            if saved_buffer._animation_buffer.len() == self._bone_count
                && saved_buffer._prev_animation_buffer.len() == self._bone_count
            {
                self._animation_buffer.clone_from(&save_data._animation_buffer);
            }
        }
    }
}
