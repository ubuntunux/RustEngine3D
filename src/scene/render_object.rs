use std::collections::HashMap;
use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use strum::EnumCount;
use strum_macros::EnumCount;
use crate::scene::animation::{AnimationLayerData, AnimationBuffer, AnimationData, AnimationPlayArgs, AnimationPlayInfo};
use crate::scene::mesh::MeshData;
use crate::scene::model::ModelData;
use crate::scene::transform_object::TransformObjectData;
use crate::scene::bounding_box::BoundingBox;
use crate::scene::collision::{CollisionData, CollisionType};
use crate::scene::scene_manager::SceneObjectID;
use crate::scene::socket::Socket;
use crate::utilities::system::{ptr_as_ref, ptr_as_mut, RcRefCell, newRcRefCell};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

#[repr(i32)]
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy, EnumCount)]
pub enum AnimationLayer {
    BaseLayer,
    ActionLayer
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(default)]
pub struct RenderObjectCreateInfo {
    pub _model_data_name: String,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
}

impl Default for RenderObjectCreateInfo {
    fn default() -> RenderObjectCreateInfo {
        RenderObjectCreateInfo {
            _model_data_name: String::new(),
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RenderObjectData<'a> {
    pub _object_id: SceneObjectID,
    pub _is_visible: bool,
    pub _is_render_camera: bool,
    pub _is_render_shadow: bool,
    pub _is_render_height_map: bool,
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData<'a>>,
    pub _push_constant_data_list_group: Vec<Vec<PipelinePushConstantData>>,
    pub _collision: CollisionData,
    pub _bounding_box: BoundingBox,
    pub _geometry_bound_boxes: Vec<BoundingBox>,
    pub _local_transform: Matrix4<f32>,
    pub _transform_object: TransformObjectData,
    pub _prev_transform: Matrix4<f32>,
    pub _final_transform: Matrix4<f32>,
    pub _animation_play_infos: Vec<AnimationPlayInfo>,
    pub _animation_buffer: Option<AnimationBuffer>,
    pub _bone_count: usize,
    pub _sockets: HashMap<String, RcRefCell<Socket>>
}

impl<'a> RenderObjectData<'a> {
    pub fn create_render_object_data(
        object_id: SceneObjectID,
        render_object_name: &String,
        model_data: &RcRefCell<ModelData<'a>>,
        render_object_create_data: &RenderObjectCreateInfo,
    ) -> RenderObjectData<'a> {
        log::debug!("create_render_object_data: {}", render_object_name);
        let mut transform_object_data = TransformObjectData::create_transform_object_data();
        transform_object_data.set_position(&render_object_create_data._position);
        transform_object_data.set_rotation(&render_object_create_data._rotation);
        transform_object_data.set_scale(&render_object_create_data._scale);

        let model_data_ref = model_data.borrow();
        let push_constant_data_list_group = model_data_ref
            ._material_instance_data_list
            .iter()
            .map(|material_instance_data| {
                material_instance_data
                    .borrow()
                    .get_default_pipeline_binding_data()
                    ._push_constant_data_list
                    .clone()
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
            _object_id: object_id,
            _is_visible: true,
            _is_render_camera: model_data_ref.is_render_camera(),
            _is_render_shadow: model_data_ref.is_render_shadow(),
            _is_render_height_map: false,
            _render_object_name: render_object_name.clone(),
            _model_data: model_data.clone(),
            _mesh_data: mesh_data.clone(),
            _collision: model_data_ref._collision.clone(),
            _bounding_box: mesh_data.borrow()._bound_box.clone(),
            _geometry_bound_boxes: geometry_bound_boxes,
            _local_transform: model_data_ref._local_transform.clone(),
            _transform_object: transform_object_data,
            _prev_transform: Matrix4::identity(),
            _final_transform: Matrix4::identity(),
            _push_constant_data_list_group: push_constant_data_list_group,
            _animation_play_infos: Vec::new(),
            _animation_buffer: None,
            _bone_count: 0,
            _sockets: sockets
        };

        log::debug!("create_render_object_data: {}", render_object_name);

        render_object_data.initialize_render_object_data();

        render_object_data
    }

    pub fn initialize_render_object_data(&mut self) {
        if  self._mesh_data.borrow().has_animation_data() {
            self.initialize_animation_play_info();
        }

        self.update_render_object_data(0.0);

        // for valid initial velocity
        self._prev_transform = self._final_transform.clone();
    }

    pub fn get_object_id(&self) -> SceneObjectID {
        self._object_id
    }

    pub fn set_object_id(&mut self, object_id: SceneObjectID) {
        self._object_id = object_id;
    }

    pub fn is_visible(&self) -> bool {
        self._is_visible
    }

    pub fn set_is_visible(&mut self, visible: bool) {
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

    pub fn initialize_animation_play_info(&mut self) {
        let animation_data_list = &self._mesh_data.borrow()._animation_data_list;
        assert!(!animation_data_list.is_empty());
        let first_animation = animation_data_list.first();
        assert!(!first_animation.is_none());
        let bone_count = first_animation.unwrap().get_bone_count();

        assert!(0 < bone_count);
        let base_layer_index = AnimationLayer::BaseLayer as usize;
        for i in 0..AnimationLayer::COUNT {
            let mesh_data = if base_layer_index == i { Some(self._mesh_data.clone()) } else { None };
            self._animation_play_infos.push(
                AnimationPlayInfo::create_animation_play_info(mesh_data, bone_count)
            );
        }
        self._animation_buffer = Some(AnimationBuffer::create_animation_buffer(bone_count));
        self._bone_count = bone_count;
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_model_data(&self) -> &RcRefCell<ModelData<'a>> {
        &self._model_data
    }

    pub fn get_push_constant_data_list(
        &self,
        model_index: usize,
    ) -> &Vec<PipelinePushConstantData> {
        &self._push_constant_data_list_group[model_index]
    }

    pub fn get_push_constant_data_list_mut(
        &mut self,
        model_index: usize,
    ) -> &mut Vec<PipelinePushConstantData> {
        &mut self._push_constant_data_list_group[model_index]
    }

    pub fn get_transform_object_data(&self) -> &TransformObjectData {
        &self._transform_object
    }

    pub fn has_animation(&self) -> bool {
        self._animation_buffer.is_some()
    }

    pub fn get_bone_count(&self) -> usize {
        self._bone_count
    }

    pub fn get_animation_play_info(&self, layer: AnimationLayer) -> &AnimationPlayInfo {
        &self._animation_play_infos[layer as usize]
    }

    pub fn get_animation_play_info_mut(&mut self, layer: AnimationLayer) -> &mut AnimationPlayInfo {
        &mut self._animation_play_infos[layer as usize]
    }

    pub fn get_animation_layers(&self, layer: AnimationLayer) -> *const AnimationLayerData {
        self.get_animation_play_info(layer)._animation_layers
    }

    pub fn set_animation_layers(&mut self, animation_layers: *const AnimationLayerData, layer: AnimationLayer) {
        self.get_animation_play_info_mut(layer)._animation_layers = animation_layers;
    }

    pub fn clear_animation_layers(&mut self, layer: AnimationLayer) {
        self.get_animation_play_info_mut(layer)._animation_layers = std::ptr::null();
    }

    pub fn set_animation_none(&mut self, layer: AnimationLayer) {
        let animation_play_info = &mut self.get_animation_play_info_mut(layer);
        animation_play_info._animation_mesh = None;
    }

    pub fn set_animation(&mut self, animation_mesh: &RcRefCell<MeshData>, animation_args: &AnimationPlayArgs, layer: AnimationLayer) {
        if let Some(first_animation_data) = animation_mesh.borrow()._animation_data_list.first() {
            if self._bone_count == first_animation_data.get_bone_count() {
                let animation_play_info = &mut self.get_animation_play_info_mut(layer);
                let was_valid_animation = animation_play_info.is_valid();
                let prev_animation_mesh_ptr: *mut MeshData =
                    (if was_valid_animation { animation_play_info._animation_mesh.as_ref().unwrap().as_ptr() } else { std::ptr::null() }) as *mut MeshData;
                if animation_args._force_animation_setting || animation_mesh.as_ptr() != prev_animation_mesh_ptr {
                    animation_play_info.set_animation_play_info(animation_mesh, animation_args, was_valid_animation);
                    if was_valid_animation && 0.0 < animation_play_info._animation_blend_time {
                        animation_play_info._last_animation_transforms.clone_from(&animation_play_info._animation_transforms);
                    }
                }
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
        self._prev_transform = self._final_transform.clone();
        if self._transform_object.update_transform_object() {
            self._final_transform = ptr_as_ref(self._transform_object.get_matrix()) * self._local_transform;
            // update bound box
            self._bounding_box.update_aixs_aligned_bounding_box(
                &self._mesh_data.borrow()._bound_box,
                &self._final_transform
            );

            // update collision
            if self._collision.is_valid_collision() {
                self._collision._bounding_box.update_oriented_bouding_box(
                    &self._model_data.borrow()._collision._bounding_box,
                    self._transform_object.get_matrix()
                )
            }
        }

        if self.has_animation() {
            let mut updated = false;
            for animation_play_info in self._animation_play_infos.iter_mut() {
                if animation_play_info.is_valid() {
                    updated |= animation_play_info.update_animation_frame_time(delta_time);
                }
            }

            if true || updated {
                for animation_play_info in self._animation_play_infos.iter_mut() {
                    if animation_play_info.is_valid() {
                        // update animation nodes
                        let animation_data_list: &Vec<AnimationData> = &(*animation_play_info)._animation_mesh.as_ref().unwrap().borrow()._animation_data_list;
                        let animation_data = &animation_data_list[animation_play_info._animation_index];
                        for bone_node in animation_data._nodes.iter() {
                            let bone_index = ptr_as_ref(bone_node._bone)._index;
                            animation_play_info._animation_transforms[bone_index] = bone_node.calc_animation_transform(animation_play_info._animation_frame);
                        }

                        // blend last animation
                        if animation_play_info._animation_blend_ratio < 1.0 {
                            for (bone_index, animation_transform) in animation_play_info._animation_transforms.iter_mut().enumerate() {
                                let last_animation_buffer = &animation_play_info._last_animation_transforms[bone_index];
                                *animation_transform = last_animation_buffer.lerp(animation_transform, animation_play_info._animation_blend_ratio);
                            }
                        }
                    }
                }

                // update additive animation
                let additive_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::ActionLayer));
                if additive_animation.is_valid() /* && false == additive_animation._is_animation_end */ {
                    let base_animation = ptr_as_mut(self.get_animation_play_info(AnimationLayer::BaseLayer));
                    base_animation.combine_additive_animation(additive_animation);
                }

                // transform to matrix
                let base_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::BaseLayer));
                let animation_buffer = self._animation_buffer.as_mut().unwrap();
                animation_buffer.swap_animation_buffer();
                animation_buffer.update_animation_buffer(base_animation, &mut self._sockets);

                // update sockets
                for (_socket_name, socket) in self._sockets.iter_mut() {
                    let mut socket_borrowed = socket.borrow_mut();
                    let parent_bon_index = socket_borrowed._socket_data.borrow()._parent_bone_index;
                    if parent_bon_index < self._bone_count {
                        let local_transform = socket_borrowed._socket_data.borrow()._local_transform;
                        socket_borrowed._transform = self._final_transform * socket_borrowed._transform * local_transform;
                    }
                }
            }
        }
    }
}
