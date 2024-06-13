use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};
use crate::scene::animation::{AnimationLayerData, AnimationBuffer, AnimationData, AnimationPlayArgs, AnimationPlayInfo};
use crate::scene::mesh::MeshData;
use crate::scene::model::ModelData;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{ptr_as_ref, ptr_as_mut, RcRefCell};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

#[repr(i32)]
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum AnimationLayer {
    BaseLayer,
    ActionLayer,
    LayerCount
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
    pub _object_id: i64,
    pub _render: bool,
    pub _render_shadow: bool,
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData<'a>>,
    pub _push_constant_data_list_group: Vec<Vec<PipelinePushConstantData>>,
    pub _bound_box: BoundingBox,
    pub _geometry_bound_boxes: Vec<BoundingBox>,
    pub _transform_object: TransformObjectData,
    pub _animation_play_infos: Vec<AnimationPlayInfo>,
    pub _animation_buffer: Option<AnimationBuffer>,
    pub _bone_count: usize
}

impl<'a> RenderObjectData<'a> {
    pub fn create_render_object_data(
        object_id: i64,
        render_object_name: &String,
        model_data: &RcRefCell<ModelData<'a>>,
        render_object_create_data: &RenderObjectCreateInfo,
    ) -> RenderObjectData<'a> {
        log::debug!("create_render_object_data: {}", render_object_name);
        let mut transform_object_data = TransformObjectData::create_transform_object_data();
        transform_object_data.set_position(&render_object_create_data._position);
        transform_object_data.set_rotation(&render_object_create_data._rotation);
        transform_object_data.set_scale(&render_object_create_data._scale);

        let push_constant_data_list_group = model_data
            .borrow()
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

        let mesh_data = model_data.borrow()._mesh_data.clone();
        let bound_box = mesh_data.borrow()._bound_box.clone();
        let geometry_bound_boxes = mesh_data
            .borrow()
            ._geometry_data_list
            .iter()
            .map(|geometry_data| geometry_data.borrow()._geometry_bounding_box.clone())
            .collect();

        let mut render_object_data = RenderObjectData {
            _object_id: object_id,
            _render: true,
            _render_shadow: true,
            _render_object_name: render_object_name.clone(),
            _model_data: model_data.clone(),
            _mesh_data: mesh_data.clone(),
            _bound_box: bound_box,
            _geometry_bound_boxes: geometry_bound_boxes,
            _transform_object: transform_object_data,
            _push_constant_data_list_group: push_constant_data_list_group,
            _animation_play_infos: Vec::new(),
            _animation_buffer: None,
            _bone_count: 0
        };

        if  mesh_data.borrow().has_animation_data() {
            render_object_data.initialize_animation_play_info();
        }
        render_object_data.update_render_object_data(0.0);
        render_object_data
    }

    pub fn set_render(&mut self, render: bool) {
        self._render = render;
    }

    pub fn set_render_shadow(&mut self, render: bool) {
        self._render_shadow = render;
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
        assert!(false == animation_data_list.is_empty());
        let first_animation = animation_data_list.first();
        assert!(false == first_animation.is_none());
        let bone_count = first_animation.unwrap().get_bone_count();

        assert!(0 < bone_count);
        let base_layer_index = AnimationLayer::BaseLayer as i32;
        for i in 0..(AnimationLayer::LayerCount as i32) {
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
        if self._bone_count == animation_mesh.borrow()._animation_data_list.first().unwrap().get_bone_count() {
            let animation_play_info = &mut self.get_animation_play_info_mut(layer);
            let was_valid_animation = animation_play_info.is_valid();
            let prev_animation_mesh_ptr: *mut MeshData =
                (if was_valid_animation { animation_play_info._animation_mesh.as_ref().unwrap().as_ptr() } else { std::ptr::null() }) as *mut MeshData;
            if animation_args._force_animation_setting || animation_mesh.as_ptr() != prev_animation_mesh_ptr {
                animation_play_info.set_animation_play_info(animation_args, was_valid_animation);
                animation_play_info._animation_mesh = Some(animation_mesh.clone());
                if was_valid_animation && 0.0 < animation_play_info._animation_blend_time {
                    animation_play_info._last_animation_transforms.clone_from(&animation_play_info._animation_transforms);
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

    pub fn update_bound_box(&mut self, transform_matrix: &Matrix4<f32>) {
        self._bound_box.update_with_matrix(&self._mesh_data.borrow()._bound_box, transform_matrix);
    }

    pub fn update_geometry_bound_boxes(&mut self, transform_matrix: &Matrix4<f32>) {
        for (i, geometry_data) in self
            ._mesh_data
            .borrow()
            ._geometry_data_list
            .iter()
            .enumerate() {
            self._geometry_bound_boxes
                .get_mut(i)
                .unwrap()
                .update_with_matrix(
                    &geometry_data.borrow()._geometry_bounding_box,
                    transform_matrix
                );
        }
    }

    pub fn update_render_object_data(&mut self, delta_time: f32) {
        let updated_transform = self._transform_object.update_transform_object();
        if updated_transform {
            let transform_matrix = ptr_as_ref(self._transform_object.get_matrix());
            const UPDATE_BOUND_BOX_WITH_ANIMATION: bool = false;
            if UPDATE_BOUND_BOX_WITH_ANIMATION && self.has_animation() {
                let root_bone_index = 0;
                self.update_bound_box(&(transform_matrix * self._animation_buffer.as_ref().unwrap()._animation_buffer[root_bone_index]));
            } else {
                self.update_bound_box(transform_matrix);
            }
        }

        if self.has_animation() {
            let mut updated = false;
            for animation_play_info in self._animation_play_infos.iter_mut() {
                if animation_play_info.is_valid() {
                    updated |= animation_play_info.update_animation_frame_time(delta_time);
                }
            }

            if updated {
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
                {
                    let additive_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::ActionLayer));
                    if additive_animation.is_valid() && false == additive_animation._is_animation_end {
                        let base_animation = ptr_as_mut(self.get_animation_play_info(AnimationLayer::BaseLayer));
                        base_animation.combine_additive_animation(additive_animation);
                    }
                }

                // transform to matrix
                let base_animation = ptr_as_ref(self.get_animation_play_info(AnimationLayer::BaseLayer));
                let animation_buffer = self._animation_buffer.as_mut().unwrap();
                animation_buffer.swap_animation_buffer();
                animation_buffer.update_animation_buffer(base_animation);
            }
        }
    }
}
