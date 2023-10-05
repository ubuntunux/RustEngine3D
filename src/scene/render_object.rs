use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};

use crate::scene::animation::{AnimationData, AnimationPlayArgs, AnimationPlayInfo};
use crate::scene::mesh::MeshData;
use crate::scene::model::ModelData;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{ptr_as_ref, RcRefCell};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

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
pub struct RenderObjectData {
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData>,
    pub _push_constant_data_list_group: Vec<Vec<PipelinePushConstantData>>,
    pub _bound_box: BoundingBox,
    pub _geometry_bound_boxes: Vec<BoundingBox>,
    pub _transform_object: TransformObjectData,
    pub _animation_play_info: Option<AnimationPlayInfo>,
    pub _bone_count: usize,
}

impl RenderObjectData {
    pub fn create_render_object_data(
        render_object_name: &String,
        model_data: &RcRefCell<ModelData>,
        render_object_create_data: &RenderObjectCreateInfo,
    ) -> RenderObjectData {
        log::debug!("create_render_object_data: {}", render_object_name);
        let mut transform_object_data = TransformObjectData::new_transform_object_data();
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
        let has_animation_data = mesh_data.borrow().has_animation_data();
        let geometry_bound_boxes = mesh_data
            .borrow()
            ._geometry_data_list
            .iter()
            .map(|geometry_data| geometry_data.borrow()._geometry_bounding_box.clone())
            .collect();
        let mut render_object_data = RenderObjectData {
            _render_object_name: render_object_name.clone(),
            _model_data: model_data.clone(),
            _mesh_data: mesh_data,
            _bound_box: bound_box,
            _geometry_bound_boxes: geometry_bound_boxes,
            _transform_object: transform_object_data,
            _push_constant_data_list_group: push_constant_data_list_group,
            _animation_play_info: None,
            _bone_count: 0,
        };

        render_object_data.initialize_animation_play_info(has_animation_data);
        render_object_data
    }

    pub fn initialize_animation_play_info(&mut self, has_animation_data: bool) {
        if has_animation_data {
            let mut bone_count = 0usize;
            let mut animation_play_info = AnimationPlayInfo::default();
            for animation in self._mesh_data.borrow_mut()._animation_data_list.iter_mut() {
                assert!(0 == bone_count || bone_count == animation.get_bone_count());
                bone_count = animation.get_bone_count();
                let mut animation_buffers: Vec<Matrix4<f32>> =
                    vec![Matrix4::identity(); bone_count];
                animation.update_animation_transforms(0.0, &mut animation_buffers);
                animation_play_info
                    ._animation_buffers
                    .push(animation_buffers.clone());
                animation_play_info
                    ._prev_animation_buffers
                    .push(animation_buffers.clone());
                animation_play_info
                    ._blend_animation_buffers
                    .push(animation_buffers.clone())
            }
            animation_play_info._animation_mesh = Some(self._mesh_data.clone());

            self._animation_play_info = Some(animation_play_info);
            self._bone_count = bone_count;
        }
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_model_data(&self) -> &RcRefCell<ModelData> {
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

    pub fn has_animation_play_info(&self) -> bool {
        self._animation_play_info.is_some()
    }

    pub fn get_bone_count(&self) -> usize {
        self._bone_count
    }

    pub fn set_animation(
        &mut self,
        animation_mesh: &RcRefCell<MeshData>,
        animation_args: &AnimationPlayArgs,
    ) {
        let animation_play_info = &mut self._animation_play_info.as_mut().unwrap();
        // if animation_args._force || animation_mesh.as_ptr() != animation_play_info._animation_mesh.as_ref().unwrap().as_ptr()
        {
            animation_play_info._animation_mesh = Some(animation_mesh.clone());
            animation_play_info.set_animation_play_info(animation_args);
            std::mem::swap(
                &mut animation_play_info._prev_animation_buffers,
                &mut animation_play_info._animation_buffers,
            );
        }
    }

    pub fn get_prev_animation_buffer(&self, index: usize) -> &Vec<Matrix4<f32>> {
        &self
            ._animation_play_info
            .as_ref()
            .unwrap()
            ._prev_animation_buffers[index]
    }

    pub fn get_animation_buffer(&self, index: usize) -> &Vec<Matrix4<f32>> {
        &self
            ._animation_play_info
            .as_ref()
            .unwrap()
            ._animation_buffers[index]
    }

    pub fn update_bound_box(&mut self, transform_matrix: &Matrix4<f32>) {
        self._bound_box
            .update_with_matrix(&self._mesh_data.borrow()._bound_box, transform_matrix);
    }

    pub fn update_geometry_bound_boxes(&mut self, transform_matrix: &Matrix4<f32>) {
        for (i, geometry_data) in self
            ._mesh_data
            .borrow()
            ._geometry_data_list
            .iter()
            .enumerate()
        {
            self._geometry_bound_boxes
                .get_mut(i)
                .unwrap()
                .update_with_matrix(
                    &geometry_data.borrow()._geometry_bounding_box,
                    transform_matrix,
                );
        }
    }

    pub fn update_render_object_data(&mut self, delta_time: f32) {
        let updated_transform = self._transform_object.update_transform_object();
        if updated_transform {
            let transform_matrix = ptr_as_ref(self._transform_object.get_matrix());
            if self.has_animation_play_info() {
                let animation_play_info = ptr_as_ref(self._animation_play_info.as_ref().unwrap());
                self.update_bound_box(
                    &(transform_matrix * animation_play_info._animation_buffers[0][0]),
                );
            } else {
                self.update_bound_box(transform_matrix);
            }
        }

        // update animation
        if self.has_animation_play_info() {
            let animation_play_info = &mut self._animation_play_info.as_mut().unwrap();
            let mut blend_ratio: f32 = 1.0;
            let animation_data_list: &Vec<AnimationData> = &animation_play_info
                ._animation_mesh
                .as_ref()
                .unwrap()
                .borrow()
                ._animation_data_list;
            for (i, animation) in animation_data_list.iter().enumerate() {
                // update animation frame only first animation
                if 0 == i {
                    if 1 < animation._frame_count {
                        animation_play_info._animation_play_time +=
                            animation_play_info._animation_speed * delta_time;

                        let mut animation_end_time = animation._animation_length;
                        if let Some(custom_end_time) = animation_play_info._animation_end_time {
                            if custom_end_time < animation_end_time {
                                animation_end_time = custom_end_time;
                            }
                        }

                        if animation_play_info._animation_loop {
                            if animation_end_time < animation_play_info._animation_play_time {
                                animation_play_info._animation_play_time =
                                    animation_play_info._animation_play_time % animation_end_time;
                            }
                        } else {
                            if animation_end_time <= animation_play_info._animation_play_time {
                                animation_play_info._animation_play_time = animation_end_time;
                                animation_play_info._is_animation_end = true;
                            }
                        }
                        animation_play_info._animation_frame = animation.get_time_to_frame(
                            animation_play_info._animation_frame,
                            animation_play_info._animation_play_time,
                        );
                    } else {
                        animation_play_info._animation_frame = 0.0;
                    }

                    if animation_play_info._animation_elapsed_time
                        < animation_play_info._animation_blend_time
                    {
                        blend_ratio = animation_play_info._animation_elapsed_time
                            / animation_play_info._animation_blend_time;
                    }
                    animation_play_info._animation_elapsed_time += delta_time;
                }

                // swap
                std::mem::swap(
                    &mut animation_play_info._prev_animation_buffers[i],
                    &mut animation_play_info._animation_buffers[i],
                );

                // update animation buffers
                if animation_play_info._last_animation_frame != animation_play_info._animation_frame
                {
                    animation_play_info._last_animation_frame =
                        animation_play_info._animation_frame;
                    animation.update_animation_transforms(
                        animation_play_info._animation_frame,
                        &mut animation_play_info._animation_buffers[i],
                    );

                    if blend_ratio < 1.0 {
                        for (buffer_index, animation_buffer) in animation_play_info
                            ._animation_buffers[i]
                            .iter_mut()
                            .enumerate()
                        {
                            let blend_animation_buffer =
                                &animation_play_info._blend_animation_buffers[i][buffer_index];
                            animation_buffer.copy_from(
                                &((blend_animation_buffer * (1.0 - blend_ratio))
                                    + (&(*animation_buffer) * blend_ratio)),
                            );
                        }
                    }
                }
            }
        }
    }
}
