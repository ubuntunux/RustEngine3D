use nalgebra::{Matrix4, Vector3};
use serde::{Deserialize, Serialize};

use crate::scene::animation::{AnimationPlayArgs, AnimationPlayInfo};
use crate::scene::mesh::MeshData;
use crate::scene::model::ModelData;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::bounding_box::BoundingBox;
use crate::utilities::system::{ptr_as_mut, ptr_as_ref, RcRefCell};
use crate::vulkan_context::render_pass::PipelinePushConstantData;

#[repr(i32)]
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum AnimationLayer {
    BaseLayer,
    AdditiveLayer,
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
pub struct RenderObjectData {
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData>,
    pub _push_constant_data_list_group: Vec<Vec<PipelinePushConstantData>>,
    pub _bound_box: BoundingBox,
    pub _geometry_bound_boxes: Vec<BoundingBox>,
    pub _transform_object: TransformObjectData,
    pub _animation_play_infos: Vec<AnimationPlayInfo>,
    pub _bone_count: usize,
}

impl RenderObjectData {
    pub fn create_render_object_data(
        render_object_name: &String,
        model_data: &RcRefCell<ModelData>,
        render_object_create_data: &RenderObjectCreateInfo,
    ) -> RenderObjectData {
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
            _render_object_name: render_object_name.clone(),
            _model_data: model_data.clone(),
            _mesh_data: mesh_data.clone(),
            _bound_box: bound_box,
            _geometry_bound_boxes: geometry_bound_boxes,
            _transform_object: transform_object_data,
            _push_constant_data_list_group: push_constant_data_list_group,
            _animation_play_infos: Vec::new(),
            _bone_count: 0
        };

        if  mesh_data.borrow().has_animation_data() {
            render_object_data.initialize_animation_play_info();
        }
        render_object_data
    }

    pub fn initialize_animation_play_info(&mut self) {
        let animation_data_list = &self._mesh_data.borrow()._animation_data_list;
        assert!(false == animation_data_list.is_empty());
        let first_animation = animation_data_list.first();
        assert!(false == first_animation.is_none());
        let bone_count = animation_data_list[0].get_bone_count();

        // display bone names
        // log::info!("-------------------");
        // for bone in animation_data_list[0]._nodes.iter() {
        //     log::info!("bone: {:?}", bone._name);
        // }

        assert!(0 < bone_count);
        for _i in 0..(AnimationLayer::LayerCount as i32) {
            self._animation_play_infos.push(AnimationPlayInfo {
                _animation_buffer: vec![Matrix4::identity(); bone_count],
                _prev_animation_buffer: vec![Matrix4::identity(); bone_count],
                _blend_animation_buffer: vec![Matrix4::identity(); bone_count],
                _animation_mesh: Some(self._mesh_data.clone()),
                ..Default::default()
            });
        }

        self._bone_count = bone_count;
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
        !self._animation_play_infos.is_empty()
    }

    pub fn get_bone_count(&self) -> usize {
        self._bone_count
    }

    pub fn set_animation(&mut self, animation_mesh: &RcRefCell<MeshData>, animation_args: &AnimationPlayArgs, layer: AnimationLayer) {
        let animation_play_info = &mut self._animation_play_infos[layer as usize];
        if animation_args._force_animation_setting || animation_mesh.as_ptr() != animation_play_info._animation_mesh.as_ref().unwrap().as_ptr() {
            animation_play_info._animation_mesh = Some(animation_mesh.clone());
            animation_play_info.set_animation_play_info(animation_args);
            if 0.0 < animation_play_info._animation_blend_time {
                animation_play_info._blend_animation_buffer.clone_from(&animation_play_info._animation_buffer);
            }
        }
    }

    pub fn get_prev_animation_buffer(&self) -> &Vec<Matrix4<f32>> {
        &self
            ._animation_play_infos.first()
            .unwrap()
            ._prev_animation_buffer
    }

    pub fn get_animation_buffer(&self) -> &Vec<Matrix4<f32>> {
        &self
            ._animation_play_infos.first()
            .unwrap()
            ._animation_buffer
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
            .enumerate() {
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
                let base_animation = ptr_as_ref(&self._animation_play_infos[AnimationLayer::BaseLayer as usize]);
                let root_bone_index = 0;
                self.update_bound_box(&(transform_matrix * base_animation._animation_buffer[root_bone_index]));
            } else {
                self.update_bound_box(transform_matrix);
            }
        }

        if self.has_animation_play_info() {
            for animation_play_info in self._animation_play_infos.iter_mut() {
                animation_play_info.update_animation_play_info(delta_time);
            }

            // update additive animation
            let additive_animation = ptr_as_ref(&self._animation_play_infos[AnimationLayer::AdditiveLayer as usize]);
            let base_animation = ptr_as_mut(&self._animation_play_infos[AnimationLayer::BaseLayer as usize]);
            base_animation.update_additive_animation(additive_animation);
        }
    }
}
