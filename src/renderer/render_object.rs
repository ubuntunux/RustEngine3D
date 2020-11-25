use nalgebra::{
    Vector3,
    Matrix4,
};

use crate::renderer::mesh::MeshData;
use crate::renderer::model::ModelData;
use crate::renderer::transform_object::TransformObjectData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct RenderObjectCreateInfo {
    pub _model_data: Option<RcRefCell<ModelData>>,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
}

#[derive(Clone, Debug)]
pub struct RenderObjectData {
    pub _render_object_name: String,
    pub _mesh_data: RcRefCell<MeshData>,
    pub _model_data: RcRefCell<ModelData>,
    pub _transform_object: TransformObjectData,
    pub _animation_play_info: Option<AnimationPlayInfo>,
}

#[derive(Clone, Debug)]
pub struct AnimationPlayInfo {
    pub _last_animation_frame: f32,
    pub _animation_loop: bool,
    pub _animation_blend_time: f32,
    pub _animation_elapsed_time: f32,
    pub _animation_speed: f32,
    pub _animation_frame: f32,
    pub _animation_start_time: f32,
    pub _animation_end_time: f32,
    pub _is_animation_end: bool,
    pub _flip_animation_buffer: bool,
    pub _animation_buffers0: Vec<Vec<Matrix4<f32>>>,
    pub _animation_buffers1: Vec<Vec<Matrix4<f32>>>,
    pub _blend_animation_buffers: Vec<Vec<Matrix4<f32>>>,
    pub _animation_count: i32,
    pub _animation_mesh: Option<RcRefCell<MeshData>>,
}

impl Default for RenderObjectCreateInfo {
    fn default() -> RenderObjectCreateInfo {
        RenderObjectCreateInfo {
            _model_data: None,
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
        }
    } 
}

impl Default for AnimationPlayInfo {
    fn default() -> AnimationPlayInfo {
        AnimationPlayInfo {
            _last_animation_frame: 0.0,
            _animation_loop: true,
            _animation_blend_time: 0.5,
            _animation_elapsed_time: 0.0,
            _animation_speed: 1.0,
            _animation_frame: 0.0,
            _animation_start_time: 0.0,
            _animation_end_time: -1.0,
            _is_animation_end: false,
            _flip_animation_buffer: false,
            _animation_buffers0: Vec::new(),
            _animation_buffers1: Vec::new(),
            _blend_animation_buffers: Vec::new(),
            _animation_count: 0,
            _animation_mesh: None,
        }
    } 
}

impl RenderObjectData {
    pub fn create_render_object_data(
        render_object_name: &String,
        render_object_create_data: RenderObjectCreateInfo
    ) -> RenderObjectData {
        log::info!("create_render_object_data: {}", render_object_name);
        let mut transform_object_data = TransformObjectData::new_transform_object_data();
        transform_object_data.set_position(&render_object_create_data._position);
        transform_object_data.set_rotation(&render_object_create_data._rotation);
        transform_object_data.set_scale(&render_object_create_data._scale);

        let model_data = render_object_create_data._model_data.unwrap();
        let mesh_data = model_data.borrow()._mesh_data.clone();
        let has_animation_data = mesh_data.borrow().has_animation_data();

        let mut render_object_data = RenderObjectData {
            _render_object_name: render_object_name.clone(),
            _model_data: model_data,
            _mesh_data: mesh_data,
            _transform_object: transform_object_data,
            _animation_play_info: if has_animation_data {
                Some(AnimationPlayInfo::default())
            } else {
                None
            },
        };

        render_object_data.initialize_animation_play_info();
        render_object_data
    }

    pub fn get_mesh_data(&self) -> &RcRefCell<MeshData> {
        &self._mesh_data
    }

    pub fn get_model_data(&self) -> &RcRefCell<ModelData> {
        &self._model_data
    }

    pub fn get_transform_object_data(&self) -> &TransformObjectData {
        &self._transform_object
    }

    pub fn update_render_object_data(&mut self) {
        self._transform_object.update_transform_object();
    }

    pub fn has_animation_play_info(&self) -> bool {
        self._animation_play_info.is_some()
    }

    pub fn initialize_animation_play_info(&mut self) {
        let animation_play_info = &mut self._animation_play_info.as_mut().unwrap();
        for animation in self._mesh_data.borrow_mut()._animation_datas.iter_mut() {
            let animation_buffers: &Vec<Matrix4<f32>> = animation.get_animation_transforms(0.0);
            animation_play_info._animation_buffers0.push((*animation_buffers).clone());
            animation_play_info._animation_buffers1.push((*animation_buffers).clone());
            animation_play_info._blend_animation_buffers.push((*animation_buffers).clone())
        }
        self._animation_play_info.as_mut().unwrap()._animation_mesh = Some(self._mesh_data.clone());
    }
}