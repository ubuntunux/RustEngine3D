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
    pub _animation_end_time: Option<f32>,
    pub _is_animation_end: bool,
    pub _animation_buffers: Vec<Vec<Matrix4<f32>>>,
    pub _prev_animation_buffers: Vec<Vec<Matrix4<f32>>>,
    pub _blend_animation_buffers: Vec<Vec<Matrix4<f32>>>,
    pub _animation_count: i32,
    pub _animation_mesh: Option<RcRefCell<MeshData>>,
}

#[derive(Clone, Debug)]
pub struct AnimationPlayArgs {
    pub _speed: f32,
    pub _loop: bool,
    pub _start_time: f32,
    pub _end_time: Option<f32>,
    pub _blend_time: f32,
    pub _force: bool,
    pub _reset: bool,
}

impl Default for AnimationPlayArgs {
    fn default() -> AnimationPlayArgs {
        AnimationPlayArgs {
            _speed: 1.0,
            _loop: true,
            _start_time: 0.0,
            _end_time: None,
            _blend_time: 0.5,
            _force: false,
            _reset: true,
        }
    }
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
            _animation_end_time: None,
            _is_animation_end: false,
            _animation_buffers: Vec::new(),
            _prev_animation_buffers: Vec::new(),
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
            _animation_play_info: None,
        };

        render_object_data.initialize_animation_play_info(has_animation_data);
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

    pub fn initialize_animation_play_info(&mut self, has_animation_data: bool) {
        if has_animation_data {
            let mut animation_play_info = AnimationPlayInfo::default();
            for animation in self._mesh_data.borrow_mut()._animation_datas.iter_mut() {
                let mut animation_buffers: Vec<Matrix4<f32>> = vec![Matrix4::identity(); animation.get_bone_count()];
                animation.get_animation_transforms(0.0, &mut animation_buffers);
                animation_play_info._animation_buffers.push(animation_buffers.clone());
                animation_play_info._prev_animation_buffers.push(animation_buffers.clone());
                animation_play_info._blend_animation_buffers.push(animation_buffers.clone())
            }
            animation_play_info._animation_mesh = Some(self._mesh_data.clone());
            self._animation_play_info = Some(animation_play_info);
        }
    }

    pub fn set_animation(&mut self, animation_mesh: &RcRefCell<MeshData>, animation_args: &AnimationPlayArgs) {
        let animation_play_info = &mut self._animation_play_info.as_mut().unwrap();
        if animation_args._force || animation_mesh.as_ptr() != animation_play_info._animation_mesh.as_ref().unwrap().as_ptr() {
            log::info!("set_animation: {:?}", animation_mesh.borrow()._name);
            animation_play_info._animation_mesh = Some(animation_mesh.clone());
            animation_play_info._animation_speed = animation_args._speed;
            animation_play_info._animation_loop = animation_args._loop;
            animation_play_info._animation_blend_time = animation_args._blend_time;
            animation_play_info._animation_end_time = animation_args._end_time;
            if animation_args._reset {
                animation_play_info._animation_elapsed_time = 0.0;
                animation_play_info._animation_start_time = animation_args._start_time;
                animation_play_info._animation_frame = 0.0;
                animation_play_info._is_animation_end = false;
            }

            // swap
            std::mem::swap(&mut animation_play_info._prev_animation_buffers, &mut animation_play_info._animation_buffers);
        }
    }

    /*
    def get_prev_animation_buffer(self, index):
        return self.prev_animation_buffers[index]

    def get_animation_buffer(self, index):
        return self.animation_buffers[index]

    def update(self, dt):
        StaticActor.update(self, dt)

        # update animation
        animation_end = self.is_animation_end
        blend_ratio = 1.0
        update_animation_frame = True
        for i, animation in enumerate(self.animation_mesh.animations):
            if animation is not None:
                # update animation frame only first animation
                if update_animation_frame:
                    update_animation_frame = False
                    frame_count = animation.frame_count
                    if frame_count > 1:
                        self.animation_start_time += dt * self.animation_speed

                        animation_end_time = animation.animation_length

                        if self.animation_end_time is not None and self.animation_end_time < animation_end_time:
                            animation_end_time = self.animation_end_time

                        if self.animation_loop:
                            self.animation_start_time = math.fmod(self.animation_start_time + dt * self.animation_speed, animation_end_time)
                        else:
                            self.animation_start_time = min(animation_end_time, self.animation_start_time)
                            if animation_end_time == self.animation_start_time:
                                animation_end = True
                        self.animation_frame = animation.get_time_to_frame(self.animation_frame, self.animation_start_time)
                    else:
                        self.animation_frame = 0.0
                    if self.animation_elapsed_time < self.animation_blend_time:
                        blend_ratio = self.animation_elapsed_time / self.animation_blend_time
                    self.animation_elapsed_time += dt

                # update animation buffers
                self.prev_animation_buffers[i][...] = self.animation_buffers[i]

                if self.last_animation_frame != self.animation_frame:
                    self.last_animation_frame = self.animation_frame
                    animation_buffer = animation.get_animation_transforms(self.animation_frame)

                    if blend_ratio < 1.0:
                        self.animation_buffers[i][...] = self.blend_animation_buffers[i] * (1.0 - blend_ratio) + animation_buffer * blend_ratio
                    else:
                        self.animation_buffers[i][...] = animation_buffer
        self.is_animation_end = animation_end

    */
}