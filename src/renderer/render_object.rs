use nalgebra::{
    Vector3,
    Matrix4,
};

use crate::renderer::mesh::MeshData;
use crate::renderer::model::ModelData;
use crate::renderer::transform_object::TransformObjectData;

#[derive(Clone, Debug)]
pub struct RenderObjectCreateData {
    pub _model_data: Option<ModelData>,
    pub _position: Vector3<f32>,
    pub _rotation: Vector3<f32>,
    pub _scale: Vector3<f32>,
    pub _has_animation_data: bool,
}

#[derive(Clone, Debug)]
pub struct RenderObjectData {
    pub _render_object_name: String,
    pub _model_data: ModelData,
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
    pub _animation_buffers: Vec<Matrix4<f32>>,
    pub _prev_animation_buffers: Vec<Matrix4<f32>>,
    pub _blend_animation_buffers: Vec<Matrix4<f32>>,
    pub _animation_count: i32,
    pub _animation_mesh: Option<MeshData>,
}

impl Default for RenderObjectCreateData {
    fn default() -> RenderObjectCreateData {
        RenderObjectCreateData {
            _model_data: None,
            _position: Vector3::zeros(),
            _rotation: Vector3::zeros(),
            _scale: Vector3::new(1.0, 1.0, 1.0),
            _has_animation_data: false,
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
            _animation_buffers: Vec::new(),
            _prev_animation_buffers: Vec::new(),
            _blend_animation_buffers: Vec::new(),
            _animation_count: 0,
            _animation_mesh: None,
        }
    } 
}

// instance RenderObjecti32erface RenderObjectData where
//     createRenderObjectData: String -> RenderObjectCreateData -> IO RenderObjectData
//     createRenderObjectData renderObjectName renderObjectCreateData = do
//         log::info!("createRenderObjectData: " ++ show renderObjectName
//         transformObjectData <- newTransformObjectData
//         setPosition transformObjectData $ _position renderObjectCreateData
//         setRotation transformObjectData $ _rotation renderObjectCreateData
//         setScale transformObjectData $ _scale renderObjectCreateData
//         return RenderObjectData
//             { _renderObjectName = renderObjectName
//             , _modelData = _modelData renderObjectCreateData
//             , _transformObject = transformObjectData
//             , _animation_play_info =
//                 case _has_animation_data renderObjectCreateData of
//                     True -> default_animation_play_info
//                     otherwise -> EmptyAnimationPlayInfo
//             }
//
//     getModelData: RenderObjectData -> ModelData
//     getModelData renderObjectData = _modelData renderObjectData
//
//     getTransformObjectData: RenderObjectData -> TransformObjectData
//     getTransformObjectData renderObjectData = _transformObject renderObjectData
//
//     updateRenderObjectData: RenderObjectData -> IO ()
//     updateRenderObjectData renderObjectData = do
//         updateTransformObject (_transformObject renderObjectData)
//         return ()
//
