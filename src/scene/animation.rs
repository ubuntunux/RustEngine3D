use std::collections::HashMap;

use nalgebra::{Matrix4, Quaternion, Vector3};
use serde::{Deserialize, Serialize};

use crate::scene::mesh::MeshData;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::system::RcRefCell;

#[derive(Clone, Debug)]
pub struct BoneData {
    pub _name: String,
    pub _transform: TransformObjectData,
    pub _inv_bind_matrix: Matrix4<f32>,
    pub _parent: *mut BoneData,
    pub _children: Vec<*mut BoneData>,
    pub _index: usize,
    pub _depth: u32,
}

#[derive(Clone, Debug)]
pub struct SkeletonData {
    pub _name: String,
    pub _index: usize,
    pub _bone_names: Vec<String>,
    pub _bone_index_map: HashMap<String, usize>,
    pub _bones: Vec<BoneData>,
    pub _hierarchy: Vec<*mut BoneData>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct AnimationNodeCreateInfo {
    pub _name: String,
    pub _hierarchically_accumulated_matrix: bool,
    pub _combined_inv_bind_matrix: bool,
    pub _target: String,
    pub _times: Vec<f32>,
    pub _locations: Vec<Vector3<f32>>,
    pub _rotations: Vec<Quaternion<f32>>,
    pub _scales: Vec<Vector3<f32>>,
    pub _interpolations: Vec<String>,
    pub _in_tangents: Vec<Vec<f32>>,
    pub _out_tangents: Vec<Vec<f32>>,
}

#[derive(Clone, Debug)]
pub struct AnimationNodeData {
    pub _name: String,
    pub _hierarchically_accumulated_matrix: bool,
    pub _combined_inv_bind_matrix: bool,
    pub _target: String,
    pub _frame_times: Vec<f32>,
    pub _locations: Vec<Vector3<f32>>,
    pub _rotations: Vec<Quaternion<f32>>,
    pub _scales: Vec<Vector3<f32>>,
    pub _interpolations: Vec<String>,
    pub _in_tangents: Vec<Vec<f32>>,
    pub _out_tangents: Vec<Vec<f32>>,
    pub _bone: *const BoneData,
    pub _frame_count: usize,
}

#[derive(Clone, Debug)]
pub struct AnimationData {
    pub _name: String,
    pub _index: usize,
    pub _skeleton: *const SkeletonData,
    pub _frame_count: usize,
    pub _frame_times: Vec<f32>,
    pub _animation_length: f32,
    pub _nodes: Vec<AnimationNodeData>, // order by bone index
    pub _root_node: *const AnimationNodeData,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct SkeletonHierarchyTree {
    pub _children: HashMap<String, SkeletonHierarchyTree>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
#[serde(default)]
pub struct SkeletonDataCreateInfo {
    pub _name: String,
    pub _transform: Matrix4<f32>,
    pub _hierarchy: SkeletonHierarchyTree, // bone names map as hierarchy
    pub _bone_names: Vec<String>,          // bone name list ordered by index
    pub _inv_bind_matrices: Vec<Matrix4<f32>>, // inverse matrix of bone
}

#[derive(Clone, Debug)]
pub struct AnimationPlayInfo {
    pub _is_updated_animation: bool,
    pub _is_animation_end: bool,
    pub _last_animation_frame: f32,
    pub _animation_loop: bool,
    pub _animation_blend_time: f32,
    pub _animation_elapsed_time: f32,
    pub _animation_speed: f32,
    pub _animation_frame: f32,
    pub _animation_play_time: f32,
    pub _animation_end_time: Option<f32>,
    pub _animation_buffer: Vec<Matrix4<f32>>,
    pub _prev_animation_buffer: Vec<Matrix4<f32>>,
    pub _blend_animation_buffer: Vec<Matrix4<f32>>,
    pub _animation_index: usize,
    pub _animation_mesh: Option<RcRefCell<MeshData>>,
    pub _animation_blend_masks: HashMap<String, f32>
}

#[derive(Clone, Debug)]
pub struct AnimationPlayArgs {
    pub _animation_speed: f32,
    pub _animation_loop: bool,
    pub _animation_start_time: f32,
    pub _animation_end_time: Option<f32>,
    pub _animation_blend_time: f32,
    pub _force_animation_setting: bool,
    pub _reset_animation_time: bool,
    pub _animation_blend_masks: HashMap<String, f32>
}
