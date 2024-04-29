use std::collections::HashMap;

use nalgebra::{Matrix4, Quaternion, Vector3};
use serde::{Deserialize, Serialize};

use crate::scene::mesh::MeshData;
use crate::scene::transform_object::{SimpleTransform, TransformObjectData};

use nalgebra_glm as glm;
use crate::constants;
use crate::utilities::system::{ptr_as_ref, RcRefCell};

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
    pub _transform: Matrix4<f32>,
    pub _bone_names: Vec<String>,
    pub _bone_index_map: HashMap<String, usize>,
    pub _bones: Vec<BoneData>,
    pub _hierarchy: Vec<*mut BoneData>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct AnimationNodeCreateInfo {
    pub _name: String,
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
pub struct AnimationBuffer {
    pub _prev_animation_buffer: Vec<Matrix4<f32>>,
    pub _animation_buffer: Vec<Matrix4<f32>>
}


#[derive(Clone, Debug)]
pub struct AnimationPlayInfo {
    pub _is_animation_end: bool,
    pub _is_last_animation_frame: bool,
    pub _animation_loop: bool,
    pub _animation_blend_ratio: f32,
    pub _animation_blend_time: f32,
    pub _animation_fade_out_ratio: f32,
    pub _animation_fade_out_time: f32,
    pub _animation_elapsed_time: f32,
    pub _animation_speed: f32,
    pub _prev_animation_frame: f32,
    pub _animation_frame: f32,
    pub _prev_animation_play_time: f32,
    pub _animation_play_time: f32,
    pub _animation_end_time: Option<f32>,
    pub _animation_transforms: Vec<SimpleTransform>,
    pub _last_animation_transforms: Vec<SimpleTransform>,
    pub _animation_index: usize,
    pub _animation_mesh: Option<RcRefCell<MeshData>>,
    pub _animation_blend_masks: *const HashMap<String, f32>
}

#[derive(Clone, Debug)]
pub struct AnimationPlayArgs {
    pub _animation_speed: f32,
    pub _animation_loop: bool,
    pub _animation_start_time: f32,
    pub _animation_end_time: Option<f32>,
    pub _animation_blend_time: f32,
    pub _animation_fade_out_time: f32,
    pub _force_animation_setting: bool,
    pub _reset_animation_time: bool,
    pub _animation_blend_masks: *const HashMap<String, f32>
}

// Implementations

impl Default for AnimationNodeCreateInfo {
    fn default() -> AnimationNodeCreateInfo {
        AnimationNodeCreateInfo {
            _name: String::new(),
            _combined_inv_bind_matrix: constants::COMBINED_INVERSE_BIND_MATRIX,
            _target: String::new(),
            _times: Vec::new(),
            _locations: Vec::new(),
            _rotations: Vec::new(),
            _scales: Vec::new(),
            _interpolations: Vec::new(),
            _in_tangents: Vec::new(),
            _out_tangents: Vec::new(),
        }
    }
}

impl Default for SkeletonHierarchyTree {
    fn default() -> SkeletonHierarchyTree {
        SkeletonHierarchyTree {
            _children: HashMap::new(),
        }
    }
}

impl BoneData {
    pub fn create_bone(
        name: &String,
        index: usize,
        depth: u32,
        inv_bind_matrix: &Matrix4<f32>,
    ) -> BoneData {
        BoneData {
            _name: name.clone(),
            _transform: TransformObjectData::create_transform_object_data(),
            _inv_bind_matrix: inv_bind_matrix.clone() as Matrix4<f32>,
            _parent: std::ptr::null_mut(),
            _children: Vec::new(),
            _index: index,
            _depth: depth,
        }
    }

    pub fn set_parent(&mut self, parent_bone: *mut BoneData) {
        self._parent = parent_bone;
    }

    pub fn add_child(&mut self, child_bone: *mut BoneData) {
        unsafe {
            (*child_bone).set_parent(self);
            self._children.push(child_bone)
        }
    }
}

impl SkeletonData {
    pub fn create_skeleton_data(
        index: usize,
        skeleton_data_create_info: &SkeletonDataCreateInfo,
    ) -> SkeletonData {
        let mut skeleton_data = SkeletonData {
            _name: skeleton_data_create_info._name.clone(),
            _index: index,
            _transform: skeleton_data_create_info._transform.clone_owned(),
            _bone_names: skeleton_data_create_info._bone_names.clone(),
            _bone_index_map: HashMap::new(),
            _bones: skeleton_data_create_info
                ._bone_names
                .iter()
                .enumerate()
                .map(|(bone_index, bone_name)| {
                    BoneData::create_bone(bone_name, bone_index, 0, &Matrix4::identity())
                })
                .collect(),
            _hierarchy: Vec::new(),
        };

        for (bone_index, bone_name) in skeleton_data_create_info._bone_names.iter().enumerate() {
            skeleton_data._bone_index_map.insert(bone_name.clone(), bone_index);
        }

        skeleton_data.build_bone(
            &skeleton_data_create_info._hierarchy,
            &skeleton_data_create_info._inv_bind_matrices,
            std::ptr::null_mut(),
            0,
        );
        skeleton_data
    }

    pub fn build_bone(
        &mut self,
        hierarchy: &SkeletonHierarchyTree,
        inv_bind_matrices: &Vec<Matrix4<f32>>,
        parent_bone: *mut BoneData,
        depth: u32,
    ) {
        for (bone_name, child_hierarchy) in hierarchy._children.iter() {
            if let Some(index) = self._bone_names.iter().position(|key| key == bone_name) {
                self._bones[index] =
                    BoneData::create_bone(bone_name, index, depth, &inv_bind_matrices[index]);
                let bone: *mut BoneData = &mut self._bones[index];
                if parent_bone.is_null() {
                    // add root
                    self._hierarchy.push(bone);
                } else {
                    unsafe {
                        (*parent_bone).add_child(bone);
                    }
                }
                // recursive build bone
                self.build_bone(child_hierarchy, inv_bind_matrices, bone, depth + 1);
            }
        }
    }
}

impl AnimationData {
    pub fn create_animation_data(
        name: &String,
        index: usize,
        skeleton: *const SkeletonData,
        animation_node_create_infos: &Vec<AnimationNodeCreateInfo>,
    ) -> AnimationData {
        let mut animation_data = AnimationData {
            _name: name.clone(),
            _index: index,
            _skeleton: skeleton,
            _frame_count: 0,
            _frame_times: Vec::new(),
            _animation_length: 0.0,
            _nodes: Vec::new(), // order by bone index
            _root_node: std::ptr::null(),
        };

        unsafe {
            for (i, animation_node_create_info) in animation_node_create_infos.iter().enumerate() {
                let animation_node = AnimationNodeData::create_animation_node_data(
                    &(*animation_data._skeleton)._bones[i],
                    animation_node_create_info,
                );
                let frame_count = animation_node._frame_times.len();
                if animation_data._frame_count < frame_count {
                    animation_data._frame_count = frame_count;
                    animation_data._frame_times = animation_node._frame_times.clone();
                }
                animation_data._nodes.push(animation_node);
            }
        }

        if false == animation_data._nodes.is_empty() {
            animation_data._root_node = &animation_data._nodes[0];
        }

        for frame_time in animation_data._frame_times.iter() {
            animation_data._animation_length = animation_data._animation_length.max(*frame_time);
        }

        animation_data
    }

    pub fn get_bone_count(&self) -> usize {
        return self._nodes.len();
    }

    pub fn get_time_to_frame(&self, current_frame: f32, current_time: f32) -> f32 {
        if 1 < self._frame_count {
            let mut frame = current_frame as usize;
            let last_index = self._frame_count - 1;

            if last_index <= frame {
                frame %= last_index;
            }

            loop {
                if (0 == frame && current_time <= self._frame_times[frame])
                    || (self._frame_times[frame] <= current_time
                    && current_time <= self._frame_times[frame + 1])
                {
                    break;
                }
                frame = (frame + 1) % last_index;
            }

            let frame_time = self._frame_times[frame];
            let next_frame_time = self._frame_times[frame + 1];
            let ratio = (current_time - frame_time) / (next_frame_time - frame_time);
            return frame as f32 + ratio;
        }
        0.0
    }
}

impl AnimationNodeData {
    pub fn create_animation_node_data(
        bone: *const BoneData,
        animation_node_create_info: &AnimationNodeCreateInfo,
    ) -> AnimationNodeData {
        AnimationNodeData {
            _name: animation_node_create_info._name.clone(),
            _combined_inv_bind_matrix: animation_node_create_info._combined_inv_bind_matrix,
            _target: animation_node_create_info._target.clone(), // bone name
            _frame_times: animation_node_create_info._times.clone(),
            _locations: animation_node_create_info._locations.clone(),
            _rotations: animation_node_create_info._rotations.clone(),
            _scales: animation_node_create_info._scales.clone(),
            _interpolations: animation_node_create_info._interpolations.clone(),
            _in_tangents: animation_node_create_info._in_tangents.clone(),
            _out_tangents: animation_node_create_info._out_tangents.clone(),
            _bone: bone,
            _frame_count: animation_node_create_info._times.len(),
        }
    }

    pub fn calc_animation_transform(&self, frame: f32) -> SimpleTransform{
        if (frame as usize) < self._frame_count {
            let rate = frame.fract();
            let frame: usize = (frame as usize) % self._frame_count;
            let next_frame: usize = (frame + 1) % self._frame_count;
            if frame < self._frame_count {
                return SimpleTransform {
                    _position: glm::lerp(&self._locations[frame], &self._locations[next_frame], rate),
                    _rotation: glm::quat_slerp(&self._rotations[frame], &self._rotations[next_frame], rate),
                    _scale: glm::lerp(&self._scales[frame], &self._scales[next_frame], rate)
                }
            }
        }
        SimpleTransform::default()
    }
}


impl AnimationBuffer {
    pub fn create_animation_buffer(bone_count: usize) -> AnimationBuffer {
        AnimationBuffer {
            _animation_buffer: vec![Matrix4::identity(); bone_count],
            _prev_animation_buffer: vec![Matrix4::identity(); bone_count]
        }
    }

    pub fn swap_animation_buffer(&mut self) {
        std::mem::swap(
            &mut self._prev_animation_buffer,
            &mut self._animation_buffer
        );
    }

    pub fn update_animation_buffer(&mut self, animation_play_info: &AnimationPlayInfo) {
        let animation_transforms: &Vec<SimpleTransform> = &animation_play_info._animation_transforms;
        let animation_data_list: &Vec<AnimationData> = &animation_play_info._animation_mesh.as_ref().unwrap().borrow()._animation_data_list;
        let animation_data: &AnimationData = &animation_data_list[animation_play_info._animation_index];
        for bone_data in ptr_as_ref(animation_data._skeleton)._hierarchy.iter() {
            let bone_index: usize = ptr_as_ref(*bone_data)._index;
            let bone_node = &animation_data._nodes[bone_index];
            let transform = ptr_as_ref(animation_data._skeleton)._transform * animation_transforms[bone_index].to_matrix();
            self._animation_buffer[bone_index] = transform * ptr_as_ref(bone_node._bone)._inv_bind_matrix;
            self.update_hierarchical_animation_transform(
                *bone_data,
                &transform,
                animation_data,
                animation_transforms,
            );
        }
    }

    pub fn update_hierarchical_animation_transform(
        &mut self,
        parent_bone: *const BoneData,
        parent_matrix: *const Matrix4<f32>,
        animation_data: &AnimationData,
        animation_transforms: &Vec<SimpleTransform>,
    ) {
        for bone_data in ptr_as_ref(parent_bone)._children.iter() {
            let bone_index: usize = ptr_as_ref(*bone_data)._index;
            let bone_node = &animation_data._nodes[bone_index];
            let transform = ptr_as_ref(parent_matrix) * animation_transforms[bone_index].to_matrix();
            self._animation_buffer[bone_index] = transform * ptr_as_ref(bone_node._bone)._inv_bind_matrix;
            self.update_hierarchical_animation_transform(
                *bone_data,
                &transform,
                animation_data,
                animation_transforms,
            );
        }
    }
}



impl Default for AnimationPlayInfo {
    fn default() -> AnimationPlayInfo {
        AnimationPlayInfo {
            _animation_loop: true,
            _animation_blend_ratio: 0.0,
            _animation_blend_time: 0.1,
            _animation_fade_out_ratio: 1.0,
            _animation_fade_out_time: 0.0,
            _animation_elapsed_time: 0.0,
            _animation_speed: 1.0,
            _prev_animation_frame: 0.0,
            _animation_frame: 0.0,
            _prev_animation_play_time: 0.0,
            _animation_play_time: 0.0,
            _animation_end_time: None,
            _is_animation_end: false,
            _is_last_animation_frame: false,
            _animation_transforms: Vec::new(),
            _last_animation_transforms: Vec::new(),
            _animation_index: 0,
            _animation_mesh: None,
            _animation_blend_masks: std::ptr::null()
        }
    }
}

impl AnimationPlayInfo {
    pub fn create_animation_play_info(mesh_data: Option<RcRefCell<MeshData>>, bone_count: usize) -> AnimationPlayInfo {
        AnimationPlayInfo {
            _animation_transforms: vec![SimpleTransform::default(); bone_count],
            _last_animation_transforms: vec![SimpleTransform::default(); bone_count],
            _animation_mesh: mesh_data,
            ..Default::default()
        }
    }

    pub fn is_valid(&self) -> bool {
        self._animation_mesh.is_some()
    }

    pub fn update_animation_frame_time(&mut self, delta_time: f32) -> bool {
        let anim_delta_time: f32 = self._animation_speed * delta_time;
        self._prev_animation_frame = self._animation_frame;
        self._prev_animation_play_time = self._animation_play_time;

        if self._is_last_animation_frame {
            self._is_animation_end = true;
            return false;
        }

        let animation_data_list: &Vec<AnimationData> = &self._animation_mesh.as_ref().unwrap().borrow()._animation_data_list;
        let animation_data = &animation_data_list[self._animation_index];

        // update animation time
        if 1 < animation_data._frame_count {
            self._animation_play_time += anim_delta_time;
            let mut animation_end_time = animation_data._animation_length;
            if let Some(custom_end_time) = self._animation_end_time {
                if custom_end_time < animation_end_time {
                    animation_end_time = custom_end_time;
                }
            }

            if self._animation_loop {
                if animation_end_time < self._animation_play_time {
                    self._animation_play_time = self._animation_play_time % animation_end_time;
                }
            } else {
                if animation_end_time <= self._animation_play_time {
                    self._animation_play_time = animation_end_time;
                    self._is_last_animation_frame = true;
                }
            }

            self._animation_frame = animation_data.get_time_to_frame(
                self._animation_frame,
                self._animation_play_time,
            );

            if 0.0 == self._animation_blend_time {
                self._animation_blend_ratio = 1.0;
            } else {
                self._animation_blend_ratio = 1f32.min(self._animation_elapsed_time / self._animation_blend_time);
            }

            if self._animation_loop || 0.0 == self._animation_fade_out_time {
                self._animation_fade_out_ratio = 1.0;
            } else {
                self._animation_fade_out_ratio = 1f32.min(0f32.max((animation_end_time - self._animation_elapsed_time) / self._animation_fade_out_time));
            }
        } else {
            self._animation_frame = 0.0;
        }
        self._animation_elapsed_time += anim_delta_time;

        // update animation buffers
        if self._prev_animation_frame != self._animation_frame {
            return true;
        }
        return false;
    }

    pub fn combine_additive_animation(&mut self, additive_animation_play_info: &AnimationPlayInfo) {
        let mesh_data = self._animation_mesh.as_ref().unwrap().borrow();
        let skeleton_data = &mesh_data._skeleton_data_list[self._animation_index];
        if additive_animation_play_info._animation_blend_masks.is_null() {
            for (bone_index, additive_animation_transform) in additive_animation_play_info._animation_transforms.iter().enumerate() {
                self._animation_transforms[bone_index] =
                    self._animation_transforms[bone_index].lerp(additive_animation_transform, additive_animation_play_info._animation_fade_out_ratio);
            }
        } else {
            for (bone_name, blend_ratio) in ptr_as_ref(additive_animation_play_info._animation_blend_masks).iter() {
                let bone_index = skeleton_data._bone_index_map.get(bone_name);
                if bone_index.is_some() {
                    let blend_ratio = *blend_ratio * additive_animation_play_info._animation_fade_out_ratio;
                    let bone_index = *bone_index.unwrap();
                    let base_animation_transform = self._animation_transforms[bone_index].clone();
                    let additive_animation_transform = &additive_animation_play_info._animation_transforms[bone_index];
                    self._animation_transforms[bone_index] = base_animation_transform.lerp(additive_animation_transform, blend_ratio);
                }
            }
        }
    }

    pub fn set_animation_play_info(&mut self, animation_args: &AnimationPlayArgs, enable_blend_animatioin: bool) {
        self._animation_speed = animation_args._animation_speed;
        self._animation_loop = animation_args._animation_loop;
        self._animation_blend_time = if enable_blend_animatioin { animation_args._animation_blend_time } else { 0.0 };
        self._animation_fade_out_time = animation_args._animation_fade_out_time;
        self._animation_end_time = animation_args._animation_end_time;
        self._animation_blend_masks = animation_args._animation_blend_masks.clone();
        if animation_args._reset_animation_time {
            self._is_animation_end = false;
            self._is_last_animation_frame = false;
            self._animation_elapsed_time = 0.0;
            self._prev_animation_play_time = animation_args._animation_start_time;
            self._animation_play_time = animation_args._animation_start_time;
            self._prev_animation_frame = 0.0;
            self._animation_frame = 0.0;
        }
    }
}

impl Default for AnimationPlayArgs {
    fn default() -> AnimationPlayArgs {
        AnimationPlayArgs {
            _animation_speed: 1.0,
            _animation_loop: true,
            _animation_start_time: 0.0,
            _animation_end_time: None,
            _animation_blend_time: 0.1,
            _animation_fade_out_time: 0.0,
            _force_animation_setting: false,
            _reset_animation_time: true,
            _animation_blend_masks: std::ptr::null()
        }
    }
}