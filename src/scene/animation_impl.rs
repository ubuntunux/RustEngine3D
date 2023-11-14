use std::collections::HashMap;

use nalgebra::Matrix4;
use nalgebra_glm as glm;

use crate::constants;
use crate::scene::animation::*;
use crate::scene::transform_object::TransformObjectData;
use crate::utilities::math;


impl Default for AnimationNodeCreateInfo {
    fn default() -> AnimationNodeCreateInfo {
        AnimationNodeCreateInfo {
            _name: String::new(),
            _hierarchically_accumulated_matrix: constants::ENABLE_HIERARCHICALLY_ACCUMULATED_MATRIX,
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

    pub fn update_hierarchical_animation_transform(
        &self,
        frame: f32,
        parent_bone: *const BoneData,
        parent_matrix: *const Matrix4<f32>,
        animation_transforms: &mut [Matrix4<f32>],
    ) {
        unsafe {
            for bone in (*parent_bone)._children.iter() {
                let index: usize = (**bone)._index;
                let node = &self._nodes[index];
                let mut transform = node.calc_animation_matrix(frame);
                if false == node._combined_inv_bind_matrix {
                    transform = &transform* &(*node._bone)._inv_bind_matrix;
                }
                animation_transforms[index] = &(*parent_matrix) * &transform;
                self.update_hierarchical_animation_transform(
                    frame,
                    *bone,
                    &animation_transforms[index],
                    animation_transforms,
                );
            }
        }
    }

    pub fn update_animation_transforms(
        &self,
        frame: f32,
        animation_transforms: &mut [Matrix4<f32>],
    ) {
        unsafe {
            if (*self._root_node)._hierarchically_accumulated_matrix {
                for (index, node) in self._nodes.iter().enumerate() {
                    animation_transforms[index].copy_from(&node.calc_animation_matrix(frame));
                }
            } else {
                for bone in (*self._skeleton)._hierarchy.iter() {
                    let index: usize = (**bone)._index;
                    let node = &self._nodes[index];
                    let mut transform = node.calc_animation_matrix(frame);
                    if false == node._combined_inv_bind_matrix {
                        transform = &transform* &(*node._bone)._inv_bind_matrix;
                    }
                    animation_transforms[index] = &(*self._skeleton)._transform * &transform;
                    self.update_hierarchical_animation_transform(
                        frame,
                        *bone,
                        &animation_transforms[index],
                        animation_transforms,
                    );
                }
            }
        }
    }
}

impl AnimationNodeData {
    pub fn create_animation_node_data(
        bone: *const BoneData,
        animation_node_create_info: &AnimationNodeCreateInfo,
    ) -> AnimationNodeData {
        AnimationNodeData {
            _name: animation_node_create_info._name.clone(),
            _hierarchically_accumulated_matrix: animation_node_create_info                ._hierarchically_accumulated_matrix,
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

    pub fn calc_animation_matrix(&self, frame: f32) -> Matrix4<f32> {
        let mut transform: Matrix4<f32> = Matrix4::identity();
        if (frame as usize) < self._frame_count {
            let rate = frame.fract();
            let frame: usize = (frame as usize) % self._frame_count;
            let next_frame: usize = (frame + 1) % self._frame_count;
            if frame < self._frame_count {
                let rotation = glm::quat_slerp(&self._rotations[frame], &self._rotations[next_frame], rate);
                let location = glm::lerp(&self._locations[frame], &self._locations[next_frame], rate);
                let scale = glm::lerp(&self._scales[frame], &self._scales[next_frame], rate);
                transform = math::combinate_matrix(
                    &location,
                    &math::quaternion_to_matrix(&rotation),
                    &scale,
                );
            }
        }
        transform
    }
}

impl Default for AnimationPlayInfo {
    fn default() -> AnimationPlayInfo {
        AnimationPlayInfo {
            _is_updated_animation: false,
            _last_animation_frame: 0.0,
            _animation_loop: true,
            _animation_blend_time: 0.1,
            _animation_elapsed_time: 0.0,
            _animation_speed: 1.0,
            _animation_frame: 0.0,
            _animation_play_time: 0.0,
            _animation_end_time: None,
            _is_animation_end: false,
            _animation_buffer: Vec::new(),
            _prev_animation_buffer: Vec::new(),
            _blend_animation_buffer: Vec::new(),
            _animation_index: 0,
            _animation_mesh: None,
            _animation_blend_masks: HashMap::new()
        }
    }
}

impl AnimationPlayInfo {
    pub fn update_animation_play_info(&mut self, delta_time: f32) {
        // reset
        self._is_updated_animation = false;
        if self._is_animation_end {
            return
        }

        let animation_data_list: &Vec<AnimationData> = &self
            ._animation_mesh
            .as_ref()
            .unwrap()
            .borrow()
            ._animation_data_list;
        let animation_index = self._animation_index.max(animation_data_list.len() - 1);
        let animation_data = &animation_data_list[animation_index];

        // update animation time
        if 1 < animation_data._frame_count {
            self._animation_play_time += self._animation_speed * delta_time;
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
                    self._is_animation_end = true;
                }
            }
            self._animation_frame = animation_data.get_time_to_frame(
                self._animation_frame,
                self._animation_play_time,
            );
        } else {
            self._animation_frame = 0.0;
        }
        let animation_elapsed_time = self._animation_elapsed_time;
        self._animation_elapsed_time += delta_time;

        // update animation buffers
        if self._last_animation_frame != self._animation_frame {
            self._is_updated_animation = true;
            self._last_animation_frame = self._animation_frame;

            std::mem::swap(
                &mut self._prev_animation_buffer,
                &mut self._animation_buffer
            );

            animation_data.update_animation_transforms(
                self._animation_frame,
                &mut self._animation_buffer,
            );

            // blend animation
            let blend_ratio: f32 = animation_elapsed_time / self._animation_blend_time;
            if blend_ratio < 1.0 {
                for (bone_index, animation_buffer) in self._animation_buffer.iter_mut().enumerate() {
                    let blend_animation_buffer = &self._blend_animation_buffer[bone_index];
                    animation_buffer.copy_from(
                        &((blend_animation_buffer * (1.0 - blend_ratio)) + (&(*animation_buffer) * blend_ratio)),
                    );
                }
            }
        }
    }

    pub fn update_additive_animation(&mut self, additive_animation_play_info: &AnimationPlayInfo) {
        if self._is_updated_animation && false == additive_animation_play_info._is_animation_end {
            let mesh_data = self._animation_mesh.as_ref().unwrap().borrow();
            let skeleton_data = mesh_data._skeleton_data_list.first().unwrap();
            for (bone_name, blend_ratio) in additive_animation_play_info._animation_blend_masks.iter() {
                let bone_index = skeleton_data._bone_index_map.get(bone_name);
                if bone_index.is_some() {
                    let blend_ratio = *blend_ratio;
                    let bone_index = *bone_index.unwrap();
                    let base_animation_buffer = &mut self._animation_buffer[bone_index];
                    let additive_animation_buffer = &additive_animation_play_info._animation_buffer[bone_index];
                    base_animation_buffer.copy_from(
                        &(&*base_animation_buffer * (1.0 - blend_ratio) + additive_animation_buffer * blend_ratio),
                    );
                }
            }
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
            _force_animation_setting: false,
            _reset_animation_time: true,
            _animation_blend_masks: HashMap::new()
        }
    }
}

impl AnimationPlayInfo {
    pub fn set_animation_play_info(&mut self, animation_args: &AnimationPlayArgs) {
        self._animation_speed = animation_args._animation_speed;
        self._animation_loop = animation_args._animation_loop;
        self._animation_blend_time = animation_args._animation_blend_time;
        self._animation_end_time = animation_args._animation_end_time;
        if animation_args._reset_animation_time {
            self._is_updated_animation = true;
            self._is_animation_end = false;
            self._animation_elapsed_time = 0.0;
            self._animation_play_time = animation_args._animation_start_time;
            self._animation_frame = 0.0;
            self._animation_blend_masks = animation_args._animation_blend_masks.clone();
        }
    }
}
