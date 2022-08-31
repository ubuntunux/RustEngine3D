use std::collections::HashMap;

use serde::{ Serialize, Deserialize };
use nalgebra::{ Vector3, Vector4, Quaternion, Matrix4 };
use nalgebra_glm as glm;

use crate::renderer::transform_object::{ TransformObjectData };
use crate::utilities::math;
use crate::constants;

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
    pub _bones: Vec<BoneData>,
    pub _hierachy: Vec<*mut BoneData>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(default)]
pub struct AnimationNodeCreateInfo {
    pub _name: String,
    pub _precomputed_root_matrix: bool,
    pub _precomputed_combine_inv_bind_matrix: bool,
    pub _target: String,
    pub _times: Vec<f32>,
    pub _locations: Vec<Vector3<f32>>,
    pub _rotations: Vec<Quaternion<f32>>,
    pub _scales: Vec<Vector3<f32>>,
    pub _interpoations: Vec<String>,
    pub _in_tangents: Vec<Vec<f32>>,
    pub _out_tangents: Vec<Vec<f32>>,
}

#[derive(Clone, Debug)]
pub struct AnimationNodeData {
    pub _name: String,
    pub _precomputed_root_matrix: bool,
    pub _precomputed_combine_inv_bind_matrix: bool,
    pub _target: String,
    pub _frame_times: Vec<f32>,
    pub _locations: Vec<Vector3<f32>>,
    pub _rotations: Vec<Quaternion<f32>>,
    pub _scales: Vec<Vector3<f32>>,
    pub _interpoations: Vec<String>,
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
pub struct SkeletonHierachyTree {
    pub _children: HashMap<String, SkeletonHierachyTree>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
#[serde(default)]
pub struct SkeletonDataCreateInfo {
    pub _name: String,
    pub _hierachy: SkeletonHierachyTree, // bone names map as hierachy
    pub _bone_names: Vec<String>, // bone name list ordered by index
    pub _inv_bind_matrices: Vec<Matrix4<f32>>,  // inverse matrix of bone
}

impl Default for AnimationNodeCreateInfo {
    fn default() -> AnimationNodeCreateInfo {
        AnimationNodeCreateInfo {
            _name: String::new(),
            _precomputed_root_matrix: constants::PRECOMPUTED_ROOT_MATRIX,
            _precomputed_combine_inv_bind_matrix: constants::PRECOMPUTED_COMBINE_INV_BIND_MATRIX,
            _target: String::new(),
            _times: Vec::new(),
            _locations: Vec::new(),
            _rotations: Vec::new(),
            _scales: Vec::new(),
            _interpoations: Vec::new(),
            _in_tangents: Vec::new(),
            _out_tangents: Vec::new(),
        }
    }
}

impl Default for SkeletonHierachyTree {
    fn default() -> SkeletonHierachyTree {
        SkeletonHierachyTree {
            _children: HashMap::new(),
        }
    }
}

impl BoneData {
    pub fn create_bone(name: &String, index: usize, depth: u32, inv_bind_matrix: &Matrix4<f32>) -> BoneData {
        BoneData {
            _name: name.clone(),
            _transform: TransformObjectData::new_transform_object_data(),
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
    pub fn create_skeleton_data(index: usize, skeleton_data_create_info: &SkeletonDataCreateInfo) -> SkeletonData {
        let mut skeleton_data = SkeletonData {
            _name: skeleton_data_create_info._name.clone(),
            _index: index,
            _bone_names: skeleton_data_create_info._bone_names.clone(),
            _bones: skeleton_data_create_info._bone_names
                .iter()
                .enumerate()
                .map(|(index, bone_name)| {
                BoneData::create_bone(bone_name, index, 0, &Matrix4::identity())
            }).collect(),
            _hierachy: Vec::new(),
        };
        skeleton_data.build_bone(&skeleton_data_create_info._hierachy, &skeleton_data_create_info._inv_bind_matrices, std::ptr::null_mut(), 0);
        skeleton_data
    }

    pub fn build_bone(&mut self, hierachy: &SkeletonHierachyTree, inv_bind_matrices: &Vec<Matrix4<f32>>, parent_bone: *mut BoneData, depth: u32) {
        for (bone_name, child_hierachy) in hierachy._children.iter() {
            if let Some(index) = self._bone_names.iter().position(|key| key == bone_name) {
                self._bones[index] = BoneData::create_bone(bone_name, index, depth, &inv_bind_matrices[index]);
                let bone: *mut BoneData = &mut self._bones[index];
                if parent_bone.is_null() {
                    // add root
                    self._hierachy.push(bone);
                } else {
                    unsafe {
                        (*parent_bone).add_child(bone);
                    }
                }
                // recursive build bone
                self.build_bone(child_hierachy, inv_bind_matrices, bone, depth + 1);
            }
        }
    }
}


impl AnimationData {
    pub fn create_animation_data(
        name: &String,
        index: usize,
        skeleton: *const SkeletonData,
        animation_node_create_infos: &Vec<AnimationNodeCreateInfo>
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
                let animation_node = AnimationNodeData::create_animation_node_data(&(*animation_data._skeleton)._bones[i], animation_node_create_info);
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
        return self._nodes.len()
    }

    pub fn get_time_to_frame(&self, current_frame: f32, current_time: f32) -> f32 {
        if 1 < self._frame_count {
            let mut frame = current_frame as usize;
            let last_index = self._frame_count - 1;

            if last_index <= frame {
                frame %= last_index;
            }

            loop {
                if (0 == frame && current_time <= self._frame_times[frame]) || (self._frame_times[frame] <= current_time && current_time <= self._frame_times[frame + 1]) {
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
                node.update_animation_node(frame, &mut animation_transforms[index]);
                animation_transforms[index] = &(*parent_matrix) * &animation_transforms[index];
                self.update_hierarchical_animation_transform(frame, *bone, &animation_transforms[index], animation_transforms);
                animation_transforms[index] = &animation_transforms[index] * &(*node._bone)._inv_bind_matrix;
            }
        }
    }

    pub fn update_animation_transforms(&self, frame: f32, animation_transforms: &mut [Matrix4<f32>]) {
        unsafe {
            if (*self._root_node)._precomputed_root_matrix {
                for (index, node) in self._nodes.iter().enumerate() {
                    node.update_animation_node(frame, &mut animation_transforms[index]);
                    // Why multipication inv_bind_matrix? let's suppose to the bone is T pose. Since the vertices do not move,
                    // the result must be an identity. Therefore, inv_bind_matrix is the inverse of T pose transform.
                    if false == node._precomputed_combine_inv_bind_matrix {
                        animation_transforms[index] = &animation_transforms[index] * &(*node._bone)._inv_bind_matrix;
                    }
                }
            } else {
                for bone in (*self._skeleton)._hierachy.iter() {
                    let index: usize = (**bone)._index;
                    let node = &self._nodes[index];
                    node.update_animation_node(frame, &mut animation_transforms[index]);
                    self.update_hierarchical_animation_transform(frame, *bone, &animation_transforms[index], animation_transforms);
                    animation_transforms[index] = &animation_transforms[index] * &(*node._bone)._inv_bind_matrix;
                }
            }
        }
    }
}

impl AnimationNodeData {
    pub fn create_animation_node_data(bone: *const BoneData, animation_node_create_info: &AnimationNodeCreateInfo) -> AnimationNodeData {
        AnimationNodeData {
            _name: animation_node_create_info._name.clone(),
            _precomputed_root_matrix: animation_node_create_info._precomputed_root_matrix,
            _precomputed_combine_inv_bind_matrix: animation_node_create_info._precomputed_combine_inv_bind_matrix,
            _target: animation_node_create_info._target.clone(),  // bone name
            _frame_times: animation_node_create_info._times.clone(),
            _locations: animation_node_create_info._locations.clone(),
            _rotations: animation_node_create_info._rotations.clone(),
            _scales: animation_node_create_info._scales.clone(),
            _interpoations: animation_node_create_info._interpoations.clone(),
            _in_tangents: animation_node_create_info._in_tangents.clone(),
            _out_tangents: animation_node_create_info._out_tangents.clone(),
            _bone: bone,
            _frame_count: animation_node_create_info._times.len(),
        }
    }

    pub fn update_animation_node(&self, frame: f32, transform: &mut Matrix4<f32>) {
        if (frame as usize) < self._frame_count {
            let rate = frame.fract();
            let frame: usize = (frame as usize) % self._frame_count;
            let next_frame: usize = (frame + 1) % self._frame_count;
            if frame < self._frame_count {
                let rotation = glm::quat_slerp(&self._rotations[frame], &self._rotations[next_frame], rate);
                let location = glm::lerp(&self._locations[frame], &self._locations[next_frame], rate);
                let scale = glm::lerp(&self._scales[frame], &self._scales[next_frame], rate);
                transform.copy_from(&math::quaternion_to_matrix(&rotation));
                math::matrix_scale(transform, scale.x, scale.y, scale.z);
                transform.set_column(3, &Vector4::new(location.x, location.y, location.z, 1.0));
            }
        }
    }
}