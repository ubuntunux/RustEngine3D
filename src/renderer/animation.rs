{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NegativeLiterals       #-}
{-# LANGUAGE OverloadedStrings      #-}


module HulkanEngine3D.Render.Animation where

import GHC.Generics (Generic)
import Data.Aeson


data BoneData = BoneData
    {
    } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data SkeletonData = SkeletonData
    {
    } deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

--class Bone:
--    def __init__(self, name, index, depth, inv_bind_matrix):
--        # v += {[(v * BindShapeMatrix) * InvBindMatrix * JointMatrix(animation)] * JointWeight}
--        self.name = name
--        self.transform = TransformObject()
--        self.inv_bind_matrix = inv_bind_matrix
--        self.parent = None
--        self.children = []
--        self.index = index
--        self.depth = depth
--        # print("\t" * depth, self.name, self.index)
--
--    def set_parent(self, parent_bone):
--        self.parent = parent_bone
--
--    def add_child(self, child_bone):
--        child_bone.set_parent(self)
--        self.children.append(child_bone)
--
--
--class Skeleton:
--    def __init__(self, index, **skeleton_data):
--        self.name = skeleton_data.get('name', '')
--        self.index = index
--
--        self.bone_names = skeleton_data.get('bone_names', [])
--        self.bones = [None, ] * len(self.bone_names)
--        self.hierachy = []
--
--        inv_bind_matrices = skeleton_data.get('inv_bind_matrices', [])
--
--        def build_bone(hierachy, parent_bone, depth):
--            for bone_name in hierachy:
--                if bone_name in self.bone_names:
--                    index = self.bone_names.index(bone_name)
--                    bone = Bone(
--                        name=bone_name,
--                        index=index,
--                        depth=depth,
--                        inv_bind_matrix=inv_bind_matrices[index]
--                    )
--                    self.bones[index] = bone
--                    if parent_bone is None:
--                        # add root
--                        self.hierachy.append(bone)
--                    else:
--                        parent_bone.add_child(bone)
--                    # recursive build bone
--                    build_bone(hierachy[bone_name], bone, depth+1)
--        build_bone(skeleton_data.get('hierachy', {}), None, 0)


--class Animation:
--    def __init__(self, name, index, skeleton, animation_data):
--        self.name = name
--        self.index = index
--        self.skeleton = skeleton
--        self.frame_count = 0
--        self.frame_times = []
--        self.animation_length = 0.0
--        self.nodes = []  # order by bone index
--        for i, animation_node_data in enumerate(animation_data):
--            animation_node = AnimationNode(self.skeleton.bones[i], animation_node_data)
--            frame_count = len(animation_node.frame_times)
--            if self.frame_count < frame_count:
--                self.frame_count = frame_count
--                self.frame_times = copy.copy(animation_node.frame_times)
--            self.nodes.append(animation_node)
--
--        self.root_node = self.nodes[0] if 0 < len(self.nodes) else None
--
--        if 0 < self.frame_count:
--            self.animation_length = max(self.frame_times)
--
--        self.last_frame = 0.0
--
--        # just update animation transforms
--        self.animation_transforms = np.array([Matrix4() for i in range(len(self.nodes))], dtype=np.float32)
--        self.get_animation_transforms(0.0)
--
--    def get_time_to_frame(self, current_frame, current_time):
--        if 1 < self.frame_count:
--            frame = int(current_frame)
--            last_index = self.frame_count - 1
--
--            if last_index <= frame:
--                frame %= last_index
--
--            while True:
--                if (0 == frame and current_time <= self.frame_times[frame]) or (self.frame_times[frame] <= current_time <= self.frame_times[frame + 1]):
--                    break
--                frame = (frame + 1) % last_index
--
--            frame_time = self.frame_times[frame]
--            next_frame_time = self.frame_times[frame + 1]
--            ratio = (current_time - frame_time) / (next_frame_time - frame_time)
--            return float(frame) + ratio
--        return 0.0
--
--    def get_animation_transforms(self, frame=0.0):
--        if self.last_frame == frame:
--            return self.animation_transforms
--        else:
--            self.last_frame = frame
--
--            if self.root_node.precompute_parent_matrix:
--                for i, node in enumerate(self.nodes):
--                    self.animation_transforms[i][...] = node.get_transform(frame)
--            else:
--                def animation(parent_bone, parent_matrix):
--                    for bone in parent_bone.children:
--                        node = self.nodes[bone.index]
--                        node.get_transform(frame)
--                        transform = self.animation_transforms[bone.index]
--                        transform[...] = np.dot(node.transform, parent_matrix)
--                        animation(bone, transform)
--
--                for bone in self.skeleton.hierachy:
--                    node = self.nodes[bone.index]
--                    transform = self.animation_transforms[bone.index]
--                    transform[...] = node.get_transform(frame)
--                    animation(bone, transform)
--            return self.animation_transforms
--
--
--class AnimationNode:
--    def __init__(self, bone, animation_node_data):
--        self.name = animation_node_data.get('name', '')
--        self.bone = bone
--        self.precompute_parent_matrix = animation_node_data.get('precompute_parent_matrix', False)
--        self.precompute_inv_bind_matrix = animation_node_data.get('precompute_inv_bind_matrix', False)
--        self.target = animation_node_data.get('target', '')  # bone name
--        self.frame_times = animation_node_data.get('times', [])
--        self.locations = animation_node_data.get('locations', [])
--        self.rotations = animation_node_data.get('rotations', [])
--        self.scales = animation_node_data.get('scales', [])
--        self.interpoations = animation_node_data.get('interpoations', [])
--        self.in_tangents = animation_node_data.get('in_tangents', [])
--        self.out_tangents = animation_node_data.get('out_tangents', [])
--
--        self.frame_count = len(self.frame_times)
--        self.last_frame = -1.0
--        self.transform = MATRIX4_IDENTITY.copy()
--        # just update transform
--        self.get_transform(0.0)
--
--    def get_transform(self, frame=0.0):
--        if self.last_frame == frame or self.frame_count == 0:
--            return self.transform
--        else:
--            self.last_frame = frame
--
--            rate = frame - int(frame)
--            frame = int(frame) % self.frame_count
--            next_frame = (frame + 1) % self.frame_count
--
--            if frame < self.frame_count:
--                rotation = slerp(self.rotations[frame], self.rotations[next_frame], rate)
--                # rotation = normalize(lerp(self.rotations[frame], self.rotations[next_frame], rate))
--                location = lerp(self.locations[frame], self.locations[next_frame], rate)
--                scale = lerp(self.scales[frame], self.scales[next_frame], rate)
--                quaternion_to_matrix(rotation, self.transform)
--                matrix_scale(self.transform, *scale)
--                self.transform[3, 0:3] = location
--
--                # Why multipication inv_bind_matrix? let's suppose to the bone is T pose. Since the vertices do not move,
--                # the result must be an identity. Therefore, inv_bind_matrix is ​​the inverse of T pose transform.
--                if not self.precompute_inv_bind_matrix:
--                    self.transform[...] = np.dot(self.bone.inv_bind_matrix, self.transform)
--            return self.transform
--
