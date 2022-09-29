#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_object_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;
#if (RenderObjectType_Skeletal == RenderObjectType)
layout (location = 5) in uvec4 inBoneIndices;
layout (location = 6) in vec4 inBoneWeights;
#endif
layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    vec4 position = vec4(0.0);
    vec4 prev_position = vec4(0.0);
    vec3 vertex_normal = vec3(0.0);
    vec3 vertex_tangent = vec3(0.0);

    const uint local_matrix_prev_offset = pushConstant._transform_matrix_offset;
    const uint local_matrix_offset = local_matrix_prev_offset + 1;
    const uint prev_bone_matrix_offset = local_matrix_offset + 1;
    const uint bone_matrix_offset = prev_bone_matrix_offset + pushConstant._bone_count;

#if (RenderObjectType_Skeletal == RenderObjectType)
    if (0 < pushConstant._bone_count)
    {
        for(int i = 0; i < MAX_BONES_PER_VERTEX; ++i)
        {
            const float boneWeight = inBoneWeights[i];
            if(0.0 < boneWeight)
            {
                prev_position += (transform_matrices[prev_bone_matrix_offset + int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * boneWeight;
                position += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * boneWeight;
                vertex_normal += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inNormal, 0.0)).xyz * boneWeight;
                vertex_tangent += (transform_matrices[bone_matrix_offset + int(inBoneIndices[i])] * vec4(inTangent, 0.0)).xyz * boneWeight;
            }
        }
        position /= position.w;
        prev_position /= prev_position.w;
    }
    else
    {
        position = vec4(inPosition, 1.0);
        prev_position = vec4(inPosition, 1.0);
        vertex_normal = inNormal;
        vertex_tangent = inTangent;
    }
#else
    position = vec4(inPosition, 1.0);
    prev_position = vec4(inPosition, 1.0);
    vertex_normal = inNormal;
    vertex_tangent = inTangent;
#endif
    vertex_normal = normalize(vertex_normal);
    vertex_tangent = normalize(vertex_tangent);

    mat4 localMatrix = transform_matrices[local_matrix_offset];
    mat4 localMatrixPrev = transform_matrices[local_matrix_prev_offset];

    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;
    localMatrixPrev[3].xyz -= view_constants.CAMERA_POSITION_PREV;

    vec3 relative_pos = (localMatrix * position).xyz;
    vec3 relative_pos_prev = (localMatrixPrev * prev_position).xyz;

#if (RenderMode_GBuffer == RenderMode || RenderMode_Forward == RenderMode)
    vs_output.projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV_JITTER * vec4(relative_pos_prev, 1.0);
    vs_output.projection_pos = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(relative_pos, 1.0);
#elif (RenderMode_Shadow == RenderMode)
    vs_output.projection_pos = light_constants.SHADOW_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#elif (RenderMode_CaptureHeightMap == RenderMode)
    vs_output.projection_pos = view_constants.CAPTURE_HEIGHT_MAP_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#endif
    gl_Position = vs_output.projection_pos;

    vs_output.relative_position = relative_pos;
    vs_output.color = inColor;
    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vec3 bitangent = cross(vertex_tangent, vertex_normal);
    vs_output.tangent_to_world = mat3(localMatrix) * mat3(vertex_tangent, bitangent, vertex_normal);
    vs_output.texCoord = inTexCoord;
}
