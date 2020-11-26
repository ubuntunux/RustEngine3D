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

#if (RenderObjectType_Skeletal == RenderObjectType)
    for(int i = 0; i < MAX_BONES_PER_VERTEX; ++i)
    {
        prev_position += (prev_bone_matrices[int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * inBoneWeights[i];
        position += (bone_matrices[int(inBoneIndices[i])] * vec4(inPosition, 1.0)) * inBoneWeights[i];
        vertex_normal += (bone_matrices[int(inBoneIndices[i])] * vec4(inNormal, 0.0)).xyz * inBoneWeights[i];
        vertex_tangent += (bone_matrices[int(inBoneIndices[i])] * vec4(inTangent, 0.0)).xyz * inBoneWeights[i];
    }
    position /= position.w;
    prev_position /= prev_position.w;
#else
    position = vec4(inPosition, 1.0);
    prev_position = vec4(inPosition, 1.0);
    vertex_normal = inNormal;
    vertex_tangent = inTangent;
#endif
    vertex_normal = normalize(vertex_normal);
    vertex_tangent = normalize(vertex_tangent);

    mat4 localMatrix = pushConstant.localMatrix;
    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;

    vec3 relative_pos = (localMatrix * position).xyz;

#if (RenderMode_Common == RenderMode)
    vec3 relative_pos_prev = relative_pos + view_constants.CAMERA_POSITION - view_constants.CAMERA_POSITION_PREV;
    vs_output.projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV * vec4(relative_pos_prev, 1.0);
    vs_output.projection_pos = view_constants.VIEW_ORIGIN_PROJECTION * vec4(relative_pos, 1.0);
#elif (RenderMode_Shadow == RenderMode)
    vs_output.projection_pos = light_constants.SHADOW_VIEW_PROJECTION * vec4(relative_pos + view_constants.CAMERA_POSITION, 1.0);
#endif
    gl_Position = vs_output.projection_pos;

    vs_output.color = inColor;
    vec3 bitangent = cross(vertex_tangent, vertex_normal);

    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vs_output.tangent_to_world = mat3(pushConstant.localMatrix) * mat3(vertex_tangent, bitangent, vertex_normal);

    vs_output.texCoord = inTexCoord;
}
