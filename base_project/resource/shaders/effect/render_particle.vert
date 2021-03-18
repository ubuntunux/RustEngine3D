#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "render_particle_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;
layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    vec4 position = vec4(0.0);
    vec4 prev_position = vec4(0.0);
    vec3 vertex_normal = vec3(0.0);
    vec3 vertex_tangent = vec3(0.0);
    position = vec4(inPosition, 1.0);
    prev_position = vec4(inPosition, 1.0);
    vertex_normal = inNormal;
    vertex_tangent = inTangent;
    vertex_normal = normalize(vertex_normal);
    vertex_tangent = normalize(vertex_tangent);

    mat4 localMatrix = pushConstant._localMatrix;
    localMatrix[3].xyz -= view_constants.CAMERA_POSITION;

    vec3 relative_pos = (localMatrix * position).xyz;
    vec3 relative_pos_prev = (localMatrix * prev_position).xyz + (view_constants.CAMERA_POSITION - view_constants.CAMERA_POSITION_PREV);

    vs_output.projection_pos_prev = view_constants.VIEW_ORIGIN_PROJECTION_PREV_JITTER * vec4(relative_pos_prev, 1.0);
    vs_output.projection_pos = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(relative_pos, 1.0);
    gl_Position = vs_output.projection_pos;

    vs_output.relative_position = relative_pos;
    vs_output.color = inColor;
    // Note : Normalization is very important because tangent_to_world may have been scaled..
    vec3 bitangent = cross(vertex_tangent, vertex_normal);
    vs_output.tangent_to_world = mat3(localMatrix) * mat3(vertex_tangent, bitangent, vertex_normal);
    vs_output.texCoord = inTexCoord;
}
