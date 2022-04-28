#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout(location = 0) out VERTEX_OUTPUT vs_output;

void main() {
    // NOTE: invert y cause convert world to ndc
    gl_Position = vec4(inPosition.x, -inPosition.y, inPosition.z, 1.0);
    vs_output.uv = inTexCoord;
    vs_output.eye_ray = (view_constants.INV_VIEW_ORIGIN_PROJECTION * vec4(inTexCoord * 2.0 - 1.0, -1.0, 1.0)).xyz;
}