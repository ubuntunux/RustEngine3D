#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outAlbedo;
layout(location = 1) out vec4 outMaterial;
layout(location = 2) out vec4 outNormal;
layout(location = 3) out vec2 outVelocity;
void main() {
    outAlbedo = vec4(0.0);
    outMaterial = vec4(0.0);
    outNormal = vec4(0.0);
    outVelocity = vec2(0.0);
}
