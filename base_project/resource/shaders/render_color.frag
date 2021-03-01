#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout( push_constant ) uniform PushConstant_Color
{
    vec4 _color;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    outColor = pushConstant._color;
}
