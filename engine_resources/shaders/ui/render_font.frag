#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "render_font_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = 0) out vec4 fs_output;

void main()
{
    vec4 font_sample = textureLod(texture_font, vs_output.texcoord, 0.0);
    float opacity = distance_field_font_opacity(font_sample);
    fs_output = vec4(1.0, 1.0, 1.0, opacity);
}