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
    fs_output = texture(texture_font, vs_output.texcoord);
    fs_output.w = distance_field_font_opacity(fs_output.w);
}