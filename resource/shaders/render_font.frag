#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_font_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = 0) out vec4 fs_output;

void main()
{
    fs_output = texture(texture_font, vs_output.texcoord);
    //fs_output.w = pow(texture(texture_font, vs_output.texcoord).w, 0.1);
    fs_output.w = smoothstep(0.0, 1.0, fs_output.w);
}