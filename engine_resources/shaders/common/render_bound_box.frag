#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "render_bound_box_common.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

void main() {
    vec2 screen_texCoord = (vs_output.projection_pos.xy / vs_output.projection_pos.w) * 0.5 + 0.5;
    float scene_depth = textureLod(texture_scene_depth, screen_texCoord, 0.0).x;
    if(gl_FragCoord.z < scene_depth)
    {
        discard;
    }
    outColor = vs_output.color;
}