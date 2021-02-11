#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_font_common.glsl"

layout (location = 0) in vec3 vs_in_position;
layout (location = 1) in vec4 vs_in_font_infos; // instancing data

layout (location = 0) out VERTEX_OUTPUT vs_output;

void main()
{
    vec2 inv_texture_size = 1.0 / textureSize(texture_font, 0).xy;
    vec2 font_texcoord_size = 1.0 / pushConstant._count_of_side - inv_texture_size;

    vec2 ratio = vec2(pushConstant._font_size) * pushConstant._inv_canvas_size;
    vec2 texcoord = vs_in_position.xy * 0.5 + 0.5;

    vs_output.texcoord = vs_in_font_infos.zw + texcoord * font_texcoord_size + inv_texture_size * 0.5;

    float column = vs_in_font_infos.x;
    float row = vs_in_font_infos.y;
    vec2 position;
    position.x = (column + texcoord.x) * pushConstant._font_size * pushConstant._inv_canvas_size.x;
    position.y = (texcoord.y - row) * pushConstant._font_size * pushConstant._inv_canvas_size.y;
    position.xy += pushConstant._offset * pushConstant._inv_canvas_size.xy;

    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
    //gl_Position = vec4(vs_in_position.xy * 0.0025 + position * 2.0 - 1.0 + vec2(0.5, -0.5), 0.0, 1.0);
}