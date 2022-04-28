#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "render_font_common.glsl"

layout (location = 0) in vec3 vs_in_position;
layout (location = 0) out VERTEX_OUTPUT vs_output;

void main()
{
    FontInstanceData font_instance_data = font_instance_data[gl_InstanceIndex];
    vec2 inv_texture_size = 1.0 / textureSize(texture_font, 0).xy;
    vec2 ratio = pushConstant._font_size * pushConstant._inv_canvas_size;
    vec2 texcoord = vs_in_position.xy + 0.5;

    vs_output.texcoord = font_instance_data._font_texcoord + texcoord * (1.0 / pushConstant._count_of_side - inv_texture_size) + inv_texture_size * 0.5;

    const float column = font_instance_data._font_column;
    const float row = font_instance_data._font_row;
    const float ui_render_font_padding_ratio = 0.9;
    vec2 position;
    position.x = (column * ui_render_font_padding_ratio + texcoord.x) * ratio.x;
    position.y = (texcoord.y - row * ui_render_font_padding_ratio) * ratio.y;
    position.xy += pushConstant._offset * pushConstant._inv_canvas_size.xy;

    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
}