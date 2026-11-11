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
    vec2 position = (mix(font_instance_data._font_render_area.xy, font_instance_data._font_render_area.zw, vs_in_position.xy) + pushConstant._offset) * pushConstant._inv_canvas_size;
    vec2 texcoord = mix(font_instance_data._font_texcoord.xy, font_instance_data._font_texcoord.zw, vs_in_position.xy);

    vs_output.texcoord = texcoord;
    vs_output.font_offset = vec2(0.0);
    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
}
