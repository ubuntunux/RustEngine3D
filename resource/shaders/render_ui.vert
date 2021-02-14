#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"
#include "scene_constants.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in vec3 vs_in_position;

layout (location = 0) out VERTEX_OUTPUT vs_output;
layout (location = INSTANCE_ID_LOCATION) flat out uint vs_out_instanceIndex;

void main()
{
    vs_out_instanceIndex = gl_InstanceIndex;
    UIInstanceData ui_instance_data = ui_instance_data[gl_InstanceIndex];

    vec4 color = uint_color_to_float_color(ui_instance_data._ui_color);
    vec4 border_color = uint_color_to_float_color(ui_instance_data._ui_border_color);

    vs_output._color = color;
    vs_output._border_color = border_color;
    vs_output._texcoord = mix(ui_instance_data._ui_texcoord.xy, ui_instance_data._ui_texcoord.zw, vs_in_position.xy + 0.5);

    vec2 position = (ui_instance_data._ui_pos + vs_in_position.xy * ui_instance_data._ui_size) * pushConstant._inv_canvas_size;
    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
}