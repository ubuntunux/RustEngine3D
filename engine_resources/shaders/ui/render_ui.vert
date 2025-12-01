#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/utility.glsl"
#include "../common/scene_constants.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in vec3 vs_in_position;

layout (location = 0) out VERTEX_OUTPUT vs_output;
layout (location = INSTANCE_ID_LOCATION) flat out uint vs_out_instanceIndex;

void main()
{
    uint instance_id = gl_InstanceIndex + pushConstant._instance_id_offset;
    vs_out_instanceIndex = instance_id;
    UIRenderData ui_render_data = ui_render_data_list[instance_id];

    vec4 color = uint_color_to_float_color(ui_render_data._ui_color);
    vec4 border_color = uint_color_to_float_color(ui_render_data._ui_border_color);
    vec2 position = mix(ui_render_data._ui_render_area.xy, ui_render_data._ui_render_area.zw, vs_in_position.xy) * pushConstant._inv_canvas_size;

    vec2 texcoord = vs_in_position.xy;
    if(ui_render_data._ui_rotation != 0.0)
    {
        const vec2 offset = vs_in_position.xy - vec2(0.5, 0.5);
        const float rad = ui_render_data._ui_rotation / 180.0 * 3.141592;
        const float c = cos(rad);
        const float s = sin(rad);
        texcoord = vec2(0.5, 0.5) + offset * c + vec2(offset.y, -offset.x) * s;
    }
    texcoord = mix(ui_render_data._ui_texcoord.xy, ui_render_data._ui_texcoord.zw, texcoord);

    if(check_flags_any(UI_RENDER_FLAG_TOUCHED, ui_render_data._ui_render_flags))
    {
        color.xyz = mix(color.xyz, vec3(0.5, 0.5, 1.0), 0.75);
        border_color.xyz = mix(border_color.xyz, vec3(0.5, 0.5, 1.0), 0.75);
    }

    vs_output._color = color;
    vs_output._border_color = border_color;
    vs_output._texcoord = texcoord;
    gl_Position = vec4(position * 2.0 - 1.0, 0.0, 1.0);
}