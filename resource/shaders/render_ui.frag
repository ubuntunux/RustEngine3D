#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"
#include "scene_constants.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = INSTANCE_ID_LOCATION) flat in uint vs_out_instanceIndex;

layout (location = 0) out vec4 fs_output;

void main()
{
    UIInstanceData ui_instance_data = ui_instance_data[vs_out_instanceIndex];

    const vec2 offset_from_center = gl_FragCoord.xy - ui_instance_data._ui_pos;
    const vec2 dist_from_center = abs(offset_from_center);
    const vec2 half_size = ui_instance_data._ui_size * 0.5;
    float round_opacity = 1.0;
    vec4 color = vs_output._color;

    if(0.0 != ui_instance_data._ui_round)
    {
        vec2 round_thickness = dist_from_center - max(vec2(0.0), half_size - vec2(ui_instance_data._ui_round));
        if(0.0 < round_thickness.x && 0.0 < round_thickness.y)
        {
            float round_dist = dot(round_thickness, round_thickness) - ui_instance_data._ui_round * ui_instance_data._ui_round;
            if(0.0 < floor(round_dist))
            {
                round_opacity = 0.0;
            }
        }
    }

    if(0.0 != ui_instance_data._ui_border)
    {
        vec2 border_thickness = dist_from_center - max(vec2(0.0), half_size - vec2(ui_instance_data._ui_border));
        vec2 border_thickness_with_round = border_thickness + vec2(ui_instance_data._ui_round);
        if(0.0 < border_thickness_with_round.x && 0.0 < border_thickness_with_round.y)
        {
            float border_with_round_opacity = dot(border_thickness_with_round, border_thickness_with_round) - ui_instance_data._ui_round * ui_instance_data._ui_round;
            if(0.0 < floor(border_with_round_opacity))
            {
                color = vs_output._border_color;
            }
        }
        else if(0.0 < border_thickness.x || 0.0 < border_thickness.y)
        {
            color = vs_output._border_color;
        }

    }

    color.w *= round_opacity;
    fs_output = color;
}