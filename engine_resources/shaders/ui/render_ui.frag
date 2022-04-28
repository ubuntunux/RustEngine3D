#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = INSTANCE_ID_LOCATION) flat in uint vs_out_instanceIndex;

layout (location = 0) out vec4 fs_output;

void main()
{
    UIRenderData ui_render_data = ui_render_datas[vs_out_instanceIndex];

    if(gl_FragCoord.x < ui_render_data._ui_parent_render_area.x || ui_render_data._ui_parent_render_area.z <= gl_FragCoord.x ||
       gl_FragCoord.y < ui_render_data._ui_parent_render_area.y || ui_render_data._ui_parent_render_area.w <= gl_FragCoord.y)
    {
        discard;
    }

    const float ui_round = ui_render_data._ui_round;
    const vec2 half_size = (ui_render_data._ui_render_area.zw - ui_render_data._ui_render_area.xy) * 0.5;
    const vec2 ui_center = (ui_render_data._ui_render_area.xy + ui_render_data._ui_render_area.zw) * 0.5;
    const vec2 offset_from_center = gl_FragCoord.xy - ui_center;
    const vec2 dist_from_outer = max(vec2(0.0), half_size - abs(offset_from_center));
    vec4 color = vs_output._color;

    // disacrd border
    if(0.0 != ui_round)
    {
        if(dist_from_outer.x < ui_round && dist_from_outer.y < ui_round)
        {
            vec2 round_offset = dist_from_outer - vec2(ui_round);
            if((ui_round * ui_round) < dot(round_offset, round_offset))
            {
                discard;
            }
        }
    }

    // texture color
    if(check_flags_any(UI_RENDER_FLAG_RENDER_TEXTURE, ui_render_data._ui_render_flags))
    {
        vec4 texture_color = texture(texture_normal, vs_output._texcoord);
        texture_color.xyz = pow(texture_color.xyz, vec3(2.2));
        color *= texture_color;
    }

    // render text or render solid
    if(check_flags_any(UI_RENDER_FLAG_RENDER_TEXT, ui_render_data._ui_render_flags))
    {
        vec4 font_texture_color = texture(texture_font, vs_output._texcoord);
        font_texture_color.xyz = pow(font_texture_color.xyz, vec3(2.2));
        font_texture_color.w = distance_field_font_opacity(font_texture_color.w);
        color *= font_texture_color;
    }
    else
    {
        if(0.0 != ui_render_data._ui_border)
        {
            float inner_ui_round = ui_round - ui_render_data._ui_border;
            if(dist_from_outer.x < ui_render_data._ui_border || dist_from_outer.y < ui_render_data._ui_border)
            {
                color = vs_output._border_color;
            }
            else if(dist_from_outer.x < ui_round && dist_from_outer.y < ui_round)
            {
                vec2 round_offset = dist_from_outer - vec2(ui_round);
                if((inner_ui_round * inner_ui_round) < dot(round_offset, round_offset))
                {
                    color = vs_output._border_color;
                }
            }
        }
    }

    color.w = saturate(color.w * ui_render_data._ui_opacity);
    fs_output = color;
}