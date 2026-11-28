#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "render_ui_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = INSTANCE_ID_LOCATION) flat in uint vs_out_instanceIndex;

layout (location = 0) out vec4 fs_output;

float map_nine_slice_axis(float p, float dst_len, float dst_b0, float dst_b1, float src_len, float src_b0, float src_b1)
{
    float src_p = p;
    if(p < dst_b0)
    {
        src_p = (dst_b0 > 0.0) ? (p / dst_b0) * src_b0 : 0.0;
    }
    else if(p > dst_len - dst_b1)
    {
        src_p = (dst_b1 > 0.0) ? (src_len - ((dst_len - p) / dst_b1) * src_b1) : src_len;
    }
    else
    {
        float dst_mid = max(dst_len - dst_b0 - dst_b1, 1e-5);
        float t = (p - dst_b0) / dst_mid;
        src_p = mix(src_b0, src_len - src_b1, t);
    }
    return src_p / src_len;
}

void main()
{
    UIRenderData ui_render_data = ui_render_data_list[vs_out_instanceIndex];

    if(check_flags_any(UI_RENDER_FLAG_ENABLE_RENDERABLE_AREA, ui_render_data._ui_render_flags))
    {
        // disacrd with parent round
        const float ui_round = ui_render_data._ui_renderable_area_round;
        const vec2 half_size = (ui_render_data._ui_renderable_area.zw - ui_render_data._ui_renderable_area.xy) * 0.5 + max(0.0, ui_render_data._ui_renderable_area_border - 1.0);
        const vec2 ui_center = (ui_render_data._ui_renderable_area.xy + ui_render_data._ui_renderable_area.zw) * 0.5;
        const vec2 offset_from_center = gl_FragCoord.xy - ui_center;
        const vec2 dist_from_outer = max(vec2(0.0), half_size - abs(offset_from_center));
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

        if(gl_FragCoord.x < ui_render_data._ui_renderable_area.x || ui_render_data._ui_renderable_area.z <= gl_FragCoord.x ||
        gl_FragCoord.y < ui_render_data._ui_renderable_area.y || ui_render_data._ui_renderable_area.w <= gl_FragCoord.y)
        {
            discard;
        }
    }

    // disacrd round
    const float ui_round = ui_render_data._ui_round;
    const vec2 half_size = (ui_render_data._ui_render_area.zw - ui_render_data._ui_render_area.xy) * 0.5;
    const vec2 ui_center = (ui_render_data._ui_render_area.xy + ui_render_data._ui_render_area.zw) * 0.5;
    const vec2 offset_from_center = gl_FragCoord.xy - ui_center;
    const vec2 dist_from_outer = max(vec2(0.0), half_size - abs(offset_from_center));
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
    vec4 color = vs_output._color;
    if(check_flags_any(UI_RENDER_FLAG_RENDER_TEXTURE, ui_render_data._ui_render_flags))
    {
        vec2 texcoord = vs_output._texcoord * pushConstant._uv_size + pushConstant._uv_offset;
        if(check_flags_any(UI_RENDER_FLAG_ENABLE_NINE_SLICE, ui_render_data._ui_render_flags))
        {
            const vec2 dst_size = max(ui_render_data._ui_render_area.zw - ui_render_data._ui_render_area.xy, vec2(1.0));
            const vec2 src_size = max(vec2(textureSize(sampler2D(texture_color, g_samplers[SAMPLER_LINEAR_CLAMP]), 0)), vec2(1.0));
            const float inv_dpi = 1.0 / max(ui_render_data._ui_dpi_scale, 0.001);
            vec4 dst_pad = max(ui_render_data._ui_padding, vec4(0.0));
            vec4 src_pad = dst_pad * inv_dpi;

            float scale_x = (dst_pad.x + dst_pad.z > dst_size.x && (dst_pad.x + dst_pad.z) > 0.0) ? (dst_size.x / (dst_pad.x + dst_pad.z)) : 1.0;
            float scale_y = (dst_pad.y + dst_pad.w > dst_size.y && (dst_pad.y + dst_pad.w) > 0.0) ? (dst_size.y / (dst_pad.y + dst_pad.w)) : 1.0;
            dst_pad.xz *= scale_x;
            dst_pad.yw *= scale_y;

            float src_scale_x = (src_pad.x + src_pad.z >= src_size.x && (src_pad.x + src_pad.z) > 0.0) ? ((src_size.x - 1.0) / (src_pad.x + src_pad.z)) : 1.0;
            float src_scale_y = (src_pad.y + src_pad.w >= src_size.y && (src_pad.y + src_pad.w) > 0.0) ? ((src_size.y - 1.0) / (src_pad.y + src_pad.w)) : 1.0;
            src_pad.xz *= src_scale_x;
            src_pad.yw *= src_scale_y;

            const vec2 frag_local = texcoord * dst_size;
            texcoord.x = map_nine_slice_axis(frag_local.x, dst_size.x, dst_pad.x, dst_pad.z, src_size.x, src_pad.x, src_pad.z);
            texcoord.y = map_nine_slice_axis(frag_local.y, dst_size.y, dst_pad.y, dst_pad.w, src_size.y, src_pad.y, src_pad.w);
        }
        vec2 sample_uv = texcoord;
        bool out_of_bounds = false;
        const bool is_clamped = check_flags_any(UI_RENDER_FLAG_CLAMP_TEXTURE, ui_render_data._ui_render_flags);
        if(is_clamped)
        {
            out_of_bounds = (texcoord.x < 0.0 || 1.0 < texcoord.x || texcoord.y < 0.0 || 1.0 < texcoord.y);
            vec2 half_texel = 0.5 / max(vec2(textureSize(sampler2D(texture_color, g_samplers[SAMPLER_LINEAR_CLAMP]), 0)), vec2(1.0));
            sample_uv = clamp(texcoord, half_texel, 1.0 - half_texel);
        }
        vec4 sampled_color = is_clamped
            ? textureLod(sampler2D(texture_color, g_samplers[SAMPLER_LINEAR_CLAMP]), sample_uv, 0.0)
            : textureLod(sampler2D(texture_color, g_samplers[SAMPLER_LINEAR_REPEAT]), sample_uv, 0.0);
        sampled_color.xyz = pow(sampled_color.xyz, vec3(2.2));

        if(out_of_bounds)
            sampled_color.a = 0.0;
        color *= sampled_color;
    }

    // render text or render solid
    if(check_flags_any(UI_RENDER_FLAG_RENDER_TEXT, ui_render_data._ui_render_flags))
    {
        vec4 font_texture_color = textureLod(sampler2D(texture_font, g_samplers[SAMPLER_LINEAR_CLAMP]), vs_output._texcoord, 0.0);
        float opacity = distance_field_font_opacity(font_texture_color);
        color.w *= opacity;
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
