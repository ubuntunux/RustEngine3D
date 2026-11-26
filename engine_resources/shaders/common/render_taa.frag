#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};

layout(binding = 2) uniform sampler2D texture_input;
layout(binding = 3) uniform sampler2D texture_prev_resolve;
layout(binding = 4) uniform sampler2D texture_velocity;
layout(binding = 5) uniform sampler2D texture_scene_depth;
layout(binding = 6) uniform sampler2D texture_hiz;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

vec3 RGBToYCoCg(vec3 rgb)
{
    float Y  = dot(rgb, vec3( 0.25,  0.50,  0.25));
    float Co = dot(rgb, vec3( 0.50,  0.00, -0.50));
    float Cg = dot(rgb, vec3(-0.25,  0.50, -0.25));
    return vec3(Y, Co, Cg);
}

vec3 YCoCgToRGB(vec3 ycocg)
{
    float Y  = ycocg.x;
    float Co = ycocg.y;
    float Cg = ycocg.z;
    return max(vec3(Y + Co - Cg, Y + Cg, Y - Co - Cg), vec3(0.0));
}

// Fast AABB clipping toward box center (Playdead / Salvi variance clipping)
vec3 ClipAABB(vec3 aabbMin, vec3 aabbMax, vec3 prevSample)
{
    vec3 p_clip = 0.5 * (aabbMax + aabbMin);
    vec3 e_clip = max(0.5 * (aabbMax - aabbMin), vec3(1e-5));

    vec3 v_clip = prevSample - p_clip;
    vec3 v_unit = v_clip / e_clip;
    vec3 a_unit = abs(v_unit);
    float ma_unit = max(a_unit.x, max(a_unit.y, a_unit.z));

    if (ma_unit > 1.0)
    {
        return p_clip + v_clip / ma_unit;
    }
    return prevSample;
}

// 3x3 closest-foreground depth dilation (Reverse-Z: larger device depth = closer to camera)
vec2 DilateClosestVelocity(vec2 jitteredUV)
{
    vec2 depth_size = vec2(textureSize(texture_scene_depth, 0));
    vec2 inv_depth_size = 1.0 / max(depth_size, vec2(1.0));
    vec2 min_uv = 0.5 * inv_depth_size;
    vec2 max_uv = 1.0 - min_uv;

    vec2 base_uv = clamp((floor(jitteredUV * depth_size) + 0.5) * inv_depth_size, min_uv, max_uv);
    float best_depth = -1.0;
    vec2 best_uv = base_uv;

    for (int y = -1; y <= 1; ++y)
    {
        for (int x = -1; x <= 1; ++x)
        {
            vec2 sample_uv = clamp(base_uv + vec2(x, y) * inv_depth_size, min_uv, max_uv);
            float d = textureLod(texture_scene_depth, sample_uv, 0.0).x;
            if (d > best_depth)
            {
                best_depth = d;
                best_uv = sample_uv;
            }
        }
    }

    return textureLod(texture_velocity, best_uv, 0.0).xy;
}

// 5-tap bilinear Catmull-Rom history filter with anti-ringing neighborhood clamp
vec3 SampleHistoryCatmullRom(vec2 uv, vec2 tex_size, vec2 inv_tex_size)
{
    vec2 sample_pos = uv * tex_size;
    vec2 tex_pos1 = floor(sample_pos - 0.5) + 0.5;
    vec2 f = sample_pos - tex_pos1;

    vec2 w0 = f * (-0.5 + f * (1.0 - 0.5 * f));
    vec2 w1 = 1.0 + f * f * (-2.5 + 1.5 * f);
    vec2 w2 = f * (0.5 + f * (2.0 - 1.5 * f));
    vec2 w3 = f * f * (-0.5 + 0.5 * f);

    vec2 w12 = w1 + w2;
    vec2 offset12 = w2 / max(w12, vec2(1e-5));

    vec2 min_uv = 0.5 * inv_tex_size;
    vec2 max_uv = 1.0 - min_uv;
    vec2 tex_pos0 = clamp((tex_pos1 - 1.0) * inv_tex_size, min_uv, max_uv);
    vec2 tex_pos3 = clamp((tex_pos1 + 2.0) * inv_tex_size, min_uv, max_uv);
    vec2 tex_pos12 = clamp((tex_pos1 + offset12) * inv_tex_size, min_uv, max_uv);

    vec3 c0 = max(textureLod(texture_prev_resolve, vec2(tex_pos12.x, tex_pos0.y), 0.0).rgb, vec3(0.0));
    vec3 c1 = max(textureLod(texture_prev_resolve, vec2(tex_pos0.x, tex_pos12.y), 0.0).rgb, vec3(0.0));
    vec3 c2 = max(textureLod(texture_prev_resolve, vec2(tex_pos12.x, tex_pos12.y), 0.0).rgb, vec3(0.0));
    vec3 c3 = max(textureLod(texture_prev_resolve, vec2(tex_pos3.x, tex_pos12.y), 0.0).rgb, vec3(0.0));
    vec3 c4 = max(textureLod(texture_prev_resolve, vec2(tex_pos12.x, tex_pos3.y), 0.0).rgb, vec3(0.0));

    float cw0 = w12.x * w0.y;
    float cw1 = w0.x * w12.y;
    float cw2 = w12.x * w12.y;
    float cw3 = w3.x * w12.y;
    float cw4 = w12.x * w3.y;

    vec3 filtered = (c0 * cw0 + c1 * cw1 + c2 * cw2 + c3 * cw3 + c4 * cw4) / max(cw0 + cw1 + cw2 + cw3 + cw4, 1e-5);
    vec3 min_tap = min(c2, min(min(c0, c1), min(c3, c4)));
    vec3 max_tap = max(c2, max(max(c0, c1), max(c3, c4)));
    return clamp(filtered, min_tap, max_tap);
}

vec4 ResolveTAAU(vec2 texCoord)
{
    vec2 input_size = vec2(textureSize(texture_input, 0));
    vec2 output_size = vec2(textureSize(texture_prev_resolve, 0));
    vec2 inv_input_size = 1.0 / max(input_size, vec2(1.0));
    vec2 inv_output_size = 1.0 / max(output_size, vec2(1.0));

    float upscale_ratio = output_size.x / max(input_size.x, 1.0);
    bool is_upscaling = upscale_ratio > 1.05;

    // Projection jitter shifts rasterized UV by +JITTER_OFFSET * 0.5
    vec2 jitter_uv = view_constants.JITTER_OFFSET * 0.5;
    vec2 jittered_uv = texCoord + jitter_uv;

    // Subpixel jitter-compensated 3x3 spatial reconstruction + YCoCg neighborhood statistics
    vec2 input_sample_pos = jittered_uv * input_size;
    vec2 base_texel = floor(input_sample_pos) + 0.5;
    vec2 min_input_uv = 0.5 * inv_input_size;
    vec2 max_input_uv = 1.0 - min_input_uv;

    float kernel_sharpness = is_upscaling ? 2.80 : 2.29;
    vec3 sum_color = vec3(0.0);
    float total_color_weight = 0.0;
    float sum_alpha = 0.0;
    float total_alpha_weight = 0.0;
    float alpha_min = 1.0;
    float alpha_max = 0.0;
    float min_dist_sq = 1e6;

    vec3 m1 = vec3(0.0);
    vec3 m2 = vec3(0.0);

    for (int y = -1; y <= 1; ++y)
    {
        for (int x = -1; x <= 1; ++x)
        {
            vec2 tap_pos = base_texel + vec2(x, y);
            vec2 tap_uv = clamp(tap_pos * inv_input_size, min_input_uv, max_input_uv);
            vec4 tap_rgba = textureLod(texture_input, tap_uv, 0.0);
            vec3 tap_rgb = clamp(tap_rgba.rgb, vec3(0.0), vec3(65000.0));
            float tap_alpha = saturate(tap_rgba.a);

            vec2 delta = tap_pos - input_sample_pos;
            float dist_sq = dot(delta, delta);
            min_dist_sq = min(min_dist_sq, dist_sq);

            float spatial_w = exp(-kernel_sharpness * dist_sq);
            float karis_w = 1.0 / (1.0 + get_luminance(tap_rgb));
            float w = spatial_w * karis_w;

            sum_color += tap_rgb * w;
            total_color_weight += w;

            sum_alpha += tap_alpha * spatial_w;
            total_alpha_weight += spatial_w;
            alpha_min = min(alpha_min, tap_alpha);
            alpha_max = max(alpha_max, tap_alpha);

            vec3 tap_ycocg = RGBToYCoCg(tap_rgb);
            m1 += tap_ycocg;
            m2 += tap_ycocg * tap_ycocg;
        }
    }

    vec3 curr_color = sum_color / max(total_color_weight, 1e-5);
    float curr_alpha = sum_alpha / max(total_alpha_weight, 1e-5);

    // Closest-foreground depth-dilated velocity reprojection
    vec2 velocity = DilateClosestVelocity(jittered_uv);
    vec2 reprojected_uv = texCoord - velocity;
    bool offscreen = any(lessThan(reprojected_uv, vec2(0.0))) || any(greaterThan(reprojected_uv, vec2(1.0)));

    if (offscreen || scene_constants.ELAPSED_FRAME <= 1u)
    {
        return vec4(curr_color, curr_alpha);
    }

    // Catmull-Rom history reprojection (anti-ringing clamped)
    vec3 prev_color = SampleHistoryCatmullRom(reprojected_uv, output_size, inv_output_size);
    float prev_alpha = saturate(textureLod(texture_prev_resolve, reprojected_uv, 0.0).a);

    // YCoCg variance clipping with velocity-adaptive tightness
    const float inv_sample_count = 1.0 / 9.0;
    vec3 mu = m1 * inv_sample_count;
    vec3 sigma = sqrt(max(m2 * inv_sample_count - mu * mu, vec3(0.0)));

    float vel_px = length(velocity * output_size);
    float motion_factor = saturate(vel_px / 12.0);
    float base_gamma = is_upscaling ? 1.25 : 1.15;
    float gamma = mix(base_gamma, 0.85, motion_factor);

    vec3 aabb_min = mu - gamma * sigma;
    vec3 aabb_max = mu + gamma * sigma;
    vec3 prev_ycocg = RGBToYCoCg(prev_color);
    vec3 clipped_ycocg = ClipAABB(aabb_min, aabb_max, prev_ycocg);
    vec3 clipped_prev = YCoCgToRGB(clipped_ycocg);

    // Disocclusion responsiveness from YCoCg clip distance
    float clip_delta = length(prev_ycocg - clipped_ycocg) / max(abs(mu.x) + 0.15, 0.15);
    float disocclusion = saturate(clip_delta * 0.45);

    // TAAU subpixel proximity weighting: lock in sharp new samples that land close to this display pixel
    float curr_blend;
    if (is_upscaling)
    {
        float sample_proximity = exp(-3.5 * min_dist_sq);
        float base_curr_blend = mix(0.045, 0.14, sample_proximity);
        curr_blend = mix(base_curr_blend, 0.28, motion_factor);
    }
    else
    {
        curr_blend = mix(0.10, 0.28, motion_factor);
    }
    curr_blend = mix(curr_blend, max(curr_blend, 0.35), disocclusion);
    float history_weight = saturate(1.0 - curr_blend);

    // Karis luminance-weighted temporal resolve to suppress HDR specular fireflies
    float w_curr = curr_blend / (1.0 + get_luminance(curr_color));
    float w_prev = history_weight / (1.0 + get_luminance(clipped_prev));
    vec3 resolved_rgb = (curr_color * w_curr + clipped_prev * w_prev) / max(w_curr + w_prev, 1e-5);
    float resolved_alpha = mix(curr_alpha, clamp(prev_alpha, alpha_min, alpha_max), history_weight);

    return vec4(max(resolved_rgb, vec3(0.0)), resolved_alpha);
}

void main()
{
    outColor = ResolveTAAU(vs_output.texCoord.xy);
}
