#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureColor;
layout(binding = 1) uniform sampler2D textureBloom0;
layout(binding = 2) uniform sampler2D textureColorGradingLut;
layout(binding = 3) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 4) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 5) uniform LightData
{
    LIGHT_DATA light_data;
};
layout(binding = 6) uniform sampler2D textureSceneDepth;
layout(binding = 7) uniform sampler2D textureSceneNormal;
layout(binding = 8) uniform sampler2D textureSceneColor;

layout(push_constant) uniform PushConstant_RenderFinal
{
    uint _tone_mapping_mode;
    float _exposure;
    float _rcas_sharpness;
    float _color_grading_strength;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

#define TONEMAP_MODE_AGX 0u
#define TONEMAP_MODE_ACES_FILMIC 1u

// Compress HDR [0, inf) into [0, 1) so AMD RCAS operates cleanly without specular ringing
vec3 RcasCompress(vec3 hdr)
{
    vec3 c = max(hdr, vec3(0.0));
    return c / (1.0 + c);
}

vec3 RcasExpand(vec3 comp)
{
    vec3 c = clamp(comp, vec3(0.0), vec3(0.9999));
    return c / (1.0 - c);
}

// AMD FidelityFX RCAS (Robust Contrast Adaptive Sharpening) with noise suppression
vec3 SampleRCAS(vec2 texCoord, float sharpness)
{
    if (sharpness <= 0.001)
    {
        return max(textureLod(textureColor, texCoord, 0.0).rgb, vec3(0.0));
    }

    ivec2 tex_size = textureSize(textureColor, 0);
    ivec2 max_px = max(tex_size - ivec2(1), ivec2(0));
    ivec2 p = clamp(ivec2(texCoord * vec2(tex_size)), ivec2(0), max_px);

    vec3 b = RcasCompress(texelFetch(textureColor, clamp(p + ivec2( 0, -1), ivec2(0), max_px), 0).rgb);
    vec3 d = RcasCompress(texelFetch(textureColor, clamp(p + ivec2(-1,  0), ivec2(0), max_px), 0).rgb);
    vec3 e = RcasCompress(texelFetch(textureColor, p, 0).rgb);
    vec3 f = RcasCompress(texelFetch(textureColor, clamp(p + ivec2( 1,  0), ivec2(0), max_px), 0).rgb);
    vec3 h = RcasCompress(texelFetch(textureColor, clamp(p + ivec2( 0,  1), ivec2(0), max_px), 0).rgb);

    float bL = b.g * 0.5 + (b.r + b.b) * 0.25;
    float dL = d.g * 0.5 + (d.r + d.b) * 0.25;
    float eL = e.g * 0.5 + (e.r + e.b) * 0.25;
    float fL = f.g * 0.5 + (f.r + f.b) * 0.25;
    float hL = h.g * 0.5 + (h.r + h.b) * 0.25;

    float nz = 0.25 * (bL + dL + fL + hL) - eL;
    float luma_range = max(max(max(bL, dL), max(fL, hL)), eL) - min(min(min(bL, dL), min(fL, hL)), eL);
    nz = saturate(abs(nz) / max(luma_range, 1e-5));
    nz = -0.5 * nz + 1.0;

    vec3 mn4 = min(min(b, d), min(f, h));
    vec3 mx4 = max(max(b, d), max(f, h));
    vec3 mn = min(mn4, e);
    vec3 mx = max(mx4, e);

    vec3 hit_min = mn / max(4.0 * mx4, vec3(1e-5));
    vec3 hit_max = (1.0 - mx) / max(4.0 * (1.0 - mn4), vec3(1e-5));
    vec3 lobe_rgb = max(-hit_min, -hit_max);
    float lobe = max(lobe_rgb.r, max(lobe_rgb.g, lobe_rgb.b));

    const float FSR_RCAS_LIMIT = 0.1875;
    float w = clamp(lobe, -FSR_RCAS_LIMIT, 0.0) * saturate(sharpness) * nz;
    vec3 sharpened_comp = (e + w * (b + d + f + h)) / (1.0 + 4.0 * w);
    return RcasExpand(sharpened_comp);
}

// 3x3 tent filter sample for CoD Dual-Filter Bloom upsampling
vec3 SampleBloomTent(vec2 uv, float mip_level)
{
    vec2 mip_size = max(vec2(textureSize(textureBloom0, int(mip_level))), vec2(1.0));
    vec2 d = 1.0 / mip_size;
    vec3 c = textureLod(textureBloom0, uv, mip_level).rgb * 4.0;
    c += (textureLod(textureBloom0, uv + vec2(-d.x,  0.0), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2( d.x,  0.0), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2( 0.0, -d.y), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2( 0.0,  d.y), mip_level).rgb) * 2.0;
    c += textureLod(textureBloom0, uv + vec2(-d.x, -d.y), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2( d.x, -d.y), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2(-d.x,  d.y), mip_level).rgb
        + textureLod(textureBloom0, uv + vec2( d.x,  d.y), mip_level).rgb;
    return max(c * (1.0 / 16.0), vec3(0.0));
}

// Center-weighted auto-focus far/near Bokeh Depth of Field (12-tap Vogel disk with foreground bleed rejection)
vec3 ApplyDepthOfField(vec2 texCoord, vec3 sharpColor, float dof_intensity)
{
    if (dof_intensity <= 0.001)
    {
        return sharpColor;
    }
    float center_raw = textureLod(textureSceneDepth, texCoord, 0.0).x;
    // Reverse-Z: sky / celestial bodies (stars, Moon, Sun, outer space) have depth == 0.0; never blur them with DoF
    if (center_raw <= 0.0)
    {
        return sharpColor;
    }
    float pixel_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, center_raw);

    // Stabilized 5-tap center-screen focus metering
    float focus_depth = 0.0;
    float focus_weight = 0.0;
    const vec2 focus_offsets[5] = vec2[5](
        vec2(0.0, 0.0), vec2(0.03, 0.0), vec2(-0.03, 0.0), vec2(0.0, 0.03), vec2(0.0, -0.03)
    );
    for (int i = 0; i < 5; ++i)
    {
        float fd = textureLod(textureSceneDepth, vec2(0.5) + focus_offsets[i], 0.0).x;
        if (fd > 0.0)
        {
            float fl = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, fd);
            float w = (i == 0) ? 2.0 : 1.0;
            focus_depth += clamp(fl, 3.0, 45.0) * w;
            focus_weight += w;
        }
    }
    focus_depth = (focus_weight > 0.0) ? (focus_depth / focus_weight) : 16.0;

    float far_coc = smoothstep(focus_depth * 1.45 + 6.0, focus_depth * 4.0 + 45.0, pixel_linear);
    float near_coc = smoothstep(min(focus_depth * 0.45, 2.2), 0.5, pixel_linear) * 0.45;
    float coc = max(far_coc, near_coc);
    if (coc <= 0.01)
    {
        return sharpColor;
    }

    const int DOF_TAPS = 12;
    const float GOLDEN_ANGLE = 2.39996323;
    vec2 tex_size = max(vec2(textureSize(textureColor, 0)), vec2(1.0));
    vec2 max_radius_uv = (6.5 * coc * dof_intensity) / tex_size;
    float angle_offset = interleaved_gradient_noise(ivec2(gl_FragCoord.xy)) * TWO_PI;

    vec3 sum_color = sharpColor;
    float sum_weight = 1.0;
    for (int i = 0; i < DOF_TAPS; ++i)
    {
        float r = sqrt((float(i) + 0.5) / float(DOF_TAPS));
        float theta = float(i) * GOLDEN_ANGLE + angle_offset;
        vec2 sample_uv = clamp(texCoord + vec2(cos(theta), sin(theta)) * r * max_radius_uv, vec2(0.0), vec2(1.0));
        float sample_raw = textureLod(textureSceneDepth, sample_uv, 0.0).x;
        float sample_linear = (sample_raw > 0.0)
            ? device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, sample_raw)
            : 200.0;
        // Reject sharp foreground samples so silhouettes stay crisp against blurred backgrounds
        float sample_far_coc = smoothstep(focus_depth * 1.35 + 4.0, focus_depth * 3.8 + 40.0, sample_linear);
        float w = (sample_linear >= pixel_linear - 0.5) ? 1.0 : sample_far_coc;
        if (w > 0.01)
        {
            vec3 sample_rgb = max(textureLod(textureColor, sample_uv, 0.0).rgb, vec3(0.0));
            // Gentle Karis weighting keeps specular bokeh smooth
            w /= (1.0 + get_luminance(sample_rgb) * 0.15);
            sum_color += sample_rgb * w;
            sum_weight += w;
        }
    }
    return mix(sharpColor, sum_color / max(sum_weight, 1e-5), saturate(coc * clamp(dof_intensity, 0.0, 1.5)));
}

// Screen-Space Radial God Rays (Sun & Night Moon Light Shafts) occluded by SceneDepth
vec3 ComputeGodRays(vec2 texCoord)
{
    // Fade out atmospheric shafts in upper atmosphere / outer space
    float cam_alt_fade = 1.0 - smoothstep(8000.0, 35000.0, max(view_constants.CAMERA_POSITION.y, 0.0));
    if (cam_alt_fade <= 0.005)
    {
        return vec3(0.0);
    }

    vec3 sun_dir = normalize(-light_data.LIGHT_DIRECTION);
    vec3 moon_dir = -sun_dir;
    bool is_moon_shaft = (sun_dir.y <= -0.02);
    vec3 L = is_moon_shaft ? moon_dir : sun_dir;

    if (is_moon_shaft && moon_dir.y <= 0.04)
    {
        return vec3(0.0);
    }

    vec4 light_clip = view_constants.PROJECTION * view_constants.VIEW_ORIGIN * vec4(L, 0.0);
    if (light_clip.w <= 0.001)
    {
        return vec3(0.0);
    }

    vec2 light_uv = (light_clip.xy / light_clip.w) * 0.5 + 0.5;
    vec3 cam_forward = normalize(vec3(
        view_constants.VIEW_ORIGIN[0][2],
        view_constants.VIEW_ORIGIN[1][2],
        view_constants.VIEW_ORIGIN[2][2]
    ));
    float facing = saturate(dot(cam_forward, L));
    float elev_weight = is_moon_shaft
        ? (smoothstep(0.04, 0.20, moon_dir.y) * smoothstep(-0.02, -0.10, sun_dir.y))
        : smoothstep(-0.02, 0.12, sun_dir.y);
    float angle_weight = pow(facing, 1.8) * elev_weight * cam_alt_fade;
    if (angle_weight <= 0.005)
    {
        return vec3(0.0);
    }

    vec2 scene_size = max(vec2(textureSize(textureSceneDepth, 0)), vec2(1.0));
    float aspect = scene_size.x / scene_size.y;
    float pixel_dist_to_light = length((texCoord - light_uv) * vec2(aspect, 1.0));
    float pixel_radial_falloff = exp(-pixel_dist_to_light * 0.85);
    if (pixel_radial_falloff <= 0.01)
    {
        return vec3(0.0);
    }

    // Clamp radial march length so off-screen light positions still cast soft edge shafts
    vec2 delta_to_light = light_uv - texCoord;
    float ray_len = length(delta_to_light);
    const float MAX_RAY_UV = 0.78;
    if (ray_len > MAX_RAY_UV)
    {
        delta_to_light *= (MAX_RAY_UV / ray_len);
    }

    const int SHAFT_SAMPLES = 20;
    const float DENSITY = 0.94;
    const float DECAY = 0.94;
    vec2 step_uv = (delta_to_light * DENSITY) / float(SHAFT_SAMPLES);
    float jitter = interleaved_gradient_noise(ivec2(gl_FragCoord.xy));
    vec2 sample_uv = texCoord + step_uv * jitter;

    float illumination_decay = 1.0;
    vec3 shaft_accum = vec3(0.0);
    // Subtract ambient sky floor so bright Sun/corona (or Moon) drives crisp shafts without washing out distant shadows
    float sky_threshold = is_moon_shaft ? 0.04 : 0.75;

    for (int i = 0; i < SHAFT_SAMPLES; ++i)
    {
        if (sample_uv.x < 0.0 || sample_uv.x > 1.0 || sample_uv.y < 0.0 || sample_uv.y > 1.0)
        {
            break;
        }
        float d = textureLod(textureSceneDepth, sample_uv, 0.0).x;
        // Reverse-Z: sky has depth == 0.0
        if (d <= 0.00001)
        {
            vec3 sky_sample = clamp(textureLod(textureSceneColor, sample_uv, 1.5).rgb, vec3(0.0), vec3(12.0));
            float sample_luma = get_luminance(sky_sample);
            float excess = max(sample_luma - sky_threshold, 0.0) / max(sample_luma, 1e-4);
            float sample_dist = length((sample_uv - light_uv) * vec2(aspect, 1.0));
            float sun_radial = exp(-sample_dist * 2.2) * 0.75 + exp(-sample_dist * 0.9) * 0.25;
            vec2 edge_fade = smoothstep(vec2(0.0), vec2(0.08), sample_uv) * smoothstep(vec2(1.0), vec2(0.92), sample_uv);
            shaft_accum += (sky_sample * excess) * sun_radial * illumination_decay * edge_fade.x * edge_fade.y;
        }
        illumination_decay *= DECAY;
        sample_uv += step_uv;
    }

    vec3 shaft_tint = is_moon_shaft
        ? mix(vec3(0.72, 0.84, 1.0), normalize(max(scene_constants.MOON_LIGHT_COLOR, vec3(0.1))) * 1.35, 0.45)
        : mix(vec3(1.0, 0.94, 0.82), normalize(max(light_data.LIGHT_COLOR, vec3(0.1))) * 1.4, 0.35);
    float shaft_strength = is_moon_shaft ? 0.48 : 0.62;
    return (shaft_accum / float(SHAFT_SAMPLES)) * shaft_tint * angle_weight * pixel_radial_falloff * shaft_strength;
}

// Center-weighted Auto-Exposure (Eye Adaptation) using pre-downsampled SceneColor mip chain
float ComputeAutoExposureMultiplier()
{
    int mip_levels = textureQueryLevels(textureSceneColor);
    float meter_mip = float(max(mip_levels - 3, 0));

    const vec2 sample_uvs[9] = vec2[9](
        vec2(0.50, 0.50),
        vec2(0.32, 0.50), vec2(0.68, 0.50),
        vec2(0.50, 0.34), vec2(0.50, 0.66),
        vec2(0.25, 0.28), vec2(0.75, 0.28),
        vec2(0.25, 0.72), vec2(0.75, 0.72)
    );
    const float sample_weights[9] = float[9](
        2.5, 1.5, 1.5, 1.4, 1.6, 0.7, 0.7, 0.8, 0.8
    );

    float log_luma_sum = 0.0;
    float weight_sum = 0.0;
    for (int i = 0; i < 9; ++i)
    {
        // Cap extreme HDR spikes (e.g. Sun disc) during metering to prevent exposure pumping
        vec3 c = clamp(textureLod(textureSceneColor, sample_uvs[i], meter_mip).rgb, vec3(0.0), vec3(4.0));
        float lum = max(get_luminance(c), 0.015);
        log_luma_sum += log2(lum) * sample_weights[i];
        weight_sum += sample_weights[i];
    }

    float avg_luma = exp2(log_luma_sum / max(weight_sum, 1e-5));
    // Key value calibrated for StoneAge's stylized sunlit scenes (~0.32) and dark cave interiors (~0.08)
    const float TARGET_KEY = 0.32;
    float raw_mult = clamp(TARGET_KEY / max(avg_luma, 0.045), 0.65, 1.80);
    // Damped curve preserves deliberate mood contrast between caves and bright daylight
    return pow(raw_mult, 0.60);
}

// Stylized Depth + Normal Sobel Outline / Edge Highlight with distance fade and surface-tinted ink
vec3 ApplyStylizedOutline(vec2 texCoord, vec3 color)
{
    float center_raw = textureLod(textureSceneDepth, texCoord, 0.0).x;
    if (center_raw <= 0.0)
    {
        return color;
    }

    float center_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, center_raw);
    float dist_fade = 1.0 - smoothstep(18.0, 48.0, center_linear);
    if (dist_fade <= 0.005)
    {
        return color;
    }

    vec2 inv_depth_size = 1.0 / max(vec2(textureSize(textureSceneDepth, 0)), vec2(1.0));
    vec2 uvL = clamp(texCoord + vec2(-inv_depth_size.x, 0.0), vec2(0.0), vec2(1.0));
    vec2 uvR = clamp(texCoord + vec2( inv_depth_size.x, 0.0), vec2(0.0), vec2(1.0));
    vec2 uvU = clamp(texCoord + vec2(0.0, -inv_depth_size.y), vec2(0.0), vec2(1.0));
    vec2 uvD = clamp(texCoord + vec2(0.0,  inv_depth_size.y), vec2(0.0), vec2(1.0));

    float dL = textureLod(textureSceneDepth, uvL, 0.0).x;
    float dR = textureLod(textureSceneDepth, uvR, 0.0).x;
    float dU = textureLod(textureSceneDepth, uvU, 0.0).x;
    float dD = textureLod(textureSceneDepth, uvD, 0.0).x;

    float linL = (dL > 0.0) ? device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, dL) : center_linear * 2.5;
    float linR = (dR > 0.0) ? device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, dR) : center_linear * 2.5;
    float linU = (dU > 0.0) ? device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, dU) : center_linear * 2.5;
    float linD = (dD > 0.0) ? device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, dD) : center_linear * 2.5;

    // Silhouette depth edge normalized by pixel distance
    float depth_diff = (abs(linL - linR) + abs(linU - linD)) / max(center_linear, 1.0);
    float depth_edge = smoothstep(0.09, 0.24, depth_diff);

    // Crease normal discontinuity edge
    vec3 nL = normalize(textureLod(textureSceneNormal, uvL, 0.0).xyz * 2.0 - 1.0);
    vec3 nR = normalize(textureLod(textureSceneNormal, uvR, 0.0).xyz * 2.0 - 1.0);
    vec3 nU = normalize(textureLod(textureSceneNormal, uvU, 0.0).xyz * 2.0 - 1.0);
    vec3 nD = normalize(textureLod(textureSceneNormal, uvD, 0.0).xyz * 2.0 - 1.0);
    float normal_diff = (1.0 - dot(nL, nR)) + (1.0 - dot(nU, nD));
    float normal_edge = smoothstep(0.45, 0.95, normal_diff) * 0.45;

    float outline_strength = saturate(max(depth_edge, normal_edge)) * dist_fade;
    // Warm surface-tinted ink avoids harsh black lines on bright stylized colors
    vec3 ink_color = color * vec3(0.32, 0.28, 0.26);
    return mix(color, ink_color, outline_strength * 0.80);
}

vec3 agxDefaultContrastApprox(vec3 x)
{
    vec3 x2 = x * x;
    vec3 x4 = x2 * x2;
    return + 15.5     * x4 * x2
           - 40.14    * x4 * x
           + 31.96    * x4
           - 6.868    * x2 * x
           + 0.4298   * x2
           + 0.1191   * x
           - 0.00232;
}

vec3 agxLookPunchy(vec3 val)
{
    const vec3 lw = vec3(0.2126, 0.7152, 0.0722);
    float luma = dot(val, lw);
    val = pow(max(vec3(0.0), val), vec3(1.06));
    float sat = 1.22;
    return max(vec3(0.0), luma + sat * (val - luma));
}

// AgX tonemapper: returns linear sRGB for R8G8B8A8_SRGB swapchain output
vec3 AgXTonemap(vec3 hdrColor, float exposure)
{
    const mat3 agx_mat = mat3(
        0.842479062253094, 0.0423282422610123, 0.0423756549057051,
        0.0784335999999992, 0.878468636469772, 0.0784336,
        0.0792237451477643, 0.0791661274605434, 0.879142973793104
    );
    const mat3 agx_mat_inv = mat3(
         1.19687900512017,  -0.0528968517574562, -0.0529716355144438,
        -0.0980208811401368, 1.15190312990417,   -0.0980434501171241,
        -0.0990297440797205, -0.0989611768448433, 1.15107367264116
    );
    const float min_ev = -12.47393;
    const float max_ev = 4.026069;

    vec3 val = max(vec3(0.0), hdrColor * exposure);
    val = agx_mat * val;
    val = clamp(log2(max(val, vec3(1e-10))), min_ev, max_ev);
    val = (val - min_ev) / (max_ev - min_ev);
    val = agxDefaultContrastApprox(val);
    val = agxLookPunchy(val);
    val = agx_mat_inv * val;
    return pow(saturate(val), vec3(2.2));
}

// ACES Filmic tonemapper (Stephen Hill fit): returns linear sRGB for R8G8B8A8_SRGB swapchain output
vec3 ACESFilmicTonemap(vec3 hdrColor, float exposure)
{
    const mat3 aces_input = mat3(
        0.59719, 0.07600, 0.02840,
        0.35458, 0.90834, 0.13383,
        0.04823, 0.01566, 0.83777
    );
    const mat3 aces_output = mat3(
         1.60475, -0.10208, -0.00327,
        -0.53108,  1.10813, -0.07276,
        -0.07367, -0.00605,  1.07602
    );

    vec3 v = max(vec3(0.0), hdrColor * exposure * 0.6);
    v = aces_input * v;
    vec3 a = v * (v + 0.0245786) - 0.000090537;
    vec3 b = v * (0.983729 * v + 0.4329510) + 0.238081;
    v = a / b;
    v = aces_output * v;
    return saturate(v);
}

// 32x32x32 2D-strip (1024x32) color grading LUT lookup in perceptual sRGB space
vec3 ApplyColorGradingLUT(vec3 linearColor, float strength)
{
    if (strength <= 0.001)
    {
        return linearColor;
    }

    vec3 srgb = pow(saturate(linearColor), vec3(1.0 / 2.2));

    const float LUT_SIZE = 32.0;
    const float MAX_IDX = LUT_SIZE - 1.0;

    float b_scaled = srgb.b * MAX_IDX;
    float slice0 = floor(b_scaled);
    float slice1 = min(slice0 + 1.0, MAX_IDX);
    float b_frac = b_scaled - slice0;

    float u_in_slice = (0.5 + srgb.r * MAX_IDX) / LUT_SIZE;
    float v = (0.5 + srgb.g * MAX_IDX) / LUT_SIZE;

    float u0 = (slice0 + u_in_slice) / LUT_SIZE;
    float u1 = (slice1 + u_in_slice) / LUT_SIZE;

    vec3 c0 = textureLod(textureColorGradingLut, vec2(u0, v), 0.0).rgb;
    vec3 c1 = textureLod(textureColorGradingLut, vec2(u1, v), 0.0).rgb;
    vec3 graded_srgb = mix(c0, c1, b_frac);

    vec3 graded_linear = pow(saturate(graded_srgb), vec3(2.2));
    return mix(linearColor, graded_linear, saturate(strength));
}

float vignetting(vec2 uv, float inner_value, float outter_value)
{
    float f = smoothstep(0.0, 1.0, length(uv - vec2(0.5)));
    return mix(inner_value, outter_value, f * f);
}

void main()
{
    const vec2 texCoord = vs_output.texCoord;
    const uint render_option = scene_constants.RENDER_OPTION;

    // 1. AMD RCAS Sharpening on TAAU output
    vec3 hdr_color = SampleRCAS(texCoord, pushConstant._rcas_sharpness);

    // 2. Depth of Field (far/near Bokeh with foreground silhouette protection)
    if ((render_option & RenderOption_RenderDepthOfField) != 0u && scene_constants.DOF_INTENSITY > 0.001)
    {
        hdr_color = ApplyDepthOfField(texCoord, hdr_color, scene_constants.DOF_INTENSITY);
    }

    // 3. Karis + CoD Dual-Filter Bloom (3x3 Tent multi-mip upsample)
    if ((render_option & RenderOption_RenderBloom) != 0u && scene_constants.BLOOM_INTENSITY > 0.001)
    {
        const float bloom_intensity = 0.11 * max(scene_constants.BLOOM_INTENSITY, 0.0);
        const float bloom_weight[5] = float[] (0.20, 0.22, 0.22, 0.20, 0.16);
        vec3 bloom = vec3(0.0);
        bloom += SampleBloomTent(texCoord, 0.0) * bloom_weight[0];
        bloom += SampleBloomTent(texCoord, 1.0) * bloom_weight[1];
        bloom += SampleBloomTent(texCoord, 2.0) * bloom_weight[2];
        bloom += SampleBloomTent(texCoord, 3.0) * bloom_weight[3];
        bloom += SampleBloomTent(texCoord, 4.0) * bloom_weight[4];
        hdr_color += bloom * bloom_intensity;
    }

    // 4. God Rays (Screen-Space Radial Light Shafts)
    if ((render_option & RenderOption_RenderGodRays) != 0u && scene_constants.GOD_RAYS_INTENSITY > 0.001)
    {
        hdr_color += ComputeGodRays(texCoord) * max(scene_constants.GOD_RAYS_INTENSITY, 0.0);
    }

    // 5. Auto-Exposure (Eye Adaptation) + Tone Mapping
    float exposure = pushConstant._exposure > 0.0 ? pushConstant._exposure : 0.90;
    if ((render_option & RenderOption_RenderAutoExposure) != 0u)
    {
        exposure *= ComputeAutoExposureMultiplier();
    }

    vec3 ldr_linear;
#ifdef FORCE_TONEMAP_MODE
    #if FORCE_TONEMAP_MODE == 1
        ldr_linear = ACESFilmicTonemap(hdr_color, exposure);
    #else
        ldr_linear = AgXTonemap(hdr_color, exposure);
    #endif
#else
    if (pushConstant._tone_mapping_mode == TONEMAP_MODE_ACES_FILMIC)
    {
        ldr_linear = ACESFilmicTonemap(hdr_color, exposure);
    }
    else
    {
        ldr_linear = AgXTonemap(hdr_color, exposure);
    }
#endif

    // 6. Color Grading LUT (32^3)
    ldr_linear = ApplyColorGradingLUT(ldr_linear, pushConstant._color_grading_strength);

    // 7. Stylized Outline / Edge Highlight
    if ((render_option & RenderOption_RenderStylizedOutline) != 0u)
    {
        ldr_linear = ApplyStylizedOutline(texCoord, ldr_linear);
    }

    // 8. Vignetting
    ldr_linear *= vignetting(texCoord, 1.0, 0.85);
    outColor = vec4(ldr_linear, 1.0);
}
