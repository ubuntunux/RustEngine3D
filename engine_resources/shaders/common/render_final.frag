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

layout(push_constant) uniform PushConstant_RenderFinal
{
    uint _tone_mapping_mode;
    float _exposure;
    float _rcas_sharpness;
    float _color_grading_strength;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

// Select tonemap operator:
//   TONEMAP_MODE_AGX         (0) : Blender/Troy Sobotka AgX (default, vibrant & hue-preserving)
//   TONEMAP_MODE_ACES_FILMIC (1) : Stephen Hill ACES Filmic RRT+ODT
// Runtime selection uses pushConstant._tone_mapping_mode; define FORCE_TONEMAP_MODE to force compile-time selection.
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

    // Approximate perceptual luminance (2*G + R + B) * 0.25
    float bL = b.g * 0.5 + (b.r + b.b) * 0.25;
    float dL = d.g * 0.5 + (d.r + d.b) * 0.25;
    float eL = e.g * 0.5 + (e.r + e.b) * 0.25;
    float fL = f.g * 0.5 + (f.r + f.b) * 0.25;
    float hL = h.g * 0.5 + (h.r + h.b) * 0.25;

    // Noise detection attenuates sharpening on isolated 1-pixel spikes
    float nz = 0.25 * (bL + dL + fL + hL) - eL;
    float luma_range = max(max(max(bL, dL), max(fL, hL)), eL) - min(min(min(bL, dL), min(fL, hL)), eL);
    nz = saturate(abs(nz) / max(luma_range, 1e-5));
    nz = -0.5 * nz + 1.0;

    // Min and max of 4-neighbor ring + center per channel
    vec3 mn4 = min(min(b, d), min(f, h));
    vec3 mx4 = max(max(b, d), max(f, h));
    vec3 mn = min(mn4, e);
    vec3 mx = max(mx4, e);

    // Maximum negative sharpening lobe that stays strictly within [0, 1]
    vec3 hit_min = mn / max(4.0 * mx4, vec3(1e-5));
    vec3 hit_max = (1.0 - mx) / max(4.0 * (1.0 - mn4), vec3(1e-5));
    vec3 lobe_rgb = max(-hit_min, -hit_max);
    float lobe = max(lobe_rgb.r, max(lobe_rgb.g, lobe_rgb.b));

    const float FSR_RCAS_LIMIT = 0.1875; // 0.25 - 1/16
    float w = clamp(lobe, -FSR_RCAS_LIMIT, 0.0) * saturate(sharpness) * nz;
    vec3 sharpened_comp = (e + w * (b + d + f + h)) / (1.0 + 4.0 * w);
    return RcasExpand(sharpened_comp);
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
    // Gentle midtone contrast + saturation boost so stylized colors stay rich without crushing shadows
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
    // AgX contrast curve outputs display-encoded (gamma ~2.2) values; linearize for sRGB swapchain
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

    // Half-texel inset avoids cross-slice bleeding and sampler wrap artifacts
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
    vec4 color = vec4(SampleRCAS(texCoord, pushConstant._rcas_sharpness), 1.0);

    vec3 bloom = vec3(0.0);
    const bool is_render_bloom = true;
    const float bloom_intensity = 0.12;
    const float bloom_weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);
    if (is_render_bloom)
    {
        bloom += textureLod(textureBloom0, texCoord, 0.0).xyz * bloom_weight[0];
        bloom += textureLod(textureBloom0, texCoord, 1.0).xyz * bloom_weight[1];
        bloom += textureLod(textureBloom0, texCoord, 2.0).xyz * bloom_weight[2];
        bloom += textureLod(textureBloom0, texCoord, 3.0).xyz * bloom_weight[3];
        bloom += textureLod(textureBloom0, texCoord, 4.0).xyz * bloom_weight[4];
        bloom *= bloom_intensity;
    }
    color.xyz += bloom;

    const float exposure = pushConstant._exposure > 0.0 ? pushConstant._exposure : 0.90;
#ifdef FORCE_TONEMAP_MODE
    #if FORCE_TONEMAP_MODE == 1
        color.xyz = ACESFilmicTonemap(color.xyz, exposure);
    #else
        color.xyz = AgXTonemap(color.xyz, exposure);
    #endif
#else
    if (pushConstant._tone_mapping_mode == TONEMAP_MODE_ACES_FILMIC)
    {
        color.xyz = ACESFilmicTonemap(color.xyz, exposure);
    }
    else
    {
        color.xyz = AgXTonemap(color.xyz, exposure);
    }
#endif

    color.xyz = ApplyColorGradingLUT(color.xyz, pushConstant._color_grading_strength);
    color.xyz *= vignetting(texCoord, 1.0, 0.85);
    outColor = color;
}
