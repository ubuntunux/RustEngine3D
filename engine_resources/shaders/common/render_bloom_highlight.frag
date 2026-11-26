#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureSrc;

layout(push_constant) uniform PushConstant_BloomHighlight
{
    float _bloom_threshold_min;
    float _bloom_threshold_max;
    uint _reserved0;
    uint _reserved1;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

vec3 SampleClamped(vec2 uv)
{
    return clamp(textureLod(textureSrc, clamp(uv, vec2(0.0), vec2(1.0)), 0.0).rgb, vec3(0.0), vec3(65000.0));
}

float KarisWeight(vec3 color)
{
    return 1.0 / (1.0 + get_luminance(color));
}

// Karis-weighted CoD 13-tap prefilter downsample to eliminate HDR specular fireflies before bloom extraction
void main()
{
    const vec2 uv = vs_output.texCoord.xy;
    const vec2 texel = 1.0 / max(vec2(textureSize(textureSrc, 0)), vec2(1.0));

    vec3 a = SampleClamped(uv + texel * vec2(-2.0, -2.0));
    vec3 b = SampleClamped(uv + texel * vec2( 0.0, -2.0));
    vec3 c = SampleClamped(uv + texel * vec2( 2.0, -2.0));

    vec3 d = SampleClamped(uv + texel * vec2(-1.0, -1.0));
    vec3 e = SampleClamped(uv + texel * vec2( 1.0, -1.0));

    vec3 f = SampleClamped(uv + texel * vec2(-2.0,  0.0));
    vec3 g = SampleClamped(uv);
    vec3 h = SampleClamped(uv + texel * vec2( 2.0,  0.0));

    vec3 i = SampleClamped(uv + texel * vec2(-1.0,  1.0));
    vec3 j = SampleClamped(uv + texel * vec2( 1.0,  1.0));

    vec3 k = SampleClamped(uv + texel * vec2(-2.0,  2.0));
    vec3 l = SampleClamped(uv + texel * vec2( 0.0,  2.0));
    vec3 m = SampleClamped(uv + texel * vec2( 2.0,  2.0));

    // 5 overlapping 2x2 groups weighted by Karis inverse luminance
    vec3 g0 = (d + e + i + j) * 0.25;
    vec3 g1 = (a + b + f + g) * 0.25;
    vec3 g2 = (b + c + g + h) * 0.25;
    vec3 g3 = (f + g + k + l) * 0.25;
    vec3 g4 = (g + h + l + m) * 0.25;

    float w0 = KarisWeight(g0) * 0.5;
    float w1 = KarisWeight(g1) * 0.125;
    float w2 = KarisWeight(g2) * 0.125;
    float w3 = KarisWeight(g3) * 0.125;
    float w4 = KarisWeight(g4) * 0.125;

    vec3 color = (g0 * w0 + g1 * w1 + g2 * w2 + g3 * w3 + g4 * w4) / max(w0 + w1 + w2 + w3 + w4, 1e-5);

    // Soft-knee HDR bloom extraction so only genuine bright highlights (sun/sky/emissives) bloom
    float luminance = get_luminance(color);
    const float threshold = max(pushConstant._bloom_threshold_min, 1.55);
    const float knee = 0.55;
    float soft = clamp(luminance - threshold + knee, 0.0, 2.0 * knee);
    soft = (soft * soft) / (4.0 * knee + 1e-5);
    float contribution = max(soft, luminance - threshold) / max(luminance, 1e-5);
    outColor = vec4(color * saturate(contribution), 1.0);
}
