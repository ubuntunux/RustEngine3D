#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureColor;
layout(binding = 1) uniform sampler2D textureBloom0;

layout(push_constant) uniform PushConstant_RenderFinal
{
    uint _tone_mapping_mode;
    float _exposure;
    uint _reserved0;
    uint _reserved1;
} pushConstant;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;

// Select tonemap operator:
//   TONEMAP_MODE_AGX         (0) : Blender/Troy Sobotka AgX (default, vibrant & hue-preserving)
//   TONEMAP_MODE_ACES_FILMIC (1) : Stephen Hill ACES Filmic RRT+ODT
// Runtime selection uses pushConstant._tone_mapping_mode; define FORCE_TONEMAP_MODE to force compile-time selection.
#define TONEMAP_MODE_AGX 0u
#define TONEMAP_MODE_ACES_FILMIC 1u

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

vec3 Uncharted2TonemapFunction(vec3 x)
{
    const float A = 0.15;
    const float B = 0.50;
    const float C = 0.10;
    const float D = 0.20;
    const float E = 0.02;
    const float F = 0.30;
    return ((x*(A*x+C*B)+D*E)/(x*(A*x+B)+D*F))-E/F;
}

vec3 Uncharted2Tonemap(vec3 hdrColor, float exposure)
{
    const float W = 11.2;
    const float ExposureBias = 2.0f;
    hdrColor *= exposure;
    hdrColor = Uncharted2TonemapFunction(hdrColor * ExposureBias);
    vec3 whiteScale = 1.0f / Uncharted2TonemapFunction(vec3(W));
    return hdrColor * whiteScale;
}

vec3 ReinhardTonemap(vec3 hdrColor, float exposure)
{
    hdrColor *= exposure;
    float L = get_luminance(hdrColor);
    float Ld = L / (1.0 + L);
    return clamp(Ld * hdrColor / max(L, 0.0001), 0.0, 1.0);
}

vec3 SimpleTonemap(vec3 hdrColor, float exposure)
{
    // Exposure tone mapping
    return vec3(1.0) - exp(-hdrColor * exposure);
}

float vignetting(vec2 uv, float inner_value, float outter_value)
{
    float f = smoothstep(0.0, 1.0, length(uv - vec2(0.5)));
    return mix(inner_value, outter_value, f * f);
}

void main() {
    // Tonemapping
    const vec2 texCoord = vs_output.texCoord;
    vec4 color = texture(textureColor, texCoord);
    vec3 bloom = vec3(0.0);
    bool is_render_bloom = true;
    const float bloom_intensity = 0.12;
    const float bloom_weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);
    if(is_render_bloom)
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
    color.xyz *= vignetting(texCoord, 1.0, 0.85);
    outColor = color;
}
