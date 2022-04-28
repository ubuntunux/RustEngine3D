#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureColor;
layout(binding = 1) uniform sampler2D textureBloom0;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 outColor;


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
    return clamp(Ld * hdrColor / L, 0.0, 1.0);
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
    float bloom_intensity = 0.1;
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

    color.xyz = ReinhardTonemap(color.xyz, 1.0);
    color.xyz *= vignetting(texCoord, 1.0, 0.20);
    color.xyz = Contrast(color.xyz, 1.0);
    outColor = color;
}
