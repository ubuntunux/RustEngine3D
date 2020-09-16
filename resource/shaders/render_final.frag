#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"

layout(binding = 0) uniform sampler2D textureColor;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

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

vec3 ReinhardTonemap(vec3 hdrColor)
{
    return hdrColor / (hdrColor + vec3(1.0));
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
    vec4 color = texture(textureColor, texCoord);
    color.xyz = ReinhardTonemap(color.xyz);
    color.xyz *= vignetting(texCoord, 1.0, 0.20);
    color.xyz = Contrast(color.xyz, 1.0);
    outColor = color;
}
