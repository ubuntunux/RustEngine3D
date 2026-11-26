#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform sampler2D textureSrc;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

vec3 SampleClamped(vec2 uv)
{
    return max(vec3(0.0), textureLod(textureSrc, clamp(uv, vec2(0.0), vec2(1.0)), 0.0).rgb);
}

// Call of Duty: Advanced Warfare 13-tap overlapping tent downsample filter
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

    vec3 color = (d + e + i + j) * 0.125;
    color += (a + c + k + m) * 0.03125;
    color += (b + f + h + l) * 0.0625;
    color += g * 0.125;

    outColor = vec4(color, 1.0);
}
