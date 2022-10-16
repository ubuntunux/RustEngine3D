#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/render_quad_common.glsl"
#include "common_fft_ocean.glsl"

layout(binding = 0) uniform sampler2D texture_spectrum_1_2;
layout(binding = 1) uniform sampler2D texture_spectrum_3_4;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 color0;
layout(location = 1) out vec4 color1;
layout(location = 2) out vec4 color2;
layout(location = 3) out vec4 color3;
layout(location = 4) out vec4 color4;

vec2 getSpectrum(float k, vec2 s0, vec2 s0c)
{
    float w = sqrt(9.81 * k * (1.0 + k * k / (370.0 * 370.0)));
    float c = cos(w * pushConstant._t);
    float s = sin(w * pushConstant._t);
    return vec2((s0.x + s0c.x) * c - (s0.y + s0c.y) * s, (s0.x - s0c.x) * s + (s0.y - s0c.y) * c);
}

vec2 i(vec2 z)
{
    return vec2(-z.y, z.x); // returns i times z (complex number)
}

void main()
{
    const float fft_size = pushConstant._fft_size;
    vec2 uv = vs_output.texCoord;
    vec2 st = floor(uv * fft_size) / fft_size;
    float x = uv.x > 0.5 ? st.x - 1.0 : st.x;
    float y = uv.y > 0.5 ? st.y - 1.0 : st.y;

    vec4 s12 = textureLod(texture_spectrum_1_2, uv, 0.0);
    vec4 s34 = textureLod(texture_spectrum_3_4, uv, 0.0);
    vec4 s12c = textureLod(texture_spectrum_1_2, vec2(1.0 + 0.5 / fft_size) - st, 0.0);
    vec4 s34c = textureLod(texture_spectrum_3_4, vec2(1.0 + 0.5 / fft_size) - st, 0.0);

    vec2 k1 = vec2(x, y) * pushConstant._inverse_grid_sizes.x;
    vec2 k2 = vec2(x, y) * pushConstant._inverse_grid_sizes.y;
    vec2 k3 = vec2(x, y) * pushConstant._inverse_grid_sizes.z;
    vec2 k4 = vec2(x, y) * pushConstant._inverse_grid_sizes.w;

    float K1 = length(k1);
    float K2 = length(k2);
    float K3 = length(k3);
    float K4 = length(k4);

    float IK1 = K1 == 0.0 ? 0.0 : 1.0 / K1;
    float IK2 = K2 == 0.0 ? 0.0 : 1.0 / K2;
    float IK3 = K3 == 0.0 ? 0.0 : 1.0 / K3;
    float IK4 = K4 == 0.0 ? 0.0 : 1.0 / K4;

    vec2 h1 = getSpectrum(K1, s12.xy, s12c.xy);
    vec2 h2 = getSpectrum(K2, s12.zw, s12c.zw);
    vec2 h3 = getSpectrum(K3, s34.xy, s34c.xy);
    vec2 h4 = getSpectrum(K4, s34.zw, s34c.zw);

    color0 = vec4(h1 + i(h2), h3 + i(h4));
    color1 = vec4(i(k1.x * h1) - k1.y * h1, i(k2.x * h2) - k2.y * h2);
    color2 = vec4(i(k3.x * h3) - k3.y * h3, i(k4.x * h4) - k4.y * h4);
    color3 = color1 * vec4(IK1, IK1, IK2, IK2);
    color4 = color2 * vec4(IK3, IK3, IK4, IK4);
}