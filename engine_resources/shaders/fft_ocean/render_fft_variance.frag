#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/render_quad_common.glsl"
#include "common_fft_ocean.glsl"

layout(binding = 0) uniform sampler2D texture_spectrum_1_2;
layout(binding = 1) uniform sampler2D texture_spectrum_3_4;

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 color;

vec2 getSlopeVariances(vec2 k, float A, float B, float C, vec2 spectrumSample)
{
    float w = 1.0 - exp(A * k.x * k.x + B * k.x * k.y + C * k.y * k.y);
    vec2 kw = k * w;
    return kw * kw * dot(spectrumSample, spectrumSample) * 2.0;
}

void main()
{
    vec2 uv = vs_output.texCoord;
    const float SCALE = 10.0;
    float a = floor(uv.x * pushConstant._n_slope_variance);
    float b = floor(uv.y * pushConstant._n_slope_variance);
    float A = pow(a / (pushConstant._n_slope_variance - 1.0), 4.0) * SCALE;
    float C = pow(pushConstant._c / (pushConstant._n_slope_variance - 1.0), 4.0) * SCALE;
    float B = (2.0 * b / (pushConstant._n_slope_variance - 1.0) - 1.0) * sqrt(A * C);
    A = -0.5 * A;
    B = - B;
    C = -0.5 * C;

    vec2 slopeVariances = vec2(pushConstant._slope_variance_delta);
    vec4 spectrum12;
    vec4 spectrum34;

    for (int y = 0; y < pushConstant._fft_size; ++y)
    {
        for (int x = 0; x < pushConstant._fft_size; ++x)
        {
            int i = ((pushConstant._fft_size / 2) <= x) ? (x - pushConstant._fft_size) : x;
            int j = ((pushConstant._fft_size / 2) <= y) ? (y - pushConstant._fft_size) : y;
            vec2 k = 2.0 * PI * vec2(i, j);

            spectrum12 = textureLod(texture_spectrum_1_2, vec2(float(x) + 0.5, float(y) + 0.5) / float(pushConstant._fft_size), 0.0);
            spectrum34 = textureLod(texture_spectrum_3_4, vec2(float(x) + 0.5, float(y) + 0.5) / float(pushConstant._fft_size), 0.0);

            slopeVariances += getSlopeVariances(k / pushConstant._grid_sizes.x, A, B, C, spectrum12.xy) * 100.0;
            slopeVariances += getSlopeVariances(k / pushConstant._grid_sizes.y, A, B, C, spectrum12.zw) * 100.0;
            slopeVariances += getSlopeVariances(k / pushConstant._grid_sizes.z, A, B, C, spectrum34.xy) * 100.0;
            slopeVariances += getSlopeVariances(k / pushConstant._grid_sizes.w, A, B, C, spectrum34.zw) * 100.0;
        }
    }
    color = slopeVariances.xxxy;
}