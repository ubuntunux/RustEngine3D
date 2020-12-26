#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "render_quad_common.glsl"

layout( push_constant ) uniform PushConstant_FFT_Variance
{
    vec4 GRID_SIZES;
    float N_SLOPE_VARIANCE;
    int FFT_SIZE;
    float slopeVarianceDelta;
    float c;
} pushConstant;

layout(binding = 0) uniform sampler2D spectrum_1_2_Sampler;
layout(binding = 1) uniform sampler2D spectrum_3_4_Sampler;

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
    float a = floor(uv.x * N_SLOPE_VARIANCE);
    float b = floor(uv.y * N_SLOPE_VARIANCE);
    float A = pow(a / (N_SLOPE_VARIANCE - 1.0), 4.0) * SCALE;
    float C = pow(c / (N_SLOPE_VARIANCE - 1.0), 4.0) * SCALE;
    float B = (2.0 * b / (N_SLOPE_VARIANCE - 1.0) - 1.0) * sqrt(A * C);
    A = -0.5 * A;
    B = - B;
    C = -0.5 * C;

    vec2 slopeVariances = vec2(slopeVarianceDelta);
    vec4 spectrum12;
    vec4 spectrum34;

    for (int y = 0; y < FFT_SIZE; ++y)
    {
        for (int x = 0; x < FFT_SIZE; ++x)
        {
            int i = x >= (FFT_SIZE / 2) ? x - FFT_SIZE : x;
            int j = y >= (FFT_SIZE / 2) ? y - FFT_SIZE : y;
            vec2 k = 2.0 * PI * vec2(i, j);

            spectrum12 = texture(spectrum_1_2_Sampler, vec2(float(x) + 0.5, float(y) + 0.5) / float(FFT_SIZE));
            spectrum34 = texture(spectrum_3_4_Sampler, vec2(float(x) + 0.5, float(y) + 0.5) / float(FFT_SIZE));

            slopeVariances += getSlopeVariances(k / GRID_SIZES.x, A, B, C, spectrum12.xy) * 100.0;
            slopeVariances += getSlopeVariances(k / GRID_SIZES.y, A, B, C, spectrum12.zw) * 100.0;
            slopeVariances += getSlopeVariances(k / GRID_SIZES.z, A, B, C, spectrum34.xy) * 100.0;
            slopeVariances += getSlopeVariances(k / GRID_SIZES.w, A, B, C, spectrum34.zw) * 100.0;
        }
    }
    color = slopeVariances.xxxy;
}