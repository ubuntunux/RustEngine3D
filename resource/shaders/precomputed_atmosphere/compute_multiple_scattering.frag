#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "../utility.glsl"
#include "atmosphere_common.glsl"

layout( push_constant ) uniform PushConstant_MultipleScattering
{
    mat3 luminance_from_radiance;
    int layer;
    int reserved0;
    int reserved1;
} pushConstant;

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 delta_multiple_scattering;
layout(location = 1) out vec4 scattering;

void main()
{
    float nu;
    delta_multiple_scattering = ComputeMultipleScatteringtexture2D(
        ATMOSPHERE, transmittance_texture, scattering_density_texture,
        vec3(gl_FragCoord.xy, float(pushConstant.layer) + 0.5), nu);
    scattering = vec4(pushConstant.luminance_from_radiance * delta_multiple_scattering.rgb / RayleighPhaseFunction(nu), 0.0);
}