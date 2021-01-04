#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "atmosphere_common.glsl"

layout( push_constant ) uniform PushConstant_IndirectIrradiance
{
    mat3 luminance_from_radiance;
    int scattering_order;
    int reserved0;
    int reserved1;
} pushConstant;

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 delta_irradiance;
layout(location = 1) out vec3 irradiance;

void main()
{
    delta_irradiance = ComputeIndirectIrradiancetexture2D(
        ATMOSPHERE, single_rayleigh_scattering_texture,
        single_mie_scattering_texture, multiple_scattering_texture,
        gl_FragCoord.xy, pushConstant.scattering_order);

    irradiance = pushConstant.luminance_from_radiance * delta_irradiance;
}