#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "atmosphere_common.glsl"

layout( push_constant ) uniform PushConstant_SingleScattering
{
    mat3 luminance_from_radiance;
    int layer;
    int reserved0;
    int reserved1;
} pushConstant;

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 delta_rayleigh;
layout(location = 1) out vec3 delta_mie;
layout(location = 2) out vec4 scattering;
layout(location = 3) out vec3 single_mie_scattering;

void main()
{
    ComputeSingleScatteringtexture2D(
        ATMOSPHERE, transmittance_texture, vec3(gl_FragCoord.xy, float(pushConstant.layer) + 0.5),
        delta_rayleigh, delta_mie);
    scattering = vec4(pushConstant.luminance_from_radiance * delta_rayleigh.rgb, (pushConstant.luminance_from_radiance * delta_mie).r);
    single_mie_scattering = pushConstant.luminance_from_radiance * delta_mie;
}