#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"
#include "precomputed_atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 scattering_density;

void main()
{
    scattering_density = ComputeScatteringDensitytexture2D(
        ATMOSPHERE,
        transmittance_texture,
        single_rayleigh_scattering_texture,
        single_mie_scattering_texture,
        multiple_scattering_texture,
        irradiance_texture,
        vec3(gl_FragCoord.xy, float(pushConstant._layer) + 0.5),
        pushConstant._scattering_order
    );
}