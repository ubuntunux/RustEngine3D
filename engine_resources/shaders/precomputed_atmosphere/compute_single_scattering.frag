#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"
#include "precomputed_atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 delta_rayleigh;
layout(location = 1) out vec3 delta_mie;
layout(location = 2) out vec4 scattering;
layout(location = 3) out vec3 single_mie_scattering;

void main()
{
    ComputeSingleScatteringtexture2D(
        ATMOSPHERE,
        transmittance_texture,
        vec3(gl_FragCoord.xy, float(pushConstant._layer) + 0.5),
        delta_rayleigh,
        delta_mie
    );
    scattering = vec4(pushConstant._luminance_from_radiance * delta_rayleigh.rgb, (pushConstant._luminance_from_radiance * delta_mie).r);

#if 0 == COMBINED_SCATTERING_TEXTURES
    single_mie_scattering = pushConstant._luminance_from_radiance * delta_mie;
#endif
}