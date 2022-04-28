#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 delta_irradiance;
layout(location = 1) out vec3 irradiance;

void main()
{
    delta_irradiance = ComputeDirectIrradiancetexture2D(ATMOSPHERE, transmittance_texture, gl_FragCoord.xy);
    irradiance = vec3(0.0);
}