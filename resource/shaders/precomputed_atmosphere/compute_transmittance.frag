#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "../utility.glsl"
#include "atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec3 transmittance;

void main()
{
    transmittance = ComputeTransmittanceToTopAtmosphereBoundarytexture2D(ATMOSPHERE, gl_FragCoord.xy);
}