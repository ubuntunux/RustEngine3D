#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"
#include "render_quad_common.glsl"

const vec2 gaussFilter[7] =
{
	vec2(-3.0,	0.015625),
	vec2(-2.0,	0.09375),
	vec2(-1.0,	0.234375),
	vec2(0.0,	0.3125),
	vec2(1.0,	0.234375),
	vec2(2.0,	0.09375),
	vec2(3.0,	0.015625)
};

const float gaussFilter7x7[49] =
{
    0.00000067, 0.00002292,	0.00019117,	0.00038771,	0.00019117,	0.00002292,	0.00000067,
    0.00002292,	0.00078634,	0.00655965,	0.01330373,	0.00655965,	0.00078633,	0.00002292,
    0.00019117,	0.00655965,	0.05472157,	0.11098164,	0.05472157,	0.00655965,	0.00019117,
    0.00038771,	0.01330373,	0.11098164,	0.22508352,	0.11098164,	0.01330373,	0.00038771,
    0.00019117,	0.00655965,	0.05472157,	0.11098164,	0.05472157,	0.00655965,	0.00019117,
    0.00002292,	0.00078633,	0.00655965,	0.01330373,	0.00655965,	0.00078633,	0.00002292,
    0.00000067,	0.00002292,	0.00019117,	0.00038771,	0.00019117,	0.00002292,	0.00000067
};

layout( push_constant ) uniform PushConstant_GaussianBlur
{
    vec2 _blur_scale;
    uint _reserved0;
    uint _reserved1;
} pushConstant;

layout(binding = 0) uniform sampler2D textureSrc;

layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = 0) out vec4 outColor;

void main() {
    outColor = vec4(0.0, 0.0, 0.0, 1.0);
    vec2 texCoord = vs_output.texCoord.xy;
    vec2 blur_scale = pushConstant._blur_scale / textureSize(textureSrc, 0);
    for( int i = 0; i < 7; i++ )
	{
		outColor += texture(textureSrc, texCoord + gaussFilter[i].x * blur_scale) * gaussFilter[i].yyyy;
	}
}