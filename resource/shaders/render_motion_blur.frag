#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform sampler2D textureSceneColor;
layout(binding = 3) uniform sampler2D textureSceneVelocity;

layout(location = 0) in vec4 vertexColor;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

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


void main() {
    outColor = vec4(0.0);
    float motion_blur_scale = 0.002 / scene_constants.DELTA_TIME;
    vec2 velocity = texture(textureSceneVelocity, texCoord).xy * motion_blur_scale;
    float weights = 0.0;
    for( int i = 0; i < 7; i++ )
	{
	    vec2 uv = vec2(texCoord.x + gaussFilter[i].x * velocity.x, texCoord.y + gaussFilter[i].x * velocity.y);
        outColor += texture(textureSceneColor, uv) * gaussFilter[i].yyyy;
		weights += gaussFilter[i].y;
	}

    outColor /= weights;
    outColor.w = 1.0;
}