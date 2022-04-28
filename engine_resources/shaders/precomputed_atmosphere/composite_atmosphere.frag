#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout (location = 0) out vec4 fs_output;

void main()
{
    vec2 texcoord = vs_output.uv.xy;
    float device_depth = texture(texture_depth, texcoord).x;
    float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    float distance_ratio = clamp(linear_depth / view_constants.NEAR_FAR.y, 0.0, 1.0);
    vec4 color = max(textureLod(texture_atmosphere, texcoord, 0.0), vec4(0.0));
    vec3 inscatter_color = texture(texture_inscatter, texcoord).xyz * pow(distance_ratio, atmosphere_constants.inscatter_power);

    if(atmosphere_constants.cloud_altitude < view_constants.CAMERA_POSITION.y)
    {
        //color.w = (NEAR_FAR.y <= linear_depth) ? 1.0 : color.w;
        color.w = saturate(max(pow(distance_ratio, 2.0), color.w));
    }
    else
    {
        color.w = saturate(pow(distance_ratio, 2.0));
    }

    // for blending : src_color * one + dst_color * (1.0 - src_alpha)
    fs_output.xyz = color.xyz * color.w + inscatter_color;
    fs_output.w = color.w;
}