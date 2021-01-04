#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../scene_constants.glsl"
#include "../utility.glsl"
#include "../render_quad_common.glsl"

layout(binding = 0) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 1) uniform sampler2D texture_atmosphere;
layout(binding = 2) uniform sampler2D texture_inscatter;
layout(binding = 3) uniform sampler2D texture_depth;

layout( push_constant ) uniform PushConstant_CompositeAtmosphere
{
    int above_the_cloud;
    float inscatter_power;
    int reserved0;
    int reserved1;
} pushConstant;

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout (location = 0) out vec4 fs_output;

void main()
{
    vec2 texcoord = vs_output.texCoord.xy;
    float device_depth = texture(texture_depth, texcoord).x;
    float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    vec4 color = textureLod(texture_atmosphere, texcoord, 0.0);

    color.w = max(color.w, 0.0);

    float depth_ratio = clamp(linear_depth / view_constants.NEAR_FAR.y, 0.0, 1.0);

    if(0 != pushConstant.above_the_cloud)
    {
        //color.w = (NEAR_FAR.y <= linear_depth) ? 1.0 : color.w;
        color.w = saturate(max(pow(depth_ratio, 2.0), color.w));
    }
    else
    {
        color.w = saturate(pow(depth_ratio, 2.0));
    }

    // for blending : src_color * one + dst_color * (1.0 - src_alpha)
    //color.w = saturate(max(pow(depth_ratio, 2.0), color.w));
    color.xyz *= color.w;

    // Upscaling Inscatter
    vec2 inv_lod_texel_size = 1.0 / textureSize(texture_depth, 2);
    float fixed_device_depth = texture(texture_depth, texcoord).x;
    float fixed_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, fixed_device_depth);
    //float fixed_linear_depth = textureLod(texture_linear_depth, texcoord, 2.0).x;
    float fixed_depth_diff = abs(linear_depth - fixed_linear_depth);
    vec2 fixed_texcoord = (floor(texcoord / inv_lod_texel_size) + 0.5) * inv_lod_texel_size;
    vec2 fixed_uv = fixed_texcoord;

    vec2 offset[8] = {
        vec2(-inv_lod_texel_size.x, 0.0),
        vec2(inv_lod_texel_size.x, 0.0),
        vec2(0.0, inv_lod_texel_size.y),
        vec2(0.0, -inv_lod_texel_size.y),
        vec2(inv_lod_texel_size.x, inv_lod_texel_size.y),
        vec2(-inv_lod_texel_size.x, inv_lod_texel_size.y),
        vec2(inv_lod_texel_size.x, -inv_lod_texel_size.y),
        vec2(-inv_lod_texel_size.x, -inv_lod_texel_size.y),
    };

    for(int i=0; i<8; ++i)
    {
        float lod_device_depth = texture(texture_depth, fixed_texcoord + offset[i]).x;
        float lod_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, lod_device_depth);
        //float lod_linear_depth = textureLod(texture_linear_depth, fixed_texcoord + offset[i], 2.0).x;
        float depth_diff = abs(linear_depth - lod_linear_depth);
        if(depth_diff < fixed_depth_diff)
        {
            fixed_linear_depth = lod_linear_depth;
            fixed_depth_diff = depth_diff;
            fixed_uv = fixed_texcoord + offset[i];
        }
    }

    // add inscatter
    color.xyz += texture(texture_inscatter, fixed_uv).xyz * pow(depth_ratio, pushConstant.inscatter_power);

    fs_output = color;
}