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
    vec4 color = max(textureLod(texture_atmosphere, texcoord, 0.0), vec4(0.0));

    // Sky pixels and LightProbe capture (depth == 0.0 in Reverse-Z): write atmosphere directly with no double inscatter
    if (device_depth == 0.0)
    {
        fs_output = vec4(color.xyz, 1.0);
        return;
    }

    float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    float distance_ratio = clamp(linear_depth / view_constants.NEAR_FAR.y, 0.0, 1.0);
    vec3 inscatter_color = texture(texture_inscatter, texcoord).xyz * pow(distance_ratio, atmosphere_constants.inscatter_power);

    // Near/mid-range Exponential Height Fog + Sun-Tinted Aerial Perspective
    vec4 relative_pos = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION, texcoord, device_depth);
    float ray_dist = length(relative_pos.xyz);
    float world_y = relative_pos.y + view_constants.CAMERA_POSITION.y;
    float height_above_sea = max(0.0, world_y - scene_constants.SEA_HEIGHT);
    float height_falloff = exp(-height_above_sea * 0.015);

    const float FOG_START_DIST = 35.0;
    const float FOG_DENSITY = 0.0020;
    const float FOG_MAX_OPACITY = 0.72;
    float fog_dist = max(0.0, ray_dist - FOG_START_DIST);
    float fog_factor = clamp(1.0 - exp(-fog_dist * FOG_DENSITY * mix(0.45, 1.0, height_falloff)), 0.0, FOG_MAX_OPACITY);

    vec3 eye_dir = normalize(vs_output.eye_ray);
    vec3 sun_dir = normalize(-light_data.LIGHT_DIRECTION.xyz);
    float sun_amount = saturate(dot(eye_dir, sun_dir));
    float sun_height = smoothstep(-0.08, 0.25, sun_dir.y);
    float weather_scale = clamp(get_luminance(light_data.LIGHT_COLOR.xyz) / 1.2, 0.25, 1.5);

    // Blend aerial fog toward sky color with warm sunlit Mie forward-scattering glow
    vec3 day_haze = mix(vec3(0.42, 0.62, 0.88) * 0.65 * weather_scale, color.xyz * 0.85, 0.65);
    vec3 night_haze = scene_constants.MOON_LIGHT_COLOR * 0.35;
    vec3 sun_glow = vec3(1.15, 0.88, 0.58) * light_data.LIGHT_COLOR.xyz * pow(sun_amount, 6.0) * 0.45 * sun_height;
    vec3 aerial_fog_color = mix(night_haze, day_haze, sun_height) + sun_glow;

    float far_blend = saturate(pow(distance_ratio, 2.0));
    float final_alpha = saturate(max(far_blend, fog_factor));

    // PreMultipliedAlpha blending: src_color * 1.0 + dst_color * (1.0 - src_alpha)
    vec3 fog_contribution = mix(aerial_fog_color, color.xyz, far_blend) * final_alpha;
    fs_output.xyz = fog_contribution + inscatter_color * (1.0 - final_alpha);
    fs_output.w = final_alpha;
}