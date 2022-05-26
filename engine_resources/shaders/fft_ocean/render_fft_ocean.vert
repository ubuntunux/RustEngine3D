#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "render_fft_ocean_common.glsl"
#include "../common/shading.glsl"

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inTangent;
layout(location = 3) in vec4 inColor;
layout(location = 4) in vec2 inTexCoord;

layout (location = 0) out VERTEX_OUTPUT vs_output;

vec3 oceanWorldPos(vec4 vertex)
{
    float height_diff = scene_constants.SEA_HEIGHT - view_constants.CAMERA_POSITION.y;
    vec3 cameraDir = normalize((view_constants.INV_PROJECTION * (vertex + vec4(view_constants.JITTER_OFFSET, 0.0, 0.0))).xyz);
    vec3 worldDir = (view_constants.INV_VIEW_ORIGIN * vec4(cameraDir, 0.0)).xyz;
    const float far_dist = view_constants.NEAR_FAR.y * 2.0;

    float dist = 0.0;

    if(0.0 < height_diff)
    {
        dist = (0.0 < worldDir.y) ? (height_diff / worldDir.y) : far_dist;
    }
    else
    {
        dist = (worldDir.y < 0.0) ? (height_diff / worldDir.y) : far_dist;
    }

    dist = min(far_dist, dist);

    vec3 world_pos = vec3(0.0, scene_constants.SEA_HEIGHT, 0.0);
    world_pos.xz += view_constants.CAMERA_POSITION.xz + dist * worldDir.xz;
    return world_pos;
}

void main()
{
    vec3 vertex_scale = vec3(1.5, 1.5, 1.0);
    vec4 vertex_pos = vec4(inPosition * vertex_scale, 1.0);
    vec3 world_pos = oceanWorldPos(vertex_pos);
    vec3 relative_pos = world_pos - view_constants.CAMERA_POSITION.xyz;
    float dist_xz = length(relative_pos.xz);
    float dist = length(relative_pos);

    vec2 u = world_pos.xz;
    vec2 ux = oceanWorldPos(vertex_pos + vec4(pushConstant._cell_size.x, 0.0, 0.0, 0.0)).xz;
    vec2 uy = oceanWorldPos(vertex_pos + vec4(0.0, pushConstant._cell_size.y, 0.0, 0.0)).xz;
    vec2 dux = abs(ux - u) * 2.0;
    vec2 duy = abs(uy - u) * 2.0;

    vec3 dP = vec3(0.0);
    dP.y += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.x, 0.0), dux / pushConstant._simulation_size.x, duy / pushConstant._simulation_size.x).x;
    dP.y += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.y, 0.0), dux / pushConstant._simulation_size.y, duy / pushConstant._simulation_size.y).y;
    dP.y += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.z, 0.0), dux / pushConstant._simulation_size.z, duy / pushConstant._simulation_size.z).z;
    dP.y += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.w, 0.0), dux / pushConstant._simulation_size.w, duy / pushConstant._simulation_size.w).w;

    dP.xz += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.x, 3.0), dux / pushConstant._simulation_size.x, duy / pushConstant._simulation_size.x).xy;
    dP.xz += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.y, 3.0), dux / pushConstant._simulation_size.y, duy / pushConstant._simulation_size.y).zw;
    dP.xz += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.z, 4.0), dux / pushConstant._simulation_size.z, duy / pushConstant._simulation_size.z).xy;
    dP.xz += textureGrad(fftWavesSampler, vec3(u / pushConstant._simulation_size.w, 4.0), dux / pushConstant._simulation_size.w, duy / pushConstant._simulation_size.w).zw;

    vec3 vertex_normal = vec3(-dP.x, dP.y * 0.5 + 0.5, -dP.z);
    vertex_normal = safe_normalize(mix(vec3(0.0, 1.0, 0.0), vertex_normal, saturate(pushConstant._simulation_amplitude)));

    world_pos += dP * pushConstant._simulation_amplitude;
    relative_pos = world_pos - view_constants.CAMERA_POSITION.xyz;

    vec3 eye_direction = normalize(relative_pos);
    vec4 proj_pos = view_constants.VIEW_PROJECTION * vec4(world_pos.xyz, 1.0);

    // water line
    float screen_fade = 1.0f - saturate(ceil(max(abs(inPosition.x), abs(inPosition.y)) - 0.999f));
    if(dist_xz < view_constants.NEAR_FAR.y && (1.0 <= vertex_pos.y || 1.0 <= vertex_pos.x || vertex_pos.x <= -1.0))
    {
        proj_pos.xy = mix(vertex_pos.xy * proj_pos.w, proj_pos.xy, screen_fade);
    }

    vec2 screen_coord = (proj_pos.xy / proj_pos.w) * 0.5 + 0.5;

    float vertex_noise = textureLod(texture_noise, world_pos.xz * 0.005, 0.0).x;
    float shadow_factor = get_shadow_factor(
        0.0,
        ivec2(screen_coord * scene_constants.SCREEN_SIZE),
        world_pos,
        light_constants.SHADOW_VIEW_PROJECTION,
        8,
        0.0,
        texture_shadow
    );

    // Atmosphere
    vec3 in_scatter = vec3(0.0);
    vec3 sun_irradiance = vec3(0.0);
    vec3 sky_irradiance = vec3(0.0);
    GetSceneRadiance(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        irradiance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        dist,
        view_constants.CAMERA_POSITION.xyz,
        eye_direction,
        light_constants.LIGHT_DIRECTION.xyz,
        vertex_normal,
        sun_irradiance,
        sky_irradiance,
        in_scatter
    );

    vs_output.sun_irradiance = sun_irradiance;
    vs_output.sky_irradiance = sky_irradiance;
    vs_output.in_scatter = in_scatter;
    vs_output.uvs.xy = u;
    vs_output.uvs.zw = world_pos.xz;
    vs_output.shadow_factor = shadow_factor;
    vs_output.vertex_noise = vertex_noise;
    vs_output.screen_fade = screen_fade;
    vs_output.wave_offset = dP;
    vs_output.vertex_normal = vertex_normal;
    vs_output.relative_pos = relative_pos;
    vs_output.proj_pos = proj_pos;
    gl_Position = proj_pos;
}