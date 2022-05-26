#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "render_fft_ocean_common.glsl"
#include "../common/shading.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout (location = 0) out vec4 fs_output;

void main()
{
    vec2 uv = vs_output.uvs.xy;
    vec2 fft_uv = vs_output.uvs.zw;
    vec2 screen_texcoord = (vs_output.proj_pos.xy / vs_output.proj_pos.w) * 0.5 + 0.5;
    vec3 view_center_ray = vec3(view_constants.VIEW_ORIGIN[0].z, view_constants.VIEW_ORIGIN[1].z, view_constants.VIEW_ORIGIN[2].z);
    float screen_fade = pow(vs_output.screen_fade, 100.0f);

    vec3 relative_pos = vs_output.relative_pos;
    vec3 world_pos = relative_pos + view_constants.CAMERA_POSITION.xyz;
    float dist = length(relative_pos);
    vec3 V = -relative_pos / dist;
    float view_ray_angle = dot(view_center_ray, V);

    float device_depth = texture(texture_depth, screen_texcoord).x;
    float scene_dist = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth) / view_ray_angle;
    float vertex_noise = vs_output.vertex_noise;

    // fix scene_depth
    vec3 tempPos = -V * scene_dist;
    tempPos.xz = mix(relative_pos.xz, tempPos.xz, abs(V.y));
    scene_dist = length(tempPos.xyz);

    vec3 sun_irradiance = vs_output.sun_irradiance;
    vec3 sky_irradiance = vs_output.sky_irradiance;

    vec2 slopes = textureLod(fftWavesSampler, vec3(uv / pushConstant._simulation_size.x, 1.0), 0.0).xy;
    slopes += textureLod(fftWavesSampler, vec3(uv / pushConstant._simulation_size.y, 1.0), 0.0).zw;
    slopes += textureLod(fftWavesSampler, vec3(uv / pushConstant._simulation_size.z, 2.0), 0.0).xy;
    slopes += textureLod(fftWavesSampler, vec3(uv / pushConstant._simulation_size.w, 2.0), 0.0).zw;

    const vec3 vertex_normal = normalize(vs_output.vertex_normal);
    const vec3 N = normalize(vec3(-slopes.x, 1.0, -slopes.y) + vertex_normal * 0.2);
    const vec3 smooth_normal = normalize(vec3(-slopes.x, 1.0, -slopes.y) + vertex_normal * 0.5);
    const vec3 L = light_constants.LIGHT_DIRECTION.xyz;
    const vec3 H = normalize(V + L);
    const float NdL = dot(N, L);
    const float clampled_NdL = clamp(NdL, 0.0, 1.0);
    const float NdV = clamp(dot(N, V), 0.001, 1.0);
    const float NdH = clamp(dot(N, H), 0.001, 1.0);
    const float HdV = clamp(dot(H, V), 0.001, 1.0);
    const float LdV = clamp(dot(L, V), 0.001, 1.0);
    const vec3 R = reflect(-V, N);
    const vec3 smoothR = reflect(-V, smooth_normal);

    // refract
    vec2 reflected_screen_uv = screen_texcoord + N.xz * 0.05f;
    float reflected_device_depth = texture(texture_depth, reflected_screen_uv).x;
    float refracted_scene_dist = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, reflected_device_depth) / view_ray_angle;
    float refracted_scene_dist_origin = refracted_scene_dist;

    // fix refractedSceneDepth
    tempPos = -V * refracted_scene_dist;
    tempPos.xz = mix(relative_pos.xz, tempPos.xz, abs(V.y));
    refracted_scene_dist = length(tempPos.xyz);

    float dist_diff = max(0.0f, max(scene_dist, refracted_scene_dist) - dist);

    // groud pos
    vec3 groundPos = world_pos - V * dist_diff + vec3(N.x, 0.0f, N.z) * 0.5f;

    bool isUnderWater = view_constants.CAMERA_POSITION.y < scene_constants.SEA_HEIGHT;
    float opacity = saturate(1.0 - exp(-dist_diff * 0.2)) * screen_fade;
    float inv_opacity = 1.0f - opacity;

    // Water Color
    const vec3 sea_color_near = pow(vec3(15.0, 60.0, 45.0) / 255.0, vec3(2.2)) * 0.5;
    const vec3 sea_color_far = pow(vec3(15.0, 60.0, 60.0) / 255.0, vec3(2.2)) * 0.5;
    vec3 water_color = mix(sea_color_near, sea_color_far, saturate(1.0 - exp(-dist * 0.025)));

    // Under Water
    vec3 under_water_color = textureLod(texture_scene, (refracted_scene_dist <= dist) ? screen_texcoord : reflected_screen_uv, 0.0).xyz;
    {
        // Under Water Caustic
        if(false == isUnderWater)
        {
            vec3 under_water_shadow = vec3(get_shadow_factor(
                0.0,
                ivec2(screen_texcoord * scene_constants.SCREEN_SIZE),
                world_pos,
                light_constants.SHADOW_VIEW_PROJECTION,
                8,
                0.0,
                texture_shadow
            ));
            under_water_shadow = max(sky_irradiance, under_water_shadow);

            const float chromaSeperation = sin(pushConstant._t * 3.5f) * 0.0025;
            vec3 caustic_uv = vec3((groundPos + L * dist_diff).xz * 0.3 + vertex_normal.xz * 0.5, scene_constants.TIME);
            vec3 caustic_color;
            caustic_color.r = texture(texture_caustic, caustic_uv + vec3(0.0f, chromaSeperation, 0.0)).r;
            caustic_color.g = texture(texture_caustic, caustic_uv + vec3(chromaSeperation, 0.0f, 0.0)).g;
            caustic_color.b = texture(texture_caustic, caustic_uv - vec3(chromaSeperation, chromaSeperation, 0.0)).b;
            caustic_color *= under_water_shadow * sun_irradiance * screen_fade * saturate(dist_diff);

            // apply caustic
            under_water_color += caustic_color;
        }

        float fog_ratio = saturate(abs(refracted_scene_dist_origin) * 0.05f);
        vec3 fog_color = mix(under_water_color, sea_color_far, fog_ratio * fog_ratio);

        under_water_color = mix(under_water_color, water_color * under_water_color, opacity);
        under_water_color = mix(fog_color, under_water_color, screen_fade) * pow(inv_opacity, 4.0);
    }

    // White cap
    float wave_peak = pow(saturate((vs_output.wave_offset.y * 0.5 + 0.5) * 1.7 + saturate(1.0f - N.y) * 2.0), 12.0f);
    float white_cap = saturate(wave_peak * pushConstant._simulation_wind);

    // Foam
    float foam_amount = 0.0;
    {
        const vec3 foam_albedo = vec3(0.5);
        float sea_coast_line = saturate(inv_opacity * saturate(dist_diff) + white_cap * 1.0);
        float sharpen = mix(0.6, 0.1, sea_coast_line);
        vec3 foam = pow(texture(texture_foam, uv * 0.3 + vertex_noise * 0.2).xyz, vec3(2.2));
        foam += pow(texture(texture_foam, uv * -0.05 + vertex_noise * 0.3).xyz, vec3(2.2));
        foam *= 0.5;
        foam_amount = saturate((foam.x - sharpen) / (1.0f - sharpen) * 2.0f * sea_coast_line);
        water_color = mix(water_color, foam_albedo, saturate(foam * foam_amount));
    }

    // Lighting
    const vec3 F0 = vec3(0.04);
    float fresnel = fresnelSchlick(max(0.2, dot(smooth_normal, V)), F0).x;
    vec3 diffuse_light = vec3(0.0, 0.0, 0.0);
    vec3 specular_light = vec3(0.0, 0.0, 0.0);
    float roughness = clamp(abs(fresnel), 0.1, 0.8);
    float roughness2 = roughness * roughness;
    vec4 scene_reflect_color = vec4(0.0);
    vec3 light_color = mix(sky_irradiance, sun_irradiance, vec3(vs_output.shadow_factor));
    const float sky_visibility = 1.0;

    // Image based lighting
    apply_image_based_lighting(
        texture_probe,
        scene_reflect_color,
        sky_irradiance,
        vec3(vs_output.shadow_factor),
        sky_visibility,
        roughness,
        F0,
        L,
        N,
        smoothR,
        NdV,
        diffuse_light,
        specular_light
    );

    // Directional Light
    {
        const vec3 F = fresnelSchlick(HdV, F0);
        diffuse_light += oren_nayar(roughness2, clampled_NdL, NdV, N, V, L) * (vec3(1.0) - F) * light_color;
        specular_light += cooktorrance_specular(F, clampled_NdL, NdV, NdH, roughness) * clampled_NdL * light_color;
    }

    // scattering
    vec3 scattering_normal = normalize(vec3(vertex_normal.x, 5.0, vertex_normal.z));
    float scattering = pow(abs(dot(H, scattering_normal)), 20.0) * (0.5 - dot(scattering_normal, V) * 0.5);
    scattering = clamp(wave_peak * scattering, 0.0, 1.0);
    float sea_color_max_intensity = max(sea_color_near.x, max(sea_color_near.y, sea_color_near.z));
    vec3 transmission = light_color * sea_color_near / sea_color_max_intensity * 0.5;

    fs_output.xyz = diffuse_light * water_color + transmission * scattering + specular_light;

    // final output
    float specular_intensity = max(specular_light.x, max(specular_light.y, specular_light.z));
    opacity = saturate(opacity + (foam_amount + specular_intensity + fresnel) * saturate(dist_diff));
    fs_output.xyz = mix(under_water_color.xyz, fs_output.xyz, opacity * screen_fade);
    fs_output.w = 1.0;
}