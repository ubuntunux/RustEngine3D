#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "shading.glsl"
#include "fft_ocean/render_fft_ocean_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout (location = 0) out vec4 fs_output;

void main()
{
    vec2 uv = vs_output.uvs.xy;
    vec2 fft_uv = vs_output.uvs.zw;
    vec2 screen_texcoord = (vs_output.proj_pos.xy / vs_output.proj_pos.w) * 0.5 + 0.5;
    vec3 view_center_ray = vec3(VIEW_ORIGIN[0].z, VIEW_ORIGIN[1].z, VIEW_ORIGIN[2].z);
    float screen_fade = pow(vs_output.screen_fade, 100.0f);

    vec3 relative_pos = vs_output.relative_pos;
    vec3 world_pos = relative_pos + CAMERA_POSITION.xyz;
    float dist = length(relative_pos);
    vec3 V = -relative_pos / dist;
    float view_ray_angle = dot(view_center_ray, V);

    float scene_dist = texture(texture_linear_depth, screen_texcoord).x / view_ray_angle;
    float vertex_noise = vs_output.vertex_noise;

    // fix scene_depth
    vec3 tempPos = -V * scene_dist;
    tempPos.xz = mix(relative_pos.xz, tempPos.xz, abs(V.y));
    scene_dist = length(tempPos.xyz);

    vec3 sun_irradiance = vs_output.sun_irradiance;
    vec3 sky_irradiance = vs_output.sky_irradiance;
    vec3 shadow_factor = max(sky_irradiance, vec3(vs_output.shadow_factor));

    vec2 slopes = textureLod(fftWavesSampler, vec3(uv / simulation_size.x, 1.0), 0.0).xy;
    slopes += textureLod(fftWavesSampler, vec3(uv / simulation_size.y, 1.0), 0.0).zw;
    slopes += textureLod(fftWavesSampler, vec3(uv / simulation_size.z, 2.0), 0.0).xy;
    slopes += textureLod(fftWavesSampler, vec3(uv / simulation_size.w, 2.0), 0.0).zw;

    vec3 vertex_normal = normalize(vs_output.vertex_normal);
    vec3 N = normalize(vec3(-slopes.x, 1.0, -slopes.y) + vertex_normal * 0.2);
    vec3 smooth_normal = normalize(vec3(-slopes.x, 1.0, -slopes.y) + vertex_normal * 0.5);

    vec3 L = LIGHT_DIRECTION.xyz;
    vec3 H = normalize(V + L);

    float NdL = max(0.0, dot(N, L));
    float NdV = max(0.0, dot(N, V));
    float NdH = max(0.0, dot(N, H));
    float HdV = max(0.0, dot(H, V));
    float LdV = max(0.0, dot(L, V));

    vec3 F0 = vec3(0.04);
    float fresnel = fresnelSchlick(max(0.2, dot(smooth_normal, V)), F0).x;

    // refract
    vec2 reflected_screen_uv = screen_texcoord + N.xz * 0.05f;
    float refracted_scene_dist = texture(texture_linear_depth, reflected_screen_uv).x / view_ray_angle;
    float refracted_scene_dist_origin = refracted_scene_dist;

    // fix refractedSceneDepth
    tempPos = -V * refracted_scene_dist;
    tempPos.xz = mix(relative_pos.xz, tempPos.xz, abs(V.y));
    refracted_scene_dist = length(tempPos.xyz);

    float dist_diff = max(0.0f, max(scene_dist, refracted_scene_dist) - dist);

    // groud pos
    vec3 groundPos = world_pos - V * dist_diff + vec3(N.x, 0.0f, N.z) * 0.5f;

    bool isUnderWater = CAMERA_POSITION.y < height;
    float opacity = pow(saturate(dist_diff * 0.3), 1.0) * screen_fade;
    float inv_opacity = 1.0f - opacity;

    vec3 light_color = LIGHT_COLOR.xyz * sun_irradiance;

    // Water Base Color
    vec3 sea_color_near = vec3(198.0, 230.0, 213.0) / 255.0;
    vec3 sea_color_mid = vec3(121.0, 176.0, 188.0) / 255.0;
    vec3 sea_color_far = vec3(58.0, 47.0, 99.0) / 255.0;
    vec3 water_color;
    {
        water_color = mix(sea_color_near, sea_color_mid, sqrt(saturate(dist * 0.5)));
        water_color = mix(water_color, sea_color_far, sqrt(saturate(dist * 0.05)));
        water_color = pow(water_color, vec3(2.2));
    }

    // Reflection
    vec3 R = reflect(-V, smooth_normal);
    vec3 scene_reflect_color = textureLod(texture_probe, invert_y(R), 0.0).xyz;

    // Under Water
    vec3 under_water_color = textureLod(texture_scene, (refracted_scene_dist <= dist) ? screen_texcoord : reflected_screen_uv, 0.0).xyz;
    {
        // Under Water Caustic
        if(false == isUnderWater)
        {
            vec3 under_water_shadow = vec3(get_shadow_factor_simple(screen_texcoord, world_pos, dot(L, vertex_normal.xyz), texture_shadow));
            under_water_shadow = max(sky_irradiance, under_water_shadow);

            const float chromaSeperation = sin(t * 3.5f) * 0.005;
            vec2 caustic_uv = (groundPos + L * dist_diff).xz * 0.3 + vertex_normal.xz * 0.5;

            vec3 caustic_color;
            caustic_color.r = texture(texture_caustic, caustic_uv + vec2(0.0f, chromaSeperation)).r;
            caustic_color.g = texture(texture_caustic, caustic_uv + vec2(chromaSeperation, 0.0f)).g;
            caustic_color.b = texture(texture_caustic, caustic_uv - vec2(chromaSeperation, chromaSeperation)).b;
            caustic_color *= under_water_shadow * sun_irradiance * screen_fade * saturate(dist_diff) * 2.0;

            // apply caustic
            under_water_color += caustic_color;
        }

        float fog_ratio = saturate(abs(refracted_scene_dist_origin) * 0.05f);
        vec3 fog_color = mix(under_water_color, sea_color_far, fog_ratio * fog_ratio);

        under_water_color = mix(under_water_color, water_color * under_water_color, opacity);
        under_water_color = mix(fog_color, under_water_color, screen_fade) * inv_opacity;
    }

    // White cap
    float wave_peak = pow(saturate((vs_output.wave_offset.y * 0.5 + 0.5) * 1.7 + saturate(1.0f - N.y) * 2.0), 12.0f);
    float white_cap = saturate(wave_peak * simulation_wind);

    // Transmission
    float transmission = wave_peak * 2.0;

    // Foam
    vec3 foam = vec3(0.0);
    {
        foam = pow(texture(texture_foam, uv * 0.3 + vertex_noise * 0.2).xyz, vec3(2.2));
        foam += pow(texture(texture_foam, uv * -0.05 + vertex_noise * 0.3).xyz, vec3(2.2));
        foam *= 0.5;

        float foam_amount = saturate(inv_opacity * saturate(dist_diff) + white_cap * 1.0);
        float sharpen = mix(0.6, 0.1, foam_amount);
        foam = mix(vec3(0.0), foam, saturate((foam.x - sharpen) / (1.0f - sharpen) * 3.0f)) * foam_amount;
    }

    // Specular
    vec3 light_fresnel = fresnelSchlick(HdV, F0);
    float roughness = 0.1;
    float specular_light = cooktorrance_specular(light_fresnel, NdL, NdV, NdH, roughness).x * 2.0;

    // scattering
    vec3 scattering_normal = normalize(vec3(vertex_normal.x, 5.0, vertex_normal.z));
    float scattering = pow(abs(dot(H, scattering_normal)), 20.0) * (0.5 - dot(scattering_normal, V) * 0.5);
    specular_light += scattering;
    transmission += scattering * 10.0;

    float foam_lum = dot(vec3(0.3f, 0.59f, 0.11f), foam);
    specular_light *= saturate(1.0f - foam_lum * 2.0f);
    fresnel *= saturate(1.0f - foam_lum * 2.0f);

    vec3 diffuse_lighting = max(sky_irradiance, vec3(dot(N, L) * 0.5 + 0.5));

    fs_output.xyz = (mix(water_color, scene_reflect_color, fresnel) + foam) * diffuse_lighting + specular_light;
    fs_output.xyz += transmission * water_color;
    fs_output.xyz *= light_color * shadow_factor;

    // final output
    opacity = saturate(opacity + (foam_lum + specular_light + fresnel + transmission * 0.01f)) * saturate(dist_diff);
    fs_output.xyz = mix(under_water_color.xyz, fs_output.xyz, opacity * screen_fade);
    fs_output.w = 1.0;
}