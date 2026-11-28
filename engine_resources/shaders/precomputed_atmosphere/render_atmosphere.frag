#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/blending.glsl"
#include "../common/utility.glsl"
#include "precomputed_atmosphere_common.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"

layout(location = 0) in VERTEX_OUTPUT vs_output;

layout(location = 0) out vec4 out_color;
layout(location = 1) out vec4 out_inscatter;

// Ray-sphere intersection for sphere centered at origin (all distances in km).
// Returns vec2(-1.0) on miss.
vec2 IntersectSphere(vec3 ro, vec3 rd, float radius)
{
    float b = dot(ro, rd);
    float c = dot(ro, ro) - radius * radius;
    float disc = b * b - c;
    if (disc < 0.0)
    {
        return vec2(-1.0);
    }
    float s = sqrt(disc);
    return vec2(-b - s, -b + s);
}

// https://www.astro.umd.edu/~jph/HG_note.pdf HG, g in [-1,1]
float HenyeyGreensteinPhaseFunction(float cos_theta, float g)
{
    float g2 = g * g;
    return (1.0 / (4.0 * PI)) * (1.0 - g2) / pow(max(1.0 + g2 - 2.0 * g * cos_theta, 0.0001), 1.5);
}

float calc_Fr_c(float cos_theta, float sunset_boost)
{
    // Dual-lobe HG phase: strong forward scattering silver lining + soft back-scattering lobe
    float g_fwd = mix(0.45, 0.72, sunset_boost);
    return mix(
        HenyeyGreensteinPhaseFunction(cos_theta, g_fwd),
        HenyeyGreensteinPhaseFunction(cos_theta, -0.3),
        0.28
    ) * 4.0 * PI;
}

float get_cloud_density_ground(vec3 cloud_scale, vec3 noise_scale, vec3 world_uvw, vec3 speed, float weight)
{
    float cloud = texture(texture_cloud, world_uvw * cloud_scale + speed * atmosphere_constants.cloud_tiling / max(atmosphere_constants.noise_tiling, 1e-6)).x;
    cloud = saturate(Contrast((cloud - 1.0 + atmosphere_constants.cloud_coverage), atmosphere_constants.cloud_contrast));

    float noise = texture(texture_noise, world_uvw * noise_scale + speed * 0.3).x;
    noise = saturate(Contrast((noise - 1.0 + atmosphere_constants.noise_coverage) * weight, atmosphere_constants.noise_contrast));

    return saturate(Remap(noise, 1.0 - cloud, 1.0, 0.0, 1.0));
}

float get_cloud_density_spherical(vec3 n_sphere, float height_ratio, vec3 speed, float weight)
{
    vec3 wind = vec3(speed.x * 0.06, 0.0, speed.y * 0.06);
    vec3 macro_uvw = n_sphere * 2.3 + vec3(0.0, height_ratio * 0.14, 0.0) + wind;
    vec3 detail_uvw = n_sphere * 6.8 + vec3(0.0, height_ratio * 0.32, 0.0) + wind * 1.4;

    float cloud = texture(texture_cloud, macro_uvw).x;
    cloud = saturate(Contrast((cloud - 1.0 + atmosphere_constants.cloud_coverage), atmosphere_constants.cloud_contrast));

    float noise = texture(texture_noise, detail_uvw).x;
    noise = saturate(Contrast((noise - 1.0 + atmosphere_constants.noise_coverage) * weight, atmosphere_constants.noise_contrast));

    return saturate(Remap(noise, 1.0 - cloud, 1.0, 0.0, 1.0));
}

float sample_cloud_density(
    vec3 ray_pos_km,
    float relative_alt_m,
    float height_ratio,
    float weight,
    float space_factor,
    vec3 cloud_scale,
    vec3 noise_scale,
    vec3 speed
)
{
    float density = 0.0;
    if (space_factor < 0.999)
    {
        // Convert planet-centered km back to world-space meters around the playable origin
        vec3 pos_m = (ray_pos_km + atmosphere_constants.earth_center) / ATMOSPHERE_RATIO;
        vec3 world_uvw = vec3(pos_m.x, pos_m.z, relative_alt_m);
        density = get_cloud_density_ground(cloud_scale, noise_scale, world_uvw, speed, weight);
    }
    if (space_factor > 0.001)
    {
        vec3 n_sphere = normalize(ray_pos_km);
        float sph_density = get_cloud_density_spherical(n_sphere, height_ratio, speed, weight);
        density = (space_factor >= 0.999) ? sph_density : mix(density, sph_density, space_factor);
    }
    return density;
}

void main()
{
    out_color = vec4(0.0, 0.0, 0.0, 0.0);
    out_inscatter = vec4(0.0, 0.0, 0.0, 1.0);

    const bool is_render_light_probe_mode = 0 != pushConstant._render_light_probe_mode;
    const bool doLambertianReflectance = true;

    // Camera position in Earth-centered kilometers (1 world unit = 1 meter, ATMOSPHERE_RATIO = 0.001 km/m)
    vec3 p_cam_raw = view_constants.CAMERA_POSITION.xyz * ATMOSPHERE_RATIO - atmosphere_constants.earth_center;
    float p_cam_raw_r = length(p_cam_raw);
    vec3 p_cam = (p_cam_raw_r < ATMOSPHERE.bottom_radius + 0.01)
        ? normalize(p_cam_raw_r > 0.001 ? p_cam_raw : vec3(0.0, 1.0, 0.0)) * (ATMOSPHERE.bottom_radius + 0.01)
        : p_cam_raw;
    float p_cam_r = length(p_cam);
    float cam_alt_km = max(0.0, p_cam_r - ATMOSPHERE.bottom_radius);

    vec3 sun_direction = normalize(-light_data.LIGHT_DIRECTION.xyz);
    vec3 moon_direction = -sun_direction;
    vec3 eye_direction = normalize(vs_output.eye_ray);
    float VdotL = dot(eye_direction, sun_direction);
    float VdotM = dot(eye_direction, moon_direction);
    float weather_scale = clamp(get_luminance(light_data.LIGHT_COLOR.xyz) / 1.2, 0.25, 2.0);

    // Fade atmospheric ambient floor smoothly to 0 in outer space vacuum
    float atm_presence = 1.0 - smoothstep(35.0, 110.0, cam_alt_km);
    float space_factor = smoothstep(18.0, 80.0, cam_alt_km);
    float night_factor = smoothstep(0.08, -0.10, sun_direction.y);
    float moon_elevation = smoothstep(-0.08, 0.25, moon_direction.y);

    vec3 moon_light_sky_color = scene_constants.MOON_LIGHT_COLOR * 0.25 * mix(0.35, 1.0, moon_elevation);
    vec3 moon_light_color = moon_light_sky_color * 0.65 * atm_presence;

    float device_depth = texture(texture_depth, vs_output.uv).x;
    float scene_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    bool enable_atm_shadow = ((scene_constants.RENDER_OPTION & RenderOption_RenderShadow) != 0u)
        && (light_data.CSM_CASCADE_COUNT_ACTIVE > 0);
    int atm_cascade_idx = clamp(min(light_data.CSM_CASCADE_COUNT_ACTIVE - 1, 2), 0, CSM_CASCADE_COUNT - 1);
    float scene_shadow_length = (is_render_light_probe_mode || cam_alt_km > 15.0 || !enable_atm_shadow) ? 0.0 : GetSceneShadowLengthAtlas(
        atmosphere_constants,
        scene_linear_depth,
        view_constants.NEAR_FAR.y,
        view_constants.CAMERA_POSITION.xyz,
        eye_direction,
        sun_direction,
        light_data.SHADOW_VIEW_PROJECTION,
        light_data.CSM_ATLAS_RECTS[atm_cascade_idx],
        texture_shadow
    );

    // Sky & Atmospheric Limb Radiance
    vec3 transmittance;
    vec3 radiance = GetSkyRadiance(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        p_cam,
        eye_direction,
        scene_shadow_length,
        sun_direction,
        transmittance
    ) * weather_scale;
    radiance = max(radiance, moon_light_color);

    // Subtle cool silver-blue Lunar Mie scattering glow around the Moon at night inside atmosphere
    if (night_factor > 0.01 && moon_direction.y > -0.05 && atm_presence > 0.01)
    {
        float lunar_mie = pow(saturate(VdotM), 10.0) * 0.16 + pow(saturate(VdotM), 64.0) * 0.24;
        float horizon_boost = mix(1.35, 0.85, saturate(eye_direction.y * 2.5));
        radiance += scene_constants.MOON_LIGHT_COLOR * lunar_mie * night_factor * moon_elevation * atm_presence * horizon_boost * 0.45;
    }

    // Light probe lower hemisphere ground bounce (physically based warm terrain reflection)
    if (is_render_light_probe_mode && eye_direction.y < 0.08)
    {
        vec3 ground_sky_irradiance = vec3(0.0);
        vec3 ground_sun_irradiance = GetSunAndSkyIrradiance(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            irradiance_texture,
            p_cam,
            vec3(0.0, 1.0, 0.0),
            sun_direction,
            false,
            ground_sky_irradiance
        );
        vec3 ground_bounce_albedo = atmosphere_constants.ground_bounce_albedo;
        vec3 direct_ground = ground_sun_irradiance * max(sun_direction.y, 0.05) * light_data.LIGHT_COLOR.xyz;
        vec3 sky_ground = ground_sky_irradiance * weather_scale * 1.4;
        vec3 ground_radiance = (direct_ground + sky_ground) * (ground_bounce_albedo / PI);
        ground_radiance = max(ground_radiance, moon_light_color * 0.85);
        float ground_blend = smoothstep(0.08, -0.15, eye_direction.y);
        radiance = mix(radiance, ground_radiance, ground_blend);
    }

    // Spherical Earth planet surface for high-altitude / outer-space views
    vec2 earth_hit = IntersectSphere(p_cam, eye_direction, ATMOSPHERE.bottom_radius);
    bool hits_earth = (earth_hit.x > 0.0) && (device_depth == 0.0) && !is_render_light_probe_mode && (cam_alt_km > 1.5);
    if (hits_earth)
    {
        vec3 p_ground = p_cam + eye_direction * earth_hit.x;
        vec3 n_ground = normalize(p_ground);

        vec3 sph_uv = n_ground * 0.5 + 0.5;
        float c_macro = texture(texture_cloud, fract(sph_uv * 2.1 + vec3(0.13, 0.07, 0.29))).x;
        float c_detail = texture(texture_noise, fract(sph_uv * 6.5 + vec3(0.41, 0.19, 0.05))).x;
        float continent = c_macro * 0.72 + c_detail * 0.28;
        float lat = abs(n_ground.y);
        float polar_ice = smoothstep(0.76, 0.90, lat + (c_detail - 0.5) * 0.14);
        float land_mask = smoothstep(0.47, 0.53, continent);
        float desert_mask = smoothstep(0.55, 0.78, c_detail) * (1.0 - smoothstep(0.30, 0.58, lat));
        float mountain_mask = smoothstep(0.68, 0.84, continent);

        vec3 deep_ocean = vec3(0.012, 0.048, 0.125);
        vec3 shallow_ocean = vec3(0.025, 0.115, 0.210);
        vec3 ocean_albedo = mix(deep_ocean, shallow_ocean, smoothstep(0.36, 0.48, continent));

        vec3 forest_albedo = vec3(0.055, 0.135, 0.048);
        vec3 desert_albedo = vec3(0.26, 0.20, 0.12);
        vec3 mountain_albedo = vec3(0.18, 0.16, 0.14);
        vec3 land_albedo = mix(forest_albedo, desert_albedo, desert_mask);
        land_albedo = mix(land_albedo, mountain_albedo, mountain_mask * 0.65);

        vec3 surface_albedo = mix(ocean_albedo, land_albedo, land_mask);
        surface_albedo = mix(surface_albedo, vec3(0.62, 0.68, 0.74), polar_ice);

        vec3 earth_sky_irr = vec3(0.0);
        vec3 earth_sun_irr = GetSunAndSkyIrradiance(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            irradiance_texture,
            p_ground,
            n_ground,
            sun_direction,
            true,
            earth_sky_irr
        );

        vec3 surface_lit = surface_albedo * (earth_sun_irr * light_data.LIGHT_COLOR.xyz + earth_sky_irr * weather_scale * 0.85) / PI;

        // Ocean specular sun glint
        float ocean_mask = (1.0 - land_mask) * (1.0 - polar_ice);
        vec3 H_earth = normalize(sun_direction - eye_direction);
        float NoL_earth = saturate(dot(n_ground, sun_direction));
        float glint = pow(saturate(dot(n_ground, H_earth)), 48.0) * ocean_mask * NoL_earth;
        surface_lit += earth_sun_irr * light_data.LIGHT_COLOR.xyz * glint * 0.45;

        // Subtle moonlight on Earth's night hemisphere
        float night_hem = saturate(dot(n_ground, moon_direction));
        surface_lit += surface_albedo * scene_constants.MOON_LIGHT_COLOR * night_hem * 0.04;

        vec3 earth_transmittance;
        vec3 earth_inscatter = GetSkyRadianceToPoint(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            scattering_texture,
            single_mie_scattering_texture,
            p_cam,
            p_ground,
            0.0,
            sun_direction,
            earth_transmittance
        ) * weather_scale;

        vec3 planet_radiance = max(vec3(0.0), surface_lit * earth_transmittance + earth_inscatter);
        float earth_blend = smoothstep(1.5, 8.0, cam_alt_km);
        radiance = mix(radiance, planet_radiance, earth_blend);
    }

    // Spherical-shell Volumetric Clouds (works seamlessly from ground, inside clouds, above clouds, and outer space)
    bool render_cloud = ((scene_constants.RENDER_OPTION & RenderOption_RenderSky) != 0);
    vec3 cloud_color = vec3(0.0);
    float cloud_opacity = 0.0;

    if (render_cloud)
    {
        float cloud_alt_km = atmosphere_constants.cloud_altitude * ATMOSPHERE_RATIO;
        float cloud_height_km = max(atmosphere_constants.cloud_height * ATMOSPHERE_RATIO, 0.1);
        float R_cloud_bot = ATMOSPHERE.bottom_radius + cloud_alt_km;
        float R_cloud_top = R_cloud_bot + cloud_height_km;

        vec2 hit_bot = IntersectSphere(p_cam, eye_direction, R_cloud_bot);
        vec2 hit_top = IntersectSphere(p_cam, eye_direction, R_cloud_top);

        bool in_the_cloud = (p_cam_r >= R_cloud_bot) && (p_cam_r <= R_cloud_top);
        bool above_the_cloud = (p_cam_r > R_cloud_top);

        float t_enter = -1.0;
        float t_exit = -1.0;
        bool has_cloud_segment = false;

        if (in_the_cloud)
        {
            t_enter = view_constants.NEAR_FAR.x * ATMOSPHERE_RATIO;
            t_exit = (hit_bot.x > 0.0) ? hit_bot.x : hit_top.y;
            has_cloud_segment = (t_exit > t_enter);
        }
        else if (above_the_cloud)
        {
            if (hit_top.y > 0.0)
            {
                t_enter = max(0.0, hit_top.x);
                t_exit = (hit_bot.x > 0.0) ? hit_bot.x : hit_top.y;
                has_cloud_segment = (t_exit > t_enter);
            }
        }
        else
        {
            // Below cloud deck
            if (hit_bot.y > 0.0 && hit_top.y > hit_bot.y)
            {
                t_enter = hit_bot.y;
                t_exit = hit_top.y;
                has_cloud_segment = (earth_hit.x <= 0.0 || earth_hit.x > t_enter);
            }
        }

        if (has_cloud_segment)
        {
            if (earth_hit.x > 0.0)
            {
                t_exit = min(t_exit, earth_hit.x);
            }
            if (device_depth > 0.0)
            {
                t_exit = min(t_exit, scene_linear_depth * ATMOSPHERE_RATIO);
            }

            float max_enter_km = mix(110.0, 500000.0, space_factor);
            float max_chord_km = mix(24.0, 220.0, space_factor);
            t_exit = min(t_exit, t_enter + max_chord_km);

            if (t_enter < max_enter_km && t_exit > t_enter + 0.005)
            {
                vec3 p_cloud_mid = p_cam + eye_direction * mix(t_enter, t_exit, 0.35);
                vec3 n_cloud_mid = normalize(p_cloud_mid);
                float local_sun_elev = dot(n_cloud_mid, sun_direction);
                float local_moon_elev = dot(n_cloud_mid, moon_direction);

                vec3 cloud_sky_irradiance = vec3(0.0);
                vec3 cloud_sun_irradiance = GetSunAndSkyIrradiance(
                    ATMOSPHERE,
                    atmosphere_constants,
                    transmittance_texture,
                    irradiance_texture,
                    p_cloud_mid,
                    sun_direction,
                    sun_direction,
                    doLambertianReflectance,
                    cloud_sky_irradiance
                );

                vec3 cloud_transmittance = vec3(1.0);
                vec3 cloud_inscatter = vec3(0.0);
                if (!in_the_cloud)
                {
                    cloud_inscatter = GetSkyRadianceToPoint(
                        ATMOSPHERE,
                        atmosphere_constants,
                        transmittance_texture,
                        scattering_texture,
                        single_mie_scattering_texture,
                        p_cam,
                        p_cloud_mid,
                        scene_shadow_length,
                        sun_direction,
                        cloud_transmittance
                    ) * weather_scale;
                }

                cloud_sun_irradiance = max(vec3(0.0), cloud_sun_irradiance) * max(vec3(0.0), cloud_transmittance) / PI;
                cloud_sky_irradiance = max(vec3(0.0), cloud_sky_irradiance) * max(vec3(0.0), cloud_transmittance) / PI;

                // Terminator & Sunset / Twilight / Night lighting transition
                float sun_vis = smoothstep(-0.09, 0.14, local_sun_elev);
                float sunset_factor = smoothstep(-0.09, 0.03, local_sun_elev) * (1.0 - smoothstep(0.06, 0.28, local_sun_elev));
                float moon_vis = smoothstep(0.04, -0.08, local_sun_elev) * smoothstep(-0.06, 0.18, local_moon_elev);

                vec3 sunset_tint = mix(vec3(1.0), vec3(1.45, 0.72, 0.38), sunset_factor * 0.85);
                vec3 twilight_ambient_tint = mix(vec3(1.0), vec3(0.78, 0.68, 0.92), sunset_factor * 0.50);

                float atmosphere_lighting = mix(0.28, 1.0, saturate(local_sun_elev * 0.5 + 0.5));
                vec3 cloud_sun_light = cloud_sun_irradiance * light_data.LIGHT_COLOR.xyz * atmosphere_lighting * sunset_tint * sun_vis * 0.62;
                vec3 cloud_moon_light = scene_constants.MOON_LIGHT_COLOR * 0.42 * moon_vis * max(vec3(0.0), cloud_transmittance);
                vec3 cloud_direct_light = cloud_sun_light + cloud_moon_light;

                vec3 cloud_ambient_floor = moon_light_sky_color * mix(1.0, 0.08, space_factor);
                vec3 cloud_ambient_light = (max(cloud_sky_irradiance * weather_scale, cloud_ambient_floor) * 0.90
                    + radiance * (0.42 * (1.0 - space_factor))) * twilight_ambient_tint;

                // Switch volumetric light march direction smoothly to Moon at night
                bool march_to_moon = (local_sun_elev < -0.04) && (local_moon_elev > 0.0);
                vec3 cloud_light_dir = march_to_moon ? moon_direction : sun_direction;
                float phase_cos = march_to_moon ? VdotM : VdotL;
                float c0 = saturate(calc_Fr_c(phase_cos, march_to_moon ? 0.35 : sunset_factor));
                float c1 = saturate(calc_Fr_c(1.0, 0.0));
                float silver_lining = pow(saturate(phase_cos), 12.0) * (march_to_moon ? 0.55 : mix(0.25, 0.95, sunset_factor));

                const vec3 speed = vec3(atmosphere_constants.cloud_speed, atmosphere_constants.cloud_speed, 0.0) * scene_constants.TIME;

                vec3 cloud_scale = textureSize(texture_cloud, 0);
                cloud_scale = max(cloud_scale.x, max(cloud_scale.y, cloud_scale.z)) / cloud_scale;
                cloud_scale *= atmosphere_constants.cloud_tiling;

                vec3 noise_scale = textureSize(texture_noise, 0);
                noise_scale = max(noise_scale.x, max(noise_scale.y, noise_scale.z)) / noise_scale;
                noise_scale *= atmosphere_constants.noise_tiling;

                const int march_count = 48;
                const int light_march_count = 8;
                const float absorption_exp = atmosphere_constants.cloud_absorption * -8.0;
                float chord_km = t_exit - t_enter;
                float march_step_km = chord_km / float(march_count);
                float light_step_km = (cloud_height_km * 0.85) / float(light_march_count);
                // Golden-ratio temporal jitter lets atmosphere TAA integrate multiple raymarch offsets cleanly without static stipple
                float temporal_phase = float(scene_constants.ELAPSED_FRAME % 64u) * 0.61803398875;
                float step_jitter = fract(interleaved_gradient_noise(ivec2(gl_FragCoord.xy)) + temporal_phase) * march_step_km;

                float visibility = 1.0;
                for (int i = 0; i < march_count; ++i)
                {
                    float t_cur = t_enter + float(i) * march_step_km + step_jitter;
                    if (t_cur > t_exit)
                    {
                        break;
                    }

                    vec3 ray_pos_km = p_cam + eye_direction * t_cur;
                    float rel_alt_km = length(ray_pos_km) - R_cloud_bot;
                    if (rel_alt_km < 0.0 || rel_alt_km > cloud_height_km)
                    {
                        continue;
                    }

                    float height_ratio = saturate(rel_alt_km / cloud_height_km);
                    float fade = 1.0 - pow(abs(height_ratio * 2.0 - 1.0), 3.0);

                    float cloud_density = sample_cloud_density(
                        ray_pos_km,
                        rel_alt_km / ATMOSPHERE_RATIO,
                        height_ratio,
                        fade,
                        space_factor,
                        cloud_scale,
                        noise_scale,
                        speed
                    );
                    if (cloud_density <= 0.005)
                    {
                        continue;
                    }

                    float light_intensity = 1.0;
                    for (int j = 0; j < light_march_count; ++j)
                    {
                        vec3 light_pos_km = ray_pos_km + cloud_light_dir * (float(j + 1) * light_step_km);
                        float light_alt_km = length(light_pos_km) - R_cloud_bot;
                        if (light_alt_km < 0.0 || light_alt_km > cloud_height_km)
                        {
                            break;
                        }

                        float light_hr = saturate(light_alt_km / cloud_height_km);
                        float light_fade = 1.0 - pow(abs(light_hr * 2.0 - 1.0), 3.0);

                        float light_density = sample_cloud_density(
                            light_pos_km,
                            light_alt_km / ATMOSPHERE_RATIO,
                            light_hr,
                            light_fade,
                            space_factor,
                            cloud_scale,
                            noise_scale,
                            speed
                        );
                        light_intensity *= c1 * exp(light_density * absorption_exp);
                        if (light_intensity <= 0.005)
                        {
                            light_intensity = 0.0;
                            break;
                        }
                    }

                    // Normalize optical step weight relative to nominal vertical step
                    float step_optical_scale = clamp(march_step_km / max(cloud_height_km / float(march_count), 0.01), 0.5, 2.5);
                    float prev_visibility = visibility;
                    visibility *= exp(cloud_density * absorption_exp * step_optical_scale);
                    float step_opacity = prev_visibility - visibility;
                    cloud_opacity = 1.0 - visibility;

                    float direct_scatter = mix(0.18, 1.0, light_intensity) * (mix(0.85, 1.25, c0) + silver_lining * light_intensity);
                    float ambient_height = mix(0.55, 1.0, height_ratio);
                    vec3 sample_light = cloud_direct_light * direct_scatter + cloud_ambient_light * ambient_height;
                    cloud_color += step_opacity * sample_light;

                    if (visibility <= 0.005)
                    {
                        break;
                    }
                }

                // Soft horizon fade on ground only; disable in high-altitude / space view so Earth's curved cloud limb stays crisp
                float horizon_fade = mix(
                    pow(saturate(((eye_direction.y * 0.5 + 0.5) - 0.49) * 30.0), 0.1),
                    1.0,
                    smoothstep(4.0, 20.0, cam_alt_km)
                );
                cloud_opacity *= horizon_fade;
                cloud_color *= horizon_fade;

                // Apply atmospheric aerial veil in front of clouds
                if (!in_the_cloud)
                {
                    cloud_color += max(vec3(0.0), cloud_inscatter) * cloud_opacity * mix(0.35, 0.85, space_factor);
                }
            }
        }
    }

    out_color.xyz = max(vec3(0.0), cloud_color + radiance * (1.0 - cloud_opacity));
    out_color.w = clamp(cloud_opacity, 0.0, 1.0);

    // Inscattering (only for scene geometry, never double-add onto sky or light probe)
    if (0.0 == device_depth || is_render_light_probe_mode)
    {
        out_inscatter.xyz = vec3(0.0);
    }
    else
    {
        vec3 far_point = p_cam + eye_direction * max(view_constants.NEAR_FAR.x, scene_linear_depth) * ATMOSPHERE_RATIO;
        vec3 scene_transmittance;
        vec3 scene_inscatter = GetSkyRadianceToPoint(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            scattering_texture,
            single_mie_scattering_texture,
            p_cam,
            far_point,
            scene_shadow_length,
            sun_direction,
            scene_transmittance
        ) * weather_scale;

        out_inscatter.xyz = max(vec3(0.0), scene_inscatter);
    }
}
