#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "../common/scene_constants.glsl"
#include "../common/utility.glsl"
#include "precomputed_atmosphere_common.glsl"
#include "atmosphere_common.glsl"
#include "render_atmosphere_common.glsl"

layout (location = 0) in VERTEX_OUTPUT vs_output;

layout (location = 0) out vec4 fs_output;

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

// Atmospheric transmittance from p_cam along eye_dir to outer space (handles inside-atmosphere & space limb rays)
vec3 ComputeCelestialTransmittance(vec3 p_cam, vec3 eye_dir)
{
    float r = length(p_cam);
    float rmu = dot(p_cam, eye_dir);
    if (r <= ATMOSPHERE.top_radius)
    {
        float mu = ClampCosine(rmu / max(r, 1e-4));
        if (RayIntersectsGround(ATMOSPHERE, r, mu))
        {
            return vec3(0.0);
        }
        return GetTransmittanceToTopAtmosphereBoundary(ATMOSPHERE, transmittance_texture, ClampRadius(ATMOSPHERE, r), mu);
    }

    float disc_top = rmu * rmu - r * r + ATMOSPHERE.top_radius * ATMOSPHERE.top_radius;
    if (disc_top > 0.0)
    {
        float d_top = -rmu - sqrt(disc_top);
        if (d_top > 0.0)
        {
            vec3 p_entry = p_cam + eye_dir * d_top;
            float r_e = ATMOSPHERE.top_radius;
            float mu_e = ClampCosine(dot(p_entry, eye_dir) / r_e);
            if (RayIntersectsGround(ATMOSPHERE, r_e, mu_e))
            {
                return vec3(0.0);
            }
            return GetTransmittanceToTopAtmosphereBoundary(ATMOSPHERE, transmittance_texture, r_e, mu_e);
        }
    }
    return vec3(1.0);
}

void main()
{
    vec2 texcoord = vs_output.uv.xy;
    float device_depth = texture(texture_depth, texcoord).x;
    vec4 color = max(textureLod(texture_atmosphere, texcoord, 0.0), vec4(0.0));

    vec3 eye_dir = normalize(vs_output.eye_ray);
    vec3 sun_dir = normalize(-light_data.LIGHT_DIRECTION.xyz);
    vec3 moon_dir = -sun_dir;

    // Camera position in Earth-centered km (consistent with render_atmosphere.frag)
    vec3 p_cam_raw = view_constants.CAMERA_POSITION.xyz * ATMOSPHERE_RATIO - atmosphere_constants.earth_center;
    float p_cam_raw_r = length(p_cam_raw);
    vec3 p_cam = (p_cam_raw_r < ATMOSPHERE.bottom_radius + 0.01)
        ? normalize(p_cam_raw_r > 0.001 ? p_cam_raw : vec3(0.0, 1.0, 0.0)) * (ATMOSPHERE.bottom_radius + 0.01)
        : p_cam_raw;
    float cam_alt_km = max(0.0, length(p_cam) - ATMOSPHERE.bottom_radius);

    // Sky / Outer-Space pixels (depth == 0.0 in Reverse-Z): composite crisp full-res Sun, Textured Moon & Milky Way behind atmosphere & clouds
    if (device_depth == 0.0)
    {
        const bool is_light_probe = (0 != pushConstant._render_light_probe_mode);
        vec2 earth_hit = IntersectSphere(p_cam, eye_dir, ATMOSPHERE.bottom_radius);
        bool occluded_by_earth = (earth_hit.x > 0.0) && (cam_alt_km > 1.5) && !is_light_probe;

        if (!occluded_by_earth)
        {
            vec3 T_atm = ComputeCelestialTransmittance(p_cam, eye_dir);
            float cloud_vis = 1.0 - saturate(color.a);
            float space_vis = smoothstep(25.0, 90.0, cam_alt_km);
            float night_vis = smoothstep(0.08, -0.10, sun_dir.y);

            // Precompute Moon disc occlusion mask so Milky Way stars never shine through the Moon body
            const float MOON_ANGULAR_RADIUS = 0.042;
            float VdotM = dot(eye_dir, moon_dir);
            float moon_angle = acos(clamp(VdotM, -1.0, 1.0));
            float moon_aa = max(fwidth(moon_angle), 0.0008);
            float moon_disc_geom = (!is_light_probe && VdotM > 0.90)
                ? (1.0 - smoothstep(MOON_ANGULAR_RADIUS - moon_aa, MOON_ANGULAR_RADIUS + moon_aa, moon_angle))
                : 0.0;

            // 1. Milky Way & Deep-Space Stars (Equirectangular panorama behind atmosphere, clouds, and Moon)
            if (!is_light_probe && cloud_vis > 0.005)
            {
                float sky_luma = get_luminance(color.rgb);
                float star_vis = max(night_vis * exp(-sky_luma * 6.0), space_vis * exp(-sky_luma * 2.0));
                if (star_vis > 0.005)
                {
                    float phi = atan(eye_dir.z, eye_dir.x);
                    float theta = asin(clamp(eye_dir.y, -1.0, 1.0));
                    vec2 mw_uv = vec2(phi * (1.0 / TWO_PI) + 0.5, 0.5 - theta * (1.0 / PI));
                    vec3 mw_srgb = texture(texture_milky_way, mw_uv).rgb;
                    vec3 mw_linear = pow(max(mw_srgb, vec3(0.0)), vec3(2.2));

                    // Subtle atmospheric scintillation (fades out in space vacuum)
                    float twinkle_phase = dot(floor(eye_dir * 420.0), vec3(12.9898, 78.233, 37.719));
                    float twinkle = mix(0.84 + 0.16 * sin(scene_constants.TIME * 3.2 + twinkle_phase), 1.0, space_vis);

                    vec3 star_trans = mix(T_atm, vec3(1.0), space_vis * step(0.999, T_atm.r));
                    color.rgb += mw_linear * twinkle * star_vis * star_trans * cloud_vis * (1.0 - moon_disc_geom) * mix(1.15, 1.45, space_vis);
                }
            }

            // 2. Full-Resolution HDR Sun Disc + Solar Corona (drives Karis Bloom & God Rays)
            float VdotS = dot(eye_dir, sun_dir);
            if (VdotS > 0.85 && cloud_vis > 0.005)
            {
                const float SUN_ANGULAR_RADIUS = 0.022;
                float sun_angle = acos(clamp(VdotS, -1.0, 1.0));
                float sun_aa = max(fwidth(sun_angle), 0.0006);
                float sun_disc_mask = 1.0 - smoothstep(SUN_ANGULAR_RADIUS - sun_aa, SUN_ANGULAR_RADIUS + sun_aa, sun_angle);

                float mu_sun = sqrt(max(0.0, 1.0 - pow(min(sun_angle / SUN_ANGULAR_RADIUS, 1.0), 2.0)));
                vec3 limb_tint = mix(vec3(1.0, 0.80, 0.56), vec3(1.0, 0.99, 0.96), pow(mu_sun, 0.45));

                float sun_corona = exp(-sun_angle * 22.0) * 0.90 + exp(-sun_angle * 6.5) * 0.18;
                float sun_core_intensity = is_light_probe ? 2.2 : 18.0;
                float corona_scale = is_light_probe ? 0.35 : 1.0;

                vec3 sun_color = light_data.LIGHT_COLOR.xyz * T_atm;
                vec3 sun_contrib = (sun_disc_mask * limb_tint * sun_core_intensity + sun_corona * vec3(1.0, 0.92, 0.78) * corona_scale) * sun_color;
                color.rgb += sun_contrib * cloud_vis;
            }

            // 3. Full-Resolution Textured Moon + Lunar Halo behind atmosphere & clouds
            if (!is_light_probe && VdotM > 0.90 && cloud_vis > 0.005)
            {
                vec3 up_ref = (abs(moon_dir.y) < 0.98) ? vec3(0.0, 1.0, 0.0) : vec3(1.0, 0.0, 0.0);
                vec3 moon_right = normalize(cross(up_ref, moon_dir));
                vec3 moon_up = cross(moon_dir, moon_right);

                vec2 moon_plane = vec2(dot(eye_dir, moon_right), dot(eye_dir, moon_up)) / sin(MOON_ANGULAR_RADIUS);
                float moon_r2 = dot(moon_plane, moon_plane);

                // Soft silver-blue Lunar Halo (subtler in space vacuum)
                float halo_atm = mix(1.0, 0.20, space_vis);
                float moon_halo = (exp(-moon_angle * 16.0) * 0.28 + exp(-moon_angle * 5.5) * 0.08) * halo_atm * max(night_vis, space_vis * 0.40);
                color.rgb += vec3(0.72, 0.82, 0.98) * T_atm * moon_halo * cloud_vis;

                if (moon_r2 < 1.08 && moon_disc_geom > 0.001)
                {
                    vec2 moon_uv = clamp(moon_plane * 0.5 + 0.5, vec2(0.002), vec2(0.998));
                    vec4 moon_tex = texture(texture_moon, moon_uv);
                    vec3 moon_albedo = pow(max(moon_tex.rgb, vec3(0.0)), vec3(2.2));

                    // Spherical limb darkening + soft 3D gibbous relief shading
                    float moon_z = sqrt(max(0.0, 1.0 - min(moon_r2, 1.0)));
                    vec3 moon_normal = normalize(vec3(moon_plane.x, moon_plane.y, moon_z));
                    vec3 moon_light_local = normalize(vec3(0.26, 0.22, 0.94));
                    float moon_shade = mix(0.65, 1.08, saturate(dot(moon_normal, moon_light_local)));
                    float limb_dark = pow(moon_z, 0.28);

                    float moon_mask = moon_disc_geom * moon_tex.a;
                    float moon_brightness = mix(0.65, 2.35, max(night_vis, space_vis));
                    vec3 moon_tint = vec3(0.98, 0.97, 0.95);
                    vec3 moon_lit = moon_albedo * moon_shade * limb_dark * moon_tint * moon_brightness * T_atm;
                    color.rgb += moon_lit * moon_mask * cloud_vis;
                }
            }
        }

        fs_output = vec4(max(color.rgb, vec3(0.0)), 1.0);
        return;
    }

    float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    float distance_ratio = clamp(linear_depth / view_constants.NEAR_FAR.y, 0.0, 1.0);
    vec3 inscatter_color = texture(texture_inscatter, texcoord).xyz * pow(distance_ratio, atmosphere_constants.inscatter_power);

    // Near/mid-range Exponential Height Fog + Sun/Moon-Tinted Aerial Perspective (fades out at high altitude / space)
    vec4 relative_pos = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION, texcoord, device_depth);
    float ray_dist = length(relative_pos.xyz);
    float world_y = relative_pos.y + view_constants.CAMERA_POSITION.y;
    float height_above_sea = max(0.0, world_y - scene_constants.SEA_HEIGHT);
    float height_falloff = exp(-height_above_sea * 0.015);
    float alt_fog_fade = 1.0 - smoothstep(3.5, 16.0, cam_alt_km);

    const float FOG_MAX_OPACITY = 0.72;
    float fog_dist = max(0.0, ray_dist - atmosphere_constants.fog_start_distance);
    float fog_factor = clamp(1.0 - exp(-fog_dist * atmosphere_constants.fog_density * mix(0.45, 1.0, height_falloff)), 0.0, FOG_MAX_OPACITY) * alt_fog_fade;

    float sun_amount = saturate(dot(eye_dir, sun_dir));
    float moon_amount = saturate(dot(eye_dir, moon_dir));
    float sun_height = smoothstep(-0.08, 0.25, sun_dir.y);
    float sunset_factor = smoothstep(-0.08, 0.04, sun_dir.y) * (1.0 - smoothstep(0.08, 0.30, sun_dir.y));
    float night_factor = smoothstep(0.06, -0.10, sun_dir.y) * smoothstep(-0.04, 0.18, moon_dir.y);
    float weather_scale = clamp(get_luminance(light_data.LIGHT_COLOR.xyz) / 1.2, 0.25, 1.5);

    // Blend aerial fog toward sky color with warm sunset haze, sunlit Mie glow, and cool night moonlight glow
    vec3 sunset_haze = vec3(1.10, 0.56, 0.32) * weather_scale * 0.55;
    vec3 day_haze = mix(atmosphere_constants.fog_tint * 0.65 * weather_scale, color.xyz * 0.85, 0.65);
    day_haze = mix(day_haze, sunset_haze, sunset_factor * 0.45);
    vec3 night_haze = scene_constants.MOON_LIGHT_COLOR * 0.35;
    vec3 sun_glow = vec3(1.15, 0.88, 0.58) * light_data.LIGHT_COLOR.xyz * pow(sun_amount, 6.0) * 0.45 * sun_height;
    vec3 moon_glow = scene_constants.MOON_LIGHT_COLOR * pow(moon_amount, 6.0) * 0.35 * night_factor;
    vec3 aerial_fog_color = mix(night_haze, day_haze, sun_height) + sun_glow + moon_glow;

    float far_blend = saturate(pow(distance_ratio, 2.0)) * alt_fog_fade;
    float final_alpha = saturate(max(far_blend, fog_factor));

    // PreMultipliedAlpha blending: src_color * 1.0 + dst_color * (1.0 - src_alpha)
    vec3 fog_contribution = mix(aerial_fog_color, color.xyz, far_blend) * final_alpha;
    fs_output.xyz = fog_contribution + inscatter_color * (1.0 - final_alpha);
    fs_output.w = final_alpha;
}
