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

// https://www.astro.umd.edu/~jph/HG_note.pdf HG, g in [-1,1]
// note: multiply by 4*PI to integrate to 1 on the sphere, I think... (check with integral_of_spherical_func)
float HenyeyGreensteinPhaseFunction( float cos_theta, float g )
{
    float g2 = g * g;
    return ( 1.0 / ( 4.0 * 3.141592 ) ) * ( 1.0 - g2 ) / pow( 1.0 + g2 - 2.0 * g * cos_theta, 1.5 );
}

float calc_Fr_c( float cos_theta )
{
    // *4.0*PI is so that function integrates to 1 on the sphere
    // note: linear combination still integrate to 1
    return mix(HenyeyGreensteinPhaseFunction( cos_theta, 0.42 ), // normal phase (increase g to strengthen illumination in sun direction)
            HenyeyGreensteinPhaseFunction( cos_theta, -0.3 ), // hack boost clouds on the other side of the sun too..
        0.3 ) * 4.0 * 3.141592;
    // also see Physically based Sky, Atmosphere and Cloud Rendering in Frostbite:
    // http://blog.selfshadow.com/publications/s2016-shading-course/hillaire/s2016_pbs_frostbite_sky_clouds.pptx
}

float get_cloud_density(vec3 cloud_scale, vec3 noise_scale, vec3 uvw, vec3 speed, float weight)
{
    uvw.xy += view_constants.CAMERA_POSITION.xz;

    float cloud = texture(texture_cloud, uvw * cloud_scale + speed * atmosphere_constants.cloud_tiling / atmosphere_constants.noise_tiling).x;
    cloud = saturate(Contrast((cloud - 1.0 + atmosphere_constants.cloud_coverage), atmosphere_constants.cloud_contrast));

    float noise = texture(texture_noise, uvw * noise_scale + speed * 0.3).x;
    noise = saturate(Contrast((noise - 1.0 + atmosphere_constants.noise_coverage) * weight, atmosphere_constants.noise_contrast));

    // Remap is very important!!
    return saturate(Remap(noise, 1.0 - cloud, 1.0, 0.0, 1.0));
}

void main()
{
    out_color = vec4(0.0, 0.0, 0.0, 1.0);
    out_inscatter = vec4(0.0, 0.0, 0.0, 1.0);

    const bool is_render_light_probe_mode = 0 != pushConstant._render_light_probe_mode;
    const float far_dist = view_constants.NEAR_FAR.y * 4.0;

    vec3 camera = vec3(0.0, max(10.0, view_constants.CAMERA_POSITION.y), 0.0) * ATMOSPHERE_RATIO;

    float world_pos_y = max(0.0, view_constants.CAMERA_POSITION.y);

    vec3 sun_direction = -light_data.LIGHT_DIRECTION.xyz;
    vec3 eye_direction = normalize(vs_output.eye_ray);
    float VdotL = dot(eye_direction, sun_direction);

    float device_depth = texture(texture_depth, vs_output.uv).x;
    float scene_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    float scene_shadow_length = GetSceneShadowLength(
    atmosphere_constants,
    scene_linear_depth,
    view_constants.NEAR_FAR.y,
    view_constants.CAMERA_POSITION.xyz,
    eye_direction,
    sun_direction,
    light_data.SHADOW_VIEW_PROJECTION,
    texture_shadow
    );

    // Sky
    vec3 transmittance;
    vec3 radiance = GetSkyRadiance(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        camera - atmosphere_constants.earth_center,
        eye_direction,
        scene_shadow_length,
        sun_direction,
        transmittance
    );

    vec3 solar_radiance = GetSolarRadiance(ATMOSPHERE, atmosphere_constants);

    // Sun
    vec3 sun_disc = vec3(0.0);
    const float sun_absorption = 0.9;
    const float sun_disc_intensity = 20.0;
    if (false == is_render_light_probe_mode && atmosphere_constants.sun_size.y < VdotL && 0.0 == device_depth)
    {
        sun_disc = transmittance * solar_radiance.x * light_data.LIGHT_COLOR.xyz * sun_disc_intensity;
        sun_disc *= pow(clamp((VdotL - atmosphere_constants.sun_size.y) / (1.0 - atmosphere_constants.sun_size.y), 0.0, 1.0), 2.0);
    }

    // distance from earch center
    vec3 earth_center_pos = atmosphere_constants.earth_center / ATMOSPHERE_RATIO;
    const float cloud_bottom_dist = atmosphere_constants.cloud_altitude - earth_center_pos.y;
    const float cloud_top_dist = cloud_bottom_dist + atmosphere_constants.cloud_height;
    float altitude_diff = atmosphere_constants.cloud_altitude - world_pos_y;
    const bool in_the_cloud = -atmosphere_constants.cloud_height < altitude_diff && altitude_diff < 0.0;
    bool above_the_cloud = false;
    bool render_cloud = true;

    // relative ray march start pos from the camera
    vec3 ray_start_pos;
    float hit_dist;

    if(in_the_cloud)
    {
        // be in clouds
        hit_dist = view_constants.NEAR_FAR.x;
    }
    else
    {
        // https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
        vec3 to_origin = vec3(0.0, world_pos_y, 0.0) - earth_center_pos;
        float c = pow(dot(eye_direction, to_origin), 2.0) - dot(to_origin, to_origin);

        if(atmosphere_constants.cloud_altitude < world_pos_y)
        {
            // above the sky
            if(eye_direction.y < 0.0)
            {
                // look down
                c = -sqrt(c + cloud_top_dist * cloud_top_dist);
                above_the_cloud = true;
            }
            else
            {
                // look up, discard
                render_cloud = false;
            }
        }
        else
        {
            // under the sky
            float r = atmosphere_constants.cloud_altitude - earth_center_pos.y;
            c = sqrt(c + cloud_bottom_dist * cloud_bottom_dist);
        }

        hit_dist = -dot(eye_direction, to_origin) + c;
    }
    ray_start_pos = eye_direction * hit_dist;

    // apply altitude of camera
    ray_start_pos.y += world_pos_y;

    // vec3 smooth_N = normalize(ray_start_pos.xyz - earth_center);
    vec3 N = normalize(ray_start_pos.xyz);

    // Cloud
    vec3 cloud_color = vec3(0.0);
    float visibility = 1.0;
    float cloud_opacity = 0.0;

    if(render_cloud)
    {
        vec3 cloud_inscatter = vec3(0.0);
        vec3 cloud_sun_irradiance = vec3(0.0);
        vec3 cloud_sky_irradiance = vec3(0.0);

        // NOTE : 0.1 is more colorful scattering cloud.
        float dist_to_point = hit_dist * (above_the_cloud ? 1.0 : 0.01);
        GetCloudRadiance(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            irradiance_texture,
            scattering_texture,
            single_mie_scattering_texture,
            dist_to_point,
            view_constants.NEAR_FAR.x,
            view_constants.CAMERA_POSITION.xyz,
            eye_direction,
            sun_direction,
            scene_shadow_length,
            cloud_sun_irradiance,
            cloud_sky_irradiance,
            cloud_inscatter
        );

        if(in_the_cloud || above_the_cloud)
        {
            cloud_inscatter = vec3(0.0);
        }

        float altitude_ratio = saturate(world_pos_y / (atmosphere_constants.cloud_altitude + atmosphere_constants.cloud_height));
        float atmosphere_lighting = max(0.2, pow(saturate(dot(N, sun_direction) * 0.5 + 0.5), 1.0));
        vec3 light_color = (cloud_sun_irradiance + cloud_sky_irradiance);
        light_color *= atmosphere_constants.cloud_exposure * light_data.LIGHT_COLOR.xyz * atmosphere_lighting;

        const float c0 = saturate(calc_Fr_c(VdotL));
        const float c1 = saturate(calc_Fr_c(1.0));

        if(0.0 <= hit_dist && hit_dist < far_dist && hit_dist < scene_linear_depth)
        {
            const vec3 speed = vec3(atmosphere_constants.cloud_speed, atmosphere_constants.cloud_speed, 0.0) * scene_constants.TIME;

            vec3 cloud_scale = textureSize(texture_cloud, 0);
            cloud_scale = max(cloud_scale.x, max(cloud_scale.y, cloud_scale.z)) / cloud_scale;
            cloud_scale *= atmosphere_constants.cloud_tiling;

            vec3 noise_scale = textureSize(texture_noise, 0);
            noise_scale = max(noise_scale.x, max(noise_scale.y, noise_scale.z)) / noise_scale;
            noise_scale *= atmosphere_constants.noise_tiling;

            const int march_count = 32;
            const int light_march_count = 16;
            const float absorption_exp = atmosphere_constants.cloud_absorption * -8.0;
            float march_step = atmosphere_constants.cloud_height / float(march_count);
            float cloud_march_step = march_step;
            float increase_march_step = march_step * 0.05;
            float ray_start_dist = length(ray_start_pos.xyz);
            uint seed = uint(scene_constants.TIME * 1000.0);
            float step_noise = (interleaved_gradient_noise(ivec2(vs_output.uv * 1024) + ivec2(seed)) * 2.0 - 1.0) * march_step;
            for(int i = 0; i < march_count; ++i)
            {
                float ray_dist = float(i) * cloud_march_step;
                if (scene_linear_depth <= ((ray_start_dist + ray_dist) * ATMOSPHERE_RATIO))
                {
                    continue;
                }

                vec3 ray_pos = ray_start_pos.xyz + eye_direction.xyz * (ray_dist + step_noise);

                // fade top and bottom
                float relative_altitude = length(ray_pos - earth_center_pos.xyz) - cloud_bottom_dist;
                if(atmosphere_constants.cloud_height < relative_altitude || relative_altitude < 0.0)
                {
                    continue;
                }

                float fade = saturate(relative_altitude / atmosphere_constants.cloud_height);
                fade = 1.0 - pow(abs(fade * 2.0 - 1.0), 3.0);

                float cloud_density = get_cloud_density(cloud_scale, noise_scale, ray_pos.xzy, speed, fade);
                if(cloud_density <= 0.005)
                {
                    // increase march step
                    cloud_march_step += increase_march_step;
                    continue;
                }
                else
                {
                    // NOTE : decrease is more detail, but not natural.
                    //cloud_march_step = max(march_step, cloud_march_step - increase_march_step * 0.5);
                }

                float light_intensity = 1.0;
                for(int j = 0; j <= light_march_count; ++j)
                {
                    vec3 light_pos = ray_pos + sun_direction * float(j + 1.0) * march_step;
                    relative_altitude = length(light_pos.xyz - earth_center_pos.xyz) - cloud_bottom_dist;
                    if(atmosphere_constants.cloud_height < relative_altitude || relative_altitude < 0.0)
                    {
                        continue;
                    }

                    fade = saturate(relative_altitude / atmosphere_constants.cloud_height);
                    fade = 1.0 - pow(abs(fade * 2.0 - 1.0), 3.0);

                    float cloud_density_for_light = get_cloud_density(cloud_scale, noise_scale, light_pos.xzy, speed, fade);
                    light_intensity *= c1 * exp(cloud_density_for_light * absorption_exp);
                    if(light_intensity <= 0.005)
                    {
                        light_intensity = 0.0;
                        break;
                    }
                }

                visibility *= c0 * exp(cloud_density * absorption_exp);
                cloud_opacity = 1.0 - visibility;
                cloud_color += visibility * cloud_density * light_color * light_intensity;

                if(visibility <= 0.005 || i == (march_count - 1))
                {
                    break;
                }
            }

            float horizontal_line = pow(saturate(((N.y * 0.5 + 0.5) - 0.49) * 30.0), 0.1);
            cloud_opacity *= horizontal_line;
        }

        // atmosphere
        out_color.xyz += cloud_color * cloud_opacity * 20.0 + (radiance + sun_disc) * (1.0 - cloud_opacity);
        out_color.w = clamp(cloud_opacity, 0.0, 1.0);

        // inscattering
        vec3 far_point = camera + eye_direction.xyz * max(view_constants.NEAR_FAR.x, scene_linear_depth) * ATMOSPHERE_RATIO;
        vec3 scene_transmittance;
        vec3 scene_inscatter = GetSkyRadianceToPoint(
            ATMOSPHERE,
            atmosphere_constants,
            transmittance_texture,
            scattering_texture,
            single_mie_scattering_texture,
            camera - atmosphere_constants.earth_center,
            far_point.xyz - atmosphere_constants.earth_center,
            scene_shadow_length,
            sun_direction,
            scene_transmittance
        );

        out_inscatter.xyz = max(vec3(0.0), scene_inscatter);
    }
}
