#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "blending.glsl"
#include "PCFKernels.glsl"
#include "shading.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform LightData
{
    LIGHT_DATA light_data;
};
layout(binding = 3) uniform sampler2D textureSceneNormal;
layout(binding = 4) uniform sampler2D textureSceneDepth;
layout(binding = 5) uniform sampler2D ssaoNoise;
layout(binding = 6) uniform sampler2D texture_shadow;

// uniform_buffer_data - struct ShadowAOConstants
layout(binding = 7) uniform ShadowAOConstants
{
    vec4 SSAO_KERNEL_SAPLES[SSAO_KERNEL_SIZE];
} uboSSAOKernel;

layout(binding = 8) uniform AtmosphereConstants
{
    ATMOSPHERE_CONSTANTS atmosphere_constants;
};
layout(binding = 9) uniform sampler3D texture_cloud;

layout(location = 0) in VERTEX_OUTPUT vs_output;

// x: direct sun shadow factor, y: GTAO visibility, zw: octahedral world-space bent normal
layout(location = 0) out vec4 outColor;

// Monotonic antiderivative of |sin(theta)| * cos(theta - n) over [-PI, PI] (Jimenez et al. 2016 GTAO)
float gtao_arc_antiderivative(float theta, float n)
{
    float g_pos = 0.25 * (-cos(2.0 * theta - n) + cos(n) + 2.0 * theta * sin(n));
    return (theta >= 0.0) ? g_pos : -g_pos;
}

void main() {
    vec2 texCoord = vs_output.texCoord;
    float device_depth = texture(textureSceneDepth, texCoord).x;
    if (0.0 == device_depth)
    {
        outColor = vec4(1.0, 1.0, 0.5, 0.5);
        return;
    }

    const vec2 depth_size = vec2(textureSize(textureSceneDepth, 0).xy);
    const vec2 inv_depth_size = 1.0 / depth_size;
    // Use exact integer pixel coordinates of this render target (avoids 1.333x integer-skipping moire/checkerboard during Upscale)
    const ivec2 screen_pos = ivec2(gl_FragCoord.xy);
    const vec4 relative_pos = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, texCoord, device_depth);
    const vec3 N = normalize(texture(textureSceneNormal, texCoord).xyz * 2.0 - 1.0);
    const vec3 V = normalize(-relative_pos.xyz);

    float gtao_visibility = 1.0;
    vec3 bent_normal = N;

    if ((scene_constants.RENDER_OPTION & RenderOption_RenderSSAO) != 0)
    {
        const float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
        const float GTAO_WORLD_RADIUS = 1.8;
        const float proj_scale = abs(view_constants.PROJECTION[1][1]) * 0.5 * depth_size.y;
        const float screen_radius = clamp((GTAO_WORLD_RADIUS * proj_scale) / max(linear_depth, 0.25), 4.0, 48.0);

        const int SLICE_COUNT = 4;
        const int STEP_COUNT = 4;
        const float frame_rot = float(scene_constants.ELAPSED_FRAME & 3u) * 0.25;
        const float spatial_noise = interleaved_gradient_noise(screen_pos);
        const float base_phi = fract(spatial_noise + frame_rot) * PI;
        const float step_jitter = fract(interleaved_gradient_noise(screen_pos + ivec2(37, 17)) + frame_rot * 0.5);

        const vec3 cam_right = normalize(view_constants.INV_VIEW_ORIGIN[0].xyz);
        const vec3 cam_up = normalize(view_constants.INV_VIEW_ORIGIN[1].xyz);
        const float inv_radius2 = 1.0 / (GTAO_WORLD_RADIUS * GTAO_WORLD_RADIUS);

        float ao_sum = 0.0;
        vec3 bent_sum = vec3(0.0);

        for (int s = 0; s < SLICE_COUNT; ++s)
        {
            float phi = base_phi + (float(s) / float(SLICE_COUNT)) * PI;
            vec2 slice_dir_ss = vec2(cos(phi), sin(phi));
            vec3 slice_dir_ws = normalize(cam_right * slice_dir_ss.x - cam_up * slice_dir_ss.y);
            vec3 ortho_dir = safe_normalize(slice_dir_ws - V * dot(slice_dir_ws, V));
            vec3 plane_normal = safe_normalize(cross(ortho_dir, V));

            vec3 proj_N = N - plane_normal * dot(N, plane_normal);
            float proj_N_len = length(proj_N);
            if (proj_N_len < 0.0001)
            {
                ao_sum += 1.0;
                bent_sum += N;
                continue;
            }
            vec3 proj_N_norm = proj_N / proj_N_len;
            float cos_n = clamp(dot(proj_N_norm, V), -1.0, 1.0);
            float n_angle = sign(dot(proj_N_norm, ortho_dir)) * acos(cos_n);

            float max_cos_h_pos = -1.0;
            float max_cos_h_neg = -1.0;

            for (int step_idx = 0; step_idx < STEP_COUNT; ++step_idx)
            {
                float t = (float(step_idx) + 0.35 + 0.65 * step_jitter) / float(STEP_COUNT);
                float sample_px = max(1.0, t * t * screen_radius);
                vec2 uv_offset = slice_dir_ss * sample_px * inv_depth_size;

                // Positive side
                vec2 uv_pos = texCoord + uv_offset;
                if (uv_pos.x >= 0.0 && uv_pos.x <= 1.0 && uv_pos.y >= 0.0 && uv_pos.y <= 1.0)
                {
                    float d_pos = texture(textureSceneDepth, uv_pos).x;
                    if (d_pos > 0.0)
                    {
                        vec3 rel_pos_s = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, uv_pos, d_pos).xyz;
                        vec3 delta = rel_pos_s - relative_pos.xyz;
                        float dist2 = dot(delta, delta);
                        if (dist2 > 0.0001)
                        {
                            float falloff = saturate(1.0 - dist2 * inv_radius2);
                            float cos_h = dot(delta * inversesqrt(dist2), V);
                            max_cos_h_pos = max(max_cos_h_pos, mix(-1.0, cos_h, falloff));
                        }
                    }
                }

                // Negative side
                vec2 uv_neg = texCoord - uv_offset;
                if (uv_neg.x >= 0.0 && uv_neg.x <= 1.0 && uv_neg.y >= 0.0 && uv_neg.y <= 1.0)
                {
                    float d_neg = texture(textureSceneDepth, uv_neg).x;
                    if (d_neg > 0.0)
                    {
                        vec3 rel_neg_s = relative_world_from_device_depth(view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER, uv_neg, d_neg).xyz;
                        vec3 delta = rel_neg_s - relative_pos.xyz;
                        float dist2 = dot(delta, delta);
                        if (dist2 > 0.0001)
                        {
                            float falloff = saturate(1.0 - dist2 * inv_radius2);
                            float cos_h = dot(delta * inversesqrt(dist2), V);
                            max_cos_h_neg = max(max_cos_h_neg, mix(-1.0, cos_h, falloff));
                        }
                    }
                }
            }

            float h1 = max(-acos(clamp(max_cos_h_neg, -1.0, 1.0)), n_angle - HALF_PI);
            float h2 = min( acos(clamp(max_cos_h_pos, -1.0, 1.0)), n_angle + HALF_PI);
            h1 = min(h1, h2);

            float vis_full = max(0.001, gtao_arc_antiderivative(n_angle + HALF_PI, n_angle) - gtao_arc_antiderivative(n_angle - HALF_PI, n_angle));
            float vis_slice = saturate((gtao_arc_antiderivative(h2, n_angle) - gtao_arc_antiderivative(h1, n_angle)) / vis_full);
            ao_sum += vis_slice;

            float bent_angle = 0.5 * (h1 + h2);
            vec3 bent_dir_slice = V * cos(bent_angle) + ortho_dir * sin(bent_angle);
            bent_sum += proj_N_len * bent_dir_slice;
        }

        gtao_visibility = clamp(ao_sum / float(SLICE_COUNT), 0.18, 1.0);
        if (length(bent_sum) > 0.001)
        {
            bent_normal = normalize(mix(N, normalize(bent_sum), 0.75));
        }
    }

    const vec3 world_position = relative_pos.xyz + view_constants.CAMERA_POSITION;
    const float pixel_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
    float shadow_factor = 1.0;
    const bool enable_shadow = ((scene_constants.RENDER_OPTION & RenderOption_RenderShadow) != 0u)
        && (light_data.CSM_CASCADE_COUNT_ACTIVE > 0);
    if (enable_shadow)
    {
        if ((scene_constants.RENDER_OPTION & RenderOption_RenderPCSS) != 0u)
        {
            shadow_factor = get_shadow_factor_pcss_csm(
                scene_constants.TIME,
                screen_pos,
                world_position,
                pixel_linear_depth,
                light_data,
                light_data.SHADOW_SAMPLES,
                SHADOW_BIAS,
                texture_shadow
            );
        }
        else
        {
            shadow_factor = get_shadow_factor_csm(
                scene_constants.TIME,
                screen_pos,
                world_position,
                pixel_linear_depth,
                light_data,
                light_data.SHADOW_SAMPLES,
                SHADOW_BIAS,
                texture_shadow
            );
        }
    }

    // Screen-Space Contact Shadows: short 8-step ray march toward sun for crisp grounding on grass, rocks, and feet
    if (enable_shadow && (scene_constants.RENDER_OPTION & RenderOption_RenderContactShadow) != 0u && shadow_factor > 0.01)
    {
        const vec3 L = normalize(-light_data.LIGHT_DIRECTION);
        const float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, device_depth);
        if (L.y > 0.05 && linear_depth < 25.0 && dot(N, L) > 0.02)
        {
            const int CONTACT_STEPS = 8;
            const float max_ray_len = mix(0.28, 0.85, saturate(linear_depth / 20.0));
            const float thickness = 0.045 + linear_depth * 0.012;
            const float frame_jitter = fract(interleaved_gradient_noise(screen_pos + ivec2(13, 29)) + float(scene_constants.ELAPSED_FRAME & 3u) * 0.25);
            const vec3 ray_origin = relative_pos.xyz + N * (0.015 + linear_depth * 0.002);

            float contact_occlusion = 0.0;
            for (int step_i = 0; step_i < CONTACT_STEPS; ++step_i)
            {
                float t = (float(step_i) + 0.35 + 0.65 * frame_jitter) / float(CONTACT_STEPS);
                vec3 sample_rel_pos = ray_origin + L * (t * max_ray_len);
                vec4 proj = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(sample_rel_pos, 1.0);
                if (proj.w <= 0.0)
                {
                    break;
                }
                proj.xyz /= proj.w;
                vec2 sample_uv = proj.xy * 0.5 + 0.5;
                if (sample_uv.x <= 0.0 || sample_uv.x >= 1.0 || sample_uv.y <= 0.0 || sample_uv.y >= 1.0)
                {
                    break;
                }
                float sample_depth = textureLod(textureSceneDepth, sample_uv, 0.0).x;
                if (sample_depth <= 0.0)
                {
                    continue;
                }
                float ray_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, proj.z);
                float scene_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, sample_depth);
                float depth_diff = ray_linear - scene_linear;
                if (depth_diff > 0.015 && depth_diff < thickness)
                {
                    contact_occlusion = 1.0 - t * 0.35;
                    break;
                }
            }
            float dist_fade = smoothstep(25.0, 14.0, linear_depth);
            shadow_factor *= (1.0 - contact_occlusion * dist_fade * 0.85);
        }
    }

    // Cloud Shadow Projection: project world position along sun ray onto overhead 3D cloud deck
    if (enable_shadow
        && (scene_constants.RENDER_OPTION & RenderOption_RenderSky) != 0u
        && atmosphere_constants.cloud_coverage > 0.28
        && shadow_factor > 0.01)
    {
        const vec3 L = normalize(-light_data.LIGHT_DIRECTION);
        float sun_elev_fade = smoothstep(0.05, 0.22, L.y);
        if (sun_elev_fade > 0.001)
        {
            float cloud_mid_alt = atmosphere_constants.cloud_altitude + atmosphere_constants.cloud_height * 0.35;
            float t_cloud = clamp((cloud_mid_alt - world_position.y) / max(L.y, 0.18), 0.0, 45000.0);
            vec3 cloud_hit = world_position + L * t_cloud;

            vec3 cloud_scale = vec3(textureSize(texture_cloud, 0));
            cloud_scale = max(cloud_scale.x, max(cloud_scale.y, cloud_scale.z)) / max(cloud_scale, vec3(1.0));
            cloud_scale *= atmosphere_constants.cloud_tiling;

            vec3 speed = vec3(atmosphere_constants.cloud_speed, atmosphere_constants.cloud_speed, 0.0) * scene_constants.TIME;
            vec3 uv_offset = speed * atmosphere_constants.cloud_tiling / max(atmosphere_constants.noise_tiling, 1e-6);

            vec3 uvw0 = vec3(cloud_hit.x, cloud_hit.z, atmosphere_constants.cloud_height * 0.25) * cloud_scale + uv_offset;
            vec3 uvw1 = vec3(cloud_hit.x, cloud_hit.z, atmosphere_constants.cloud_height * 0.60) * cloud_scale + uv_offset;
            float c0 = textureLod(texture_cloud, uvw0, 1.0).x;
            float c1 = textureLod(texture_cloud, uvw1, 1.0).x;
            float cloud_raw = c0 * 0.6 + c1 * 0.4;
            float cloud_density = saturate(Contrast(cloud_raw - 1.0 + atmosphere_constants.cloud_coverage, atmosphere_constants.cloud_contrast));
            float cloud_shadow = mix(1.0, 0.38, smoothstep(0.06, 0.85, cloud_density) * sun_elev_fade);
            shadow_factor *= cloud_shadow;
        }
    }

    outColor = vec4(saturate(shadow_factor), gtao_visibility, oct_encode_normal(bent_normal));
}
