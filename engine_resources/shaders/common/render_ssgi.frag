#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "utility.glsl"
#include "scene_constants.glsl"
#include "render_quad_common.glsl"

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};
layout(binding = 2) uniform sampler2D texture_random;
layout(binding = 3) uniform sampler2D texture_scene;
layout(binding = 4) uniform sampler2D texture_normal;
layout(binding = 5) uniform sampler2D texture_velocity;
layout(binding = 6) uniform sampler2D texture_depth;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

mat3 get_tangent_basis(vec3 N)
{
    vec3 up = abs(N.y) < 0.999 ? vec3(0.0, 1.0, 0.0) : vec3(0.0, 0.0, 1.0);
    vec3 tangent_x = normalize(cross(up, N));
    vec3 tangent_z = cross(N, tangent_x);
    return mat3(tangent_x, N, tangent_z);
}

vec3 sample_cosine_hemisphere(vec2 u)
{
    float phi = TWO_PI * u.x;
    float cos_theta = sqrt(saturate(1.0 - u.y));
    float sin_theta = sqrt(saturate(u.y));
    return vec3(cos(phi) * sin_theta, cos_theta, sin(phi) * sin_theta);
}

void main()
{
    outColor = vec4(0.0);

    vec2 texCoord = vs_output.texCoord.xy;
    float device_depth = texture(texture_depth, texCoord).x;
    if (device_depth <= 0.0)
    {
        return;
    }

    vec4 relative_pos = relative_world_from_device_depth(
        view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER,
        texCoord,
        device_depth
    );
    vec3 N = normalize(texture(texture_normal, texCoord).xyz * 2.0 - 1.0);
    mat3 tbn = get_tangent_basis(N);

    const ivec2 pixel_pos = ivec2(gl_FragCoord.xy);
    const float frame_seed = fract(float(scene_constants.ELAPSED_FRAME % 64u) * 0.61803398875);
    const float spatial_seed_x = interleaved_gradient_noise(pixel_pos);
    const float spatial_seed_y = interleaved_gradient_noise(pixel_pos + ivec2(59, 23));

    const int NUM_RAYS = 4;
    const int NUM_STEPS = 6;
    const float MAX_RAY_DIST = 6.5;
    const float THICKNESS_SCALE = 0.08;

    vec3 gi_sum = vec3(0.0);
    float hit_weight_sum = 0.0;

    for (int r = 0; r < NUM_RAYS; ++r)
    {
        vec2 u = vec2(
            fract(spatial_seed_x + frame_seed + float(r) / float(NUM_RAYS)),
            fract(spatial_seed_y + frame_seed * 1.6180339 + (float(r) + 0.5) * 0.381966)
        );
        vec3 ray_dir = normalize(tbn * sample_cosine_hemisphere(u));
        float step_jitter = fract(u.x * 7.31 + u.y * 13.17);

        for (int s = 0; s < NUM_STEPS; ++s)
        {
            float t = (float(s) + 0.35 + 0.65 * step_jitter) / float(NUM_STEPS);
            float march_dist = t * t * MAX_RAY_DIST;
            vec3 sample_rel_pos = relative_pos.xyz + ray_dir * march_dist + N * 0.05;

            vec4 proj = view_constants.VIEW_ORIGIN_PROJECTION_JITTER * vec4(sample_rel_pos, 1.0);
            if (proj.w <= 0.001)
            {
                break;
            }
            proj.xyz /= proj.w;
            vec2 sample_uv = proj.xy * 0.5 + 0.5;
            if (sample_uv.x < 0.0 || sample_uv.x > 1.0 || sample_uv.y < 0.0 || sample_uv.y > 1.0)
            {
                break;
            }

            float scene_depth = texture(texture_depth, sample_uv).x;
            if (scene_depth <= 0.0)
            {
                continue;
            }

            float ray_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, proj.z);
            float hit_linear = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, scene_depth);
            float depth_diff = ray_linear - hit_linear;
            float max_thickness = max(0.35, hit_linear * THICKNESS_SCALE);

            if (depth_diff > 0.04 && depth_diff < max_thickness)
            {
                // Reject back-facing surfaces relative to incoming bounce ray
                vec3 hit_N = normalize(texture(texture_normal, sample_uv).xyz * 2.0 - 1.0);
                float facing = saturate(dot(hit_N, -ray_dir) * 0.5 + 0.5);

                // Reproject hit UV into previous frame's downsampled SceneColor
                vec2 prev_uv = sample_uv - texture(texture_velocity, sample_uv).xy;
                if (prev_uv.x >= 0.0 && prev_uv.x <= 1.0 && prev_uv.y >= 0.0 && prev_uv.y <= 1.0)
                {
                    float sample_mip = mix(2.0, 3.5, t);
                    vec3 sample_radiance = max(vec3(0.0), textureLod(texture_scene, prev_uv, sample_mip).rgb);
                    // Soft firefly suppression for temporal stability
                    float lum = get_luminance(sample_radiance);
                    sample_radiance /= (1.0 + lum * 0.25);

                    vec2 edge = smoothstep(vec2(0.0), vec2(0.08), prev_uv) * smoothstep(vec2(1.0), vec2(0.92), prev_uv);
                    float dist_fade = saturate(1.0 - t * 0.75);
                    float w = edge.x * edge.y * dist_fade * facing;

                    gi_sum += sample_radiance * w;
                    hit_weight_sum += w;
                }
                break;
            }
        }
    }

    vec3 gi_color = gi_sum / float(NUM_RAYS);
    float confidence = saturate(hit_weight_sum / float(NUM_RAYS));
    outColor = vec4(gi_color, confidence);
}
