#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "PCFKernels.glsl"
#include "utility.glsl"
#include "scene_constants.glsl"
#include "screen_space_raycast.glsl"
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
layout(binding = 5) uniform sampler2D texture_material;
layout(binding = 6) uniform sampler2D texture_velocity;
layout(binding = 7) uniform sampler2D texture_depth;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

uint ReverseBits32( uint bits )
{
    bits = ( bits << 16) | ( bits >> 16);
	bits = ( (bits & 0x00ff00ff) << 8 ) | ( (bits & 0xff00ff00) >> 8 );
	bits = ( (bits & 0x0f0f0f0f) << 4 ) | ( (bits & 0xf0f0f0f0) >> 4 );
	bits = ( (bits & 0x33333333) << 2 ) | ( (bits & 0xcccccccc) >> 2 );
	bits = ( (bits & 0x55555555) << 1 ) | ( (bits & 0xaaaaaaaa) >> 1 );
	return bits;
}

vec2 Hammersley( uint Index, uint NumSamples, uvec2 Random )
{
	float E1 = fract( float(Index) / NumSamples + float( Random.x & 0xffff ) / (1<<16) );
	float E2 = float( ReverseBits32(Index) ^ Random.y ) * 2.3283064365386963e-10;
	return vec2( E1, E2 );
}

float ClampedPow(float X, float Y)
{
    return pow(max(abs(X), 0.000001f), Y);
}

vec3 ImportanceSampleGGX(vec2 E, float Roughness)
{
    float a = max(Roughness * Roughness, 0.001);
    float Phi = TWO_PI * E.x;
    float CosTheta = sqrt((1.0 - E.y) / max(1.0 + (a * a - 1.0) * E.y, 1e-5));
    float SinTheta = sqrt(max(0.0, 1.0 - CosTheta * CosTheta));

    vec3 H;
    H.x = SinTheta * cos(Phi);
    H.y = CosTheta;
    H.z = SinTheta * sin(Phi);
    return H;
}

mat3 GetTangentBasis(vec3 TangentY)
{
    vec3 UpVector = abs(TangentY.y) < 0.999 ? vec3(0, 1, 0) : vec3(0, 0, 1);
    vec3 TangentZ = normalize(cross(UpVector, TangentY));
    vec3 TangentX = cross(TangentY, TangentZ);
    return mat3(TangentX, TangentY, TangentZ);
}

vec3 TangentToWorld(vec3 vector, vec3 TangentY)
{
    return GetTangentBasis(TangentY) * vector;
}

vec4 SampleScreenColor(sampler2D texPrevSceneColor, vec2 uv, float lod)
{
    vec4 OutColor;
    OutColor.xyz = textureLod(texPrevSceneColor, uv, lod).xyz;
    OutColor.w = 1;

    // Off screen masking
    vec2 ScreenPos = uv * 2.0 - 1.0;

    // ver1
    //vec2 Vignette = saturate(abs(ScreenPos) * 5.0 - 4.0);
    //OutColor.w *= saturate(1.0 - dot(Vignette, Vignette));

    // ver2
    float sharpen = 0.7f;
    vec2 Vignette = clamp((abs(ScreenPos) - sharpen) / (1.0 - sharpen), 0.0, 1.0);
    OutColor.w *= clamp(1.0 - length(Vignette), 0.0, 1.0);

    return OutColor;
}

void main() {
    outColor = vec4(0.0);

    vec2 texCoord = vs_output.texCoord.xy;
    float depth = texture(texture_depth, texCoord).x;
    float linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, depth);

    if(depth <= 0.0)
    {
        return;
    }

    ivec2 PixelPos = ivec2(gl_FragCoord.xy);
    vec4 relative_pos = relative_world_from_device_depth(
        view_constants.INV_VIEW_ORIGIN_PROJECTION_JITTER,
        texCoord,
        depth
    );
    vec3 world_pos = relative_pos.xyz + view_constants.CAMERA_POSITION;
    vec4 material = texture(texture_material, texCoord);
    float Roughness = material.x;
    vec4 normal = texture(texture_normal, texCoord);
    vec3 vertexNormal = oct_decode_normal(material.zw);
    vec3 worldNormal = normalize(normal.xyz * 2.0 - 1.0);
    uint shading_model = decode_shading_model(normal.w);
    vec3 N = normalize(mix(worldNormal, vertexNormal, Roughness * 0.25));

    // Apply weather wetness & puddle surface overrides so puddles cast sharp SSR reflections
    if (scene_constants.WETNESS > 0.001)
    {
        float surface_wetness = 0.0;
        float puddle_mask = 0.0;
        vec2 ripple_grad = vec2(0.0);
        compute_surface_wetness_and_puddle(
            world_pos,
            scene_constants.SEA_HEIGHT,
            vertexNormal,
            1.0,
            shading_model,
            scene_constants.WETNESS,
            scene_constants.RAIN_INTENSITY,
            scene_constants.TIME,
            linear_depth,
            surface_wetness,
            puddle_mask,
            ripple_grad
        );
        if (surface_wetness > 0.001)
        {
            Roughness = mix(Roughness, min(Roughness, 0.22), surface_wetness * 0.85);
            if (puddle_mask > 0.001)
            {
                Roughness = mix(Roughness, 0.04, puddle_mask);
                vec3 puddle_normal = normalize(vec3(ripple_grad.x, 1.0, ripple_grad.y));
                N = normalize(mix(N, puddle_normal, puddle_mask));
            }
        }
    }

    if (Roughness > 0.85)
    {
        return;
    }

    vec3 V = normalize(-relative_pos.xyz);
    float NdotV = dot(V, N);
    if (NdotV <= 0.001)
    {
        return;
    }

    const int NumSteps = 16;
    const int NumRays = int(mix(2.0, 5.0, Roughness));
    const float frame_seed = fract(float(scene_constants.ELAPSED_FRAME % 64u) * 0.61803398875);
    const float spatial_x = interleaved_gradient_noise(PixelPos);
    const float spatial_y = interleaved_gradient_noise(PixelPos + ivec2(47, 19));

    vec2 HitSampleUV = vec2(-1.0, -1.0);
    float hit_count = 0.0;
    float valid_rays = 0.0;

    for (int i = 0; i < NumRays; i++)
    {
        vec2 E = Hammersley(uint(i), uint(NumRays), uvec2(uint(spatial_x * 65535.0), uint(spatial_y * 65535.0)));
        E = fract(E + vec2(frame_seed, frame_seed * 1.6180339));
        // Bias grazing rough samples slightly toward mirror peak to reduce variance
        E.y *= 0.85;
        float StepOffset = fract(spatial_x + float(i) * 0.381966 + frame_seed) - 0.5;

        vec3 H = TangentToWorld(ImportanceSampleGGX(E, Roughness), N);
        vec3 R = reflect(-V, H);
        if (dot(R, N) <= 0.001)
        {
            continue;
        }
        valid_rays += 1.0;

        vec4 HitUVzTime = RayCast(
            texture_depth,
            view_constants.VIEW_ORIGIN,
            view_constants.PROJECTION_JITTER,
            relative_pos.xyz,
            R,
            Roughness,
            0.001,
            max(linear_depth * 0.02, 0.05),
            NumSteps,
            StepOffset
        );

        // if there was a hit
        if (HitUVzTime.w < 1.0)
        {
            HitSampleUV = HitUVzTime.xy - texture(texture_velocity, HitUVzTime.xy).xy;
            if (HitSampleUV.x >= 0.0 && HitSampleUV.x <= 1.0 && HitSampleUV.y >= 0.0 && HitSampleUV.y <= 1.0)
            {
                vec4 SampleColor = SampleScreenColor(texture_scene, HitSampleUV, Roughness * 5.0);
                SampleColor.rgb = max(SampleColor.rgb, vec3(0.0));
                SampleColor.rgb /= (1.0 + get_luminance(SampleColor.rgb));
                outColor += SampleColor;
                hit_count += 1.0;
            }
        }
    }

    if (0.0 < hit_count && 0.0 < valid_rays)
    {
        outColor.rgb /= hit_count;
        outColor.rgb /= max(1.0 - get_luminance(outColor.rgb), 0.05);
        float roughness_fade = smoothstep(0.85, 0.45, Roughness);
        outColor.a = saturate((outColor.a / valid_rays) * roughness_fade);
    }
    else
    {
        outColor = vec4(0.0);
    }
}