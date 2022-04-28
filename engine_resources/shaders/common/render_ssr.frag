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

vec4 ImportanceSampleBlinn(vec2 E, float Roughness)
{
    float PI = 3.14159265f;
    float m = Roughness * Roughness;
    float n = 2 / (m*m) - 2;

    float Phi = 2 * PI * E.x;
    float CosTheta = ClampedPow(E.y, 1 / (n + 1));
    float SinTheta = sqrt(1 - CosTheta * CosTheta);

    vec3 H;
    H.x = SinTheta * cos(Phi);
    /*H.y = SinTheta * sin(Phi);
    H.z = CosTheta;*/
    H.y = CosTheta;
    H.z = SinTheta * sin(Phi);

    float D = (n + 2) / (2 * PI) * ClampedPow(CosTheta, n);
    float PDF = D * CosTheta;

    return vec4(H, PDF);
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

    if(1.0 <= depth)
    {
        return;
    }

    ivec2 PixelPos = ivec2(gl_FragCoord.xy);

    vec4 ndc_coord = vec4(vs_output.texCoord.xy * 2.0 - 1.0, depth * 2.0 - 1.0, 1.0);
    vec4 relative_pos = view_constants.INV_VIEW_ORIGIN_PROJECTION * ndc_coord;
    relative_pos.xyz /= relative_pos.w;

    vec4 material = texture(texture_material, vs_output.texCoord.xy);
    float Roughness = material.x;
    vec4 normal = texture(texture_normal, vs_output.texCoord.xy);
    vec3 vertexNormal = vec3(material.z, material.w, normal.w);
    vec3 N = normalize(mix(vertexNormal, normal.xyz, Roughness) * 2.0 - 1.0);
    vec3 V = normalize(-relative_pos.xyz);
    float NdotV = dot(V, N);

    if(0.9 < NdotV)
    {
        return;
    }

    float distance_ratio = clamp(1.0 - relative_pos.w * relative_pos.w, 0.0, 1.0);
    float fresnel = pow(1.0 - clamp(NdotV, 0.0, 1.0), 4.0);
    Roughness = mix(Roughness, Roughness * Roughness, fresnel * distance_ratio);

    const int NumSteps = 12;
    const int NumRays = int(mix(2.0, 6.0, Roughness));

    vec2 HitSampleUV = vec2(-1.0, -1.0);
    float hit_count = 0.0;
    vec2 random_texture_size = textureSize(texture_random, 0);

    for (int i = 0; i < NumRays; i++)
    {
        vec2 poisson = PoissonSamples[int(view_constants.JITTER_FRAME + i * PoissonSampleCount / NumRays) % PoissonSampleCount];
        vec2 random = texture(texture_random, texCoord + poisson).xy;
        float StepOffset = 0.5 - rand(texCoord + random);

        vec2 E = Hammersley(i, NumRays, uvec2(random * 117));
        vec3 H = TangentToWorld(ImportanceSampleBlinn( random, Roughness * 0.5 ).xyz, N);
        vec3 R = reflect(-V, H);

        vec4 HitUVzTime = RayCast(
            texture_depth,
            view_constants.VIEW_ORIGIN,
            view_constants.PROJECTION,
            relative_pos.xyz,
            R,
            Roughness,
            0.001,
            linear_depth * 0.01,
            NumSteps,
            StepOffset
        );

        // if there was a hit
        if (HitUVzTime.w < 1)
        {
            HitSampleUV = HitUVzTime.xy - texture(texture_velocity, HitUVzTime.xy).xy;
            vec4 SampleColor = SampleScreenColor(texture_scene, HitSampleUV, Roughness * 6.0);
            SampleColor.rgb /= 1 + get_luminance(SampleColor.rgb);
            outColor += SampleColor;
            hit_count += 1.0;
        }
    }

    if(0.0 < hit_count)
    {
        outColor.rgb /= hit_count;
        outColor.rgb /= 1.0 - get_luminance(outColor.rgb);
        outColor.a /= NumRays;
    }
}