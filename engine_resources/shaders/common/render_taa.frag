#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_GOOGLE_include_directive : enable

#include "scene_constants.glsl"
#include "utility.glsl"
#include "render_quad_common.glsl"

const float Pi = 3.141592f;

const int ClampModes_Disabled = 0;
const int ClampModes_RGB_Clamp = 1;
const int ClampModes_RGB_Clip = 2;
const int ClampModes_Variance_Clip = 3;

const int DilationModes_CenterAverage = 0;
const int DilationModes_DilateNearestDepth = 1;
const int DilationModes_DilateGreatestVelocity = 2;

const int ResolveFilterType = FilterTypes_BSpline;
const float ResolveFilterDiameter = 2.0;  // 0.0 ~ 6.0
const float ExposureFilterOffset = 2.0;     // -16.0 ~ 16.0
const float TemporalAABlendFactor = 0.9;    // default: 0.9, 0.0 ~ 1.0
const int NeighborhoodClampMode = ClampModes_Variance_Clip;
const float VarianceClipGamma = 1.5;    // 0.0 ~ 2.0
const float LowFreqWeight = 0.25;   // 0.0 ~ 100.0
const float HiFreqWeight = 0.85;    // 0.0 ~ 100.0
const int DilationMode = DilationModes_DilateNearestDepth;
const int ReprojectionFilter = FilterTypes_CatmullRom;
const float ExposureScale = 0.0;    // -16.0 ~ 16.0
const float ManualExposure = -2.5;  // -10.0 ~ 10.0

const bool UseStandardReprojection = false;
const bool UseTemporalColorWeighting = false;
const bool InverseLuminanceFiltering = true;
const bool UseExposureFiltering = false;

layout(binding = 0) uniform SceneConstants
{
    SCENE_CONSTANTS scene_constants;
};
layout(binding = 1) uniform ViewConstants
{
    VIEW_CONSTANTS view_constants;
};

layout(binding = 2) uniform sampler2D texture_input;
layout(binding = 3) uniform sampler2D texture_prev_resolve;
layout(binding = 4) uniform sampler2D texture_velocity;
layout(binding = 5) uniform sampler2D texture_scene_depth;

layout(location = 0) in VERTEX_OUTPUT vs_output;
layout(location = 0) out vec4 outColor;

// From "Temporal Reprojection Anti-Aliasing"
// https://github.com/playdeadgames/temporal
vec3 ClipAABB(vec3 aabbMin, vec3 aabbMax, vec3 prevSample, vec3 avg)
{
#if 1
    // note: only clips towards aabb center (but fast!)
    vec3 p_clip = 0.5 * (aabbMax + aabbMin);
    vec3 e_clip = 0.5 * (aabbMax - aabbMin);

    vec3 v_clip = prevSample - p_clip;
    vec3 v_unit = v_clip.xyz / e_clip;
    vec3 a_unit = abs(v_unit);
    float ma_unit = max(a_unit.x, max(a_unit.y, a_unit.z));

    if (ma_unit > 1.0)
    {
        return p_clip + v_clip / ma_unit;
    }
    else
    {
        return prevSample;// point inside aabb
    }
#else
    vec3 r = prevSample - avg;
    vec3 rmax = aabbMax - avg.xyz;
    vec3 rmin = aabbMin - avg.xyz;

    const float eps = 0.000001f;

    if (r.x > rmax.x + eps)
    r *= (rmax.x / r.x);
    if (r.y > rmax.y + eps)
    r *= (rmax.y / r.y);
    if (r.z > rmax.z + eps)
    r *= (rmax.z / r.z);

    if (r.x < rmin.x - eps)
    r *= (rmin.x / r.x);
    if (r.y < rmin.y - eps)
    r *= (rmin.y / r.y);
    if (r.z < rmin.z - eps)
    r *= (rmin.z / r.z);

    return avg + r;
#endif
}

vec3 Reproject(vec2 texCoord)
{
    vec2 inv_velocity_tex_size = 1.0 / textureSize(texture_velocity, 0).xy;
    vec2 velocity = vec2(0.0, 0.0);

    if(DilationMode == DilationModes_CenterAverage)
    {
        velocity += texture(texture_velocity, texCoord).xy;
    }
    else if(DilationMode == DilationModes_DilateNearestDepth)
    {
        vec2 inv_depth_tex_size = 1.0 / textureSize(texture_scene_depth, 0).xy;
        float closestDepth = 10.0f;
        for(int vy = -1; vy <= 1; ++vy)
        {
            for(int vx = -1; vx <= 1; ++vx)
            {
                vec2 neighborVelocity = texture(texture_velocity, texCoord + vec2(vx, vy) * inv_velocity_tex_size).xy;
                float neighborDepth = textureLod(texture_scene_depth, texCoord + vec2(vx, vy) * inv_depth_tex_size, 0.0).x;
                neighborDepth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, neighborDepth);
                if(neighborDepth < closestDepth)
                {
                    velocity = neighborVelocity;
                    closestDepth = neighborDepth;
                }
            }
        }
    }
    else if(DilationMode == DilationModes_DilateGreatestVelocity)
    {
        float greatestVelocity = -1.0f;
        for(int vy = -1; vy <= 1; ++vy)
        {
            for(int vx = -1; vx <= 1; ++vx)
            {
                vec2 neighborVelocity = texture(texture_velocity, texCoord + vec2(vx, vy) * inv_velocity_tex_size).xy;
                float neighborVelocityMag = dot(neighborVelocity, neighborVelocity).x;
                if(dot(neighborVelocity, neighborVelocity) > greatestVelocity)
                {
                    velocity = neighborVelocity;
                    greatestVelocity = neighborVelocityMag;
                }
            }
        }
    }

    vec2 texture_size = textureSize(texture_prev_resolve, 0).xy;
    vec2 reprojectedUV = texCoord - velocity;
    vec2 reprojectedPos = reprojectedUV * texture_size;

    if(UseStandardReprojection)
    {
        return texture(texture_prev_resolve, reprojectedUV).xyz;
    }

    vec3 sum = vec3(0.0f);
    float totalWeight = 0.0f;

    for(int ty = -1; ty <= 2; ++ty)
    {
        for(int tx = -1; tx <= 2; ++tx)
        {
            vec2 samplePos = floor(reprojectedPos + vec2(tx, ty)) + 0.5f;
            vec3 reprojectedSample = texture(texture_prev_resolve, samplePos / texture_size).xyz;

            vec2 sampleDist = abs(samplePos - reprojectedPos);
            float filterWeight = Filter(sampleDist.x, ReprojectionFilter, 1.0f, false) *
            Filter(sampleDist.y, ReprojectionFilter, 1.0f, false);

            if(InverseLuminanceFiltering)
            {
                float sampleLum = get_luminance(reprojectedSample);
                if(UseExposureFiltering)
                {
                    sampleLum *= exp2(ManualExposure - ExposureScale + ExposureFilterOffset);
                }
                filterWeight /= (1.0f + sampleLum);
            }

            sum += reprojectedSample * filterWeight;
            totalWeight += filterWeight;
        }
    }
    return max(sum / totalWeight, 0.0f);
}

vec4 ResolvePS(vec2 texCoord, vec2 pixelPos)
{
    vec3 sum = vec3(0.0f);
    float totalWeight = 0.0f;

    vec3 clrMin = vec3(99999999.0f);
    vec3 clrMax = vec3(-99999999.0f);

    vec3 m1 = vec3(0.0f);
    vec3 m2 = vec3(0.0f);
    float mWeight = 0.0f;

    vec2 texture_input_size = textureSize(texture_input, 0).xy;

    const float filterRadius = ResolveFilterDiameter / 2.0f;

    for(int y = -1; y <= 1; ++y)
    {
        for(int x = -1; x <= 1; ++x)
        {
            vec2 sampleOffset = vec2(x, y);
            vec2 sampleUV = texCoord + sampleOffset / texture_input_size;
            sampleUV = clamp(sampleUV, 0.0, 1.0);

            vec3 sample_color = texture(texture_input, sampleUV).xyz;

            vec2 sampleDist = abs(sampleOffset) / (ResolveFilterDiameter / 2.0f);

            float weight = Filter(sampleDist.x, ResolveFilterType, filterRadius, true) *
            Filter(sampleDist.y, ResolveFilterType, filterRadius, true);
            clrMin = min(clrMin, sample_color);
            clrMax = max(clrMax, sample_color);

            if(InverseLuminanceFiltering)
            {
                float sampleLum = get_luminance(sample_color);
                if(UseExposureFiltering)
                {
                    sampleLum *= exp2(ManualExposure - ExposureScale + ExposureFilterOffset);
                }
                weight /= (1.0f + sampleLum);
            }

            sum += sample_color * weight;
            totalWeight += weight;

            m1 += sample_color;
            m2 += sample_color * sample_color;
            mWeight += 1.0f;
        }
    }

    vec4 result = texture(texture_input, texCoord);

    vec3 currColor = result.xyz;
    vec3 prevColor = Reproject(texCoord);

    if(NeighborhoodClampMode == ClampModes_RGB_Clamp)
    {
        prevColor = clamp(prevColor, clrMin, clrMax);
    }
    else if(NeighborhoodClampMode == ClampModes_RGB_Clip)
    {
        prevColor = ClipAABB(clrMin, clrMax, prevColor, m1 / mWeight);
    }
    else if(NeighborhoodClampMode == ClampModes_Variance_Clip)
    {
        vec3 mu = m1 / mWeight;
        vec3 sigma = sqrt(abs(m2 / mWeight - mu * mu));
        vec3 minc = mu - VarianceClipGamma * sigma;
        vec3 maxc = mu + VarianceClipGamma * sigma;
        prevColor = ClipAABB(minc, maxc, prevColor, mu);
    }

    vec3 weightA = vec3(clamp(1.0f - TemporalAABlendFactor, 0.0, 1.0));
    vec3 weightB = vec3(clamp(TemporalAABlendFactor, 0.0, 1.0));

    if(UseTemporalColorWeighting)
    {
        vec3 temporalWeight = clamp(abs(clrMax - clrMin) / currColor, 0.0, 1.0);
        weightB = clamp(mix(vec3(LowFreqWeight), vec3(HiFreqWeight), temporalWeight), 0.0, 1.0);
        weightA = 1.0f - weightB;
    }

    if(InverseLuminanceFiltering)
    {
        weightA /= (1.0f + get_luminance(currColor));
        weightB /= (1.0f + get_luminance(prevColor));
    }

    result.xyz = (currColor * weightA + prevColor * weightB) / (weightA + weightB);

    return result;
}


void main() {
    outColor = ResolvePS(vs_output.texCoord.xy, gl_FragCoord.xy);
}