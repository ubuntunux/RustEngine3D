#include "scene_constants.glsl"
#include "PCFKernels.glsl"
#include "utility.glsl"
//#include "precomputed_atmosphere/atmosphere_predefine.glsl"

float get_shadow_factor_func(
    const in LIGHT_CONSTANTS light_constants,
    const in vec3 world_position,
    const in float NdotL,
    sampler2D texture_shadow,
    const bool isSimpleShadow
)
{
    const vec2 shadow_size = textureSize(texture_shadow, 0);
    const vec2 shadow_texel_size = 1.0 / shadow_size;
    const int samnple_count = isSimpleShadow ? 1 : light_constants.SHADOW_SAMPLES;
    const vec2 shadow_noise_radius = shadow_texel_size * max(1.0, log2(samnple_count));
    const vec2 shadow_uv_min = shadow_texel_size * 0.5;
    const vec2 shadow_uv_max = vec2(1.0) - shadow_texel_size * 0.5;
    vec4 shadow_proj = light_constants.SHADOW_VIEW_PROJECTION * vec4(world_position, 1.0);
    shadow_proj.xyz /= shadow_proj.w;
    shadow_proj.xy = shadow_proj.xy * 0.5 + 0.5;

    if(1.0 < shadow_proj.x || shadow_proj.x < 0.0 || 1.0 < shadow_proj.y || shadow_proj.y < 0.0 || 1.0 < shadow_proj.z || shadow_proj.z < 0.0)
    {
        return 1.0;
    }

    const vec2 uv_offsets[4] = {
        vec2(0.0, 0.0),
        vec2(shadow_texel_size.x, 0.0),
        vec2(0.0, shadow_texel_size.y),
        vec2(shadow_texel_size.x, shadow_texel_size.y),
    };

    const float shadow_bias = light_constants.SHADOW_BIAS * tan(1.0 - acos(saturate(NdotL)));

    float total_shadow_factor = 0.0;
    for(int sample_index = 0; sample_index < samnple_count; ++sample_index)
    {
        vec2 uv = shadow_proj.xy + PoissonSamples[sample_index % PoissonSampleCount] * shadow_noise_radius;

        vec4 shadow_factors = vec4(1.0);
        for(int component_index = 0; component_index < 4; ++component_index)
        {
            const vec2 shadow_uv = clamp(uv + uv_offsets[component_index], shadow_uv_min, shadow_uv_max);
            const float shadow_depth = textureLod(texture_shadow, shadow_uv, 0.0).x + shadow_bias;
            if(shadow_depth <= shadow_proj.z)
            {
                shadow_factors[component_index] = saturate(exp(-light_constants.SHADOW_EXP * (shadow_proj.z - shadow_depth)));
            }
        }

        const vec2 pixel_ratio = fract(uv * shadow_size);
        total_shadow_factor += mix(
            mix(shadow_factors[0], shadow_factors[1], pixel_ratio.x),
            mix(shadow_factors[2], shadow_factors[3], pixel_ratio.x), pixel_ratio.y);
    }
    return clamp(total_shadow_factor / float(samnple_count), 0.0, 1.0);
}

float get_shadow_factor_simple(
    const in LIGHT_CONSTANTS light_constants,
    const in vec3 world_position,
    const in float NdotL,
    sampler2D texture_shadow)
{
    const bool isSimpleShadow = true;
    return get_shadow_factor_func(light_constants, world_position, NdotL, texture_shadow, isSimpleShadow);
}

float get_shadow_factor(
    const in LIGHT_CONSTANTS light_constants,
    const in vec3 world_position,
    const in float NdotL,
    sampler2D texture_shadow)
{
    const bool isSimpleShadow = false;
    return get_shadow_factor_func(light_constants, world_position, NdotL, texture_shadow, isSimpleShadow);
}

// https://en.wikipedia.org/wiki/Oren%E2%80%93Nayar_reflectance_model
float oren_nayar(float roughness2, float NdotL, float NdotV, vec3 N, vec3 V, vec3 L)
{
    float incidentTheta = acos(NdotL);
    float outTheta = acos(NdotV);
    float A = 1.0 - 0.5 * (roughness2 / (roughness2 + 0.33));
    float B = (0.45 * roughness2) / (roughness2 + 0.09);
    float alpha = max(incidentTheta, outTheta);
    float beta  = min(incidentTheta, outTheta);
    vec3 u = normalize(V - N * NdotV);
    vec3 v = normalize(L - N * NdotL);
    float phiDiff = max(0.0, dot(u, v));
    return (A + (B * phiDiff * sin(alpha) * tan(beta))) * NdotL / PI;
}

vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness)
{
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

float D_blinn(float roughness, float NdH)
{
    float m = roughness * roughness;
    float m2 = m * m;
    float n = 2.0 / m2 - 2.0;
    return (n + 2.0) / (2.0 * PI) * pow(NdH, n);
}

float D_beckmann(float roughness, float NdH)
{
    float m = roughness * roughness;
    float m2 = m * m;
    float NdH2 = NdH * NdH;
    return exp((NdH2 - 1.0) / (m2 * NdH2)) / (PI * m2 * NdH2 * NdH2);
}

float DistributionGGX(float NdH, float roughness)
{
    const float a = roughness * roughness;
    const float a2 = a * a;
    const float NdH2 = NdH * NdH;
    const float nom = a2;
    float denom = (NdH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;
    return nom / denom;
}

float GeometrySchlickGGX(float NdV, float roughness)
{
    const float r = (roughness + 1.0);
    const float k = (r * r) / 8.0;
    const float num = NdV;
    const float denom = NdV * (1.0 - k) + k;
    return num / denom;
}

float GeometrySmith(float NdV, float NdL, float roughness)
{
    float ggx2  = GeometrySchlickGGX(NdV, roughness);
    float ggx1  = GeometrySchlickGGX(NdL, roughness);
    return ggx1 * ggx2;
}

// simple phong specular calculation with normalization
vec3 phong_specular(in float LdR, in vec3 specular, in float roughness)
{
    float spec = max(0.0, LdR);
    float k = 1.999 / (roughness * roughness);
    return min(1.0, 3.0 * 0.0398 * k) * pow(spec, min(10000.0, k)) * specular;
}

// simple blinn specular calculation with normalization
vec3 blinn_specular(in float NdH, in vec3 specular, in float roughness)
{
    float k = 1.999 / (roughness * roughness);
    return min(1.0, 3.0 * 0.0398 * k) * pow(NdH, min(10000.0, k)) * specular;
}

vec3 cooktorrance_specular(vec3 F, float NdL, float NdV, float NdH, float roughness)
{
    float NDF = DistributionGGX(NdH, roughness);
    float G = GeometrySmith(NdV, NdL, roughness);
    vec3 numerator = NDF * G * F;
    float denominator = 4.0 * NdV * NdL;
    return numerator / max(denominator, 0.00001);
}

vec2 env_BRDF_pproximate(float NdV, float roughness)
{
    // see https://www.unrealengine.com/blog/physically-based-shading-on-mobile
    const vec4 c0 = vec4(-1.0, -0.0275, -0.572,  0.022);
    const vec4 c1 = vec4( 1.0,  0.0425,  1.040, -0.040);
    vec4 r = roughness * c0 + c1;
    float a004 = min(r.x * r.x, exp2(-9.28 * NdV)) * r.x + r.y;
    return vec2(-1.04, 1.04) * a004 + r.zw;
}


/* PBR reference
    - http://www.curious-creature.com/pbr_sandbox/shaders/pbr.fs
    - https://gist.github.com/galek/53557375251e1a942dfa */
vec4 surface_shading(
    //const in AtmosphereParameters ATMOSPHERE,
    const in SCENE_CONSTANTS scene_constants,
    const in VIEW_CONSTANTS view_constants,
    const in LIGHT_CONSTANTS light_constants,
    //const in POINT_LIGHTS point_lights,
    const in vec3 base_color,
    float opacity,
    const in vec3 emissive_color,
    const in float metallic,
    float roughness,
    const in float reflectance,
    const in float ssao_factor,
    const in vec4 scene_reflect_color,
    const in samplerCube texture_probe,
    //const in sampler2D ibl_brdf_lut,
    const in sampler2D texture_shadow,
    const in vec2 texCoord,
    const in vec3 world_position,
    vec3 light_color,
    const in vec3 N,
    const in vec3 V,
    const in vec3 L,
    const in float depth
)
{
    // safe roughness
    roughness = clamp(roughness, 0.05, 1.0);
    const float roughness2 = roughness * roughness;

    // compute material reflectance
    const vec3 R = reflect(-V, N);
    const vec3 H = normalize(V + L);

    const float NdL = dot(N, L);
    const float clampled_NdL = clamp(NdL, 0.0, 1.0);
    const float NdV = clamp(dot(N, V), 0.001, 1.0);
    const float NdH = clamp(dot(N, H), 0.001, 1.0);
    const float HdV = clamp(dot(H, V), 0.001, 1.0);
    const float LdV = clamp(dot(L, V), 0.001, 1.0);

    // Atmosphere
    vec3 scene_in_scatter = vec3(0.0);
    vec3 scene_sun_irradiance = vec3(1.0);
    vec3 scene_sky_irradiance = vec3(0.0);
    float scene_shadow_length = 0.0;
    float scene_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, depth);
    //GetSceneRadiance(ATMOSPHERE, scene_linear_depth, -V, N, scene_sun_irradiance, scene_sky_irradiance, scene_in_scatter);

    vec3 result = vec3(0.0, 0.0, 0.0);
    vec3 F0 = mix(vec3(max(0.04, reflectance)), base_color.xyz, metallic);
    vec3 fresnel = fresnelSchlick(NdV, F0);
    vec3 diffuse_light = vec3(0.0, 0.0, 0.0);
    vec3 specular_light = vec3(0.0, 0.0, 0.0);
    vec3 shadow_factor = vec3(get_shadow_factor(light_constants, world_position, NdL, texture_shadow));
    light_color *= scene_sun_irradiance * shadow_factor;

    // Image based lighting
    {
        const vec2 env_size = textureSize(texture_probe, 0);
        const float max_env_mipmap = min(8.0, log2(max(env_size.x, env_size.y)) - 1.0);
        vec2 envBRDF = env_BRDF_pproximate(NdV, roughness);
        // clamp uv ( half_texel ~ 1.0 - half_texel)
        //vec2 envBRDF2  = texture(ibl_brdf_lut, clamp(vec2(NdV, 1.0 - roughness), vec2(0.0009765625), vec2(0.9990234375))).xy;
        const vec3 kS = fresnelSchlickRoughness(NdV, F0, roughness);
        const vec3 kD = vec3(1.0) - kS;
        const vec3 shValue = kS * envBRDF.x + envBRDF.y;
        const vec3 ibl_diffuse_light = pow(textureLod(texture_probe, N, max_env_mipmap).xyz, vec3(2.2));
        const vec3 ibl_specular_light = pow(textureLod(texture_probe, R, roughness * max_env_mipmap).xyz, vec3(2.2));

        //ambient_light = normalize(mix(ibl_diffuse_light, scene_sky_irradiance, 0.5)) * length(scene_sky_irradiance);
        diffuse_light += ibl_diffuse_light * kD;
        // if(RENDER_SSR)
        {
            specular_light.xyz += mix(ibl_specular_light, scene_reflect_color.xyz, scene_reflect_color.w) * shValue;
        }
    }

#if TRANSPARENT_MATERIAL == 1
    float reflectivity = max(max(fresnel.r, fresnel.g), fresnel.b);
    opacity = clamp(opacity + opacity * reflectivity, 0.0, 1.0);
#endif

    // Directional Light
    {
        const vec3 F = fresnelSchlick(HdV, F0);
        diffuse_light += oren_nayar(roughness2, clampled_NdL, NdV, N, V, L) * (vec3(1.0) - F) * light_color;
        specular_light += cooktorrance_specular(F, clampled_NdL, NdV, NdH, roughness) * clampled_NdL * light_color;
    }

    // Point Lights
//    for(int i = 0; i < MAX_POINT_LIGHTS; ++i)
//    {
//        if(1.0 != point_lights.data[i].render)
//        {
//            break;
//        }
//
//        const float point_light_radius = point_lights.data[i].radius;
//        vec3 point_light_dir = point_lights.data[i].pos.xyz - world_position;
//        const float point_light_dist = length(point_light_dir);
//
//        if(point_light_radius < point_light_dist)
//        {
//            continue;
//        }
//
//        point_light_dir /= point_light_dist;
//
//        const vec3 point_light_half = normalize(V + point_light_dir);
//        float point_light_attenuation = clamp(1.0 - point_light_dist / point_light_radius, 0.0, 1.0);
//        point_light_attenuation *= point_light_attenuation;
//        const vec3 point_light_color = point_lights.data[i].color.xyz * point_light_attenuation;
//        const float point_light_NdL = max(0.01, dot(N, point_light_dir));
//        const float point_light_NdH = max(0.01, dot(N, point_light_half));
//        const float point_light_HdV = max(0.01, dot(V, point_light_half));
//        const vec3 point_light_F = fresnelSchlick(point_light_HdV, F0);
//        diffuse_light += oren_nayar(roughness2, point_light_NdL, NdV, N, V, point_light_dir) * (vec3(1.0) - point_light_F) * point_light_color;
//        specular_light += cooktorrance_specular(point_light_F, point_light_NdL, NdV, point_light_NdH, roughness) * point_light_NdL * point_light_color;
//    }

/*
#ifdef FRAGMENT_SHADER
    // Compute curvature
    vec3 ddx = dFdx(N);
    vec3 ddy = dFdy(N);
    vec3 xneg = N - ddx;
    vec3 xpos = N + ddx;
    vec3 yneg = N - ddy;
    vec3 ypos = N + ddy;
    float curvature = (cross(xneg, xpos).y - cross(yneg, ypos).x) / scene_linear_depth;

    float corrosion = clamp(-curvature * 3.0, 0.0, 1.0);
    float shine = clamp(curvature * 5.0, 0.0, 1.0);

    curvature = pow(saturate((curvature * 0.5 + 0.5) * 1.0), 4.0);

    return vec4(curvature, curvature, curvature, 1.0);
#endif
*/

    // final result
    result = diffuse_light * base_color * (1.0 - max(reflectance, metallic));
    result += mix(specular_light, specular_light * base_color, vec3(metallic));

    // SSAO
    //if(RENDER_SSAO)
    {
        result *= ssao_factor;
    }

    // Emissive
    result += emissive_color;

    return vec4(max(vec3(0.0), result), opacity);
}