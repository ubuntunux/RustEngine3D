#ifndef _SHADING_
#define _SHADING_

#include "scene_constants.glsl"
#include "PCFKernels.glsl"
#include "utility.glsl"

float get_shadow_factor_func(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const int sample_count,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    const vec2 shadow_size = textureSize(texture_shadow, 0);
    const vec2 shadow_texel_size = 1.0 / shadow_size;
    const vec2 shadow_noise_radius = shadow_texel_size * max(1.0, log2(sample_count));
    const vec2 shadow_uv_min = shadow_texel_size * 0.5;
    const vec2 shadow_uv_max = vec2(1.0) - shadow_texel_size * 0.5;
    vec4 shadow_proj = shadow_view_projection * vec4(world_position, 1.0);
    const float shadow_dist = saturate((length(shadow_proj.xy) - 0.9) * 10.0);
    shadow_proj.xyz /= shadow_proj.w;
    shadow_proj.xy = shadow_proj.xy * 0.5 + 0.5;

    if(1.0 < shadow_proj.x || shadow_proj.x < 0.0 || 1.0 < shadow_proj.y || shadow_proj.y < 0.0 || 1.0 < shadow_proj.z || shadow_proj.z < 0.0)
    {
        return 1.0;
    }

    //const float center_depth = textureLod(texture_shadow, shadow_proj.xy, 0.0).x;
    const int timeIndex = int(mod(time, 1.0) * 1000.0);
    const vec2 noise = (0.0 != time) ? vec2(interleaved_gradient_noise(screen_pos + timeIndex) * 2.0 - 1.0) * 0.5 : vec2(0.0);
    float total_shadow_factor = 0.0;
    for(int sample_index = 0; sample_index < sample_count; ++sample_index)
    {
        const vec2 shadow_uv = shadow_proj.xy + (PoissonSamples[sample_index % PoissonSampleCount] + noise) * shadow_noise_radius;
        const vec2 pixel_ratio = fract(shadow_uv * shadow_size);
        vec4 shadow_depths = vec4(
            textureLod(texture_shadow, shadow_uv, 0.0).x,
            textureLod(texture_shadow, shadow_uv + vec2(shadow_texel_size.x, 0.0), 0.0).x,
            textureLod(texture_shadow, shadow_uv + vec2(0.0, shadow_texel_size.y), 0.0).x,
            textureLod(texture_shadow, shadow_uv + vec2(shadow_texel_size.x, shadow_texel_size.y), 0.0).x
        );

        float shadow_depth = mix(
            mix(shadow_depths[0], shadow_depths[1], pixel_ratio.x),
            mix(shadow_depths[2], shadow_depths[3], pixel_ratio.x), pixel_ratio.y
        );

        float shadow_factor = 1.0;
        if((shadow_depth - depth_bias) <= shadow_proj.z)
        {
            shadow_factor = saturate(shadow_dist);
        }
        total_shadow_factor += shadow_factor;
    }
    return saturate(total_shadow_factor / float(sample_count));
}

float get_shadow_factor_simple(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    return get_shadow_factor_func(time, screen_pos, world_position, shadow_view_projection, 1, depth_bias, texture_shadow);
}

float get_shadow_factor(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const in int sample_count,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    return get_shadow_factor_func(time, screen_pos, world_position, shadow_view_projection, sample_count, depth_bias, texture_shadow);
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

void apply_image_based_lighting(
    const in samplerCube texture_probe,
    const in vec4 scene_reflect_color,
    const in vec3 scene_sky_irradiance,
    in vec3 shadow_factor,
    const float sky_visibility,
    float roughness,
    const in vec3 F0,
    const in vec3 sun_direction,
    const in vec3 N,
    const in vec3 R,
    float NdV,
    inout vec3 diffuse_light,
    inout vec3 specular_light
)
{
    const vec2 env_size = textureSize(texture_probe, 0);
    const float max_env_mipmap = min(8.0, log2(max(env_size.x, env_size.y)) - 1.0);
    vec2 envBRDF = env_BRDF_pproximate(NdV, roughness);
    // clamp uv ( half_texel ~ 1.0 - half_texel)
    //vec2 envBRDF2  = texture(ibl_brdf_lut, clamp(vec2(NdV, 1.0 - roughness), vec2(0.0009765625), vec2(0.9990234375))).xy;
    const vec3 kS = fresnelSchlickRoughness(NdV, F0, roughness);
    const vec3 kD = vec3(1.0) - kS;
    const vec3 shValue = kS * envBRDF.x + envBRDF.y;
    vec3 ibl_diffuse_light = textureLod(texture_probe, N, max_env_mipmap).xyz;
    vec3 ibl_specular_light = textureLod(texture_probe, R, roughness * max_env_mipmap).xyz;

    shadow_factor = pow(max(shadow_factor, vec3(dot(vec3(0.33333333), scene_sky_irradiance))), vec3(1.5 - sky_visibility * 0.5));
    ibl_diffuse_light *= shadow_factor;
    ibl_specular_light *= shadow_factor;

    // if(RENDER_SSR)
    {
        ibl_specular_light = mix(ibl_specular_light, scene_reflect_color.xyz, scene_reflect_color.w);
    }

    diffuse_light += ibl_diffuse_light * kD;
    specular_light.xyz += ibl_specular_light * shValue;
}


/* PBR reference
    - http://www.curious-creature.com/pbr_sandbox/shaders/pbr.fs
    - https://gist.github.com/galek/53557375251e1a942dfa */
vec4 surface_shading(
    const in AtmosphereParameters ATMOSPHERE,
    const in ATMOSPHERE_CONSTANTS atmosphere_constants,
    const in sampler2D transmittance_texture,
    const in sampler2D irradiance_texture,
    const in sampler3D scattering_texture,
    const in sampler3D single_mie_scattering_texture,
    const in SCENE_CONSTANTS scene_constants,
    const in VIEW_CONSTANTS view_constants,
    const in LIGHT_CONSTANTS light_constants,
    //const in POINT_LIGHTS point_lights,
    vec3 base_color,
    float opacity,
    const in float metallic,
    float roughness,
    const in float reflectance,
    const in float ssao_factor,
    const in vec4 scene_reflect_color,
    const in samplerCube texture_probe,
    const in sampler2D texture_shadow,
    const in sampler2D texture_height_map,
    const in vec2 screen_texcoord,
    const in vec3 world_position,
    const in vec3 vertexNormal,
    const in vec3 N,
    const in vec3 V,
    const in float depth
)
{
    const vec3 L = normalize(light_constants.LIGHT_DIRECTION);
    vec3 light_color = light_constants.LIGHT_COLOR.xyz;

    // under water
    float sea_diff = world_position.y - scene_constants.SEA_HEIGHT;
    float inv_sea_ratio = saturate(exp(sea_diff * 0.5));
    float under_water_material = saturate(-(sea_diff - 1.0) / SEA_COASTLINE_THICKNESS);
    under_water_material = 1.0 - under_water_material * under_water_material * 0.5;
    roughness *= under_water_material;
    base_color *= under_water_material;

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
    GetSceneRadiance(
        ATMOSPHERE,
        atmosphere_constants,
        transmittance_texture,
        irradiance_texture,
        scattering_texture,
        single_mie_scattering_texture,
        scene_linear_depth,
        view_constants.CAMERA_POSITION.xyz,
        -V,
        light_constants.LIGHT_DIRECTION.xyz,
        N,
        scene_sun_irradiance,
        scene_sky_irradiance,
        scene_in_scatter
    );

    vec3 result = vec3(0.0, 0.0, 0.0);
    vec3 F0 = mix(vec3(max(0.04, reflectance)), base_color.xyz, metallic);
    vec3 fresnel = fresnelSchlick(NdV, F0);
    vec3 diffuse_light = vec3(0.0, 0.0, 0.0);
    vec3 specular_light = vec3(0.0, 0.0, 0.0);
    vec3 shadow_factor = vec3(get_shadow_factor(
        scene_constants.TIME,
        ivec2(screen_texcoord * scene_constants.SCREEN_SIZE),
        world_position,
        light_constants.SHADOW_VIEW_PROJECTION,
        light_constants.SHADOW_SAMPLES,
        0.0,
        texture_shadow
    ));

    float sky_visibility = get_shadow_factor(
        scene_constants.TIME,
        ivec2(screen_texcoord * scene_constants.SCREEN_SIZE),
        world_position,
        view_constants.CAPTURE_HEIGHT_MAP_VIEW_PROJECTION,
        4,
        -0.0005,
        texture_height_map
    );

    light_color *= scene_sun_irradiance * shadow_factor;

    // Image based lighting
    apply_image_based_lighting(
        texture_probe,
        scene_reflect_color,
        scene_sky_irradiance,
        shadow_factor,
        sky_visibility,
        roughness,
        F0,
        L,
        N,
        R,
        NdV,
        diffuse_light,
        specular_light
    );

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

    // apply sea ratio
    diffuse_light *= inv_sea_ratio;
    specular_light *= inv_sea_ratio;

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
    result += mix(specular_light, specular_light * F0, vec3(max(reflectance, metallic)));

    // SSAO
    //if(RENDER_SSAO)
    {
        result *= ssao_factor;
    }

    return vec4(max(vec3(0.0), result), opacity);
}

#endif // _SHADING_