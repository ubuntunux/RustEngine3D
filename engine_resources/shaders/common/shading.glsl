#ifndef _SHADING_
#define _SHADING_

#include "scene_constants.glsl"
#include "PCFKernels.glsl"
#include "utility.glsl"
#include "../precomputed_atmosphere/atmosphere_predefined.glsl"

float get_shadow_factor_func(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const in mat4 inv_shadow_view_projection,
    const int sample_count,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    const vec2 shadow_texture_size = textureSize(texture_shadow, 0);
    const vec2 shadow_texel_size = 1.0 / shadow_texture_size;
    vec4 shadow_proj = shadow_view_projection * vec4(world_position, 1.0);
    shadow_proj.xyz /= shadow_proj.w;
    shadow_proj.xy = shadow_proj.xy * 0.5 + 0.5;

    if(1.0 < shadow_proj.x || shadow_proj.x < 0.0 || 1.0 < shadow_proj.y || shadow_proj.y < 0.0 || 1.0 < shadow_proj.z || shadow_proj.z < 0.0)
    {
        return 1.0;
    }

    /*
    float shadow_depth = texture(texture_shadow, shadow_proj.xy).x;
    vec4 shadow_ndc = vec4(shadow_proj.xy * 2.0 - 1.0, shadow_depth, 1.0);
    vec4 shadow_world_pos = inv_shadow_view_projection * shadow_ndc;
    shadow_world_pos.xyz /= shadow_world_pos.w;
    const float shadow_dist = length(world_position.xyz - shadow_world_pos.xyz);
    */
    const vec2 shadow_noise_radius = shadow_texel_size * 3.0;
    const float rot_angle = interleaved_gradient_noise(screen_pos) * TWO_PI;
    const float cs = cos(rot_angle);
    const float sn = sin(rot_angle);
    const mat2 rot = mat2(cs, -sn, sn, cs);

    float total_shadow_factor = 0.0;
    for(int sample_index = 0; sample_index < sample_count; ++sample_index)
    {
        const vec2 offset = (sample_count > 4) ? (rot * PoissonSamples[sample_index % PoissonSampleCount]) : PoissonSamples[sample_index % PoissonSampleCount];
        const vec2 shadow_uv = clamp(shadow_proj.xy + offset * shadow_noise_radius, vec2(0.0), vec2(1.0));
        const float sample_shadow_depth = textureLod(texture_shadow, shadow_uv, 0.0).x;
        float shadow_factor = shadow_proj.z < (sample_shadow_depth + depth_bias) ? 0.0 : 1.0;
        total_shadow_factor += shadow_factor;
    }
    return saturate(total_shadow_factor / float(sample_count));
}

float get_shadow_factor_simple(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const in mat4 inv_shadow_view_projection,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    return get_shadow_factor_func(time, screen_pos, world_position, shadow_view_projection, inv_shadow_view_projection, 1, depth_bias, texture_shadow);
}

float get_shadow_factor(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const in mat4 inv_shadow_view_projection,
    const in int sample_count,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    return get_shadow_factor_func(time, screen_pos, world_position, shadow_view_projection, inv_shadow_view_projection, sample_count, depth_bias, texture_shadow);
}

// Percentage-Closer Soft Shadows (PCSS): blocker search -> world-space penumbra width -> rotated Poisson PCF
float get_shadow_factor_pcss(
    const float time,
    const in ivec2 screen_pos,
    const in vec3 world_position,
    const in mat4 shadow_view_projection,
    const in mat4 inv_shadow_view_projection,
    const int sample_count,
    const float depth_bias,
    sampler2D texture_shadow
)
{
    const vec2 shadow_texture_size = vec2(textureSize(texture_shadow, 0));
    const vec2 shadow_texel_size = 1.0 / max(shadow_texture_size, vec2(1.0));
    vec4 shadow_proj = shadow_view_projection * vec4(world_position, 1.0);
    shadow_proj.xyz /= shadow_proj.w;
    shadow_proj.xy = shadow_proj.xy * 0.5 + 0.5;

    if (1.0 < shadow_proj.x || shadow_proj.x < 0.0 || 1.0 < shadow_proj.y || shadow_proj.y < 0.0 || 1.0 < shadow_proj.z || shadow_proj.z < 0.0)
    {
        return 1.0;
    }

    // Purely spatial IGN rotation eliminates temporal shadow strobing while bilateral filtering removes spatial dither
    const float rot_angle = interleaved_gradient_noise(screen_pos) * TWO_PI;
    const float cs = cos(rot_angle);
    const float sn = sin(rot_angle);
    const mat2 rot = mat2(cs, -sn, sn, cs);

    // Step 1: 8-tap rotated Poisson blocker search (Reverse-Z: larger depth = closer to sun)
    const int BLOCKER_SAMPLES = 8;
    const vec2 blocker_search_radius = shadow_texel_size * 4.5;
    float blocker_depth_sum = 0.0;
    int blocker_count = 0;
    for (int i = 0; i < BLOCKER_SAMPLES; ++i)
    {
        vec2 offset = rot * PoissonSamples[(i * 2) % PoissonSampleCount];
        vec2 uv = clamp(shadow_proj.xy + offset * blocker_search_radius, vec2(0.0), vec2(1.0));
        float sample_depth = textureLod(texture_shadow, uv, 0.0).x;
        if (shadow_proj.z < (sample_depth + depth_bias))
        {
            blocker_depth_sum += sample_depth;
            blocker_count += 1;
        }
    }

    if (blocker_count == 0)
    {
        return 1.0;
    }

    // Step 2: Estimate world-space receiver-to-blocker distance and penumbra radius in shadow texels
    float avg_blocker_depth = blocker_depth_sum / float(blocker_count);
    vec4 blocker_ndc = vec4(shadow_proj.xy * 2.0 - 1.0, avg_blocker_depth, 1.0);
    vec4 blocker_world = inv_shadow_view_projection * blocker_ndc;
    blocker_world.xyz /= max(abs(blocker_world.w), 1e-5);
    float blocker_dist = length(world_position - blocker_world.xyz);
    float penumbra_texels = clamp(1.10 + blocker_dist * 0.35, 1.10, 5.5);

    // Step 3: 16-tap Vogel-disk variable-radius PCF (uniform angular coverage avoids Poisson clumping)
    const int VOGEL_SAMPLES = 16;
    const float GOLDEN_ANGLE = 2.39996323;
    const vec2 filter_radius = shadow_texel_size * penumbra_texels;
    float total_shadow = 0.0;
    for (int i = 0; i < VOGEL_SAMPLES; ++i)
    {
        float r = sqrt((float(i) + 0.5) / float(VOGEL_SAMPLES));
        float theta = float(i) * GOLDEN_ANGLE + rot_angle;
        vec2 offset = vec2(cos(theta), sin(theta)) * r;
        vec2 uv = clamp(shadow_proj.xy + offset * filter_radius, vec2(0.0), vec2(1.0));
        float sample_depth = textureLod(texture_shadow, uv, 0.0).x;
        total_shadow += (shadow_proj.z < (sample_depth + depth_bias)) ? 0.0 : 1.0;
    }
    return saturate(total_shadow / float(VOGEL_SAMPLES));
}

// [Burley 2012, "Physically-Based Shading at Disney"]
float diffuse_burley( float Roughness, float NoV, float NoL, float VoH )
{
    float FD90 = 0.5 + 2 * VoH * VoH * Roughness;
    float FdV = 1 + (FD90 - 1) * pow( 1 - NoV, 5.0 );
    float FdL = 1 + (FD90 - 1) * pow( 1 - NoL, 5.0 );
    return ( (1 / PI) * FdV * FdL );
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

float D_blinn(float roughness, float NoH)
{
    float m = roughness * roughness;
    float m2 = m * m;
    float n = 2.0 / m2 - 2.0;
    return (n + 2.0) / (2.0 * PI) * pow(NoH, n);
}

float D_beckmann(float roughness, float NoH)
{
    float m = roughness * roughness;
    float m2 = m * m;
    float NoH2 = NoH * NoH;
    return exp((NoH2 - 1.0) / (m2 * NoH2)) / (PI * m2 * NoH2 * NoH2);
}

float DistributionGGX(float NoH, float roughness)
{
    const float a = roughness * roughness;
    const float a2 = a * a;
    const float NoH2 = NoH * NoH;
    const float nom = a2;
    float denom = (NoH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;
    return nom / denom;
}

float GeometrySchlickGGX(float NoV, float roughness)
{
    const float r = (roughness + 1.0);
    const float k = (r * r) / 8.0;
    const float num = NoV;
    const float denom = NoV * (1.0 - k) + k;
    return num / denom;
}

float GeometrySmith(float NoV, float NoL, float roughness)
{
    float ggx2  = GeometrySchlickGGX(NoV, roughness);
    float ggx1  = GeometrySchlickGGX(NoL, roughness);
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
vec3 blinn_specular(in float NoH, in vec3 specular, in float roughness)
{
    float k = 1.999 / (roughness * roughness);
    return min(1.0, 3.0 * 0.0398 * k) * pow(NoH, min(10000.0, k)) * specular;
}

vec3 cooktorrance_specular(vec3 F, float NoL, float NoV, float NoH, float roughness)
{
    float NDF = DistributionGGX(NoH, roughness);
    float G = GeometrySmith(NoV, NoL, roughness);
    vec3 numerator = NDF * G * F;
    float denominator = 4.0 * NoV * NoL;
    return numerator / max(denominator, 0.00001);
}

vec2 env_BRDF_pproximate(float NoV, float roughness)
{
    // see https://www.unrealengine.com/blog/physically-based-shading-on-mobile
    const vec4 c0 = vec4(-1.0, -0.0275, -0.572,  0.022);
    const vec4 c1 = vec4( 1.0,  0.0425,  1.040, -0.040);
    vec4 r = roughness * c0 + c1;
    float a004 = min(r.x * r.x, exp2(-9.28 * NoV)) * r.x + r.y;
    return vec2(-1.04, 1.04) * a004 + r.zw;
}

// [Jimenez et al. 2016, "Practical Realtime Strategies for Accurate Indirect Occlusion"]
vec3 multi_bounce_ao(float visibility, vec3 albedo)
{
    vec3 a = 2.0404 * albedo - 0.3324;
    vec3 b = -4.7951 * albedo + 0.6417;
    vec3 c = 2.7552 * albedo + 0.6903;
    return max(vec3(visibility), ((visibility * a + b) * visibility + c) * visibility);
}

// [Lagarde 2014, "Moving Frostbite to PBR"]
float compute_specular_occlusion(float NoV, float ao, float roughness)
{
    return saturate(pow(NoV + ao, exp2(-16.0 * roughness - 1.0)) - 1.0 + ao);
}

void apply_image_based_lighting(
    const uint render_option,
    const in samplerCube texture_probe,
    const in vec4 scene_reflect_color,
    const in vec4 scene_gi_color,
    const in vec3 scene_sky_irradiance,
    const in vec3 base_color,
    float shadow_factor,
    float raw_NoL,
    float ao_factor,
    const float sky_visibility,
    float roughness,
    const in vec3 F0,
    const in vec3 sun_direction,
    const in vec3 N,
    const in vec3 bent_normal,
    const in vec3 R,
    float NoV,
    inout vec3 diffuse_light,
    inout vec3 specular_light
)
{
    const vec2 env_size = textureSize(texture_probe, 0);
    const float max_env_mipmap = min(8.0, log2(max(env_size.x, env_size.y)) - 1.0);
    // Mips 0..(max_env_mipmap - 2) store GGX prefiltered specular chain; (max_env_mipmap - 1) stores cosine-weighted diffuse irradiance
    const float max_spec_mipmap = max(1.0, max_env_mipmap - 2.0);
    const float diffuse_irradiance_mipmap = max(1.0, max_env_mipmap - 1.0);

    vec2 envBRDF = env_BRDF_pproximate(NoV, roughness);
    // Multi-Scattering GGX energy compensation (Turquin 2019 / Fdez-Aguera)
    float Ess = envBRDF.x + envBRDF.y;
    float Ems = 1.0 - Ess;
    vec3 Favg = F0 + (1.0 - F0) / 21.0;
    vec3 single_scatter_brdf = F0 * envBRDF.x + vec3(envBRDF.y);
    vec3 Fms = single_scatter_brdf * Favg / max(vec3(1.0) - Favg * Ems, vec3(1e-4));
    vec3 specular_brdf = single_scatter_brdf + Fms * Ems;
    const vec3 kS = fresnelSchlickRoughness(NoV, F0, roughness);
    const vec3 kD = vec3(1.0) - kS;

    vec3 ibl_diffuse_light = textureLod(texture_probe, bent_normal, diffuse_irradiance_mipmap).xyz;
    vec3 ibl_specular_light = textureLod(texture_probe, R, roughness * max_spec_mipmap).xyz;

    // Multi-bounce GI boost: single-scatter sky probe underestimates real-world multi-bounce ambient fill
    const float IBL_DIFFUSE_INTENSITY = 1.45;
    const float IBL_SPECULAR_INTENSITY = 1.20;
    ibl_diffuse_light *= IBL_DIFFUSE_INTENSITY;
    ibl_specular_light *= IBL_SPECULAR_INTENSITY;

    // Supplement with physical hemisphere sky irradiance fallback & gentle cave/dawn floor without washing out shadows
    float sky_hemi = bent_normal.y * 0.5 + 0.5;
    vec3 sky_irrad_fill = (scene_sky_irradiance / PI) * mix(0.40, 1.0, sky_hemi) * 0.70;
    vec3 min_ambient_fill = mix(vec3(0.04, 0.038, 0.034), vec3(0.045, 0.052, 0.065), sky_hemi);
    ibl_diffuse_light = max(ibl_diffuse_light, sky_irrad_fill) + min_ambient_fill;

    // Keep outdoor tree canopies and cave interiors readable (CaptureHeightMap sets sky_visibility=0 under trees too)
    float sky_vis_factor = mix(0.55, 1.0, sky_visibility);
    vec3 diffuse_ao = multi_bounce_ao(ao_factor, base_color);
    float specular_ao = compute_specular_occlusion(NoV, ao_factor, roughness);

    // Cube Map-based Shadow Tint: enrich sky-colored chromatic ambient inside shadowed areas (ToD & biome aware)
    if ((render_option & RenderOption_RenderShadowTint) != 0u)
    {
        float shadow_mask = saturate(1.0 - shadow_factor * saturate(raw_NoL));
        vec3 zenith_sky = textureLod(texture_probe, vec3(0.0, 1.0, 0.0), diffuse_irradiance_mipmap).xyz * IBL_DIFFUSE_INTENSITY;
        float zenith_luma = max(get_luminance(zenith_sky), 1e-4);
        vec3 zenith_hue = clamp(zenith_sky / zenith_luma, vec3(0.65), vec3(1.55));
        float tint_weight = shadow_mask * mix(0.25, 1.0, sky_visibility);
        float diff_luma = get_luminance(ibl_diffuse_light);
        vec3 saturated_diff = max(vec3(0.0), vec3(diff_luma) + (ibl_diffuse_light - vec3(diff_luma)) * (1.0 + 0.45 * tint_weight));
        ibl_diffuse_light = mix(ibl_diffuse_light, saturated_diff * mix(vec3(1.0), zenith_hue, 0.35), tint_weight * 0.70);
    }

    ibl_diffuse_light *= sky_vis_factor * diffuse_ao;
    ibl_specular_light *= sky_vis_factor * specular_ao;

    // Blend SSR into specular IBL
    {
        ibl_specular_light = mix(ibl_specular_light, scene_reflect_color.xyz, scene_reflect_color.w);
    }

    // 1-bounce Screen-Space GI diffuse color bleeding
    diffuse_light += scene_gi_color.xyz * kD * diffuse_ao * 0.75;

    diffuse_light += ibl_diffuse_light * kD;
    specular_light.xyz += ibl_specular_light * specular_brdf;

    // Cube Map-based Stylized Rim Light: broad prefiltered env sample along grazing silhouette (ToD & roughness aware)
    if ((render_option & RenderOption_RenderRimLight) != 0u)
    {
        vec3 rim_dir = safe_normalize(mix(R, N + vec3(0.0, 0.35, 0.0), 0.35));
        vec3 rim_env = textureLod(texture_probe, rim_dir, max_spec_mipmap * 0.65).xyz;
        // Soft HDR clamp so direct sun-disk pixels in the cube map do not blow out silhouette edges
        rim_env = rim_env / (1.0 + get_luminance(rim_env) * 0.65);
        float rim_fresnel = pow(1.0 - NoV, 3.2);
        float rim_facing = smoothstep(-0.35, 0.55, N.y) * (0.45 + 0.55 * saturate(raw_NoL * 0.5 + 0.5));
        float rim_roughness_mask = mix(0.25, 1.0, roughness);
        float rim_cave_mask = mix(0.28, 1.0, sky_visibility) * ao_factor;
        specular_light.xyz += rim_env * rim_fresnel * rim_facing * rim_roughness_mask * rim_cave_mask * 0.55;
    }
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
    const in LIGHT_DATA light_data,
    const in POINT_LIGHTS point_lights,
    vec3 base_color,
    float opacity,
    float metallic,
    float roughness,
    float reflectance,
    float shadow_factor,
    float ao_factor,
    uint shading_model,
    const in vec4 scene_reflect_color,
    const in vec4 scene_gi_color,
    const in samplerCube texture_probe,
    const in sampler2D texture_shadow,
    const in sampler2D texture_height_map,
    const in vec2 screen_texcoord,
    const in vec3 world_position,
    const in vec3 vertexNormal,
    vec3 N,
    vec3 bent_normal,
    const in vec3 V,
    const in float depth,
    const in bool IS_COMBINED_SHADOW
)
{
    const vec3 L = normalize(-light_data.LIGHT_DIRECTION);
    vec3 light_color = light_data.LIGHT_COLOR.xyz;
    float scene_linear_depth = device_depth_to_linear_depth(view_constants.NEAR_FAR.x, view_constants.NEAR_FAR.y, depth);

    #if SHADER_STAGE_FLAG == FRAGMENT
    const ivec2 frag_pixel_pos = ivec2(gl_FragCoord.xy);
    #else
    const ivec2 frag_pixel_pos = ivec2(screen_texcoord * scene_constants.SCREEN_SIZE);
    #endif

    float sky_visibility = get_shadow_factor(
        scene_constants.TIME,
        frag_pixel_pos,
        world_position,
        view_constants.CAPTURE_HEIGHT_MAP_VIEW_PROJECTION,
        view_constants.INV_CAPTURE_HEIGHT_MAP_VIEW_PROJECTION,
        4,
        -0.0005,
        texture_height_map
    );

    // under water
    float sea_diff = world_position.y - scene_constants.SEA_HEIGHT;
    float inv_sea_ratio = saturate(exp(sea_diff * 0.5));
    float under_water_material = saturate(-(sea_diff - 1.0) / SEA_COASTLINE_THICKNESS);
    under_water_material = 1.0 - under_water_material * under_water_material * 0.5;
    roughness *= under_water_material;
    base_color *= under_water_material;

    // Weather Wetness, Procedural Puddles & Animated Raindrop Ripples
    float surface_wetness = 0.0;
    float puddle_mask = 0.0;
    vec2 ripple_grad = vec2(0.0);
    compute_surface_wetness_and_puddle(
        world_position,
        scene_constants.SEA_HEIGHT,
        vertexNormal,
        sky_visibility,
        shading_model,
        scene_constants.WETNESS,
        scene_constants.RAIN_INTENSITY,
        scene_constants.TIME,
        scene_linear_depth,
        surface_wetness,
        puddle_mask,
        ripple_grad
    );
    if (surface_wetness > 0.001)
    {
        // Porous dielectric darkening & wet sheen roughness reduction
        float porosity = (1.0 - metallic) * smoothstep(0.15, 0.85, roughness);
        base_color *= mix(1.0, 0.56, surface_wetness * porosity * 0.85);
        roughness = mix(roughness, min(roughness, 0.22), surface_wetness * 0.85);

        if (puddle_mask > 0.001)
        {
            base_color *= mix(1.0, 0.72, puddle_mask * (1.0 - metallic));
            roughness = mix(roughness, 0.04, puddle_mask);
            vec3 puddle_normal = normalize(vec3(ripple_grad.x, 1.0, ripple_grad.y));
            N = normalize(mix(N, puddle_normal, puddle_mask));
            bent_normal = normalize(mix(bent_normal, puddle_normal, puddle_mask));
        }
    }

    // safe roughness
    roughness = clamp(roughness, 0.04, 1.0);

    // compute material reflectance
    const vec3 R = reflect(-V, N);
    const vec3 H = normalize(V + L);

    const float raw_NoL = dot(N, L);
    float NoL = raw_NoL;
    if ((scene_constants.RENDER_OPTION & RenderOption_RenderHalfLambert) != 0u)
    {
        NoL = pow(saturate(NoL * 0.5 + 0.5), 2.0);
    }
    const float clampled_NoL = saturate(NoL);
    const float specular_clampled_NoL = saturate(raw_NoL);
    const float NoV = clamp(dot(N, V), 0.001, 1.0);
    const float NoH = clamp(dot(N, H), 0.001, 1.0);
    const float VoH = clamp(dot(H, V), 0.001, 1.0);

    // Atmosphere
    const bool doLambertianReflectance = false;
    vec3 scene_in_scatter = vec3(0.0);
    vec3 scene_sun_irradiance = vec3(1.0);
    vec3 scene_sky_irradiance = vec3(0.0);
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
        L,
        N,
        doLambertianReflectance,
        scene_sun_irradiance,
        scene_sky_irradiance,
        scene_in_scatter
    );

    vec3 result = vec3(0.0, 0.0, 0.0);
    vec3 F0 = mix(vec3(max(0.04, reflectance)), base_color.xyz, metallic);
    // Multi-Scattering GGX direct specular energy compensation (Turquin 2019)
    vec2 direct_env_brdf = env_BRDF_pproximate(NoV, roughness);
    float direct_ess = max(direct_env_brdf.x + direct_env_brdf.y, 0.05);
    vec3 ms_energy_comp = vec3(1.0) + F0 * (1.0 / direct_ess - 1.0);
    vec3 diffuse_light = vec3(0.0, 0.0, 0.0);
    vec3 specular_light = vec3(0.0, 0.0, 0.0);

    if(!IS_COMBINED_SHADOW && ((scene_constants.RENDER_OPTION & RenderOption_RenderShadow) != 0u)) {
        if ((scene_constants.RENDER_OPTION & RenderOption_RenderPCSS) != 0u)
        {
            shadow_factor *= get_shadow_factor_pcss(
                scene_constants.TIME,
                frag_pixel_pos,
                world_position,
                light_data.SHADOW_VIEW_PROJECTION,
                light_data.INV_SHADOW_VIEW_PROJECTION,
                light_data.SHADOW_SAMPLES,
                SHADOW_BIAS,
                texture_shadow
            );
        }
        else
        {
            shadow_factor *= get_shadow_factor(
                scene_constants.TIME,
                frag_pixel_pos,
                world_position,
                light_data.SHADOW_VIEW_PROJECTION,
                light_data.INV_SHADOW_VIEW_PROJECTION,
                light_data.SHADOW_SAMPLES,
                SHADOW_BIAS,
                texture_shadow
            );
        }
    }

    // Physical direct sun visibility: zero in complete shadow or below horizon
    float sun_horizon_fade = smoothstep(0.0, 0.05, L.y);
    shadow_factor = saturate(shadow_factor) * sun_horizon_fade;

    vec3 unshadowed_sun_color = light_color * scene_sun_irradiance * sun_horizon_fade;
    light_color = unshadowed_sun_color * shadow_factor;

    // Attenuate clear-sky irradiance fallback under heavy cloud cover and rain
    float sky_weather_scale = clamp(get_luminance(light_data.LIGHT_COLOR.xyz) / 1.2, 0.35, 1.0);
    float cloud_sky_dim = mix(1.0, 0.55, smoothstep(0.55, 1.0, atmosphere_constants.cloud_coverage));
    scene_sky_irradiance *= min(sky_weather_scale, cloud_sky_dim);

    // Image based lighting (indirect sky + ground bounce + SSGI, modulated by AO & sky_visibility, NOT direct shadow)
    apply_image_based_lighting(
        scene_constants.RENDER_OPTION,
        texture_probe,
        scene_reflect_color,
        scene_gi_color,
        scene_sky_irradiance,
        base_color,
        shadow_factor,
        raw_NoL,
        ao_factor,
        sky_visibility,
        roughness,
        F0,
        L,
        N,
        bent_normal,
        R,
        NoV,
        diffuse_light,
        specular_light
    );

    #if TRANSPARENT_MATERIAL == 1
    {
        vec3 fresnel = fresnelSchlick(NoV, F0);
        float reflectivity = max(max(fresnel.r, fresnel.g), fresnel.b);
        opacity = clamp(opacity + opacity * reflectivity, 0.0, 1.0);
    }
    #endif

    // Analytic 1-bounce sunlit terrain GI fill (attenuated where SSGI provides real screen-space color bleeding)
    {
        vec3 bounce_dir = normalize(vec3(-L.x, 0.35, -L.z));
        float bounce_ndl = saturate(dot(bent_normal, bounce_dir) * 0.5 + 0.5);
        float ground_facing = saturate(-bent_normal.y * 0.5 + 0.5);
        vec3 warm_bounce_tint = mix(vec3(0.95, 0.88, 0.74), atmosphere_constants.ground_bounce_albedo * 2.1, ground_facing);
        vec3 diffuse_ao = multi_bounce_ao(ao_factor, base_color);
        float analytic_gi_weight = mix(1.0, 0.35, scene_gi_color.w);
        diffuse_light += unshadowed_sun_color * (0.08 / PI) * bounce_ndl * warm_bounce_tint * mix(0.50, 1.0, sky_visibility) * diffuse_ao * analytic_gi_weight;
    }

    // Directional Light
    if (clampled_NoL > 0.0)
    {
        diffuse_light += diffuse_burley(roughness, NoV, clampled_NoL, VoH) * clampled_NoL * light_color;
    }
    if (specular_clampled_NoL > 0.0)
    {
        const vec3 F = fresnelSchlick(VoH, F0);
        specular_light += cooktorrance_specular(F, specular_clampled_NoL, NoV, NoH, roughness) * ms_energy_comp * specular_clampled_NoL * light_color;
    }

    // Stylized Shading Models (Foliage backlit SSS & Character skin wrap + soft rim)
    if ((scene_constants.RENDER_OPTION & RenderOption_RenderFoliageSSS) != 0u)
    {
        float foliage_mask = 0.0;
        if (shading_model == ShadingModel_Foliage)
        {
            foliage_mask = 1.0;
        }
        else if (shading_model == ShadingModel_Default)
        {
            // Automatically detect matte green vegetation (trees, bushes, vines, grass) even without explicit ShadingModel tag
            float green_dominance = base_color.g - max(base_color.r * 0.92, base_color.b * 1.05);
            foliage_mask = saturate(green_dominance * 5.0) * smoothstep(0.30, 0.70, roughness) * (1.0 - metallic);
        }
        if (foliage_mask > 0.01)
        {
            vec3 back_lit_dir = normalize(L + N * 0.35);
            float back_scatter = pow(saturate(dot(V, -back_lit_dir)), 3.0);
            float wrap_diffuse = saturate((raw_NoL + 0.45) / 1.45) * 0.24;
            // Allow transmitted sunlight through thin leaves even on back-facing shaded side of canopy
            vec3 sss_sun = unshadowed_sun_color * mix(0.35, 1.0, shadow_factor) * mix(0.40, 1.0, sky_visibility);
            vec3 foliage_tint = vec3(1.15, 1.28, 0.62);
            diffuse_light += foliage_mask * (back_scatter * 0.75 + wrap_diffuse) * (1.0 / PI) * sss_sun * foliage_tint;
        }
    }

    if (shading_model == ShadingModel_Character)
    {
        // Warm skin wrap lighting across the shadow terminator
        float wrap_NoL = saturate((raw_NoL + 0.35) / 1.35);
        float skin_wrap = max(0.0, wrap_NoL - clampled_NoL) * 0.35;
        diffuse_light += skin_wrap * (1.0 / PI) * light_color * vec3(1.15, 0.90, 0.80);

        // Soft Fresnel rim light from sky/environment for silhouette clarity in shade and caves
        float rim = pow(1.0 - NoV, 3.0) * smoothstep(-0.25, 0.65, N.y);
        vec3 rim_color = max(scene_sky_irradiance * 0.14, vec3(0.10, 0.12, 0.14));
        specular_light += rim * rim_color * 0.35 * mix(0.45, 1.0, sky_visibility) * ao_factor;
    }

    // apply sea ratio
    diffuse_light *= inv_sea_ratio;
    specular_light *= inv_sea_ratio;

    // Point Lights (physical inverse-square attenuation with smooth radius window)
    const int light_count = min(MAX_POINT_LIGHTS, scene_constants.RENDER_POINT_LIGHT_COUNT);
    for(int i = 0; i < light_count; ++i)
    {
        const float point_light_radius = point_lights.point_light_data[i].RADIUS;
        vec3 to_point_light = point_lights.point_light_data[i].LIGHT_POSITION - world_position;
        const float point_light_dist = length(to_point_light);
        if(point_light_dist <= point_light_radius && point_light_dist > 0.0001)
        {
            vec3 point_light_dir = to_point_light / point_light_dist;
            const float raw_point_NoL = dot(N, point_light_dir);
            if(raw_point_NoL > 0.0)
            {
                const vec3 point_light_half = normalize(V + point_light_dir);
                const float dist_ratio = point_light_dist / max(point_light_radius, 0.001);
                float window = saturate(1.0 - dist_ratio * dist_ratio * dist_ratio * dist_ratio);
                window *= window;
                const float point_light_attenuation = window / (1.0 + point_light_dist * point_light_dist * 0.25);
                const vec3 point_light_color = point_lights.point_light_data[i].LIGHT_COLOR * point_light_attenuation;

                const float point_light_NoL = saturate(raw_point_NoL);
                const float point_light_NoH = clamp(dot(N, point_light_half), 0.001, 1.0);
                const float point_light_VoH = clamp(dot(V, point_light_half), 0.001, 1.0);
                const vec3 point_light_F = fresnelSchlick(point_light_VoH, F0);

                diffuse_light += diffuse_burley(roughness, NoV, point_light_NoL, point_light_VoH) * point_light_NoL * point_light_color;
                specular_light += cooktorrance_specular(point_light_F, point_light_NoL, NoV, point_light_NoH, roughness) * ms_energy_comp * point_light_NoL * point_light_color;
            }
        }
    }

    // Moon Light (only active at dusk/night, with elevation fade and subtle Cook-Torrance specular)
    float night_factor = smoothstep(0.15, -0.1, L.y);
    if (night_factor > 0.0)
    {
        vec3 moon_light_dir = -L;
        float moon_elevation = smoothstep(-0.05, 0.20, moon_light_dir.y);
        float moon_weight = night_factor * moon_elevation * mix(0.30, 1.0, sky_visibility) * ao_factor;
        const float moon_light_NoL = saturate(dot(N, moon_light_dir) * 0.5 + 0.5);
        diffuse_light += moon_light_NoL * scene_constants.MOON_LIGHT_COLOR * moon_weight;

        float raw_moon_NoL = saturate(dot(N, moon_light_dir));
        if (raw_moon_NoL > 0.001 && moon_weight > 0.001)
        {
            vec3 moon_H = normalize(V + moon_light_dir);
            float moon_NoH = clamp(dot(N, moon_H), 0.001, 1.0);
            float moon_VoH = clamp(dot(V, moon_H), 0.001, 1.0);
            vec3 moon_F = fresnelSchlick(moon_VoH, F0);
            specular_light += cooktorrance_specular(moon_F, raw_moon_NoL, NoV, moon_NoH, roughness)
                * ms_energy_comp * raw_moon_NoL * scene_constants.MOON_LIGHT_COLOR * moon_weight * 0.45;
        }
    }

    // Final PBR energy-conserving combination (F0 is already applied in specular_light)
    result = diffuse_light * base_color * (1.0 - max(reflectance, metallic));
    result += specular_light;

    // Exponential Height Fog (analytic height-falloff optical depth + cube-map horizon color + Mie sun in-scattering)
    if ((scene_constants.RENDER_OPTION & RenderOption_RenderHeightFog) != 0u && scene_constants.HEIGHT_FOG_INTENSITY > 0.001)
    {
        const float fog_start_dist = 18.0;
        float eff_dist = max(0.0, scene_linear_depth - fog_start_dist);
        if (eff_dist > 0.01)
        {
            const float fog_intensity = max(scene_constants.HEIGHT_FOG_INTENSITY, 0.0);
            const float fog_density = 0.0022 * fog_intensity;
            const float fog_height_falloff = 0.045;
            float cam_y = max(view_constants.CAMERA_POSITION.y - scene_constants.SEA_HEIGHT, 0.0);
            float frag_y = max(world_position.y - scene_constants.SEA_HEIGHT, 0.0);
            float dy = frag_y - cam_y;
            float c_cam = exp(-fog_height_falloff * cam_y);
            float height_integral = (abs(dy) > 0.001)
                ? (1.0 - exp(-fog_height_falloff * dy)) / (fog_height_falloff * dy)
                : 1.0;
            float fog_max_opacity = saturate(0.65 * min(fog_intensity, 1.45));
            float fog_amount = saturate((1.0 - exp(-fog_density * eff_dist * c_cam * height_integral)) * fog_max_opacity);
            fog_amount *= mix(0.25, 1.0, sky_visibility);

            const vec2 env_size = vec2(textureSize(texture_probe, 0));
            const float fog_mip = max(1.0, min(8.0, log2(max(env_size.x, env_size.y)) - 1.0) - 1.0);
            vec3 horizon_dir = safe_normalize(vec3(-V.x, 0.14, -V.z));
            vec3 fog_color = textureLod(texture_probe, horizon_dir, fog_mip).xyz * 1.15;
            float sun_scatter = pow(saturate(dot(-V, L)), 6.0);
            fog_color += unshadowed_sun_color * (0.18 / PI) * sun_scatter;
            fog_color = max(fog_color, vec3(0.04, 0.05, 0.065));
            result = mix(result, fog_color, fog_amount);
        }
    }

    return vec4(max(vec3(0.0), result), opacity);
}

#endif // _SHADING_