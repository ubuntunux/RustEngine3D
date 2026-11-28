#ifndef _UTILITY_
#define _UTILITY_

#include "scene_constants.glsl"

const int FilterTypes_Box = 0;
const int FilterTypes_Triangle = 1;
const int FilterTypes_Gaussian = 2;
const int FilterTypes_BlackmanHarris = 3;
const int FilterTypes_Smoothstep = 4;
const int FilterTypes_BSpline = 5;
const int FilterTypes_CatmullRom = 6;
const int FilterTypes_Mitchell = 7;
const int FilterTypes_GeneralizedCubic = 8;
const int FilterTypes_Sinc = 9;

const mat4 Mat4Identity = mat4(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);

float FilterBox(in float x)
{
    return x <= 1.0 ? 1.0 : 0.0;
}

float FilterTriangle(in float x)
{
    return clamp(1.0f - x, 0.0, 1.0);
}

float FilterGaussian(in float x)
{
    const float sigma = 0.25;
    const float g = 1.0f / sqrt(2.0f * 3.14159f * sigma * sigma);
    return (g * exp(-(x * x) / (2 * sigma * sigma)));
}

float FilterCubic(in float x, in float B, in float C)
{
    float y = 0.0f;
    float x2 = x * x;
    float x3 = x * x * x;

    if(x < 1)
    {
        y = (12 - 9 * B - 6 * C) * x3 + (-18 + 12 * B + 6 * C) * x2 + (6 - 2 * B);
    }
    else if(x <= 2)
    {
        y = (-B - 6 * C) * x3 + (6 * B + 30 * C) * x2 + (-12 * B - 48 * C) * x + (8 * B + 24 * C);
    }

    return y / 6.0f;
}

float FilterSinc(in float x, in float filterRadius)
{
    float s;
    x *= filterRadius * 2.0f;
    if(x < 0.001f)
    s = 1.0f;
    else
    s = sin(x * PI) / (x * PI);
    return s;
}

float FilterBlackmanHarris(in float x)
{
    x = 1.0f - x;
    const float a0 = 0.35875f;
    const float a1 = 0.48829f;
    const float a2 = 0.14128f;
    const float a3 = 0.01168f;
    return clamp(a0 - a1 * cos(PI * x) + a2 * cos(2 * PI * x) - a3 * cos(3 * PI * x), 0.0, 1.0);
}

float FilterSmoothstep(in float x)
{
    return 1.0f - smoothstep(0.0f, 1.0f, x);
}

float Filter(in float x, in int filterType, in float filterRadius, in bool rescaleCubic)
{
    // Cubic filters naturually work in a [-2, 2] domain. For the resolve case we
    // want to rescale the filter so that it works in [-1, 1] instead
    float cubicX = rescaleCubic ? x * 2.0f : x;

    if(filterType == FilterTypes_Box)
    return FilterBox(x);
    else if(filterType == FilterTypes_Triangle)
    return FilterTriangle(x);
    else if(filterType == FilterTypes_Gaussian)
    return FilterGaussian(x);
    else if(filterType == FilterTypes_BlackmanHarris)
    return FilterBlackmanHarris(x);
    else if(filterType == FilterTypes_Smoothstep)
    return FilterSmoothstep(x);
    else if(filterType == FilterTypes_BSpline)
    return FilterCubic(cubicX, 1.0, 0.0f);
    else if(filterType == FilterTypes_CatmullRom)
    return FilterCubic(cubicX, 0, 0.5f);
    else if(filterType == FilterTypes_Mitchell)
    return FilterCubic(cubicX, 1 / 3.0f, 1 / 3.0f);
    else if(filterType == FilterTypes_GeneralizedCubic)
    return FilterCubic(cubicX, 0.33, 0.33);
    else if(filterType == FilterTypes_Sinc)
    return FilterSinc(x, filterRadius);
    else
    return 1.0f;
}

bool check_flags_any(uint a, uint b) { return 0 != (a & b); }
bool check_flags_all(uint a, uint b) { return a == (a & b); }

float saturate(float value) { return clamp(value, 0.0, 1.0); }
vec2 saturate(vec2 value) { return clamp(value, 0.0, 1.0); }
vec3 saturate(vec3 value) { return clamp(value, 0.0, 1.0); }
vec4 saturate(vec4 value) { return clamp(value, 0.0, 1.0); }

float get_luminance(vec3 color)
{
    return dot(vec3(0.2126, 0.7152, 0.0722), color);
}

vec4 uint_color_to_float_color(uint color) {
    const uint rmask = uint(255);
    const uint gmask = uint(255 << 8);
    const uint bmask = uint(255 << 16);
    const uint amask = uint(255 << 24);
    vec4 fColor = saturate(vec4(color & rmask, (color & gmask) >> 8, (color & bmask) >> 16, (color & amask) >> 24) / 255.0);
    fColor.xyz = pow(fColor.xyz, vec3(2.2));
    return fColor;
}

// depth(1.0 ~ 0.0) to linear depth(near ~ far)
float device_depth_to_linear_depth(const float zNear, const float zFar, const float depth)
{
    return zNear * zFar / (zNear + depth * (zFar - zNear));
}

// vectorized version
vec4 device_depth_to_linear_depth(const vec4 zNear, const vec4 zFar, const vec4 depth)
{
    return zNear * zFar / (zNear + depth * (zFar - zNear));
}

// linear depth(near ~ far) to device depth(1.0 ~ 0.0)
float linear_depth_to_device_depth(const float zNear, const float zFar, const float linear_depth)
{
    return saturate((zNear - (zNear * zFar / linear_depth)) / (zNear - zFar));
}

// vectorized version
vec4 linear_depth_to_device_depth(const vec4 zNear, const vec4 zFar, const vec4 linear_depth)
{
    return saturate((zNear - (zNear * zFar / linear_depth)) / (zNear - zFar));
}

vec4 relative_world_from_device_depth(const in mat4x4 inv_view_origin_projection, const in vec2 tex_coord, const float depth)
{
    vec4 ndc = vec4(tex_coord.xy * 2.0 - 1.0, depth, 1.0);
    vec4 relative_pos = inv_view_origin_projection * ndc;
    relative_pos.xyz /= relative_pos.w;
    return relative_pos;
}

// Interleaved gradient noise
float interleaved_gradient_noise(ivec2 pos)
{
    return mod(52.9829189 * mod(0.06711056 * float(pos.x) + 0.00583715 * float(pos.y), 1.0), 1.0f);
}


// generate random
float random(inout uint seed)
{
    seed = seed * 214013 + 2531011;
    uint ret = (seed >> 16) & 0xffff;
    seed = seed * 214013 + 2531011;
    ret = (ret | (seed & 0xffff0000));
    float result = float(ret % 0x7fff) / float(0x7fff - 1);
    seed = (seed >> 1) + (floatBitsToUint(result) >> 1);
    return result;
}

vec2 generate_random2(inout uint randomSeed)
{
    return vec2(random(randomSeed), random(randomSeed));
}

vec3 generate_random3(inout uint randomSeed)
{
    return vec3(random(randomSeed), random(randomSeed), random(randomSeed));
}

vec4 generate_random4(inout uint randomSeed)
{
    return vec4(random(randomSeed), random(randomSeed), random(randomSeed), random(randomSeed));
}

// @param xy should be a integer position (e.g. pixel position on the screen), repeats each 128x128 pixels
// similar to a texture lookup but is only ALU
// ~13 ALU operations (3 frac, 6 *, 4 mad)
float PseudoRandom(vec2 xy)
{
    vec2 pos = fract(xy / 128.0f) * 128.0f + vec2(-64.340622f, -72.465622f);

    // found by experimentation
    return fract(dot(pos.xyx * pos.xyy, vec3(20.390625f, 60.703125f, 2.4281209f)));
}

float rand(vec2 co){
    return fract(sin(dot(co.xy, vec2(12.9898, 78.233))) * 43758.5453123);
}

float rand3(vec3 uvw, float scale)
{
    return fract(sin(dot(uvw, vec3(12.9898, 78.233, 45.164))) * 43758.5453123);
}

float rand4(vec4 seed4){
    return fract(sin(dot(seed4, vec4(12.9898, 78.233, 45.164, 94.673))) * 43758.5453123);
}

vec3 invert_y(vec3 vector)
{
    return vec3(vector.x, -vector.y, vector.z);
}

float safe_atan(float y, float x)
{
    return mod(atan(y, x), TWO_PI);
}

vec2 safe_normalize(vec2 vector)
{
    float dist = length(vector);
    return vector / (dist == 0.0 ? 1.0 : dist);
}

vec3 safe_normalize(vec3 vector)
{
    float dist = length(vector);
    return vector / (dist == 0.0 ? 1.0 : dist);
}

vec4 safe_normalize(vec4 vector)
{
    float dist = length(vector);
    return vector / (dist == 0.0 ? 1.0 : dist);
}

// Reconstruct tangent-space normal from RG channels (compatible with BC5_UNORM and RGB/RGBA normal maps)
vec3 unpack_normal_map(vec2 encoded_xy)
{
    vec2 xy = encoded_xy * 2.0 - 1.0;
    float z = sqrt(max(0.0, 1.0 - dot(xy, xy)));
    return vec3(xy, z);
}

#if SHADER_STAGE_FLAG == FRAGMENT
float median(float r, float g, float b)
{
    return max(min(r, g), min(max(r, g), b));
}

float distance_field_font_opacity(vec4 font_sample)
{
    // MTSDF (Multi-channel True Signed Distance Field)
    // Reconstruct sharp-cornered signed distance from RGB channels via median filter
    float dist = median(font_sample.r, font_sample.g, font_sample.b);
    float grad = (font_sample.a > 0.0) ? min(fwidth(dist), fwidth(font_sample.a)) : fwidth(dist);
    float afwidth = clamp(grad * 0.85, 0.015, 0.10);
    const float edge = 0.38;
    return smoothstep(edge - afwidth, edge + afwidth, dist);
}

float distance_field_font_opacity(float dist)
{
    // Single-channel True SDF fallback (e.g. from font_sample.a)
    float afwidth = clamp(fwidth(dist) * 0.85, 0.015, 0.10);
    const float edge = 0.38;
    return smoothstep(edge - afwidth, edge + afwidth, dist);
}
#endif

float encode_emissive_intensity(float emissive_intensity)
{
    return saturate(emissive_intensity * 0.1);
}

float decode_emissive_intensity(float emissive_intensity)
{
    return emissive_intensity * 10.0;
}

// Octahedral normal encoding to [0, 1]^2 (Cigolle et al. 2014)
vec2 oct_sign_not_zero(vec2 v)
{
    return vec2((v.x >= 0.0) ? 1.0 : -1.0, (v.y >= 0.0) ? 1.0 : -1.0);
}

vec2 oct_encode_normal(vec3 n)
{
    float denom = abs(n.x) + abs(n.y) + abs(n.z);
    vec2 p = n.xy / max(denom, 0.00001);
    if (n.z < 0.0)
    {
        p = (1.0 - abs(p.yx)) * oct_sign_not_zero(p);
    }
    return p * 0.5 + 0.5;
}

vec3 oct_decode_normal(vec2 e)
{
    vec2 p = e * 2.0 - 1.0;
    vec3 n = vec3(p.xy, 1.0 - abs(p.x) - abs(p.y));
    if (n.z < 0.0)
    {
        n.xy = (1.0 - abs(n.yx)) * oct_sign_not_zero(n.xy);
    }
    return safe_normalize(n);
}

float encode_shading_model(uint shading_model)
{
    return float(shading_model) / 255.0;
}

uint decode_shading_model(float encoded)
{
    return uint(floor(encoded * 255.0 + 0.5));
}

mat4 make_rotation_matrix(float pitch, float yaw, float roll)
{
    float ch = cos(yaw);
    float sh = sin(yaw);
    float ca = cos(roll);
    float sa = sin(roll);
    float cb = cos(pitch);
    float sb = sin(pitch);
    return mat4(
        ch*ca, sa, -sh*ca, 0.0,
        sh*sb - ch*sa*cb, ca*cb, sh*sa*cb + ch*sb, 0.0,
        ch*sa*sb + sh*cb, -ca*sb, -sh*sa*sb + ch*cb, 0.0,
        0.0, 0.0, 0.0, 1.0
    );
}

// Smooth 2D value noise for world-space weather puddles
float hash21_weather(vec2 p)
{
    p = fract(p * vec2(123.34, 456.21));
    p += dot(p, p + 45.32);
    return fract(p.x * p.y);
}

vec2 hash22_weather(vec2 p)
{
    vec3 p3 = fract(vec3(p.xyx) * vec3(0.1031, 0.1030, 0.0973));
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.xx + p3.yz) * p3.zy);
}

float value_noise_2d(vec2 p)
{
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    float a = hash21_weather(i);
    float b = hash21_weather(i + vec2(1.0, 0.0));
    float c = hash21_weather(i + vec2(0.0, 1.0));
    float d = hash21_weather(i + vec2(1.0, 1.0));
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

// Procedural concentric raindrop ripples in world XZ (2 staggered grid layers)
vec2 compute_rain_ripple_gradient(vec2 world_xz, float time)
{
    vec2 grad = vec2(0.0);
    for (int layer = 0; layer < 2; ++layer)
    {
        float scale = (layer == 0) ? 1.85 : 2.95;
        vec2 uv = world_xz * scale + vec2(float(layer) * 17.3, float(layer) * 9.7);
        vec2 cell = floor(uv);
        vec2 local = fract(uv);
        vec2 rnd = hash22_weather(cell + float(layer) * 31.7);
        float cycle = fract(time * 0.85 + rnd.x * 1.7 + rnd.y * 0.9);
        vec2 drop_center = 0.2 + 0.6 * rnd;
        vec2 delta = local - drop_center;
        float dist = length(delta);
        float ring_radius = cycle * 0.48;
        float ring_delta = dist - ring_radius;
        float envelope = exp(-ring_delta * ring_delta * 110.0) * (1.0 - cycle) * smoothstep(0.0, 0.08, cycle);
        float wave = sin(ring_delta * 42.0) * envelope;
        grad += (delta / max(dist, 0.005)) * wave;
    }
    return grad;
}

// Shared weather wetness & procedural puddle evaluation for both shading and SSR
void compute_surface_wetness_and_puddle(
    vec3 world_position,
    float sea_height,
    vec3 vertex_normal,
    float sky_visibility,
    uint shading_model,
    float wetness,
    float rain_intensity,
    float time,
    float linear_depth,
    out float out_surface_wetness,
    out float out_puddle_mask,
    out vec2 out_ripple_grad
)
{
    out_surface_wetness = 0.0;
    out_puddle_mask = 0.0;
    out_ripple_grad = vec2(0.0);

    if (wetness <= 0.001)
    {
        return;
    }

    float above_sea = smoothstep(sea_height + 0.15, sea_height + 0.85, world_position.y);
    float sky_exposed = smoothstep(0.10, 0.65, sky_visibility);
    float up_slope = smoothstep(-0.15, 0.65, vertex_normal.y);
    out_surface_wetness = wetness * above_sea * sky_exposed * mix(0.35, 1.0, up_slope);

    // Puddles form only on flat upward-facing non-character, non-foliage surfaces
    if (shading_model == ShadingModel_Default && out_surface_wetness > 0.12)
    {
        float flat_ground = smoothstep(0.86, 0.97, vertex_normal.y);
        if (flat_ground > 0.001)
        {
            float n0 = value_noise_2d(world_position.xz * 0.16);
            float n1 = value_noise_2d(world_position.xz * 0.47 + vec2(11.3, 5.7));
            float puddle_noise = n0 * 0.68 + n1 * 0.32;
            float threshold = mix(0.84, 0.46, out_surface_wetness);
            out_puddle_mask = smoothstep(threshold, threshold + 0.14, puddle_noise) * flat_ground * smoothstep(0.15, 0.60, out_surface_wetness);

            if (rain_intensity > 0.01 && out_puddle_mask > 0.05 && linear_depth < 38.0)
            {
                float dist_fade = smoothstep(38.0, 10.0, linear_depth);
                out_ripple_grad = compute_rain_ripple_gradient(world_position.xz, time) * (rain_intensity * out_puddle_mask * dist_fade * 0.16);
            }
        }
    }
}
#endif // _UTILITY_
