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

// depth(0.0 ~ 1.0) to linear depth(near ~ far)
float device_depth_to_linear_depth(const float zNear, const float zFar, const float depth)
{
    return zNear * zFar / (zFar + depth * (zNear - zFar));
}

// vectorized version
vec4 device_depth_to_linear_depth(const vec4 zNear, const vec4 zFar, const vec4 depth)
{
    return zNear * zFar / (zFar + depth * (zNear - zFar));
}

// linear depth(near ~ far) to device depth(0.0 ~ 1.0)
float linear_depth_to_device_depth(const float zNear, const float zFar, const float linear_depth)
{
    return saturate((zFar - (zNear * zFar / linear_depth)) / (zFar - zNear));
}

// vectorized version
vec4 linear_depth_to_device_depth(const vec4 zNear, const vec4 zFar, const vec4 linear_depth)
{
    return saturate((zFar - (zNear * zFar / linear_depth)) / (zFar - zNear));
}

vec4 relative_world_from_device_depth(const in mat4x4 inv_view_origin_projection, const in vec2 tex_coord, const float depth)
{
    vec4 clip_coord = vec4(tex_coord * 2.0 - 1.0, depth, 1.0);
    vec4 relative_pos = inv_view_origin_projection * clip_coord;
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

float distance_field_font_opacity(float opacity)
{
    const float threshold = 0.2;
    return smoothstep(threshold, 1.0, exp(-saturate(1.0 - opacity) * 5.0));
}

float encode_emissive_intensity(float emissive_intensity)
{
    return saturate(emissive_intensity * 0.1);
}

float decode_emissive_intensity(float emissive_intensity)
{
    return emissive_intensity * 10.0;
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
#endif // _UTILITY_
