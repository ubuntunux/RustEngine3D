#include "scene_constants.glsl"
#include "utility.glsl"
#include "quad.glsl"

uniform float focus_near;
uniform float focus_far;
uniform float dof_blur;
uniform vec2 blur_scale;
uniform sampler2D texture_diffuse;
uniform sampler2D texture_focus_distance;
uniform sampler2D texture_linear_depth;

const vec2 gaussFilter[7] =
{
	vec2(-3.0,	0.015625),
	vec2(-2.0,	0.09375),
	vec2(-1.0,	0.234375),
	vec2(0.0,	0.3125),
	vec2(1.0,	0.234375),
	vec2(2.0,	0.09375),
	vec2(3.0,	0.015625)
};

const float gaussFilter7x7[49] =
{
    0.00000067, 0.00002292,	0.00019117,	0.00038771,	0.00019117,	0.00002292,	0.00000067,
    0.00002292,	0.00078634,	0.00655965,	0.01330373,	0.00655965,	0.00078633,	0.00002292,
    0.00019117,	0.00655965,	0.05472157,	0.11098164,	0.05472157,	0.00655965,	0.00019117,
    0.00038771,	0.01330373,	0.11098164,	0.22508352,	0.11098164,	0.01330373,	0.00038771,
    0.00019117,	0.00655965,	0.05472157,	0.11098164,	0.05472157,	0.00655965,	0.00019117,
    0.00002292,	0.00078633,	0.00655965,	0.01330373,	0.00655965,	0.00078633,	0.00002292,
    0.00000067,	0.00002292,	0.00019117,	0.00038771,	0.00019117,	0.00002292,	0.00000067
};

#ifdef FRAGMENT_SHADER
layout (location = 0) in VERTEX_OUTPUT vs_output;
layout (location = 0) out vec4 fs_output;

// reference : https://www.shadertoy.com/view/lst3Df

void main() {
    vec2 tex_coord = vs_output.tex_coord.xy;
    float focus_distance = texture2D(texture_focus_distance, vec2(0.5, 0.5)).x;
    float linear_depth = texture2DLod(texture_linear_depth, tex_coord, 0.0).x;
    float blur_amount = clamp(abs(linear_depth - focus_distance - focus_near) / (focus_far - focus_near), 0.0, 1.0);
    vec2 scale = blur_scale / textureSize(texture_diffuse, 0) * blur_amount;

    fs_output = vec4(0.0, 0.0, 0.0, 1.0);

    for( int i = 0; i < 7; i++ )
	{
	    vec2 uv = vec2(tex_coord.x + gaussFilter[i].x * scale.x, tex_coord.y + gaussFilter[i].x * scale.y);
		fs_output += texture2D(texture_diffuse, uv) * gaussFilter[i].yyyy;
	}
}
#endif // FRAGMENT_SHADER