#include "../common/scene_constants.glsl"

#define INSTANCE_ID_LOCATION 3

const uint UI_RENDER_FLAG_NONE = 0;
const uint UI_RENDER_FLAG_RENDER_TEXT = 1 << 0;
const uint UI_RENDER_FLAG_RENDER_TEXTURE = 1 << 1;
const uint UI_RENDER_FLAG_TOUCHED_OVER = 1 << 2;
const uint UI_RENDER_FLAG_TOUCHED = 1 << 3;
const uint UI_RENDER_FLAG_ENABLE_RENDERABLE_AREA = 1 << 4;
const uint UI_RENDER_FLAG_CLAMP_TEXTURE = 1 << 5;
const uint UI_RENDER_FLAG_ENABLE_NINE_SLICE = 1 << 6;

struct VERTEX_OUTPUT
{
    vec4 _color;
    vec4 _border_color;
    vec2 _texcoord;
};

struct UIRenderData {
    vec4 _ui_texcoord;
    vec4 _ui_render_area;
    vec4 _ui_renderable_area;
    vec4 _ui_padding;
    float _ui_renderable_area_round;
    float _ui_renderable_area_border;
    float _ui_round;
    float _ui_border;
    uint _ui_color;
    uint _ui_touched_over_color;
    uint _ui_touched_color;
    uint _ui_border_color;
    uint _ui_touched_over_border_color;
    uint _ui_touched_border_color;
    uint _ui_render_flags;
    float _ui_opacity;
    float _ui_rotation;
    float _ui_dpi_scale;
    uint _reserved1;
    uint _reserved2;
};

layout(binding = 0) uniform texture2D texture_font;
layout(binding = 1) uniform texture2D texture_color;
layout(binding = 2) buffer UIRenderDataBuffer
{
    UIRenderData ui_render_data_list[MAX_UI_INSTANCE_COUNT];
};
layout(binding = 3) uniform sampler g_samplers[GLOBAL_SAMPLER_COUNT];

layout( push_constant ) uniform PushConstant_RenderUI
{
    vec2 _inv_canvas_size;
    vec2 _uv_size;
    vec2 _uv_offset;
    uint _instance_id_offset;
    uint _reserved0;
} pushConstant;
