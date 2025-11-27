#include "../common/scene_constants.glsl"

#define INSTANCE_ID_LOCATION 3

const uint UI_RENDER_FLAG_NONE = 0;
const uint UI_RENDER_FLAG_RENDER_TEXT = 1 << 0;
const uint UI_RENDER_FLAG_RENDER_TEXTURE = 1 << 1;
const uint UI_RENDER_FLAG_TOUCHED = 1 << 2;
const uint UI_RENDER_FLAG_ENABLE_RENDERABLE_AREA = 1 << 3;

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
    float _ui_renderable_area_round;
    float _ui_renderable_area_border;
    uint _ui_color;
    float _ui_round;
    float _ui_border;
    uint _ui_border_color;
    uint _ui_render_flags;
    float _ui_opacity;
};

layout(binding = 0) uniform sampler2D texture_font;
layout(binding = 1) uniform sampler2D texture_color;
layout(binding = 2) buffer UIRenderDataBuffer
{
    UIRenderData ui_render_data_list[MAX_UI_INSTANCE_COUNT];
};

layout( push_constant ) uniform PushConstant_RenderUI
{
    vec2 _inv_canvas_size;
    vec2 _uv_size;
    vec2 _uv_offset;
    uint _instance_id_offset;
    uint _reserved0;
} pushConstant;