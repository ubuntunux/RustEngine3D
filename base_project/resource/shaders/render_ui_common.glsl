#define INSTANCE_ID_LOCATION 3

// must match with ui.rs
const uint MAX_UI_INSTANCE_COUNT = 1024;
const uint UI_RENDER_FLAG_NONE = 0;
const uint UI_RENDER_FLAG_RENDER_TEXT = 1 << 0;
const uint UI_RENDER_FLAG_RENDER_TEXTURE = 1 << 1;
const uint UI_RENDER_FLAG_TOUCHED = 1 << 2;

struct VERTEX_OUTPUT
{
    vec4 _color;
    vec4 _border_color;
    vec2 _texcoord;
};

struct UIRenderData {
    vec4 _ui_texcoord;
    vec4 _ui_render_area;
    vec4 _ui_parent_render_area;
    uint _ui_color;
    float _ui_round;
    float _ui_border;
    uint _ui_border_color;
    uint _ui_render_flags;
    float _ui_opacity;
    uint _reserved0;
    uint _reserved1;
};

layout(binding = 0) uniform sampler2D texture_font;
layout(binding = 1) uniform sampler2D texture_normal;
layout(binding = 2) buffer UIRenderDataBuffer
{
    UIRenderData ui_render_datas[MAX_UI_INSTANCE_COUNT];
};

layout( push_constant ) uniform PushConstant_RenderUI
{
    vec2 _inv_canvas_size;
    uint _instance_id_offset;
    uint _reserved0;
} pushConstant;