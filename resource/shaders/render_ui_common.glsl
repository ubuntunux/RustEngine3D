// MAX_UI_INSTANCE_COUNT must match with font.rs
const uint MAX_UI_INSTANCE_COUNT = 1024;

struct VERTEX_OUTPUT
{
    vec2 texcoord;
};

struct UIInstanceData {
    vec4 _ui_texcoord;
    vec2 _ui_pos;
    vec2 _ui_size;
    uint _ui_color;
    uint _reserved0;
    uint _reserved1;
    uint _reserved2;
};

layout(binding = 0) uniform sampler2D texture_normal;
layout(binding = 1) buffer UIInstanceDataBuffer
{
    UIInstanceData ui_instance_data[MAX_UI_INSTANCE_COUNT];
};

layout( push_constant ) uniform PushConstant_RenderUI
{
    vec2 _inv_canvas_size;
    uint _reserved0;
    uint _reserved1;
} pushConstant;