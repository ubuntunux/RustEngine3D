// MAX_UI_INSTANCE_COUNT must match with font.rs
const uint MAX_UI_INSTANCE_COUNT = 1024;

layout( push_constant ) uniform PushConstant_RenderUI
{
    vec2 _ui_pos;
    vec2 _ui_size;
    vec2 _inv_canvas_size;
    uint _reserved0;
    uint _reserved1;
} pushConstant;

layout(binding = 0) uniform sampler2D texture_normal;
layout(binding = 1) buffer UIInstanceData
{
    vec4 ui_instance_infos[MAX_UI_INSTANCE_COUNT];
};

struct VERTEX_OUTPUT
{
    vec2 texcoord;
};