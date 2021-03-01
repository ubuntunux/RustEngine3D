// MAX_FONT_INSTANCE_COUNT must match with font.rs
const uint MAX_FONT_INSTANCE_COUNT = 1024;

struct FontInstanceData {
    vec2 _font_texcoord;
    float _font_column;
    float _font_row;
};

layout( push_constant ) uniform PushConstant_RenderFont
{
    vec2 _inv_canvas_size;
    vec2 _offset;
    vec2 _font_size;
    float _count_of_side;
    uint reserved0;
} pushConstant;

layout(binding = 0) uniform sampler2D texture_font;
layout(binding = 1) buffer FontInstanceDataBuffer
{
    FontInstanceData font_instance_data[MAX_FONT_INSTANCE_COUNT];
};


struct VERTEX_OUTPUT
{
    vec2 texcoord;
    vec2 font_offset;
};