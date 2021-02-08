layout( push_constant ) uniform PushConstant_RenderFont
{
    vec2 _offset;
    vec2 _inv_canvas_size;
    float _font_size;
    float _count_of_side;
    uint reserved0;
    uint reserved1;
} pushConstant;

layout(binding = 0) uniform sampler2D texture_font;


struct VERTEX_OUTPUT
{
    vec2 texcoord;
    vec2 font_offset;
};