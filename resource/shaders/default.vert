#version 450

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec3 tangent;
layout(location = 3) in uint color;
layout(location = 4) in vec2 tex_coord;

layout(location = 0) out vec3 v_color;

void main() {
    gl_Position = vec4(position, 1.0);
    v_color = normal;
}