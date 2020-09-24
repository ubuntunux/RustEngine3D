#version 450

layout(location = 0) in vec3 v_color;

layout(location = 0) out vec4 f_color;
layout(location = 1) out vec3 f_normal;

void main() {
    f_color = vec4(v_color, 1.0);
    f_normal = vec3(0.0, 0.0, 1.0);
}