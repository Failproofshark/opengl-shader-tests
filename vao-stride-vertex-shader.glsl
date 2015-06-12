#version 150

in vec2 position;
in vec3 input_color;

out vec3 color_output;

void main() {
    color_output = input_color;
    gl_Position = vec4(position, 0.0 , 1.0);
}
