#version 150
in vec3 color_output;
out vec4 final_color;

void main() {
    final_color = vec4(color_output, 1.0);
}
