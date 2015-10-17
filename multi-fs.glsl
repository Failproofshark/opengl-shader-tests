#version 150

in vec3 output_color;

out vec4 final_color;

void main()
{
    final_color = vec4(output_color, 1.0);
}
