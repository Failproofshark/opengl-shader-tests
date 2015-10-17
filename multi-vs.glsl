#version 150

in vec2 position;
in vec3 color;

out vec3 output_color;

uniform mat4 model_view_projection_matrix;

void main()
{
    gl_Position = model_view_projection_matrix * vec4(position.x, position.y, 0.0, 1.0);
    output_color = color;
}
