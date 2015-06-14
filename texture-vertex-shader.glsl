#version 150
in vec2 position;
in vec2 tex_coord;
in vec3 input_color;

out vec3 out_color;
out vec2 out_tex_coord;

void main()
{
    out_tex_coord = tex_coord;
    out_color = input_color;
    gl_Position = vec4(position, 0.0, 1.0);
}
