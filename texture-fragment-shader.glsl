#version 150

in vec2 out_tex_coord;
in vec3 out_color;

out vec4 color_output;

/* We are using the default texture unit since we only have one texture */
uniform sampler2D tex;
void main() {
    color_output = Texture(out_tex_coord, tex) * vec4(input_color,1.0)
}
