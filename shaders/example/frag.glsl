#version 330 core
out vec4 FragColor;
  
in vec3 vtx_Color;
in vec2 tex_Coords;
uniform sampler2D tex;

void main()
{
    FragColor = vec4(texture(tex, tex_Coords).rgb, 1.0) * vec4(vtx_Color, 1.0);
}  
