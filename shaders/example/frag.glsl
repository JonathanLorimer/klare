#version 330 core
out vec4 FragColor;
  
in vec3 vtx_Color;

void main()
{
    FragColor = vec4(vtx_Color, 1.0);
}  
