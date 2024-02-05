#version 430 core
layout (location = 0) in vec4 aPos;   // the position variable has attribute position 0
layout (location = 1) in vec3 aColor; // the color variable has attribute position 1
layout (location = 2) in vec2 texCoords;
  
out vec3 vtx_Color;
out vec2 tex_Coords; // output a color to the fragment shader

uniform mat4 u_MVP;

void main()
{
    gl_Position = aPos * u_MVP;
    vtx_Color = aColor;
    tex_Coords = texCoords;
}   
