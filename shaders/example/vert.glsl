#version 430 core
layout (location = 0) in vec3 aPos;   // the position variable has attribute position 0
layout (location = 1) in vec3 aColor; // the color variable has attribute position 1
layout (location = 2) in vec2 aTex;
  
out vec3 vtx_Color;
out vec2 tex_Coords; // output a color to the fragment shader

uniform mat4 u_Model;
uniform mat4 u_View;
uniform mat4 u_Projection;

void main()
{
    // gl_Position = aPos;
    // gl_Position = u_Model * aPos;
    // gl_Position = u_View * u_Model * aPos;
    gl_Position = u_Projection * u_View * u_Model * vec4(aPos, 1.0);
    vtx_Color = aColor;
    tex_Coords = aTex;
}   
