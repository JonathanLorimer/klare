#version 430 core
out vec4 FragColor;
  
in vec3 vtx_Color;
in vec2 tex_Coords;

uniform sampler2D texture0;
uniform sampler2D texture1;

vec4 layer(vec4 foreground, vec4 background) {
    return foreground * foreground.a + background * (1.0 - foreground.a);
}

void main()
{
    FragColor = layer( texture(texture1, tex_Coords), texture(texture0, tex_Coords) * vec4(vtx_Color, 1.0) ) ;
    // FragColor = vec4(vtx_Color, 1.0);
}  
