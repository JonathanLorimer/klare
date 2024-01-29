#version 430 core
out vec4 FragColor;
  
in vec3 vtx_Color;
in vec2 tex_Coords;

uniform sampler2D texture1;
uniform sampler2D texture2;

void main()
{
    FragColor = vec4( mix( texture(texture1, tex_Coords).rgb
                         , texture(texture2, tex_Coords).rgb, 0.5 
                         ) * vtx_Color 
                    , 1.0 
                    );
}  
