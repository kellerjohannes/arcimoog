#version 330 core

out vec4 color;

in vec2 TexCoords;

uniform sampler2D text;

void main()
{
    color = texture(text, TexCoords);
    //color = vec4(1.0, 1.0, 1.0, 1.0);


    //obsolete:
    //FragColor = texture(aTexture, vec2(TexCoord.x, 1.0 - TexCoord.y));
    //FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}
