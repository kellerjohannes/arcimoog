#version 330 core

in vec4 ourColor;
in vec2 TexCoord;

uniform vec4 uniColor;
uniform sampler2D ourTexture;

out vec4 FragColor;

void main()
{
    FragColor = texture(ourTexture, TexCoord) * ourColor;
}
