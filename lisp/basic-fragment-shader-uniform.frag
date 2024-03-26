#version 330 core

in vec4 ourColor;

uniform vec4 uniColor;

out vec4 FragColor;

void main()
{
    FragColor = uniColor;
}
