#version 330 core

in vec4 ourColor;

out vec4 FragColor;

void main()
{
    FragColor = ourColor;
    //FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
