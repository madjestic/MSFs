#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vRGBA;
layout(location = 2) in vec2 uvCoords;
uniform float fTime;

// Output data ; will be interpolated for each fragment.
out vec4 rgba;
out vec2 fragCoord;
out float time;

void main()
{
   gl_Position = vec4(vPosition, 1.0f);

// The color of each vertex will be interpolated
// to produce the color of each fragment
   rgba      = vRGBA;
   fragCoord = uvCoords;
   time      = fTime;
}
