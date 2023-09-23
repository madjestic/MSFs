#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vRGBA;
layout(location = 2) in vec2 uvCoords;
uniform float u_time;

// Output data ; will be interpolated for each fragment.
out vec4 rgba;
out vec2 fragCoord;
//out float u_time;

mat4 rotateY(float angle) {
	float angleS = 0.01f * angle;
    float c = cos(angleS);
	float s = sin(angleS);

    return mat4(
        c, 0, -s, 0,
        0, 1,  0, 0,
        s, 0,  c, 0,
		0, 0,  0, 1
    );
}

mat4 xform =
	mat4(
		vec4(1,0,0,0),
		vec4(0,1,0,0),
		vec4(0,0,1,0),
		vec4(0,0,0,1)); 

void main()
{
   gl_Position = rotateY(u_time) * vec4(vPosition, 1.0f);
   //float x = length(gl_Position.xyz);
   //gl_Position.z = mix (f1(x, s1), f2(x, s2), mixF(x, far));
   

// The color of each vertex will be interpolated
// to produce the color of each fragment
   rgba      = vRGBA;
   fragCoord = uvCoords;
}
