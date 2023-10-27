#version 430 core

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec4 vRGBA;
layout(location = 2) in vec2 uvCoords;

uniform float u_time;
uniform mat4 camera;
uniform mat4 persp;
//uniform mat4 xform;

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

	// mat3 viewRot =
	// 	mat3( camera[0].xyz
	// 		, camera[1].xyz
	// 		, camera[2].xyz );

	// mat4 cameraRot =
	// 	mat4 ( camera[0]
	// 		 , camera[1]
	// 		 , camera[2]
	// 		 , vec4(0,0,0,1));

	mat4 cameraRot =
		mat4 ( inverse(camera)[0] 
			 , inverse(camera)[1] 
			 , inverse(camera)[2] 
			 , camera[3]);

	// mat3 perspRot =
	// 	mat3 ( persp[0].xyz
	// 		 , persp[1].xyz
	// 		 , persp[2].xyz );

	// mat3 xformRot =
	// 	mat3 ( xform[0].xyz
	// 		 , xform[1].xyz
	// 		 , xform[2].xyz );

	vec4 position = vec4(vPosition,1.0);	

	gl_Position
		= (persp)
		* (cameraRot)
		* position;
	
// Logarithmic correction.  The idea is to blend linear coordinates with
// logarithmic, so that near object (up to a few thousand meters appear
// in linear space and the rest is logarithmic
// TODO: move it to shared lib.glsl code	
	//float x = length(gl_Position.xyz);
	//gl_Position.z = mix (f1(x, s1), f2(x, s2), mixF(x, far));
   

// The color of each vertex will be interpolated
// to produce the color of each fragment
	rgba      = vRGBA;
	fragCoord = uvCoords;
}
