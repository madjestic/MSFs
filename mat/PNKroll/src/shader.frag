#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	//fragColor = vec4( 1.0f, 1.0f, 0.0f, A );
}
