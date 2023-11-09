#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float     u_time;
uniform sampler2D fonts_ext;

void main()
{
	vec2 uv = fragCoord;
	vec4 font_clr = texture(fonts_ext, vec2(uv.x, 1.0f-uv.y));
	fragColor = font_clr;
}
