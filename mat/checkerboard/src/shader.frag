#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D checkerboard;

void main()
{
  vec2 uv       = fragCoord;
  vec4 font_clr = texture(checkerboard, vec2(uv.x, uv.y));
  fragColor     = font_clr;
}
