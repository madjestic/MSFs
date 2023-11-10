#version 450

in  vec4 rgba;
in  vec2 fragCoord;
out vec4 fragColor;

uniform float u_time;
uniform sampler2D fonts;

void main()
{
  vec2 uv       = fragCoord;
  vec4 tx       = texture(fonts, vec2(uv.x, uv.y));
  vec4 font_clr = vec4(tx.r, tx.g, tx.b, tx.r);
  fragColor     = font_clr;
}
