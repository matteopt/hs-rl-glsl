#version 460 core

uniform vec2 resolution;
uniform vec2 mouse;
uniform float time;

out vec4 outputColor;

void main() {
    float z = 1.;
    float x = mod(time, z) - (z / 2.);
    vec2 st = gl_FragCoord.xy/resolution.xy;
    vec3 color = vec3(st.x,st.y,abs(sin(x)));
    outputColor = vec4(color,1.0);
}
