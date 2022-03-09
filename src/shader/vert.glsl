#version 140

uniform mat4 projection;
uniform mat4 view;
uniform float animation_lerp;

in vec2 position;
in vec3 color;
in mat4 model_to_world;
in vec3 rotation_from;

out vec3 vColor;

mat4 rotationX(float angle) {
	float s = sin(angle);
	float c = cos(angle);

	return mat4(
		1., 0., 0., 0.,
		0., c, s, 0.,
		0., -s, c, 0.,
		0., 0., 0., 1.
	);
}
mat4 rotationY(float angle) {
	float s = sin(angle);
	float c = cos(angle);

	return mat4(
		c, 0., -s, 0.,
		0., 1., 0., 0.,
		s, 0., c, 0.,
		0., 0., 0., 1.
	);
}
mat4 rotationZ(float angle) {
	float s = sin(angle);
	float c = cos(angle);

	return mat4(
		c, s, 0., 0.,
		-s, c, 0., 0.,
		0., 0., 1., 0.,
		0., 0., 0., 1.
	);
}

void main() {
	vec3 rotation = rotation_from * animation_lerp;
	gl_Position = projection * view * rotationY(rotation.y) * rotationZ(rotation.z) * rotationX(rotation.x) * model_to_world * vec4(position * 0.1, 0.0, 1.0);// * matrix;
	vColor = color;
}
