class_name StarIcons
extends RefCounted
## Звёзды одним MultiMesh: 10 000 иконок = один draw call. Размер считает star.gdshader.

static var SHADER: Shader = preload("res://shaders/star.gdshader")

const SPECTRAL := [
	Color(0.68, 0.78, 1.0), Color(0.95, 0.96, 1.0), Color(1.0, 0.92, 0.76),
	Color(1.0, 0.76, 0.52), Color(1.0, 0.56, 0.46),
]


static func material(factions: Array) -> ShaderMaterial:
	var m := ShaderMaterial.new()
	m.shader = SHADER
	var sp := PackedVector3Array()
	for c in SPECTRAL:
		sp.append(Vector3(c.r, c.g, c.b))
	var fc := PackedVector3Array()
	for c in factions:
		fc.append(Vector3(c.r, c.g, c.b))
	m.set_shader_parameter(&"spectral", sp)
	m.set_shader_parameter(&"factions", fc)
	return m


## positions — в локальных единицах пространства; custom — по 4 числа на звезду
## (nn, величина, фракция + 1, спектральный класс).
static func build(positions: PackedVector2Array, custom: PackedFloat32Array, mat: ShaderMaterial) -> MultiMeshInstance3D:
	var mm := MultiMesh.new()
	mm.transform_format = MultiMesh.TRANSFORM_3D
	mm.use_custom_data = true
	var quad := QuadMesh.new()
	quad.size = Vector2.ONE
	mm.mesh = quad
	mm.instance_count = positions.size()
	var buf := PackedFloat32Array()
	buf.resize(positions.size() * 16)
	for i in positions.size():
		var o := i * 16
		# transform 3x4 построчно: единичный базис + позиция; затем 4 числа custom
		buf[o] = 1.0
		buf[o + 3] = positions[i].x
		buf[o + 5] = 1.0
		buf[o + 7] = 0.02
		buf[o + 10] = 1.0
		buf[o + 11] = positions[i].y
		for k in 4:
			buf[o + 12 + k] = custom[i * 4 + k]
	mm.buffer = buf
	var mmi := MultiMeshInstance3D.new()
	mmi.multimesh = mm
	mmi.material_override = mat
	mmi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	return mmi


## Тот же расчёт размера, что в шейдере, — для подписей и выбора мышью.
static func size_px(nn: float, mag: float, px_per_g: float, size_max: float) -> float:
	return clampf(0.9 * sqrt(nn * px_per_g), 1.0, size_max) * lerpf(0.6, 1.5, mag)
