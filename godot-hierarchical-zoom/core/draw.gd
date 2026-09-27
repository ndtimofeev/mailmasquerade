class_name Draw
extends RefCounted
## Небольшие помощники для процедурной геометрии: линии, круги, материалы.


static func unlit(color: Color, additive := false) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.shading_mode = BaseMaterial3D.SHADING_MODE_UNSHADED
	m.albedo_color = color
	if color.a < 1.0 or additive:
		m.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA
	if additive:
		m.blend_mode = BaseMaterial3D.BLEND_MODE_ADD
	return m


## Светящийся материал: чёрный альбедо + эмиссия выше 1.0, чтобы сработал glow.
static func glowing(color: Color, energy := 3.0) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.albedo_color = Color.BLACK
	m.emission_enabled = true
	m.emission = color
	m.emission_energy_multiplier = energy
	return m


static func lit(color: Color, roughness := 0.9) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.albedo_color = color
	m.roughness = roughness
	return m


static func circle_points(radius: float, segments := 128) -> PackedVector3Array:
	var pts := PackedVector3Array()
	for i in segments + 1:
		var a := TAU * i / segments
		pts.append(Vector3(cos(a) * radius, 0.0, sin(a) * radius))
	return pts


## Меш из линий. strip=true — ломаная, иначе пары точек (отрезки).
static func lines(points: PackedVector3Array, color: Color, strip := false, additive := false) -> MeshInstance3D:
	var arrays := []
	arrays.resize(Mesh.ARRAY_MAX)
	arrays[Mesh.ARRAY_VERTEX] = points
	var mesh := ArrayMesh.new()
	var prim := Mesh.PRIMITIVE_LINE_STRIP if strip else Mesh.PRIMITIVE_LINES
	mesh.add_surface_from_arrays(prim, arrays)
	var mi := MeshInstance3D.new()
	mi.mesh = mesh
	mi.material_override = unlit(color, additive)
	mi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	return mi


static func circle(radius: float, color: Color, segments := 128) -> MeshInstance3D:
	return lines(circle_points(radius, segments), color, true)


## Облако точек фиксированного экранного размера (в пикселях).
static func points(positions: PackedVector3Array, colors: PackedColorArray, size_px: float) -> MeshInstance3D:
	var arrays := []
	arrays.resize(Mesh.ARRAY_MAX)
	arrays[Mesh.ARRAY_VERTEX] = positions
	arrays[Mesh.ARRAY_COLOR] = colors
	var mesh := ArrayMesh.new()
	mesh.add_surface_from_arrays(Mesh.PRIMITIVE_POINTS, arrays)
	var m := StandardMaterial3D.new()
	m.shading_mode = BaseMaterial3D.SHADING_MODE_UNSHADED
	m.vertex_color_use_as_albedo = true
	m.use_point_size = true
	m.point_size = size_px
	m.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA
	m.blend_mode = BaseMaterial3D.BLEND_MODE_ADD
	var mi := MeshInstance3D.new()
	mi.mesh = mesh
	mi.material_override = m
	mi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	return mi


static func label(text: String, color: Color, font_px := 15) -> Label3D:
	var l := Label3D.new()
	l.text = text
	l.modulate = color
	l.outline_modulate = Color(0.0, 0.0, 0.0, 0.8)
	l.outline_size = 6
	l.font_size = font_px
	l.fixed_size = true          # размер в пикселях не зависит от высоты камеры
	l.pixel_size = 0.0009
	l.billboard = BaseMaterial3D.BILLBOARD_ENABLED
	l.no_depth_test = true
	l.double_sided = true
	return l
