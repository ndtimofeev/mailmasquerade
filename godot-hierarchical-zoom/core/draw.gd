class_name Draw
extends RefCounted
## Процедурная геометрия и материалы. Все материалы полупрозрачные,
## чтобы любой объект можно было плавно проявить или погасить при зуме.


static func unlit(color: Color, additive := false) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.shading_mode = BaseMaterial3D.SHADING_MODE_UNSHADED
	m.albedo_color = color
	m.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA
	if additive:
		m.blend_mode = BaseMaterial3D.BLEND_MODE_ADD
	return m


## Эмиссия выше 1.0 попадает под glow окружения.
static func glowing(color: Color, energy := 3.0) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.albedo_color = Color.BLACK
	m.emission_enabled = true
	m.emission = color
	m.emission_energy_multiplier = energy
	m.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA_DEPTH_PRE_PASS
	return m


static func lit(color: Color, roughness := 0.95) -> StandardMaterial3D:
	var m := StandardMaterial3D.new()
	m.albedo_color = color
	m.roughness = roughness
	m.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA_DEPTH_PRE_PASS
	return m


static func mesh_instance(mesh: Mesh, mat: Material) -> MeshInstance3D:
	var mi := MeshInstance3D.new()
	mi.mesh = mesh
	mi.material_override = mat
	mi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	return mi


static func arrays_mesh(prim: Mesh.PrimitiveType, verts: PackedVector3Array, colors := PackedColorArray()) -> ArrayMesh:
	var arrays := []
	arrays.resize(Mesh.ARRAY_MAX)
	arrays[Mesh.ARRAY_VERTEX] = verts
	if not colors.is_empty():
		arrays[Mesh.ARRAY_COLOR] = colors
	var mesh := ArrayMesh.new()
	mesh.add_surface_from_arrays(prim, arrays)
	return mesh


## Отрезки (пары точек) или ломаная (strip = true).
static func lines(pts: PackedVector3Array, mat: Material, strip := false) -> MeshInstance3D:
	var prim := Mesh.PRIMITIVE_LINE_STRIP if strip else Mesh.PRIMITIVE_LINES
	return mesh_instance(arrays_mesh(prim, pts), mat)


static func circle_points(radius: float, segments := 128) -> PackedVector3Array:
	var pts := PackedVector3Array()
	for i in segments + 1:
		var a := TAU * i / segments
		pts.append(Vector3(cos(a) * radius, 0.0, sin(a) * radius))
	return pts


## Облако точек фиксированного экранного размера; цвет вершины × albedo материала.
static func points(pos: PackedVector3Array, colors: PackedColorArray, size_px: float) -> MeshInstance3D:
	var m := unlit(Color.WHITE, true)
	m.vertex_color_use_as_albedo = true
	m.use_point_size = true
	m.point_size = size_px
	return mesh_instance(arrays_mesh(Mesh.PRIMITIVE_POINTS, pos, colors), m)


## Мягкое круглое пятно (для ореолов звёзд).
static func halo_texture() -> GradientTexture2D:
	var g := Gradient.new()
	g.offsets = PackedFloat32Array([0.0, 0.08, 0.25, 1.0])
	g.colors = PackedColorArray([Color(1, 1, 1, 1), Color(1, 1, 1, 0.9), Color(1, 1, 1, 0.12), Color(1, 1, 1, 0)])
	var t := GradientTexture2D.new()
	t.gradient = g
	t.fill = GradientTexture2D.FILL_RADIAL
	t.fill_from = Vector2(0.5, 0.5)
	t.fill_to = Vector2(1.0, 0.5)
	t.width = 128
	t.height = 128
	return t
