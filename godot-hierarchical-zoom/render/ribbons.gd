class_name Ribbons
extends RefCounted
## Сборщик меша для ribbon.gdshader: линии постоянной толщины в пикселях.

const TRASSA := 0.0
const TRUNK := 1.0
const BORDER := 2.0

static var SHADER: Shader = preload("res://shaders/ribbon.gdshader")

var _v := PackedVector3Array()
var _c0 := PackedFloat32Array()
var _c1 := PackedFloat32Array()
var _col := PackedColorArray()
var _idx := PackedInt32Array()


## Отрезок p1–p2 (плоскость y = 0), t1/t2 — доля пути вдоль всей линии.
func segment(p1: Vector2, p2: Vector2, t1: float, t2: float, len_g: float,
		kind: float, rnd: float, extra: float, color: Color) -> void:
	var d := (p2 - p1).normalized()
	var base := _v.size()
	for e in [[p1, t1], [p2, t2]]:
		for side in [-1.0, 1.0]:
			_v.append(Vector3(e[0].x, 0.0, e[0].y))
			_c0.append_array([d.x, d.y, side, e[1]])
			_c1.append_array([len_g, kind, rnd, extra])
			_col.append(color)
	_idx.append_array([base, base + 1, base + 2, base + 1, base + 3, base + 2])


## Прямая линия.
func line(a: Vector2, b: Vector2, kind: float, rnd: float, extra: float, color: Color) -> void:
	segment(a, b, 0.0, 1.0, a.distance_to(b), kind, rnd, extra, color)


## Дуга (квадратичная кривая Безье): bulge — прогиб в долях длины.
func arc(a: Vector2, b: Vector2, bulge: float, segs: int, kind: float, rnd: float, extra: float, color: Color) -> void:
	var ctrl := (a + b) * 0.5 + (b - a).orthogonal() * bulge
	var pts: Array[Vector2] = []
	for i in segs + 1:
		var t := float(i) / segs
		pts.append(a.lerp(ctrl, t).lerp(ctrl.lerp(b, t), t))
	var total := 0.0
	for i in segs:
		total += pts[i].distance_to(pts[i + 1])
	var acc := 0.0
	for i in segs:
		var l := pts[i].distance_to(pts[i + 1])
		segment(pts[i], pts[i + 1], acc / total, (acc + l) / total, total, kind, rnd, extra, color)
		acc += l


func build(mat: ShaderMaterial) -> MeshInstance3D:
	var arrays := []
	arrays.resize(Mesh.ARRAY_MAX)
	arrays[Mesh.ARRAY_VERTEX] = _v
	arrays[Mesh.ARRAY_CUSTOM0] = _c0
	arrays[Mesh.ARRAY_CUSTOM1] = _c1
	arrays[Mesh.ARRAY_COLOR] = _col
	arrays[Mesh.ARRAY_INDEX] = _idx
	var flags := (Mesh.ARRAY_CUSTOM_RGBA_FLOAT << Mesh.ARRAY_FORMAT_CUSTOM0_SHIFT) \
		| (Mesh.ARRAY_CUSTOM_RGBA_FLOAT << Mesh.ARRAY_FORMAT_CUSTOM1_SHIFT)
	var mesh := ArrayMesh.new()
	mesh.add_surface_from_arrays(Mesh.PRIMITIVE_TRIANGLES, arrays, [], {}, flags)
	return Draw.mesh_instance(mesh, mat)


static func material() -> ShaderMaterial:
	var m := ShaderMaterial.new()
	m.shader = SHADER
	return m
