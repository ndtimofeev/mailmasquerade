class_name Space
extends Node3D
## Пространство со своими единицами: галактика, система, планета, бой.
## Положение задаётся базой (bx, bz — точные координаты звезды) и смещением (ox, oz),
## масштаб — unit (галактических единиц в одной локальной). Содержимое строится
## в локальных единицах, числа небольшие, float32 их не портит.

const DEPTH_MAX := 1.0e6

var bx := 0.0
var bz := 0.0
var ox := 0.0
var oz := 0.0
var unit := 1.0
var angle := 0.0

var _fades := {}
var _deep: Array = []


## Поставить пространство относительно неподвижной камеры. Всё в double,
## в Transform3D попадает уже маленький результат.
func place(view: View) -> void:
	var s := unit / view.z()
	var r := view.rel(bx, bz, ox, oz)
	transform = Transform3D(Basis(Vector3.UP, angle).scaled(Vector3(s, s, s)), Vector3(r.x, 0.0, r.y))
	# Фоновые слои «поджимаем» к камере: масштабирование относительно точки камеры
	# не меняет проекцию, но не даёт слою уйти за дальнюю плоскость.
	for d in _deep:
		d[0].transform = Space.squeeze(d[1] * s) * transform


static func squeeze(deepest: float) -> Transform3D:
	var k := minf(1.0, DEPTH_MAX / (View.H + deepest))
	return Transform3D(Basis.IDENTITY.scaled(Vector3(k, k, k)), Vector3(0.0, View.H, 0.0) * (1.0 - k))


## Локальная точка → [bx, bz, ox, oz].
func pos(lx: float, lz: float) -> Array:
	var c := cos(angle)
	var sn := sin(angle)
	return [bx, bz, ox + unit * (lx * c + lz * sn), oz + unit * (-lx * sn + lz * c)]


func screen(view: View, lx := 0.0, lz := 0.0) -> Vector2:
	var p := pos(lx, lz)
	return view.screen(p[0], p[1], p[2], p[3])


func fade(group: StringName, mat: Material) -> Material:
	if not _fades.has(group):
		_fades[group] = []
	var base := 1.0
	if mat is BaseMaterial3D:
		base = (mat as BaseMaterial3D).albedo_color.a
	_fades[group].append([mat, base])
	return mat


func set_alpha(group: StringName, a: float) -> void:
	for f in _fades.get(group, []):
		if f[0] is BaseMaterial3D:
			f[0].albedo_color.a = f[1] * a
		else:
			f[0].set_shader_parameter(&"alpha", f[1] * a)


func add_deep(node: Node3D, max_depth_local: float) -> void:
	add_child(node)
	node.top_level = true
	_deep.append([node, max_depth_local])


## Слой настоящих 3D-точек под плоскостью (параллакс).
static func dust(rng: RandomNumberGenerator, count: int, depth_from: float, depth_to: float,
		spread0: float, spread_k: float, tint: Color, size_px: float, brightness: float) -> MeshInstance3D:
	var p := PackedVector3Array()
	var col := PackedColorArray()
	for i in count:
		var d := lerpf(depth_from, depth_to, sqrt(rng.randf()))
		var ext := spread0 + d * spread_k
		p.append(Vector3(rng.randf_range(-ext, ext), -d, rng.randf_range(-ext, ext)))
		var c := tint.lerp(Color(1.0, 0.9, 0.8), rng.randf() * 0.4)
		c.a = brightness * rng.randf_range(0.25, 1.0) * lerpf(1.0, 0.35, (d - depth_from) / (depth_to - depth_from))
		col.append(c)
	return Draw.points(p, col, size_px)


static func ramp(x: float, a: float, b: float) -> float:
	return smoothstep(a, b, x)
