class_name Space
extends Node3D
## «Пространство» со своими единицами длины: галактика, система, бой.
## Пространства вложены друг в друга: у каждого есть начало координат (ox, oz)
## в галактических единицах (double) и масштаб unit — сколько галактических
## единиц в одной его локальной единице. Содержимое строится в локальных
## единицах (числа порядка сотен), и float32 его не портит.

const DEPTH_MAX := 1.0e6   # глубже этого фоновые слои поджимаются к камере

var ox := 0.0
var oz := 0.0
var unit := 1.0
var angle := 0.0           # поворот вокруг вертикали

var _fades := {}           # группа -> [[material, base_alpha], ...]
var _deep: Array = []      # [[node, max_depth_local], ...]


## Главная функция: пересчитать transform так, будто камера смотрит из View.
## Вычисления в double, в Transform3D попадает уже результат относительно камеры.
func place(view: View) -> void:
	var z := view.z()
	var s := unit / z
	var basis := Basis(Vector3.UP, angle).scaled(Vector3(s, s, s))
	transform = Transform3D(basis, Vector3((ox - view.fx) / z, 0.0, (oz - view.fz) / z))

	# Глубокие фоновые слои при сильном зуме уходят на миллионы единиц вглубь.
	# Масштабирование относительно точки камеры не меняет проекцию ни одной точки,
	# поэтому слой можно «поджать» ближе, чтобы он не вылез за дальнюю плоскость.
	var cam := Vector3(0.0, View.H, 0.0)
	for d in _deep:
		var deepest: float = d[1] * s
		var k := minf(1.0, DEPTH_MAX / (View.H + deepest))
		d[0].transform = Transform3D(Basis.IDENTITY.scaled(Vector3(k, k, k)), cam * (1.0 - k)) * transform


## Локальная точка → галактические координаты [gx, gz] (double).
func to_galaxy(lx: float, lz: float) -> Array:
	var c := cos(angle)
	var sn := sin(angle)
	# Basis(UP, angle) поворачивает x→(cos, 0, -sin), z→(sin, 0, cos)
	return [ox + unit * (lx * c + lz * sn), oz + unit * (-lx * sn + lz * c)]


## Зарегистрировать материал в группе затухания.
func fade(group: StringName, mat: BaseMaterial3D) -> BaseMaterial3D:
	if not _fades.has(group):
		_fades[group] = []
	_fades[group].append([mat, mat.albedo_color.a])
	return mat


func set_alpha(group: StringName, a: float) -> void:
	for f in _fades.get(group, []):
		f[0].albedo_color.a = f[1] * a


## Фоновый слой: узел выносится из иерархии transform (top_level) и ставится вручную в place().
func add_deep(node: Node3D, max_depth_local: float) -> void:
	add_child(node)
	node.top_level = true
	_deep.append([node, max_depth_local])


## Слой настоящих 3D-точек ПОД плоскостью: при перспективе они дают параллакс.
## depth — диапазон глубин (локальные единицы), spread(d) — полуширина слоя на глубине d.
static func dust(rng: RandomNumberGenerator, count: int, depth_from: float, depth_to: float,
		spread0: float, spread_k: float, tint: Color, size_px: float, brightness: float) -> MeshInstance3D:
	var pos := PackedVector3Array()
	var col := PackedColorArray()
	for i in count:
		# больше точек на глубине: равномерно по объёму конуса видимости
		var d := lerpf(depth_from, depth_to, sqrt(rng.randf()))
		var ext := spread0 + d * spread_k
		pos.append(Vector3(rng.randf_range(-ext, ext), -d, rng.randf_range(-ext, ext)))
		var c := tint.lerp(Color(1.0, 0.9, 0.8), rng.randf() * 0.4)
		# дальние тусклее — ощущение глубины
		c.a = brightness * rng.randf_range(0.25, 1.0) * lerpf(1.0, 0.35, (d - depth_from) / (depth_to - depth_from))
		col.append(c)
	return Draw.points(pos, col, size_px)


static func ramp(x: float, a: float, b: float) -> float:
	return smoothstep(a, b, x)
