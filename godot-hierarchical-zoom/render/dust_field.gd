class_name DustField
extends Node3D
## Многомасштабная пыль: то, по чему виден зум в «разрывах».
##
## 16 уровней, каждый в 8 раз мельче предыдущего: от 4000 гал. ед. до ~10^-10.
## Уровень k — сетка квадратных тайлов размера T; в тайле ~40 точек на глубине
## 0.3T…1.6T под плоскостью. Тайлы генерируются вокруг фокуса детерминированно
## (сид из номера уровня и тайла), поэтому пыль «бесконечна» и одинакова при возврате.
## Уровень виден, пока высота камеры h сопоставима с его глубиной: слишком мелкий
## выглядел бы россыпью на карте, слишком крупный — неподвижным небом.

const TOP := 4000.0
const RATIO := 8.0
const LEVELS := 16
const PER_TILE := 40
const REACH := 2
const TINTS := [Color(0.62, 0.72, 0.95), Color(0.55, 0.8, 0.85), Color(0.9, 0.78, 0.6), Color(0.75, 0.65, 0.95)]

var _tiles := {}                  # "k:ix:iz" -> [MeshInstance3D, k, ix, iz]
var _mats: Array[StandardMaterial3D] = []


func _init() -> void:
	for k in LEVELS:
		var m := Draw.unlit(Color(TINTS[k % TINTS.size()], 1.0), true)
		m.vertex_color_use_as_albedo = true
		m.use_point_size = true
		m.point_size = 1.0 if k % 2 == 0 else 1.5
		_mats.append(m)


func update(view: View) -> void:
	var h := view.z() * View.H                  # высота камеры в гал. единицах
	var want := {}
	for k in LEVELS:
		var t := TOP / pow(RATIO, k)
		var r := t / h
		var a := Space.ramp(r, 0.4, 1.3) * (1.0 - Space.ramp(r, 12.0, 45.0))
		_mats[k].albedo_color.a = a
		if a <= 0.01:
			continue
		var ix0 := floori((view.ax + view.dx) / t)
		var iz0 := floori((view.az + view.dz) / t)
		for ix in range(ix0 - REACH, ix0 + REACH + 1):
			for iz in range(iz0 - REACH, iz0 + REACH + 1):
				var key := "%d:%d:%d" % [k, ix, iz]
				want[key] = true
				if not _tiles.has(key):
					_tiles[key] = [_make_tile(key, k), k, ix, iz]
					add_child(_tiles[key][0])
				_place(view, _tiles[key], t)
	for key in _tiles.keys():
		if not want.has(key):
			_tiles[key][0].queue_free()
			_tiles.erase(key)


func _make_tile(key: String, k: int) -> MeshInstance3D:
	var rng := RandomNumberGenerator.new()
	rng.seed = hash(key)
	var p := PackedVector3Array()
	var col := PackedColorArray()
	for i in PER_TILE:
		p.append(Vector3(rng.randf(), -lerpf(0.3, 1.6, rng.randf()), rng.randf()))
		col.append(Color(1, 1, 1, rng.randf_range(0.15, 0.6)))
	var mi := Draw.mesh_instance(Draw.arrays_mesh(Mesh.PRIMITIVE_POINTS, p, col), _mats[k])
	mi.top_level = true
	return mi


func _place(view: View, tile: Array, t: float) -> void:
	var s := t / view.z()
	var r := view.rel(tile[2] * t, tile[3] * t)
	var tr := Transform3D(Basis.IDENTITY.scaled(Vector3(s, s, s)), Vector3(r.x, 0.0, r.y))
	tile[0].transform = Space.squeeze(1.6 * s) * tr
