class_name Galaxy
extends Space
## Звёздная карта: 10 000 звёзд, трассы, магистрали, территории. 1 гал. ед. = 0,1 св. года.

const STAR_COUNT := 10000
const RADIUS := 3000.0
const MIN_SPACING := 14.0
const GRID := 60.0                 # ячейка пространственного индекса
const CELL_R := 160.0              # предельный радиус ячейки территории
const FACTIONS := [
	Color(0.35, 0.75, 1.0), Color(1.0, 0.55, 0.3), Color(0.75, 0.5, 1.0),
	Color(0.45, 0.95, 0.6), Color(1.0, 0.85, 0.35),
]

var gx := PackedFloat64Array()
var gz := PackedFloat64Array()
var nn := PackedFloat32Array()     # расстояние до ближайшего соседа
var mag := PackedFloat32Array()    # «звёздная величина» 0..1: важность, размер иконки, приоритет подписи
var faction := PackedInt32Array()
var spec := PackedInt32Array()
var names := PackedStringArray()
var seeds := PackedInt64Array()
var by_mag := PackedInt32Array()   # индексы по убыванию важности (для подписей)

var star_mat: ShaderMaterial
var lane_mat: ShaderMaterial
var _stars_mi: MultiMeshInstance3D
var _grid := {}
var _neighbors: Array[PackedInt32Array] = []


func _init(rng_seed: int) -> void:
	var rng := RandomNumberGenerator.new()
	rng.seed = rng_seed
	_place_stars(rng)
	var tri := Geometry2D.triangulate_delaunay(_points())
	_build_graph(tri)
	_assign_magnitudes(rng)
	_assign_owners(rng)
	_name_stars(rng)

	lane_mat = Ribbons.material()
	fade(&"detail", lane_mat)
	_build_territory(tri)
	_build_lanes(rng)

	star_mat = StarIcons.material(FACTIONS)
	var custom := PackedFloat32Array()
	for i in gx.size():
		custom.append_array([nn[i], mag[i], faction[i] + 1, spec[i]])
	_stars_mi = StarIcons.build(_points(), custom, star_mat)
	add_child(_stars_mi)

	# Звёзды галактического диска далеко под плоскостью: параллакс в обзоре,
	# «неподвижное небо» при глубоком зуме.
	add_deep(Space.dust(rng, 9000, 60.0, 9000.0, RADIUS * 1.3, 0.6, Color(0.7, 0.8, 1.0), 1.0, 0.5), 9000.0)


func _points() -> PackedVector2Array:
	var p := PackedVector2Array()
	for i in gx.size():
		p.append(Vector2(gx[i], gz[i]))
	return p


# ------------------------------------------------------------ генерация

## Спиральная галактика: ядро + логарифмические рукава, минимальный зазор через сетку.
func _place_stars(rng: RandomNumberGenerator) -> void:
	var occ := {}
	var tries := 0
	while gx.size() < STAR_COUNT and tries < STAR_COUNT * 12:
		tries += 1
		var p: Vector2
		if rng.randf() < 0.22:
			p = Vector2.from_angle(rng.randf() * TAU) * absf(rng.randfn(0.0, 650.0))
		else:
			var r := RADIUS * pow(rng.randf(), 0.7)
			var arm := float(rng.randi() % 3) * TAU / 3.0
			var a := arm + 2.4 * log(r / 250.0 + 1.0) + rng.randfn(0.0, 0.22 + 60.0 / (r + 60.0))
			p = Vector2.from_angle(a) * r
		if p.length() > RADIUS * 1.05:
			continue
		var c := Vector2i(floori(p.x / MIN_SPACING), floori(p.y / MIN_SPACING))
		var ok := true
		for ddx in range(-1, 2):
			for ddz in range(-1, 2):
				var q = occ.get(c + Vector2i(ddx, ddz))
				if q != null and p.distance_to(q) < MIN_SPACING:
					ok = false
		if not ok or occ.has(c):
			continue
		occ[c] = p
		_add_to_grid(gx.size(), p)
		gx.append(p.x)
		gz.append(p.y)


func _add_to_grid(i: int, p: Vector2) -> void:
	var c := Vector2i(floori(p.x / GRID), floori(p.y / GRID))
	if not _grid.has(c):
		_grid[c] = PackedInt32Array()
	_grid[c].append(i)


## Соседи по триангуляции Делоне и расстояние до ближайшего из них.
func _build_graph(tri: PackedInt32Array) -> void:
	_neighbors.resize(gx.size())
	for i in gx.size():
		_neighbors[i] = PackedInt32Array()
	nn.resize(gx.size())
	nn.fill(1e9)
	for t in range(0, tri.size(), 3):
		for e in 3:
			var a := tri[t + e]
			var b := tri[t + (e + 1) % 3]
			if a < b:
				_neighbors[a].append(b)
				_neighbors[b].append(a)
				var d := _dist(a, b)
				nn[a] = minf(nn[a], d)
				nn[b] = minf(nn[b], d)


func _dist(a: int, b: int) -> float:
	return sqrt((gx[a] - gx[b]) ** 2 + (gz[a] - gz[b]) ** 2)


## Величина: большинство звёзд мелкие, ~4 % — «хабы», столицы — максимальные.
func _assign_magnitudes(rng: RandomNumberGenerator) -> void:
	mag.resize(gx.size())
	spec.resize(gx.size())
	for i in gx.size():
		mag[i] = pow(rng.randf(), 3.0)
		spec[i] = rng.randi() % 5


## Территории растут от столиц по графу трасс со случайными весами —
## границы получаются неровными и повторяют сеть трасс.
func _assign_owners(rng: RandomNumberGenerator) -> void:
	faction.resize(gx.size())
	faction.fill(-1)
	var buckets: Array = []
	buckets.resize(400)
	for b in buckets.size():
		buckets[b] = []
	for f in FACTIONS.size():
		var target := Vector2.from_angle(TAU * f / FACTIONS.size() + 0.4) * RADIUS * 0.55
		var cap := nearest(target.x, target.y, 400.0)
		mag[cap] = 1.0
		faction[cap] = f
		buckets[0].append([cap, f, 0.0])
	for b in buckets.size():
		for item in buckets[b]:
			var i: int = item[0]
			for j in _neighbors[i]:
				if faction[j] != -1:
					continue
				var cost: float = item[2] + 1.0 + rng.randf() * 1.4
				if cost > 34.0:
					continue
				faction[j] = item[1]
				buckets[mini(int(cost * 10.0), buckets.size() - 1)].append([j, item[1], cost])


func _name_stars(rng: RandomNumberGenerator) -> void:
	var order := range(gx.size())
	order.sort_custom(func(a, b): return mag[a] > mag[b])
	by_mag = PackedInt32Array(order)
	for i in gx.size():
		seeds.append(rng.randi())
		if mag[i] > 0.5:
			names.append(Names.star(rng))
		else:
			names.append(Names.catalog(rng))


# ------------------------------------------------------------ территории

## Ячейки Вороного через двойственность к Делоне: вершины ячеек — центры описанных
## окружностей треугольников. O(n), годится для 10 000 звёзд.
func _build_territory(tri: PackedInt32Array) -> void:
	var cc := PackedVector2Array()
	var incident: Array[PackedInt32Array] = []
	incident.resize(gx.size())
	for i in gx.size():
		incident[i] = PackedInt32Array()
	var edge_tris := {}
	for t in range(0, tri.size(), 3):
		var a := tri[t]
		var b := tri[t + 1]
		var c := tri[t + 2]
		cc.append(_circumcenter(a, b, c))
		for v in [a, b, c]:
			incident[v].append(t / 3)
		for e in [[a, b], [b, c], [c, a]]:
			var key: int = mini(e[0], e[1]) * STAR_COUNT * 2 + maxi(e[0], e[1])
			if edge_tris.has(key):
				edge_tris[key].append(t / 3)
			else:
				edge_tris[key] = [t / 3]

	# Заливка: веер треугольников вокруг каждой звезды с владельцем.
	var fill := PackedVector3Array()
	var fill_col := PackedColorArray()
	for v in gx.size():
		if faction[v] < 0:
			continue
		var center := Vector2(gx[v], gz[v])
		var ring: Array = []
		for t in incident[v]:
			ring.append(center + (cc[t] - center).limit_length(CELL_R))
		ring.sort_custom(func(p, q): return (p - center).angle() < (q - center).angle())
		var col := Color(FACTIONS[faction[v]], 0.035)
		for k in ring.size():
			var p1: Vector2 = ring[k]
			var p2: Vector2 = ring[(k + 1) % ring.size()]
			if (p1 - center).angle_to(p2 - center) < 0.0:
				continue                     # звезда на краю триангуляции: веер не замкнут
			for p in [center, p1, p2]:
				fill.append(Vector3(p.x, -0.05, p.y))
				fill_col.append(col)
	var fm := Draw.unlit(Color.WHITE, true)
	fm.vertex_color_use_as_albedo = true
	fade(&"detail", fm)
	add_child(Draw.mesh_instance(Draw.arrays_mesh(Mesh.PRIMITIVE_TRIANGLES, fill, fill_col), fm))

	# Границы: ребро Вороного между звёздами разных владельцев. Если обе стороны
	# чьи-то, рисуем две линии, каждую со сдвигом в пикселях к своей звезде.
	var borders := Ribbons.new()
	for key in edge_tris:
		var ts: Array = edge_tris[key]
		if ts.size() != 2:
			continue
		var a: int = key / (STAR_COUNT * 2)
		var b: int = key % (STAR_COUNT * 2)
		if faction[a] == faction[b]:
			continue
		var pa := Vector2(gx[a], gz[a])
		var pb := Vector2(gx[b], gz[b])
		var mid := (pa + pb) * 0.5
		var p1 := mid + (cc[ts[0]] - mid).limit_length(CELL_R)
		var p2 := mid + (cc[ts[1]] - mid).limit_length(CELL_R)
		if p1.distance_to(p2) < 0.01:
			continue
		var n := (p2 - p1).normalized().orthogonal()
		for side in [[a, pa], [b, pb]]:
			var s: int = side[0]
			if faction[s] < 0:
				continue
			var shift := 0.0
			if faction[a] >= 0 and faction[b] >= 0:
				shift = 1.2 if n.dot(side[1] - p1) > 0.0 else -1.2
			borders.line(p1, p2, Ribbons.BORDER, 0.0, shift, Color(FACTIONS[faction[s]], 0.8))
	add_child(borders.build(lane_mat))


func _circumcenter(a: int, b: int, c: int) -> Vector2:
	var ax_ := gx[a]
	var ay := gz[a]
	var bx_ := gx[b] - ax_
	var by := gz[b] - ay
	var cx := gx[c] - ax_
	var cy := gz[c] - ay
	var d := 2.0 * (bx_ * cy - by * cx)
	if absf(d) < 1e-9:
		return Vector2(ax_, ay)
	var ux := (cy * (bx_ * bx_ + by * by) - by * (cx * cx + cy * cy)) / d
	var uy := (bx_ * (cx * cx + cy * cy) - cx * (bx_ * bx_ + by * by)) / d
	return Vector2(ax_ + ux, ay + uy)


# ------------------------------------------------------------ трассы

## Два слоя сети:
##  • обычные трассы — рёбра Делоне к ближайшим соседям; прямые, тонкие, внутри
##    фракции окрашены её цветом, в ничейном космосе — пунктир. Короткие гаснут при отдалении;
##  • магистрали — отдельная сеть между «хабами» (крупными звёздами): дуги с бегущими
##    импульсами, видны на любом масштабе и задают структуру карты издалека.
func _build_lanes(rng: RandomNumberGenerator) -> void:
	var lanes := Ribbons.new()
	for a in gx.size():
		for b in _neighbors[a]:
			if b < a:
				continue
			var d := _dist(a, b)
			if d > minf(nn[a], nn[b]) * 2.6 or d > 160.0:
				continue
			var same := faction[a] >= 0 and faction[a] == faction[b]
			var col: Color = Color(FACTIONS[faction[a]], 0.55) if same else Color(0.55, 0.65, 0.8, 0.4)
			lanes.line(Vector2(gx[a], gz[a]), Vector2(gx[b], gz[b]), Ribbons.TRASSA, minf(mag[a], mag[b]), 0.0 if same else 1.0, col)

	var hubs := PackedInt32Array()
	for i in gx.size():
		if mag[i] > 0.9:
			hubs.append(i)
	var hp := PackedVector2Array()
	for h in hubs:
		hp.append(Vector2(gx[h], gz[h]))
	var tri := Geometry2D.triangulate_delaunay(hp)
	var seen := {}
	for t in range(0, tri.size(), 3):
		for e in 3:
			var a := hubs[tri[t + e]]
			var b := hubs[tri[t + (e + 1) % 3]]
			var key := Vector2i(mini(a, b), maxi(a, b))
			if seen.has(key) or _dist(a, b) > 700.0:
				continue
			seen[key] = true
			var same := faction[a] >= 0 and faction[a] == faction[b]
			var col: Color = Color(FACTIONS[faction[a]], 0.75) if same else Color(1.0, 0.86, 0.6, 0.6)
			var bulge := rng.randf_range(0.06, 0.14) * (1.0 if rng.randf() < 0.5 else -1.0)
			lanes.arc(Vector2(gx[a], gz[a]), Vector2(gx[b], gz[b]), bulge, 12, Ribbons.TRUNK,
				rng.randf(), 1.0 if rng.randf() < 0.5 else -1.0, col)
	add_child(lanes.build(lane_mat))


# ------------------------------------------------------------ запросы

## Звёзды в прямоугольнике (абсолютные гал. координаты).
func query(rect: Rect2) -> PackedInt32Array:
	var out := PackedInt32Array()
	var c0 := Vector2i(floori(rect.position.x / GRID), floori(rect.position.y / GRID))
	var c1 := Vector2i(floori(rect.end.x / GRID), floori(rect.end.y / GRID))
	if (c1.x - c0.x + 1) * (c1.y - c0.y + 1) > _grid.size():
		return PackedInt32Array(range(gx.size()))
	for x in range(c0.x, c1.x + 1):
		for y in range(c0.y, c1.y + 1):
			var cell = _grid.get(Vector2i(x, y))
			if cell != null:
				out.append_array(cell)
	return out


func nearest(x: float, z: float, max_d: float, min_mag := -1.0) -> int:
	var best := -1
	var best_d := max_d
	for i in query(Rect2(x - max_d, z - max_d, max_d * 2.0, max_d * 2.0)):
		if mag[i] < min_mag:
			continue
		var d := sqrt((gx[i] - x) ** 2 + (gz[i] - z) ** 2)
		if d < best_d:
			best_d = d
			best = i
	return best


## Каждый кадр: параметры шейдеров из текущего масштаба.
func update(view: View, size_max: float, stars_visible: bool) -> void:
	var ppg := view.px(1.0)
	star_mat.set_shader_parameter(&"px_per_g", ppg)
	star_mat.set_shader_parameter(&"size_max", size_max)
	star_mat.set_shader_parameter(&"mag_cut", StarIcons.mag_cut(ppg))
	lane_mat.set_shader_parameter(&"px_per_g", ppg)
	lane_mat.set_shader_parameter(&"mag_cut", StarIcons.mag_cut(ppg))
	# зазор у концов трассы — по типичному размеру звезды на этом масштабе
	lane_mat.set_shader_parameter(&"end_gap_px", StarIcons.size_px(40.0, 0.5, ppg, size_max) * 0.7 + 3.0)
	_stars_mi.visible = stars_visible
