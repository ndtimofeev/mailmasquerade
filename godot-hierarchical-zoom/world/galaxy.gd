class_name Galaxy
extends Space
## Верхний уровень: звёздная карта. Её единица и есть «галактическая единица».

const RADIUS := 1000.0
const SPACING := 70.0              # минимальное расстояние между звёздами
const CELL_R := 95.0               # максимальный радиус ячейки территории
const FACTIONS := [
	Color(0.35, 0.75, 1.0),        # голубые
	Color(1.0, 0.55, 0.3),         # оранжевые
	Color(0.75, 0.5, 1.0),         # фиолетовые
]
const SPECTRAL := [
	Color(0.7, 0.8, 1.0), Color(0.95, 0.95, 1.0), Color(1.0, 0.92, 0.75),
	Color(1.0, 0.75, 0.5), Color(1.0, 0.55, 0.45),
]

## {gx, gz, name, color, owner, seed, sprite, base_px}
var stars: Array[Dictionary] = []
var _halo_mats: Array = []


func _init(rng_seed: int) -> void:
	var rng := RandomNumberGenerator.new()
	rng.seed = rng_seed
	var pts := _place_stars(rng)
	var owner := _assign_owners(pts)
	_build_territory(pts, owner)
	_build_lanes(pts)
	_build_stars(rng, pts, owner)

	# Глубокий 3D-фон: звёзды галактического диска на глубине до 6000 единиц.
	# При обзоре галактики они дают сильный параллакс, а при зуме в бой ведут себя
	# как неподвижное небо (угол до далёкой звезды почти не меняется) — как и в реальности.
	add_deep(Space.dust(rng, 9000, 40.0, 6000.0, 1500.0, 0.6, Color(0.7, 0.8, 1.0), 1.0, 0.75), 6000.0)
	add_deep(Space.dust(rng, 900, 40.0, 4000.0, 1500.0, 0.6, Color(0.8, 0.85, 1.0), 2.0, 0.55), 4000.0)


func _place_stars(rng: RandomNumberGenerator) -> Array[Vector2]:
	var pts: Array[Vector2] = []
	var tries := 0
	while pts.size() < 120 and tries < 30000:
		tries += 1
		var r := RADIUS * sqrt(rng.randf())
		var a := float(rng.randi() % 2) * PI + r / RADIUS * 3.4 + rng.randfn(0.0, 0.4)
		var p := Vector2(cos(a), sin(a)) * r
		if pts.all(func(q): return p.distance_to(q) >= SPACING):
			pts.append(p)
	return pts


## Три фракции: звезда принадлежит ближайшей «столице», если та не слишком далеко.
func _assign_owners(pts: Array[Vector2]) -> Array[int]:
	var homes := [pts[3], pts[45], pts[90]]
	var owner: Array[int] = []
	for p in pts:
		var o := -1
		var best := 360.0
		for f in homes.size():
			if p.distance_to(homes[f]) < best:
				best = p.distance_to(homes[f])
				o = f
		owner.append(o)
	return owner


## Территории как в RC: ячейки Вороного (круг, обрезанный серединными перпендикулярами
## к соседям), едва заметная заливка и чёткая граница вокруг объединения ячеек фракции.
func _build_territory(pts: Array[Vector2], owner: Array[int]) -> void:
	for f in FACTIONS.size():
		var cells: Array[PackedVector2Array] = []
		for i in pts.size():
			if owner[i] == f:
				cells.append(_cell(pts, i))

		var fill := PackedVector3Array()
		for c in cells:
			var tri := Geometry2D.triangulate_polygon(c)
			for idx in tri:
				fill.append(Vector3(c[idx].x, -0.05, c[idx].y))
		var fill_mat := fade(&"detail", Draw.unlit(Color(FACTIONS[f], 0.045), true))
		add_child(Draw.mesh_instance(Draw.arrays_mesh(Mesh.PRIMITIVE_TRIANGLES, fill), fill_mat))

		var border_mat := fade(&"detail", Draw.unlit(Color(FACTIONS[f], 0.55), true))
		for outline in _merge(cells):
			var line := PackedVector3Array()
			for p in outline:
				line.append(Vector3(p.x, 0.0, p.y))
			line.append(line[0])
			add_child(Draw.lines(line, border_mat, true))


func _cell(pts: Array[Vector2], i: int) -> PackedVector2Array:
	var p := pts[i]
	var cell := PackedVector2Array()
	for k in 32:
		cell.append(p + Vector2.from_angle(TAU * k / 32.0) * CELL_R)
	for j in pts.size():
		var q := pts[j]
		if j == i or p.distance_to(q) > CELL_R * 2.0:
			continue
		# полуплоскость со стороны p от серединного перпендикуляра pq
		var mid := (p + q) * 0.5
		var n := (q - p).normalized()
		var t := n.orthogonal() * 1000.0
		var half := PackedVector2Array([mid + t, mid + t - n * 1000.0, mid - t - n * 1000.0, mid - t])
		var cut := Geometry2D.intersect_polygons(cell, half)
		if not cut.is_empty():
			cell = cut[0]
	return cell


## Слить соседние ячейки в общий контур (дырки игнорируем).
func _merge(cells: Array[PackedVector2Array]) -> Array[PackedVector2Array]:
	var result: Array[PackedVector2Array] = []
	for c in cells:
		var cur: PackedVector2Array = Geometry2D.offset_polygon(c, 0.5)[0]
		var i := 0
		while i < result.size():
			if Geometry2D.intersect_polygons(result[i], cur).is_empty():
				i += 1
				continue
			var merged := Geometry2D.merge_polygons(result[i], cur)
			var best := 0.0
			for m in merged:              # внешний контур — самый большой по площади
				var area := _area(m)
				if area > best:
					best = area
					cur = m
			result.remove_at(i)
			i = 0
		result.append(cur)
	return result


static func _area(poly: PackedVector2Array) -> float:
	var s := 0.0
	for i in poly.size():
		s += poly[i].cross(poly[(i + 1) % poly.size()])
	return absf(s) * 0.5


## Трассы: рёбра триангуляции Делоне короче порога.
func _build_lanes(pts: Array[Vector2]) -> void:
	var tri := Geometry2D.triangulate_delaunay(PackedVector2Array(pts))
	var seen := {}
	var segs := PackedVector3Array()
	for t in range(0, tri.size(), 3):
		for e in 3:
			var a := tri[t + e]
			var b := tri[t + (e + 1) % 3]
			var key := Vector2i(mini(a, b), maxi(a, b))
			if seen.has(key) or pts[a].distance_to(pts[b]) > 190.0:
				continue
			seen[key] = true
			# отступ от звёзд, чтобы линия не упиралась в ореол
			var dir := (pts[b] - pts[a]).normalized() * 9.0
			segs.append(Vector3(pts[a].x + dir.x, 0.0, pts[a].y + dir.y))
			segs.append(Vector3(pts[b].x - dir.x, 0.0, pts[b].y - dir.y))
	add_child(Draw.lines(segs, fade(&"detail", Draw.unlit(Color(0.55, 0.7, 0.9, 0.16), true))))


func _build_stars(rng: RandomNumberGenerator, pts: Array[Vector2], owner: Array[int]) -> void:
	var tex := Draw.halo_texture()
	var mats := []
	for c in SPECTRAL:
		var m := Draw.unlit(Color(c.r * 1.6, c.g * 1.6, c.b * 1.6), true)
		m.albedo_texture = tex
		mats.append(m)
		_halo_mats.append(m)
	var quad := PlaneMesh.new()          # лежит в плоскости XZ, лицом вверх — к камере
	quad.size = Vector2.ONE

	for i in pts.size():
		var cls := rng.randi() % SPECTRAL.size()
		var sprite := Draw.mesh_instance(quad, mats[cls])
		sprite.position = Vector3(pts[i].x, 0.02, pts[i].y)
		add_child(sprite)
		stars.append({
			"gx": float(pts[i].x), "gz": float(pts[i].y),
			"name": Names.star(rng), "color": SPECTRAL[cls], "owner": owner[i],
			"seed": rng.randi(), "sprite": sprite, "base_px": rng.randf_range(16.0, 26.0),
		})


## Каждый кадр: ореол звезды держит постоянный размер в пикселях,
## пока настоящий диск звезды (из уровня системы) не станет больше.
func update_sprites(view: View) -> void:
	var g_per_px := 1.0 / view.px(1.0)
	var sun_px := view.px(StarSystem.SUN_R * StarSystem.UNIT)
	for s in stars:
		var size_px: float = maxf(s.base_px, sun_px * 2.4)
		s.sprite.scale = Vector3.ONE * size_px * g_per_px
	# Когда диск звезды уже виден сам, ореол становится лёгкой короной, а не пятном.
	for m in _halo_mats:
		m.albedo_color.a = 1.0 - 0.75 * Space.ramp(sun_px, 4.0, 40.0)
