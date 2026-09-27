class_name GalaxyLevel
extends Level
## Уровень 1: звёздная карта. 1 единица ≈ 0.1 светового года.

const STAR_COUNT := 110
const RADIUS := 1000.0
const LANE_MAX := 230.0
const FACTIONS := [
	{"name": "Содружество", "color": Color(0.25, 0.75, 1.0)},
	{"name": "Синдикат", "color": Color(1.0, 0.45, 0.25)},
	{"name": "Хор", "color": Color(0.7, 0.45, 1.0)},
]
const SPECTRAL := [
	Color(0.65, 0.78, 1.0), Color(1.0, 0.97, 0.9), Color(1.0, 0.88, 0.55),
	Color(1.0, 0.68, 0.38), Color(1.0, 0.5, 0.35),
]

var _labels: Array = []


func _init(rng_seed: int) -> void:
	title = "Галактика"
	units = "1 ед. = 0,1 св. года"
	min_h = 70.0
	max_h = 3200.0
	default_h = 2200.0
	bounds = 1100.0
	cam_near = 5.0
	cam_far = 20000.0
	_build(rng_seed)


func _build(rng_seed: int) -> void:
	var rng := RandomNumberGenerator.new()
	rng.seed = rng_seed

	# Звёзды: две спиральные ветви плюс ядро, с минимальным расстоянием между соседями.
	var pts: Array[Vector2] = []
	var tries := 0
	while pts.size() < STAR_COUNT and tries < 20000:
		tries += 1
		var r := RADIUS * sqrt(rng.randf())
		var arm := float(rng.randi() % 2) * PI
		var a := arm + r / RADIUS * 3.6 + rng.randfn(0.0, 0.35)
		var p := Vector2(cos(a), sin(a)) * r
		var ok := true
		for q in pts:
			if p.distance_to(q) < 70.0:
				ok = false
				break
		if ok:
			pts.append(p)

	# Три фракции с домашними звёздами; владение — по близости к дому.
	var homes := [pts[5], pts[40], pts[80]]
	var owner: Array[int] = []
	for p in pts:
		var o := -1
		var best := 380.0
		for f in homes.size():
			var d: float = p.distance_to(homes[f])
			if d < best:
				best = d
				o = f
		owner.append(o)

	# Территория: мягкие радиальные пятна под звёздами; перекрываясь, дают «облако» влияния.
	var spot_tex := GradientTexture2D.new()
	spot_tex.fill = GradientTexture2D.FILL_RADIAL
	spot_tex.fill_from = Vector2(0.5, 0.5)
	spot_tex.fill_to = Vector2(1.0, 0.5)
	var g := Gradient.new()
	g.set_color(0, Color(1, 1, 1, 1))
	g.set_color(1, Color(1, 1, 1, 0))
	spot_tex.gradient = g
	for f in FACTIONS.size():
		var mat := Draw.unlit(Color(FACTIONS[f].color, 0.10), true)
		mat.albedo_texture = spot_tex
		var plane := PlaneMesh.new()
		plane.size = Vector2(260, 260)
		for i in pts.size():
			if owner[i] != f:
				continue
			var spot := MeshInstance3D.new()
			spot.mesh = plane
			spot.material_override = mat
			spot.position = Vector3(pts[i].x, -2.0, pts[i].y)
			add_child(spot)

	# Звёздные трассы: рёбра триангуляции Делоне, отфильтрованные по длине.
	var packed := PackedVector2Array(pts)
	var tri := Geometry2D.triangulate_delaunay(packed)
	var seen := {}
	var lane_pts := PackedVector3Array()
	for t in range(0, tri.size(), 3):
		for e in 3:
			var i1 := tri[t + e]
			var i2 := tri[t + (e + 1) % 3]
			var key := Vector2i(mini(i1, i2), maxi(i1, i2))
			if seen.has(key) or pts[i1].distance_to(pts[i2]) > LANE_MAX:
				continue
			seen[key] = true
			lane_pts.append(Vector3(pts[i1].x, 0.0, pts[i1].y))
			lane_pts.append(Vector3(pts[i2].x, 0.0, pts[i2].y))
	add_child(Draw.lines(lane_pts, Color(0.45, 0.65, 0.9, 0.28), false, true))

	# Сами звёзды.
	for i in pts.size():
		var pos := Vector3(pts[i].x, 0.0, pts[i].y)
		var col: Color = SPECTRAL[rng.randi() % SPECTRAL.size()]
		var r := rng.randf_range(3.5, 7.0)
		var sphere := SphereMesh.new()
		sphere.radius = r
		sphere.height = r * 2.0
		sphere.radial_segments = 16
		sphere.rings = 8
		var star := MeshInstance3D.new()
		star.mesh = sphere
		star.material_override = Draw.glowing(col, 4.0)
		star.position = pos
		add_child(star)

		if owner[i] >= 0:
			var ring := Draw.circle(r * 2.6, Color(FACTIONS[owner[i]].color, 0.85), 40)
			ring.position = pos
			add_child(ring)

		var star_name := Names.star(rng)
		var lbl := Draw.label(star_name, Color(0.78, 0.88, 1.0))
		lbl.position = pos + Vector3(0.0, 0.0, r + 10.0)
		lbl.vertical_alignment = VERTICAL_ALIGNMENT_TOP
		add_child(lbl)
		_labels.append(lbl)
		add_item(pos, r, star_name, true, rng.randi())

	add_background(rng_seed + 1, 6000.0, Color(0.35, 0.45, 1.0))


func on_camera(height: float) -> void:
	# Семантический зум: на обзорной высоте имена скрыты, ближе — проявляются.
	Level.fade_labels(_labels, height, 900.0, 1500.0)
