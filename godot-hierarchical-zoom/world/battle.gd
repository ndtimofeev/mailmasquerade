class_name Battle
extends Space
## Третий уровень: тактический бой у планеты. 1 единица боя = 1/100 единицы системы.
## Локальная ось X смотрит от звезды наружу (планета — в стороне -X), ось Z — вдоль орбиты.

const UNIT_IN_SYSTEM := 0.01
const RADIUS := 400.0
const HEX := 24.0
const OURS := Color(0.35, 0.8, 1.0)
const THEIRS := Color(1.0, 0.4, 0.3)

var ships: Array[Dictionary] = []      # {node, home, team, phase, name}
var _beams := ImmediateMesh.new()
var _beam_mat: StandardMaterial3D
var _shots: Array[Dictionary] = []
var _rng := RandomNumberGenerator.new()
var _time := 0.0
var _next_shot := 0.0


func _init(system: StarSystem) -> void:
	var spot := system.battle_spot
	var g := system.to_galaxy(spot.lx, spot.lz)
	ox = g[0]
	oz = g[1]
	unit = system.unit * UNIT_IN_SYSTEM
	angle = system.angle + spot.angle
	_rng.seed = spot.seed

	_build_grid()
	for team in 2:
		var side := -1.0 if team == 0 else 1.0
		for i in 7:
			var row := i - 3
			var home := Vector3(row * 30.0, 0.0, side * (80.0 + absf(row) * 20.0))
			var size := 12.0 if row == 0 else 7.0
			var ship := _ship(size, OURS if team == 0 else THEIRS)
			ship.position = home
			ship.rotation.y = 0.0 if team == 0 else PI       # носом к противнику
			add_child(ship)
			var sname: String = ("Флагман" if row == 0 else "Фрегат %d" % (i + 1)) if team == 0 else "Рейдер %d" % (i + 1)
			ships.append({"node": ship, "home": home, "team": team, "phase": _rng.randf() * TAU, "name": sname})

	_beam_mat = fade(&"main", Draw.unlit(Color.WHITE, true))
	_beam_mat.vertex_color_use_as_albedo = true
	add_child(Draw.mesh_instance(_beams, _beam_mat))

	# Обломки и пыль под плоскостью боя — ближний слой параллакса.
	var debris := Space.dust(_rng, 700, 8.0, 900.0, RADIUS, 0.6, Color(0.8, 0.85, 1.0), 1.0, 0.5)
	fade(&"main", debris.material_override)
	add_deep(debris, 900.0)


func _build_grid() -> void:
	var segs := PackedVector3Array()
	var n := int(RADIUS / (HEX * 1.5))
	for q in range(-n, n + 1):
		for r in range(-n, n + 1):
			var c := Vector3(HEX * 1.5 * q, 0.0, HEX * sqrt(3.0) * (r + q * 0.5))
			if Vector2(c.x, c.z).length() > RADIUS * 0.85:
				continue
			for k in 6:
				var a1 := TAU * k / 6.0
				var a2 := TAU * (k + 1) / 6.0
				segs.append(c + Vector3(cos(a1), 0.0, sin(a1)) * HEX * 0.94)
				segs.append(c + Vector3(cos(a2), 0.0, sin(a2)) * HEX * 0.94)
	add_child(Draw.lines(segs, fade(&"main", Draw.unlit(Color(0.45, 0.65, 0.9, 0.09), true))))


func _ship(size: float, color: Color) -> Node3D:
	var hull := CylinderMesh.new()
	hull.top_radius = 0.0
	hull.bottom_radius = size * 0.45
	hull.height = size
	hull.radial_segments = 3
	var mat := Draw.lit(color.darkened(0.55), 0.4)
	mat.emission_enabled = true
	mat.emission = color
	mat.emission_energy_multiplier = 0.35
	var body := Draw.mesh_instance(hull, fade(&"main", mat))
	body.rotation.x = PI / 2.0                          # остриё по +Z

	var em := SphereMesh.new()
	em.radius = size * 0.1
	em.height = size * 0.2
	var engine := Draw.mesh_instance(em, fade(&"main", Draw.glowing(color, 6.0)))
	engine.position = Vector3(0.0, 0.0, -size * 0.55)

	var holder := Node3D.new()
	holder.add_child(body)
	holder.add_child(engine)
	return holder


func _process(delta: float) -> void:
	_time += delta
	for s in ships:
		s.node.position = s.home + Vector3(sin(_time * 0.6 + s.phase) * 2.0, 0.0, cos(_time * 0.4 + s.phase) * 1.5)

	_next_shot -= delta
	if _next_shot <= 0.0:
		_next_shot = _rng.randf_range(0.06, 0.3)
		var a: Dictionary = ships[_rng.randi() % ships.size()]
		var enemies := ships.filter(func(s): return s.team != a.team)
		var b: Dictionary = enemies[_rng.randi() % enemies.size()]
		_shots.append({"a": a.node, "b": b.node, "t": 0.0, "color": OURS if a.team == 0 else THEIRS})

	_beams.clear_surfaces()
	_shots = _shots.filter(func(s): return s.t < 0.3)
	if _shots.is_empty():
		return
	_beams.surface_begin(Mesh.PRIMITIVE_LINES)
	for s in _shots:
		s.t += delta
		var c: Color = s.color
		c.a = clampf(1.0 - s.t / 0.3, 0.0, 1.0)
		_beams.surface_set_color(c)
		_beams.surface_add_vertex(s.a.position)
		_beams.surface_set_color(c)
		_beams.surface_add_vertex(s.b.position)
	_beams.surface_end()
