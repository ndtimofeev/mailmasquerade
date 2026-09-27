class_name Battle
extends Space
## Тактический бой на орбите планеты. 1 ед. боя = 10 км. Начало координат — в точке
## боя, поэтому даже при максимальном зуме числа маленькие.
## Локальная ось X смотрит от планеты (к звезде), Z — вдоль орбиты.

const KM := 10.0
const RADIUS := 400.0
const OURS := Color(0.35, 0.8, 1.0)
const THEIRS := Color(1.0, 0.4, 0.3)

var ships: Array[Dictionary] = []      # {node, home, team, phase, title}
var _beams := ImmediateMesh.new()
var _shots: Array[Dictionary] = []
var _rng := RandomNumberGenerator.new()
var _time := 0.0
var _next_shot := 0.0


func _init(sys: StarSystem) -> void:
	var spot := sys.battle_spot
	bx = spot.pos[0]
	bz = spot.pos[1]
	ox = spot.pos[2]
	oz = spot.pos[3]
	unit = sys.unit * KM / StarSystem.KM_PER_UNIT
	angle = spot.angle
	top_level = true
	_rng.seed = spot.seed

	for team in 2:
		var side := -1.0 if team == 0 else 1.0
		for i in 7:
			var row := i - 3
			var home := Vector3(row * 30.0, 0.0, side * (80.0 + absf(row) * 20.0))
			var size := 12.0 if row == 0 else 7.0
			var ship := _ship(size, OURS if team == 0 else THEIRS)
			ship.position = home
			ship.rotation.y = 0.0 if team == 0 else PI
			add_child(ship)
			var t: String = ("Флагман" if row == 0 else "Фрегат %d" % (i + 1)) if team == 0 else "Рейдер %d" % (i + 1)
			ships.append({"node": ship, "home": home, "team": team, "phase": _rng.randf() * TAU, "title": t})

	# Свет звезды: локальная ось +X смотрит на звезду.
	var sun := DirectionalLight3D.new()
	sun.basis = Basis.looking_at(Vector3(-1.0, -0.9, 0.2).normalized(), Vector3.UP)
	sun.light_energy = 1.3
	add_child(sun)

	var beam_mat := Draw.unlit(Color.WHITE, true)
	beam_mat.vertex_color_use_as_albedo = true
	fade(&"main", beam_mat)
	add_child(Draw.mesh_instance(_beams, beam_mat))

	var debris := Space.dust(_rng, 700, 8.0, 900.0, RADIUS, 0.6, Color(0.8, 0.85, 1.0), 1.0, 0.5)
	fade(&"main", debris.material_override)
	add_deep(debris, 900.0)


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
	body.rotation.x = PI / 2.0
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
