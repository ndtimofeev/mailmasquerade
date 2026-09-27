class_name BattleLevel
extends Level
## Уровень 3: тактический бой. 1 единица ≈ 10 км.

const HEX := 26.0
const HEX_RINGS := 7
const OURS := Color(0.3, 0.8, 1.0)
const THEIRS := Color(1.0, 0.35, 0.25)
const SHIP_NAMES := ["Вега", "Гелиос", "Кондор", "Ирида", "Сокол", "Аргус", "Минерва", "Орёл", "Тайфун"]

var _ships: Array[Dictionary] = []     # {node, home, team, phase}
var _beams: ImmediateMesh
var _beam_mat: StandardMaterial3D
var _shots: Array[Dictionary] = []     # {a, b, t, color}
var _time := 0.0
var _next_shot := 0.0
var _rng := RandomNumberGenerator.new()


func _init(battle: Dictionary) -> void:
	title = battle.name
	units = "1 ед. = 10 км"
	min_h = 25.0
	max_h = 1100.0
	default_h = 560.0
	bounds = 380.0
	cam_near = 0.5
	cam_far = 20000.0
	_rng.seed = battle.seed
	_build()


func _build() -> void:
	# Гексагональная тактическая сетка.
	var hex_lines := PackedVector3Array()
	for q in range(-HEX_RINGS, HEX_RINGS + 1):
		for r in range(-HEX_RINGS, HEX_RINGS + 1):
			if absi(q + r) > HEX_RINGS:
				continue
			var c := Vector3(HEX * 1.5 * q, 0.0, HEX * sqrt(3.0) * (r + q * 0.5))
			for k in 6:
				var a1 := TAU * k / 6.0
				var a2 := TAU * (k + 1) / 6.0
				hex_lines.append(c + Vector3(cos(a1), 0.0, sin(a1)) * HEX * 0.97)
				hex_lines.append(c + Vector3(cos(a2), 0.0, sin(a2)) * HEX * 0.97)
	add_child(Draw.lines(hex_lines, Color(0.4, 0.6, 0.9, 0.16), false, true))

	var sun := DirectionalLight3D.new()
	sun.rotation = Vector3(deg_to_rad(-60.0), deg_to_rad(-35.0), 0.0)
	sun.light_energy = 1.4
	add_child(sun)

	# Два флота клином друг напротив друга.
	var names := SHIP_NAMES.duplicate()
	for team in 2:
		var dir := 1.0 if team == 0 else -1.0
		for i in 7:
			var row := i - 3
			var home := Vector3(-dir * (95.0 + absf(row) * 22.0), 0.0, row * 34.0)
			var size := 13.0 if i == 3 else 8.0
			var ship := _ship(size, OURS if team == 0 else THEIRS)
			ship.position = home
			ship.rotation.y = dir * PI / 2.0
			add_child(ship)
			var sname: String
			if team == 0:
				sname = ("Флагман «%s»" if i == 3 else "Фрегат «%s»") % names.pop_at(_rng.randi() % names.size())
			else:
				sname = "Рейдер противника %d" % (i + 1)
			add_item(home, size, sname, false)
			_ships.append({"node": ship, "home": home, "team": team, "phase": _rng.randf() * TAU, "item": items[-1]})

	_beams = ImmediateMesh.new()
	var beams_mi := MeshInstance3D.new()
	beams_mi.mesh = _beams
	beams_mi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	add_child(beams_mi)
	_beam_mat = Draw.unlit(Color.WHITE, true)
	_beam_mat.vertex_color_use_as_albedo = true

	# Планета далеко внизу: огромная и глубокая, поэтому при панорамировании почти не двигается.
	var pm := SphereMesh.new()
	pm.radius = 2600.0
	pm.height = 5200.0
	pm.radial_segments = 96
	pm.rings = 48
	var planet := MeshInstance3D.new()
	planet.mesh = pm
	planet.material_override = Draw.lit(Color(0.08, 0.14, 0.28), 1.0)
	planet.position = Vector3(2600.0, -6500.0, 1400.0)
	add_child(planet)

	add_background(_rng.randi(), 12000.0, Color(0.4, 0.3, 0.9))


func _ship(size: float, color: Color) -> Node3D:
	var hull := CylinderMesh.new()
	hull.top_radius = 0.0
	hull.bottom_radius = size * 0.5
	hull.height = size
	hull.radial_segments = 3
	var mi := MeshInstance3D.new()
	mi.mesh = hull
	var mat := Draw.lit(color.darkened(0.35), 0.5)
	mat.emission_enabled = true
	mat.emission = color
	mat.emission_energy_multiplier = 0.6
	mi.material_override = mat
	mi.rotation.x = PI / 2.0           # остриё по +Z

	var engine := MeshInstance3D.new()
	var em := SphereMesh.new()
	em.radius = size * 0.12
	em.height = size * 0.24
	engine.mesh = em
	engine.material_override = Draw.glowing(color, 6.0)
	engine.position = Vector3(0.0, 0.0, -size * 0.55)

	var holder := Node3D.new()
	holder.add_child(mi)
	holder.add_child(engine)
	return holder


func _process(delta: float) -> void:
	_time += delta
	for s in _ships:
		var sway := Vector3(sin(_time * 0.7 + s.phase) * 3.0, 0.0, cos(_time * 0.5 + s.phase) * 2.0)
		s.node.position = s.home + sway
		s.item.pos = s.node.position

	_next_shot -= delta
	if _next_shot <= 0.0:
		_next_shot = _rng.randf_range(0.05, 0.25)
		var a: Dictionary = _ships[_rng.randi() % _ships.size()]
		var candidates := _ships.filter(func(s): return s.team != a.team)
		var b: Dictionary = candidates[_rng.randi() % candidates.size()]
		_shots.append({"a": a.node, "b": b.node, "t": 0.0, "color": OURS if a.team == 0 else THEIRS})

	_beams.clear_surfaces()
	var alive: Array[Dictionary] = []
	for shot in _shots:
		shot.t += delta
		if shot.t < 0.35:
			alive.append(shot)
	_shots = alive
	if _shots.is_empty():
		return
	_beams.surface_begin(Mesh.PRIMITIVE_LINES, _beam_mat)
	for shot in _shots:
		var c: Color = shot.color * 1.6      # >1.0 — попадёт под glow
		c.a = 1.0 - shot.t / 0.35
		_beams.surface_set_color(c)
		_beams.surface_add_vertex(shot.a.position + Vector3.UP)
		_beams.surface_set_color(c)
		_beams.surface_add_vertex(shot.b.position + Vector3.UP)
	_beams.surface_end()
