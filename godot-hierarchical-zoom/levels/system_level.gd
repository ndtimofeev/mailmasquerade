class_name SystemLevel
extends Level
## Уровень 2: звёздная система. 1 единица ≈ 0,01 а.е.

const PLANET_COLORS := [
	Color(0.55, 0.45, 0.35), Color(0.3, 0.5, 0.85), Color(0.75, 0.6, 0.4),
	Color(0.45, 0.7, 0.55), Color(0.85, 0.75, 0.6), Color(0.6, 0.65, 0.8),
]

var _planets: Array[Dictionary] = []   # {node, orbit, angle, speed, item}
var _battle_item: Dictionary
var _battle_node: Node3D
var _battle_planet := 0
var _labels: Array = []
var _time := 0.0


func _init(star: Dictionary) -> void:
	title = star.name
	units = "1 ед. = 0,01 а.е."
	min_h = 45.0
	max_h = 2400.0
	default_h = 1300.0
	bounds = 800.0
	cam_near = 1.0
	cam_far = 12000.0
	_build(star)


func _build(star: Dictionary) -> void:
	var rng := RandomNumberGenerator.new()
	rng.seed = star.seed

	var star_col := Color(1.0, 0.9, 0.7)
	var sun := MeshInstance3D.new()
	var sm := SphereMesh.new()
	sm.radius = 22.0
	sm.height = 44.0
	sun.mesh = sm
	sun.material_override = Draw.glowing(star_col, 6.0)
	add_child(sun)
	add_item(Vector3.ZERO, 22.0, star.name, false)

	var light := OmniLight3D.new()
	light.light_color = star_col
	light.light_energy = 6.0
	light.omni_range = 2000.0
	light.omni_attenuation = 0.4
	add_child(light)

	var count := rng.randi_range(4, 7)
	var orbit := 90.0
	for i in count:
		orbit += rng.randf_range(55.0, 110.0)
		add_child(Draw.circle(orbit, Color(0.5, 0.7, 1.0, 0.22), 160))

		var r := rng.randf_range(5.0, 14.0)
		var pm := SphereMesh.new()
		pm.radius = r
		pm.height = r * 2.0
		var planet := MeshInstance3D.new()
		planet.mesh = pm
		planet.material_override = Draw.lit(PLANET_COLORS[rng.randi() % PLANET_COLORS.size()])
		add_child(planet)

		var pname: String = star.name + " " + Names.roman(i)
		var lbl := Draw.label(pname, Color(0.8, 0.88, 1.0), 13)
		lbl.vertical_alignment = VERTICAL_ALIGNMENT_TOP
		planet.add_child(lbl)
		lbl.position = Vector3(0.0, 0.0, r + 6.0)
		_labels.append(lbl)

		add_item(Vector3.ZERO, r, pname, false)
		_planets.append({
			"node": planet, "orbit": orbit, "angle": rng.randf() * TAU,
			"speed": 0.9 / sqrt(orbit), "item": items[-1],
		})

		# Пояс астероидов между третьей и четвёртой орбитами.
		if i == 2:
			var belt := PackedVector3Array()
			var belt_col := PackedColorArray()
			for k in 900:
				var a := rng.randf() * TAU
				var rr := orbit + 40.0 + rng.randfn(0.0, 9.0)
				belt.append(Vector3(cos(a) * rr, 0.0, sin(a) * rr))
				belt_col.append(Color(0.7, 0.65, 0.6, rng.randf_range(0.3, 0.8)))
			add_child(Draw.points(belt, belt_col, 2.0))
			orbit += 60.0

	# Столкновение флотов у одной из планет — вход на уровень боя.
	_battle_planet = mini(1, _planets.size() - 1)
	_battle_node = Node3D.new()
	add_child(_battle_node)
	var ours := _fleet_marker(Color(0.3, 0.8, 1.0))
	ours.position = Vector3(-9.0, 0.0, 0.0)
	ours.rotation.y = PI / 2.0      # остриём к противнику
	_battle_node.add_child(ours)
	var theirs := _fleet_marker(Color(1.0, 0.35, 0.25))
	theirs.position = Vector3(9.0, 0.0, 0.0)
	theirs.rotation.y = -PI / 2.0
	_battle_node.add_child(theirs)
	var pulse := Draw.circle(1.0, Color(1.0, 0.4, 0.3, 0.8), 48)
	pulse.name = "Pulse"
	_battle_node.add_child(pulse)
	var bname: String = "Сражение у " + _planets[_battle_planet].item.name
	var blbl := Draw.label(bname, Color(1.0, 0.6, 0.5), 13)
	blbl.vertical_alignment = VERTICAL_ALIGNMENT_BOTTOM
	blbl.position = Vector3(0.0, 0.0, -22.0)
	_battle_node.add_child(blbl)
	_labels.append(blbl)
	add_item(Vector3.ZERO, 18.0, bname, true, rng.randi())
	_battle_item = items[-1]

	add_background(star.seed + 7, 3500.0, star_col.lerp(Color(0.3, 0.4, 1.0), 0.6))
	_process(0.0)


func _fleet_marker(color: Color) -> Node3D:
	var cone := CylinderMesh.new()     # треугольник: 3 грани, вид сверху
	cone.top_radius = 0.0
	cone.bottom_radius = 5.0
	cone.height = 10.0
	cone.radial_segments = 3
	var mi := MeshInstance3D.new()
	mi.mesh = cone
	mi.material_override = Draw.glowing(color, 2.5)
	mi.rotation.x = PI / 2.0
	var holder := Node3D.new()        # держатель, чтобы поворачивать маркер вокруг вертикали
	holder.add_child(mi)
	return holder


func _process(delta: float) -> void:
	_time += delta
	for p in _planets:
		p.angle += p.speed * delta * 0.25
		var pos := Vector3(cos(p.angle) * p.orbit, 0.0, sin(p.angle) * p.orbit)
		p.node.position = pos
		p.item.pos = pos
	var bp: Dictionary = _planets[_battle_planet]
	var bpos: Vector3 = bp.node.position + bp.node.position.normalized() * 34.0
	_battle_node.position = bpos
	_battle_item.pos = bpos
	var s := 16.0 + 4.0 * sin(_time * 4.0)
	_battle_node.get_node("Pulse").scale = Vector3(s, 1.0, s)


func on_camera(height: float) -> void:
	Level.fade_labels(_labels, height, 1100.0, 1800.0)
