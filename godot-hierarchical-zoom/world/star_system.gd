class_name StarSystem
extends Space
## Второй уровень: звёздная система. Создаётся, когда зум приближается к звезде,
## и живёт ВНУТРИ галактики, в точке своей звезды, в 50 раз более мелких единицах.

const UNIT := 0.02                  # галактических единиц в одной единице системы
const RADIUS := 700.0               # радиус системы, в её единицах (= 14 гал. ед.)
const SUN_R := 16.0
const PLANET_COLORS := [
	Color(0.55, 0.45, 0.38), Color(0.32, 0.48, 0.8), Color(0.7, 0.62, 0.48),
	Color(0.42, 0.62, 0.5), Color(0.78, 0.72, 0.62), Color(0.55, 0.6, 0.72),
]

var star: Dictionary
var planets: Array[Dictionary] = []    # {lx, lz, r, name}
var battle_spot: Dictionary            # {lx, lz, angle, name, seed}
var battle: Battle = null              # создаётся по мере зума

var _light: OmniLight3D
var _marker: Node3D


func _init(star_: Dictionary) -> void:
	star = star_
	ox = star.gx
	oz = star.gz
	unit = UNIT
	var rng := RandomNumberGenerator.new()
	rng.seed = star.seed

	# Звезда. Все сферы опущены под плоскость на свой радиус: вершина касается y = 0,
	# поэтому камера, висящая над плоскостью, никогда не окажется внутри тела.
	var sun_mesh := SphereMesh.new()
	sun_mesh.radius = SUN_R
	sun_mesh.height = SUN_R * 2.0
	var sun_mat := fade(&"body", Draw.glowing(star.color, 2.2))
	var sun := Draw.mesh_instance(sun_mesh, sun_mat)
	sun.position = Vector3(0, -SUN_R, 0)
	add_child(sun)

	_light = OmniLight3D.new()
	_light.light_color = star.color.lerp(Color.WHITE, 0.5)
	_light.light_energy = 2.2
	_light.omni_attenuation = 0.0     # без затухания: освещение одинаково на любом масштабе
	_light.top_level = true
	add_child(_light)

	var orbit_mat := fade(&"orbits", Draw.unlit(Color(0.5, 0.7, 0.95, 0.2), true))
	var orbit := 80.0
	for i in rng.randi_range(4, 7):
		orbit += rng.randf_range(55.0, 100.0)
		add_child(Draw.lines(Draw.circle_points(orbit, 192), orbit_mat, true))
		var a := rng.randf() * TAU
		var r := rng.randf_range(5.0, 13.0)
		var pm := SphereMesh.new()
		pm.radius = r
		pm.height = r * 2.0
		pm.radial_segments = 48
		pm.rings = 24
		var planet := Draw.mesh_instance(pm, fade(&"body", Draw.lit(PLANET_COLORS[rng.randi() % PLANET_COLORS.size()])))
		planet.position = Vector3(cos(a) * orbit, -r, sin(a) * orbit)
		add_child(planet)
		planets.append({"lx": cos(a) * orbit, "lz": sin(a) * orbit, "r": r, "name": "%s %s" % [star.name, Names.roman(i)]})

		if i == 2:   # пояс астероидов
			var belt := PackedVector3Array()
			var col := PackedColorArray()
			for k in 700:
				var ba := rng.randf() * TAU
				var br := orbit + 38.0 + rng.randfn(0.0, 7.0)
				belt.append(Vector3(cos(ba) * br, 0.0, sin(ba) * br))
				col.append(Color(0.75, 0.7, 0.65, rng.randf_range(0.25, 0.7)))
			var belt_mi := Draw.points(belt, col, 1.0)
			fade(&"orbits", belt_mi.material_override)
			add_child(belt_mi)
			orbit += 55.0

	# Сражение — рядом со второй планетой, снаружи от её орбиты.
	var p: Dictionary = planets[mini(1, planets.size() - 1)]
	var out: Vector2 = Vector2(p.lx, p.lz).normalized()
	var spot: Vector2 = Vector2(p.lx, p.lz) + out * (p.r + 3.5)
	battle_spot = {"lx": spot.x, "lz": spot.y, "angle": -out.angle(), "name": "Сражение у " + p.name, "seed": rng.randi()}
	_build_fleet_marker(spot, out)

	# Слой пыли под плоскостью системы — свой параллакс на этом масштабе.
	var dust := Space.dust(rng, 1400, 45.0, 2500.0, RADIUS, 0.6, Color(0.6, 0.7, 0.9), 1.0, 0.45)
	fade(&"dust", dust.material_override)
	add_deep(dust, 2500.0)


## Условный значок сражения: два треугольника. При дальнейшем зуме он растворяется,
## а на его месте проявляются настоящие корабли уровня боя — того же размера.
func _build_fleet_marker(spot: Vector2, out: Vector2) -> void:
	var holder := Node3D.new()
	_marker = holder
	holder.position = Vector3(spot.x, 0.0, spot.y)
	holder.rotation.y = -out.angle()
	add_child(holder)
	var tri := CylinderMesh.new()
	tri.top_radius = 0.0
	tri.bottom_radius = 1.4
	tri.height = 2.6
	tri.radial_segments = 3
	for side in [-1.0, 1.0]:
		var color := Battle.OURS if side < 0 else Battle.THEIRS
		var mi := Draw.mesh_instance(tri, fade(&"marker", Draw.glowing(color, 2.5)))
		mi.rotation = Vector3(PI / 2.0, 0.0, 0.0)
		var arm := Node3D.new()
		arm.position = Vector3(0.0, 0.0, side * 2.2)     # вдоль орбиты, по разные стороны
		arm.rotation.y = 0.0 if side < 0 else PI         # остриями друг к другу
		arm.add_child(mi)
		holder.add_child(arm)


func place(view: View) -> void:
	super.place(view)
	# Свет от звезды: позиция и радиус действия пересчитываются в координаты камеры.
	_light.position = transform * Vector3.ZERO
	_light.omni_range = RADIUS * 4.0 * unit / view.z()
	# Значок флотов — это иконка: всегда ~26 px на экране, независимо от зума.
	_marker.scale = Vector3.ONE * (26.0 / view.px(5.0 * unit))
