class_name StarSystem
extends Space
## Звёздная система. 1 ед. системы = 0,1 а.е.; в галактических единицах (0,1 св. года)
## это 1,58·10⁻⁵ — система в тысячи раз меньше расстояния до соседней звезды.
## Существует, только пока камера у её звезды (её создаёт main.gd).

const UNIT := 1.5813e-5            # гал. ед. в одной ед. системы
const KM_PER_UNIT := 1.496e7       # км в одной ед. системы
const RADIUS := 200.0              # 20 а.е.
const SUN_KM := 7.0e5
const PLANET_COLORS := [
	Color(0.55, 0.45, 0.38), Color(0.32, 0.48, 0.8), Color(0.7, 0.62, 0.48),
	Color(0.42, 0.62, 0.5), Color(0.78, 0.72, 0.62), Color(0.55, 0.6, 0.72),
]

var star := -1
var title := ""
var planets: Array[Planet] = []
var marker: FleetMarker
var battle: Battle = null
var battle_planet: Planet
var battle_spot := {}               # {pos: [bx,bz,ox,oz], angle, title, seed}

var sun_mat: ShaderMaterial
var _light := OmniLight3D.new()


func _init(g: Galaxy, i: int) -> void:
	star = i
	title = g.names[i]
	bx = g.gx[i]
	bz = g.gz[i]
	unit = UNIT
	var rng := RandomNumberGenerator.new()
	rng.seed = g.seeds[i]

	# Иконка звезды — тот же шейдер и те же параметры, что у звезды на карте,
	# поэтому подмена «карта → система» не видна.
	sun_mat = StarIcons.material(Galaxy.FACTIONS)
	var sun := StarIcons.build(PackedVector2Array([Vector2.ZERO]),
		PackedFloat32Array([g.nn[i], g.mag[i], g.faction[i] + 1, g.spec[i]]), sun_mat)
	sun.name = "Sun"
	add_child(sun)

	var c: Color = StarIcons.SPECTRAL[g.spec[i]]
	_light.light_color = c.lerp(Color.WHITE, 0.5)
	_light.light_energy = 1.3
	_light.omni_attenuation = 0.0
	_light.top_level = true
	add_child(_light)

	var orbit_mat := fade(&"orbits", Draw.unlit(Color(0.5, 0.7, 0.95, 0.22), true))
	var count := rng.randi_range(5, 8)
	for k in count:
		var a_s := 4.0 * pow(1.62, k) * rng.randf_range(0.92, 1.08)   # орбиты растут геометрически
		add_child(Draw.lines(Draw.circle_points(a_s, 256), orbit_mat, true))
		var ang := rng.randf() * TAU
		var r_km := rng.randf_range(2400.0, 9000.0) if k < 3 else rng.randf_range(20000.0, 70000.0)
		var p := Planet.new(self, cos(ang) * a_s, sin(ang) * a_s, r_km,
			PLANET_COLORS[rng.randi() % PLANET_COLORS.size()], "%s %s" % [title, Names.roman(k)])
		add_child(p)
		planets.append(p)
		if k == 2:
			var belt := PackedVector3Array()
			var col := PackedColorArray()
			var br0 := a_s * 1.28
			for n in 900:
				var ba := rng.randf() * TAU
				var br := br0 * (1.0 + rng.randfn(0.0, 0.05))
				belt.append(Vector3(cos(ba) * br, 0.0, sin(ba) * br))
				col.append(Color(0.75, 0.7, 0.65, rng.randf_range(0.2, 0.6)))
			var belt_mi := Draw.points(belt, col, 1.0)
			fade(&"orbits", belt_mi.material_override)
			add_child(belt_mi)

	# Сражение — над дневной стороной второй планеты, в 5000 км над поверхностью.
	battle_planet = planets[1]
	var out := -Vector2(battle_planet.lx, battle_planet.lz).normalized()
	var off_s := battle_planet.true_r + 5000.0 / KM_PER_UNIT
	var spot := Vector2(battle_planet.lx, battle_planet.lz) + out * off_s
	battle_spot = {"pos": pos(spot.x, spot.y), "angle": -out.angle(), "title": "Бой у " + battle_planet.title, "seed": rng.randi()}
	marker = FleetMarker.new(battle_planet, out, off_s)
	add_child(marker)

	var dust := Space.dust(rng, 1400, 8.0, 900.0, RADIUS, 0.6, Color(0.6, 0.7, 0.9), 1.0, 0.4)
	fade(&"dust", dust.material_override)
	add_deep(dust, 900.0)


## Каждый кадр: положение всех вложенных пространств и прозрачности по масштабу.
func update(view: View, star_px_per_g: float, size_max: float) -> void:
	place(view)
	var l := view.log_z
	var sys_px := view.px(RADIUS * UNIT)

	sun_mat.set_shader_parameter(&"px_per_g", star_px_per_g)
	sun_mat.set_shader_parameter(&"size_max", size_max)
	sun_mat.set_shader_parameter(&"true_px", view.px(SUN_KM / KM_PER_UNIT * UNIT))
	get_node(^"Sun").visible = l <= -3.0

	_light.position = transform * Vector3.ZERO
	_light.omni_range = RADIUS * 3.0 * UNIT / view.z()

	var a := Space.ramp(sys_px, 40.0, 180.0)
	set_alpha(&"orbits", a * Space.ramp(l, -13.5, -11.0))     # в разрыве B орбиты уходят
	set_alpha(&"dust", a * Space.ramp(l, -14.5, -11.5))
	var pa := Space.ramp(sys_px, 120.0, 320.0)
	for p in planets:
		p.place(view)
		p.set_alpha(&"body", pa)

	var b := 0.0
	if battle != null:
		battle.place(view)
		b = Space.ramp(view.px(Battle.RADIUS * battle.unit), 50.0, 220.0)
		battle.set_alpha(&"main", b)
	marker.place(view)
	marker.set_alpha(&"marker", pa * (1.0 - b))


## Создать или удалить бой в зависимости от масштаба.
func manage_battle(view: View) -> void:
	var near: bool = view.log_z < -18.3 and view.screen(battle_spot.pos[0], battle_spot.pos[1], battle_spot.pos[2], battle_spot.pos[3]) \
		.distance_to(view.size * 0.5) < view.size.length() * 1.5
	if battle == null and near:
		battle = Battle.new(self)
		add_child(battle)
	elif battle != null and (view.log_z > -17.8 or not near and view.log_z > -18.3):
		battle.queue_free()
		battle = null
