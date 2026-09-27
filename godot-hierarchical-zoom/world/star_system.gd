class_name StarSystem
extends Space
## Звёздная система. 1 ед. системы = 0,1 а.е. Существует, только пока камера у своей звезды.
## Звезда и планеты — анимированные пиксельные тела (Pixel Planet Generator).

const UNIT := 1.5813e-5            # гал. ед. в одной ед. системы
const KM_PER_UNIT := 1.496e7       # км в одной ед. системы
const RADIUS := 200.0              # 20 а.е.
const SUN_KM := 7.0e5
const SUN_ICON_PX := 30.0          # радиус пиксельной звезды на карте системы

## Тип планеты по номеру орбиты: [варианты], радиус в км, диаметр иконки в px.
const ORBITS := [
	[["lava", "barren"], Vector2(2400, 4000), 18.0],
	[["dry", "barren"], Vector2(3500, 6000), 22.0],
	[["terran", "islands"], Vector2(5500, 7000), 26.0],
	[["islands", "ice", "dry"], Vector2(4000, 6500), 24.0],
	[["gas", "ringed"], Vector2(45000, 70000), 36.0],
	[["ringed", "gas"], Vector2(40000, 65000), 34.0],
	[["ice", "barren"], Vector2(3000, 6000), 22.0],
	[["gas"], Vector2(25000, 40000), 30.0],
]

var star := -1
var title := ""
var planets: Array[Planet] = []
var marker: FleetMarker
var battle: Battle = null
var battle_planet: Planet
var battle_spot := {}               # {pos: [bx,bz,ox,oz], angle, title, seed}

var sun_mat: ShaderMaterial         # иконка звезды — та же, что на карте
var sun_pixel: PixelBody            # пиксельная звезда — на карте системы


func _init(g: Galaxy, i: int) -> void:
	star = i
	title = g.names[i]
	bx = g.gx[i]
	bz = g.gz[i]
	unit = UNIT
	var rng := RandomNumberGenerator.new()
	rng.seed = g.seeds[i]

	# Иконка звезды — тот же шейдер и параметры, что на карте, поэтому подмена не видна.
	sun_mat = StarIcons.material(Galaxy.FACTIONS)
	var sun := StarIcons.build(PackedVector2Array([Vector2.ZERO]),
		PackedFloat32Array([g.nn[i], g.mag[i], g.faction[i] + 1, g.spec[i]]), sun_mat)
	sun.name = "Sun"
	add_child(sun)
	# Пиксельная звезда проявляется поверх иконки в конце перелёта к системе.
	sun_pixel = PixelBody.new("star", rng.randi())
	sun_pixel.tint_star(StarIcons.SPECTRAL[g.spec[i]])
	add_child(sun_pixel)

	var orbit_mat := fade(&"orbits", Draw.unlit(Color(0.5, 0.7, 0.95, 0.2), true))
	var count := rng.randi_range(5, 8)
	for k in count:
		var a_s := 28.0 + k * 23.0 + rng.randf_range(-4.0, 4.0)
		add_child(Draw.lines(Draw.circle_points(a_s, 256), orbit_mat, true))
		var o: Array = ORBITS[k]
		var kind: String = o[0][rng.randi() % o[0].size()]
		var ang := rng.randf() * TAU
		var p := Planet.new(self, cos(ang) * a_s, sin(ang) * a_s, kind, rng.randf_range(o[1].x, o[1].y), o[2],
			"%s %s" % [title, Names.roman(k)], rng.randi(), rng.randf() < 0.35)
		add_child(p)
		planets.append(p)
		if k == 3:
			_add_belt(rng, a_s + 11.5)

	# Сражение — над дневной стороной обитаемой планеты, в 5000 км над поверхностью.
	battle_planet = planets[2]
	var out := -Vector2(battle_planet.lx, battle_planet.lz).normalized()
	var off_s := battle_planet.true_r + 5000.0 / KM_PER_UNIT
	var spot := Vector2(battle_planet.lx, battle_planet.lz) + out * off_s
	battle_spot = {"pos": pos(spot.x, spot.y), "angle": -out.angle(), "title": "Бой у " + battle_planet.title, "seed": rng.randi()}
	marker = FleetMarker.new(battle_planet, out, off_s)
	add_child(marker)

	var dust := Space.dust(rng, 1400, 8.0, 900.0, RADIUS, 0.6, Color(0.6, 0.7, 0.9), 1.0, 0.4)
	fade(&"dust", dust.material_override)
	add_deep(dust, 900.0)


func _add_belt(rng: RandomNumberGenerator, r0: float) -> void:
	var belt := PackedVector3Array()
	var col := PackedColorArray()
	for n in 1100:
		var ba := rng.randf() * TAU
		var br := r0 + rng.randfn(0.0, 2.2)
		belt.append(Vector3(cos(ba) * br, 0.0, sin(ba) * br))
		col.append(Color(0.75, 0.7, 0.65, rng.randf_range(0.2, 0.6)))
	var belt_mi := Draw.points(belt, col, 1.0)
	fade(&"orbits", belt_mi.material_override)
	add_child(belt_mi)


## Каждый кадр: положение вложенных пространств и прозрачности по масштабу.
func update(view: View, star_px_per_g: float, size_max: float) -> void:
	place(view)
	var l := view.log_z
	var sys_px := view.px(RADIUS * UNIT)
	var pps := view.px(UNIT)

	# Иконка → пиксельная звезда: перекрёстное затухание в конце перелёта к системе.
	var px_a := Space.ramp(-l, 5.6, 7.2)
	sun_mat.set_shader_parameter(&"px_per_g", star_px_per_g)
	sun_mat.set_shader_parameter(&"size_max", size_max)
	sun_mat.set_shader_parameter(&"true_px", view.px(SUN_KM / KM_PER_UNIT * UNIT))
	sun_mat.set_shader_parameter(&"alpha", 1.0 - px_a)
	get_node(^"Sun").visible = l <= -3.0 and px_a < 0.99
	sun_pixel.set_radius(maxf(SUN_KM / KM_PER_UNIT, SUN_ICON_PX / pps))
	sun_pixel.set_alpha(px_a)

	var a := Space.ramp(sys_px, 40.0, 180.0)
	set_alpha(&"orbits", a * Space.ramp(l, -12.5, -10.2))
	set_alpha(&"dust", a * Space.ramp(l, -13.5, -10.5))
	var pa := Space.ramp(sys_px, 140.0, 320.0)
	var sun_screen := screen(view)
	for p in planets:
		p.update(view, sun_screen, pa)

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
