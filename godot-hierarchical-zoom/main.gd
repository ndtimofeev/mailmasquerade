extends Node3D
## Непрерывный иерархический зум: галактика ⊃ система ⊃ бой, без смены сцен.
##
## Камера неподвижна. Каждый кадр:
##   1) обновляем View (фокус и масштаб, в double);
##   2) каждое пространство ставим относительно камеры (Space.place);
##   3) по экранному размеру решаем, какие вложенные пространства создать, проявить,
##      погасить или удалить;
##   4) рисуем подписи и HUD в 2D поверх.

const BG := Color(0.004, 0.006, 0.014)
const INK := Color(0.62, 0.8, 0.92)
const WHEEL_STEP := 0.26                 # ln(1.3): одно деление колеса
const SMOOTH := 10.0
const FOCUS_LIMIT := 1300.0

var view := View.new()
var galaxy: Galaxy
var systems := {}                        # индекс звезды -> StarSystem

var goal_log_z := view.log_z
var _anchor := {}                        # {gx, gz, px}: точка, которая остаётся под курсором при зуме
var _drag := false
var _press := Vector2.ZERO
var _moved := false
var _flight := {}

var _labels: Array[Label] = []
var _label_i := 0
var _hud_crumbs: Label
var _hud_scale: Label
var _scale_bar: ColorRect
var _tip: Label


func _ready() -> void:
	_setup_environment()
	var cam := Camera3D.new()
	cam.position = Vector3(0.0, View.H, 0.0)
	cam.rotation.x = -PI / 2.0           # смотрит строго вниз
	cam.fov = View.FOV
	cam.near = 1.0
	cam.far = 3.0e6
	add_child(cam)

	add_child(_far_sky())
	galaxy = Galaxy.new(20240927)
	add_child(galaxy)
	_setup_hud()


# ---------------------------------------------------------------- кадр

func _process(delta: float) -> void:
	view.size = get_viewport().get_visible_rect().size
	_update_view(delta)

	galaxy.place(view)
	galaxy.update_sprites(view)
	var sys_px := view.px(StarSystem.RADIUS * StarSystem.UNIT)
	galaxy.set_alpha(&"detail", 1.0 - Space.ramp(sys_px, 260.0, 750.0))

	_update_systems(sys_px)
	_draw_labels(sys_px)
	_update_hud()


## Колесо двигает goal_log_z; фактический масштаб плавно догоняет цель,
## а фокус подстраивается так, чтобы точка под курсором не уезжала.
func _update_view(delta: float) -> void:
	if not _flight.is_empty():
		_fly_step(delta)
		return
	var keys := Input.get_axis(&"ui_page_up", &"ui_page_down") \
		+ float(Input.is_physical_key_pressed(KEY_E)) - float(Input.is_physical_key_pressed(KEY_Q))
	if keys != 0.0:
		goal_log_z += keys * 2.5 * delta
		_set_anchor(view.size * 0.5)
	goal_log_z = clampf(goal_log_z, log(View.Z_MIN), log(View.Z_MAX))
	view.log_z = lerpf(view.log_z, goal_log_z, 1.0 - exp(-SMOOTH * delta))
	if not _anchor.is_empty():
		view.anchor(_anchor.gx, _anchor.gz, _anchor.px)

	var pan := Vector2(
		float(Input.is_physical_key_pressed(KEY_D)) - float(Input.is_physical_key_pressed(KEY_A)),
		float(Input.is_physical_key_pressed(KEY_S)) - float(Input.is_physical_key_pressed(KEY_W)))
	if pan != Vector2.ZERO:
		_anchor = {}
		var step := 700.0 * delta * view.z() / view.px_per_unit()
		view.fx += pan.normalized().x * step
		view.fz += pan.normalized().y * step
	_clamp_focus()


func _clamp_focus() -> void:
	var d := sqrt(view.fx * view.fx + view.fz * view.fz)
	if d > FOCUS_LIMIT:
		view.fx *= FOCUS_LIMIT / d
		view.fz *= FOCUS_LIMIT / d


## Системы рождаются, когда их радиус на экране превышает ~50 px, и умирают ниже 40 px.
## Между 50 и 220 px они проявляются — поэтому момент создания не виден.
func _update_systems(sys_px: float) -> void:
	var margin := sys_px + 50.0
	var screen := Rect2(Vector2.ZERO, view.size).grow(margin)
	for i in galaxy.stars.size():
		var s: Dictionary = galaxy.stars[i]
		var on_screen := screen.has_point(view.to_screen(s.gx, s.gz))
		if sys_px > 50.0 and on_screen and not systems.has(i):
			systems[i] = StarSystem.new(s)
			add_child(systems[i])
		elif systems.has(i) and (sys_px < 40.0 or not on_screen):
			if systems[i].battle != null:
				systems[i].battle.queue_free()
			systems[i].queue_free()
			systems.erase(i)

	var bat_px := view.px(Battle.RADIUS * StarSystem.UNIT * Battle.UNIT_IN_SYSTEM)
	for sys: StarSystem in systems.values():
		var spot_g := sys.to_galaxy(sys.battle_spot.lx, sys.battle_spot.lz)
		var bat_on := Rect2(Vector2.ZERO, view.size).grow(bat_px + 50.0).has_point(view.to_screen(spot_g[0], spot_g[1]))
		if sys.battle == null and bat_px > 50.0 and bat_on:
			sys.battle = Battle.new(sys)
			add_child(sys.battle)
		elif sys.battle != null and (bat_px < 40.0 or not bat_on):
			sys.battle.queue_free()
			sys.battle = null

		var a := Space.ramp(sys_px, 50.0, 220.0)
		var b := Space.ramp(bat_px, 50.0, 220.0) if sys.battle != null else 0.0
		sys.place(view)
		sys.set_alpha(&"body", a)
		sys.set_alpha(&"dust", a)
		sys.set_alpha(&"orbits", a * (1.0 - 0.85 * b))
		sys.set_alpha(&"marker", a * (1.0 - b))
		if sys.battle != null:
			sys.battle.place(view)
			sys.battle.set_alpha(&"main", b)


# ---------------------------------------------------------------- ввод

func _unhandled_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		var mb := event as InputEventMouseButton
		if mb.pressed and mb.button_index in [MOUSE_BUTTON_WHEEL_UP, MOUSE_BUTTON_WHEEL_DOWN]:
			_flight = {}
			goal_log_z += -WHEEL_STEP if mb.button_index == MOUSE_BUTTON_WHEEL_UP else WHEEL_STEP
			_set_anchor(mb.position)
		elif mb.button_index == MOUSE_BUTTON_LEFT:
			if mb.pressed:
				_drag = true
				_moved = false
				_press = mb.position
			else:
				_drag = false
				if not _moved:
					var hit := _pick(mb.position)
					if not hit.is_empty():
						_fly_to(hit.gx, hit.gz, hit.fit)
	elif event is InputEventMouseMotion and _drag:
		var mm := event as InputEventMouseMotion
		if mm.position.distance_to(_press) > 4.0:
			_moved = true
		if _moved:
			_flight = {}
			_anchor = {}
			goal_log_z = view.log_z
			var k := view.z() / view.px_per_unit()
			view.fx -= mm.relative.x * k
			view.fz -= mm.relative.y * k
	elif event is InputEventMagnifyGesture:
		goal_log_z -= log((event as InputEventMagnifyGesture).factor)
		_set_anchor((event as InputEventMagnifyGesture).position)
	elif event.is_action_pressed(&"ui_cancel"):
		_zoom_out_level()


func _set_anchor(px: Vector2) -> void:
	var g := view.to_galaxy(px)
	_anchor = {"gx": g[0], "gz": g[1], "px": px}


## Что под курсором: сначала самые мелкие объекты (бой), потом системы, потом звёзды.
## Возвращает {name, gx, gz, fit}, где fit — масштаб, с которым к объекту лететь.
func _pick(p: Vector2) -> Dictionary:
	for sys: StarSystem in systems.values():
		if sys.battle != null and sys.battle.visible:
			for s in sys.battle.ships:
				var g := sys.battle.to_galaxy(s.node.position.x, s.node.position.z)
				if view.to_screen(g[0], g[1]).distance_to(p) < 16.0:
					return {"name": s.name, "gx": g[0], "gz": g[1], "fit": _fit(40.0 * sys.battle.unit)}
		var bs := sys.battle_spot
		var bg := sys.to_galaxy(bs.lx, bs.lz)
		if sys.visible and view.to_screen(bg[0], bg[1]).distance_to(p) < maxf(18.0, view.px(4.0 * sys.unit)):
			return {"name": bs.name, "gx": bg[0], "gz": bg[1], "fit": _fit(Battle.RADIUS * sys.unit * Battle.UNIT_IN_SYSTEM)}
		for pl in sys.planets:
			var pg := sys.to_galaxy(pl.lx, pl.lz)
			if sys.visible and view.to_screen(pg[0], pg[1]).distance_to(p) < maxf(14.0, view.px(pl.r * sys.unit)):
				return {"name": pl.name, "gx": pg[0], "gz": pg[1], "fit": _fit(pl.r * sys.unit * 4.0)}
	for s in galaxy.stars:
		if view.to_screen(s.gx, s.gz).distance_to(p) < 14.0:
			return {"name": s.name, "gx": s.gx, "gz": s.gz, "fit": _fit(StarSystem.RADIUS * StarSystem.UNIT)}
	return {}


## Масштаб, при котором радиус r (галактические единицы) занимает ~45% высоты экрана.
func _fit(r: float) -> float:
	return r * view.px_per_unit() / (0.45 * view.size.y)


## Esc: подняться на уровень выше тоже плавным полётом.
func _zoom_out_level() -> void:
	var sys_px := view.px(StarSystem.RADIUS * StarSystem.UNIT)
	var bat_px := view.px(Battle.RADIUS * StarSystem.UNIT * Battle.UNIT_IN_SYSTEM)
	for sys: StarSystem in systems.values():
		if bat_px > 150.0 and sys.battle != null:
			_fly_to(sys.ox, sys.oz, _fit(StarSystem.RADIUS * StarSystem.UNIT))
			return
	if sys_px > 150.0:
		_fly_to(view.fx, view.fz, _fit(260.0))
	else:
		_fly_to(0.0, 0.0, _fit(Galaxy.RADIUS))


# ---------------------------------------------------------------- полёт

## Полёт к точке: масштаб меняется линейно в логарифме, а фокус — пропорционально
## текущему масштабу, поэтому на экране движение равномерное (упрощённый Van Wijk–Nuij).
## Если цель далеко за краем экрана, по пути камера дополнительно «приподнимается».
func _fly_to(gx: float, gz: float, z1: float) -> void:
	var l1 := clampf(log(z1), log(View.Z_MIN), log(View.Z_MAX))
	var dist_px := view.to_screen(gx, gz).distance_to(view.size * 0.5)
	_flight = {
		"t": 0.0, "T": clampf(1.0 + 0.12 * absf(l1 - view.log_z), 1.0, 3.2),
		"x0": view.fx, "z0": view.fz, "x1": gx, "z1": gz, "l0": view.log_z, "l1": l1,
		"bump": maxf(0.0, log(dist_px / view.size.x) * 0.8),
	}
	_anchor = {}


func _fly_step(delta: float) -> void:
	var f := _flight
	f.t += delta
	var s := smoothstep(0.0, 1.0, f.t / f.T)
	var dl: float = f.l1 - f.l0
	var w: float = s if absf(dl) < 1e-3 else (exp(dl * s) - 1.0) / (exp(dl) - 1.0)
	view.log_z = lerpf(f.l0, f.l1, s) + f.bump * sin(PI * s)
	view.fx = lerpf(f.x0, f.x1, w)
	view.fz = lerpf(f.z0, f.z1, w)
	goal_log_z = view.log_z
	if f.t >= f.T:
		_flight = {}


# ---------------------------------------------------------------- подписи и HUD

func _draw_labels(sys_px: float) -> void:
	_label_i = 0
	var name_a := Space.ramp(view.px(Galaxy.SPACING), 70.0, 150.0)
	for i in galaxy.stars.size():
		var s: Dictionary = galaxy.stars[i]
		var a := name_a
		if systems.has(i):
			a = maxf(a, 1.0 - Space.ramp(view.px(Battle.RADIUS * StarSystem.UNIT * Battle.UNIT_IN_SYSTEM), 150.0, 400.0))
		var p := view.to_screen(s.gx, s.gz)
		var col: Color = Galaxy.FACTIONS[s.owner] if s.owner >= 0 else INK
		_label(p + Vector2(0, s.sprite.scale.x * view.px(1.0) * 0.28 + 4.0), s.name.to_upper(), col, a, 11)

	for sys: StarSystem in systems.values():
		var bat_px := view.px(Battle.RADIUS * sys.unit * Battle.UNIT_IN_SYSTEM)
		var pa := Space.ramp(sys_px, 260.0, 480.0) * (1.0 - Space.ramp(bat_px, 150.0, 400.0))
		for pl in sys.planets:
			var g := sys.to_galaxy(pl.lx, pl.lz)
			_label(view.to_screen(g[0], g[1]) + Vector2(0, view.px(pl.r * sys.unit) + 5.0), pl.name, INK, pa * 0.8, 11)
		var bg := sys.to_galaxy(sys.battle_spot.lx, sys.battle_spot.lz)
		var ma := Space.ramp(sys_px, 300.0, 600.0) * (1.0 - Space.ramp(bat_px, 50.0, 150.0))
		_label(view.to_screen(bg[0], bg[1]) + Vector2(0, -view.px(3.5 * sys.unit) - 18.0), sys.battle_spot.name.to_upper(), Battle.THEIRS, ma, 11)
		if sys.battle != null:
			var sa := Space.ramp(bat_px, 500.0, 900.0)
			for sh in sys.battle.ships:
				var g := sys.battle.to_galaxy(sh.node.position.x, sh.node.position.z)
				_label(view.to_screen(g[0], g[1]) + Vector2(0, 14), sh.name, Battle.OURS if sh.team == 0 else Battle.THEIRS, sa * 0.8, 10)

	for k in range(_label_i, _labels.size()):
		_labels[k].visible = false


## Пул 2D-подписей: переиспользуем Label между кадрами.
func _label(center: Vector2, text: String, color: Color, alpha: float, font: int) -> void:
	if alpha < 0.02 or not Rect2(Vector2(-200, -50), view.size + Vector2(400, 100)).has_point(center):
		return
	if _label_i == _labels.size():
		var l := Label.new()
		l.mouse_filter = Control.MOUSE_FILTER_IGNORE
		l.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
		l.add_theme_color_override(&"font_outline_color", Color(BG, 0.9))
		l.add_theme_constant_override(&"outline_size", 4)
		$Hud/Labels.add_child(l)
		_labels.append(l)
	var l := _labels[_label_i]
	_label_i += 1
	l.visible = true
	l.text = text
	l.add_theme_font_size_override(&"font_size", font)
	l.add_theme_color_override(&"font_color", Color(color, alpha))
	l.reset_size()
	l.position = center - Vector2(l.size.x * 0.5, 0.0)


func _update_hud() -> void:
	# Хлебные крошки — по тому, что сейчас реально заполняет экран.
	var crumbs := ["ГАЛАКТИКА"]
	var center := view.size * 0.5
	var best: StarSystem = null
	for sys: StarSystem in systems.values():
		if view.px(StarSystem.RADIUS * sys.unit) > 250.0 and \
				view.to_screen(sys.ox, sys.oz).distance_to(center) < view.px(StarSystem.RADIUS * sys.unit):
			best = sys
	var units := "св. лет"
	var per_g := 0.1                                  # 1 гал. ед. = 0,1 св. года
	if best != null:
		crumbs.append(best.star.name.to_upper())
		units = "а. е."
		per_g = 0.05 / StarSystem.UNIT                # 1 ед. системы = 0,05 а. е.
		if best.battle != null and view.px(Battle.RADIUS * best.battle.unit) > 250.0:
			crumbs.append(best.battle_spot.name.to_upper())
			units = "км"
			per_g = 10.0 / best.battle.unit           # 1 ед. боя = 10 км
	_hud_crumbs.text = "  /  ".join(crumbs)

	var bar_g := 120.0 * view.z() / view.px_per_unit()
	_hud_scale.text = "%s %s        увеличение ×%s" % [_nice(bar_g * per_g), units, _nice(View.Z_MAX / view.z())]

	var mouse := get_viewport().get_mouse_position()
	var hit: Dictionary = {} if _drag else _pick(mouse)
	_tip.visible = not hit.is_empty()
	if _tip.visible:
		_tip.text = hit.name
		_tip.position = mouse + Vector2(16, 12)


static func _nice(v: float) -> String:
	if v >= 1000.0:
		return _group(int(v))
	if v >= 100.0:
		return str(int(round(v)))
	if v >= 10.0:
		return String.num(v, 1)
	return String.num(v, 2)


static func _group(n: int) -> String:
	var s := str(n)
	var out := ""
	while s.length() > 3:
		out = " " + s.right(3) + out
		s = s.left(s.length() - 3)
	return s + out


# ---------------------------------------------------------------- окружение

## Небо «на бесконечности»: точки на огромной полусфере вокруг камеры, никогда не двигаются.
## Это самый дальний план — даёт черноту с редкими искрами, как в RC.
func _far_sky() -> MeshInstance3D:
	var rng := RandomNumberGenerator.new()
	rng.seed = 7
	var pos := PackedVector3Array()
	var col := PackedColorArray()
	for i in 1800:
		var d := Vector3(rng.randfn(), 0.0, rng.randfn()).normalized() * rng.randf_range(0.0, 0.8)
		d.y = -1.0
		pos.append(Vector3(0, View.H, 0) + d.normalized() * 2.0e6)
		col.append(Color(0.8, 0.85, 1.0, rng.randf_range(0.08, 0.35)))
	return Draw.points(pos, col, 1.0)


func _setup_environment() -> void:
	var env := Environment.new()
	env.background_mode = Environment.BG_COLOR
	env.background_color = BG
	env.ambient_light_source = Environment.AMBIENT_SOURCE_COLOR
	env.ambient_light_color = Color(0.3, 0.35, 0.5)
	env.ambient_light_energy = 0.06            # теневая сторона планет почти чёрная
	env.glow_enabled = true
	env.glow_intensity = 0.7
	env.glow_bloom = 0.0
	env.glow_hdr_threshold = 1.0
	env.glow_blend_mode = Environment.GLOW_BLEND_MODE_ADDITIVE
	for i in 7:
		env.set_glow_level(i, 1.0 if i in [1, 2, 3] else 0.0)
	var we := WorldEnvironment.new()
	we.environment = env
	add_child(we)


func _setup_hud() -> void:
	var hud := CanvasLayer.new()
	hud.name = "Hud"
	add_child(hud)
	var labels := Control.new()
	labels.name = "Labels"
	labels.mouse_filter = Control.MOUSE_FILTER_IGNORE
	hud.add_child(labels)

	_hud_crumbs = _hud_label(13, INK)
	_hud_crumbs.position = Vector2(28, 22)
	hud.add_child(_hud_crumbs)

	_scale_bar = ColorRect.new()
	_scale_bar.color = Color(INK, 0.6)
	_scale_bar.size = Vector2(120, 1)
	_scale_bar.position = Vector2(28, 52)
	hud.add_child(_scale_bar)
	_hud_scale = _hud_label(11, Color(INK, 0.6))
	_hud_scale.position = Vector2(28, 56)
	hud.add_child(_hud_scale)

	var help := _hud_label(11, Color(INK, 0.4))
	help.text = "КОЛЕСО / Q E — ЗУМ К КУРСОРУ     ПЕРЕТАСКИВАНИЕ / WASD — ПАНОРАМА     КЛИК — ЛЕТЕТЬ К ОБЪЕКТУ     ESC — УРОВЕНЬ ВЫШЕ"
	help.set_anchors_preset(Control.PRESET_BOTTOM_LEFT)
	help.offset_left = 28
	help.offset_top = -38
	hud.add_child(help)

	_tip = _hud_label(12, Color.WHITE)
	hud.add_child(_tip)


func _hud_label(size: int, color: Color) -> Label:
	var l := Label.new()
	l.add_theme_font_size_override(&"font_size", size)
	l.add_theme_color_override(&"font_color", color)
	l.mouse_filter = Control.MOUSE_FILTER_IGNORE
	return l
