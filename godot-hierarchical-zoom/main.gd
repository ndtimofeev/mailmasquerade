extends Node3D
## Непрерывный иерархический зум: карта 10 000 звёзд ⊃ система ⊃ планета/бой.
##
## Каждый кадр:
##   1) колесо/полёт двигают параметр u; масштаб ln z = profile.l_of(u); разрывы между
##      уровнями проходятся только автоматическим перелётом;
##   2) якорь View переносится на ближайшую звезду (плавающее начало координат);
##   3) система создаётся только у «своей» звезды и только в разрыве A;
##   4) все пространства ставятся относительно неподвижной камеры;
##   5) подписи расставляются по приоритету без наложений.

const BG := Color(0.004, 0.006, 0.014)
const INK := Color(0.62, 0.8, 0.92)
const SMOOTH := 9.0
const FOCUS_LIMIT := 3300.0
const MAX_LABELS := 45

var profile := ZoomProfile.new()
var view := View.new()
var galaxy: Galaxy
var dust := DustField.new()
var system: StarSystem = null
var context_star := -1              # звезда, к которой привязан якорь

var u := 0.0
var goal_u := 0.0
var _zoom := {}                     # {pos, px}: точка, которая держится под курсором при зуме
var _drag := false
var _press := Vector2.ZERO
var _moved := false
var _flight := {}
var _label_a := {}                  # звезда -> текущая прозрачность подписи (плавное появление)

var _labels: Array[Label] = []
var _label_i := 0
var _crumbs: Label
var _scale: Label
var _tip: Label
var _overlay: Control
var _hover := {}


func _ready() -> void:
	_setup_environment()
	var cam := Camera3D.new()
	cam.position = Vector3(0.0, View.H, 0.0)
	cam.rotation.x = -PI / 2.0
	cam.fov = View.FOV
	cam.near = 1.0
	cam.far = 3.0e6
	add_child(cam)
	add_child(_far_sky())
	add_child(dust)
	galaxy = Galaxy.new(20240927)
	add_child(galaxy)
	_setup_hud()
	u = profile.u_of(ZoomProfile.L_TOP - 0.3)
	goal_u = u


# ================================================================ кадр

func _process(delta: float) -> void:
	view.size = get_viewport().get_visible_rect().size
	_update_view(delta)
	_update_context()

	var l := view.log_z
	# Верхний предел размера иконки чуть растёт в системе: звезда там главный объект.
	var size_max := lerpf(7.0, 10.0, Space.ramp(-l, 3.0, 7.0))
	galaxy.place(view)
	galaxy.update(view, size_max, l > -3.0)
	galaxy.set_alpha(&"detail", Space.ramp(l, -2.2, 0.2))    # трассы и территории уходят в разрыве A
	dust.update(view)
	if system != null:
		system.manage_battle(view)
		system.update(view, view.px(1.0), size_max)

	_hover = {} if _drag else _pick(get_viewport().get_mouse_position())
	_draw_labels(size_max)
	_update_hud()
	_overlay.queue_redraw()


## Колесо двигает goal_u, u плавно догоняет; точка под курсором остаётся на месте.
func _update_view(delta: float) -> void:
	if not _flight.is_empty():
		_fly_step(delta)
		return
	var keys := float(Input.is_physical_key_pressed(KEY_E)) - float(Input.is_physical_key_pressed(KEY_Q))
	if keys != 0.0:
		goal_u += keys * 6.0 * delta
		_begin_zoom(view.size * 0.5, keys > 0.0)
		if not _flight.is_empty():
			return
	goal_u = clampf(goal_u, 0.0, profile.u_max())
	u = lerpf(u, goal_u, 1.0 - exp(-SMOOTH * delta))
	view.log_z = profile.l_of(u)
	if not _zoom.is_empty():
		view.put(_zoom.pos, _zoom.px)
		if absf(goal_u - u) < 0.002 and keys == 0.0:
			_zoom = {}

	var pan := Vector2(
		float(Input.is_physical_key_pressed(KEY_D)) - float(Input.is_physical_key_pressed(KEY_A)),
		float(Input.is_physical_key_pressed(KEY_S)) - float(Input.is_physical_key_pressed(KEY_W)))
	if pan != Vector2.ZERO:
		_zoom = {}
		var step := 700.0 * delta * view.z() / view.px_per_unit()
		view.dx += pan.normalized().x * step
		view.dz += pan.normalized().y * step
	_clamp_focus()


func _clamp_focus() -> void:
	var f := view.focus_abs()
	if f.length() > FOCUS_LIMIT:
		var k := FOCUS_LIMIT / f.length()
		view.dx = f.x * k - view.ax
		view.dz = f.y * k - view.az


## Зум колесом в точке p (goal_u уже изменён).
## Разрывы между уровнями колесом не проходятся. Когда колесо упирается в край разрыва,
## включается автоматический перелёт: к звезде под курсором — пока её система не заполнит
## экран; к бою или планете — пока они не заполнят экран. Обратно — так же, одним перелётом
## на уровень выше. Если цели под курсором нет, зум просто останавливается у края разрыва.
func _begin_zoom(p: Vector2, zoom_in: bool) -> void:
	var l := view.log_z
	var gl := profile.l_of(goal_u)
	_zoom = {"pos": view.pos_at(p), "px": p}
	for g: Vector2 in [ZoomProfile.GAP_A, ZoomProfile.GAP_B]:
		var inside := l < g.x - 0.3 and l > g.y + 0.3
		if zoom_in and (inside or (l > g.x - 0.3 and l > g.y and gl < g.x)):
			var target := _attractor(p, g)
			if not target.is_empty() and target.l < l - 0.5:
				_fly_to(target.pos, target.l, 2.6)
				return
			if not inside:
				goal_u = minf(goal_u, profile.u_of(g.x))
				return
		if not zoom_in and (inside or (l < g.y + 0.3 and l < g.x and gl > g.y)):
			if g == ZoomProfile.GAP_B and system != null:
				_fly_to(system.pos(0, 0), log(_fit(StarSystem.RADIUS * StarSystem.UNIT)), 2.0)
				return
			if g == ZoomProfile.GAP_A and context_star >= 0:
				_fly_to([galaxy.gx[context_star], galaxy.gz[context_star], 0.0, 0.0], g.x + 0.9, 2.0)
				return


## Цель перелёта через разрыв рядом с курсором: {pos, l} — куда и с каким масштабом лететь.
## Разрыв A — звезда (лететь, пока система не заполнит экран);
## разрыв B — бой, планета или звезда системы.
func _attractor(p: Vector2, gap: Vector2) -> Dictionary:
	var radius := view.size.y * 0.35
	if gap == ZoomProfile.GAP_A:
		var g := view.pos_at(p)
		var i := galaxy.nearest(g[0] + g[2], g[1] + g[3], radius * view.z() / view.px_per_unit())
		if i < 0:
			return {}
		return {"pos": [galaxy.gx[i], galaxy.gz[i], 0.0, 0.0], "l": log(_fit(StarSystem.RADIUS * StarSystem.UNIT))}
	if system == null:
		return {}
	var u_s := StarSystem.UNIT
	var cands: Array = [[system.pos(0, 0), log(_fit(StarSystem.SUN_KM / StarSystem.KM_PER_UNIT * u_s * 3.0))]]
	for pl in system.planets:
		cands.append([pl.pos(0, 0), log(_fit(pl.true_r * u_s * 4.0))])
	cands.append([system.battle_spot.pos, log(_fit(Battle.RADIUS * 0.55 * Battle.KM / StarSystem.KM_PER_UNIT * u_s))])
	var best := {}
	var best_d := radius
	for c in cands:
		var d := view.screen(c[0][0], c[0][1], c[0][2], c[0][3]).distance_to(p)
		if d < best_d:
			best_d = d
			best = {"pos": c[0], "l": c[1]}
	return best


## Якорь — ближайшая к фокусу звезда. Система — только у неё и только глубже ln z = -2.
func _update_context() -> void:
	var l := view.log_z
	if l < ZoomProfile.GAP_A.x + 0.5:
		var f := [view.ax + view.dx, view.az + view.dz]
		var i := galaxy.nearest(f[0], f[1], maxf(200.0, view.z() * 40.0))
		if i >= 0 and i != context_star and (system == null or l > -2.5):
			context_star = i
			view.rebase(galaxy.gx[i], galaxy.gz[i])   # точки в _zoom/_flight хранят базу явно
	if system != null and (l > -1.5 or system.star != context_star):
		system.queue_free()
		system = null
	if system == null and context_star >= 0 and l < -2.0:
		system = StarSystem.new(galaxy, context_star)
		add_child(system)


# ================================================================ ввод

func _unhandled_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		var mb := event as InputEventMouseButton
		if mb.pressed and mb.button_index in [MOUSE_BUTTON_WHEEL_UP, MOUSE_BUTTON_WHEEL_DOWN]:
			if not _flight.is_empty():
				return                      # во время перелёта колесо не мешает анимации
			var zoom_in := mb.button_index == MOUSE_BUTTON_WHEEL_UP
			goal_u += 1.0 if zoom_in else -1.0
			_begin_zoom(mb.position, zoom_in)
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
						_fly_to(hit.pos, hit.l)
	elif event is InputEventMouseMotion and _drag:
		var mm := event as InputEventMouseMotion
		if mm.position.distance_to(_press) > 4.0:
			_moved = true
		if _moved:
			_flight = {}
			_zoom = {}
			goal_u = u
			var k := view.z() / view.px_per_unit()
			view.dx -= mm.relative.x * k
			view.dz -= mm.relative.y * k
	elif event.is_action_pressed(&"ui_cancel"):
		_zoom_out_level()


## Что под курсором. Возвращает {title, pos, l, r_px, screen}: l — масштаб, с которым к нему лететь.
func _pick(p: Vector2) -> Dictionary:
	var l := view.log_z
	if system != null and l < -5.0:
		if system.battle != null:
			for s in system.battle.ships:
				var sp: Vector2 = system.battle.screen(view, s.node.position.x, s.node.position.z)
				if sp.distance_to(p) < 16.0:
					return {"title": s.title, "pos": system.battle.pos(s.node.position.x, s.node.position.z),
						"l": log(_fit(60.0 * system.battle.unit)), "r_px": 12.0, "screen": sp}
		var ms := system.marker.icon_screen(view)
		if ms.distance_to(p) < 16.0 and (system.battle == null or not system.battle.visible or l > -19.5):
			return {"title": system.battle_spot.title, "pos": system.battle_spot.pos,
				"l": log(_fit(Battle.RADIUS * 0.55 * StarSystem.UNIT * Battle.KM / StarSystem.KM_PER_UNIT)), "r_px": 16.0, "screen": ms}
		for pl in system.planets:
			var sp := pl.screen(view)
			var r := pl.display_px(view)
			if sp.distance_to(p) < maxf(10.0, r):
				return {"title": pl.title, "pos": pl.pos(0, 0), "l": log(_fit(pl.true_r * pl.unit * 4.0)), "r_px": r + 4.0, "screen": sp}
		var ss := system.screen(view)
		if ss.distance_to(p) < 14.0:
			return {"title": system.title, "pos": system.pos(0, 0), "l": log(_fit(StarSystem.RADIUS * StarSystem.UNIT)), "r_px": 12.0, "screen": ss}
		return {}
	var g := view.pos_at(p)
	var i := galaxy.nearest(g[0] + g[2], g[1] + g[3], 14.0 * view.z() / view.px_per_unit())
	if i < 0:
		return {}
	var sp := view.screen(galaxy.gx[i], galaxy.gz[i])
	var r := StarIcons.size_px(galaxy.nn[i], galaxy.mag[i], view.px(1.0), 7.0) * 0.5 + 5.0
	return {"title": galaxy.names[i], "pos": [galaxy.gx[i], galaxy.gz[i], 0.0, 0.0],
		"l": log(_fit(StarSystem.RADIUS * StarSystem.UNIT)), "r_px": r, "screen": sp}


func _fit(r_g: float) -> float:
	return r_g * view.px_per_unit() / (0.42 * view.size.y)


func _zoom_out_level() -> void:
	var l := view.log_z
	if l < ZoomProfile.GAP_B.x - 0.5 and system != null:
		_fly_to(system.pos(0, 0), log(_fit(StarSystem.RADIUS * StarSystem.UNIT)), 2.0)
	elif l < ZoomProfile.GAP_A.x - 0.5 and context_star >= 0:
		_fly_to([galaxy.gx[context_star], galaxy.gz[context_star], 0.0, 0.0], ZoomProfile.GAP_A.x + 0.9, 2.0)
	else:
		_fly_to([0.0, 0.0, 0.0, 0.0], ZoomProfile.L_TOP - 0.3)


# ================================================================ полёт

## Полёт линеен по u (а не по ln z) — поэтому разрывы пролетаются так же быстро, как колесом.
## Фокус движется пропорционально текущему масштабу: на экране скорость равномерная.
func _fly_to(target: Array, l1: float, duration := -1.0) -> void:
	l1 = clampf(l1, ZoomProfile.L_BOTTOM, ZoomProfile.L_TOP)
	var u1 := profile.u_of(l1)
	var start := view.pos_at(view.size * 0.5)
	var dist_px := view.screen(target[0], target[1], target[2], target[3]).distance_to(view.size * 0.5)
	var bump := maxf(0.0, log(dist_px / view.size.x) / ZoomProfile.STEP * 0.8)
	# веса движения фокуса: интеграл z(s) ds, нормированный
	var w := PackedFloat64Array([0.0])
	var n := 64
	for k in n:
		var s := (k + 0.5) / n
		var us := lerpf(u, u1, s) - bump * sin(PI * s)
		w.append(w[k] + exp(profile.l_of(us)))
	for k in w.size():
		w[k] /= w[n]
	var t := duration if duration > 0.0 else clampf(0.9 + absf(u1 - u) * 0.05 + bump * 0.05, 0.9, 3.0)
	_flight = {"t": 0.0, "T": t,
		"u0": u, "u1": u1, "bump": bump, "w": w, "from": start, "to": target}
	_zoom = {}


func _fly_step(delta: float) -> void:
	var f := _flight
	f.t += delta
	var s := smoothstep(0.0, 1.0, minf(f.t / f.T, 1.0))
	u = lerpf(f.u0, f.u1, s) - f.bump * sin(PI * s)
	goal_u = u
	view.log_z = profile.l_of(u)
	var wi: float = s * (f.w.size() - 1)
	var k := mini(int(wi), f.w.size() - 2)
	var w: float = lerpf(f.w[k], f.w[k + 1], wi - k)
	var a: Array = f.from
	var b: Array = f.to
	# интерполяция в координатах текущего якоря, в double
	var ax_: float = (a[0] - view.ax) + a[2]
	var az_: float = (a[1] - view.az) + a[3]
	var bx_: float = (b[0] - view.ax) + b[2]
	var bz_: float = (b[1] - view.az) + b[3]
	view.dx = lerpf(ax_, bx_, w)
	view.dz = lerpf(az_, bz_, w)
	if f.t >= f.T:
		_flight = {}


# ================================================================ подписи и HUD

## Подписи звёзд: по убыванию важности, пропуская те, что налезают на уже поставленные.
## Появление/исчезновение плавное, так что при зуме подписи не мигают.
func _draw_labels(size_max: float) -> void:
	_label_i = 0
	var l := view.log_z
	var ppg := view.px(1.0)
	var screen := Rect2(Vector2(-40, -20), view.size + Vector2(80, 40))
	var taken := {}
	var placed := {}
	if l > ZoomProfile.GAP_A.y + 1.0:
		# Кандидаты: при сильном зуме — только видимые (через сетку), иначе все по важности.
		var f := view.focus_abs()
		var half := view.size * 0.6 * view.z() / view.px_per_unit()
		var cand := galaxy.query(Rect2(f - half, half * 2.0))
		if cand.size() < 3000:
			var arr := Array(cand)
			arr.sort_custom(func(a, b): return galaxy.mag[a] > galaxy.mag[b])
			cand = PackedInt32Array(arr)
		else:
			cand = galaxy.by_mag
		var checked := 0
		for i in cand:
			if placed.size() >= MAX_LABELS or checked > 3000:
				break
			var sp := view.screen(galaxy.gx[i], galaxy.gz[i])
			if not screen.has_point(sp):
				continue
			checked += 1
			var s := StarIcons.size_px(galaxy.nn[i], galaxy.mag[i], ppg, size_max)
			if s < 3.2 and i != context_star:
				continue
			var at := sp + Vector2(0, s * 0.5 + 5.0)
			if _free(taken, at, galaxy.names[i].length() * 7.0):
				placed[i] = at
	for i in _label_a.keys() + placed.keys():
		var target := 1.0 if placed.has(i) else 0.0
		_label_a[i] = move_toward(_label_a.get(i, 0.0), target, get_process_delta_time() * 4.0)
		if _label_a[i] <= 0.0:
			_label_a.erase(i)
			continue
		var s := StarIcons.size_px(galaxy.nn[i], galaxy.mag[i], ppg, size_max)
		var at := view.screen(galaxy.gx[i], galaxy.gz[i]) + Vector2(0, s * 0.5 + 5.0)
		var big: bool = galaxy.mag[i] > 0.5
		var col: Color = Galaxy.FACTIONS[galaxy.faction[i]] if galaxy.faction[i] >= 0 else INK
		var a: float = _label_a[i] * (1.0 if big else 0.6) * Space.ramp(l, ZoomProfile.GAP_A.y + 1.0, ZoomProfile.GAP_A.y + 2.0)
		_label(at, galaxy.names[i].to_upper() if big else galaxy.names[i], col, a, 11 if big else 9)

	if system != null:
		var sys_px := view.px(StarSystem.RADIUS * StarSystem.UNIT)
		var sun_a := Space.ramp(-l, 6.8, 7.8) * Space.ramp(l, ZoomProfile.GAP_B.x - 1.5, ZoomProfile.GAP_B.x)
		var sun_col: Color = Galaxy.FACTIONS[galaxy.faction[system.star]] if galaxy.faction[system.star] >= 0 else INK
		_label(system.screen(view) + Vector2(0, StarSystem.SUN_ICON_PX + 6.0), system.title.to_upper(), sun_col, sun_a, 12)
		for pl in system.planets:
			var orbit_px := view.px(Vector2(pl.lx, pl.lz).length() * pl.unit)
			var a := Space.ramp(sys_px, 220.0, 420.0) * Space.ramp(orbit_px, 30.0, 60.0)
			_label(pl.screen(view) + Vector2(0, pl.display_px(view) + 5.0), pl.title, INK, a * 0.85, 11)
		var bl := Space.ramp(view.log_z, -21.0, -19.5) * Space.ramp(-view.log_z, 9.0, 12.0)
		_label(system.marker.icon_screen(view) + Vector2(0, -26), system.battle_spot.title.to_upper(), Battle.THEIRS, bl, 10)
		if system.battle != null:
			var sa := Space.ramp(-view.log_z, 22.5, 23.5)
			for s in system.battle.ships:
				var sp: Vector2 = system.battle.screen(view, s.node.position.x, s.node.position.z)
				_label(sp + Vector2(0, 12), s.title, Battle.OURS if s.team == 0 else Battle.THEIRS, sa * 0.8, 10)

	for k in range(_label_i, _labels.size()):
		_labels[k].visible = false


## Грубая сетка занятости 24×14 px: подпись ставится, только если её ячейки свободны.
func _free(taken: Dictionary, at: Vector2, width: float) -> bool:
	var c0 := Vector2i(floori((at.x - width * 0.5) / 24.0), floori(at.y / 14.0))
	var c1 := Vector2i(floori((at.x + width * 0.5) / 24.0), c0.y)
	for x in range(c0.x, c1.x + 1):
		if taken.has(Vector2i(x, c0.y)):
			return false
	for x in range(c0.x, c1.x + 1):
		taken[Vector2i(x, c0.y)] = true
	return true


func _label(center: Vector2, text: String, color: Color, alpha: float, font: int) -> void:
	if alpha < 0.02 or not Rect2(Vector2(-200, -50), view.size + Vector2(400, 100)).has_point(center):
		return
	if _label_i == _labels.size():
		var nl := Label.new()
		nl.mouse_filter = Control.MOUSE_FILTER_IGNORE
		nl.add_theme_color_override(&"font_outline_color", Color(BG, 0.9))
		nl.add_theme_constant_override(&"outline_size", 4)
		$Hud/Labels.add_child(nl)
		_labels.append(nl)
	var lb := _labels[_label_i]
	_label_i += 1
	lb.visible = true
	lb.text = text
	lb.add_theme_font_size_override(&"font_size", font)
	lb.add_theme_color_override(&"font_color", Color(color, alpha))
	lb.reset_size()
	lb.position = center - Vector2(lb.size.x * 0.5, 0.0)


func _update_hud() -> void:
	var l := view.log_z
	var crumbs := ["ГАЛАКТИКА"]
	if system != null and l < ZoomProfile.GAP_A.y + 1.0:
		crumbs.append(system.title.to_upper())
		if l < ZoomProfile.GAP_B.x - 3.0:
			var near_battle := system.battle != null and system.battle.visible
			crumbs.append(system.battle_spot.title.to_upper() if near_battle else _nearest_planet_title())
	elif context_star >= 0 and l < ZoomProfile.GAP_A.x:
		crumbs.append(galaxy.names[context_star].to_upper())
	_crumbs.text = "  /  ".join(crumbs)

	# Линейка: одна и та же физика на всех уровнях (1 гал. ед. = 0,1 св. года).
	var ly := 120.0 * view.z() / view.px_per_unit() * 0.1
	var text: String
	if ly >= 0.05:
		text = "%s св. лет" % _nice(ly)
	elif ly * 63241.0 >= 0.02:
		text = "%s а.е." % _nice(ly * 63241.0)
	else:
		text = "%s км" % _nice(ly * 63241.0 * 1.496e8)
	_scale.text = "%s          увеличение ×%s" % [text, _sci(exp(ZoomProfile.L_TOP - l))]

	_tip.visible = not _hover.is_empty()
	if _tip.visible:
		_tip.text = _hover.title
		_tip.position = get_viewport().get_mouse_position() + Vector2(16, 12)


func _nearest_planet_title() -> String:
	var best := ""
	var best_d := INF
	for pl in system.planets:
		var d := pl.screen(view).distance_to(view.size * 0.5)
		if d < best_d:
			best_d = d
			best = pl.title.to_upper()
	return best


static func _nice(v: float) -> String:
	if v >= 1000.0:
		var s := str(int(v))
		var out := ""
		while s.length() > 3:
			out = " " + s.right(3) + out
			s = s.left(s.length() - 3)
		return s + out
	if v >= 100.0:
		return str(int(round(v)))
	if v >= 10.0:
		return String.num(v, 1)
	return String.num(v, 2)


static func _sci(v: float) -> String:
	if v < 10000.0:
		return _nice(v)
	var e := floori(log(v) / log(10.0))
	const SUP := "⁰¹²³⁴⁵⁶⁷⁸⁹"
	var es := ""
	for ch in str(e):
		es += SUP[int(ch)]
	return "%s·10%s" % [String.num(v / pow(10.0, e), 1), es]


## Альтиметр справа: вся шкала u с зонами. Разрывы короткие — их пролетают за 2–3 деления.
func _draw_overlay() -> void:
	var o := _overlay
	var font := ThemeDB.fallback_font
	var top := 110.0
	var h := minf(view.size.y - 220.0, 420.0)
	var x := view.size.x - 36.0
	var um := profile.u_max()
	var zones := [
		[ZoomProfile.L_TOP, ZoomProfile.GAP_A.x, "КАРТА", 1.0],
		[ZoomProfile.GAP_A.x, ZoomProfile.GAP_A.y, "", 0.25],
		[ZoomProfile.GAP_A.y, ZoomProfile.GAP_B.x, "СИСТЕМА", 1.0],
		[ZoomProfile.GAP_B.x, ZoomProfile.GAP_B.y, "", 0.25],
		[ZoomProfile.GAP_B.y, ZoomProfile.L_BOTTOM, "ПЛАНЕТА / БОЙ", 1.0],
	]
	for zn in zones:
		var y0: float = top + profile.u_of(zn[0]) / um * h
		var y1: float = top + profile.u_of(zn[1]) / um * h
		o.draw_line(Vector2(x, y0 + 2), Vector2(x, y1 - 2), Color(INK, 0.35 * zn[3] + 0.1), 2.0)
		if zn[2] != "":
			o.draw_string(font, Vector2(x - 12 - font.get_string_size(zn[2], HORIZONTAL_ALIGNMENT_LEFT, -1, 10).x, (y0 + y1) * 0.5 + 4),
				zn[2], HORIZONTAL_ALIGNMENT_LEFT, -1, 10, Color(INK, 0.45))
	var yc := top + u / um * h
	o.draw_line(Vector2(x - 7, yc), Vector2(x + 7, yc), INK, 2.0)
	if not _hover.is_empty():
		o.draw_arc(_hover.screen, _hover.r_px, 0.0, TAU, 48, Color(INK, 0.8), 1.0, true)


# ================================================================ окружение

func _far_sky() -> MeshInstance3D:
	var rng := RandomNumberGenerator.new()
	rng.seed = 7
	var p := PackedVector3Array()
	var col := PackedColorArray()
	for i in 1800:
		var d := Vector3(rng.randfn(), 0.0, rng.randfn()).normalized() * rng.randf_range(0.0, 0.8)
		d.y = -1.0
		p.append(Vector3(0, View.H, 0) + d.normalized() * 2.0e6)
		col.append(Color(0.8, 0.85, 1.0, rng.randf_range(0.06, 0.3)))
	return Draw.points(p, col, 1.0)


func _setup_environment() -> void:
	var env := Environment.new()
	env.background_mode = Environment.BG_COLOR
	env.background_color = BG
	env.ambient_light_source = Environment.AMBIENT_SOURCE_COLOR
	env.ambient_light_color = Color(0.3, 0.35, 0.5)
	env.ambient_light_energy = 0.06
	env.glow_enabled = true
	env.glow_intensity = 0.6
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
	_overlay = Control.new()
	_overlay.mouse_filter = Control.MOUSE_FILTER_IGNORE
	_overlay.set_anchors_preset(Control.PRESET_FULL_RECT)
	_overlay.draw.connect(_draw_overlay)
	hud.add_child(_overlay)

	_crumbs = _hud_label(13, INK)
	_crumbs.position = Vector2(28, 22)
	hud.add_child(_crumbs)
	var bar := ColorRect.new()
	bar.color = Color(INK, 0.6)
	bar.size = Vector2(120, 1)
	bar.position = Vector2(28, 52)
	hud.add_child(bar)
	_scale = _hud_label(11, Color(INK, 0.6))
	_scale.position = Vector2(28, 56)
	hud.add_child(_scale)

	var help := _hud_label(11, Color(INK, 0.4))
	help.text = "КОЛЕСО / Q E — ЗУМ     ПЕРЕТАСКИВАНИЕ / WASD — ПАНОРАМА     КЛИК — ЛЕТЕТЬ К ОБЪЕКТУ     ESC — УРОВЕНЬ ВЫШЕ"
	help.set_anchors_preset(Control.PRESET_BOTTOM_LEFT)
	help.offset_left = 28
	help.offset_top = -38
	hud.add_child(help)
	_tip = _hud_label(12, Color.WHITE)
	hud.add_child(_tip)


func _hud_label(size: int, color: Color) -> Label:
	var lb := Label.new()
	lb.add_theme_font_size_override(&"font_size", size)
	lb.add_theme_color_override(&"font_color", color)
	lb.mouse_filter = Control.MOUSE_FILTER_IGNORE
	return lb
