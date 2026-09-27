extends Node3D
## Оркестратор иерархического зума: галактика → система → бой.
##
## Каждый уровень — отдельное поддерево со своими единицами. Родительские уровни
## не удаляются, а прячутся (возврат мгновенный, камера встаёт над тем же объектом).
## Переход: камера «ныряет» в объект ниже своего минимума, кадр затемняется,
## уровни меняются, и новая камера «опускается» сверху со своей максимальной высоты.

const BG := Color(0.02, 0.03, 0.07)
const ACCENT := Color(0.62, 0.85, 1.0)
const DIVE_TIME := 0.55
const LAND_TIME := 0.9

var cam: TopDownCamera
var stack: Array[Dictionary] = []     # [{level, entered_from: Vector3}]
var busy := false
var hovered := {}

var _fade: ColorRect
var _crumbs: Label
var _meta: Label
var _hint: Label
var _tip: Label


func _ready() -> void:
	_setup_environment()
	cam = TopDownCamera.new()
	add_child(cam)
	cam.clicked.connect(_on_click)
	cam.push_in.connect(_on_push_in)
	cam.pull_out.connect(_go_up)
	_setup_hud()

	var galaxy := GalaxyLevel.new(1337)
	add_child(galaxy)
	stack.append({"level": galaxy, "entered_from": Vector3.ZERO})
	cam.configure(galaxy)
	cam.snap(Vector3.ZERO, galaxy.max_h)
	_update_crumbs()
	busy = true
	cam.input_enabled = false
	_land(galaxy.default_h)


func current() -> Level:
	return stack[-1].level


func _process(_delta: float) -> void:
	var lvl := current()
	lvl.on_camera(cam.height)
	var wpp := cam.world_per_pixel()
	var mouse := get_viewport().get_mouse_position()
	hovered = {} if busy else lvl.pick(cam.ground_at(mouse), wpp)
	lvl.show_hover(hovered, wpp)
	_tip.visible = not hovered.is_empty()
	if _tip.visible:
		_tip.text = hovered.name + ("   [клик — войти]" if hovered.enter else "")
		_tip.position = mouse + Vector2(18, 14)
	_meta.text = "%s\nвысота камеры: %.0f ед.  (%.0f–%.0f)" % [lvl.units, cam.height, lvl.min_h, lvl.max_h]


func _unhandled_input(event: InputEvent) -> void:
	if event.is_action_pressed(&"ui_cancel") or (event is InputEventKey and event.pressed and event.keycode == KEY_BACKSPACE):
		_go_up()


func _on_click(ground: Vector3) -> void:
	var it := current().pick(ground, cam.world_per_pixel())
	if not it.is_empty() and it.enter:
		_go_down(it)


## Колесо в упор: входим в объект под курсором, если он есть.
func _on_push_in(ground: Vector3) -> void:
	var it := current().pick(ground, cam.world_per_pixel() * 2.0)
	if not it.is_empty() and it.enter:
		_go_down(it)


func _go_down(item: Dictionary) -> void:
	if busy or stack.size() >= 3:
		return
	busy = true
	cam.input_enabled = false
	var parent := current()

	# 1. Ныряем в объект — ниже минимальной высоты, чтобы он заполнил кадр.
	var tw := create_tween().set_parallel()
	tw.tween_property(cam, "goal_target", item.pos, DIVE_TIME).set_trans(Tween.TRANS_SINE)
	tw.tween_property(cam, "goal_height", maxf(item.radius * 3.0, parent.min_h * 0.25), DIVE_TIME) \
		.set_trans(Tween.TRANS_QUAD).set_ease(Tween.EASE_IN)
	tw.tween_property(_fade, "color:a", 1.0, DIVE_TIME * 0.7).set_delay(DIVE_TIME * 0.3)
	await tw.finished

	# 2. Меняем уровень. Родителя не удаляем — прячем.
	var child: Level
	if stack.size() == 1:
		child = SystemLevel.new(item)
	else:
		child = BattleLevel.new(item)
	parent.visible = false
	parent.process_mode = Node.PROCESS_MODE_DISABLED
	add_child(child)
	stack.append({"level": child, "entered_from": item.pos})
	_update_crumbs()

	# 3. Новый уровень открывается «сверху» и камера опускается до рабочей высоты.
	cam.configure(child)
	cam.snap(Vector3.ZERO, child.max_h * 1.25)
	await _land(child.default_h)


func _go_up() -> void:
	if busy or stack.size() <= 1:
		return
	busy = true
	cam.input_enabled = false
	var leaving := current()

	# 1. Улетаем вверх за пределы уровня.
	var tw := create_tween().set_parallel()
	tw.tween_property(cam, "goal_height", leaving.max_h * 1.8, DIVE_TIME) \
		.set_trans(Tween.TRANS_QUAD).set_ease(Tween.EASE_IN)
	tw.tween_property(_fade, "color:a", 1.0, DIVE_TIME * 0.7).set_delay(DIVE_TIME * 0.3)
	await tw.finished

	# 2. Возвращаем родителя, камера — над тем объектом, из которого пришли.
	var from: Vector3 = stack[-1].entered_from
	stack.pop_back()
	leaving.queue_free()
	var parent := current()
	parent.visible = true
	parent.process_mode = Node.PROCESS_MODE_INHERIT
	_update_crumbs()
	cam.configure(parent)
	cam.snap(from, parent.min_h * 0.4)
	await _land(parent.min_h * 3.5)


func _land(h: float) -> void:
	var tw := create_tween().set_parallel()
	tw.tween_property(cam, "goal_height", h, LAND_TIME).set_trans(Tween.TRANS_CUBIC).set_ease(Tween.EASE_OUT)
	tw.tween_property(_fade, "color:a", 0.0, LAND_TIME * 0.6)
	await tw.finished
	cam.input_enabled = true
	busy = false


func _update_crumbs() -> void:
	var parts: PackedStringArray = []
	for s in stack:
		parts.append(s.level.title)
	_crumbs.text = "  ›  ".join(parts)


func _setup_environment() -> void:
	var env := Environment.new()
	env.background_mode = Environment.BG_COLOR
	env.background_color = BG
	env.ambient_light_source = Environment.AMBIENT_SOURCE_COLOR
	env.ambient_light_color = Color(0.25, 0.3, 0.45)
	env.ambient_light_energy = 0.35
	env.tonemap_mode = Environment.TONE_MAPPER_FILMIC
	env.glow_enabled = true
	env.glow_intensity = 0.9
	env.glow_bloom = 0.05
	env.glow_hdr_threshold = 0.9
	env.glow_blend_mode = Environment.GLOW_BLEND_MODE_ADDITIVE
	for i in 7:
		env.set_glow_level(i, 1.0 if i in [1, 2, 3, 4] else 0.0)
	var we := WorldEnvironment.new()
	we.environment = env
	add_child(we)


func _setup_hud() -> void:
	var hud := CanvasLayer.new()
	add_child(hud)

	_fade = ColorRect.new()
	_fade.color = Color(BG, 1.0)
	_fade.set_anchors_preset(Control.PRESET_FULL_RECT)
	_fade.mouse_filter = Control.MOUSE_FILTER_IGNORE
	hud.add_child(_fade)

	_crumbs = _hud_label(22, ACCENT)
	_crumbs.position = Vector2(24, 18)
	hud.add_child(_crumbs)

	_meta = _hud_label(14, Color(ACCENT, 0.6))
	_meta.position = Vector2(24, 54)
	hud.add_child(_meta)

	_hint = _hud_label(14, Color(ACCENT, 0.55))
	_hint.text = "Перетаскивание / WASD — панорама    Колесо — зум к курсору    " \
		+ "Клик по объекту или колесо в упор — войти    Esc или колесо наружу до упора — выйти"
	_hint.set_anchors_preset(Control.PRESET_BOTTOM_WIDE)
	_hint.offset_left = 24
	_hint.offset_top = -44
	_hint.offset_bottom = -16
	hud.add_child(_hint)

	_tip = _hud_label(15, Color.WHITE)
	_tip.visible = false
	hud.add_child(_tip)


func _hud_label(size: int, color: Color) -> Label:
	var l := Label.new()
	l.add_theme_font_size_override(&"font_size", size)
	l.add_theme_color_override(&"font_color", color)
	l.add_theme_color_override(&"font_outline_color", Color(0, 0, 0, 0.9))
	l.add_theme_constant_override(&"outline_size", 5)
	l.mouse_filter = Control.MOUSE_FILTER_IGNORE
	return l
