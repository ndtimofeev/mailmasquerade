class_name TopDownCamera
extends Camera3D
## Перспективная камера, смотрящая строго вниз (перпендикулярно плоскости y = 0).
## Узкий FOV: сама карта выглядит почти плоской, а фон под ней даёт параллакс.

signal clicked(ground: Vector3)
## Колесо «внутрь», когда камера уже на минимальной высоте.
signal push_in(ground: Vector3)
## Колесо «наружу», когда камера уже на максимальной высоте.
signal pull_out

const FOV_DEG := 32.0
const ZOOM_STEP := 1.18
const SMOOTH := 12.0
const DRAG_THRESHOLD_PX := 5.0

var target := Vector3.ZERO        # точка на плоскости под центром экрана
var height := 1000.0
var goal_target := Vector3.ZERO
var goal_height := 1000.0

var min_height := 10.0
var max_height := 1000.0
var bounds := 1000.0
var input_enabled := true

var _press_pos := Vector2.ZERO
var _pressed := false
var _dragging := false


func _ready() -> void:
	fov = FOV_DEG
	rotation = Vector3(-PI / 2.0, 0.0, 0.0)   # -Z камеры смотрит в -Y мира; «верх» экрана = -Z мира


func configure(level: Level) -> void:
	min_height = level.min_h
	max_height = level.max_h
	bounds = level.bounds
	near = level.cam_near
	far = level.cam_far


func snap(t: Vector3, h: float) -> void:
	target = t
	goal_target = t
	height = h
	goal_height = h
	_apply()


func world_per_pixel() -> float:
	var vp_h := get_viewport().get_visible_rect().size.y
	return 2.0 * height * tan(deg_to_rad(fov) * 0.5) / vp_h


func ground_at(screen: Vector2) -> Vector3:
	var hit = Plane(Vector3.UP, 0.0).intersects_ray(project_ray_origin(screen), project_ray_normal(screen))
	return hit if hit != null else target


func _process(delta: float) -> void:
	if input_enabled:
		var move := Vector2(
			Input.get_axis(&"ui_left", &"ui_right") + _key_axis(KEY_A, KEY_D),
			Input.get_axis(&"ui_up", &"ui_down") + _key_axis(KEY_W, KEY_S))
		if move != Vector2.ZERO:
			var speed := height * 1.2
			goal_target += Vector3(move.x, 0.0, move.y).limit_length(1.0) * speed * delta
			_clamp_goal()
	var k := 1.0 - exp(-SMOOTH * delta)
	target = target.lerp(goal_target, k)
	height = lerpf(height, goal_height, k)
	_apply()


func _key_axis(neg: Key, pos: Key) -> float:
	return float(Input.is_physical_key_pressed(pos)) - float(Input.is_physical_key_pressed(neg))


func _apply() -> void:
	position = Vector3(target.x, height, target.z)


func _unhandled_input(event: InputEvent) -> void:
	if not input_enabled:
		return
	if event is InputEventMouseButton:
		var mb := event as InputEventMouseButton
		match mb.button_index:
			MOUSE_BUTTON_WHEEL_UP:
				if mb.pressed:
					_zoom(1.0 / ZOOM_STEP, mb.position)
			MOUSE_BUTTON_WHEEL_DOWN:
				if mb.pressed:
					_zoom(ZOOM_STEP, mb.position)
			MOUSE_BUTTON_LEFT, MOUSE_BUTTON_RIGHT, MOUSE_BUTTON_MIDDLE:
				if mb.pressed:
					_pressed = true
					_dragging = false
					_press_pos = mb.position
				else:
					if _pressed and not _dragging and mb.button_index == MOUSE_BUTTON_LEFT:
						clicked.emit(ground_at(mb.position))
					_pressed = false
					_dragging = false
	elif event is InputEventMouseMotion and _pressed:
		var mm := event as InputEventMouseMotion
		if not _dragging and mm.position.distance_to(_press_pos) > DRAG_THRESHOLD_PX:
			_dragging = true
		if _dragging:
			var wpp := 2.0 * goal_height * tan(deg_to_rad(fov) * 0.5) / get_viewport().get_visible_rect().size.y
			goal_target -= Vector3(mm.relative.x, 0.0, mm.relative.y) * wpp
			_clamp_goal()
	elif event is InputEventMagnifyGesture:
		var g := event as InputEventMagnifyGesture
		_zoom(1.0 / g.factor, g.position)
	elif event is InputEventPanGesture:
		var p := event as InputEventPanGesture
		_zoom(pow(ZOOM_STEP, p.delta.y * 0.5), p.position)


## Зум к курсору: точка под курсором остаётся на месте.
func _zoom(factor: float, screen: Vector2) -> void:
	var cursor := ground_at(screen)
	if factor < 1.0 and goal_height <= min_height * 1.001:
		push_in.emit(cursor)
		return
	if factor > 1.0 and goal_height >= max_height * 0.999:
		pull_out.emit()
		return
	var new_h := clampf(goal_height * factor, min_height, max_height)
	var ratio := new_h / goal_height
	goal_target = cursor + (goal_target - cursor) * ratio
	goal_height = new_h
	_clamp_goal()


func _clamp_goal() -> void:
	var flat := Vector2(goal_target.x, goal_target.z).limit_length(bounds)
	goal_target = Vector3(flat.x, 0.0, flat.y)
