class_name Level
extends Node3D
## Базовый класс уровня иерархии. У каждого уровня СВОИ единицы длины и своё
## пространство координат: так float32 не теряет точность при переходе
## от масштаба галактики к масштабу боя.

var title := ""
var units := ""          # что означает 1 единица на этом уровне (для HUD)
var min_h := 10.0        # пределы высоты камеры
var max_h := 1000.0
var default_h := 300.0   # высота, на которой уровень открывается
var bounds := 1000.0     # радиус панорамирования
var cam_near := 0.1
var cam_far := 10000.0

## Объекты, на которые можно навести курсор:
## {pos: Vector3, radius: float, name: String, enter: bool, seed: int}
var items: Array[Dictionary] = []

var _hover_ring: MeshInstance3D


func _ready() -> void:
	_hover_ring = Draw.circle(1.0, Color(0.6, 0.9, 1.0, 0.9), 64)
	_hover_ring.visible = false
	add_child(_hover_ring)


## Ближайший объект в пределах его радиуса или ~16 пикселей экрана.
func pick(ground: Vector3, wpp: float) -> Dictionary:
	var best := {}
	var best_d := INF
	for it in items:
		var d := Vector2(ground.x - it.pos.x, ground.z - it.pos.z).length()
		if d < maxf(it.radius * 1.3, 16.0 * wpp) and d < best_d:
			best = it
			best_d = d
	return best


func show_hover(it: Dictionary, wpp: float) -> void:
	_hover_ring.visible = not it.is_empty()
	if it.is_empty():
		return
	var r := maxf(it.radius * 1.6, 12.0 * wpp)
	_hover_ring.position = it.pos + Vector3(0.0, 0.01, 0.0)
	_hover_ring.scale = Vector3(r, 1.0, r)


## Вызывается каждый кадр: «семантический зум» — что показывать на данной высоте.
func on_camera(_height: float) -> void:
	pass


func add_item(pos: Vector3, radius: float, item_name: String, enter: bool, item_seed := 0) -> void:
	items.append({"pos": pos, "radius": radius, "name": item_name, "enter": enter, "seed": item_seed})


func add_background(rng_seed: int, depth: float, tint: Color) -> void:
	var sf := Starfield.new()
	add_child(sf)
	sf.build(rng_seed, bounds, max_h, depth, tint)


static func fade_labels(labels: Array, height: float, show_below: float, hide_above: float) -> void:
	var a := clampf(inverse_lerp(hide_above, show_below, height), 0.0, 1.0)
	for l: Label3D in labels:
		l.visible = a > 0.01
		l.modulate.a = a
		l.outline_modulate.a = a * 0.8
