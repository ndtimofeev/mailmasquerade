class_name FleetMarker
extends Space
## Значок сражения у планеты: два треугольника постоянного размера на экране.
## Стоит на орбите планеты, но не ближе 18 px к её иконке; при подлёте растворяется,
## и на его месте проявляются настоящие корабли.

var _holder := Node3D.new()
var _out := Vector2.RIGHT
var _true_off := 0.0          # настоящее расстояние от центра планеты, ед. системы


func _init(planet: Planet, out: Vector2, true_off: float) -> void:
	bx = planet.bx
	bz = planet.bz
	ox = planet.ox
	oz = planet.oz
	unit = planet.unit
	top_level = true
	_out = out
	_true_off = true_off
	_holder.rotation.y = -out.angle()
	add_child(_holder)
	var tri := CylinderMesh.new()
	tri.top_radius = 0.0
	tri.bottom_radius = 1.4
	tri.height = 2.6
	tri.radial_segments = 3
	for side in [-1.0, 1.0]:
		var mi := Draw.mesh_instance(tri, fade(&"marker", Draw.glowing(Battle.OURS if side < 0 else Battle.THEIRS, 2.5)))
		mi.rotation.x = PI / 2.0
		var arm := Node3D.new()
		arm.position = Vector3(0.0, 0.0, side * 2.2)
		arm.rotation.y = 0.0 if side < 0 else PI
		arm.add_child(mi)
		_holder.add_child(arm)


func place(view: View) -> void:
	super.place(view)
	var pps := view.px(unit)
	var off := maxf(_true_off, 18.0 / pps)
	_holder.position = Vector3(_out.x * off, 0.0, _out.y * off)
	_holder.scale = Vector3.ONE * (26.0 / (5.0 * pps))


func icon_screen(view: View) -> Vector2:
	var off := maxf(_true_off, 18.0 / view.px(unit))
	return view.screen(bx, bz, ox + unit * _out.x * off, oz + unit * _out.y * off)
