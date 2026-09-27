class_name Planet
extends Space
## Планета — отдельное пространство с началом координат в её центре: при зуме
## к планете числа остаются маленькими, и точность не теряется.
## Рисуется как max(настоящий диск, иконка): издалека — шарик постоянных 5–11 px,
## вблизи — настоящая сфера, которая растёт вместе с зумом.

var title := ""
var lx := 0.0                 # положение в системе, ед. системы
var lz := 0.0
var true_r := 0.0             # настоящий радиус, ед. системы
var icon_px := 8.0
var _mesh: MeshInstance3D


func _init(sys: Space, lx_: float, lz_: float, radius_km: float, color: Color, title_: String) -> void:
	title = title_
	lx = lx_
	lz = lz_
	var p := sys.pos(lx, lz)
	bx = p[0]
	bz = p[1]
	ox = p[2]
	oz = p[3]
	unit = sys.unit
	top_level = true
	true_r = radius_km / StarSystem.KM_PER_UNIT
	icon_px = clampf(4.0 + radius_km / 9000.0, 5.0, 11.0)
	var sm := SphereMesh.new()
	sm.radius = 1.0
	sm.height = 2.0
	sm.radial_segments = 64
	sm.rings = 32
	_mesh = Draw.mesh_instance(sm, fade(&"body", Draw.lit(color)))
	add_child(_mesh)


func place(view: View) -> void:
	super.place(view)
	var r := display_r(view)
	_mesh.scale = Vector3.ONE * r
	_mesh.position.y = -r         # вершина касается плоскости: камера никогда не внутри


## Радиус, которым планета нарисована (ед. системы).
func display_r(view: View) -> float:
	return maxf(true_r, icon_px * 0.5 / view.px(unit))


func display_px(view: View) -> float:
	return display_r(view) * view.px(unit)
