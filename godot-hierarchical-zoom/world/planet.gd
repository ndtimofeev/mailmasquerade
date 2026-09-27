class_name Planet
extends Space
## Планета — своё пространство с началом координат в центре (точность при глубоком зуме).
## Рисуется анимированной пиксельной планетой: max(настоящий диск, иконка 18–40 px).
## Свет — со стороны звезды, пересчитывается каждый кадр.

var title := ""
var kind := ""
var lx := 0.0                 # положение в системе, ед. системы
var lz := 0.0
var true_r := 0.0             # настоящий радиус, ед. системы
var icon_px := 24.0           # диаметр иконки на карте системы
var pixel: PixelBody


func _init(sys: Space, lx_: float, lz_: float, kind_: String, radius_km: float, icon: float,
		title_: String, body_seed: int, recolor: bool) -> void:
	title = title_
	kind = kind_
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
	icon_px = icon
	pixel = PixelBody.new(kind, body_seed, recolor)
	pixel.position.y = -0.001
	add_child(pixel)


func update(view: View, sun_screen: Vector2, alpha: float) -> void:
	place(view)
	pixel.set_radius(display_r(view))
	pixel.set_light_dir(sun_screen - screen(view))
	pixel.set_alpha(alpha)


func display_r(view: View) -> float:
	return maxf(true_r, icon_px * 0.5 / view.px(unit))


func display_px(view: View) -> float:
	return display_r(view) * view.px(unit)
