class_name View
extends RefCounted
## Состояние камеры: где фокус и какой масштаб.
##
## Камера неподвижна, двигается мир. Диапазон зума ~10^12, а double даёт ~16 знаков,
## поэтому фокус хранится относительно ЯКОРЯ (плавающее начало координат):
## якорь (ax, az) — координаты ближайшей звезды, фокус (dx, dz) — маленькое смещение от неё.
## Любая точка мира задаётся парой «база + смещение» [bx, bz, ox, oz]: база — точные
## координаты звезды, смещение — малая величина внутри системы. Тогда при вычитании
## (bx - ax) даёт ровно 0, и точность не теряется даже на масштабе корабля.

const H := 100.0           # высота камеры над плоскостью
const FOV := 30.0

var ax := 0.0              # якорь, галактические единицы
var az := 0.0
var dx := 0.0              # фокус относительно якоря
var dz := 0.0
var log_z := ZoomProfile.L_TOP   # ln(галактических единиц на экранную единицу)
var size := Vector2(1600, 900)


func z() -> float:
	return exp(log_z)


func px_per_unit() -> float:
	return size.y / (2.0 * H * tan(deg_to_rad(FOV) * 0.5))


## Сколько пикселей занимает длина len_g (гал. ед.).
func px(len_g: float) -> float:
	return len_g / z() * px_per_unit()


## Положение точки относительно фокуса в экранных единицах (для transform).
func rel(bx: float, bz: float, ox := 0.0, oz := 0.0) -> Vector2:
	var k := 1.0 / z()
	return Vector2(((bx - ax) + (ox - dx)) * k, ((bz - az) + (oz - dz)) * k)


func screen(bx: float, bz: float, ox := 0.0, oz := 0.0) -> Vector2:
	return rel(bx, bz, ox, oz) * px_per_unit() + size * 0.5


## Точка экрана → [bx, bz, ox, oz] (база — текущий якорь).
func pos_at(p: Vector2) -> Array:
	var k := z() / px_per_unit()
	return [ax, az, dx + (p.x - size.x * 0.5) * k, dz + (p.y - size.y * 0.5) * k]


## Поставить фокус так, чтобы точка pos оказалась под пикселем p.
func put(pos: Array, p: Vector2) -> void:
	var k := z() / px_per_unit()
	dx = (pos[0] - ax) + pos[2] - (p.x - size.x * 0.5) * k
	dz = (pos[1] - az) + pos[3] - (p.y - size.y * 0.5) * k


## Сменить якорь, не сдвигая картинку.
func rebase(nx: float, nz: float) -> void:
	dx = (ax - nx) + dx
	dz = (az - nz) + dz
	ax = nx
	az = nz


func focus_abs() -> Vector2:
	return Vector2(ax + dx, az + dz)
