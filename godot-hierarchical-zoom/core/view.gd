class_name View
extends RefCounted
## Состояние камеры в «масштабном пространстве».
##
## Настоящая Camera3D неподвижна: висит на высоте H над плоскостью y = 0 и смотрит вниз.
## Двигается и масштабируется МИР. Где мы и с каким увеличением, хранится здесь
## в галактических единицах. В GDScript тип float — это 64-битный double, поэтому
## точности хватает от всей галактики до отдельного корабля (диапазон ~10^6).
## Движку (там float32) отдаются только координаты относительно камеры.

const H := 100.0           # высота камеры, «экранные» единицы
const FOV := 30.0
const Z_MIN := 2.2e-4      # галактических единиц на экранную единицу: максимальное приближение
const Z_MAX := 40.0        # максимальное отдаление

var fx := 0.0              # фокус — точка под центром экрана, в галактических единицах
var fz := 0.0
var log_z := log(28.0)     # масштаб храним логарифмом: зум в нём линеен
var size := Vector2(1600, 900)


func z() -> float:
	return exp(log_z)


## Пикселей на экранную единицу на плоскости карты.
func px_per_unit() -> float:
	return size.y / (2.0 * H * tan(deg_to_rad(FOV) * 0.5))


## Сколько пикселей на экране занимает длина len_g (галактические единицы).
func px(len_g: float) -> float:
	return len_g / z() * px_per_unit()


func to_screen(gx: float, gz: float) -> Vector2:
	var k := px_per_unit() / z()
	return Vector2((gx - fx) * k, (gz - fz) * k) + size * 0.5


## Точка экрана → галактические координаты [gx, gz] (массив, чтобы не терять double).
func to_galaxy(p: Vector2) -> Array:
	var k := z() / px_per_unit()
	return [fx + (p.x - size.x * 0.5) * k, fz + (p.y - size.y * 0.5) * k]


## Поставить фокус так, чтобы галактическая точка g оказалась под пикселем p.
func anchor(gx: float, gz: float, p: Vector2) -> void:
	var k := z() / px_per_unit()
	fx = gx - (p.x - size.x * 0.5) * k
	fz = gz - (p.y - size.y * 0.5) * k
