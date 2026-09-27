class_name ZoomProfile
extends RefCounted
## Нелинейная шкала зума.
##
## Колесо двигает параметр u равномерно: одно деление = +1 к u. Масштаб ln z = f(u)
## меняется медленно там, где есть на что смотреть, и очень быстро в «разрывах» между
## уровнями: звёздная карта → система (A) и система → планета/бой (B). В разрыве на
## экране ничего не растёт, кроме фона, поэтому пролетать его нужно в 2–3 деления.

const L_TOP := 4.9                        # ln z: вся галактика
const L_BOTTOM := -25.0                   # ln z: отдельный корабль
const GAP_A := Vector2(0.3, -7.0)         # (верх, низ) разрыва A в ln z
const GAP_B := Vector2(-11.8, -17.3)      # (верх, низ) разрыва B
const STEP := 0.33                        # ln z на одно деление колеса в обычной зоне
const GAP_STEP := 1.8                     # ... внутри разрыва
const EDGE := 0.8                         # плавность смены скорости на краях разрыва
const N := 4096

var _u := PackedFloat64Array()            # u в равномерных узлах по ln z
var _dl := (L_TOP - L_BOTTOM) / (N - 1)


func _init() -> void:
	_u.resize(N)
	_u[0] = 0.0
	for i in range(1, N):
		var l := L_TOP - (i - 0.5) * _dl
		_u[i] = _u[i - 1] + _dl * _density(l)


## Делений колеса на единицу ln z.
func _density(l: float) -> float:
	var g := maxf(_in(l, GAP_A), _in(l, GAP_B))
	return lerpf(1.0 / STEP, 1.0 / GAP_STEP, g)


static func _in(l: float, gap: Vector2) -> float:
	var enter := clampf((gap.x - l) / EDGE + 0.5, 0.0, 1.0)
	var leave := clampf((l - gap.y) / EDGE + 0.5, 0.0, 1.0)
	return smoothstep(0.0, 1.0, enter) * smoothstep(0.0, 1.0, leave)


func u_max() -> float:
	return _u[N - 1]


func u_of(l: float) -> float:
	var f := clampf((L_TOP - l) / _dl, 0.0, N - 1.001)
	var i := int(f)
	return lerpf(_u[i], _u[i + 1], f - i)


func l_of(u: float) -> float:
	u = clampf(u, 0.0, u_max())
	var lo := 0
	var hi := N - 1
	while hi - lo > 1:
		var mid := (lo + hi) >> 1
		if _u[mid] <= u:
			lo = mid
		else:
			hi = mid
	var f := (u - _u[lo]) / maxf(_u[hi] - _u[lo], 1e-12)
	return L_TOP - (lo + f) * _dl


## Разрыв, внутри которого (или у верхнего края которого) находится l; ZERO — вне разрывов.
static func gap_at(l: float) -> Vector2:
	if l < GAP_A.x + 0.5 and l > GAP_A.y:
		return GAP_A
	if l < GAP_B.x + 0.5 and l > GAP_B.y:
		return GAP_B
	return Vector2.ZERO
