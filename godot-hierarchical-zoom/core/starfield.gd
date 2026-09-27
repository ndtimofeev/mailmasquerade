class_name Starfield
extends Node3D
## Фон уровня: настоящие 3D-точки и туманности ПОД плоскостью карты.
## Камера перспективная и смотрит строго вниз, поэтому дальние слои при
## панорамировании и зуме смещаются медленнее ближних — параллакс получается
## сам собой, без отдельной логики.


## bounds — радиус игровой области, max_h — максимальная высота камеры,
## depth — насколько глубоко под плоскостью лежит самый дальний слой.
func build(rng_seed: int, bounds: float, max_h: float, depth: float, tint: Color) -> void:
	var rng := RandomNumberGenerator.new()
	rng.seed = rng_seed
	var half_fov := deg_to_rad(TopDownCamera.FOV_DEG) * 0.5
	# 16:9 и небольшой запас по краям
	var aspect := 1.9

	# Три слоя точек разного экранного размера; каждый слой занимает свой диапазон глубин.
	var layers := [
		{"count": 2600, "size": 1.0, "from": 0.35, "to": 1.0, "bright": 0.55},
		{"count": 900, "size": 2.0, "from": 0.15, "to": 0.7, "bright": 0.8},
		{"count": 160, "size": 3.0, "from": 0.05, "to": 0.4, "bright": 1.0},
	]
	for layer in layers:
		var pos := PackedVector3Array()
		var col := PackedColorArray()
		for i in layer.count:
			var d := depth * lerpf(layer.from, layer.to, rng.randf())
			# чем глубже слой, тем шире он должен быть, чтобы не было видно краёв
			var ext := bounds + (max_h + d) * tan(half_fov) * aspect
			pos.append(Vector3(rng.randf_range(-ext, ext), -d, rng.randf_range(-ext, ext)))
			var t := rng.randf()
			var c := Color(0.75, 0.85, 1.0).lerp(Color(1.0, 0.85, 0.7), t * t)
			c = c.lerp(tint, 0.25)
			c.a = layer.bright * rng.randf_range(0.35, 1.0)
			col.append(c)
		add_child(Draw.points(pos, col, layer.size))

	# Две туманности на разной глубине — самый заметный параллакс.
	for k in 2:
		var d := depth * (0.3 if k == 0 else 0.8)
		var ext := bounds + (max_h + d) * tan(half_fov) * aspect
		add_child(_nebula(rng, ext * 2.0, -d, tint if k == 0 else tint.lerp(Color(0.6, 0.2, 0.8), 0.5)))


func _nebula(rng: RandomNumberGenerator, size: float, y: float, tint: Color) -> MeshInstance3D:
	var noise := FastNoiseLite.new()
	noise.seed = rng.randi()
	noise.noise_type = FastNoiseLite.TYPE_SIMPLEX_SMOOTH
	noise.frequency = 0.004
	noise.fractal_octaves = 5

	var ramp := Gradient.new()
	ramp.set_color(0, Color(0, 0, 0, 0))
	ramp.set_color(1, Color(tint.r, tint.g, tint.b, 0.05))
	ramp.add_point(0.6, Color(0, 0, 0, 0))
	ramp.add_point(0.8, Color(tint.r, tint.g, tint.b, 0.07))

	var tex := NoiseTexture2D.new()
	tex.width = 512
	tex.height = 512
	tex.noise = noise
	tex.color_ramp = ramp

	var mat := Draw.unlit(Color.WHITE, true)
	mat.albedo_texture = tex

	var plane := PlaneMesh.new()      # PlaneMesh лежит в XZ, нормаль +Y — ровно под камерой
	plane.size = Vector2(size, size)
	var mi := MeshInstance3D.new()
	mi.mesh = plane
	mi.material_override = mat
	mi.position = Vector3(rng.randf_range(-size, size) * 0.1, y, rng.randf_range(-size, size) * 0.1)
	mi.cast_shadow = GeometryInstance3D.SHADOW_CASTING_SETTING_OFF
	return mi
