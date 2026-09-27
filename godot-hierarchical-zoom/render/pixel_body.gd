class_name PixelBody
extends Node3D
## Анимированная пиксельная планета или звезда из Pixel Planet Generator (Deep-Fold, MIT).
##
## Сцены генератора — 2D-шейдеры на ColorRect. Каждую рендерим в свой маленький
## SubViewport (100 px на диаметр планеты), а его текстуру кладём на плоский квадрат,
## лежащий на плоскости карты. Фильтрация NEAREST — пиксели остаются чёткими при любом зуме.

const PIXELS := 100
const SCENES := {
	"lava": preload("res://Planets/LavaWorld/LavaWorld.tscn"),
	"barren": preload("res://Planets/NoAtmosphere/NoAtmosphere.tscn"),
	"dry": preload("res://Planets/DryTerran/DryTerran.tscn"),
	"terran": preload("res://Planets/Rivers/Rivers.tscn"),
	"islands": preload("res://Planets/LandMasses/LandMasses.tscn"),
	"ice": preload("res://Planets/IceWorld/IceWorld.tscn"),
	"gas": preload("res://Planets/GasPlanet/GasPlanet.tscn"),
	"ringed": preload("res://Planets/GasPlanetLayers/GasPlanetLayers.tscn"),
	"star": preload("res://Planets/Star/Star.tscn"),
}

var body: Control              # экземпляр сцены генератора
var relative_scale := 1.0      # у колец и звёздных протуберанцев квадрат больше самой планеты
var material: StandardMaterial3D
var _quad: MeshInstance3D


func _init(kind: String, body_seed: int, randomize_colors := false) -> void:
	var vp := SubViewport.new()
	vp.transparent_bg = true
	vp.render_target_update_mode = SubViewport.UPDATE_ALWAYS
	vp.disable_3d = true
	add_child(vp)

	body = SCENES[kind].instantiate()
	# Материалы в сценах генератора общие для всех экземпляров — делаем копии,
	# иначе две планеты одного типа получили бы один сид и одно освещение.
	for c in body.get_children():
		if c is CanvasItem and c.material != null:
			c.material = c.material.duplicate(true)
	vp.add_child(body)
	relative_scale = body.relative_scale
	body.set_pixels(PIXELS)
	body.position = PIXELS * 0.5 * (relative_scale - 1.0) * Vector2.ONE
	body.set_seed(body_seed)
	if randomize_colors:
		seed(body_seed)
		body.randomize_colors()
	vp.size = Vector2i.ONE * int(PIXELS * relative_scale)

	material = StandardMaterial3D.new()
	material.shading_mode = BaseMaterial3D.SHADING_MODE_UNSHADED
	material.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA
	material.texture_filter = BaseMaterial3D.TEXTURE_FILTER_NEAREST
	material.albedo_texture = vp.get_texture()
	var plane := PlaneMesh.new()          # лежит в XZ лицом вверх — к камере
	plane.size = Vector2.ONE
	_quad = Draw.mesh_instance(plane, material)
	add_child(_quad)


## Радиус самой планеты в локальных единицах (квадрат больше на relative_scale).
func set_radius(r: float) -> void:
	_quad.scale = Vector3.ONE * (2.0 * r * relative_scale)


## Направление на звезду в экранных координатах → точка освещения в UV планеты.
func set_light_dir(dir: Vector2) -> void:
	body.set_light(Vector2(0.5, 0.5) + dir.normalized() * 0.32)


func set_alpha(a: float) -> void:
	material.albedo_color.a = a
	visible = a > 0.01


## Перекрасить звезду под спектральный класс.
func tint_star(c: Color) -> void:
	var star := PackedColorArray([Color("f5ffe8"), c.lightened(0.25), c.darkened(0.2), c.darkened(0.65)])
	body.get_node(^"Star").material.set_shader_parameter(&"colors", star)
	body.get_node(^"Blobs").material.set_shader_parameter(&"colors", PackedColorArray([c.lightened(0.4)]))
	body.get_node(^"StarFlares").material.set_shader_parameter(&"colors", PackedColorArray([c, Color("f5ffe8")]))
