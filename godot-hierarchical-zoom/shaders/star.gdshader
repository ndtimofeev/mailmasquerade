// Иконка звезды. Размер задаётся в ПИКСЕЛЯХ и зависит не от мировых размеров,
// а от того, сколько пикселей на экране до ближайшего соседа (плотность карты):
//   size = clamp(k * (nn_px)^p, min, max) * величина.
// При p < 1 звёзды растут медленнее зума: в обзоре 10 000 звёзд — это россыпь точек,
// вблизи — аккуратные иконки, и ни в каком масштабе они не сливаются в кашу.
shader_type spatial;
render_mode unshaded, blend_add, depth_draw_never, cull_disabled, fog_disabled;

uniform float px_per_g = 1.0;     // пикселей на галактическую единицу (из View)
uniform float size_k = 0.9;
uniform float size_pow = 0.5;
uniform float size_min = 1.0;
uniform float size_max = 7.0;
uniform float true_px = 0.0;      // радиус настоящего диска в пикселях (только для звезды системы)
uniform float ring_alpha = 1.0;
uniform float alpha = 1.0;
uniform vec3 spectral[5];
uniform vec3 factions[5];

varying float v_size;
varying float v_quad;
varying vec3 v_color;
varying vec3 v_ring;
varying float v_ring_a;

void vertex() {
	// INSTANCE_CUSTOM: x — расстояние до ближайшего соседа (гал. ед.), y — величина 0..1,
	// z — фракция + 1 (0 — ничья), w — спектральный класс.
	float s = clamp(size_k * pow(INSTANCE_CUSTOM.x * px_per_g, size_pow), size_min, size_max);
	s *= mix(0.6, 1.5, INSTANCE_CUSTOM.y);
	s = max(s, true_px * 2.0);
	v_size = s;
	v_quad = s * 3.0 + 8.0;                 // квадрат с запасом под ореол и кольцо фракции
	v_color = spectral[int(INSTANCE_CUSTOM.w + 0.5)];
	int f = int(INSTANCE_CUSTOM.z + 0.5);
	v_ring = f > 0 ? factions[f - 1] : vec3(0.0);
	v_ring_a = f > 0 ? smoothstep(4.0, 7.0, s) * ring_alpha : 0.0;

	// Центр звезды проецируем как обычно, а углы квадрата сдвигаем в пикселях экрана.
	vec4 c = PROJECTION_MATRIX * MODELVIEW_MATRIX * vec4(0.0, 0.0, 0.0, 1.0);
	POSITION = c + vec4(VERTEX.xy * v_quad * 2.0 / VIEWPORT_SIZE * c.w, 0.0, 0.0);
}

void fragment() {
	vec2 p = (UV - 0.5) * v_quad;           // пиксели от центра
	float r = length(p);
	float core_r = max(v_size * 0.5, 0.6);
	float core = 1.0 - smoothstep(core_r - 0.7, core_r + 0.7, r);
	float halo = exp(-r / (core_r * 1.2 + 1.5)) * 0.3;
	float ring = (1.0 - smoothstep(0.0, 0.9, abs(r - core_r - 3.0))) * v_ring_a;
	float dim = clamp(v_size / 2.2, 0.35, 1.0);   // крошечные звёзды тусклее — нет «белой каши»
	ALBEDO = (v_color * (core * 1.7 + halo) * dim + v_ring * ring * 0.9) * alpha;
}
