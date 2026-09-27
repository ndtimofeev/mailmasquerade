// Линии постоянной толщины в пикселях: гиперлинии, магистрали, границы территорий.
// Каждый отрезок — квадрат из 4 вершин, раздвигаемых поперёк линии уже на экране.
shader_type spatial;
render_mode unshaded, blend_add, depth_draw_never, cull_disabled, fog_disabled;

uniform float px_per_g = 1.0;
uniform float alpha = 1.0;
uniform float width_px = 1.1;
uniform float trunk_width_px = 1.8;
uniform float border_width_px = 1.2;
uniform float lod_from = 14.0;        // трасса короче этого (в пикселях) не видна
uniform float lod_to = 40.0;
uniform float end_gap_px = 6.0;       // зазор у звезды, чтобы линия не упиралась в иконку

varying float v_side;
varying float v_w;
varying float v_t;
varying float v_len_px;
varying float v_kind;
varying float v_seed;
varying float v_extra;

void vertex() {
	// CUSTOM0: xy — направление отрезка на плоскости (x, z), z — сторона (-1/+1), w — t вдоль линии 0..1
	// CUSTOM1: x — длина всей линии (гал. ед.), y — тип (0 трасса, 1 магистраль, 2 граница),
	//          z — случайное число, w — доп. параметр (пунктир / направление импульсов / сдвиг границы)
	float kind = CUSTOM1.y;
	float w = kind < 0.5 ? width_px : (kind < 1.5 ? trunk_width_px : border_width_px);
	w += 1.0;                               // запас на сглаживание края
	vec2 t = normalize(CUSTOM0.xy);         // x мира → вправо, z мира → вниз экрана
	vec2 n = vec2(-t.y, t.x);
	vec2 off = n * CUSTOM0.z * w * 0.5;
	if (kind > 1.5) {
		off += n * CUSTOM1.w;               // граница: сдвиг к своей звезде (две цветные линии)
	}
	vec4 c = PROJECTION_MATRIX * MODELVIEW_MATRIX * vec4(VERTEX, 1.0);
	POSITION = c + vec4(off.x * 2.0 / VIEWPORT_SIZE.x * c.w, -off.y * 2.0 / VIEWPORT_SIZE.y * c.w, 0.0, 0.0);

	v_side = CUSTOM0.z;
	v_w = w;
	v_t = CUSTOM0.w;
	v_len_px = CUSTOM1.x * px_per_g;
	v_kind = kind;
	v_seed = CUSTOM1.z;
	v_extra = CUSTOM1.w;
}

void fragment() {
	float half_w = (v_w - 1.0) * 0.5;
	float across = abs(v_side) * v_w * 0.5;
	float a = (1.0 - smoothstep(half_w - 0.5, half_w + 0.5, across)) * COLOR.a * alpha;
	vec3 col = COLOR.rgb;
	float along = v_t * v_len_px;

	if (v_kind < 1.5) {
		float to_end = min(along, v_len_px - along);
		a *= smoothstep(end_gap_px, end_gap_px + 8.0, to_end);
	}
	if (v_kind < 0.5) {
		a *= smoothstep(lod_from, lod_to, v_len_px);          // LOD: короткие трассы гаснут
		if (v_extra > 0.5) {
			a *= step(0.45, fract(along / 7.0));               // нейтральные трассы — пунктир
		}
	} else if (v_kind < 1.5) {
		// Магистраль: по ней бегут импульсы «трафика».
		float ph = fract(along / 90.0 * v_extra - TIME * 0.25 + v_seed);
		float pulse = smoothstep(0.0, 0.03, ph) * (1.0 - smoothstep(0.03, 0.22, ph));
		col *= 1.0 + pulse * 2.5;
		a *= 0.55 + pulse * 0.9;
	}
	ALBEDO = col * a;
}
