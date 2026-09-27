class_name Names
extends RefCounted

const A := ["Ал", "Ве", "Ка", "Ор", "Те", "Ис", "Ми", "За", "Ну", "Эр", "Ли", "Со", "Ра", "Ки", "Ан", "Та"]
const B := ["ри", "го", "ла", "тен", "мар", "ви", "до", "кс", "ру", "не", "фа", "ол", "ус", "ит"]
const C := ["", "", "а", "он", "ис", "ея", "ар", "ум"]


static func star(rng: RandomNumberGenerator) -> String:
	var n: String = A[rng.randi() % A.size()] + B[rng.randi() % B.size()] + C[rng.randi() % C.size()]
	return n


static func roman(i: int) -> String:
	return ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"][clampi(i, 0, 9)]
