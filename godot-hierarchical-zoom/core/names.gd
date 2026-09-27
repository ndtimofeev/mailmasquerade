class_name Names
extends RefCounted

const A := ["Ал", "Ве", "Ка", "Ор", "Те", "Ис", "Ми", "За", "Ну", "Эр", "Ли", "Со", "Ра", "Ки", "Ан", "Та", "Ге", "Ду", "Мо", "Се"]
const B := ["ри", "го", "ла", "тен", "мар", "ви", "до", "кс", "ру", "не", "фа", "ол", "ус", "ит", "ред", "ван"]
const C := ["", "", "а", "он", "ис", "ея", "ар", "ум", "ин", "ос"]
const CAT := ["HD", "HR", "GJ", "KX", "TYC", "LHS"]


## Собственное имя — для заметных звёзд.
static func star(rng: RandomNumberGenerator) -> String:
	return A[rng.randi() % A.size()] + B[rng.randi() % B.size()] + C[rng.randi() % C.size()]


## Каталожный номер — для остальных тысяч.
static func catalog(rng: RandomNumberGenerator) -> String:
	return "%s %d" % [CAT[rng.randi() % CAT.size()], rng.randi_range(1000, 99999)]


static func roman(i: int) -> String:
	return ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"][clampi(i, 0, 9)]
