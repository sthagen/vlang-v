fn test_fixed_array_map() {
	a := [1, 2, 3]!

	b1 := a.map(it * 2)
	println(b1)
	assert b1 == [2, 4, 6]!
	b10 := [1, 2, 3]!.map(it * 2)
	assert b10 == [2, 4, 6]!
	assert [1, 2, 3]!.map(it * 2) == [2, 4, 6]!

	b11 := a.map(|x| x * 2)
	println(b11)
	assert b11 == [2, 4, 6]!
	b110 := [1, 2, 3]!.map(|x| x * 2)
	assert b110 == [2, 4, 6]!
	assert [1, 2, 3]!.map(|x| x * 2) == [2, 4, 6]!

	b2 := a.map('${it}')
	println(b2)
	assert b2 == ['1', '2', '3']!
	b20 := [1, 2, 3]!.map('${it}')
	assert b20 == ['1', '2', '3']!
	assert [1, 2, 3]!.map('${it}') == ['1', '2', '3']!

	b22 := a.map(|x| '${x}')
	println(b22)
	assert b22 == ['1', '2', '3']!
	b220 := [1, 2, 3]!.map(|x| '${x}')
	assert b220 == ['1', '2', '3']!
	assert [1, 2, 3]!.map(|x| '${x}') == ['1', '2', '3']!

	b3 := a.map(it + 2)
	println(b3)
	assert b3 == [3, 4, 5]!
	b30 := [1, 2, 3]!.map(it + 2)
	assert b30 == [3, 4, 5]!
	assert [1, 2, 3]!.map(it + 2) == [3, 4, 5]!

	b33 := a.map(|x| x + 2)
	println(b33)
	assert b33 == [3, 4, 5]!
	b330 := [1, 2, 3]!.map(|x| x + 2)
	assert b330 == [3, 4, 5]!
	assert [1, 2, 3]!.map(|x| x + 2) == [3, 4, 5]!
}
