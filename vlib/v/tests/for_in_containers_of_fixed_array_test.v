fn test_for_in_array_of_fixed_array() {
	mut rets := []string{}
	arr := [][2]int{len: 3}

	for pair in arr {
		println(pair)
		rets << '$pair'
	}
	assert rets[0] == '[0, 0]'
	assert rets[1] == '[0, 0]'
	assert rets[2] == '[0, 0]'
}

fn test_for_mut_in_array_of_fixed_array() {
	mut rets := []string{}
	mut arr := [][2]int{len: 3}

	for mut pair in arr {
		println(pair)
		rets << '$pair'
	}
	assert rets[0] == '[0, 0]'
	assert rets[1] == '[0, 0]'
	assert rets[2] == '[0, 0]'
}

fn test_for_in_fixed_array_of_fixed_array() {
	mut rets := []string{}
	arr := [[1, 2]!, [3, 4]!, [5, 6]!]!

	for pair in arr {
		println(pair)
		rets << '$pair'
	}
	assert rets[0] == '[1, 2]'
	assert rets[1] == '[3, 4]'
	assert rets[2] == '[5, 6]'
}

fn test_for_mut_in_fixed_array_of_fixed_array() {
	mut rets := []string{}
	mut arr := [[1, 2]!, [3, 4]!, [5, 6]!]!

	for mut pair in arr {
		println(pair)
		rets << '$pair'
	}
	assert rets[0] == '[1, 2]'
	assert rets[1] == '[3, 4]'
	assert rets[2] == '[5, 6]'
}
