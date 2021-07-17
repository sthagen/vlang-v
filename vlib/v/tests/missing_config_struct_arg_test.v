struct Foo {
	x int
}

fn foo(f Foo) int {
	return f.x
}

struct Bar {
	x int
	y int = 1234
}

fn bar(b Bar) Bar {
	return b
}

fn test_missing_config_struct_arg() {
	assert foo() == 0
	assert bar() == {
		x: 0
		y: 1234
	}
}
