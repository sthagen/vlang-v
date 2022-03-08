// Test whether a sumtype that has multiple subtypes,
// some with custom .str() methods, can be converted to a string,
// i.e. whether its own autogenerated .str() method will work.
// Note: Dictionary.str() calls $v.str() in the string interpolation,
// which in turn can call Dictionary.str(), i.e. they are mutually
// recursive.
type Object = Dictionary | Stream | bool | f32 | int | string

struct Dictionary {
	items map[string]Object
}

struct Stream {
mut:
	content string
}

fn (dict Dictionary) str() string {
	mut temp := []string{}
	for k, v in dict.items {
		temp << '    << "$k": ' + v.str().replace('\n', ' ')
	}
	return '\n' + temp.join('\n') + '\n'
}

fn test_str_of_sumtype_works() {
	o := Object(Dictionary{
		items: {
			'abc': Object(Stream{
				content: 'xyz'
			})
			'aaa': Object(int(321))
			'bbb': Object(f32(3.14))
		}
	})
	so := o.str()
	println(so)
	assert so.starts_with('Object(')
	assert so.contains('<< "abc": Object(Stream{')
	assert so.contains('<< "aaa": Object(321)')
	assert so.contains('<< "bbb": Object(3.14')
	assert so.ends_with(')')
}
