// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

// Encoder encodes the an `Any` type into JSON representation.
// It provides parameters in order to change the end result.
pub struct Encoder {
	newline              u8
	newline_spaces_count int
	escape_unicode       bool = true
}

// byte array versions of the most common tokens/chars to avoid reallocations
const null_in_bytes = 'null'

const true_in_string = 'true'

const false_in_string = 'false'

const zero_rune = `0`

const comma_rune = `,`

const colon_rune = `:`

const unicode_escape_chars = [`\\`, `u`]!

const quote_rune = `"`

const escaped_chars = [(r'\b'), (r'\f'), (r'\n'), (r'\r'), (r'\t')]!

const curly_open_rune = `{`

const curly_close_rune = `}`

// encode_value encodes a value to the specific buffer.
pub fn (e &Encoder) encode_value[T](val T, mut buf []u8) ! {
	e.encode_value_with_level[T](val, 1, mut buf)!
}

fn (e &Encoder) encode_newline(level int, mut buf []u8) ! {
	if e.newline != 0 {
		buf << e.newline
		for j := 0; j < level * e.newline_spaces_count; j++ {
			buf << ` `
		}
	}
}

fn (e &Encoder) encode_any(val Any, level int, mut buf []u8) ! {
	match val {
		string {
			e.encode_string(val, mut buf)!
		}
		bool {
			if val == true {
				unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
			} else {
				unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
			}
		}
		i8, i16, int, i64 {
			str_int := val.str()
			unsafe { buf.push_many(str_int.str, str_int.len) }
		}
		u8, u16, u32, u64 {
			str_int := val.str()
			unsafe { buf.push_many(str_int.str, str_int.len) }
		}
		f32, f64 {
			$if !nofloat ? {
				str_float := val.str()
				unsafe { buf.push_many(str_float.str, str_float.len) }
				if str_float[str_float.len - 1] == `.` {
					buf << json2.zero_rune
				}
				return
			}
			buf << json2.zero_rune
		}
		map[string]Any {
			buf << json2.curly_open_rune
			mut i := 0
			for k, v in val {
				e.encode_newline(level, mut buf)!
				e.encode_string(k, mut buf)!
				buf << json2.colon_rune
				if e.newline != 0 {
					buf << ` `
				}
				e.encode_value_with_level(v, level + 1, mut buf)!
				if i < val.len - 1 {
					buf << json2.comma_rune
				}
				i++
			}
			e.encode_newline(level - 1, mut buf)!
			buf << json2.curly_close_rune
		}
		[]Any {
			buf << `[`
			for i in 0 .. val.len {
				e.encode_newline(level, mut buf)!
				e.encode_value_with_level(val[i], level + 1, mut buf)!
				if i < val.len - 1 {
					buf << json2.comma_rune
				}
			}
			e.encode_newline(level - 1, mut buf)!
			buf << `]`
		}
		time.Time {
			str_time := val.format_rfc3339()
			buf << `"`
			unsafe { buf.push_many(str_time.str, str_time.len) }
			buf << `"`
		}
		Null {
			unsafe { buf.push_many(json2.null_in_bytes.str, json2.null_in_bytes.len) }
		}
	}
}

fn (e &Encoder) encode_map[T](value T, level int, mut buf []u8) ! {
	buf << json2.curly_open_rune
	mut idx := 0
	for k, v in value {
		e.encode_newline(level, mut buf)!
		// e.encode_string(k.str(), mut buf)!
		e.encode_string(k, mut buf)!
		buf << json2.colon_rune
		if e.newline != 0 {
			buf << ` `
		}
		e.encode_value_with_level(v, level + 1, mut buf)!
		if idx < value.len - 1 {
			buf << json2.comma_rune
		}
		idx++
	}
	// e.encode_newline(level, mut buf)!
	e.encode_newline(level - 1, mut buf)!
	buf << json2.curly_close_rune
}

fn (e &Encoder) encode_value_with_level[T](val T, level int, mut buf []u8) ! {
	$if T is string {
		e.encode_string(val, mut buf)!
	} $else $if T is Any {
		e.encode_any(val, level, mut buf)!
	} $else $if T is $sumtype {
		if val.str() != 'unknown sum type value' {
			$for v in val.variants {
				if val is v {
					e.encode_value_with_level(val, level, mut buf)!
				}
			}
		}
	} $else $if T is $alias {
		// TODO
	} $else $if T is time.Time {
		parsed_time := time.parse(val.str()) or { time.Time{} }
		e.encode_string(parsed_time.format_rfc3339(), mut buf)!
	} $else $if T is $map {
		e.encode_map(val, level, mut buf)!
	} $else $if T is []Any {
		// TODO test
		e.encode_any(val, level, mut buf)!
	} $else $if T is Encodable {
		str_value := val.json_str()
		unsafe { buf.push_many(str_value.str, str_value.len) }
	} $else $if T is $struct {
		e.encode_struct(val, level, mut buf)!
	} $else $if T is $enum {
		str_int := int(val).str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else $if T is $int || T is $float || T is bool {
		str_int := val.str()
		unsafe { buf.push_many(str_int.str, str_int.len) }
	} $else $if T is Null {
		unsafe { buf.push_many(json2.null_in_bytes.str, json2.null_in_bytes.len) }
	} $else {
		return error('cannot encode value with ${typeof(val).name} type')
	}
}

fn (e &Encoder) encode_struct[U](val U, level int, mut buf []u8) ! {
	buf << json2.curly_open_rune
	mut i := 0
	mut fields_len := 0

	$for field in U.fields {
		$if field.is_option {
			if val.$(field.name) != none {
				fields_len++
			}
		} $else {
			fields_len++
		}
	}
	$for field in U.fields {
		mut ignore_field := false
		value := val.$(field.name)

		is_nil := val.$(field.name).str() == '&nil'

		mut json_name := ''
		for attr in field.attrs {
			if attr.contains('json: ') {
				json_name = attr.replace('json: ', '')
				break
			}
		}

		$if field.is_option {
			is_none := value == none

			if !is_none {
				e.encode_newline(level, mut buf)!
				if json_name != '' {
					e.encode_string(json_name, mut buf)!
				} else {
					e.encode_string(field.name, mut buf)!
				}
				buf << json2.colon_rune

				if e.newline != 0 {
					buf << ` `
				}

				$if field.typ is ?string {
					e.encode_string(val.$(field.name) ?.str(), mut buf)!
				} $else $if field.typ is ?bool {
					if value ?.str() == json2.true_in_string {
						unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
					} else {
						unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
					}
				} $else $if field.typ is ?f32 || field.typ is ?f64 || field.typ is ?i8
					|| field.typ is ?i16 || field.typ is ?int || field.typ is ?i64
					|| field.typ is ?u8 || field.typ is ?u16 || field.typ is ?u32
					|| field.typ is ?u64 {
					str_value := val.$(field.name) ?.str()
					unsafe { buf.push_many(str_value.str, str_value.len) }
				} $else $if field.typ is ?time.Time {
					option_value := val.$(field.name) as ?time.Time
					parsed_time := option_value as time.Time
					e.encode_string(parsed_time.format_rfc3339(), mut buf)!
				} $else $if field.is_array {
					e.encode_array(value, level + 1, mut buf)!
				} $else $if field.is_struct {
					e.encode_struct(value, level + 1, mut buf)!
				} $else $if field.is_enum {
					// FIXME - checker and cast error
					// wr.write(int(val.$(field.name)?).str().bytes())!
					return error('type ${typeof(val).name} cannot be encoded yet')
				} $else $if field.is_alias {
					match field.unaliased_typ {
						typeof[string]().idx {
							e.encode_string(value.str(), mut buf)!
						}
						typeof[bool]().idx {}
						typeof[f32]().idx, typeof[f64]().idx, typeof[i8]().idx, typeof[i16]().idx,
						typeof[int]().idx, typeof[i64]().idx, typeof[u8]().idx, typeof[u16]().idx,
						typeof[u32]().idx, typeof[u64]().idx {
							str_value := value.str()
							unsafe { buf.push_many(str_value.str, str_value.len) }
						}
						typeof[[]int]().idx {
							// FIXME - error: could not infer generic type `U` in call to `encode_array`
							// e.encode_array(value, level, mut buf)!
						}
						else {
							// e.encode_value_with_level(value, level + 1, mut buf)!
						}
					}
				} $else {
					return error('type ${typeof(val).name} cannot be array encoded')
				}
			} else {
				ignore_field = true
			}
		} $else {
			is_none := val.$(field.name).str() == 'unknown sum type value'
			if !is_none && !is_nil {
				e.encode_newline(level, mut buf)!
				if json_name != '' {
					e.encode_string(json_name, mut buf)!
				} else {
					e.encode_string(field.name, mut buf)!
				}
				buf << json2.colon_rune

				if e.newline != 0 {
					buf << ` `
				}
			}

			$if field.indirections != 0 {
				if val.$(field.name) != unsafe { nil } {
					$if field.indirections == 1 {
						e.encode_value_with_level(*val.$(field.name), level + 1, mut buf)!
					}
					$if field.indirections == 2 {
						e.encode_value_with_level(**val.$(field.name), level + 1, mut
							buf)!
					}
					$if field.indirections == 3 {
						e.encode_value_with_level(***val.$(field.name), level + 1, mut
							buf)!
					}
				}
			} $else $if field.typ is string {
				e.encode_string(val.$(field.name).str(), mut buf)!
			} $else $if field.typ is time.Time {
				str_value := val.$(field.name).format_rfc3339()
				buf << json2.quote_rune
				unsafe { buf.push_many(str_value.str, str_value.len) }
				buf << json2.quote_rune
			} $else $if field.typ is bool {
				if value {
					unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
				} else {
					unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
				}
			} $else $if field.typ in [$float, $int] {
				str_value := val.$(field.name).str()
				unsafe { buf.push_many(str_value.str, str_value.len) }
			} $else $if field.is_array {
				// TODO - replace for `field.typ is $array`
				e.encode_array(value, level + 1, mut buf)!
			} $else $if field.typ is $array {
				// e.encode_array(value, level + 1, mut buf)! // FIXME - error: could not infer generic type `U` in call to `encode_array`
			} $else $if field.typ is $struct {
				e.encode_struct(value, level + 1, mut buf)!
			} $else $if field.is_map {
				e.encode_map(value, level + 1, mut buf)!
			} $else $if field.is_enum {
				// TODO - replace for `field.typ is $enum`
				// str_value := int(val.$(field.name)).str()
				// unsafe { buf.push_many(str_value.str, str_value.len) }
				e.encode_value_with_level(val.$(field.name), level + 1, mut buf)!
			} $else $if field.typ is $enum {
			} $else $if field.typ is $sumtype {
				field_value := val.$(field.name)
				if field_value.str() != 'unknown sum type value' {
					$for v in field_value.variants {
						if field_value is v {
							e.encode_value_with_level(field_value, level, mut buf)!
						}
					}
				}
			} $else $if field.typ is $alias {
				$if field.unaliased_typ is string {
					e.encode_string(val.$(field.name).str(), mut buf)!
				} $else $if field.unaliased_typ is time.Time {
					parsed_time := time.parse(val.$(field.name).str()) or { time.Time{} }
					e.encode_string(parsed_time.format_rfc3339(), mut buf)!
				} $else $if field.unaliased_typ is bool {
					if val.$(field.name).str() == json2.true_in_string {
						unsafe { buf.push_many(json2.true_in_string.str, json2.true_in_string.len) }
					} else {
						unsafe { buf.push_many(json2.false_in_string.str, json2.false_in_string.len) }
					}
				} $else $if field.unaliased_typ in [$float, $int] {
					str_value := val.$(field.name).str()
					unsafe { buf.push_many(str_value.str, str_value.len) }
				} $else $if field.unaliased_typ is $array {
					// TODO
				} $else $if field.unaliased_typ is $struct {
					e.encode_struct(value, level + 1, mut buf)!
				} $else $if field.unaliased_typ is $enum {
					// TODO
				} $else $if field.unaliased_typ is $sumtype {
					// TODO
				} $else {
					return error('the alias ${typeof(val).name} cannot be encoded')
				}
			} $else {
				return error('type ${typeof(val).name} cannot be array encoded')
			}
		}

		if i < fields_len - 1 && !ignore_field {
			if !is_nil {
				buf << json2.comma_rune
			}
		}
		if !ignore_field {
			i++
		}
	}
	e.encode_newline(level - 1, mut buf)!
	buf << json2.curly_close_rune
	// b.measure('encode_struct')
}

fn (e &Encoder) encode_array[U](val []U, level int, mut buf []u8) ! {
	buf << `[`
	for i in 0 .. val.len {
		e.encode_newline(level, mut buf)!

		$if U is string || U is bool || U is $int || U is $float {
			e.encode_value_with_level(val[i], level + 1, mut buf)!
		} $else $if U is $array {
			e.encode_array(val[i], level + 1, mut buf)!
		} $else $if U is $struct {
			e.encode_struct(val[i], level + 1, mut buf)!
		} $else $if U is $sumtype {
			// TODO test
			$if U is Any {
				e.encode_any(val[i], level + 1, mut buf)!
			} $else {
				// TODO
			}
		} $else $if U is $enum {
			// TODO test
			e.encode_any(i64(val[i]), level + 1, mut buf)!
		} $else {
			return error('type ${typeof(val).name} cannot be array encoded')
		}
		if i < val.len - 1 {
			buf << json2.comma_rune
		}
	}

	e.encode_newline(level - 1, mut buf)!
	buf << `]`
}

// str returns the JSON string representation of the `map[string]Any` type.
pub fn (f map[string]Any) str() string {
	return Any(f).json_str()
}

// str returns the JSON string representation of the `[]Any` type.
pub fn (f []Any) str() string {
	return Any(f).json_str()
}

// str returns the string representation of the `Any` type. Use the `json_str` method
// if you want to use the escaped str() version of the `Any` type.
pub fn (f Any) str() string {
	if f is string {
		return f
	} else {
		return f.json_str()
	}
}

// json_str returns the JSON string representation of the `Any` type.
@[manualfree]
pub fn (f Any) json_str() string {
	return encode(f)
}

// prettify_json_str returns the pretty-formatted JSON string representation of the `Any` type.
@[manualfree]
pub fn (f Any) prettify_json_str() string {
	mut buf := []u8{}
	defer {
		unsafe { buf.free() }
	}
	mut enc := Encoder{
		newline: `\n`
		newline_spaces_count: 2
	}
	enc.encode_value(f, mut buf) or {}
	return buf.bytestr()
}

// CharLengthIterator is an iterator that generates a char
// length value of every iteration based on the given text.
// (e.g.: "t✔" => [t => 1, ✔ => 2])
struct CharLengthIterator {
	text string
mut:
	idx int
}

fn (mut iter CharLengthIterator) next() ?int {
	if iter.idx >= iter.text.len {
		return none
	}
	defer {
		iter.idx++
	}
	mut len := 1
	c := iter.text[iter.idx]
	if (c & (1 << 7)) != 0 {
		for t := u8(1 << 6); (c & t) != 0; t >>= 1 {
			len++
			iter.idx++
		}
	}
	return len
}

// TODO - Need refactor. Is so slow. The longer the string, the lower the performance.
// encode_string returns the JSON spec-compliant version of the string.
@[manualfree]
fn (e &Encoder) encode_string(s string, mut buf []u8) ! {
	mut char_lens := CharLengthIterator{
		text: s
	}
	mut i := 0
	buf << json2.quote_rune
	for char_len in char_lens {
		if char_len == 1 {
			chr := s[i]
			if chr in important_escapable_chars {
				for j := 0; j < important_escapable_chars.len; j++ {
					if chr == important_escapable_chars[j] {
						unsafe { buf.push_many(json2.escaped_chars[j].str, json2.escaped_chars[j].len) }
						break
					}
				}
			} else if chr == `"` || chr == `/` || chr == `\\` {
				buf << `\\`
				buf << chr
			} else if int(chr) < 0x20 {
				// unsafe { buf.push_many(json2.unicode_escape_chars.str, json2.unicode_escape_chars.len) } // \u
				for r in json2.unicode_escape_chars {
					buf << r
				}
				buf << json2.zero_rune // \u0
				buf << json2.zero_rune // \u00

				hex_code := chr.hex()
				unsafe { buf.push_many(hex_code.str, hex_code.len) }
			} else {
				buf << chr
			}
		} else {
			slice := s[i..i + char_len]
			hex_code := slice.utf32_code().hex()
			if !e.escape_unicode || hex_code.len < 4 {
				// unescaped non-ASCII char
				unsafe { buf.push_many(slice.str, slice.len) }
			} else if hex_code.len == 4 {
				// a unicode endpoint

				// unsafe { buf.push_many(json2.unicode_escape_chars.str, json2.unicode_escape_chars.len) }
				for r in json2.unicode_escape_chars {
					buf << r
				}
				unsafe { buf.push_many(hex_code.str, hex_code.len) }
			} else {
				// TODO: still figuring out what
				// to do with more than 4 chars
				// According to https://www.json.org/json-en.html however, any codepoint is valid inside a string,
				// so just passing it along should hopefully also work.
				unsafe { buf.push_many(slice.str, slice.len) }
			}
			unsafe {
				slice.free()
				hex_code.free()
			}
		}
		i += char_len
	}

	buf << json2.quote_rune
}
