// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings
import time

// Decodes a JSON string into an `Any` type. Returns an option.
pub fn raw_decode(src string) !Any {
	mut p := new_parser(src, true)
	return p.decode()
}

// Same with `raw_decode`, but skips the type conversion for certain types when decoding a certain value.
pub fn fast_raw_decode(src string) !Any {
	mut p := new_parser(src, false)
	return p.decode()
}

// decode is a generic function that decodes a JSON string into the target type.
pub fn decode[T](src string) !T {
	res := raw_decode(src)!.as_map()
	mut typ := T{}
	$for field in T.fields {
		$if field.typ is u8 {
			typ.$(field.name) = u8(res[field.name]!.u64())
		} $else $if field.typ is u16 {
			typ.$(field.name) = u16(res[field.name]!.u64())
		} $else $if field.typ is u32 {
			typ.$(field.name) = u32(res[field.name]!.u64())
		} $else $if field.typ is u64 {
			typ.$(field.name) = res[field.name]!.u64()
		} $else $if field.typ is int {
			typ.$(field.name) = res[field.name]!.int()
		} $else $if field.typ is i8 {
			typ.$(field.name) = i8(res[field.name]!.i64())
		} $else $if field.typ is i16 {
			typ.$(field.name) = i16(res[field.name]!.i64())
		} $else $if field.typ is i32 {
			// typ.$(field.name) = res[field.name]!.i32()
		} $else $if field.typ is i64 {
			typ.$(field.name) = res[field.name]!.i64()
		} $else $if field.typ is f32 {
			typ.$(field.name) = res[field.name]!.f32()
		} $else $if field.typ is f64 {
			typ.$(field.name) = res[field.name]!.f64()
		} $else $if field.typ is bool {
			typ.$(field.name) = res[field.name]!.bool()
		} $else $if field.typ is string {
			typ.$(field.name) = res[field.name]!.str()
		} $else $if field.typ is time.Time {
			typ.$(field.name) = res[field.name]!.to_time()!
		} $else {
			return error("The type of `${field.name}` can't be decoded. Please open an issue at https://github.com/vlang/v/issues/new/choose")
		}
	}
	return typ
}

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T) string {
	mut sb := strings.new_builder(64)
	defer {
		unsafe { sb.free() }
	}
	$if T is $Array {
		mut array_of_any := []Any{}
		for value in val {
			array_of_any << value
		}
		default_encoder.encode_value(array_of_any, mut sb) or {
			dump(err)
			default_encoder.encode_value[Null](null, mut sb) or {}
		}
	} $else {
		default_encoder.encode_value(val, mut sb) or {
			dump(err)
			default_encoder.encode_value[Null](null, mut sb) or {}
		}
	}
	return sb.str()
}

// encode_pretty ...
pub fn encode_pretty[T](typed_data T) string {
	encoded := encode(typed_data)
	raw_decoded := raw_decode(encoded) or { 0 }
	return raw_decoded.prettify_json_str()
}

// int uses `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int {
			return f
		}
		i8, i16, i64, u8, u16, u32, u64, f32, f64, bool {
			return int(f)
		}
		string {
			if f == 'false' || f == 'true' {
				return int(f.bool())
			}
			return f.int()
		}
		else {
			return 0
		}
	}
}

// i64 uses `Any` as a 64-bit integer.
pub fn (f Any) i64() i64 {
	match f {
		i64 {
			return f
		}
		i8, i16, int, u8, u16, u32, u64, f32, f64, bool {
			return i64(f)
		}
		string {
			if f == 'false' || f == 'true' {
				return i64(f.bool())
			}
			return f.i64()
		}
		else {
			return 0
		}
	}
}

// u64 uses `Any` as a 64-bit unsigned integer.
pub fn (f Any) u64() u64 {
	match f {
		u64 {
			return f
		}
		u8, u16, u32, i8, i16, int, i64, f32, f64, bool {
			return u64(f)
		}
		string {
			if f == 'false' || f == 'true' {
				return u64(f.bool())
			}
			return f.u64()
		}
		else {
			return 0
		}
	}
}

// f32 uses `Any` as a 32-bit float.
pub fn (f Any) f32() f32 {
	match f {
		f32 {
			return f
		}
		bool, i8, i16, int, i64, u8, u16, u32, u64, f64 {
			return f32(f)
		}
		string {
			if f == 'false' || f == 'true' {
				return f32(f.bool())
			}
			return f.f32()
		}
		else {
			return 0.0
		}
	}
}

// f64 uses `Any` as a 64-bit float.
pub fn (f Any) f64() f64 {
	match f {
		f64 {
			return f
		}
		i8, i16, int, i64, u8, u16, u32, u64, f32 {
			return f64(f)
		}
		string {
			if f == 'false' || f == 'true' {
				return f64(f.bool())
			}
			return f.f64()
		}
		else {
			return 0.0
		}
	}
}

// bool uses `Any` as a bool.
pub fn (f Any) bool() bool {
	match f {
		bool {
			return f
		}
		string {
			if f.len > 0 {
				return f != '0' && f != '0.0' && f != 'false'
			} else {
				return false
			}
		}
		i8, i16, int, i64 {
			return i64(f) != 0
		}
		u8, u16, u32, u64 {
			return u64(f) != 0
		}
		f32, f64 {
			return f64(f) != 0.0
		}
		else {
			return false
		}
	}
}

// arr uses `Any` as an array.
pub fn (f Any) arr() []Any {
	if f is []Any {
		return f
	} else if f is map[string]Any {
		mut arr := []Any{}
		for _, v in f {
			arr << v
		}
		return arr
	}
	return [f]
}

// as_map uses `Any` as a map.
pub fn (f Any) as_map() map[string]Any {
	if f is map[string]Any {
		return f
	} else if f is []Any {
		mut mp := map[string]Any{}
		for i, fi in f {
			mp['${i}'] = fi
		}
		return mp
	}
	return {
		'0': f
	}
}

// to_time uses `Any` as a time.Time.
pub fn (f Any) to_time() !time.Time {
	match f {
		time.Time {
			return f
		}
		i64 {
			return time.unix(f)
		}
		string {
			if f.len == 10 && f[4] == `-` && f[7] == `-` {
				// just a date in the format `2001-01-01`
				return time.parse_iso8601(f)!
			}
			is_rfc3339 := f.len == 24 && f[23] == `Z` && f[10] == `T`
			if is_rfc3339 {
				return time.parse_rfc3339(f)!
			}
			mut is_unix_timestamp := true
			for c in f {
				if c == `-` || (c >= `0` && c <= `9`) {
					continue
				}
				is_unix_timestamp = false
				break
			}
			if is_unix_timestamp {
				return time.unix(f.i64())
			}
			// TODO - parse_iso8601
			// TODO - parse_rfc2822
			return time.parse(f)!
		}
		else {
			return error('not a time value: ${f} of type: ${f.type_name()}')
		}
	}
}
