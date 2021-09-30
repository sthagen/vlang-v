// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strings

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

pub fn (c rune) str() string {
	return utf32_to_str(u32(c))
	/*
	unsafe {
		fst_byte := int(c)>>8 * 3 & 0xff
		len := utf8_char_len(byte(fst_byte))
		println('len=$len')
		mut str := string{
			len: len
			str: malloc(len + 1)
		}
		for i in 0..len {
			str.str[i] = byte(int(c)>>8 * (3 - i) & 0xff)
		}
		str.str[len] = `\0`
		println(str)
		return str
	}
	*/
}

// string converts a rune array to a string
[manualfree]
pub fn (ra []rune) string() string {
	mut sb := strings.new_builder(ra.len)
	sb.write_runes(ra)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// repeat returns a new string with `count` number of copies of the rune it was called on.
pub fn (c rune) repeat(count int) string {
	if count < 0 {
		panic('rune.repeat: count is negative: $count')
	} else if count == 0 {
		return ''
	} else if count == 1 {
		return c.str()
	}
	mut buffer := [5]byte{}
	res := unsafe { utf32_to_str_no_malloc(u32(c), &buffer[0]) }
	return res.repeat(count)
}
