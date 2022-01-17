// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module scanner

import v.token
import v.pref

fn scan_kinds(text string) []token.Kind {
	mut scanner := new_scanner(text, .skip_comments, &pref.Preferences{})
	mut token_kinds := []token.Kind{}
	for {
		tok := scanner.scan()
		if tok.kind == .eof {
			break
		}
		token_kinds << tok.kind
	}
	return token_kinds
}

fn scan_tokens(text string) []token.Token {
	mut scanner := new_scanner(text, .parse_comments, &pref.Preferences{})
	mut tokens := []token.Token{}
	for {
		tok := scanner.scan()
		if tok.kind == .eof {
			break
		}
		tokens << tok
	}
	return tokens
}

fn test_scan() {
	token_kinds := scan_kinds('println(2 + 3)')
	assert token_kinds.len == 6
	assert token_kinds[0] == .name
	assert token_kinds[1] == .lpar
	assert token_kinds[2] == .number
	assert token_kinds[3] == .plus
	assert token_kinds[4] == .number
	assert token_kinds[5] == .rpar
}

fn test_number_constant_input_format() {
	mut c := 0xa0
	assert c == 0xa0
	c = 0b1001
	assert c == 9
	c = 1000000
	assert c == 1000000
}

fn test_float_conversion_and_reading() {
	d := 23000000e-3
	assert int(d) == 23000
	mut e := 1.2E3 * -1e-1
	assert e == -120.0
	e = 1.2E3 * 1e-1
	x := 55.0
	assert e == 120.0
	assert 1.23e+10 == 1.23e10
	assert 1.23e+10 == 1.23e0010
	assert (-1.23e+10) == (1.23e0010 * -1.0)
	assert x == 55.0
}

fn test_float_without_fraction() {
	mut result := scan_kinds('x := 10.0')
	assert result.len == 3
	assert result[0] == .name
	assert result[1] == .decl_assign
	assert result[2] == .number
	result = scan_kinds('return 3.0, 4.0')
	assert result.len == 4
	assert result[0] == .key_return
	assert result[1] == .number
	assert result[2] == .comma
	assert result[3] == .number
	result = scan_kinds('fun(5.0)')
	assert result.len == 4
	assert result[0] == .name
	assert result[1] == .lpar
	assert result[2] == .number
	assert result[3] == .rpar
}

fn test_reference_bools() {
	result := scan_kinds('true && false')
	assert result.len == 3
	assert result[0] == .key_true
	assert result[1] == .and
	assert result[2] == .key_false
}

fn test_reference_var() {
	result := scan_kinds('&foo')
	assert result.len == 2
	assert result[0] == .amp
	assert result[1] == .name
}

fn test_array_of_references() {
	result := scan_kinds('[]&foo')
	assert result.len == 4
	assert result[0] == .lsbr
	assert result[1] == .rsbr
	assert result[2] == .amp
	assert result[3] == .name
}

fn test_ref_array_of_references() {
	result := scan_kinds('&[]&foo')
	assert result.len == 5
	assert result[0] == .amp
	assert result[1] == .lsbr
	assert result[2] == .rsbr
	assert result[3] == .amp
	assert result[4] == .name
}

fn test_ref_ref_foo() {
	result := scan_kinds('&&foo')
	assert result.len == 3
	assert result[0] == .amp
	assert result[1] == .amp
	assert result[2] == .name
}

fn test_array_of_ref_ref_foo() {
	result := scan_kinds('[]&&foo')
	assert result.len == 5
	assert result[0] == .lsbr
	assert result[1] == .rsbr
	assert result[2] == .amp
	assert result[3] == .amp
	assert result[4] == .name
}

fn test_ref_ref_array_ref_ref_foo() {
	result := scan_kinds('&&[]&&foo')
	assert result.len == 7
	assert result[0] == .amp
	assert result[1] == .amp
	assert result[2] == .lsbr
	assert result[3] == .rsbr
	assert result[4] == .amp
	assert result[5] == .amp
	assert result[6] == .name
}

fn test_escape_string() {
	// these assertions aren't helpful...
	// they test the vlib built-in to the compiler,
	// but we want to test this module before compilation
	assert '\x61' == 'a'
	assert '\x62' == 'b'
	// assert `\x61` == `a` // will work after pull request goes through

	// SINGLE CHAR ESCAPES
	// SINGLE CHAR APOSTROPHE
	mut result := scan_tokens(r"`'`")
	assert result[0].kind == .chartoken
	assert result[0].lit == r"\'"

	// SINGLE CHAR BACKTICK
	result = scan_tokens(r'`\``')
	assert result[0].kind == .chartoken
	assert result[0].lit == r'\`'

	// SINGLE CHAR SLASH
	result = scan_tokens(r'`\\`')
	assert result[0].kind == .chartoken
	assert result[0].lit == r'\\'

	// SINGLE CHAR UNICODE ESCAPE
	result = scan_tokens(r'`\u2605`')
	assert result[0].kind == .chartoken
	assert result[0].lit == r'★'

	// SINGLE CHAR ESCAPED ASCII
	result = scan_tokens(r'`\x61`')
	assert result[0].kind == .chartoken
	assert result[0].lit == r'a'

	// SINGLE CHAR INCORRECT ESCAPE
	// result = scan_tokens(r'`\x61\x61`') // should always result in an error

	// SINGLE CHAR MULTI-BYTE UTF-8
	// Compilation blocked by vlib/v/checker/check_types.v, but works in the repl
	result = scan_tokens(r'`\xe29885`')
	assert result[0].lit == r'★'

	// STRING ESCAPES =================
	// STRING APOSTROPHE
	result = scan_tokens(r"'\''")
	assert result[0].kind == .string
	assert result[0].lit == r"\'"

	// STRING BACKTICK
	result = scan_tokens(r"'\`'")
	assert result[0].kind == .string
	assert result[0].lit == r'\`'

	// STRING SLASH
	result = scan_tokens(r"'\\'")
	assert result[0].kind == .string
	assert result[0].lit == r'\\'

	// STRING UNICODE ESCAPE
	result = scan_tokens(r"'\u2605'")
	assert result[0].kind == .string
	assert result[0].lit == r'★'

	// STRING ESCAPED ASCII
	result = scan_tokens(r"'\x61'")
	assert result[0].kind == .string
	assert result[0].lit == r'a'

	// STRING ESCAPED EXTENDED ASCII
	// (should not be converted to unicode)
	result = scan_tokens(r"'\xe29885'")
	assert result[0].kind == .string
	assert result[0].lit.bytes() == [byte(0xe2), `9`, `8`, `8`, `5`]

	// SHOULD RESULT IN ERRORS
	// result = scan_tokens(r'`\x61\x61`') // should always result in an error
	// result = scan_tokens(r"'\x'") // should always result in an error
	// result = scan_tokens(r'`hello`') // should always result in an error
}

fn test_comment_string() {
	mut result := scan_tokens('// single line comment will get an \\x01 prepended')
	assert result[0].kind == .comment
	assert result[0].lit[0] == byte(1) // \x01
	// result = scan_tokens('/// doc comment will keep third / at beginning')
	// result = scan_tokens('/* block comment will be stripped of whitespace */')
	// result = scan_tokens('a := 0 // line end comment also gets \\x01 prepended')
}
