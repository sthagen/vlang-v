import x.json2

fn test_json_string_characters() {
	text := json2.raw_decode(r'"\n\r\b\f\t\\\"\/"') or { '' }
	assert text.json_str() == '\\n\\r\\b\\f\\t\\\\\\"\\/'
}

fn test_json_escape_low_chars() {
	esc := '\u001b'
	assert esc.len == 1
	text := json2.Any(esc)
	assert text.json_str() == r'\u001b'
}

fn test_json_string() {
	text := json2.Any('te✔st')
	assert text.json_str() == r'te\u2714st'
	boolean := json2.Any(true)
	assert boolean.json_str() == 'true'
	integer := json2.Any(int(-5))
	assert integer.json_str() == '-5'
	u64integer := json2.Any(u64(5000))
	assert u64integer.json_str() == '5000'
	i64integer := json2.Any(i64(-17))
	assert i64integer.json_str() == '-17'
}

fn test_json_string_emoji() {
	text := json2.Any('🐈')
	assert text.json_str() == r' '
}

fn test_json_string_non_ascii() {
	text := json2.Any('ひらがな')
	assert text.json_str() == r'\u3072\u3089\u304c\u306a'
}

fn test_utf8_strings_are_not_modified() ? {
	original := '{"s":"Schilddrüsenerkrankungen"}'
	// dump(original)
	deresult := json2.raw_decode(original) ?
	// dump(deresult)
	assert deresult.str() == original
}
