import os
import toml
import toml.ast
import x.json2

// Instructions for developers:
// The actual tests and data can be obtained by doing:
// `cd vlib/toml/tests/testdata`
// `git clone --depth 1 https://github.com/BurntSushi/toml-test.git burntsushi/toml-test`
// See also the CI toml tests
// TODO Goal: make value retrieval of all of https://github.com/BurntSushi/toml-test/test/ pass
const (
	// Kept for easier handling of future updates to the tests
	valid_exceptions       = []string{}
	invalid_exceptions     = []string{}

	valid_value_exceptions = [
		// Integer
		'integer/long.toml', // TODO awaits BUG fix with strconv.parse_int('-9223372036854775808')
		// Date-time
		'datetime/milliseconds.toml',
	]

	jq                     = os.find_abs_path_of_executable('jq') or { '' }
	compare_work_dir_root  = os.join_path(os.temp_dir(), 'v', 'toml', 'burntsushi')
	// From: https://stackoverflow.com/a/38266731/1904615
	jq_normalize           = r'# Apply f to composite entities recursively using keys[], and to atoms
def sorted_walk(f):
  . as $in
  | if type == "object" then
      reduce keys[] as $key
        ( {}; . + { ($key):  ($in[$key] | sorted_walk(f)) } ) | f
  elif type == "array" then map( sorted_walk(f) ) | f
  else f
  end;

def normalize: sorted_walk(if type == "array" then sort else . end);

normalize'
)

fn run(args []string) ?string {
	res := os.execute(args.join(' '))
	if res.exit_code != 0 {
		return error('${args[0]} failed with return code ${res.exit_code}.\n$res.output')
	}
	return res.output
}

// test_burnt_sushi_tomltest run though 'testdata/burntsushi/toml-test/*' if found.
fn test_burnt_sushi_tomltest() {
	this_file := @FILE
	test_root := os.join_path(os.dir(this_file), 'testdata', 'burntsushi', 'toml-test',
		'tests')
	if os.is_dir(test_root) {
		valid_test_files := os.walk_ext(os.join_path(test_root, 'valid'), '.toml')
		println('Testing $valid_test_files.len valid TOML files...')
		mut valid := 0
		mut e := 0
		for i, valid_test_file in valid_test_files {
			mut relative := valid_test_file.all_after(os.join_path('toml-test', 'tests',
				'valid')).trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}
			if relative !in valid_exceptions {
				println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
				toml_doc := toml.parse_file(valid_test_file) or { panic(err) }
				valid++
			} else {
				e++
				println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_exceptions.len]...')
			}
		}
		println('$valid/$valid_test_files.len TOML files was parsed correctly')
		if valid_exceptions.len > 0 {
			println('TODO Skipped parsing of $valid_exceptions.len valid TOML files...')
		}

		// If the command-line tool `jq` is installed, value tests can be run as well.
		if jq != '' {
			println('Testing value output of $valid_test_files.len valid TOML files using "$jq"...')

			if os.exists(compare_work_dir_root) {
				os.rmdir_all(compare_work_dir_root) or { panic(err) }
			}
			os.mkdir_all(compare_work_dir_root) or { panic(err) }

			jq_normalize_path := os.join_path(compare_work_dir_root, 'normalize.jq')
			os.write_file(jq_normalize_path, jq_normalize) or { panic(err) }

			valid = 0
			e = 0
			for i, valid_test_file in valid_test_files {
				mut relative := valid_test_file.all_after(os.join_path('toml-test', 'tests',
					'valid')).trim_left(os.path_separator)
				$if windows {
					relative = relative.replace('/', '\\')
				}
				// Skip the file if we know it can't be parsed or we know that the value retrieval needs work.
				if relative !in valid_exceptions && relative !in valid_value_exceptions {
					println('OK   [${i + 1}/$valid_test_files.len] "$valid_test_file"...')
					toml_doc := toml.parse_file(valid_test_file) or { panic(err) }

					v_toml_json_path := os.join_path(compare_work_dir_root,
						os.file_name(valid_test_file).all_before_last('.') + '.v.json')
					bs_toml_json_path := os.join_path(compare_work_dir_root,
						os.file_name(valid_test_file).all_before_last('.') + '.json')

					os.write_file(v_toml_json_path, to_burntsushi(toml_doc.ast.table)) or {
						panic(err)
					}

					bs_json := os.read_file(valid_test_file.all_before_last('.') + '.json') or {
						panic(err)
					}
					os.write_file(bs_toml_json_path, bs_json) or { panic(err) }

					v_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"', v_toml_json_path]) or {
						contents := os.read_file(v_toml_json_path) or { panic(err) }
						panic(err.msg + '\n$contents')
					}
					bs_normalized_json := run([jq, '-S', '-f "$jq_normalize_path"', bs_toml_json_path]) or {
						contents := os.read_file(v_toml_json_path) or { panic(err) }
						panic(err.msg + '\n$contents')
					}

					assert bs_normalized_json == v_normalized_json

					valid++
				} else {
					e++
					println('SKIP [${i + 1}/$valid_test_files.len] "$valid_test_file" EXCEPTION [$e/$valid_value_exceptions.len]...')
				}
			}
			println('$valid/$valid_test_files.len TOML files was parsed correctly and value checked')
			if valid_value_exceptions.len > 0 {
				println('TODO Skipped value checks of $valid_value_exceptions.len valid TOML files...')
			}
		}

		// TODO test cases where the parser should fail
		invalid_test_files := os.walk_ext(os.join_path(test_root, 'invalid'), '.toml')
		println('Testing $invalid_test_files.len invalid TOML files...')
		mut invalid := 0
		e = 0
		for i, invalid_test_file in invalid_test_files {
			mut relative := invalid_test_file.all_after(os.join_path('toml-test', 'tests',
				'invalid')).trim_left(os.path_separator)
			$if windows {
				relative = relative.replace('/', '\\')
			}
			if relative !in invalid_exceptions {
				println('OK   [${i + 1}/$invalid_test_files.len] "$invalid_test_file"...')
				if toml_doc := toml.parse_file(invalid_test_file) {
					content_that_should_have_failed := os.read_file(invalid_test_file) or {
						panic(err)
					}
					println('     This TOML should have failed:\n${'-'.repeat(40)}\n$content_that_should_have_failed\n${'-'.repeat(40)}')
					assert false
				} else {
					println('     $err.msg')
					assert true
				}
				invalid++
			} else {
				e++
				println('SKIP [${i + 1}/$invalid_test_files.len] "$invalid_test_file" EXCEPTION [$e/$invalid_exceptions.len]...')
			}
		}
		println('$invalid/$invalid_test_files.len TOML files was parsed correctly')
		if invalid_exceptions.len > 0 {
			println('TODO Skipped parsing of $invalid_exceptions.len invalid TOML files...')
		}
	} else {
		println('No test data directory found in "$test_root"')
		assert true
	}
}

// to_burntsushi returns a BurntSushi compatible json string converted from the `value` ast.Value.
fn to_burntsushi(value ast.Value) string {
	match value {
		ast.Quoted {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "string", "value": "$json_text" }'
		}
		ast.DateTime {
			// Normalization for json
			json_text := json2.Any(value.text).json_str().to_upper().replace(' ', 'T')
			typ := if json_text.ends_with('Z') || json_text.all_after('T').contains('-')
				|| json_text.all_after('T').contains('+') {
				'datetime'
			} else {
				'datetime-local'
			}
			return '{ "type": "$typ", "value": "$json_text" }'
		}
		ast.Date {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "date-local", "value": "$json_text" }'
		}
		ast.Time {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "time-local", "value": "$json_text" }'
		}
		ast.Bool {
			json_text := json2.Any(value.text.bool()).json_str()
			return '{ "type": "bool", "value": "$json_text" }'
		}
		ast.Null {
			json_text := json2.Any(value.text).json_str()
			return '{ "type": "null", "value": "$json_text" }'
		}
		ast.Number {
			if value.text.contains('inf') || value.text.contains('nan') {
				return '{ "type": "float", "value": "$value.text" }'
			}
			if !value.text.starts_with('0x')
				&& (value.text.contains('.') || value.text.to_lower().contains('e')) {
				mut val := '$value.f64()'.replace('.e+', '.0e') // json notation
				if !val.contains('.') && val != '0' { // json notation
					val += '.0'
				}
				return '{ "type": "float", "value": "$val" }'
			}
			return '{ "type": "integer", "value": "$value.i64()" }'
		}
		map[string]ast.Value {
			mut str := '{ '
			for key, val in value {
				json_key := json2.Any(key).json_str()
				str += ' "$json_key": ${to_burntsushi(val)},'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]ast.Value {
			mut str := '[ '
			for val in value {
				str += ' ${to_burntsushi(val)},'
			}
			str = str.trim_right(',')
			str += ' ]\n'
			return str
		}
	}
	return '<error>'
}
