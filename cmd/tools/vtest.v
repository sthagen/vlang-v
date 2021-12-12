module main

import os
import os.cmdline
import testing
import v.pref

fn main() {
	args := os.args.clone()
	if os.args.last() == 'test' {
		show_usage()
		return
	}
	args_to_executable := args[1..]
	mut args_before := cmdline.options_before(args_to_executable, ['test'])
	mut args_after := cmdline.options_after(args_to_executable, ['test'])
	fail_fast := extract_flag('-fail-fast', mut args_after, testing.fail_fast)
	if args_after == ['v'] {
		eprintln('`v test v` has been deprecated.')
		eprintln('Use `v test-all` instead.')
		exit(1)
	}
	backend_pos := args_before.index('-b')
	backend := if backend_pos == -1 { '.c' } else { args_before[backend_pos + 1] } // this giant mess because closures are not implemented

	mut ts := testing.new_test_session(args_before.join(' '), true)
	ts.fail_fast = fail_fast
	for targ in args_after {
		if os.is_dir(targ) {
			// Fetch all tests from the directory
			files, skip_files := should_test_dir(targ.trim_right(os.path_separator), backend)
			ts.files << files
			ts.skip_files << skip_files
			continue
		} else if os.exists(targ) {
			match should_test(targ, backend) {
				.test {
					ts.files << targ
					continue
				}
				.skip {
					ts.files << targ
					ts.skip_files << targ
					continue
				}
				.ignore {}
			}
		} else {
			eprintln('\nUnrecognized test file `$targ`.\n `v test` can only be used with folders and/or _test.v files.\n')
			show_usage()
			exit(1)
		}
	}
	testing.header('Testing...')
	ts.test()
	println(ts.benchmark.total_message('all V _test.v files'))
	if ts.failed {
		exit(1)
	}
}

fn show_usage() {
	println('Usage:')
	println('   A)')
	println('      v test folder/ : run all v tests in the given folder.')
	println('      v -stats test folder/ : the same, but print more stats.')
	println('   B)')
	println('      v test file_test.v : run test functions in a given test file.')
	println('      v -stats test file_test.v : as above, but with more stats.')
	println('   NB: you can also give many and mixed folder/ file_test.v arguments after `v test` .')
	println('')
}

pub fn should_test_dir(path string, backend string) ([]string, []string) { // return is (files, skip_files)
	mut files := os.ls(path) or { return []string{}, []string{} }
	mut local_path_separator := os.path_separator
	if path.ends_with(os.path_separator) {
		local_path_separator = ''
	}
	mut res_files := []string{}
	mut skip_files := []string{}
	for file in files {
		p := path + local_path_separator + file
		if os.is_dir(p) && !os.is_link(p) {
			if file == 'testdata' {
				continue
			}
			ret_files, ret_skip_files := should_test_dir(p, backend)
			res_files << ret_files
			skip_files << ret_skip_files
		} else if os.exists(p) {
			match should_test(p, backend) {
				.test {
					res_files << p
				}
				.skip {
					res_files << p
					skip_files << p
				}
				.ignore {}
			}
		}
	}
	return res_files, skip_files
}

enum ShouldTestStatus {
	test // do test, print OK or FAIL, depending on if it passes
	skip // print SKIP for the test
	ignore // just ignore the file, so it will not be printed at all in the list of tests
}

fn should_test(path string, backend string) ShouldTestStatus {
	if path.ends_with('mysql_orm_test.v') {
		testing.find_started_process('mysqld') or { return .skip }
	}
	if path.ends_with('pg_orm_test.v') {
		testing.find_started_process('postgres') or { return .skip }
	}
	if path.ends_with('onecontext_test.v') {
		return .skip
	}
	$if tinyc {
		if path.ends_with('naked_attr_test.amd64.v') {
			return .skip
		}
	}
	if path.ends_with('_test.v') {
		return .test
	}
	if path.ends_with('_test.js.v') {
		if testing.is_node_present {
			return .test
		}
		return .skip
	}
	if path.ends_with('.v') && path.count('.') == 2 {
		if !path.all_before_last('.v').all_before_last('.').ends_with('_test') {
			return .ignore
		}
		backend_arg := path.all_before_last('.v').all_after_last('.')
		arch := pref.arch_from_string(backend_arg) or { pref.Arch._auto }
		if arch == pref.get_host_arch() {
			return .test
		} else if arch == ._auto {
			if backend_arg == 'c' { // .c.v
				return if backend == 'c' { ShouldTestStatus.test } else { ShouldTestStatus.skip }
			}
			if backend_arg == 'js' {
				return if backend == 'js' { ShouldTestStatus.test } else { ShouldTestStatus.skip }
			}
		} else {
			return .skip
		}
	}
	return .ignore
}

fn extract_flag(flag_name string, mut after []string, flag_default bool) bool {
	mut res := flag_default
	orig_after := after.clone() // workaround for after.filter() codegen bug, when `mut after []string`
	matches_after := orig_after.filter(it != flag_name)
	if matches_after.len < after.len {
		after = matches_after.clone()
		res = true
	}
	return res
}
