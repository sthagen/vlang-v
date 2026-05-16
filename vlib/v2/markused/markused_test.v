// vtest build: !linux && !windows
module markused

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn pos(id int) token.Pos {
	return token.Pos{
		offset: id
		id:     id
	}
}

fn mark_used_for_code(code string) map[string]bool {
	tmp_file := os.join_path(os.temp_dir(), 'v2_markused_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	return mark_used(files, env)
}

fn mark_used_for_code_files(code_files map[string]string) map[string]bool {
	return mark_used_for_code_files_with_transform(code_files, false)
}

fn mark_used_for_code_files_transformed(code_files map[string]string) map[string]bool {
	return mark_used_for_code_files_with_transform(code_files, true)
}

fn mark_used_for_code_files_with_transform(code_files map[string]string, should_transform bool) map[string]bool {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_markused_test_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic('failed to create temp dir') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{}
	for name, code in code_files {
		path := os.join_path(tmp_dir, name)
		os.mkdir_all(os.dir(path)) or { panic('failed to create temp file dir') }
		os.write_file(path, code) or { panic('failed to write temp file') }
		paths << path
	}
	prefs := &vpref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	mut files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	if should_transform {
		mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
		trans.set_file_set(file_set)
		files = trans.transform_files(files)
	}
	return mark_used(files, env)
}

fn has_used_key_containing(used map[string]bool, needle string) bool {
	for key, value in used {
		if value && key.contains(needle) {
			return true
		}
	}
	return false
}

fn test_mark_used_tracks_transitive_function_calls() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(1)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'foo'
									pos:  pos(2)
								}
								pos: pos(2)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'foo'
					typ:   ast.FnType{}
					pos:   pos(3)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'bar'
									pos:  pos(4)
								}
								pos: pos(4)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'bar'
					typ:  ast.FnType{}
					pos:  pos(5)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'dead'
					typ:  ast.FnType{}
					pos:  pos(6)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	foo_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	bar_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	dead_key := decl_key('main', files[0].stmts[3] as ast.FnDecl, env)
	assert used[main_key]
	assert used[foo_key]
	assert used[bar_key]
	assert !used[dead_key]
}

fn test_mark_used_tracks_imported_module_function_calls() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:     'searcher'
			name:    'searcher.v'
			imports: [
				ast.ImportStmt{
					name: 'matcher'
				},
			]
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name:  'test_use'
					typ:   ast.FnType{}
					pos:   pos(20)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'matcher'
										pos:  pos(21)
									}
									rhs: ast.Ident{
										name: 'byte_set_contains'
										pos:  pos(22)
									}
									pos: pos(22)
								}
								pos: pos(22)
							}
						}),
					]
				}),
			]
		},
		ast.File{
			mod:   'matcher'
			name:  'matcher.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'byte_set_contains'
					typ:  ast.FnType{}
					pos:  pos(23)
				}),
			]
		},
	]
	used := mark_used(files, env)
	test_key := decl_key('searcher', files[0].stmts[0] as ast.FnDecl, env)
	contains_key := decl_key('matcher', files[1].stmts[0] as ast.FnDecl, env)
	assert used[test_key]
	assert used[contains_key]
}

fn test_mark_used_tracks_imported_static_method_calls() {
	used := mark_used_for_code_files({
		'searcher.v': '
module searcher

import matcher

fn test_static_call() {
	_ = matcher.NoCaptures.new()
}
'
		'matcher.v':  '
module matcher

pub struct NoCaptures {}

pub fn NoCaptures.new() NoCaptures {
	return NoCaptures{}
}
'
	})
	assert has_used_key_containing(used, 'searcher|f|test_static_call')
	assert has_used_key_containing(used, 'matcher|m|NoCaptures|new')
}

fn test_mark_used_tracks_implicit_interface_call_arg_methods() {
	used := mark_used_for_code('
module main

interface Sink {
	foo() int
	bar() int
}

struct Impl {}
struct Runner {}

fn (i Impl) foo() int {
	_ = i
	return 1
}

fn (i Impl) bar() int {
	_ = i
	return 2
}

fn Impl.new() Impl {
	return Impl{}
}

fn takes(s Sink) {
	_ = s
}

fn (r Runner) takes(s Sink) {
	_ = r
	_ = s
}

fn test_interface_arg() {
	takes(Impl{})
	takes(Impl.new())
	r := Runner{}
	r.takes(Impl{})
	r.takes(Impl.new())
}
')
	assert has_used_key_containing(used, '|m|Impl|foo')
	assert has_used_key_containing(used, '|m|Impl|bar')
}

fn test_mark_used_tracks_generic_interface_call_arg_methods() {
	used := mark_used_for_code_files_transformed({
		'main.v': '
module main

interface Captures {
	len() int
	get(i int) ?int
}

struct TestCaptures {}

fn (caps TestCaptures) len() int {
	_ = caps
	return 0
}

fn (caps TestCaptures) get(i int) ?int {
	_ = caps
	_ = i
	return none
}

fn capture_match_or_panic(caps Captures, i int) int {
	return caps.get(i) or { 0 }
}

fn use_caps[T](caps T) int {
	return capture_match_or_panic(caps, 0)
}

fn test_use_caps() {
	_ = use_caps(TestCaptures{})
}
'
	})
	assert has_used_key_containing(used, '|m|TestCaptures|len')
	assert has_used_key_containing(used, '|m|TestCaptures|get')
}

fn test_mark_used_tracks_interface_arg_to_pointer_receiver_method() {
	used := mark_used_for_code_files_transformed({
		'searcher.v': '
module searcher

import matcher

struct Searcher {}
struct LiteralMatcher {}

fn LiteralMatcher.new() LiteralMatcher {
	return LiteralMatcher{}
}

fn (m LiteralMatcher) new_captures() !matcher.NoCaptures {
	_ = m
	return matcher.NoCaptures.new()
}

fn (s &Searcher) search_slice(matcher_ matcher.Matcher) ! {
	_ = s
	_ = matcher_
}

fn test_search_slice() {
	searcher_ := Searcher{}
	searcher_.search_slice(LiteralMatcher.new()) or {}
}
'
		'matcher.v':  '
module matcher

pub struct NoCaptures {}

pub interface Matcher {
	new_captures() !NoCaptures
}

pub fn NoCaptures.new() NoCaptures {
	return NoCaptures{}
}
'
	})
	assert has_used_key_containing(used, 'searcher|m|LiteralMatcher|new_captures')
	assert has_used_key_containing(used, 'matcher|m|NoCaptures|new')
}

fn test_mark_used_tracks_string_interpolation_str_method() {
	used := mark_used_for_code_files_transformed({
		'main.v': '
module main

struct Thing {
	x int
}

fn test_interpolation_str() {
	t := Thing{}
	_ = "\${t}"
}
'
	})
	assert has_used_key_containing(used, 'main|f|Thing__str')
}

fn test_mark_used_tracks_string_interpolation_str_method_for_field() {
	used := mark_used_for_code_files_transformed({
		'main.v': '
module main

struct Term {
	x int
}

struct Holder {
	term Term
}

fn test_interpolation_field_str() {
	h := Holder{}
	_ = "\${h.term}"
}
'
	})
	assert has_used_key_containing(used, 'main|f|Term__str')
}

fn test_mark_used_tracks_string_interpolation_str_method_for_receiver_field() {
	used := mark_used_for_code_files_transformed({
		'main.v': '
module main

struct Term {
	x int
}

struct Holder {
	term Term
}

fn (h Holder) msg() string {
	return "\${h.term}"
}

fn test_interpolation_receiver_field_str() {
	_ = Holder{}.msg()
}
'
	})
	assert has_used_key_containing(used, 'main|f|Term__str')
}

fn test_mark_used_tracks_string_interpolation_str_method_for_imported_field() {
	used := mark_used_for_code_files_transformed({
		'main.v':    '
module main

import matcher

struct Holder {
	term matcher.LineTerminator
}

fn (h Holder) msg() string {
	return "\${h.term}"
}

fn test_interpolation_imported_field_str() {
	_ = Holder{}.msg()
}
'
		'matcher.v': '
module matcher

pub struct LineTerminator {
	byte u8
}
'
	})
	assert has_used_key_containing(used, 'matcher|f|matcher__LineTerminator__str')
}

fn test_mark_used_tracks_result_error_return_methods() {
	used := mark_used_for_code_files_transformed({
		'main.v': '
module main

struct Term {
	x int
}

struct MyError {
	term Term
}

fn MyError.new() MyError {
	return MyError{}
}

fn (err MyError) msg() string {
	return "\${err.term}"
}

fn (err MyError) code() int {
	_ = err
	return 0
}

fn fail() ! {
	return MyError.new()
}

fn test_result_error() {
	fail() or {}
}
'
	})
	assert has_used_key_containing(used, 'main|m|MyError|msg')
	assert has_used_key_containing(used, 'main|m|MyError|code')
	assert has_used_key_containing(used, 'main|f|Term__str')
}

fn test_mark_used_tracks_method_calls_with_env_types() {
	mut env := types.Environment.new()
	env.set_expr_type(12, types.Struct{
		name: 'Widget'
	})
	files := [
		ast.File{
			mod:   'main'
			name:  'main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'main'
					typ:   ast.FnType{}
					pos:   pos(10)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'w'
										pos:  pos(12)
									}
									rhs: ast.Ident{
										name: 'ping'
										pos:  pos(13)
									}
									pos: pos(13)
								}
								pos: pos(13)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'w'
						typ:  ast.Ident{
							name: 'Widget'
							pos:  pos(14)
						}
						pos:  pos(14)
					}
					name:      'ping'
					typ:       ast.FnType{}
					pos:       pos(15)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'w'
						typ:  ast.Ident{
							name: 'Widget'
							pos:  pos(16)
						}
						pos:  pos(16)
					}
					name:      'unused'
					typ:       ast.FnType{}
					pos:       pos(17)
				}),
			]
		},
	]
	used := mark_used(files, env)
	main_key := decl_key('main', files[0].stmts[0] as ast.FnDecl, env)
	ping_key := decl_key('main', files[0].stmts[1] as ast.FnDecl, env)
	unused_key := decl_key('main', files[0].stmts[2] as ast.FnDecl, env)
	assert used[main_key]
	assert used[ping_key]
	assert !used[unused_key]
}

fn test_mark_used_tracks_transformed_generic_method_calls() {
	mut env := types.Environment.new()
	env.set_expr_type(32, types.Struct{
		name: 'FlagMapper'
	})
	env.generic_types['FlagMapper.build_schema[T]'] = [
		{
			'T': types.Type(types.Struct{
				name: 'TestSchema'
			})
		},
	]
	files := [
		ast.File{
			mod:   'ownflag'
			name:  'ownflag_test.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name:  'test_parse'
					typ:   ast.FnType{}
					pos:   pos(30)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'fm'
										pos:  pos(32)
									}
									rhs: ast.Ident{
										name: 'parse_T_ownflag_TestSchema'
										pos:  pos(33)
									}
									pos: pos(33)
								}
								pos: pos(33)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'fm'
						typ:  ast.Ident{
							name: 'FlagMapper'
							pos:  pos(34)
						}
						pos:  pos(34)
					}
					name:      'parse'
					typ:       ast.FnType{
						generic_params: [
							ast.Expr(ast.Ident{
								name: 'T'
								pos:  pos(35)
							}),
						]
					}
					pos:       pos(36)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'fm'
										pos:  pos(37)
									}
									rhs: ast.Ident{
										name: 'reset_state'
										pos:  pos(38)
									}
									pos: pos(38)
								}
								pos: pos(38)
							}
						}),
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'fm'
										pos:  pos(41)
									}
									rhs: ast.Ident{
										name: 'build_schema_T_ownflag_TestSchema'
										pos:  pos(42)
									}
									pos: pos(42)
								}
								pos: pos(42)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'fm'
						typ:  ast.Ident{
							name: 'FlagMapper'
							pos:  pos(39)
						}
						pos:  pos(39)
					}
					name:      'reset_state'
					typ:       ast.FnType{}
					pos:       pos(40)
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'fm'
						typ:  ast.Ident{
							name: 'FlagMapper'
							pos:  pos(43)
						}
						pos:  pos(43)
					}
					name:      'build_schema'
					typ:       ast.FnType{
						generic_params: [
							ast.Expr(ast.Ident{
								name: 'T'
								pos:  pos(44)
							}),
						]
					}
					pos:       pos(45)
				}),
			]
		},
	]
	used := mark_used(files, env)
	test_key := decl_key('ownflag', files[0].stmts[0] as ast.FnDecl, env)
	parse_key := decl_key('ownflag', files[0].stmts[1] as ast.FnDecl, env)
	reset_key := decl_key('ownflag', files[0].stmts[2] as ast.FnDecl, env)
	build_schema_key := decl_key('ownflag', files[0].stmts[3] as ast.FnDecl, env)
	assert used[test_key]
	assert used[parse_key]
	assert used[reset_key]
	assert used[build_schema_key]
}

fn test_mark_used_tracks_lifetime_generic_receiver_body_dependencies() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'globset'
			name:  'glob.v'
			stmts: [
				ast.Stmt(ast.StructDecl{
					name: 'GlobBuilder'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Tokens'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'test_build'
					typ:   ast.FnType{}
					pos:   pos(50)
					stmts: [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.Ident{
									name: 'globset__GlobBuilder__build'
									pos:  pos(51)
								}
								pos: pos(51)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					receiver:  ast.Parameter{
						name: 'builder'
						typ:  ast.Type(ast.GenericType{
							name:   ast.Expr(ast.Ident{
								name: 'GlobBuilder'
								pos:  pos(52)
							})
							params: [
								ast.Expr(ast.LifetimeExpr{
									name: 'a'
									pos:  pos(53)
								}),
							]
						})
						pos:  pos(52)
					}
					name:      'build'
					typ:       ast.FnType{}
					pos:       pos(54)
					stmts:     [
						ast.Stmt(ast.ExprStmt{
							expr: ast.CallExpr{
								lhs: ast.SelectorExpr{
									lhs: ast.Ident{
										name: 'Tokens'
										pos:  pos(55)
									}
									rhs: ast.Ident{
										name: 'default'
										pos:  pos(56)
									}
									pos: pos(56)
								}
								pos: pos(56)
							}
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					is_method: true
					is_static: true
					receiver:  ast.Parameter{
						typ: ast.Ident{
							name: 'Tokens'
							pos:  pos(57)
						}
						pos: pos(57)
					}
					name:      'default'
					typ:       ast.FnType{}
					pos:       pos(58)
				}),
			]
		},
	]
	used := mark_used(files, env)
	test_key := decl_key('globset', files[0].stmts[2] as ast.FnDecl, env)
	build_key := decl_key('globset', files[0].stmts[3] as ast.FnDecl, env)
	default_key := decl_key('globset', files[0].stmts[4] as ast.FnDecl, env)
	assert used[test_key]
	assert used[build_key]
	assert used[default_key]
}

fn test_mark_used_tracks_struct_operator_overload_from_infix_expr() {
	used := mark_used_for_code('
struct Stats {
	n int
}

fn (left Stats) + (right Stats) Stats {
	return Stats{
		n: left.n + right.n
	}
}

fn test_stats_plus() {
	left := Stats{
		n: 1
	}
	right := Stats{
		n: 2
	}
	_ = left + right
}
')
	assert has_used_key_containing(used, 'main|m|Stats|+')
}

fn test_mark_used_keeps_all_functions_when_no_entry_root_exists() {
	mut env := types.Environment.new()
	files := [
		ast.File{
			mod:   'mylib'
			name:  'lib.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'a'
					typ:  ast.FnType{}
					pos:  pos(21)
				}),
				ast.Stmt(ast.FnDecl{
					name: 'b'
					typ:  ast.FnType{}
					pos:  pos(22)
				}),
			]
		},
	]
	used := mark_used(files, env)
	a_key := decl_key('mylib', files[0].stmts[0] as ast.FnDecl, env)
	b_key := decl_key('mylib', files[0].stmts[1] as ast.FnDecl, env)
	assert used[a_key]
	assert used[b_key]
}
