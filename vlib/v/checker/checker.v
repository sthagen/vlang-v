// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import os
import v.ast
import v.vmod
import v.token
import v.pref
import v.util
import v.util.version
import v.errors
import v.pkgconfig

const int_min = int(0x80000000)

const int_max = int(0x7FFFFFFF)

const (
	valid_comptime_if_os             = ['windows', 'ios', 'macos', 'mach', 'darwin', 'hpux', 'gnu',
		'qnx', 'linux', 'freebsd', 'openbsd', 'netbsd', 'bsd', 'dragonfly', 'android', 'solaris',
		'haiku', 'serenity', 'vinix']
	valid_comptime_compression_types = ['none', 'zlib']
	valid_comptime_if_compilers      = ['gcc', 'tinyc', 'clang', 'mingw', 'msvc', 'cplusplus']
	valid_comptime_if_platforms      = ['amd64', 'i386', 'aarch64', 'arm64', 'arm32', 'rv64', 'rv32']
	valid_comptime_if_cpu_features   = ['x64', 'x32', 'little_endian', 'big_endian']
	valid_comptime_if_other          = ['js', 'debug', 'prod', 'test', 'glibc', 'prealloc',
		'no_bounds_checking', 'freestanding', 'threads', 'js_node', 'js_browser', 'js_freestanding',
		'interpreter']
	valid_comptime_not_user_defined  = all_valid_comptime_idents()
	array_builtin_methods            = ['filter', 'clone', 'repeat', 'reverse', 'map', 'slice',
		'sort', 'contains', 'index', 'wait', 'any', 'all', 'first', 'last', 'pop']
	reserved_type_names              = ['bool', 'char', 'i8', 'i16', 'int', 'i64', 'byte', 'u16',
		'u32', 'u64', 'f32', 'f64', 'map', 'string', 'rune']
	vroot_is_deprecated_message      = '@VROOT is deprecated, use @VMODROOT or @VEXEROOT instead'
)

fn all_valid_comptime_idents() []string {
	mut res := []string{}
	res << checker.valid_comptime_if_os
	res << checker.valid_comptime_if_compilers
	res << checker.valid_comptime_if_platforms
	res << checker.valid_comptime_if_cpu_features
	res << checker.valid_comptime_if_other
	return res
}

[heap]
pub struct Checker {
	pref &pref.Preferences // Preferences shared from V struct
pub mut:
	table            &ast.Table
	file             &ast.File = 0
	nr_errors        int
	nr_warnings      int
	nr_notices       int
	should_abort     bool // when too many errors/warnings/notices are accumulated, .should_abort becomes true. It is checked in statement/expression loops, so the checker can return early, instead of wasting time.
	errors           []errors.Error
	warnings         []errors.Warning
	notices          []errors.Notice
	error_lines      []int // to avoid printing multiple errors for the same line
	expected_type    ast.Type
	expected_or_type ast.Type // fn() or { 'this type' } eg. string. expected or block type
	const_decl       string
	const_deps       []string
	const_names      []string
	global_names     []string
	locked_names     []string // vars that are currently locked
	rlocked_names    []string // vars that are currently read-locked
	in_for_count     int      // if checker is currently in a for loop
	// checked_ident  string // to avoid infinite checker loops
	returns        bool
	scope_returns  bool
	mod            string // current module name
	is_builtin_mod bool   // true inside the 'builtin', 'os' or 'strconv' modules; TODO: remove the need for special casing this
	is_generated   bool   // true for `[generated] module xyz` .v files
	inside_unsafe  bool   // true inside `unsafe {}` blocks
	inside_const   bool   // true inside `const ( ... )` blocks
	inside_anon_fn bool   // true inside `fn() { ... }()`
	inside_ref_lit bool   // true inside `a := &something`
	inside_defer   bool   // true inside `defer {}` blocks
	inside_fn_arg  bool   // `a`, `b` in `a.f(b)`
	inside_ct_attr bool   // true inside `[if expr]`
	skip_flags     bool   // should `#flag` and `#include` be skipped
	fn_level       int    // 0 for the top level, 1 for `fn abc() {}`, 2 for a nested fn, etc
	ct_cond_stack  []ast.Expr
mut:
	stmt_level int // the nesting level inside each stmts list;
	// .stmt_level is used to check for `evaluated but not used` ExprStmts like `1 << 1`
	// 1 for statements directly at each inner scope level;
	// increases for `x := if cond { statement_list1} else {statement_list2}`;
	// increases for `x := optfn() or { statement_list3 }`;
	is_last_stmt                     bool
	files                            []ast.File
	expr_level                       int  // to avoid infinite recursion segfaults due to compiler bugs
	inside_sql                       bool // to handle sql table fields pseudo variables
	cur_orm_ts                       ast.TypeSymbol
	error_details                    []string
	vmod_file_content                string     // needed for @VMOD_FILE, contents of the file, *NOT its path**
	vweb_gen_types                   []ast.Type // vweb route checks
	prevent_sum_type_unwrapping_once bool       // needed for assign new values to sum type, stopping unwrapping then
	loop_label                       string     // set when inside a labelled for loop
	timers                           &util.Timers = util.get_timers()
	comptime_fields_default_type     ast.Type
	comptime_fields_type             map[string]ast.Type
	fn_scope                         &ast.Scope = voidptr(0)
	main_fn_decl_node                ast.FnDecl
	match_exhaustive_cutoff_limit    int = 10
	// TODO: these are here temporarily and used for deprecations; remove soon
	using_new_err_struct     bool
	inside_selector_expr     bool
	inside_println_arg       bool
	inside_decl_rhs          bool
	inside_if_guard          bool // true inside the guard condition of `if x := opt() {}`
	need_recheck_generic_fns bool // need recheck generic fns because there are cascaded nested generic fn
}

pub fn new_checker(table &ast.Table, pref &pref.Preferences) &Checker {
	mut timers_should_print := false
	$if time_checking ? {
		timers_should_print = true
	}
	return &Checker{
		table: table
		pref: pref
		timers: util.new_timers(should_print: timers_should_print, label: 'checker')
		match_exhaustive_cutoff_limit: pref.checker_match_exhaustive_cutoff_limit
	}
}

fn (mut c Checker) reset_checker_state_at_start_of_new_file() {
	c.expected_type = ast.void_type
	c.expected_or_type = ast.void_type
	c.const_decl = ''
	c.in_for_count = 0
	c.returns = false
	c.scope_returns = false
	c.mod = ''
	c.is_builtin_mod = false
	c.inside_unsafe = false
	c.inside_const = false
	c.inside_anon_fn = false
	c.inside_ref_lit = false
	c.inside_defer = false
	c.inside_fn_arg = false
	c.inside_ct_attr = false
	c.skip_flags = false
	c.fn_level = 0
	c.expr_level = 0
	c.stmt_level = 0
	c.inside_sql = false
	c.cur_orm_ts = ast.TypeSymbol{}
	c.prevent_sum_type_unwrapping_once = false
	c.loop_label = ''
	c.using_new_err_struct = false
	c.inside_selector_expr = false
	c.inside_println_arg = false
	c.inside_decl_rhs = false
	c.inside_if_guard = false
}

pub fn (mut c Checker) check(ast_file_ &ast.File) {
	mut ast_file := ast_file_
	c.reset_checker_state_at_start_of_new_file()
	c.change_current_file(ast_file)
	for i, ast_import in ast_file.imports {
		for sym in ast_import.syms {
			full_name := ast_import.mod + '.' + sym.name
			if full_name in c.const_names {
				c.error('cannot selectively import constant `$sym.name` from `$ast_import.mod`, import `$ast_import.mod` and use `$full_name` instead',
					sym.pos)
			}
		}
		for j in 0 .. i {
			if ast_import.mod == ast_file.imports[j].mod {
				c.error('`$ast_import.mod` was already imported on line ${
					ast_file.imports[j].mod_pos.line_nr + 1}', ast_import.mod_pos)
			}
		}
	}
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt in [ast.ConstDecl, ast.ExprStmt] {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt is ast.GlobalDecl {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.stmt_level = 0
	for mut stmt in ast_file.stmts {
		if stmt !is ast.ConstDecl && stmt !is ast.GlobalDecl && stmt !is ast.ExprStmt {
			c.expr_level = 0
			c.stmt(stmt)
		}
		if c.should_abort {
			return
		}
	}
	//
	c.check_scope_vars(c.file.scope)
}

pub fn (mut c Checker) check_scope_vars(sc &ast.Scope) {
	if !c.pref.is_repl && !c.file.is_test {
		for _, obj in sc.objects {
			match obj {
				ast.Var {
					if !obj.is_used && obj.name[0] != `_` {
						c.warn('unused variable: `$obj.name`', obj.pos)
					}
					if obj.is_mut && !obj.is_changed && !c.is_builtin_mod && obj.name != 'it' {
						// if obj.is_mut && !obj.is_changed && !c.is_builtin {  //TODO C error bad field not checked
						// c.warn('`$obj.name` is declared as mutable, but it was never changed',
						// obj.pos)
					}
				}
				else {}
			}
		}
	}
	for child in sc.children {
		c.check_scope_vars(child)
	}
}

// not used right now
pub fn (mut c Checker) check2(ast_file &ast.File) []errors.Error {
	c.change_current_file(ast_file)
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
	return c.errors
}

pub fn (mut c Checker) change_current_file(file &ast.File) {
	c.file = unsafe { file }
	c.vmod_file_content = ''
	c.mod = file.mod.name
	c.is_generated = file.is_generated
}

pub fn (mut c Checker) check_files(ast_files []&ast.File) {
	// c.files = ast_files
	mut has_main_mod_file := false
	mut has_main_fn := false
	mut files_from_main_module := []&ast.File{}
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
		c.timers.start('checker_check $file.path')
		c.check(file)
		if file.mod.name == 'main' {
			files_from_main_module << file
			has_main_mod_file = true
			if c.file_has_main_fn(file) {
				has_main_fn = true
			}
		}
		c.timers.show('checker_check $file.path')
	}
	if has_main_mod_file && !has_main_fn && files_from_main_module.len > 0 {
		if c.pref.is_script && !c.pref.is_test {
			// files_from_main_module contain preludes at the start
			mut the_main_file := files_from_main_module.last()
			the_main_file.stmts << ast.FnDecl{
				name: 'main.main'
				mod: 'main'
				is_main: true
				file: the_main_file.path
				return_type: ast.void_type
				scope: &ast.Scope{
					parent: 0
				}
			}
			has_main_fn = true
		}
	}
	c.timers.start('checker_post_process_generic_fns')
	last_file := c.file
	// post process generic functions. must be done after all files have been
	// checked, to eunsure all generic calls are processed as this information
	// is needed when the generic type is auto inferred from the call argument
	// Check more times if there are more new registered fn concrete types
	for {
		for file in ast_files {
			if file.generic_fns.len > 0 {
				c.change_current_file(file)
				c.post_process_generic_fns()
			}
		}
		if !c.need_recheck_generic_fns {
			break
		}
		c.need_recheck_generic_fns = false
	}
	// restore the original c.file && c.mod after post processing
	c.change_current_file(last_file)
	c.timers.show('checker_post_process_generic_fns')

	c.timers.start('checker_verify_all_vweb_routes')
	c.verify_all_vweb_routes()
	c.timers.show('checker_verify_all_vweb_routes')

	if c.pref.is_test {
		mut n_test_fns := 0
		for _, f in c.table.fns {
			if f.is_test {
				n_test_fns++
			}
		}
		if n_test_fns == 0 {
			c.add_error_detail('The name of a test function in V, should start with `test_`.')
			c.add_error_detail('The test function should take 0 parameters, and no return type. Example:')
			c.add_error_detail('fn test_xyz(){ assert 2 + 2 == 4 }')
			c.error('a _test.v file should have *at least* one `test_` function', token.Position{})
		}
	}
	// Make sure fn main is defined in non lib builds
	if c.pref.build_mode == .build_module || c.pref.is_test {
		return
	}
	if c.pref.is_shared {
		// shared libs do not need to have a main
		return
	}
	if c.pref.no_builtin {
		// `v -no-builtin module/` do not necessarily need to have a `main` function
		// This is useful for compiling linux kernel modules for example.
		return
	}
	if !has_main_mod_file {
		c.error('project must include a `main` module or be a shared library (compile with `v -shared`)',
			token.Position{})
	} else if !has_main_fn {
		c.error('function `main` must be declared in the main module', token.Position{})
	}
}

// do checks specific to files in main module
// returns `true` if a main function is in the file
fn (mut c Checker) file_has_main_fn(file &ast.File) bool {
	mut has_main_fn := false
	for stmt in file.stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.main' {
				if has_main_fn {
					c.error('function `main` is already defined', stmt.pos)
				}
				has_main_fn = true
				if stmt.params.len > 0 {
					c.error('function `main` cannot have arguments', stmt.pos)
				}
				if stmt.return_type != ast.void_type {
					c.error('function `main` cannot return values', stmt.pos)
				}
				if stmt.no_body {
					c.error('function `main` must declare a body', stmt.pos)
				}
			} else if stmt.attrs.contains('console') {
				c.error('only `main` can have the `[console]` attribute', stmt.pos)
			}
		}
	}
	return has_main_fn
}

fn (mut c Checker) check_valid_snake_case(name string, identifier string, pos token.Position) {
	if !c.pref.is_vweb && !c.pref.translated && name.len > 0
		&& (name[0] == `_` || name.contains('._')) {
		c.error('$identifier `$name` cannot start with `_`', pos)
	}
	if !c.pref.experimental && !c.pref.translated && util.contains_capital(name) {
		c.error('$identifier `$name` cannot contain uppercase letters, use snake_case instead',
			pos)
	}
}

fn stripped_name(name string) string {
	idx := name.last_index('.') or { -1 }
	return name[(idx + 1)..]
}

fn (mut c Checker) check_valid_pascal_case(name string, identifier string, pos token.Position) {
	sname := stripped_name(name)
	if sname.len > 0 && !sname[0].is_capital() && !c.pref.translated {
		c.error('$identifier `$name` must begin with capital letter', pos)
	}
}

pub fn (mut c Checker) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl { c.alias_type_decl(node) }
		ast.FnTypeDecl { c.fn_type_decl(node) }
		ast.SumTypeDecl { c.sum_type_decl(node) }
	}
}

pub fn (mut c Checker) alias_type_decl(node ast.AliasTypeDecl) {
	// TODO Remove when `u8` isn't an alias in builtin anymore
	if c.file.mod.name != 'builtin' {
		c.check_valid_pascal_case(node.name, 'type alias', node.pos)
	}
	c.ensure_type_exists(node.parent_type, node.type_pos) or { return }
	typ_sym := c.table.get_type_symbol(node.parent_type)
	if typ_sym.kind in [.placeholder, .int_literal, .float_literal] {
		c.error('unknown type `$typ_sym.name`', node.type_pos)
	} else if typ_sym.kind == .alias {
		orig_sym := c.table.get_type_symbol((typ_sym.info as ast.Alias).parent_type)
		c.error('type `$typ_sym.str()` is an alias, use the original alias type `$orig_sym.name` instead',
			node.type_pos)
	} else if typ_sym.kind == .chan {
		c.error('aliases of `chan` types are not allowed.', node.type_pos)
	}
}

pub fn (mut c Checker) fn_type_decl(node ast.FnTypeDecl) {
	c.check_valid_pascal_case(node.name, 'fn type', node.pos)
	typ_sym := c.table.get_type_symbol(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	c.ensure_type_exists(fn_info.return_type, fn_info.return_type_pos) or {}
	ret_sym := c.table.get_type_symbol(fn_info.return_type)
	if ret_sym.kind == .placeholder {
		c.error('unknown type `$ret_sym.name`', fn_info.return_type_pos)
	}
	for arg in fn_info.params {
		c.ensure_type_exists(arg.typ, arg.type_pos) or { return }
		arg_sym := c.table.get_type_symbol(arg.typ)
		if arg_sym.kind == .placeholder {
			c.error('unknown type `$arg_sym.name`', arg.type_pos)
		}
	}
}

pub fn (mut c Checker) sum_type_decl(node ast.SumTypeDecl) {
	c.check_valid_pascal_case(node.name, 'sum type', node.pos)
	mut names_used := []string{}
	for variant in node.variants {
		if variant.typ.is_ptr() {
			c.error('sum type cannot hold a reference type', variant.pos)
		}
		c.ensure_type_exists(variant.typ, variant.pos) or {}
		mut sym := c.table.get_type_symbol(variant.typ)
		if sym.name in names_used {
			c.error('sum type $node.name cannot hold the type `$sym.name` more than once',
				variant.pos)
		} else if sym.kind in [.placeholder, .int_literal, .float_literal] {
			c.error('unknown type `$sym.name`', variant.pos)
		} else if sym.kind == .interface_ && sym.language != .js {
			c.error('sum type cannot hold an interface', variant.pos)
		} else if sym.kind == .struct_ && sym.language == .js {
			c.error('sum type cannot hold an JS struct', variant.pos)
		}
		if sym.name.trim_prefix(sym.mod + '.') == node.name {
			c.error('sum type cannot hold itself', variant.pos)
		}
		names_used << sym.name
	}
}

pub fn (mut c Checker) expand_iface_embeds(idecl &ast.InterfaceDecl, level int, iface_embeds []ast.InterfaceEmbedding) []ast.InterfaceEmbedding {
	// eprintln('> expand_iface_embeds: idecl.name: $idecl.name | level: $level | iface_embeds.len: $iface_embeds.len')
	if level > 100 {
		c.error('too many interface embedding levels: $level, for interface `$idecl.name`',
			idecl.pos)
		return []
	}
	if iface_embeds.len == 0 {
		return []
	}
	mut res := map[int]ast.InterfaceEmbedding{}
	mut ares := []ast.InterfaceEmbedding{}
	for ie in iface_embeds {
		if iface_decl := c.table.interfaces[ie.typ] {
			mut list := iface_decl.ifaces
			if !iface_decl.are_ifaces_expanded {
				list = c.expand_iface_embeds(idecl, level + 1, iface_decl.ifaces)
				c.table.interfaces[ie.typ].ifaces = list
				c.table.interfaces[ie.typ].are_ifaces_expanded = true
			}
			for partial in list {
				res[partial.typ] = partial
			}
		}
		res[ie.typ] = ie
	}
	for _, v in res {
		ares << v
	}
	return ares
}

pub fn (mut c Checker) struct_decl(mut node ast.StructDecl) {
	if node.language == .v && !c.is_builtin_mod {
		c.check_valid_pascal_case(node.name, 'struct name', node.pos)
	}
	mut struct_sym := c.table.find_type(node.name) or { ast.invalid_type_symbol }
	mut has_generic_types := false
	if mut struct_sym.info is ast.Struct {
		for embed in node.embeds {
			if embed.typ.has_flag(.generic) {
				has_generic_types = true
			}
			embed_sym := c.table.get_type_symbol(embed.typ)
			if embed_sym.kind != .struct_ {
				c.error('`$embed_sym.name` is not a struct', embed.pos)
			} else {
				info := embed_sym.info as ast.Struct
				if info.is_heap && !embed.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
		}
		for attr in node.attrs {
			if attr.name == 'typedef' && node.language != .c {
				c.error('`typedef` attribute can only be used with C structs', node.pos)
			}
		}
		for i, field in node.fields {
			if field.typ == ast.any_type {
				c.error('struct field cannot be the `any` type, use generics instead',
					field.type_pos)
			}
			c.ensure_type_exists(field.typ, field.type_pos) or { return }
			if field.typ.has_flag(.generic) {
				has_generic_types = true
			}
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.get_type_symbol(field.typ)
			for j in 0 .. i {
				if field.name == node.fields[j].name {
					c.error('field name `$field.name` duplicate', field.pos)
				}
			}
			if sym.kind == .struct_ {
				info := sym.info as ast.Struct
				if info.is_heap && !field.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
			if field.has_default_expr {
				c.expected_type = field.typ
				mut field_expr_type := c.expr(field.default_expr)
				if !field.typ.has_flag(.optional) {
					c.check_expr_opt_call(field.default_expr, field_expr_type)
				}
				struct_sym.info.fields[i].default_expr_typ = field_expr_type
				c.check_expected(field_expr_type, field.typ) or {
					if sym.kind == .interface_
						&& c.type_implements(field_expr_type, field.typ, field.pos) {
						if !field_expr_type.is_ptr() && !field_expr_type.is_pointer()
							&& !c.inside_unsafe {
							field_expr_type_sym := c.table.get_type_symbol(field_expr_type)
							if field_expr_type_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.fields[i].default_expr,
									true)
							}
						}
					} else {
						c.error('incompatible initializer for field `$field.name`: $err.msg',
							field.default_expr.position())
					}
				}
				// Check for unnecessary inits like ` = 0` and ` = ''`
				if field.typ.is_ptr() {
					continue
				}
				if field.default_expr is ast.IntegerLiteral {
					if field.default_expr.val == '0' {
						c.warn('unnecessary default value of `0`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.StringLiteral {
					if field.default_expr.val == '' {
						c.warn("unnecessary default value of '': struct fields are zeroed by default",
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.BoolLiteral {
					if field.default_expr.val == false {
						c.warn('unnecessary default value `false`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				}
			}
		}
		if node.generic_types.len == 0 && has_generic_types {
			c.error('generic struct declaration must specify the generic type names, e.g. Foo<T>',
				node.pos)
		}
	}
}

pub fn (mut c Checker) struct_init(mut node ast.StructInit) ast.Type {
	if node.typ == ast.void_type {
		// Short syntax `({foo: bar})`
		if c.expected_type == ast.void_type {
			c.error('unexpected short struct syntax', node.pos)
			return ast.void_type
		}
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .array {
			node.typ = c.table.value_type(c.expected_type)
		} else {
			node.typ = c.expected_type
		}
	}
	struct_sym := c.table.get_type_symbol(node.typ)
	if struct_sym.info is ast.Struct {
		if struct_sym.info.generic_types.len > 0 && struct_sym.info.concrete_types.len == 0
			&& c.table.cur_concrete_types.len == 0 {
			c.error('generic struct init must specify type parameter, e.g. Foo<int>',
				node.pos)
		}
	} else if struct_sym.info is ast.Alias {
		parent_sym := c.table.get_type_symbol(struct_sym.info.parent_type)
		// e.g. ´x := MyMapAlias{}´, should be a cast to alias type ´x := MyMapAlias(map[...]...)´
		if parent_sym.kind == .map {
			alias_str := c.table.type_to_str(node.typ)
			map_str := c.table.type_to_str(struct_sym.info.parent_type)
			c.error('direct map alias init is not possible, use `${alias_str}($map_str{})` instead',
				node.pos)
			return ast.void_type
		}
	}
	// register generic struct type when current fn is generic fn
	if c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(node.typ, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	c.ensure_type_exists(node.typ, node.pos) or {}
	type_sym := c.table.get_type_symbol(node.typ)
	if !c.inside_unsafe && type_sym.kind == .sum_type {
		c.note('direct sum type init (`x := SumType{}`) will be removed soon', node.pos)
	}
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.table.cur_concrete_types.len == 0 {
		pos := type_sym.name.last_index('.') or { -1 }
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() {
			c.error('cannot initialize builtin type `$type_sym.name`', node.pos)
		}
	}
	if type_sym.kind == .sum_type && node.fields.len == 1 {
		sexpr := node.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}($sexpr)` not `$type_sym.name{$sexpr}`',
			node.pos)
	}
	if type_sym.kind == .interface_ && type_sym.language != .js {
		c.error('cannot instantiate interface `$type_sym.name`', node.pos)
	}
	if type_sym.info is ast.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.name`', node.pos)
			return ast.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !type_sym.is_public && type_sym.kind != .placeholder && type_sym.language != .c
		&& (type_sym.mod != c.mod && !(node.typ.has_flag(.generic) && type_sym.mod != 'builtin')) {
		c.error('type `$type_sym.name` is private', node.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if info.attrs.len > 0 && info.attrs[0].name == 'noinit' && type_sym.mod != c.mod {
			c.error('struct `$type_sym.name` is declared with a `[noinit]` attribute, so ' +
				'it cannot be initialized with `$type_sym.name{}`', node.pos)
		}
	}
	if type_sym.name.len == 1 && c.table.cur_fn.generic_names.len == 0 {
		c.error('unknown struct `$type_sym.name`', node.pos)
		return 0
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.name', node.pos)
			return ast.void_type
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.get_type_symbol(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.name', node.pos)
					return ast.void_type
				}
				if sym.kind == .struct_ {
					info = sym.info as ast.Struct
				} else {
					c.error('alias type name: $sym.name is not struct type', node.pos)
				}
			} else {
				info = type_sym.info as ast.Struct
			}
			if node.is_short {
				exp_len := info.fields.len
				got_len := node.fields.len
				if exp_len != got_len {
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.name` literal (expecting $exp_len, got $got_len)',
						node.pos)
				}
			}
			mut inited_fields := []string{}
			for i, mut field in node.fields {
				mut field_info := ast.StructField{}
				mut field_name := ''
				if node.is_short {
					if i >= info.fields.len {
						// It doesn't make sense to check for fields that don't exist.
						// We should just stop here.
						break
					}
					field_info = info.fields[i]
					field_name = field_info.name
					node.fields[i].name = field_name
				} else {
					field_name = field.name
					mut exists := true
					field_info = c.table.find_field_with_embeds(type_sym, field_name) or {
						exists = false
						ast.StructField{}
					}
					if !exists {
						c.error('unknown field `$field.name` in struct literal of type `$type_sym.name`',
							field.pos)
						continue
					}
					if field_name in inited_fields {
						c.error('duplicate field name in struct literal: `$field_name`',
							field.pos)
						continue
					}
				}
				mut expr_type := ast.Type(0)
				mut expected_type := ast.Type(0)
				inited_fields << field_name
				field_type_sym := c.table.get_type_symbol(field_info.typ)
				expected_type = field_info.typ
				c.expected_type = expected_type
				expr_type = c.expr(field.expr)
				if !field_info.typ.has_flag(.optional) {
					expr_type = c.check_expr_opt_call(field.expr, expr_type)
				}
				expr_type_sym := c.table.get_type_symbol(expr_type)
				if field_type_sym.kind == .interface_ {
					if c.type_implements(expr_type, field_info.typ, field.pos) {
						if !expr_type.is_ptr() && !expr_type.is_pointer()
							&& expr_type_sym.kind != .interface_ && !c.inside_unsafe {
							c.mark_as_referenced(mut &field.expr, true)
						}
					}
				} else if expr_type != ast.void_type && expr_type_sym.kind != .placeholder {
					c.check_expected(c.unwrap_generic(expr_type), c.unwrap_generic(field_info.typ)) or {
						c.error('cannot assign to field `$field_info.name`: $err.msg',
							field.pos)
					}
				}
				if field_info.typ.has_flag(.shared_f) {
					if !expr_type.has_flag(.shared_f) && expr_type.is_ptr() {
						c.error('`shared` field must be initialized with `shared` or value',
							field.pos)
					}
				} else {
					if field_info.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer()
						&& !expr_type.is_number() {
						c.error('reference field must be initialized with reference',
							field.pos)
					}
				}
				node.fields[i].typ = expr_type
				node.fields[i].expected_type = field_info.typ

				if field_info.typ.has_flag(.optional) {
					c.error('field `$field_info.name` is optional, but initialization of optional fields currently unsupported',
						field.pos)
				}
				if expr_type.is_ptr() && expected_type.is_ptr() {
					if mut field.expr is ast.Ident {
						if mut field.expr.obj is ast.Var {
							mut obj := unsafe { &field.expr.obj }
							if c.fn_scope != voidptr(0) {
								obj = c.fn_scope.find_var(obj.name) or { obj }
							}
							if obj.is_stack_obj && !c.inside_unsafe {
								sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
								if !sym.is_heap() && !c.pref.translated {
									suggestion := if sym.kind == .struct_ {
										'declaring `$sym.name` as `[heap]`'
									} else {
										'wrapping the `$sym.name` object in a `struct` declared as `[heap]`'
									}
									c.error('`$field.expr.name` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
										field.expr.pos)
								}
							}
						}
					}
				}
			}
			// Check uninitialized refs/sum types
			for field in info.fields {
				if field.has_default_expr || field.name in inited_fields {
					continue
				}
				if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !node.has_update_expr
					&& !c.pref.translated {
					c.error('reference field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized interfaces
				sym := c.table.get_type_symbol(field.typ)
				if sym.kind == .interface_ {
					// TODO: should be an error instead, but first `ui` needs updating.
					c.note('interface field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized sum types
				/*
				sym := c.table.get_type_symbol(field.typ)
				if sym.kind == .sum_type {
					c.warn('sum type field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				*/
				// Check for `[required]` struct attr
				if field.attrs.contains('required') && !node.is_short && !node.has_update_expr {
					mut found := false
					for init_field in node.fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.name}.$field.name` must be initialized',
							node.pos)
					}
				}
			}
		}
		else {}
	}
	if node.has_update_expr {
		update_type := c.expr(node.update_expr)
		node.update_expr_type = update_type
		if c.table.type_kind(update_type) != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `$s`', node.update_expr.position())
		} else if update_type != node.typ {
			from_sym := c.table.get_type_symbol(update_type)
			to_sym := c.table.get_type_symbol(node.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info) {
				c.error('struct `$from_sym.name` is not compatible with struct `$to_sym.name`',
					node.update_expr.position())
			}
		}
		if !node.update_expr.is_lvalue() {
			// cgen will repeat `update_expr` for each field
			// so enforce an lvalue for efficiency
			c.error('expression is not an lvalue', node.update_expr.position())
		}
	}
	return node.typ
}

fn (mut c Checker) check_div_mod_by_zero(expr ast.Expr, op_kind token.Kind) {
	match mut expr {
		ast.FloatLiteral {
			if expr.val.f64() == 0.0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.IntegerLiteral {
			if expr.val.int() == 0 {
				oper := if op_kind == .div { 'division' } else { 'modulo' }
				c.error('$oper by zero', expr.pos)
			}
		}
		ast.CastExpr {
			c.check_div_mod_by_zero(expr.expr, op_kind)
		}
		else {}
	}
}

pub fn (mut c Checker) infix_expr(mut node ast.InfixExpr) ast.Type {
	former_expected_type := c.expected_type
	defer {
		c.expected_type = former_expected_type
	}
	left_type := c.expr(node.left)
	node.left_type = left_type
	c.expected_type = left_type
	right_type := c.expr(node.right)
	node.right_type = right_type
	if left_type.is_number() && !left_type.is_ptr()
		&& right_type in [ast.int_literal_type, ast.float_literal_type] {
		node.right_type = left_type
	}
	if right_type.is_number() && !right_type.is_ptr()
		&& left_type in [ast.int_literal_type, ast.float_literal_type] {
		node.left_type = right_type
	}
	mut right_sym := c.table.get_type_symbol(right_type)
	right_final := c.table.get_final_type_symbol(right_type)
	mut left_sym := c.table.get_type_symbol(left_type)
	left_final := c.table.get_final_type_symbol(left_type)
	left_pos := node.left.position()
	right_pos := node.right.position()
	left_right_pos := left_pos.extend(right_pos)
	if left_type.is_any_kind_of_pointer()
		&& node.op in [.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe] {
		if (right_type.is_any_kind_of_pointer() && node.op != .minus)
			|| (!right_type.is_any_kind_of_pointer() && node.op !in [.plus, .minus]) {
			left_name := c.table.type_to_str(left_type)
			right_name := c.table.type_to_str(right_type)
			c.error('invalid operator `$node.op` to `$left_name` and `$right_name`', left_right_pos)
		} else if node.op in [.plus, .minus] {
			if !c.inside_unsafe && !node.left.is_auto_deref_var() && !node.right.is_auto_deref_var() {
				c.warn('pointer arithmetic is only allowed in `unsafe` blocks', left_right_pos)
			}
			if left_type == ast.voidptr_type {
				c.error('`$node.op` cannot be used with `voidptr`', left_pos)
			}
		}
	}
	mut return_type := left_type
	if node.op != .key_is {
		match mut node.left {
			ast.Ident, ast.SelectorExpr {
				if node.left.is_mut {
					c.error('remove unnecessary `mut`', node.left.mut_pos)
				}
			}
			else {}
		}
	}
	eq_ne := node.op in [.eq, .ne]
	// Single side check
	// Place these branches according to ops' usage frequency to accelerate.
	// TODO: First branch includes ops where single side check is not needed, or needed but hasn't been implemented.
	// TODO: Some of the checks are not single side. Should find a better way to organize them.
	match node.op {
		// .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .dot, .key_as, .right_shift {}
		.eq, .ne {
			is_mismatch :=
				(left_sym.kind == .alias && right_sym.kind in [.struct_, .array, .sum_type])
				|| (right_sym.kind == .alias && left_sym.kind in [.struct_, .array, .sum_type])
			if is_mismatch {
				c.error('possible type mismatch of compared values of `$node.op` operation',
					left_right_pos)
			}
		}
		.key_in, .not_in {
			match right_final.kind {
				.array {
					if left_sym.kind !in [.sum_type, .interface_] {
						elem_type := right_final.array_info().elem_type
						c.check_expected(left_type, elem_type) or {
							c.error('left operand to `$node.op` does not match the array element type: $err.msg',
								left_right_pos)
						}
					}
				}
				.map {
					map_info := right_final.map_info()
					c.check_expected(left_type, map_info.key_type) or {
						c.error('left operand to `$node.op` does not match the map key type: $err.msg',
							left_right_pos)
					}
					node.left_type = map_info.key_type
				}
				else {
					c.error('`$node.op.str()` can only be used with an array/map/string',
						node.pos)
				}
			}
			return ast.bool_type
		}
		.plus, .minus, .mul, .div, .mod, .xor, .amp, .pipe { // binary operators that expect matching types
			if right_sym.info is ast.Alias && (right_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(right_type).split('.')[0]
				&& c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type).is_primitive() {
				right_sym = c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type)
			}
			if left_sym.info is ast.Alias && (left_sym.info as ast.Alias).language != .c
				&& c.mod == c.table.type_to_str(left_type).split('.')[0]
				&& c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type).is_primitive() {
				left_sym = c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type)
			}
			// Check if the alias type is not a primitive then allow using operator overloading for aliased `arrays` and `maps`
			if left_sym.kind == .alias && left_sym.info is ast.Alias
				&& !(c.table.get_type_symbol((left_sym.info as ast.Alias).parent_type).is_primitive()) {
				if left_sym.has_method(node.op.str()) {
					if method := left_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if right_sym.kind == .alias && right_sym.info is ast.Alias
				&& !(c.table.get_type_symbol((right_sym.info as ast.Alias).parent_type).is_primitive()) {
				if right_sym.has_method(node.op.str()) {
					if method := right_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if left_sym.has_method(node.op.str()) {
					if method := left_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = left_type
					}
				} else {
					if left_sym.kind == .struct_
						&& (left_sym.info as ast.Struct).generic_types.len > 0 {
						return_type = left_type
					} else {
						left_name := c.table.type_to_str(left_type)
						right_name := c.table.type_to_str(right_type)
						if left_name == right_name {
							c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
								left_right_pos)
						} else {
							c.error('mismatched types `$left_name` and `$right_name`',
								left_right_pos)
						}
					}
				}
			} else if right_sym.kind in [.array, .array_fixed, .map, .struct_] {
				if right_sym.has_method(node.op.str()) {
					if method := right_sym.find_method(node.op.str()) {
						return_type = method.return_type
					} else {
						return_type = right_type
					}
				} else {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			} else if node.left.is_auto_deref_var() || node.right.is_auto_deref_var() {
				deref_left_type := if node.left.is_auto_deref_var() {
					left_type.deref()
				} else {
					left_type
				}
				deref_right_type := if node.right.is_auto_deref_var() {
					right_type.deref()
				} else {
					right_type
				}
				left_name := c.table.type_to_str(c.table.mktyp(deref_left_type))
				right_name := c.table.type_to_str(c.table.mktyp(deref_right_type))
				if left_name != right_name {
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				}
			} else {
				unaliased_left_type := c.table.unalias_num_type(left_type)
				unalias_right_type := c.table.unalias_num_type(right_type)
				mut promoted_type := c.promote(unaliased_left_type, unalias_right_type)
				// substract pointers is allowed in unsafe block
				is_allowed_pointer_arithmetic := left_type.is_any_kind_of_pointer()
					&& right_type.is_any_kind_of_pointer() && node.op == .minus
				if is_allowed_pointer_arithmetic {
					promoted_type = ast.int_type
				}
				if promoted_type.idx() == ast.void_type_idx {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
				} else if promoted_type.has_flag(.optional) {
					s := c.table.type_to_str(promoted_type)
					c.error('`$node.op` cannot be used with `$s`', node.pos)
				} else if promoted_type.is_float() {
					if node.op in [.mod, .xor, .amp, .pipe] {
						side := if left_type == promoted_type { 'left' } else { 'right' }
						pos := if left_type == promoted_type { left_pos } else { right_pos }
						name := if left_type == promoted_type {
							left_sym.name
						} else {
							right_sym.name
						}
						if node.op == .mod {
							c.error('float modulo not allowed, use math.fmod() instead',
								pos)
						} else {
							c.error('$side type of `$node.op.str()` cannot be non-integer type `$name`',
								pos)
						}
					}
				}
				if node.op in [.div, .mod] {
					c.check_div_mod_by_zero(node.right, node.op)
				}
				return_type = promoted_type
			}
		}
		.gt, .lt, .ge, .le {
			if left_sym.kind in [.array, .array_fixed] && right_sym.kind in [.array, .array_fixed] {
				c.error('only `==` and `!=` are defined on arrays', node.pos)
			} else if left_sym.kind == .struct_
				&& (left_sym.info as ast.Struct).generic_types.len > 0 {
				return ast.bool_type
			} else if left_sym.kind == .struct_ && right_sym.kind == .struct_
				&& node.op in [.eq, .lt] {
				if !(left_sym.has_method(node.op.str()) && right_sym.has_method(node.op.str())) {
					left_name := c.table.type_to_str(left_type)
					right_name := c.table.type_to_str(right_type)
					if left_name == right_name {
						c.error('undefined operation `$left_name` $node.op.str() `$right_name`',
							left_right_pos)
					} else {
						c.error('mismatched types `$left_name` and `$right_name`', left_right_pos)
					}
				}
			}
			if left_sym.kind == .struct_ && right_sym.kind == .struct_ {
				if !left_sym.has_method('<') && node.op in [.ge, .le] {
					c.error('cannot use `$node.op` as `<` operator method is not defined',
						left_right_pos)
				} else if !left_sym.has_method('<') && node.op == .gt {
					c.error('cannot use `>` as `<=` operator method is not defined', left_right_pos)
				}
			}
		}
		.left_shift {
			if left_final.kind == .array {
				if !node.is_stmt {
					c.error('array append cannot be used in an expression', node.pos)
				}
				// `array << elm`
				c.check_expr_opt_call(node.right, right_type)
				node.auto_locked, _ = c.fail_if_immutable(node.left)
				left_value_type := c.table.value_type(c.unwrap_generic(left_type))
				left_value_sym := c.table.get_type_symbol(c.unwrap_generic(left_value_type))
				if left_value_sym.kind == .interface_ {
					if right_final.kind != .array {
						// []Animal << Cat
						if c.type_implements(right_type, left_value_type, right_pos) {
							if !right_type.is_ptr() && !right_type.is_pointer() && !c.inside_unsafe
								&& right_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.right, true)
							}
						}
					} else {
						// []Animal << []Cat
						c.type_implements(c.table.value_type(right_type), left_value_type,
							right_pos)
					}
					return ast.void_type
				}
				// []T << T or []T << []T
				unwrapped_right_type := c.unwrap_generic(right_type)
				if c.check_types(unwrapped_right_type, left_value_type) {
					// []&T << T is wrong: we check for that, !(T.is_ptr()) && ?(&T).is_ptr()
					if !(!unwrapped_right_type.is_ptr() && left_value_type.is_ptr()
						&& left_value_type.share() == .mut_t) {
						return ast.void_type
					}
				} else if c.check_types(unwrapped_right_type, c.unwrap_generic(left_type)) {
					return ast.void_type
				}
				c.error('cannot append `$right_sym.name` to `$left_sym.name`', right_pos)
				return ast.void_type
			} else {
				return c.check_shift(mut node, left_type, right_type)
			}
		}
		.right_shift {
			return c.check_shift(mut node, left_type, right_type)
		}
		.unsigned_right_shift {
			modified_left_type := if !left_type.is_int() {
				c.error('invalid operation: shift on type `${c.table.get_type_symbol(left_type).name}`',
					left_pos)
				ast.void_type_idx
			} else if left_type.is_int_literal() {
				// int literal => i64
				ast.u32_type_idx
			} else if left_type.is_unsigned() {
				left_type
			} else {
				// signed types' idx adds with 5 will get correct relative unsigned type
				// i8 		=> byte
				// i16 		=> u16
				// int  	=> u32
				// i64  	=> u64
				// isize	=> usize
				// i128 	=> u128 NOT IMPLEMENTED YET
				left_type.idx() + ast.u32_type_idx - ast.int_type_idx
			}

			if modified_left_type == 0 {
				return ast.void_type
			}

			node = ast.InfixExpr{
				left: ast.CastExpr{
					expr: node.left
					typ: modified_left_type
					typname: c.table.type_str(modified_left_type)
					pos: node.pos
				}
				left_type: left_type
				op: .right_shift
				right: node.right
				right_type: right_type
				is_stmt: false
				pos: node.pos
				auto_locked: node.auto_locked
				or_block: node.or_block
			}

			return c.check_shift(mut node, left_type, right_type)
		}
		.key_is, .not_is {
			right_expr := node.right
			mut typ := match right_expr {
				ast.TypeNode {
					right_expr.typ
				}
				ast.None {
					ast.none_type_idx
				}
				else {
					c.error('invalid type `$right_expr`', right_expr.position())
					ast.Type(0)
				}
			}
			if typ != ast.Type(0) {
				typ_sym := c.table.get_type_symbol(typ)
				op := node.op.str()
				if typ_sym.kind == .placeholder {
					c.error('$op: type `$typ_sym.name` does not exist', right_expr.position())
				}
				if left_sym.kind !in [.interface_, .sum_type] {
					c.error('`$op` can only be used with interfaces and sum types', node.pos)
				} else if mut left_sym.info is ast.SumType {
					if typ !in left_sym.info.variants {
						c.error('`$left_sym.name` has no variant `$right_sym.name`', node.pos)
					}
				}
			}
			return ast.bool_type
		}
		.arrow { // `chan <- elem`
			if left_sym.kind == .chan {
				chan_info := left_sym.chan_info()
				elem_type := chan_info.elem_type
				if !c.check_types(right_type, elem_type) {
					c.error('cannot push `$right_sym.name` on `$left_sym.name`', right_pos)
				}
				if chan_info.is_mut {
					// TODO: The error message of the following could be more specific...
					c.fail_if_immutable(node.right)
				}
				if elem_type.is_ptr() && !right_type.is_ptr() {
					c.error('cannot push non-reference `$right_sym.name` on `$left_sym.name`',
						right_pos)
				}
				c.stmts_ending_with_expression(node.or_block.stmts)
			} else {
				c.error('cannot push on non-channel `$left_sym.name`', left_pos)
			}
			return ast.void_type
		}
		.and, .logical_or {
			if !c.pref.translated {
				if node.left_type != ast.bool_type_idx {
					c.error('left operand for `$node.op` is not a boolean', node.left.position())
				}
				if node.right_type != ast.bool_type_idx {
					c.error('right operand for `$node.op` is not a boolean', node.right.position())
				}
			}
			if mut node.left is ast.InfixExpr {
				if node.left.op != node.op && node.left.op in [.logical_or, .and] {
					// for example: `(a && b) || c` instead of `a && b || c`
					c.error('ambiguous boolean expression. use `()` to ensure correct order of operations',
						node.pos)
				}
			}
		}
		else {}
	}
	// TODO: Absorb this block into the above single side check block to accelerate.
	if left_type == ast.bool_type && node.op !in [.eq, .ne, .logical_or, .and] {
		c.error('bool types only have the following operators defined: `==`, `!=`, `||`, and `&&`',
			node.pos)
	} else if left_type == ast.string_type && node.op !in [.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		// TODO broken !in
		c.error('string types only have the following operators defined: `==`, `!=`, `<`, `>`, `<=`, `>=`, and `+`',
			node.pos)
	} else if left_sym.kind == .enum_ && right_sym.kind == .enum_ && !eq_ne {
		left_enum := left_sym.info as ast.Enum
		right_enum := right_sym.info as ast.Enum
		if left_enum.is_flag && right_enum.is_flag {
			// `[flag]` tagged enums are a special case that allow also `|` and `&` binary operators
			if node.op !in [.pipe, .amp] {
				c.error('only `==`, `!=`, `|` and `&` are defined on `[flag]` tagged `enum`, use an explicit cast to `int` if needed',
					node.pos)
			}
		} else if !c.pref.translated {
			// Regular enums
			c.error('only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed',
				node.pos)
		}
	}
	// sum types can't have any infix operation except of `is`, `eq`, `ne`.
	// `is` is checked before and doesn't reach this.
	if c.table.type_kind(left_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$left_sym.name`', node.pos)
	} else if c.table.type_kind(right_type) == .sum_type && !eq_ne {
		c.error('cannot use operator `$node.op` with `$right_sym.name`', node.pos)
	}
	// TODO move this to symmetric_check? Right now it would break `return 0` for `fn()?int `
	left_is_optional := left_type.has_flag(.optional)
	right_is_optional := right_type.has_flag(.optional)
	if (left_is_optional && !right_is_optional) || (!left_is_optional && right_is_optional) {
		c.error('unwrapped optional cannot be used in an infix expression', left_right_pos)
	}
	// Dual sides check (compatibility check)
	if !(c.symmetric_check(left_type, right_type) && c.symmetric_check(right_type, left_type))
		&& !c.pref.translated && !node.left.is_auto_deref_var() && !node.right.is_auto_deref_var() {
		// for type-unresolved consts
		if left_type == ast.void_type || right_type == ast.void_type {
			return ast.void_type
		}
		if left_type.nr_muls() > 0 && right_type.is_int() {
			// pointer arithmetic is fine, it is checked in other places
			return return_type
		}
		c.error('infix expr: cannot use `$right_sym.name` (right expression) as `$left_sym.name`',
			left_right_pos)
	}
	/*
	if (node.left is ast.InfixExpr &&
		(node.left as ast.InfixExpr).op == .inc) ||
		(node.right is ast.InfixExpr && (node.right as ast.InfixExpr).op == .inc) {
		c.warn('`++` and `--` are statements, not expressions', node.pos)
	}
	*/
	return if node.op.is_relational() { ast.bool_type } else { return_type }
}

// returns name and position of variable that needs write lock
// also sets `is_changed` to true (TODO update the name to reflect this?)
fn (mut c Checker) fail_if_immutable(expr ast.Expr) (string, token.Position) {
	mut to_lock := '' // name of variable that needs lock
	mut pos := token.Position{} // and its position
	mut explicit_lock_needed := false
	match mut expr {
		ast.CastExpr {
			// TODO
			return '', pos
		}
		ast.ComptimeSelector {
			return '', pos
		}
		ast.Ident {
			if expr.obj is ast.Var {
				mut v := expr.obj as ast.Var
				if !v.is_mut && !c.pref.translated && !c.inside_unsafe {
					c.error('`$expr.name` is immutable, declare it with `mut` to make it mutable',
						expr.pos)
				}
				v.is_changed = true
				if v.typ.share() == .shared_t {
					if expr.name !in c.locked_names {
						if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
							if expr.name in c.rlocked_names {
								c.error('$expr.name has an `rlock` but needs a `lock`',
									expr.pos)
							} else {
								c.error('$expr.name must be added to the `lock` list above',
									expr.pos)
							}
						}
						to_lock = expr.name
						pos = expr.pos
					}
				}
			} else if expr.obj is ast.ConstField && expr.name in c.const_names {
				c.error('cannot modify constant `$expr.name`', expr.pos)
			}
		}
		ast.IndexExpr {
			left_sym := c.table.get_type_symbol(expr.left_type)
			mut elem_type := ast.Type(0)
			mut kind := ''
			match left_sym.info {
				ast.Array {
					elem_type, kind = left_sym.info.elem_type, 'array'
				}
				ast.ArrayFixed {
					elem_type, kind = left_sym.info.elem_type, 'fixed array'
				}
				ast.Map {
					elem_type, kind = left_sym.info.value_type, 'map'
				}
				else {}
			}
			if elem_type.has_flag(.shared_f) {
				c.error('you have to create a handle and `lock` it to modify `shared` $kind element',
					expr.left.position().extend(expr.pos))
			}
			to_lock, pos = c.fail_if_immutable(expr.left)
		}
		ast.ParExpr {
			to_lock, pos = c.fail_if_immutable(expr.expr)
		}
		ast.PrefixExpr {
			to_lock, pos = c.fail_if_immutable(expr.right)
		}
		ast.SelectorExpr {
			if expr.expr_type == 0 {
				return '', pos
			}
			// retrieve ast.Field
			c.ensure_type_exists(expr.expr_type, expr.pos) or { return '', pos }
			mut typ_sym := c.table.get_final_type_symbol(c.unwrap_generic(expr.expr_type))
			match typ_sym.kind {
				.struct_ {
					mut has_field := true
					mut field_info := c.table.find_field_with_embeds(typ_sym, expr.field_name) or {
						has_field = false
						ast.StructField{}
					}
					if !has_field {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if field_info.typ.has_flag(.shared_f) {
						expr_name := '${expr.expr}.$expr.field_name'
						if expr_name !in c.locked_names {
							if c.locked_names.len > 0 || c.rlocked_names.len > 0 {
								if expr_name in c.rlocked_names {
									c.error('$expr_name has an `rlock` but needs a `lock`',
										expr.pos)
								} else {
									c.error('$expr_name must be added to the `lock` list above',
										expr.pos)
								}
								return '', expr.pos
							}
							to_lock = expr_name
							pos = expr.pos
						}
					} else {
						if !field_info.is_mut && !c.pref.translated {
							type_str := c.table.type_to_str(expr.expr_type)
							c.error('field `$expr.field_name` of struct `$type_str` is immutable',
								expr.pos)
						}
						to_lock, pos = c.fail_if_immutable(expr.expr)
					}
					if to_lock != '' {
						// No automatic lock for struct access
						explicit_lock_needed = true
					}
				}
				.interface_ {
					interface_info := typ_sym.info as ast.Interface
					mut field_info := interface_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of interface `$type_str` is immutable',
							expr.pos)
						return '', expr.pos
					}
					c.fail_if_immutable(expr.expr)
				}
				.sum_type {
					sumtype_info := typ_sym.info as ast.SumType
					mut field_info := sumtype_info.find_field(expr.field_name) or {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('unknown field `${type_str}.$expr.field_name`', expr.pos)
						return '', pos
					}
					if !field_info.is_mut {
						type_str := c.table.type_to_str(expr.expr_type)
						c.error('field `$expr.field_name` of sumtype `$type_str` is immutable',
							expr.pos)
						return '', expr.pos
					}
					c.fail_if_immutable(expr.expr)
				}
				.array, .string {
					// should only happen in `builtin` and unsafe blocks
					inside_builtin := c.file.mod.name == 'builtin'
					if !inside_builtin && !c.inside_unsafe {
						c.error('`$typ_sym.kind` can not be modified', expr.pos)
						return '', expr.pos
					}
				}
				.aggregate, .placeholder {
					c.fail_if_immutable(expr.expr)
				}
				else {
					c.error('unexpected symbol `$typ_sym.kind`', expr.pos)
					return '', expr.pos
				}
			}
		}
		ast.CallExpr {
			// TODO: should only work for builtin method
			if expr.name == 'slice' {
				to_lock, pos = c.fail_if_immutable(expr.left)
				if to_lock != '' {
					// No automatic lock for array slicing (yet(?))
					explicit_lock_needed = true
				}
			}
		}
		ast.ArrayInit {
			c.error('array literal can not be modified', expr.pos)
			return '', pos
		}
		ast.StructInit {
			return '', pos
		}
		ast.InfixExpr {
			return '', pos
		}
		else {
			if !expr.is_lit() {
				c.error('unexpected expression `$expr.type_name()`', expr.position())
				return '', pos
			}
		}
	}
	if explicit_lock_needed {
		c.error('`$to_lock` is `shared` and needs explicit lock for `$expr.type_name()`',
			pos)
		to_lock = ''
	}
	return to_lock, pos
}

pub fn (mut c Checker) call_expr(mut node ast.CallExpr) ast.Type {
	// First check everything that applies to both fns and methods
	// TODO merge logic from method_call and fn_call
	/*
	for i, call_arg in node.args {
		if call_arg.is_mut {
			c.fail_if_immutable(call_arg.expr)
			if !arg.is_mut {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is not `$tok`, `$tok` is not needed`',
					call_arg.expr.position())
			} else if arg.typ.share() != call_arg.share {
				c.error('wrong shared type', call_arg.expr.position())
			}
		} else {
			if arg.is_mut && (!call_arg.is_mut || arg.typ.share() != call_arg.share) {
				tok := call_arg.share.str()
				c.error('`$node.name` parameter `$arg.name` is `$tok`, you need to provide `$tok` e.g. `$tok arg${i+1}`',
					call_arg.expr.position())
			}
		}
	}
	*/
	// Now call `method_call` or `fn_call` for specific checks.
	old_inside_fn_arg := c.inside_fn_arg
	c.inside_fn_arg = true
	mut continue_check := true
	typ := if node.is_method {
		c.method_call(mut node)
	} else {
		c.fn_call(mut node, mut continue_check)
	}
	if !continue_check {
		return ast.void_type
	}
	c.inside_fn_arg = old_inside_fn_arg
	// autofree: mark args that have to be freed (after saving them in tmp exprs)
	free_tmp_arg_vars := c.pref.autofree && !c.is_builtin_mod && node.args.len > 0
		&& !node.args[0].typ.has_flag(.optional)
	if free_tmp_arg_vars && !c.inside_const {
		for i, arg in node.args {
			if arg.typ != ast.string_type {
				continue
			}
			if arg.expr in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
				// Simple expressions like variables, string literals, selector expressions
				// (`x.field`) can't result in allocations and don't need to be assigned to
				// temporary vars.
				// Only expressions like `str + 'b'` need to be freed.
				continue
			}
			node.args[i].is_tmp_autofree = true
		}
		// TODO copy pasta from above
		if node.receiver_type == ast.string_type
			&& node.left !in [ast.Ident, ast.StringLiteral, ast.SelectorExpr] {
			node.free_receiver = true
		}
	}
	c.expected_or_type = node.return_type.clear_flag(.optional)
	c.stmts_ending_with_expression(node.or_block.stmts)
	c.expected_or_type = ast.void_type
	if node.or_block.kind == .propagate && !c.table.cur_fn.return_type.has_flag(.optional)
		&& !c.inside_const {
		if !c.table.cur_fn.is_main {
			c.error('to propagate the optional call, `$c.table.cur_fn.name` must return an optional',
				node.or_block.pos)
		}
	}
	return typ
}

fn semicolonize(main string, details string) string {
	if details == '' {
		return main
	}
	return '$main; $details'
}

fn (mut c Checker) type_implements(typ ast.Type, interface_type ast.Type, pos token.Position) bool {
	$if debug_interface_type_implements ? {
		eprintln('> type_implements typ: $typ.debug() (`${c.table.type_to_str(typ)}`) | inter_typ: $interface_type.debug() (`${c.table.type_to_str(interface_type)}`)')
	}
	utyp := c.unwrap_generic(typ)
	typ_sym := c.table.get_type_symbol(utyp)
	mut inter_sym := c.table.get_type_symbol(interface_type)

	// small hack for JS.Any type. Since `any` in regular V is getting deprecated we have our own JS.Any type for JS backend.
	if typ_sym.name == 'JS.Any' {
		return true
	}
	if mut inter_sym.info is ast.Interface {
		mut generic_type := interface_type
		mut generic_info := inter_sym.info
		if inter_sym.info.parent_type.has_flag(.generic) {
			parent_sym := c.table.get_type_symbol(inter_sym.info.parent_type)
			if parent_sym.info is ast.Interface {
				generic_type = inter_sym.info.parent_type
				generic_info = parent_sym.info
			}
		}
		mut inferred_type := interface_type
		if generic_info.is_generic {
			inferred_type = c.resolve_generic_interface(typ, generic_type, pos)
			if inferred_type == 0 {
				return false
			}
		}
		if inter_sym.info.is_generic {
			return c.type_implements(typ, inferred_type, pos)
		}
	}
	// do not check the same type more than once
	if mut inter_sym.info is ast.Interface {
		for t in inter_sym.info.types {
			if t.idx() == utyp.idx() {
				return true
			}
		}
	}
	styp := c.table.type_to_str(utyp)
	if utyp.idx() == interface_type.idx() {
		// same type -> already casted to the interface
		return true
	}
	if interface_type.idx() == ast.error_type_idx && utyp.idx() == ast.none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	if typ_sym.kind == .interface_ && inter_sym.kind == .interface_ && !styp.starts_with('JS.')
		&& !inter_sym.name.starts_with('JS.') {
		c.error('cannot implement interface `$inter_sym.name` with a different interface `$styp`',
			pos)
	}
	imethods := if inter_sym.kind == .interface_ {
		(inter_sym.info as ast.Interface).methods
	} else {
		inter_sym.methods
	}
	// voidptr is an escape hatch, it should be allowed to be passed
	if utyp != ast.voidptr_type {
		// Verify methods
		for imethod in imethods {
			method := c.table.find_method_with_embeds(typ_sym, imethod.name) or {
				typ_sym.find_method_with_generic_parent(imethod.name) or {
					c.error("`$styp` doesn't implement method `$imethod.name` of interface `$inter_sym.name`",
						pos)
					continue
				}
			}
			msg := c.table.is_same_method(imethod, method)
			if msg.len > 0 {
				sig := c.table.fn_signature(imethod, skip_receiver: false)
				typ_sig := c.table.fn_signature(method, skip_receiver: false)
				c.add_error_detail('$inter_sym.name has `$sig`')
				c.add_error_detail('         $typ_sym.name has `$typ_sig`')
				c.error('`$styp` incorrectly implements method `$imethod.name` of interface `$inter_sym.name`: $msg',
					pos)
				return false
			}
		}
	}
	// Verify fields
	if mut inter_sym.info is ast.Interface {
		for ifield in inter_sym.info.fields {
			if field := c.table.find_field_with_embeds(typ_sym, ifield.name) {
				if ifield.typ != field.typ {
					exp := c.table.type_to_str(ifield.typ)
					got := c.table.type_to_str(field.typ)
					c.error('`$styp` incorrectly implements field `$ifield.name` of interface `$inter_sym.name`, expected `$exp`, got `$got`',
						pos)
					return false
				} else if ifield.is_mut && !(field.is_mut || field.is_global) {
					c.error('`$styp` incorrectly implements interface `$inter_sym.name`, field `$ifield.name` must be mutable',
						pos)
					return false
				}
				continue
			}
			// voidptr is an escape hatch, it should be allowed to be passed
			if utyp != ast.voidptr_type {
				c.error("`$styp` doesn't implement field `$ifield.name` of interface `$inter_sym.name`",
					pos)
			}
		}
		inter_sym.info.types << utyp
	}
	return true
}

// return the actual type of the expression, once the optional is handled
pub fn (mut c Checker) check_expr_opt_call(expr ast.Expr, ret_type ast.Type) ast.Type {
	if expr is ast.CallExpr {
		if expr.return_type.has_flag(.optional) {
			if expr.or_block.kind == .absent {
				if c.inside_defer {
					c.error('${expr.name}() returns an option, so it should have an `or {}` block at the end',
						expr.pos)
				} else {
					c.error('${expr.name}() returns an option, so it should have either an `or {}` block, or `?` at the end',
						expr.pos)
				}
			} else {
				c.check_or_expr(expr.or_block, ret_type, expr.return_type.clear_flag(.optional))
			}
			return ret_type.clear_flag(.optional)
		} else if expr.or_block.kind == .block {
			c.error('unexpected `or` block, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		} else if expr.or_block.kind == .propagate {
			c.error('unexpected `?`, the function `$expr.name` does not return an optional',
				expr.or_block.pos)
		}
	} else if expr is ast.IndexExpr {
		if expr.or_expr.kind != .absent {
			c.check_or_expr(expr.or_expr, ret_type, ret_type)
		}
	}
	return ret_type
}

pub fn (mut c Checker) check_or_expr(node ast.OrExpr, ret_type ast.Type, expr_return_type ast.Type) {
	if node.kind == .propagate {
		if !c.table.cur_fn.return_type.has_flag(.optional) && c.table.cur_fn.name != 'main.main'
			&& !c.inside_const {
			c.error('to propagate the optional call, `$c.table.cur_fn.name` must return an optional',
				node.pos)
		}
		return
	}
	stmts_len := node.stmts.len
	if stmts_len == 0 {
		if ret_type != ast.void_type {
			// x := f() or {}
			c.error('assignment requires a non empty `or {}` block', node.pos)
		}
		// allow `f() or {}`
		return
	}
	last_stmt := node.stmts[stmts_len - 1]
	if ret_type != ast.void_type {
		match last_stmt {
			ast.ExprStmt {
				c.expected_type = ret_type
				c.expected_or_type = ret_type.clear_flag(.optional)
				last_stmt_typ := c.expr(last_stmt.expr)
				c.expected_or_type = ast.void_type
				type_fits := c.check_types(last_stmt_typ, ret_type)
					&& last_stmt_typ.nr_muls() == ret_type.nr_muls()
				is_noreturn := is_noreturn_callexpr(last_stmt.expr)
				if type_fits || is_noreturn {
					return
				}
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				if last_stmt.typ == ast.void_type {
					c.error('`or` block must provide a default value of type `$expected_type_name`, or return/continue/break or call a [noreturn] function like panic(err) or exit(1)',
						last_stmt.pos)
				} else {
					type_name := c.table.type_to_str(last_stmt_typ)
					c.error('wrong return type `$type_name` in the `or {}` block, expected `$expected_type_name`',
						last_stmt.pos)
				}
				return
			}
			ast.BranchStmt {
				if last_stmt.kind !in [.key_continue, .key_break] {
					c.error('only break/continue is allowed as a branch statement in the end of an `or {}` block',
						last_stmt.pos)
					return
				}
			}
			ast.Return {}
			else {
				expected_type_name := c.table.type_to_str(ret_type.clear_flag(.optional))
				c.error('last statement in the `or {}` block should be an expression of type `$expected_type_name` or exit parent scope',
					node.pos)
				return
			}
		}
	} else {
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.typ == ast.void_type {
					return
				}
				if is_noreturn_callexpr(last_stmt.expr) {
					return
				}
				if c.check_types(last_stmt.typ, expr_return_type) {
					return
				}
				// opt_returning_string() or { ... 123 }
				type_name := c.table.type_to_str(last_stmt.typ)
				expr_return_type_name := c.table.type_to_str(expr_return_type)
				c.error('the default expression type in the `or` block should be `$expr_return_type_name`, instead you gave a value of type `$type_name`',
					last_stmt.expr.position())
			}
			else {}
		}
	}
}

pub fn (mut c Checker) selector_expr(mut node ast.SelectorExpr) ast.Type {
	prevent_sum_type_unwrapping_once := c.prevent_sum_type_unwrapping_once
	c.prevent_sum_type_unwrapping_once = false

	using_new_err_struct_save := c.using_new_err_struct
	// TODO remove; this avoids a breaking change in syntax
	if '$node.expr' == 'err' {
		c.using_new_err_struct = true
	}

	// T.name, typeof(expr).name
	mut name_type := 0
	match mut node.expr {
		ast.Ident {
			name := node.expr.name
			valid_generic := util.is_generic_type_name(name) && name in c.table.cur_fn.generic_names
			if valid_generic {
				name_type = ast.Type(c.table.find_type_idx(name)).set_flag(.generic)
			}
		}
		// Note: in future typeof() should be a type known at compile-time
		// sum types should not be handled dynamically
		ast.TypeOf {
			name_type = c.expr(node.expr.expr)
		}
		else {}
	}
	if name_type > 0 {
		node.name_type = name_type
		match node.gkind_field {
			.name {
				return ast.string_type
			}
			.typ {
				return ast.int_type
			}
			else {
				if node.field_name == 'name' {
					return ast.string_type
				} else if node.field_name == 'idx' {
					return ast.int_type
				}
				c.error('invalid field `.$node.field_name` for type `$node.expr`', node.pos)
				return ast.string_type
			}
		}
	}

	old_selector_expr := c.inside_selector_expr
	c.inside_selector_expr = true
	mut typ := c.expr(node.expr)
	if node.expr.is_auto_deref_var() {
		if mut node.expr is ast.Ident {
			if mut node.expr.obj is ast.Var {
				typ = node.expr.obj.typ
			}
		}
	}
	c.inside_selector_expr = old_selector_expr
	c.using_new_err_struct = using_new_err_struct_save
	if typ == ast.void_type_idx {
		c.error('`void` type has no fields', node.pos)
		return ast.void_type
	}
	node.expr_type = typ
	if node.expr_type.has_flag(.optional) && !(node.expr is ast.Ident
		&& (node.expr as ast.Ident).kind == .constant) {
		c.error('cannot access fields of an optional, handle the error with `or {...}` or propagate it with `?`',
			node.pos)
	}
	field_name := node.field_name
	sym := c.table.get_type_symbol(typ)
	if (typ.has_flag(.variadic) || sym.kind == .array_fixed) && field_name == 'len' {
		node.typ = ast.int_type
		return ast.int_type
	}
	if sym.kind == .chan {
		if field_name == 'closed' {
			node.typ = ast.bool_type
			return ast.bool_type
		} else if field_name in ['len', 'cap'] {
			node.typ = ast.u32_type
			return ast.u32_type
		}
	}
	mut unknown_field_msg := 'type `$sym.name` has no field named `$field_name`'
	mut has_field := false
	mut field := ast.StructField{}
	if field_name.len > 0 && field_name[0].is_capital() && sym.info is ast.Struct
		&& sym.language == .v {
		// x.Foo.y => access the embedded struct
		for embed in sym.info.embeds {
			embed_sym := c.table.get_type_symbol(embed)
			if embed_sym.embed_name() == field_name {
				node.typ = embed
				return embed
			}
		}
	} else {
		if f := c.table.find_field(sym, field_name) {
			has_field = true
			field = f
		} else {
			// look for embedded field
			has_field = true
			mut embed_types := []ast.Type{}
			field, embed_types = c.table.find_field_from_embeds(sym, field_name) or {
				if err.msg != '' {
					c.error(err.msg, node.pos)
				}
				has_field = false
				ast.StructField{}, []ast.Type{}
			}
			node.from_embed_types = embed_types
			if sym.kind in [.aggregate, .sum_type] {
				unknown_field_msg = err.msg
			}
		}
		if !c.inside_unsafe {
			if sym.info is ast.Struct {
				if sym.info.is_union && node.next_token !in token.assign_tokens {
					c.warn('reading a union field (or its address) requires `unsafe`',
						node.pos)
				}
			}
		}
		if typ.has_flag(.generic) && !has_field {
			gs := c.table.get_type_symbol(c.unwrap_generic(typ))
			if f := c.table.find_field(gs, field_name) {
				has_field = true
				field = f
			} else {
				// look for embedded field
				has_field = true
				mut embed_types := []ast.Type{}
				field, embed_types = c.table.find_field_from_embeds(gs, field_name) or {
					if err.msg != '' {
						c.error(err.msg, node.pos)
					}
					has_field = false
					ast.StructField{}, []ast.Type{}
				}
				node.from_embed_types = embed_types
			}
		}
	}
	if has_field {
		if sym.mod != c.mod && !field.is_pub && sym.language != .c {
			unwrapped_sym := c.table.get_type_symbol(c.unwrap_generic(typ))
			c.error('field `${unwrapped_sym.name}.$field_name` is not public', node.pos)
		}
		field_sym := c.table.get_type_symbol(field.typ)
		if field_sym.kind in [.sum_type, .interface_] {
			if !prevent_sum_type_unwrapping_once {
				if scope_field := node.scope.find_struct_field(node.expr.str(), typ, field_name) {
					return scope_field.smartcasts.last()
				}
			}
		}
		node.typ = field.typ
		return field.typ
	}
	if sym.kind !in [.struct_, .aggregate, .interface_, .sum_type] {
		if sym.kind != .placeholder {
			unwrapped_sym := c.table.get_type_symbol(c.unwrap_generic(typ))
			c.error('`$unwrapped_sym.name` has no property `$node.field_name`', node.pos)
		}
	} else {
		if sym.info is ast.Struct {
			suggestion := util.new_suggestion(field_name, sym.info.fields.map(it.name))
			c.error(suggestion.say(unknown_field_msg), node.pos)
		}
		c.error(unknown_field_msg, node.pos)
	}
	return ast.void_type
}

pub fn (mut c Checker) const_decl(mut node ast.ConstDecl) {
	if node.fields.len == 0 {
		c.warn('const block must have at least 1 declaration', node.pos)
	}
	for field in node.fields {
		// TODO Check const name once the syntax is decided
		if field.name in c.const_names {
			name_pos := token.Position{
				...field.pos
				len: util.no_cur_mod(field.name, c.mod).len
			}
			c.error('duplicate const `$field.name`', name_pos)
		}
		c.const_names << field.name
	}
	for i, mut field in node.fields {
		c.const_decl = field.name
		c.const_deps << field.name
		mut typ := c.check_expr_opt_call(field.expr, c.expr(field.expr))
		if ct_value := c.eval_comptime_const_expr(field.expr, 0) {
			field.comptime_expr_value = ct_value
			if ct_value is u64 {
				typ = ast.u64_type
			}
		}
		node.fields[i].typ = c.table.mktyp(typ)
		c.const_deps = []
	}
}

pub fn (mut c Checker) enum_decl(mut node ast.EnumDecl) {
	c.check_valid_pascal_case(node.name, 'enum name', node.pos)
	mut seen := []i64{cap: node.fields.len}
	if node.fields.len == 0 {
		c.error('enum cannot be empty', node.pos)
	}
	/*
	if node.is_pub && c.mod == 'builtin' {
		c.error('`builtin` module cannot have enums', node.pos)
	}
	*/
	for i, mut field in node.fields {
		if !c.pref.experimental && util.contains_capital(field.name) {
			// TODO C2V uses hundreds of enums with capitals, remove -experimental check once it's handled
			c.error('field name `$field.name` cannot contain uppercase letters, use snake_case instead',
				field.pos)
		}
		for j in 0 .. i {
			if field.name == node.fields[j].name {
				c.error('field name `$field.name` duplicate', field.pos)
			}
		}
		if field.has_expr {
			match mut field.expr {
				ast.IntegerLiteral {
					val := field.expr.val.i64()
					if val < checker.int_min || val > checker.int_max {
						c.error('enum value `$val` overflows int', field.expr.pos)
					} else if !c.pref.translated && !node.is_multi_allowed && i64(val) in seen {
						c.error('enum value `$val` already exists', field.expr.pos)
					}
					seen << i64(val)
				}
				ast.PrefixExpr {}
				ast.InfixExpr {
					// Handle `enum Foo { x = 1 + 2 }`
					c.infix_expr(mut field.expr)
				}
				// ast.ParExpr {} // TODO allow `.x = (1+2)`
				else {
					if field.expr is ast.Ident {
						x := field.expr as ast.Ident
						// TODO sum type bug, remove temp var
						// if field.expr.language == .c {
						if x.language == .c {
							continue
						}
					}
					mut pos := field.expr.position()
					if pos.pos == 0 {
						pos = field.pos
					}
					c.error('default value for enum has to be an integer', pos)
				}
			}
		} else {
			if seen.len > 0 {
				last := seen[seen.len - 1]
				if last == checker.int_max {
					c.error('enum value overflows', field.pos)
				} else if !c.pref.translated && !node.is_multi_allowed && last + 1 in seen {
					c.error('enum value `${last + 1}` already exists', field.pos)
				}
				seen << last + 1
			} else {
				seen << 0
			}
		}
	}
}

fn (mut c Checker) check_array_init_para_type(para string, expr ast.Expr, pos token.Position) {
	sym := c.table.get_type_symbol(c.expr(expr))
	if sym.kind !in [.int, .int_literal] {
		c.error('array $para needs to be an int', pos)
	}
}

pub fn (mut c Checker) ensure_sumtype_array_has_default_value(node ast.ArrayInit) {
	sym := c.table.get_type_symbol(node.elem_type)
	if sym.kind == .sum_type && !node.has_default {
		c.error('cannot initialize sum type array without default value', node.pos)
	}
}

pub fn (mut c Checker) array_init(mut node ast.ArrayInit) ast.Type {
	mut elem_type := ast.void_type
	// []string - was set in parser
	if node.typ != ast.void_type {
		if node.exprs.len == 0 {
			if node.has_cap {
				c.check_array_init_para_type('cap', node.cap_expr, node.pos)
			}
			if node.has_len {
				c.check_array_init_para_type('len', node.len_expr, node.pos)
			}
		}
		if node.has_default {
			default_expr := node.default_expr
			default_typ := c.check_expr_opt_call(default_expr, c.expr(default_expr))
			c.check_expected(default_typ, node.elem_type) or {
				c.error(err.msg, default_expr.position())
			}
		}
		if node.has_len {
			if node.has_len && !node.has_default {
				elem_type_sym := c.table.get_type_symbol(node.elem_type)
				if elem_type_sym.kind == .interface_ {
					c.error('cannot instantiate an array of interfaces without also giving a default `init:` value',
						node.len_expr.position())
				}
			}
			c.ensure_sumtype_array_has_default_value(node)
		}
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
		if node.typ.has_flag(.generic) && c.table.cur_fn.generic_names.len == 0 {
			c.error('generic struct cannot use in non-generic function', node.pos)
		}
		return node.typ
	}
	if node.is_fixed {
		c.ensure_sumtype_array_has_default_value(node)
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
	}
	// a = []
	if node.exprs.len == 0 {
		// a := fn_returing_opt_array() or { [] }
		if c.expected_type == ast.void_type && c.expected_or_type != ast.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.get_type_symbol(c.expected_type)
		if type_sym.kind != .array || type_sym.array_info().elem_type == ast.void_type {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				node.pos)
			return ast.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == ast.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', node.pos)
		// return ast.void_type
		// }
		array_info := type_sym.array_info()
		node.elem_type = array_info.elem_type
		// clear optional flag incase of: `fn opt_arr ?[]int { return [] }`
		return c.expected_type.clear_flag(.optional)
	}
	// [1,2,3]
	if node.exprs.len > 0 && node.elem_type == ast.void_type {
		mut expected_value_type := ast.void_type
		mut expecting_interface_array := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			if c.table.get_type_symbol(expected_value_type).kind == .interface_ {
				// Array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			}
		}
		// expecting_interface_array := c.expected_type != 0 &&
		// c.table.get_type_symbol(c.table.value_type(c.expected_type)).kind ==			.interface_
		//
		// if expecting_interface_array {
		// println('ex $c.expected_type')
		// }
		for i, mut expr in node.exprs {
			typ := c.check_expr_opt_call(expr, c.expr(expr))
			node.expr_types << typ
			// The first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.position())
				}
				if !typ.is_ptr() && !typ.is_pointer() && !c.inside_unsafe {
					typ_sym := c.table.get_type_symbol(typ)
					if typ_sym.kind != .interface_ {
						c.mark_as_referenced(mut &expr, true)
					}
				}
				continue
			}
			// The first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = c.table.mktyp(typ.deref())
				} else {
					elem_type = c.table.mktyp(typ)
				}
				c.expected_type = elem_type
				continue
			}
			if expr !is ast.TypeNode {
				c.check_expected(typ, elem_type) or {
					c.error('invalid array element: $err.msg', expr.position())
				}
			}
		}
		if node.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, node.exprs.len, ast.empty_expr())
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		} else {
			idx := c.table.find_or_register_array(elem_type)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		}
		node.elem_type = elem_type
	} else if node.is_fixed && node.exprs.len == 1 && node.elem_type != ast.void_type {
		// [50]byte
		mut fixed_size := i64(0)
		init_expr := node.exprs[0]
		c.expr(init_expr)
		match init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				if init_expr.obj is ast.ConstField {
					if comptime_value := c.eval_comptime_const_expr(init_expr.obj.expr,
						0)
					{
						fixed_size = comptime_value.i64() or { fixed_size }
					}
				} else {
					c.error('non-constant array bound `$init_expr.name`', init_expr.pos)
				}
			}
			ast.InfixExpr {
				if comptime_value := c.eval_comptime_const_expr(init_expr, 0) {
					fixed_size = comptime_value.i64() or { fixed_size }
				}
			}
			else {
				c.error('expecting `int` for fixed size', node.pos)
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative (fixed_size: $fixed_size)',
				init_expr.position())
		}
		idx := c.table.find_or_register_array_fixed(node.elem_type, int(fixed_size), init_expr)
		if node.elem_type.has_flag(.generic) {
			node.typ = ast.new_type(idx).set_flag(.generic)
		} else {
			node.typ = ast.new_type(idx)
		}
		if node.has_default {
			c.expr(node.default_expr)
		}
	}
	return node.typ
}

[inline]
fn (mut c Checker) check_loop_label(label string, pos token.Position) {
	if label.len == 0 {
		// ignore
		return
	}
	if c.loop_label.len != 0 {
		c.error('nesting of labelled `for` loops is not supported', pos)
		return
	}
	c.loop_label = label
}

fn (mut c Checker) stmt(node ast.Stmt) {
	$if trace_checker ? {
		ntype := typeof(node).replace('v.ast.', '')
		eprintln('checking: ${c.file.path:-30} | pos: ${node.pos.line_str():-39} | node: $ntype | $node')
	}
	c.expected_type = ast.void_type
	match mut node {
		ast.EmptyStmt {
			if c.pref.is_verbose {
				eprintln('Checker.stmt() EmptyStmt')
				print_backtrace()
			}
		}
		ast.NodeError {}
		ast.AsmStmt {
			c.asm_stmt(mut node)
		}
		ast.AssertStmt {
			c.assert_stmt(node)
		}
		ast.AssignStmt {
			c.assign_stmt(mut node)
		}
		ast.Block {
			c.block(node)
		}
		ast.BranchStmt {
			c.branch_stmt(node)
		}
		ast.ComptimeFor {
			c.comptime_for(node)
		}
		ast.ConstDecl {
			c.inside_const = true
			c.const_decl(mut node)
			c.inside_const = false
		}
		ast.DeferStmt {
			if node.idx_in_fn < 0 {
				node.idx_in_fn = c.table.cur_fn.defer_stmts.len
				c.table.cur_fn.defer_stmts << unsafe { &node }
			}
			if c.locked_names.len != 0 || c.rlocked_names.len != 0 {
				c.error('defers are not allowed in lock statements', node.pos)
			}
			for i, ident in node.defer_vars {
				mut id := ident
				if id.info is ast.IdentVar {
					if id.comptime && id.name in checker.valid_comptime_not_user_defined {
						node.defer_vars[i] = ast.Ident{
							scope: 0
							name: ''
						}
						continue
					}
					mut info := id.info as ast.IdentVar
					typ := c.ident(mut id)
					if typ == ast.error_type_idx {
						continue
					}
					info.typ = typ
					id.info = info
					node.defer_vars[i] = id
				}
			}
			c.inside_defer = true
			c.stmts(node.stmts)
			c.inside_defer = false
		}
		ast.EnumDecl {
			c.enum_decl(mut node)
		}
		ast.ExprStmt {
			node.typ = c.expr(node.expr)
			c.expected_type = ast.void_type
			mut or_typ := ast.void_type
			match node.expr {
				ast.IndexExpr {
					if node.expr.or_expr.kind != .absent {
						node.is_expr = true
						or_typ = node.typ
					}
				}
				ast.PrefixExpr {
					if node.expr.or_block.kind != .absent {
						node.is_expr = true
						or_typ = node.typ
					}
				}
				else {}
			}
			if !c.pref.is_repl && (c.stmt_level == 1 || (c.stmt_level > 1 && !c.is_last_stmt)) {
				if node.expr is ast.InfixExpr {
					if node.expr.op == .left_shift {
						left_sym := c.table.get_final_type_symbol(node.expr.left_type)
						if left_sym.kind != .array {
							c.error('unused expression', node.pos)
						}
					}
				}
			}
			c.check_expr_opt_call(node.expr, or_typ)
			// TODO This should work, even if it's prolly useless .-.
			// node.typ = c.check_expr_opt_call(node.expr, ast.void_type)
		}
		ast.FnDecl {
			c.fn_decl(mut node)
		}
		ast.ForCStmt {
			c.for_c_stmt(node)
		}
		ast.ForInStmt {
			c.for_in_stmt(mut node)
		}
		ast.ForStmt {
			c.for_stmt(mut node)
		}
		ast.GlobalDecl {
			c.global_decl(mut node)
		}
		ast.GotoLabel {}
		ast.GotoStmt {
			if c.inside_defer {
				c.error('goto is not allowed in defer statements', node.pos)
			}
			if !c.inside_unsafe {
				c.warn('`goto` requires `unsafe` (consider using labelled break/continue)',
					node.pos)
			}
			if node.name !in c.table.cur_fn.label_names {
				c.error('unknown label `$node.name`', node.pos)
			}
			// TODO: check label doesn't bypass variable declarations
		}
		ast.HashStmt {
			c.hash_stmt(mut node)
		}
		ast.Import {
			c.import_stmt(node)
		}
		ast.InterfaceDecl {
			c.interface_decl(mut node)
		}
		ast.Module {
			c.mod = node.name
			c.is_builtin_mod = node.name in ['builtin', 'os', 'strconv']
			c.check_valid_snake_case(node.name, 'module name', node.pos)
		}
		ast.Return {
			// c.returns = true
			c.return_stmt(mut node)
			c.scope_returns = true
		}
		ast.SqlStmt {
			c.sql_stmt(mut node)
		}
		ast.StructDecl {
			c.struct_decl(mut node)
		}
		ast.TypeDecl {
			c.type_decl(node)
		}
	}
}

fn (mut c Checker) assert_stmt(node ast.AssertStmt) {
	cur_exp_typ := c.expected_type
	assert_type := c.check_expr_opt_call(node.expr, c.expr(node.expr))
	if assert_type != ast.bool_type_idx {
		atype_name := c.table.get_type_symbol(assert_type).name
		c.error('assert can be used only with `bool` expressions, but found `$atype_name` instead',
			node.pos)
	}
	c.expected_type = cur_exp_typ
}

fn (mut c Checker) block(node ast.Block) {
	if node.is_unsafe {
		c.inside_unsafe = true
		c.stmts(node.stmts)
		c.inside_unsafe = false
	} else {
		c.stmts(node.stmts)
	}
}

fn (mut c Checker) branch_stmt(node ast.BranchStmt) {
	if c.inside_defer {
		c.error('`$node.kind.str()` is not allowed in defer statements', node.pos)
	}
	if c.in_for_count == 0 {
		c.error('$node.kind.str() statement not within a loop', node.pos)
	}
	if node.label.len > 0 {
		if node.label != c.loop_label {
			c.error('invalid label name `$node.label`', node.pos)
		}
	}
}

fn (mut c Checker) for_c_stmt(node ast.ForCStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	if node.has_init {
		c.stmt(node.init)
	}
	c.expr(node.cond)
	if node.has_inc {
		c.stmt(node.inc)
	}
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) comptime_for(node ast.ComptimeFor) {
	typ := c.unwrap_generic(node.typ)
	sym := c.table.get_type_symbol(typ)
	if sym.kind == .placeholder || typ.has_flag(.generic) {
		c.error('unknown type `$sym.name`', node.typ_pos)
	}
	if node.kind == .fields {
		c.comptime_fields_type[node.val_var] = node.typ
		c.comptime_fields_default_type = node.typ
	}
	c.stmts(node.stmts)
}

fn (mut c Checker) for_in_stmt(mut node ast.ForInStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	typ := c.expr(node.cond)
	typ_idx := typ.idx()
	if node.key_var.len > 0 && node.key_var != '_' {
		c.check_valid_snake_case(node.key_var, 'variable name', node.pos)
	}
	if node.val_var.len > 0 && node.val_var != '_' {
		c.check_valid_snake_case(node.val_var, 'variable name', node.pos)
	}
	if node.is_range {
		high_type := c.expr(node.high)
		high_type_idx := high_type.idx()
		if typ_idx in ast.integer_type_idxs && high_type_idx !in ast.integer_type_idxs {
			c.error('range types do not match', node.cond.position())
		} else if typ_idx in ast.float_type_idxs || high_type_idx in ast.float_type_idxs {
			c.error('range type can not be float', node.cond.position())
		} else if typ_idx == ast.bool_type_idx || high_type_idx == ast.bool_type_idx {
			c.error('range type can not be bool', node.cond.position())
		} else if typ_idx == ast.string_type_idx || high_type_idx == ast.string_type_idx {
			c.error('range type can not be string', node.cond.position())
		}
		if high_type in [ast.int_type, ast.int_literal_type] {
			node.val_type = typ
		} else {
			node.val_type = high_type
		}
		node.high_type = high_type
		node.scope.update_var_type(node.val_var, node.val_type)
	} else {
		sym := c.table.get_final_type_symbol(typ)
		if sym.kind == .struct_ {
			// iterators
			next_fn := sym.find_method_with_generic_parent('next') or {
				c.error('a struct must have a `next()` method to be an iterator', node.cond.position())
				return
			}
			if !next_fn.return_type.has_flag(.optional) {
				c.error('iterator method `next()` must return an optional', node.cond.position())
			}
			// the receiver
			if next_fn.params.len != 1 {
				c.error('iterator method `next()` must have 0 parameters', node.cond.position())
			}
			mut val_type := next_fn.return_type.clear_flag(.optional)
			if node.val_is_mut {
				val_type = val_type.ref()
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = val_type
			node.scope.update_var_type(node.val_var, val_type)
		} else {
			if sym.kind == .map && !(node.key_var.len > 0 && node.val_var.len > 0) {
				c.error(
					'declare a key and a value variable when ranging a map: `for key, val in map {`\n' +
					'use `_` if you do not need the variable', node.pos)
			}
			if node.key_var.len > 0 {
				key_type := match sym.kind {
					.map { sym.map_info().key_type }
					else { ast.int_type }
				}
				node.key_type = key_type
				node.scope.update_var_type(node.key_var, key_type)
			}
			mut value_type := c.table.value_type(typ)
			if value_type == ast.void_type || typ.has_flag(.optional) {
				if typ != ast.void_type {
					c.error('for in: cannot index `${c.table.type_to_str(typ)}`', node.cond.position())
				}
			}
			if node.val_is_mut {
				value_type = value_type.ref()
				match node.cond {
					ast.Ident {
						if node.cond.obj is ast.Var {
							obj := node.cond.obj as ast.Var
							if !obj.is_mut {
								c.error('`$obj.name` is immutable, it cannot be changed',
									node.cond.pos)
							}
						}
					}
					ast.ArrayInit {
						c.error('array literal is immutable, it cannot be changed', node.cond.pos)
					}
					ast.MapInit {
						c.error('map literal is immutable, it cannot be changed', node.cond.pos)
					}
					ast.SelectorExpr {
						root_ident := node.cond.root_ident() or { node.cond.expr as ast.Ident }
						if !(root_ident.obj as ast.Var).is_mut {
							c.error('field `$node.cond.field_name` is immutable, it cannot be changed',
								node.cond.pos)
						}
					}
					else {}
				}
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = value_type
			node.scope.update_var_type(node.val_var, value_type)
		}
	}
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) for_stmt(mut node ast.ForStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	c.expected_type = ast.bool_type
	typ := c.expr(node.cond)
	if !node.is_inf && typ.idx() != ast.bool_type_idx && !c.pref.translated {
		c.error('non-bool used as for condition', node.pos)
	}
	if node.cond is ast.InfixExpr {
		infix := node.cond
		if infix.op == .key_is {
			if infix.left in [ast.Ident, ast.SelectorExpr] && infix.right is ast.TypeNode {
				is_variable := if mut infix.left is ast.Ident {
					infix.left.kind == .variable
				} else {
					true
				}
				left_type := c.expr(infix.left)
				left_sym := c.table.get_type_symbol(left_type)
				if is_variable {
					if left_sym.kind in [.sum_type, .interface_] {
						c.smartcast(infix.left, infix.left_type, infix.right.typ, mut
							node.scope)
					}
				}
			}
		}
	}
	// TODO: update loop var type
	// how does this work currenly?
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) global_decl(mut node ast.GlobalDecl) {
	for mut field in node.fields {
		c.check_valid_snake_case(field.name, 'global name', field.pos)
		if field.name in c.global_names {
			c.error('duplicate global `$field.name`', field.pos)
		}
		sym := c.table.get_type_symbol(field.typ)
		if sym.kind == .placeholder {
			c.error('unknown type `$sym.name`', field.typ_pos)
		}
		if field.has_expr {
			field.typ = c.expr(field.expr)
			mut v := c.file.global_scope.find_global(field.name) or {
				panic('internal compiler error - could not find global in scope')
			}
			v.typ = c.table.mktyp(field.typ)
		}
		c.global_names << field.name
	}
}

fn (mut c Checker) go_expr(mut node ast.GoExpr) ast.Type {
	ret_type := c.call_expr(mut node.call_expr)
	if node.call_expr.or_block.kind != .absent {
		c.error('optional handling cannot be done in `go` call. Do it when calling `.wait()`',
			node.call_expr.or_block.pos)
	}
	// Make sure there are no mutable arguments
	for arg in node.call_expr.args {
		if arg.is_mut && !arg.typ.is_ptr() {
			c.error('function in `go` statement cannot contain mutable non-reference arguments',
				arg.expr.position())
		}
	}
	if node.call_expr.is_method && node.call_expr.receiver_type.is_ptr()
		&& !node.call_expr.left_type.is_ptr() {
		c.error('method in `go` statement cannot have non-reference mutable receiver',
			node.call_expr.left.position())
	}

	if c.pref.backend.is_js() {
		return c.table.find_or_register_promise(ret_type)
	} else {
		return c.table.find_or_register_thread(ret_type)
	}
}

fn (mut c Checker) asm_stmt(mut stmt ast.AsmStmt) {
	if stmt.is_goto {
		c.warn('inline assembly goto is not supported, it will most likely not work',
			stmt.pos)
	}
	if c.pref.backend.is_js() {
		c.error('inline assembly is not supported in the js backend', stmt.pos)
	}
	if c.pref.backend == .c && c.pref.ccompiler_type == .msvc {
		c.error('msvc compiler does not support inline assembly', stmt.pos)
	}
	mut aliases := c.asm_ios(stmt.output, mut stmt.scope, true)
	aliases2 := c.asm_ios(stmt.input, mut stmt.scope, false)
	aliases << aliases2
	for mut template in stmt.templates {
		if template.is_directive {
			/*
			align n[,value]
			.skip n[,value]
			.space n[,value]
			.byte value1[,...]
			.word value1[,...]
			.short value1[,...]
			.int value1[,...]
			.long value1[,...]
			.quad immediate_value1[,...]
			.globl symbol
			.global symbol
			.section section
			.text
			.data
			.bss
			.fill repeat[,size[,value]]
			.org n
			.previous
			.string string[,...]
			.asciz string[,...]
			.ascii string[,...]
			*/
			if template.name !in ['skip', 'space', 'byte', 'word', 'short', 'int', 'long', 'quad',
				'globl', 'global', 'section', 'text', 'data', 'bss', 'fill', 'org', 'previous',
				'string', 'asciz', 'ascii'] { // all tcc-supported assembler directives
				c.error('unknown assembler directive: `$template.name`', template.pos)
			}
		}
		for mut arg in template.args {
			c.asm_arg(arg, stmt, aliases)
		}
	}
	for mut clob in stmt.clobbered {
		c.asm_arg(clob.reg, stmt, aliases)
	}
}

fn (mut c Checker) asm_arg(arg ast.AsmArg, stmt ast.AsmStmt, aliases []string) {
	match mut arg {
		ast.AsmAlias {}
		ast.AsmAddressing {
			if arg.scale !in [-1, 1, 2, 4, 8] {
				c.error('scale must be one of 1, 2, 4, or 8', arg.pos)
			}
			c.asm_arg(arg.displacement, stmt, aliases)
			c.asm_arg(arg.base, stmt, aliases)
			c.asm_arg(arg.index, stmt, aliases)
		}
		ast.BoolLiteral {} // all of these are guarented to be correct.
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.AsmRegister {} // if the register is not found, the parser will register it as an alias
		ast.AsmDisp {}
		string {}
	}
}

fn (mut c Checker) asm_ios(ios []ast.AsmIO, mut scope ast.Scope, output bool) []string {
	mut aliases := []string{}
	for io in ios {
		typ := c.expr(io.expr)
		if output {
			c.fail_if_immutable(io.expr)
		}
		if io.alias != '' {
			aliases << io.alias
			if io.alias in scope.objects {
				scope.objects[io.alias] = ast.Var{
					name: io.alias
					expr: io.expr
					is_arg: true
					typ: typ
					orig_type: typ
					pos: io.pos
				}
			}
		}
	}

	return aliases
}

fn (mut c Checker) hash_stmt(mut node ast.HashStmt) {
	if c.skip_flags {
		return
	}
	if c.ct_cond_stack.len > 0 {
		node.ct_conds = c.ct_cond_stack.clone()
	}
	if c.pref.backend.is_js() {
		if !c.file.path.ends_with('.js.v') {
			c.error('hash statements are only allowed in backend specific files such "x.js.v"',
				node.pos)
		}
		if c.mod == 'main' {
			c.error('hash statements are not allowed in the main module. Place them in a separate module.',
				node.pos)
		}
		return
	}
	match node.kind {
		'include' {
			mut flag := node.main
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				vroot := util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VEXEROOT') {
				vroot := flag.replace('@VEXEROOT', os.dir(pref.vexe_path()))
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('@VMODROOT') {
				vroot := util.resolve_vmodroot(flag, c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
				node.val = 'include $vroot'
				node.main = vroot
				flag = vroot
			}
			if flag.contains('\$env(') {
				env := util.resolve_env_value(flag, true) or {
					c.error(err.msg, node.pos)
					return
				}
				node.main = env
			}
			flag_no_comment := flag.all_before('//').trim_space()
			if !((flag_no_comment.starts_with('"') && flag_no_comment.ends_with('"'))
				|| (flag_no_comment.starts_with('<') && flag_no_comment.ends_with('>'))) {
				c.error('including C files should use either `"header_file.h"` or `<header_file.h>` quoting',
					node.pos)
			}
		}
		'pkgconfig' {
			args := if node.main.contains('--') {
				node.main.split(' ')
			} else {
				'--cflags --libs $node.main'.split(' ')
			}
			mut m := pkgconfig.main(args) or {
				c.error(err.msg, node.pos)
				return
			}
			cflags := m.run() or {
				c.error(err.msg, node.pos)
				return
			}
			c.table.parse_cflag(cflags, c.mod, c.pref.compile_defines_all) or {
				c.error(err.msg, node.pos)
				return
			}
		}
		'flag' {
			// #flag linux -lm
			mut flag := node.main
			if flag.contains('@VROOT') {
				// c.note(checker.vroot_is_deprecated_message, node.pos)
				flag = util.resolve_vmodroot(flag.replace('@VROOT', '@VMODROOT'), c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
			}
			if flag.contains('@VEXEROOT') {
				// expand `@VEXEROOT` to its absolute path
				flag = flag.replace('@VEXEROOT', os.dir(pref.vexe_path()))
			}
			if flag.contains('@VMODROOT') {
				flag = util.resolve_vmodroot(flag, c.file.path) or {
					c.error(err.msg, node.pos)
					return
				}
			}
			if flag.contains('\$env(') {
				flag = util.resolve_env_value(flag, true) or {
					c.error(err.msg, node.pos)
					return
				}
			}
			for deprecated in ['@VMOD', '@VMODULE', '@VPATH', '@VLIB_PATH'] {
				if flag.contains(deprecated) {
					if !flag.contains('@VMODROOT') {
						c.error('$deprecated had been deprecated, use @VMODROOT instead.',
							node.pos)
					}
				}
			}
			// println('adding flag "$flag"')
			c.table.parse_cflag(flag, c.mod, c.pref.compile_defines_all) or {
				c.error(err.msg, node.pos)
			}
		}
		else {
			if node.kind != 'define' {
				c.error('expected `#define`, `#flag`, `#include` or `#pkgconfig` not $node.val',
					node.pos)
			}
		}
	}
}

fn (mut c Checker) import_stmt(node ast.Import) {
	c.check_valid_snake_case(node.alias, 'module alias', node.pos)
	for sym in node.syms {
		name := '${node.mod}.$sym.name'
		if sym.name[0].is_capital() {
			if type_sym := c.table.find_type(name) {
				if type_sym.kind != .placeholder {
					if !type_sym.is_public {
						c.error('module `$node.mod` type `$sym.name` is private', sym.pos)
					}
					continue
				}
			}
			c.error('module `$node.mod` has no type `$sym.name`', sym.pos)
			continue
		}
		if func := c.table.find_fn(name) {
			if !func.is_pub {
				c.error('module `$node.mod` function `${sym.name}()` is private', sym.pos)
			}
			continue
		}
		if _ := c.file.global_scope.find_const(name) {
			continue
		}
		c.error('module `$node.mod` has no constant or function `$sym.name`', sym.pos)
	}
}

// stmts should be used for processing normal statement lists (fn bodies, for loop bodies etc).
fn (mut c Checker) stmts(stmts []ast.Stmt) {
	old_stmt_level := c.stmt_level
	c.stmt_level = 0
	c.stmts_ending_with_expression(stmts)
	c.stmt_level = old_stmt_level
}

// stmts_ending_with_expression, should be used for processing list of statements, that can end with an expression.
// Examples for such lists are the bodies of `or` blocks, `if` expressions and `match` expressions:
//    `x := opt() or { stmt1 stmt2 ExprStmt }`,
//    `x := if cond { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`,
//    `x := match expr { Type1 { stmt1 stmt2 ExprStmt } else { stmt2 stmt3 ExprStmt }`.
fn (mut c Checker) stmts_ending_with_expression(stmts []ast.Stmt) {
	mut unreachable := token.Position{
		line_nr: -1
	}
	c.expected_type = ast.void_type
	c.stmt_level++
	for i, stmt in stmts {
		c.is_last_stmt = i == stmts.len - 1
		if c.scope_returns {
			if unreachable.line_nr == -1 {
				unreachable = stmt.pos
			}
		}
		c.stmt(stmt)
		if stmt is ast.GotoLabel {
			unreachable = token.Position{
				line_nr: -1
			}
			c.scope_returns = false
		}
		if c.should_abort {
			return
		}
	}
	c.stmt_level--
	if unreachable.line_nr >= 0 {
		c.error('unreachable code', unreachable)
	}
	c.find_unreachable_statements_after_noreturn_calls(stmts)
	c.scope_returns = false
	c.expected_type = ast.void_type
}

pub fn (mut c Checker) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if t_typ := c.table.resolve_generic_to_concrete(typ, c.table.cur_fn.generic_names,
			c.table.cur_concrete_types)
		{
			return t_typ
		}
	}
	return typ
}

// TODO node must be mut
pub fn (mut c Checker) expr(node ast.Expr) ast.Type {
	c.expr_level++
	defer {
		c.expr_level--
	}

	// c.expr_level set to 150 so that stack overflow does not occur on windows
	if c.expr_level > 150 {
		c.error('checker: too many expr levels: $c.expr_level ', node.position())
		return ast.void_type
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {
			c.error('checker.expr(): unhandled EmptyExpr', token.Position{})
		}
		ast.CTempVar {
			return node.typ
		}
		ast.AnonFn {
			return c.anon_fn(mut node)
		}
		ast.ArrayDecompose {
			typ := c.expr(node.expr)
			type_sym := c.table.get_type_symbol(typ)
			if type_sym.kind != .array {
				c.error('decomposition can only be used on arrays', node.expr.position())
				return ast.void_type
			}
			array_info := type_sym.info as ast.Array
			elem_type := array_info.elem_type.set_flag(.variadic)
			node.expr_type = typ
			node.arg_type = elem_type
			return elem_type
		}
		ast.ArrayInit {
			return c.array_init(mut node)
		}
		ast.AsCast {
			node.expr_type = c.expr(node.expr)
			expr_type_sym := c.table.get_type_symbol(node.expr_type)
			type_sym := c.table.get_type_symbol(node.typ)
			if expr_type_sym.kind == .sum_type {
				c.ensure_type_exists(node.typ, node.pos) or {}
				if !c.table.sumtype_has_variant(node.expr_type, node.typ, true) {
					addr := '&'.repeat(node.typ.nr_muls())
					c.error('cannot cast `$expr_type_sym.name` to `$addr$type_sym.name`',
						node.pos)
				}
			} else if expr_type_sym.kind == .interface_ && type_sym.kind == .interface_ {
				c.ensure_type_exists(node.typ, node.pos) or {}
			} else if node.expr_type != node.typ {
				mut s := 'cannot cast non-sum type `$expr_type_sym.name` using `as`'
				if type_sym.kind == .sum_type {
					s += ' - use e.g. `${type_sym.name}(some_expr)` instead.'
				}
				c.error(s, node.pos)
			}
			return node.typ
		}
		ast.Assoc {
			v := node.scope.find_var(node.var_name) or { panic(err) }
			for i, _ in node.fields {
				c.expr(node.exprs[i])
			}
			node.typ = v.typ
			return v.typ
		}
		ast.BoolLiteral {
			return ast.bool_type
		}
		ast.CastExpr {
			return c.cast_expr(mut node)
		}
		ast.CallExpr {
			mut ret_type := c.call_expr(mut node)
			if !ret_type.has_flag(.optional) {
				if node.or_block.kind == .block {
					c.error('unexpected `or` block, the function `$node.name` does not return an optional',
						node.or_block.pos)
				} else if node.or_block.kind == .propagate {
					c.error('unexpected `?`, the function `$node.name` does not return an optional',
						node.or_block.pos)
				}
			}
			if ret_type.has_flag(.optional) && node.or_block.kind != .absent {
				ret_type = ret_type.clear_flag(.optional)
			}
			return ret_type
		}
		ast.ChanInit {
			return c.chan_init(mut node)
		}
		ast.CharLiteral {
			// return int_literal, not rune, so that we can do "bytes << `A`" without a cast etc
			// return ast.int_literal_type
			return ast.rune_type
			// return ast.byte_type
		}
		ast.Comment {
			return ast.void_type
		}
		ast.AtExpr {
			return c.at_expr(mut node)
		}
		ast.ComptimeCall {
			return c.comptime_call(mut node)
		}
		ast.ComptimeSelector {
			node.left_type = c.unwrap_generic(c.expr(node.left))
			expr_type := c.unwrap_generic(c.expr(node.field_expr))
			expr_sym := c.table.get_type_symbol(expr_type)
			if expr_type != ast.string_type {
				c.error('expected `string` instead of `$expr_sym.name` (e.g. `field.name`)',
					node.field_expr.position())
			}
			if node.field_expr is ast.SelectorExpr {
				left_pos := node.field_expr.expr.position()
				if c.comptime_fields_type.len == 0 {
					c.error('compile time field access can only be used when iterating over `T.fields`',
						left_pos)
				}
				expr_name := node.field_expr.expr.str()
				if expr_name in c.comptime_fields_type {
					return c.comptime_fields_type[expr_name]
				}
				c.error('unknown `\$for` variable `$expr_name`', left_pos)
			} else {
				c.error('expected selector expression e.g. `$(field.name)`', node.field_expr.position())
			}
			return ast.void_type
		}
		ast.ConcatExpr {
			return c.concat_expr(mut node)
		}
		ast.DumpExpr {
			node.expr_type = c.expr(node.expr)
			if node.expr_type.idx() == ast.void_type_idx {
				c.error('dump expression can not be void', node.expr.position())
				return ast.void_type
			}
			tsym := c.table.get_type_symbol(node.expr_type)
			c.table.dumps[int(node.expr_type)] = tsym.cname
			node.cname = tsym.cname
			return node.expr_type
		}
		ast.EnumVal {
			return c.enum_val(mut node)
		}
		ast.FloatLiteral {
			return ast.float_literal_type
		}
		ast.GoExpr {
			return c.go_expr(mut node)
		}
		ast.Ident {
			// c.checked_ident = node.name
			res := c.ident(mut node)
			// c.checked_ident = ''
			return res
		}
		ast.IfExpr {
			return c.if_expr(mut node)
		}
		ast.IfGuardExpr {
			old_inside_if_guard := c.inside_if_guard
			c.inside_if_guard = true
			node.expr_type = c.expr(node.expr)
			c.inside_if_guard = old_inside_if_guard
			if !node.expr_type.has_flag(.optional) {
				mut no_opt := true
				match mut node.expr {
					ast.IndexExpr {
						no_opt = false
						node.expr_type = node.expr_type.set_flag(.optional)
						node.expr.is_option = true
					}
					ast.PrefixExpr {
						if node.expr.op == .arrow {
							no_opt = false
							node.expr_type = node.expr_type.set_flag(.optional)
							node.expr.is_option = true
						}
					}
					else {}
				}
				if no_opt {
					c.error('expression should return an option', node.expr.position())
				}
			}
			return ast.bool_type
		}
		ast.IndexExpr {
			return c.index_expr(mut node)
		}
		ast.InfixExpr {
			return c.infix_expr(mut node)
		}
		ast.IntegerLiteral {
			return c.int_lit(mut node)
		}
		ast.LockExpr {
			return c.lock_expr(mut node)
		}
		ast.MapInit {
			return c.map_init(mut node)
		}
		ast.MatchExpr {
			return c.match_expr(mut node)
		}
		ast.PostfixExpr {
			return c.postfix_expr(mut node)
		}
		ast.PrefixExpr {
			return c.prefix_expr(mut node)
		}
		ast.None {
			return ast.none_type
		}
		ast.OrExpr {
			// never happens
			return ast.void_type
		}
		// ast.OrExpr2 {
		// return node.typ
		// }
		ast.ParExpr {
			if node.expr is ast.ParExpr {
				c.warn('redundant parentheses are used', node.pos)
			}
			return c.expr(node.expr)
		}
		ast.RangeExpr {
			// never happens
			return ast.void_type
		}
		ast.SelectExpr {
			return c.select_expr(mut node)
		}
		ast.SelectorExpr {
			return c.selector_expr(mut node)
		}
		ast.SizeOf {
			if !node.is_type {
				node.typ = c.expr(node.expr)
			}
			return ast.u32_type
		}
		ast.IsRefType {
			if !node.is_type {
				node.typ = c.expr(node.expr)
			}
			return ast.bool_type
		}
		ast.OffsetOf {
			return c.offset_of(node)
		}
		ast.SqlExpr {
			return c.sql_expr(mut node)
		}
		ast.StringLiteral {
			if node.language == .c {
				// string literal starts with "c": `C.printf(c'hello')`
				return ast.byte_type.set_nr_muls(1)
			}
			return c.string_lit(mut node)
		}
		ast.StringInterLiteral {
			return c.string_inter_lit(mut node)
		}
		ast.StructInit {
			if node.unresolved {
				return c.expr(ast.resolve_init(node, c.unwrap_generic(node.typ), c.table))
			}
			return c.struct_init(mut node)
		}
		ast.TypeNode {
			return node.typ
		}
		ast.TypeOf {
			node.expr_type = c.expr(node.expr)
			return ast.string_type
		}
		ast.UnsafeExpr {
			return c.unsafe_expr(mut node)
		}
		ast.Likely {
			ltype := c.expr(node.expr)
			if !c.check_types(ltype, ast.bool_type) {
				ltype_sym := c.table.get_type_symbol(ltype)
				lname := if node.is_likely { '_likely_' } else { '_unlikely_' }
				c.error('`${lname}()` expects a boolean expression, instead it got `$ltype_sym.name`',
					node.pos)
			}
			return ast.bool_type
		}
	}
	return ast.void_type
}

// pub fn (mut c Checker) asm_reg(mut node ast.AsmRegister) ast.Type {
// 	name := node.name

// 	for bit_size, array in ast.x86_no_number_register_list {
// 		if name in array {
// 			return c.table.bitsize_to_type(bit_size)
// 		}
// 	}
// 	for bit_size, array in ast.x86_with_number_register_list {
// 		if name in array {
// 			return c.table.bitsize_to_type(bit_size)
// 		}
// 	}
// 	c.error('invalid register name: `$name`', node.pos)
// 	return ast.void_type
// }

pub fn (mut c Checker) cast_expr(mut node ast.CastExpr) ast.Type {
	// Given: `Outside( Inside(xyz) )`,
	//        node.expr_type: `Inside`
	//        node.typ: `Outside`
	node.expr_type = c.expr(node.expr) // type to be casted
	mut from_type := node.expr_type
	to_type := node.typ
	//
	from_type_sym := c.table.get_type_symbol(from_type)
	from_type_sym_final := c.table.get_final_type_symbol(from_type)
	to_type_sym := c.table.get_type_symbol(to_type) // type to be used as cast
	to_type_sym_final := c.table.get_final_type_symbol(to_type)
	//
	if (to_type_sym.is_number() && from_type_sym.name == 'JS.Number')
		|| (to_type_sym.is_number() && from_type_sym.name == 'JS.BigInt')
		|| (to_type_sym.is_string() && from_type_sym.name == 'JS.String')
		|| (to_type.is_bool() && from_type_sym.name == 'JS.Boolean')
		|| (from_type.is_bool() && to_type_sym.name == 'JS.Boolean')
		|| (from_type_sym.is_number() && to_type_sym.name == 'JS.Number')
		|| (from_type_sym.is_number() && to_type_sym.name == 'JS.BigInt')
		|| (from_type_sym.is_string() && to_type_sym.name == 'JS.String') {
		return to_type
	}

	if to_type_sym.language != .c {
		c.ensure_type_exists(to_type, node.pos) or {}
	}
	if from_type_sym.kind == .byte && from_type.is_ptr() && to_type_sym.kind == .string
		&& !to_type.is_ptr() {
		c.error('to convert a C string buffer pointer to a V string, use x.vstring() instead of string(x)',
			node.pos)
	}
	if from_type == ast.void_type {
		c.error('expression does not return a value so it cannot be cast', node.expr.position())
	}
	//
	if to_type_sym.kind == .sum_type {
		if from_type in [ast.int_literal_type, ast.float_literal_type] {
			xx := if from_type == ast.int_literal_type { ast.int_type } else { ast.f64_type }
			node.expr_type = c.promote_num(node.expr_type, xx)
			from_type = node.expr_type
		}
		if !c.table.sumtype_has_variant(to_type, from_type, false) && !to_type.has_flag(.optional) {
			c.error('cannot cast `$from_type_sym.name` to `$to_type_sym.name`', node.pos)
		}
	} else if mut to_type_sym.info is ast.Alias {
		if !c.check_types(from_type, to_type_sym.info.parent_type) {
			c.error('cannot convert type `$from_type_sym.name` to `$to_type_sym.name` (alias to `$to_type_sym_final.name`)',
				node.pos)
		}
	} else if to_type_sym.kind == .byte && from_type != ast.voidptr_type
		&& from_type_sym.kind != .enum_ && !from_type.is_int() && !from_type.is_float()
		&& from_type != ast.bool_type && !from_type.is_ptr() && from_type_sym.kind == .alias
		&& from_type_sym_final.name != 'byte' {
		type_name := c.table.type_to_str(from_type)
		c.error('cannot cast type `$type_name` to `byte`', node.pos)
	} else if to_type_sym.kind == .struct_ && !to_type.is_ptr()
		&& !(to_type_sym.info as ast.Struct).is_typedef {
		// For now we ignore C typedef because of `C.Window(C.None)` in vlib/clipboard
		if from_type_sym.kind == .struct_ && !from_type.is_ptr() {
			c.warn('casting to struct is deprecated, use e.g. `Struct{...expr}` instead',
				node.pos)
			from_type_info := from_type_sym.info as ast.Struct
			to_type_info := to_type_sym.info as ast.Struct
			if !c.check_struct_signature(from_type_info, to_type_info) {
				c.error('cannot convert struct `$from_type_sym.name` to struct `$to_type_sym.name`',
					node.pos)
			}
		} else {
			type_name := c.table.type_to_str(from_type)
			c.error('cannot cast `$type_name` to struct', node.pos)
		}
	} else if to_type_sym.kind == .interface_ {
		if c.type_implements(from_type, to_type, node.pos) {
			if !from_type.is_ptr() && !from_type.is_pointer() && from_type_sym.kind != .interface_
				&& !c.inside_unsafe {
				c.mark_as_referenced(mut &node.expr, true)
			}
		}
	} else if to_type == ast.bool_type && from_type != ast.bool_type && !c.inside_unsafe {
		c.error('cannot cast to bool - use e.g. `some_int != 0` instead', node.pos)
	} else if from_type == ast.none_type && !to_type.has_flag(.optional) {
		type_name := c.table.type_to_str(to_type)
		c.error('cannot cast `none` to `$type_name`', node.pos)
	} else if from_type_sym.kind == .struct_ && !from_type.is_ptr() {
		if (to_type.is_ptr() || to_type_sym.kind !in [.sum_type, .interface_]) && !c.is_builtin_mod {
			from_type_name := c.table.type_to_str(from_type)
			type_name := c.table.type_to_str(to_type)
			snexpr := node.expr.str()
			c.error('cannot cast struct `$from_type_name` to `$type_name`, use `${snexpr}.str()` instead.',
				node.pos)
		}
	} else if from_type.has_flag(.optional) || from_type.has_flag(.variadic) {
		// variadic case can happen when arrays are converted into variadic
		msg := if from_type.has_flag(.optional) { 'an optional' } else { 'a variadic' }
		c.error('cannot type cast $msg', node.pos)
	} else if !c.inside_unsafe && to_type.is_ptr() && from_type.is_ptr()
		&& to_type.deref() != ast.char_type && from_type.deref() != ast.char_type {
		ft := c.table.type_to_str(from_type)
		tt := c.table.type_to_str(to_type)
		c.warn('casting `$ft` to `$tt` is only allowed in `unsafe` code', node.pos)
	} else if from_type_sym.kind == .array_fixed && !from_type.is_ptr() {
		c.warn('cannot cast a fixed array (use e.g. `&arr[0]` instead)', node.pos)
	}

	if to_type == ast.string_type {
		if from_type in [ast.byte_type, ast.bool_type] {
			snexpr := node.expr.str()
			c.error('cannot cast type `$from_type_sym.name` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if from_type.is_real_pointer() {
			snexpr := node.expr.str()
			c.error('cannot cast pointer type `$from_type_sym.name` to string, use `&byte($snexpr).vstring()` or `cstring_to_vstring($snexpr)` instead.',
				node.pos)
		} else if from_type.is_number() {
			snexpr := node.expr.str()
			c.error('cannot cast number to string, use `${snexpr}.str()` instead.', node.pos)
		} else if from_type_sym.kind == .alias && from_type_sym_final.name != 'string' {
			c.error('cannot cast type `$from_type_sym.name` to string, use `x.str()` instead.',
				node.pos)
		} else if from_type_sym_final.kind == .array {
			snexpr := node.expr.str()
			if from_type_sym_final.name == '[]byte' {
				c.error('cannot cast []byte to string, use `${snexpr}.bytestr()` or `${snexpr}.str()` instead.',
					node.pos)
			} else {
				first_elem_idx := '[0]'
				c.error('cannot cast array to string, use `$snexpr${first_elem_idx}.str()` instead.',
					node.pos)
			}
		} else if from_type_sym_final.kind == .enum_ {
			snexpr := node.expr.str()
			c.error('cannot cast enum to string, use ${snexpr}.str() instead.', node.pos)
		} else if from_type_sym_final.kind == .map {
			c.error('cannot cast map to string.', node.pos)
		} else if from_type_sym_final.kind == .sum_type {
			snexpr := node.expr.str()
			c.error('cannot cast sumtype `$from_type_sym.name` to string, use `${snexpr}.str()` instead.',
				node.pos)
		} else if to_type != ast.string_type && from_type == ast.string_type
			&& (!(to_type_sym.kind == .alias && to_type_sym_final.name == 'string')) {
			mut error_msg := 'cannot cast a string to a type `$to_type_sym_final.name`, that is not an alias of string'
			if mut node.expr is ast.StringLiteral {
				if node.expr.val.len == 1 {
					error_msg += ", for denoting characters use `$node.expr.val` instead of '$node.expr.val'"
				}
			}
			c.error(error_msg, node.pos)
		}
	}

	if node.has_arg {
		c.expr(node.arg)
	}

	// checks on int literal to enum cast if the value represents a value on the enum
	if to_type_sym.kind == .enum_ {
		if node.expr is ast.IntegerLiteral {
			enum_typ_name := c.table.get_type_name(to_type)
			node_val := (node.expr as ast.IntegerLiteral).val.int()

			if enum_decl := c.table.enum_decls[to_type_sym.name] {
				mut in_range := false
				mut enum_val := 0

				for enum_field in enum_decl.fields {
					// check if the field of the enum value is an integer literal
					if enum_field.expr is ast.IntegerLiteral {
						enum_val = enum_field.expr.val.int()
					}

					if node_val == enum_val {
						in_range = true
						break
					}

					enum_val += 1
				}

				if !in_range {
					c.warn('$node_val does not represents a value of enum $enum_typ_name',
						node.pos)
				}
			}
		}
	}

	node.typname = c.table.get_type_symbol(to_type).name

	return to_type
}

fn (mut c Checker) at_expr(mut node ast.AtExpr) ast.Type {
	match node.kind {
		.fn_name {
			node.val = c.table.cur_fn.name.all_after_last('.')
		}
		.method_name {
			fname := c.table.cur_fn.name.all_after_last('.')
			if c.table.cur_fn.is_method {
				node.val = c.table.type_to_str(c.table.cur_fn.receiver.typ).all_after_last('.') +
					'.' + fname
			} else {
				node.val = fname
			}
		}
		.mod_name {
			node.val = c.table.cur_fn.mod
		}
		.struct_name {
			if c.table.cur_fn.is_method {
				node.val = c.table.type_to_str(c.table.cur_fn.receiver.typ).all_after_last('.')
			} else {
				node.val = ''
			}
		}
		.vexe_path {
			node.val = pref.vexe_path()
		}
		.file_path {
			node.val = os.real_path(c.file.path)
		}
		.line_nr {
			node.val = (node.pos.line_nr + 1).str()
		}
		.column_nr {
			node.val = (node.pos.col + 1).str()
		}
		.vhash {
			node.val = version.vhash()
		}
		.vmod_file {
			// cache the vmod content, do not read it many times
			if c.vmod_file_content.len == 0 {
				mut mcache := vmod.get_cache()
				vmod_file_location := mcache.get_by_file(c.file.path)
				if vmod_file_location.vmod_file.len == 0 {
					c.error('@VMOD_FILE can be used only in projects, that have v.mod file',
						node.pos)
				}
				vmod_content := os.read_file(vmod_file_location.vmod_file) or { '' }
				c.vmod_file_content = vmod_content.replace('\r\n', '\n') // normalise EOLs just in case
			}
			node.val = c.vmod_file_content
		}
		.vroot_path {
			node.val = os.dir(pref.vexe_path())
		}
		.vexeroot_path {
			node.val = os.dir(pref.vexe_path())
		}
		.vmodroot_path {
			mut mcache := vmod.get_cache()
			vmod_file_location := mcache.get_by_file(c.file.path)
			node.val = os.dir(vmod_file_location.vmod_file)
		}
		.unknown {
			c.error('unknown @ identifier: ${node.name}. Available identifiers: $token.valid_at_tokens',
				node.pos)
		}
	}
	return ast.string_type
}

pub fn (mut c Checker) ident(mut node ast.Ident) ast.Type {
	// TODO: move this
	if c.const_deps.len > 0 {
		mut name := node.name
		if !name.contains('.') && node.mod != 'builtin' {
			name = '${node.mod}.$node.name'
		}
		if name == c.const_decl {
			c.error('cycle in constant `$c.const_decl`', node.pos)
			return ast.void_type
		}
		c.const_deps << name
	}
	if node.kind == .blank_ident {
		if node.tok_kind !in [.assign, .decl_assign] {
			c.error('undefined ident: `_` (may only be used in assignments)', node.pos)
		}
		return ast.void_type
	}
	// second use
	if node.kind in [.constant, .global, .variable] {
		info := node.info as ast.IdentVar
		// Got a var with type T, return current generic type
		return info.typ
	} else if node.kind == .function {
		info := node.info as ast.IdentFn
		return info.typ
	} else if node.kind == .unresolved {
		// first use
		if node.tok_kind == .assign && node.is_mut {
			c.error('`mut` not allowed with `=` (use `:=` to declare a variable)', node.pos)
		}
		if obj := node.scope.find(node.name) {
			match mut obj {
				ast.GlobalField {
					node.kind = .global
					node.info = ast.IdentVar{
						typ: obj.typ
					}
					node.obj = obj
					return obj.typ
				}
				ast.Var {
					// incase var was not marked as used yet (vweb tmpl)
					// obj.is_used = true
					if node.pos.pos < obj.pos.pos {
						c.error('undefined variable `$node.name` (used before declaration)',
							node.pos)
					}
					is_sum_type_cast := obj.smartcasts.len != 0
						&& !c.prevent_sum_type_unwrapping_once
					c.prevent_sum_type_unwrapping_once = false
					mut typ := if is_sum_type_cast { obj.smartcasts.last() } else { obj.typ }
					if typ == 0 {
						if mut obj.expr is ast.Ident {
							if obj.expr.kind == .unresolved {
								c.error('unresolved variable: `$node.name`', node.pos)
								return ast.void_type
							}
						}
						if mut obj.expr is ast.IfGuardExpr {
							// new variable from if guard shouldn't have the optional flag for further use
							// a temp variable will be generated which unwraps it
							typ = obj.expr.expr_type.clear_flag(.optional)
						} else {
							typ = c.expr(obj.expr)
						}
					}
					is_optional := typ.has_flag(.optional)
					node.kind = .variable
					node.info = ast.IdentVar{
						typ: typ
						is_optional: is_optional
					}
					if typ == ast.error_type && c.expected_type == ast.string_type
						&& !c.using_new_err_struct && !c.inside_selector_expr
						&& !c.inside_println_arg && !c.file.mod.name.contains('v.')
						&& !c.is_builtin_mod {
						//                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ <- TODO: remove; this prevents a failure in the `performance-regressions` CI job
						c.warn('string errors are deprecated; use `err.msg` instead',
							node.pos)
					}
					// if typ == ast.t_type {
					// sym := c.table.get_type_symbol(c.cur_generic_type)
					// println('IDENT T unresolved $node.name typ=$sym.name')
					// Got a var with type T, return current generic type
					// typ = c.cur_generic_type
					// }
					// } else {
					if !is_sum_type_cast {
						obj.typ = typ
					}
					node.obj = obj
					// unwrap optional (`println(x)`)
					if is_optional {
						return typ.clear_flag(.optional)
					}
					return typ
				}
				else {}
			}
		}
		mut name := node.name
		// check for imported symbol
		if name in c.file.imported_symbols {
			name = c.file.imported_symbols[name]
		}
		// prepend mod to look for fn call or const
		else if !name.contains('.') && node.mod != 'builtin' {
			name = '${node.mod}.$node.name'
		}
		if obj := c.file.global_scope.find(name) {
			match mut obj {
				ast.ConstField {
					if !(obj.is_pub || obj.mod == c.mod || c.pref.is_test) {
						c.error('constant `$obj.name` is private', node.pos)
					}
					mut typ := obj.typ
					if typ == 0 {
						c.inside_const = true
						typ = c.expr(obj.expr)
						c.inside_const = false
						if obj.expr is ast.CallExpr {
							if obj.expr.or_block.kind != .absent {
								typ = typ.clear_flag(.optional)
							}
						}
					}
					node.name = name
					node.kind = .constant
					node.info = ast.IdentVar{
						typ: typ
					}
					obj.typ = typ
					node.obj = obj
					return typ
				}
				else {}
			}
		}
		// Non-anon-function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
			fn_type := ast.new_type(c.table.find_or_register_fn_type(node.mod, func, false,
				true))
			node.name = name
			node.kind = .function
			node.info = ast.IdentFn{
				typ: fn_type
			}
			return fn_type
		}
	}
	if node.language == .c {
		if node.name == 'C.NULL' {
			return ast.voidptr_type
		}
		return ast.int_type
	}
	if c.inside_sql {
		if field := c.table.find_field(c.cur_orm_ts, node.name) {
			return field.typ
		}
	}
	if node.kind == .unresolved && node.mod != 'builtin' {
		// search in the `builtin` idents, for example
		// main.compare_f32 may actually be builtin.compare_f32
		saved_mod := node.mod
		node.mod = 'builtin'
		builtin_type := c.ident(mut node)
		if builtin_type != ast.void_type {
			return builtin_type
		}
		node.mod = saved_mod
	}
	if node.tok_kind == .assign {
		c.error('undefined ident: `$node.name` (use `:=` to declare a variable)', node.pos)
	} else if node.name == 'errcode' {
		c.error('undefined ident: `errcode`; did you mean `err.code`?', node.pos)
	} else {
		if c.inside_ct_attr {
			c.note('`[if $node.name]` is deprecated. Use `[if $node.name?]` instead',
				node.pos)
		} else {
			c.error('undefined ident: `$node.name`', node.pos)
		}
	}
	if c.table.known_type(node.name) {
		// e.g. `User`  in `json.decode(User, '...')`
		return ast.void_type
	}
	return ast.void_type
}

pub fn (mut c Checker) concat_expr(mut node ast.ConcatExpr) ast.Type {
	mut mr_types := []ast.Type{}
	for expr in node.vals {
		mr_types << c.expr(expr)
	}
	if node.vals.len == 1 {
		typ := mr_types[0]
		node.return_type = typ
		return typ
	} else {
		typ := c.table.find_or_register_multi_return(mr_types)
		ast.new_type(typ)
		node.return_type = typ
		return typ
	}
}

// smartcast takes the expression with the current type which should be smartcasted to the target type in the given scope
fn (c Checker) smartcast(expr ast.Expr, cur_type ast.Type, to_type_ ast.Type, mut scope ast.Scope) {
	sym := c.table.get_type_symbol(cur_type)
	to_type := if sym.kind == .interface_ { to_type_.ref() } else { to_type_ }
	match expr {
		ast.SelectorExpr {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			expr_sym := c.table.get_type_symbol(expr.expr_type)
			mut orig_type := 0
			if field := c.table.find_field(expr_sym, expr.field_name) {
				if field.is_mut {
					if root_ident := expr.root_ident() {
						if v := scope.find_var(root_ident.name) {
							is_mut = v.is_mut
						}
					}
				}
				if orig_type == 0 {
					orig_type = field.typ
				}
			}
			if field := scope.find_struct_field(expr.expr.str(), expr.expr_type, expr.field_name) {
				smartcasts << field.smartcasts
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if !is_mut || expr.is_mut {
				smartcasts << to_type
				scope.register_struct_field(expr.expr.str(), ast.ScopeStructField{
					struct_type: expr.expr_type
					name: expr.field_name
					typ: cur_type
					smartcasts: smartcasts
					pos: expr.pos
					orig_type: orig_type
				})
			}
		}
		ast.Ident {
			mut is_mut := false
			mut smartcasts := []ast.Type{}
			mut is_already_casted := false
			mut orig_type := 0
			if mut expr.obj is ast.Var {
				is_mut = expr.obj.is_mut
				smartcasts << expr.obj.smartcasts
				is_already_casted = expr.obj.pos.pos == expr.pos.pos
				if orig_type == 0 {
					orig_type = expr.obj.typ
				}
			}
			// smartcast either if the value is immutable or if the mut argument is explicitly given
			if (!is_mut || expr.is_mut) && !is_already_casted {
				smartcasts << to_type
				scope.register(ast.Var{
					name: expr.name
					typ: cur_type
					pos: expr.pos
					is_used: true
					is_mut: expr.is_mut
					smartcasts: smartcasts
					orig_type: orig_type
				})
			}
		}
		else {}
	}
}

pub fn (mut c Checker) select_expr(mut node ast.SelectExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	for branch in node.branches {
		c.stmt(branch.stmt)
		match branch.stmt {
			ast.ExprStmt {
				if branch.is_timeout {
					if !branch.stmt.typ.is_int() {
						tsym := c.table.get_type_symbol(branch.stmt.typ)
						c.error('invalid type `$tsym.name` for timeout - expected integer number of nanoseconds aka `time.Duration`',
							branch.stmt.pos)
					}
				} else {
					if branch.stmt.expr is ast.InfixExpr {
						if branch.stmt.expr.left !is ast.Ident
							&& branch.stmt.expr.left !is ast.SelectorExpr
							&& branch.stmt.expr.left !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', branch.stmt.expr.left.position())
						}
					} else {
						c.error('invalid expression for `select` key', branch.stmt.expr.position())
					}
				}
			}
			ast.AssignStmt {
				expr := branch.stmt.right[0]
				match expr {
					ast.PrefixExpr {
						if expr.right !is ast.Ident && expr.right !is ast.SelectorExpr
							&& expr.right !is ast.IndexExpr {
							c.error('channel in `select` key must be predefined', expr.right.position())
						}
						if expr.or_block.kind != .absent {
							err_prefix := if expr.or_block.kind == .block {
								'or block'
							} else {
								'error propagation'
							}
							c.error('$err_prefix not allowed in `select` key', expr.or_block.pos)
						}
					}
					else {
						c.error('`<-` receive expression expected', branch.stmt.right[0].position())
					}
				}
			}
			else {
				if !branch.is_else {
					c.error('receive or send statement expected as `select` key', branch.stmt.pos)
				}
			}
		}
		c.stmts(branch.stmts)
	}
	return ast.bool_type
}

pub fn (mut c Checker) lock_expr(mut node ast.LockExpr) ast.Type {
	if c.rlocked_names.len > 0 || c.locked_names.len > 0 {
		c.error('nested `lock`/`rlock` not allowed', node.pos)
	}
	for i in 0 .. node.lockeds.len {
		e_typ := c.expr(node.lockeds[i])
		id_name := node.lockeds[i].str()
		if !e_typ.has_flag(.shared_f) {
			obj_type := if node.lockeds[i] is ast.Ident { 'variable' } else { 'struct element' }
			c.error('`$id_name` must be declared as `shared` $obj_type to be locked',
				node.lockeds[i].position())
		}
		if id_name in c.locked_names {
			c.error('`$id_name` is already locked', node.lockeds[i].position())
		} else if id_name in c.rlocked_names {
			c.error('`$id_name` is already read-locked', node.lockeds[i].position())
		}
		if node.is_rlock[i] {
			c.rlocked_names << id_name
		} else {
			c.locked_names << id_name
		}
	}
	c.stmts(node.stmts)
	c.rlocked_names = []
	c.locked_names = []
	// handle `x := rlock a { a.getval() }`
	mut ret_type := ast.void_type
	if node.stmts.len > 0 {
		last_stmt := node.stmts[node.stmts.len - 1]
		if last_stmt is ast.ExprStmt {
			ret_type = last_stmt.typ
		}
	}
	if ret_type != ast.void_type {
		node.is_expr = true
	}
	node.typ = ret_type
	return ret_type
}

pub fn (mut c Checker) unsafe_expr(mut node ast.UnsafeExpr) ast.Type {
	c.inside_unsafe = true
	t := c.expr(node.expr)
	c.inside_unsafe = false
	return t
}

fn (mut c Checker) find_definition(ident ast.Ident) ?ast.Expr {
	match ident.kind {
		.unresolved, .blank_ident { return none }
		.variable, .constant { return c.find_obj_definition(ident.obj) }
		.global { return error('$ident.name is a global variable') }
		.function { return error('$ident.name is a function') }
	}
}

fn (mut c Checker) find_obj_definition(obj ast.ScopeObject) ?ast.Expr {
	// TODO: remove once we have better type inference
	mut name := ''
	match obj {
		ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister { name = obj.name }
	}
	mut expr := ast.empty_expr()
	if obj is ast.Var {
		if obj.is_mut {
			return error('`$name` is mut and may have changed since its definition')
		}
		expr = obj.expr
	} else if obj is ast.ConstField {
		expr = obj.expr
	} else {
		return error('`$name` is a global variable and is unknown at compile time')
	}
	if expr is ast.Ident {
		return c.find_definition(expr as ast.Ident) // TODO: smartcast
	}
	if !expr.is_lit() {
		return error('definition of `$name` is unknown at compile time')
	}
	return expr
}

fn (c &Checker) has_return(stmts []ast.Stmt) ?bool {
	// complexity means either more match or ifs
	mut has_complexity := false
	for s in stmts {
		if s is ast.ExprStmt {
			if s.expr in [ast.IfExpr, ast.MatchExpr] {
				has_complexity = true
				break
			}
		}
	}
	// if the inner complexity covers all paths with returns there is no need for further checks
	if !has_complexity || !c.returns {
		return has_top_return(stmts)
	}
	return none
}

pub fn (mut c Checker) postfix_expr(mut node ast.PostfixExpr) ast.Type {
	typ := c.unwrap_generic(c.expr(node.expr))
	typ_sym := c.table.get_type_symbol(typ)
	is_non_void_pointer := (typ.is_ptr() || typ.is_pointer()) && typ_sym.kind != .voidptr
	if !c.inside_unsafe && is_non_void_pointer && !node.expr.is_auto_deref_var() {
		c.warn('pointer arithmetic is only allowed in `unsafe` blocks', node.pos)
	}
	if !(typ_sym.is_number() || (c.inside_unsafe && is_non_void_pointer)) {
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ_sym.name`)',
			node.pos)
	} else {
		node.auto_locked, _ = c.fail_if_immutable(node.expr)
	}
	return typ
}

pub fn (mut c Checker) mark_as_referenced(mut node ast.Expr, as_interface bool) {
	match mut node {
		ast.Ident {
			if mut node.obj is ast.Var {
				mut obj := unsafe { &node.obj }
				if c.fn_scope != voidptr(0) {
					obj = c.fn_scope.find_var(node.obj.name) or { obj }
				}
				type_sym := c.table.get_type_symbol(obj.typ.set_nr_muls(0))
				if obj.is_stack_obj && !type_sym.is_heap() && !c.pref.translated {
					suggestion := if type_sym.kind == .struct_ {
						'declaring `$type_sym.name` as `[heap]`'
					} else {
						'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
					}
					if !c.pref.translated {
						mischief := if as_interface {
							'used as interface object'
						} else {
							'referenced'
						}
						c.error('`$node.name` cannot be $mischief outside `unsafe` blocks as it might be stored on stack. Consider ${suggestion}.',
							node.pos)
					}
				} else if type_sym.kind == .array_fixed {
					c.error('cannot reference fixed array `$node.name` outside `unsafe` blocks as it is supposed to be stored on stack',
						node.pos)
				} else {
					match type_sym.kind {
						.struct_ {
							info := type_sym.info as ast.Struct
							if !info.is_heap {
								node.obj.is_auto_heap = true
							}
						}
						else {
							node.obj.is_auto_heap = true
						}
					}
				}
			}
		}
		ast.SelectorExpr {
			if !node.expr_type.is_ptr() {
				c.mark_as_referenced(mut &node.expr, as_interface)
			}
		}
		ast.IndexExpr {
			c.mark_as_referenced(mut &node.left, as_interface)
		}
		else {}
	}
}

pub fn (mut c Checker) get_base_name(node &ast.Expr) string {
	match node {
		ast.Ident {
			return node.name
		}
		ast.SelectorExpr {
			return c.get_base_name(&node.expr)
		}
		ast.IndexExpr {
			return c.get_base_name(&node.left)
		}
		else {
			return ''
		}
	}
}

pub fn (mut c Checker) prefix_expr(mut node ast.PrefixExpr) ast.Type {
	old_inside_ref_lit := c.inside_ref_lit
	c.inside_ref_lit = c.inside_ref_lit || node.op == .amp
	right_type := c.expr(node.right)
	c.inside_ref_lit = old_inside_ref_lit
	node.right_type = right_type
	if node.op == .amp {
		if mut node.right is ast.PrefixExpr {
			if node.right.op == .amp {
				c.error('unexpected `&`, expecting expression', node.right.pos)
			}
		}
	}
	// TODO: testing ref/deref strategy
	if node.op == .amp && !right_type.is_ptr() {
		mut expr := node.right
		// if ParExpr get the innermost expr
		for mut expr is ast.ParExpr {
			expr = expr.expr
		}
		match expr {
			ast.BoolLiteral, ast.CallExpr, ast.CharLiteral, ast.FloatLiteral, ast.IntegerLiteral,
			ast.InfixExpr, ast.StringLiteral, ast.StringInterLiteral {
				c.error('cannot take the address of $expr', node.pos)
			}
			else {}
		}
		if mut node.right is ast.IndexExpr {
			typ_sym := c.table.get_type_symbol(node.right.left_type)
			mut is_mut := false
			if mut node.right.left is ast.Ident {
				ident := node.right.left
				// TODO: temporary, remove this
				ident_obj := ident.obj
				if ident_obj is ast.Var {
					is_mut = ident_obj.is_mut
				}
			}
			if typ_sym.kind == .map {
				c.error('cannot take the address of map values', node.right.pos)
			}
			if !c.inside_unsafe {
				if typ_sym.kind == .array && is_mut {
					c.error('cannot take the address of mutable array elements outside unsafe blocks',
						node.right.pos)
				}
			}
		}
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &node.right, false)
		}
		return right_type.ref()
	} else if node.op == .amp && node.right !is ast.CastExpr {
		if !c.inside_fn_arg && !c.inside_unsafe {
			c.mark_as_referenced(mut &node.right, false)
		}
		if node.right.is_auto_deref_var() {
			return right_type
		} else {
			return right_type.ref()
		}
	}
	if node.op == .mul {
		if right_type.is_ptr() {
			return right_type.deref()
		}
		if !right_type.is_pointer() && !c.pref.translated {
			s := c.table.type_to_str(right_type)
			c.error('invalid indirect of `$s`', node.pos)
		}
	}
	if node.op == .bit_not && !right_type.is_int() && !c.pref.translated {
		c.error('operator ~ only defined on int types', node.pos)
	}
	if node.op == .not && right_type != ast.bool_type_idx && !c.pref.translated {
		c.error('! operator can only be used with bool types', node.pos)
	}
	// FIXME
	// there are currently other issues to investigate if right_type
	// is unwraped directly as initialization, so do it here
	right_sym := c.table.get_final_type_symbol(c.unwrap_generic(right_type))
	if node.op == .minus && !right_sym.is_number() {
		c.error('- operator can only be used with numeric types', node.pos)
	}
	if node.op == .arrow {
		if right_sym.kind == .chan {
			c.stmts_ending_with_expression(node.or_block.stmts)
			return right_sym.chan_info().elem_type
		}
		c.error('<- operator can only be used with `chan` types', node.pos)
	}
	return right_type
}

fn (mut c Checker) check_index(typ_sym &ast.TypeSymbol, index ast.Expr, index_type ast.Type, pos token.Position, range_index bool) {
	index_type_sym := c.table.get_type_symbol(index_type)
	// println('index expr left=$typ_sym.name $node.pos.line_nr')
	// if typ_sym.kind == .array && (!(ast.type_idx(index_type) in ast.number_type_idxs) &&
	// index_type_sym.kind != .enum_) {
	if typ_sym.kind in [.array, .array_fixed, .string] {
		if !(index_type.is_int() || index_type_sym.kind == .enum_) {
			type_str := if typ_sym.kind == .string {
				'non-integer string index `$index_type_sym.name`'
			} else {
				'non-integer index `$index_type_sym.name` (array type `$typ_sym.name`)'
			}
			c.error('$type_str', pos)
		}
		if index is ast.IntegerLiteral {
			if index.val[0] == `-` {
				c.error('negative index `$index.val`', index.pos)
			} else if typ_sym.kind == .array_fixed {
				i := index.val.int()
				info := typ_sym.info as ast.ArrayFixed
				if (!range_index && i >= info.size) || (range_index && i > info.size) {
					c.error('index out of range (index: $i, len: $info.size)', index.pos)
				}
			}
		}
		if index_type.has_flag(.optional) {
			type_str := if typ_sym.kind == .string {
				'(type `$typ_sym.name`)'
			} else {
				'(array type `$typ_sym.name`)'
			}
			c.error('cannot use optional as index $type_str', pos)
		}
	}
}

pub fn (mut c Checker) index_expr(mut node ast.IndexExpr) ast.Type {
	mut typ := c.expr(node.left)
	mut typ_sym := c.table.get_final_type_symbol(typ)
	node.left_type = typ
	for {
		match typ_sym.kind {
			.map {
				node.is_map = true
				break
			}
			.array {
				node.is_array = true
				break
			}
			.array_fixed {
				node.is_farray = true
				break
			}
			.any {
				gname := typ_sym.name
				typ = c.unwrap_generic(typ)
				node.left_type = typ
				typ_sym = c.table.get_final_type_symbol(typ)
				if typ.is_ptr() {
					continue
				} else {
					c.error('generic type $gname does not support indexing, pass an array, or a reference instead, e.g. []$gname or &$gname',
						node.pos)
				}
			}
			else {
				break
			}
		}
	}
	if typ_sym.kind !in [.array, .array_fixed, .string, .map] && !typ.is_ptr()
		&& typ !in [ast.byteptr_type, ast.charptr_type] && !typ.has_flag(.variadic) {
		c.error('type `$typ_sym.name` does not support indexing', node.pos)
	}
	if typ_sym.kind == .string && !typ.is_ptr() && node.is_setter {
		c.error('cannot assign to s[i] since V strings are immutable\n' +
			'(note, that variables may be mutable but string values are always immutable, like in Go and Java)',
			node.pos)
	}
	if !c.inside_unsafe && ((typ.is_ptr() && !typ.has_flag(.shared_f)
		&& !node.left.is_auto_deref_var()) || typ.is_pointer()) {
		mut is_ok := false
		if mut node.left is ast.Ident {
			if node.left.obj is ast.Var {
				v := node.left.obj as ast.Var
				// `mut param []T` function parameter
				is_ok = v.is_mut && v.is_arg && !typ.deref().is_ptr()
			}
		}
		if !is_ok && !c.pref.translated {
			c.warn('pointer indexing is only allowed in `unsafe` blocks', node.pos)
		}
	}
	if mut node.index is ast.RangeExpr { // [1..2]
		if node.index.has_low {
			index_type := c.expr(node.index.low)
			c.check_index(typ_sym, node.index.low, index_type, node.pos, true)
		}
		if node.index.has_high {
			index_type := c.expr(node.index.high)
			c.check_index(typ_sym, node.index.high, index_type, node.pos, true)
		}
		// array[1..2] => array
		// fixed_array[1..2] => array
		if typ_sym.kind == .array_fixed {
			elem_type := c.table.value_type(typ)
			idx := c.table.find_or_register_array(elem_type)
			typ = ast.new_type(idx)
		} else {
			typ = typ.set_nr_muls(0)
		}
	} else { // [1]
		if typ_sym.kind == .map {
			info := typ_sym.info as ast.Map
			c.expected_type = info.key_type
			index_type := c.expr(node.index)
			if !c.check_types(index_type, info.key_type) {
				err := c.expected_msg(index_type, info.key_type)
				c.error('invalid key: $err', node.pos)
			}
			value_sym := c.table.get_type_symbol(info.value_type)
			if !node.is_setter && value_sym.kind == .sum_type && node.or_expr.kind == .absent
				&& !c.inside_unsafe && !c.inside_if_guard {
				c.warn('`or {}` block required when indexing a map with sum type value',
					node.pos)
			}
		} else {
			index_type := c.expr(node.index)
			c.check_index(typ_sym, node.index, index_type, node.pos, false)
		}
		value_type := c.table.value_type(typ)
		if value_type != ast.void_type {
			typ = value_type
		}
	}
	c.stmts_ending_with_expression(node.or_expr.stmts)
	c.check_expr_opt_call(node, typ)
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
pub fn (mut c Checker) enum_val(mut node ast.EnumVal) ast.Type {
	mut typ_idx := if node.enum_name == '' {
		c.expected_type.idx()
	} else {
		c.table.find_type_idx(node.enum_name)
	}
	if typ_idx == 0 {
		// Handle `builtin` enums like `ChanState`, so that `x := ChanState.closed` works.
		// In the checker the name for such enums was set to `main.ChanState` instead of
		// just `ChanState`.
		if node.enum_name.starts_with('${c.mod}.') {
			typ_idx = c.table.find_type_idx(node.enum_name['${c.mod}.'.len..])
			if typ_idx == 0 {
				c.error('unknown enum `$node.enum_name` (type_idx=0)', node.pos)
				return ast.void_type
			}
		}
	}
	mut typ := ast.new_type(typ_idx)
	if c.pref.translated {
		// TODO make more strict
		node.typ = typ
		return typ
	}
	if typ == ast.void_type {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	mut typ_sym := c.table.get_type_symbol(typ)
	if typ_sym.kind == .array && node.enum_name.len == 0 {
		array_info := typ_sym.info as ast.Array
		typ = array_info.elem_type
		typ_sym = c.table.get_type_symbol(typ)
	}
	if typ_sym.kind != .enum_ && !c.pref.translated {
		// TODO in C int fields can be compared to enums, need to handle that in C2V
		c.error('expected type is not an enum (`$typ_sym.name`)', node.pos)
		return ast.void_type
	}
	if typ_sym.info !is ast.Enum {
		c.error('not an enum', node.pos)
		return ast.void_type
	}
	if !(typ_sym.is_public || typ_sym.mod == c.mod) {
		c.error('enum `$typ_sym.name` is private', node.pos)
	}
	info := typ_sym.enum_info()
	if node.val !in info.vals {
		suggestion := util.new_suggestion(node.val, info.vals)
		c.error(suggestion.say('enum `$typ_sym.name` does not have a value `$node.val`'),
			node.pos)
	}
	node.typ = typ
	return typ
}

pub fn (mut c Checker) chan_init(mut node ast.ChanInit) ast.Type {
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).chan_info()
		node.elem_type = info.elem_type
		if node.has_cap {
			c.check_array_init_para_type('cap', node.cap_expr, node.pos)
		}
		return node.typ
	} else {
		c.error('`chan` of unknown type', node.pos)
		return node.typ
	}
}

pub fn (mut c Checker) offset_of(node ast.OffsetOf) ast.Type {
	sym := c.table.get_final_type_symbol(node.struct_type)
	if sym.kind != .struct_ {
		c.error('first argument of __offsetof must be struct', node.pos)
		return ast.u32_type
	}
	if !c.table.struct_has_field(sym, node.field) {
		c.error('struct `$sym.name` has no field called `$node.field`', node.pos)
	}
	return ast.u32_type
}

pub fn (mut c Checker) check_dup_keys(node &ast.MapInit, i int) {
	key_i := node.keys[i]
	if key_i is ast.StringLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.StringLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "$key_i.val" in map literal', key_i.pos)
				}
			}
		}
	} else if key_i is ast.IntegerLiteral {
		for j in 0 .. i {
			key_j := node.keys[j]
			if key_j is ast.IntegerLiteral {
				if key_i.val == key_j.val {
					c.error('duplicate key "$key_i.val" in map literal', key_i.pos)
				}
			}
		}
	}
}

pub fn (mut c Checker) map_init(mut node ast.MapInit) ast.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && node.typ == 0 {
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type
			node.key_type = info.key_type
			node.value_type = info.value_type
			return node.typ
		} else {
			if sym.kind == .struct_ {
				c.error('`{}` can not be used for initialising empty structs any more. Use `${c.table.type_to_str(c.expected_type)}{}` instead.',
					node.pos)
			} else {
				c.error('invalid empty map initialisation syntax, use e.g. map[string]int{} instead',
					node.pos)
			}
			return ast.void_type
		}
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).map_info()
		c.ensure_type_exists(info.key_type, node.pos) or {}
		c.ensure_type_exists(info.value_type, node.pos) or {}
		node.key_type = c.unwrap_generic(info.key_type)
		node.value_type = c.unwrap_generic(info.value_type)
		return node.typ
	}
	if node.keys.len > 0 && node.vals.len > 0 {
		mut key0_type := ast.void_type
		mut val0_type := ast.void_type
		use_expected_type := c.expected_type != ast.void_type && !c.inside_const
			&& c.table.get_type_symbol(c.expected_type).kind == .map
		if use_expected_type {
			sym := c.table.get_type_symbol(c.expected_type)
			info := sym.map_info()
			key0_type = c.unwrap_generic(info.key_type)
			val0_type = c.unwrap_generic(info.value_type)
		} else {
			// `{'age': 20}`
			key0_type = c.table.mktyp(c.expr(node.keys[0]))
			if node.keys[0].is_auto_deref_var() {
				key0_type = key0_type.deref()
			}
			val0_type = c.table.mktyp(c.expr(node.vals[0]))
			if node.vals[0].is_auto_deref_var() {
				val0_type = val0_type.deref()
			}
		}
		mut same_key_type := true
		for i, key in node.keys {
			if i == 0 && !use_expected_type {
				continue
			}
			val := node.vals[i]
			c.expected_type = key0_type
			key_type := c.expr(key)
			c.expected_type = val0_type
			val_type := c.expr(val)
			if !c.check_types(key_type, key0_type) || (i == 0 && key_type.is_number()
				&& key0_type.is_number() && key0_type != c.table.mktyp(key_type)) {
				msg := c.expected_msg(key_type, key0_type)
				c.error('invalid map key: $msg', key.position())
				same_key_type = false
			}
			if !c.check_types(val_type, val0_type) || (i == 0 && val_type.is_number()
				&& val0_type.is_number() && val0_type != c.table.mktyp(val_type)) {
				msg := c.expected_msg(val_type, val0_type)
				c.error('invalid map value: $msg', val.position())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
		key0_type = c.unwrap_generic(key0_type)
		val0_type = c.unwrap_generic(val0_type)
		mut map_type := ast.new_type(c.table.find_or_register_map(key0_type, val0_type))
		node.typ = map_type
		node.key_type = key0_type
		node.value_type = val0_type
		return map_type
	}
	return node.typ
}

// call this *before* calling error or warn
pub fn (mut c Checker) add_error_detail(s string) {
	c.error_details << s
}

pub fn (mut c Checker) warn(s string, pos token.Position) {
	allow_warnings := !(c.pref.is_prod || c.pref.warns_are_errors) // allow warnings only in dev builds
	c.warn_or_error(s, pos, allow_warnings)
}

pub fn (mut c Checker) error(message string, pos token.Position) {
	$if checker_exit_on_first_error ? {
		eprintln('\n\n>> checker error: $message, pos: $pos')
		print_backtrace()
		exit(1)
	}
	if c.pref.translated && message.starts_with('mismatched types') {
		// TODO move this
		return
	}
	if c.pref.is_verbose {
		print_backtrace()
	}
	msg := message.replace('`Array_', '`[]')
	c.warn_or_error(msg, pos, false)
}

// check `to` has all fields of `from`
fn (c &Checker) check_struct_signature(from ast.Struct, to ast.Struct) bool {
	// Note: `to` can have extra fields
	if from.fields.len == 0 {
		return false
	}
	for field in from.fields {
		filtered := to.fields.filter(it.name == field.name)
		if filtered.len != 1 {
			// field doesn't exist
			return false
		}
		counterpart := filtered[0]
		if field.typ != counterpart.typ {
			// field has different tye
			return false
		}
		if field.is_pub != counterpart.is_pub {
			// field is not public while the other one is
			return false
		}
		if field.is_mut != counterpart.is_mut {
			// field is not mutable while the other one is
			return false
		}
	}
	return true
}

pub fn (mut c Checker) note(message string, pos token.Position) {
	if c.pref.message_limit >= 0 && c.nr_notices >= c.pref.message_limit {
		c.should_abort = true
		return
	}
	if c.is_generated {
		return
	}
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	wrn := errors.Notice{
		reporter: errors.Reporter.checker
		pos: pos
		file_path: c.file.path
		message: message
		details: details
	}
	c.file.notices << wrn
	c.notices << wrn
	c.nr_notices++
}

fn (mut c Checker) warn_or_error(message string, pos token.Position, warn bool) {
	// add backtrace to issue struct, how?
	// if c.pref.is_verbose {
	// print_backtrace()
	// }
	mut details := ''
	if c.error_details.len > 0 {
		details = c.error_details.join('\n')
		c.error_details = []
	}
	if warn && !c.pref.skip_warnings {
		c.nr_warnings++
		if c.pref.message_limit >= 0 && c.nr_warnings >= c.pref.message_limit {
			c.should_abort = true
			return
		}
		wrn := errors.Warning{
			reporter: errors.Reporter.checker
			pos: pos
			file_path: c.file.path
			message: message
			details: details
		}
		c.file.warnings << wrn
		c.warnings << wrn
		return
	}
	if !warn {
		if c.pref.fatal_errors {
			exit(1)
		}
		c.nr_errors++
		if c.pref.message_limit >= 0 && c.errors.len >= c.pref.message_limit {
			c.should_abort = true
			return
		}
		if pos.line_nr !in c.error_lines {
			err := errors.Error{
				reporter: errors.Reporter.checker
				pos: pos
				file_path: c.file.path
				message: message
				details: details
			}
			c.file.errors << err
			c.errors << err
			c.error_lines << pos.line_nr
		}
	}
}

// for debugging only
fn (c &Checker) fileis(s string) bool {
	return c.file.path.contains(s)
}

fn (mut c Checker) fetch_field_name(field ast.StructField) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.kind == .string && attr.name == 'sql' && attr.arg != '' {
			name = attr.arg
			break
		}
	}
	sym := c.table.get_type_symbol(field.typ)
	if sym.kind == .struct_ && sym.name != 'time.Time' {
		name = '${name}_id'
	}
	return name
}

fn (mut c Checker) post_process_generic_fns() {
	// Loop thru each generic function concrete type.
	// Check each specific fn instantiation.
	for i in 0 .. c.file.generic_fns.len {
		mut node := c.file.generic_fns[i]
		c.mod = node.mod
		for concrete_types in c.table.fn_generic_types[node.name] {
			c.table.cur_concrete_types = concrete_types
			c.fn_decl(mut node)
			if node.name == 'vweb.run' {
				for ct in concrete_types {
					if ct !in c.vweb_gen_types {
						c.vweb_gen_types << ct
					}
				}
			}
		}
		c.table.cur_concrete_types = []
	}
}

fn (mut c Checker) trace(fbase string, message string) {
	if c.file.path_base == fbase {
		println('> c.trace | ${fbase:-10s} | $message')
	}
}

fn (mut c Checker) ensure_type_exists(typ ast.Type, pos token.Position) ? {
	if typ == 0 {
		c.error('unknown type', pos)
		return
	}
	sym := c.table.get_type_symbol(typ)
	match sym.kind {
		.placeholder {
			if sym.language == .v && !sym.name.starts_with('C.') {
				c.error(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `$sym.name`'),
					pos)
				return
			}
		}
		.int_literal, .float_literal {
			// Separate error condition for `int_literal` and `float_literal` because `util.suggestion` may give different
			// suggestions due to f32 comparision issue.
			if !c.is_builtin_mod {
				msg := if sym.kind == .int_literal {
					'unknown type `$sym.name`.\nDid you mean `int`?'
				} else {
					'unknown type `$sym.name`.\nDid you mean `f64`?'
				}
				c.error(msg, pos)
				return
			}
		}
		.array {
			c.ensure_type_exists((sym.info as ast.Array).elem_type, pos) ?
		}
		.array_fixed {
			c.ensure_type_exists((sym.info as ast.ArrayFixed).elem_type, pos) ?
		}
		.map {
			info := sym.info as ast.Map
			c.ensure_type_exists(info.key_type, pos) ?
			c.ensure_type_exists(info.value_type, pos) ?
		}
		else {}
	}
}
