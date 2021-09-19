// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import strings

fn (mut g Gen) gen_free_method_for_type(typ ast.Type) string {
	styp := g.typ(typ).replace('*', '')
	mut sym := g.table.get_type_symbol(g.unwrap_generic(typ))
	mut fn_name := styp_to_free_fn_name(styp)
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.get_type_symbol(sym.info.parent_type)
		}
	}

	if sym.has_method('free') {
		return fn_name
	}
	match mut sym.info {
		ast.Struct {
			g.gen_free_for_struct(sym.info, styp, fn_name)
		}
		ast.Array {
			g.gen_free_for_array(sym.info, styp, fn_name)
		}
		ast.Map {
			g.gen_free_for_map(sym.info, styp, fn_name)
		}
		else {
			println(g.table.type_str(typ))
			verror("could not generate free method '$fn_name' for type '$styp'")
		}
	}
	return fn_name
}

fn (mut g Gen) gen_free_for_struct(info ast.Struct, styp string, fn_name string) {
	g.type_definitions.writeln('void ${fn_name}($styp* it); // auto')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('void ${fn_name}($styp* it) {')
	for field in info.fields {
		sym := g.table.get_type_symbol(g.unwrap_generic(field.typ))

		if sym.kind !in [.string, .array, .map, .struct_] {
			continue
		}
		mut field_styp := g.typ(field.typ).replace('*', '')
		field_styp_fn_name := if sym.has_method('free') {
			'${field_styp}_free'
		} else {
			g.gen_free_method_for_type(field.typ)
		}
		fn_builder.writeln('\t${field_styp_fn_name}(&(it->$field.name));')
	}
	fn_builder.writeln('}')
}

fn (mut g Gen) gen_free_for_array(info ast.Array, styp string, fn_name string) {
	g.type_definitions.writeln('void ${fn_name}($styp* it); // auto')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('void ${fn_name}($styp* it) {')

	sym := g.table.get_type_symbol(g.unwrap_generic(info.elem_type))
	if sym.kind in [.string, .array, .map, .struct_] {
		fn_builder.writeln('\tfor (int i = 0; i < it->len; i++) {')

		mut elem_styp := g.typ(info.elem_type).replace('*', '')
		elem_styp_fn_name := if sym.has_method('free') {
			'${elem_styp}_free'
		} else {
			g.gen_free_method_for_type(info.elem_type)
		}
		fn_builder.writeln('\t\t${elem_styp_fn_name}(&((($elem_styp*)it->data)[i]));')
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\tarray_free(it);')
	fn_builder.writeln('}')
}

fn (mut g Gen) gen_free_for_map(info ast.Map, styp string, fn_name string) {
	g.type_definitions.writeln('void ${fn_name}($styp* it); // auto')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('void ${fn_name}($styp* it) {')

	fn_builder.writeln('\tmap_free(it);')
	fn_builder.writeln('}')
}

[inline]
fn styp_to_free_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_free'
}
