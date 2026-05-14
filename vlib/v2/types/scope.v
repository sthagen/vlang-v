// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub type Object = Const | Fn | Global | Module | SmartCastSelector | Type

// pub type Object = Const | Fn | Global | Module |
// 	Alias | Array | Enum | Map | Pointer | Primitive | String | Struct | SumType

struct SmartCastSelector {
	origin    Type
	field     string
	cast_type Type
}

@[heap]
pub struct Scope {
pub:
	parent &Scope = unsafe { nil }
pub mut:
	objects       map[string]Object
	object_names  []string
	object_values []Object
	// TODO: try implement using original concept
	field_smartcasts       map[string]Type
	field_smartcast_names  []string
	field_smartcast_values []Type
	// smartcasts map[string]Type
	// TODO: it may be more efficient looking up local vars using an ID
	// even if we had to store them in two different places. investigate.
	// variables []Object
	start int
	end   int
}

pub fn new_scope(parent &Scope) &Scope {
	unsafe {
		return &Scope{
			parent: parent
		}
	}
}

// same_scope_ptr compares scope identity by address instead of structural equality.
pub fn same_scope_ptr(a &Scope, b &Scope) bool {
	return voidptr(a) == voidptr(b)
}

// TODO: try implement the alternate method I was experimenting with (SmartCastSelector)
// i'm not sure if it is actually possible though. need to explore it.
pub fn (s &Scope) lookup_field_smartcast(name string) ?Type {
	for i, smartcast_name in s.field_smartcast_names {
		if string_bytes_eq(smartcast_name, name) {
			return s.field_smartcast_values[i]
		}
	}
	if s.parent != unsafe { nil } {
		return s.parent.lookup_field_smartcast(name)
	}
	return none
}

pub fn (s &Scope) lookup(name string) ?Object {
	for i, obj_name in s.object_names {
		if string_bytes_eq(obj_name, name) {
			return s.object_values[i]
		}
	}
	return none
}

pub fn (s &Scope) lookup_parent(name string, pos int) ?Object {
	if obj := s.lookup(name) {
		return obj
		// if !pos.is_valid() || cmpPos(obj.scopePos(), pos) <= 0 {
		// 	return s, obj
		// }
	}
	if s.parent != unsafe { nil } {
		if parent_obj := s.parent.lookup_parent(name, pos) {
			return parent_obj
		}
	}
	// println('lookup_parent: NOT FOUND: ${name}')
	return none
}

// lookup_var_type looks up a variable by name and returns its type.
// Walks up the scope chain to find the variable.
pub fn (s &Scope) lookup_var_type(name string) ?Type {
	if obj := s.lookup_parent(name, 0) {
		return obj.typ()
	}
	return none
}

pub fn (s &Scope) lookup_parent_with_scope(name string, pos int) ?(&Scope, Object) {
	if obj := s.lookup(name) {
		return s, obj
		// if !pos.is_valid() || cmpPos(obj.scopePos(), pos) <= 0 {
		// 	return s, obj
		// }
	}
	if s.parent != unsafe { nil } {
		if parent_scope, parent_obj := s.parent.lookup_parent_with_scope(name, pos) {
			return parent_scope, parent_obj
		}
	}
	// println('lookup_parent: NOT FOUND: ${name}')
	return none
}

pub fn (mut s Scope) insert(name string, obj Object) {
	if existing := s.lookup(name) {
		// Module scopes pre-register a self-module placeholder so code can
		// reference `mod_name.CONST` from inside the same module. A real symbol
		// with the same name should override that placeholder.
		if existing is Module && obj !is Module {
			s.set_object(name, obj)
		}
		return
	}
	s.set_object(name, obj)
}

// insert_or_update always overwrites an existing entry. Used for fn_root_scope
// where variables from nested scopes must be updated when re-declared.
pub fn (mut s Scope) insert_or_update(name string, obj Object) {
	s.set_object(name, obj)
}

fn (mut s Scope) set_object(name string, obj Object) {
	for i, obj_name in s.object_names {
		if string_bytes_eq(obj_name, name) {
			s.object_values[i] = obj
			s.objects[name] = obj
			return
		}
	}
	s.object_names << name
	s.object_values << obj
	s.objects[name] = obj
}

pub fn (mut s Scope) set_field_smartcast(name string, typ Type) {
	for i, smartcast_name in s.field_smartcast_names {
		if string_bytes_eq(smartcast_name, name) {
			s.field_smartcast_values[i] = typ
			s.field_smartcasts[name] = typ
			return
		}
	}
	s.field_smartcast_names << name
	s.field_smartcast_values << typ
	s.field_smartcasts[name] = typ
}

pub fn (s &Scope) print(recurse_parents bool) {
	println('# SCOPE:')
	for name, obj in s.objects {
		println(' * ${name}: ${obj.type_name()}')
		// if obj is Type {
		// 	println('    - ${name}: ${obj.type_name()}')
		// }
	}
	if recurse_parents && s.parent != unsafe { nil } {
		s.parent.print(recurse_parents)
	}
}

pub fn (obj &Object) typ() Type {
	match obj {
		Const {
			return obj.typ
		}
		Fn {
			return obj.typ
		}
		Global {
			return obj.typ
		}
		Module {
			// TODO: modules don't have a type, return a placeholder
			return Type(u16_)
		}
		SmartCastSelector {
			return obj.origin
		}
		Type {
			return obj
		}
	}
}
