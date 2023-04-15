module wasm

import encoding.leb128
import math.bits

fn (mut mod Module) u32(v u32) {
	mod.buf << leb128.encode_u32(v)
}

fn (mut mod Module) patch_start() int {
	return mod.buf.len
}

fn (mut mod Module) patch_len(pos int) {
	len := mod.buf.len - pos
	data := leb128.encode_u32(u32(len))
	mod.buf.insert(pos, data)
}

fn (mut mod Module) patch_u32(pos int, val u32) {
	data := leb128.encode_u32(val)
	mod.buf.insert(pos, data)
}

fn (mut mod Module) result_type(results []ValType) {
	mod.u32(u32(results.len))
	for r in results {
		mod.buf << u8(r)
	}
}

fn (mut mod Module) function_type(ft FuncType) {
	mod.buf << 0x60 // function type indicator
	mod.result_type(ft.parameters)
	mod.result_type(ft.results)
}

fn (mut mod Module) global_type(vt ValType, is_mut bool) {
	mod.buf << u8(vt)
	mod.buf << u8(is_mut)
}

fn push_f32(mut buf []u8, v f32) {
	rv := bits.f32_bits(v)
	buf << u8(rv >> u32(0))
	buf << u8(rv >> u32(8))
	buf << u8(rv >> u32(16))
	buf << u8(rv >> u32(24))
}

fn push_f64(mut buf []u8, v f64) {
	rv := bits.f64_bits(v)
	buf << u8(rv >> u32(0))
	buf << u8(rv >> u32(8))
	buf << u8(rv >> u32(16))
	buf << u8(rv >> u32(24))
	buf << u8(rv >> u32(32))
	buf << u8(rv >> u32(40))
	buf << u8(rv >> u32(48))
	buf << u8(rv >> u32(56))
}

fn (mod &Module) get_function_idx(patch CallPatch) int {
	mut idx := -1

	match patch {
		FunctionCallPatch {
			ftt := mod.functions[patch.name] or {
				panic('called function ${patch.name} does not exist')
			}
			idx = ftt.idx + mod.fn_imports.len
		}
		ImportCallPatch {
			for fnidx, c in mod.fn_imports {
				if c.mod == patch.mod && c.name == patch.name {
					idx = fnidx
					break
				}
			}
			if idx == -1 {
				panic('called imported function ${patch.mod}.${patch.name} does not exist')
			}
		}
	}

	return idx
}

fn (mut mod Module) patch(ft Function) {
	mut ptr := 0

	for patch in ft.call_patches {
		idx := mod.get_function_idx(patch)

		mod.buf << ft.code[ptr..patch.pos]
		mod.u32(u32(idx))
		ptr = patch.pos
	}

	for patch in ft.global_patches {
		idx := mod.global_imports.len + patch.idx
		mod.buf << ft.code[ptr..patch.pos]
		mod.u32(u32(idx))
		ptr = patch.pos
	}

	mod.buf << ft.code[ptr..]
}

// compile serialises the WebAssembly module into a byte array.
// The returned byte array can be written out into a `.wasm`, or executed in memory.
pub fn (mut mod Module) compile() []u8 {
	mod.buf = []u8{cap: 128}

	// WASM_BINARY_MAGIC, WASM_BINARY_VERSION
	mod.buf << [u8(0x00), 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]

	// https://webassembly.github.io/spec/core/binary/modules.html#type-section
	//
	if mod.functypes.len > 0 {
		// Types
		mod.buf << u8(Section.type_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.functypes.len))
			for ft in mod.functypes {
				mod.function_type(ft)
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#import-section
	//
	if mod.fn_imports.len > 0 || mod.global_imports.len > 0 {
		mod.buf << u8(Section.import_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.fn_imports.len + mod.global_imports.len))
			for ft in mod.fn_imports {
				mod.u32(u32(ft.mod.len))
				mod.buf << ft.mod.bytes()
				mod.u32(u32(ft.name.len))
				mod.buf << ft.name.bytes()
				mod.buf << 0x00 // function
				mod.u32(u32(ft.tidx))
			}
			for gt in mod.global_imports {
				mod.u32(u32(gt.mod.len))
				mod.buf << gt.mod.bytes()
				mod.u32(u32(gt.name.len))
				mod.buf << gt.name.bytes()
				mod.buf << 0x03 // global
				mod.global_type(gt.typ, gt.is_mut)
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec
	//
	if mod.functions.len > 0 {
		mod.buf << u8(Section.function_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.functions.len))
			for _, ft in mod.functions {
				mod.u32(u32(ft.tidx))
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec
	//
	if memory := mod.memory {
		mod.buf << u8(Section.memory_section)
		tpatch := mod.patch_start()
		{
			mod.u32(1)
			if max := memory.max {
				mod.buf << 0x01 // limit, max present
				mod.u32(memory.min)
				mod.u32(max)
			} else {
				mod.buf << 0x00 // limit, max not present
				mod.u32(memory.min)
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#global-section
	//
	if mod.globals.len > 0 {
		mod.buf << u8(Section.global_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.globals.len))
			for gt in mod.globals {
				mod.global_type(gt.typ, gt.is_mut)

				{
					mut ptr := 0
					for patch in gt.init.call_patches {
						idx := mod.get_function_idx(patch)

						mod.buf << gt.init.code[ptr..patch.pos]
						mod.u32(u32(idx))
						ptr = patch.pos
					}
					mod.buf << gt.init.code[ptr..]
				}
				mod.buf << 0x0B // END expression opcode
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#export-section
	//
	if mod.functions.len > 0 {
		mod.buf << u8(Section.export_section)
		tpatch := mod.patch_start()
		{
			lpatch := mod.patch_start()
			mut lsz := 0
			for _, ft in mod.functions {
				if !ft.export {
					continue
				}
				lsz++
				mod.u32(u32(ft.name.len))
				mod.buf << ft.name.bytes()
				mod.buf << 0x00 // function
				mod.u32(u32(ft.idx + mod.fn_imports.len))
			}
			if memory := mod.memory {
				if memory.export {
					lsz++
					mod.u32(u32(memory.name.len))
					mod.buf << memory.name.bytes()
					mod.buf << 0x02 // function
					mod.u32(0)
				}
			}
			mod.patch_u32(lpatch, u32(lsz))
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-startsec
	//
	if start := mod.start {
		ftt := mod.functions[start] or { panic('start function ${start} does not exist') }
		mod.buf << u8(Section.start_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(ftt.idx + mod.fn_imports.len))
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#data-count-section
	//
	if mod.segments.len > 0 {
		mod.buf << u8(Section.data_count_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.segments.len))
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec
	//
	if mod.functions.len > 0 {
		mod.buf << u8(Section.code_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.functions.len))
			for _, ft in mod.functions {
				fpatch := mod.patch_start()
				{
					mod.u32(u32(ft.locals.len))
					for lt in ft.locals {
						mod.u32(1)
						mod.buf << u8(lt)
					}
					mod.patch(ft)
					mod.buf << 0x0B // END expression opcode
				}
				mod.patch_len(fpatch)
			}
		}
		mod.patch_len(tpatch)
	}
	// https://webassembly.github.io/spec/core/binary/modules.html#data-section
	//
	if mod.segments.len > 0 {
		mod.buf << u8(Section.data_section)
		tpatch := mod.patch_start()
		{
			mod.u32(u32(mod.segments.len))
			for _, seg in mod.segments {
				if idx := seg.idx {
					mod.buf << 0x00 // active
					// constant expr
					mod.buf << 0x41 // i32.const
					mod.buf << leb128.encode_i32(idx)
					mod.buf << 0x0B // END expression opcode
				} else {
					mod.buf << 0x01 // passive
				}
				mod.u32(u32(seg.data.len))
				mod.buf << seg.data
			}
		}
		mod.patch_len(tpatch)
	}

	return mod.buf
}
