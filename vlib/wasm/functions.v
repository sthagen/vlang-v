module wasm

struct ImportCallPatch {
	mod  string
	name string
	pos  int
}

struct FunctionCallPatch {
	name string
	pos  int
}

type CallPatch = FunctionCallPatch | ImportCallPatch

struct FunctionGlobalPatch {
	idx GlobalIndex
	pos int
}

pub struct Function {
	tidx int
	idx  int
mut:
	call_patches   []CallPatch
	global_patches []FunctionGlobalPatch
	label          int
	export         bool
	mod            &Module = unsafe { nil }
	code           []u8
	locals         []ValType
pub:
	name string
}
