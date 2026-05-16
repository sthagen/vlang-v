// vtest build: !linux && !windows
module cleanc

import os
import v2.markused
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_result_option_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_result_option_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn generate_markused_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_markused_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	gen_files := trans.transform_files(files)
	used := markused.mark_used(gen_files, env)
	mut gen := Gen.new_with_env_and_pref(gen_files, env, prefs)
	gen.set_used_fn_keys(used)
	return gen.gen()
}

fn test_generate_c_keeps_option_wrapper_for_fn_value_if_guard() {
	csrc := generate_result_option_c_for_test("
fn with_name_to_index(name_to_index fn (string) ?int) {
	if index := name_to_index('foo') {
		_ = index
	}
}
")
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('if ((_or_t')
	assert !csrc.contains('void* _or_t')
}

fn test_generate_c_keeps_option_wrapper_for_or_block_temp() {
	csrc := generate_result_option_c_for_test('
fn maybe_index() ?int {
	return 3
}

fn find_stop() int {
	stop_index := maybe_index() or { -1 }
	return stop_index
}
')
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('stop_index')
}

fn test_generate_c_passes_mut_arg_by_address_to_fn_pointer_param() {
	csrc := generate_result_option_c_for_test('
fn render(replacement fn (string, mut []string)) int {
	mut out := []string{}
	replacement("x", mut out)
	return 0
}
')
	assert csrc.contains('replacement((string){.str = "x"')
	assert csrc.contains(', &out);')
	assert !csrc.contains(', out);')
}

fn test_generate_c_resolves_fn_literal_param_type_for_string_interpolation() {
	csrc := generate_result_option_c_for_test('
fn render(replacement fn (string, mut []string)) {
	mut out := []string{}
	replacement("x", mut out)
}

fn demo() {
	render(fn (name string, mut out []string) {
		out << "<\${name}>"
	})
}
')
	assert csrc.contains('"<%s>"')
	assert !csrc.contains('"<%d>"')
}

fn test_generate_c_expands_builtin_option_clone_if_guard() {
	csrc := generate_result_option_c_for_test('
interface IClone {}

struct Bag implements IClone {
	name ?string
}

fn copy_bag(b Bag) Bag {
	return b.clone()
}
')
	assert csrc.contains('builtin__Option_string__clone')
	assert csrc.contains('.state == 0')
	assert !csrc.contains('if (s)')
	assert !csrc.contains('string _val = builtin__Option_string__clone')
}

fn test_generate_c_wraps_struct_field_option_value() {
	csrc := generate_result_option_c_for_test('
struct Ref {
	value int
}

struct Holder {
	item ?&Ref
}

fn make(r &Ref) Holder {
	return Holder{
		item: r
	}
}
')
	assert csrc.contains('_option_Refptr item;')
	assert csrc.contains('_option_Refptr _opt = (_option_Refptr){ .state = 2 }; Ref* _val = r; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt;')
	assert !csrc.contains('.item = r')
}

fn test_generate_c_interface_cast_strips_pointer_for_method_symbol() {
	csrc := generate_result_option_c_for_test('
interface Handle {
	len() int
}

struct File {}

fn (mut f File) len() int {
	_ = f
	return 1
}

fn consume(h &Handle) int {
	return h.len()
}

fn demo(mut file File) int {
	return consume(&file)
}
')
	assert csrc.contains('.len = (int (*)(void*))File__len')
	assert !csrc.contains('File*__len')
}

fn test_generate_c_initializes_omitted_option_struct_fields_to_none() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	item ?int
	name string
}

fn make_named() Holder {
	return Holder{
		name: "x"
	}
}

fn make_empty() Holder {
	return Holder{}
}
')
	assert csrc.contains('.item = ((_option_int){.state = 2})')
	assert !csrc.contains('return ((Holder){0})')
}

fn test_generate_c_declares_specialized_generic_option_return() {
	csrc := generate_result_option_c_for_test('
module sample

struct Item {}

struct Match[T] {
	value T
	has_value bool
}

fn (m Match[T]) inner() ?T {
	if !m.has_value {
		return none
	}
	return m.value
}

fn use(m Match[Item]) bool {
	if value := m.inner() {
		_ = value
		return true
	}
	return false
}
')
	assert csrc.contains('typedef struct _option_sample__Item _option_sample__Item;')
	assert csrc.contains('struct _option_sample__Item')
	assert csrc.contains('_option_sample__Item sample__Match__inner')
}

fn test_generate_c_emits_struct_str_function_when_interpolated() {
	csrc := generate_result_option_c_for_test('
struct Thing {
	x int
}

fn msg(t Thing) string {
	return "\${t}"
}
')
	assert csrc.contains('string Thing__str(Thing s)')
	assert csrc.contains('Thing__str(t).str')
}

fn test_generate_c_does_not_emit_unused_error_method_with_unwalked_str_dependency() {
	csrc := generate_markused_c_for_test('
struct Term {
	x int
}

struct MyError {
	term Term
}

fn (err MyError) msg() string {
	return "\${err.term}"
}

fn (err MyError) code() int {
	_ = err
	return 0
}

fn unused() ! {
	return MyError{}
}

fn test_unused_error() {
	assert true
}
')
	assert !csrc.contains('string MyError__msg(MyError err)')
	assert !csrc.contains('Term__str((err).term)')
}

fn test_generate_c_does_not_force_emit_unused_interface_candidate_method_body() {
	csrc := generate_markused_c_for_test('
interface Describer {
	msg() string
}

struct Inner {
	text string
}

fn (inner Inner) str() string {
	return inner.text
}

struct ErrorLike {
	inner Inner
}

fn (err ErrorLike) msg() string {
	return err.inner.str()
}

fn main() {
	_ = 1
}
')
	assert !csrc.contains('string ErrorLike__msg(ErrorLike err)')
	assert !csrc.contains('Inner__str((err).inner)')
}

fn test_generate_c_emits_used_struct_operator_method_body_with_markused() {
	csrc := generate_markused_c_for_test('
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
	sum := left + right
	assert sum.n == 3
}
')
	assert csrc.contains('Stats Stats__plus(Stats left, Stats right)')
	assert csrc.count('Stats__plus(') >= 2
}

fn test_generate_c_borrows_option_field_unwrap_payload_without_temp() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	value ?string
}

fn (h &Holder) value_ref() ?&string {
	if h.value != none {
		return unsafe { &h.value? }
	}
	return none
}
')
	assert !csrc.contains('.is_error')
	assert !csrc.contains('_or_t')
	assert csrc.contains('(h)->value.state != 0')
	assert csrc.contains('&(*(string*)(((u8*)(&(h)->value.err)) + sizeof(IError)))')
}

fn test_generate_c_returns_custom_error_from_result_function_as_error() {
	csrc := generate_result_option_c_for_test('
struct SizeError {}

fn (err SizeError) msg() string {
	return "bad size"
}

fn (err SizeError) code() int {
	return 7
}

fn make_error() SizeError {
	return SizeError{}
}

fn parse() !u64 {
	return make_error()
}
')
	assert csrc.contains('return (_result_u64){ .is_error=true, .err=')
	assert csrc.contains('IError_SizeError_msg_wrapper')
	assert csrc.contains('SizeError* _ierr_obj')
	assert !csrc.contains('u64 _val = main__make_error()')
}

fn test_generate_c_keeps_option_if_guard_err_as_concrete_error_ref() {
	csrc := generate_result_option_c_for_test('
struct MyError {}

fn (err MyError) msg() string {
	return "bad"
}

fn maybe_error() ?&MyError {
	return &MyError{}
}

fn read() string {
	if err := maybe_error() {
		return err.msg()
	}
	return ""
}
')
	assert csrc.contains('MyError__msg((*err))')
	assert !csrc.contains('err->_object')
	assert !csrc.contains('MyError__msg((*err),')
}

fn test_generate_c_casts_concrete_arg_for_mut_interface_param() {
	csrc := generate_result_option_c_for_test('
interface Reader {
mut:
	read(mut []u8) !int
}

struct ByteReader {}

fn (mut rdr ByteReader) read(mut buf []u8) !int {
	return 0
}

fn consume(mut rdr Reader) !int {
	mut buf := []u8{len: 4}
	return rdr.read(mut buf)
}

fn demo() !int {
	mut rdr := ByteReader{}
	return consume(mut rdr)
}
')
	assert csrc.contains('Reader* _iface_t')
	assert csrc.contains('ByteReader__read')
	assert !csrc.contains('consume(&rdr)')
}

fn test_generate_c_preserves_c_pointer_cast_selector_field_access() {
	csrc := generate_result_option_c_for_test('
@[typedef]
struct C.log__Logger {
mut:
	_object voidptr
}

interface Logger {
mut:
	free()
}

fn raw_object(logger &Logger) voidptr {
	unsafe {
		pobject := &C.log__Logger(logger)._object
		return pobject
	}
}
')
	assert csrc.contains('pobject = ((log__Logger*)(logger))->_object;')
	assert !csrc.contains('voidptr* pobject')
	assert !csrc.contains('&logger->_object')
}

fn test_generate_c_preserves_embedded_error_concrete_type_name() {
	csrc := generate_result_option_c_for_test('
struct Error {}

fn (err Error) msg() string {
	return ""
}

fn (err Error) code() int {
	return 0
}

struct Eof {
	Error
}

fn read() !int {
	return Eof{}
}

fn demo() bool {
	_ := read() or {
		return err is Eof
	}
	return false
}
')
	assert csrc.contains('IError_Eof_type_name_wrapper')
	assert csrc.contains('IError_Eof_msg_wrapper')
	assert !csrc.contains('.type_name = IError_Error_type_name_wrapper')
}

fn test_generate_c_does_not_emit_nested_generic_structs_with_placeholder_args() {
	csrc := generate_result_option_c_for_test('
struct NoColor[W] {
mut:
	wtr W
}

struct CounterWriter[W] {
mut:
	wtr W
}

struct Summary[W] {
mut:
	wtr CounterWriter[W]
}

fn build_no_color[W](wtr W) Summary[NoColor[W]] {
	_ = wtr
	return Summary[NoColor[W]]{}
}
')
	assert !csrc.contains('struct CounterWriter {\n\tNoColor wtr;')
	assert !csrc.contains('struct Summary {\n\tCounterWriter wtr;')
}

fn test_generate_c_orders_nested_generic_struct_dependencies() {
	csrc := generate_result_option_c_for_test('
struct PlainWriter {}

struct NoColor[W] {
mut:
	wtr W
}

struct CounterWriter[W] {
mut:
	wtr W
}

struct Direct[W] {
mut:
	wtr CounterWriter[W]
}

struct JSON[W] {
mut:
	wtr CounterWriter[NoColor[W]]
}

fn build_direct[W](wtr W) Direct[W] {
	_ = wtr
	return Direct[W]{}
}

fn build[W](wtr W) JSON[W] {
	_ = wtr
	return JSON[W]{}
}

fn demo() {
	_ := build_direct(PlainWriter{})
	_ := build(PlainWriter{})
}
')
	dep_pos := csrc.index('struct CounterWriter_T_NoColor {') or {
		panic('missing concrete nested generic struct body')
	}
	user_pos := csrc.index('struct JSON {') or { panic('missing generic user struct body') }
	assert dep_pos < user_pos
}

fn test_generate_c_skips_interface_clone_for_incomplete_generic_implementor() {
	csrc := generate_result_option_c_for_test('
interface Writer {
mut:
	write() !int
}

struct Wrapper[W] {
mut:
	wtr W
}

fn (mut w Wrapper[W]) write() !int {
	_ = w
	return 0
}

fn wrap[W](wtr W) Wrapper[W] {
	return Wrapper[W]{
		wtr: wtr
	}
}
')
	assert !csrc.contains('sizeof(Wrapper)')
	assert !csrc.contains('Wrapper__write(Wrapper* w) {')
}
