// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (a any) toString()

pub fn unwrap(opt any) any {
	o := &Option(opt)
	if o.state != 0 {
		js_throw(o.err)
	}
	return opt
}

pub fn panic(s string) {
	eprintln('V panic: $s')
	exit(1)
}

struct Option {
	state byte
	err   Error
}

// IError holds information about an error instance
pub interface IError {
	msg string
	code int
}

// Error is the default implementation of IError, that is returned by e.g. `error()`
pub struct Error {
pub:
	msg  string
	code int
}

const none__ = IError(&None__{})

struct None__ {
	msg  string
	code int
}

fn (_ None__) str() string {
	return 'none'
}

pub fn (err IError) str() string {
	return match err {
		None__ { 'none' }
		Error { err.msg }
		else { '$err.type_name(): $err.msg' }
	}
}

pub fn (o Option) str() string {
	if o.state == 0 {
		return 'Option{ ok }'
	}
	if o.state == 1 {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.err" }'
}

[if trace_error ?]
fn trace_error(x string) {
	eprintln('> ${@FN} | $x')
}

// error returns a default error instance containing the error given in `message`.
// Example: `if ouch { return error('an error occurred') }`
[inline]
pub fn error(message string) IError {
	trace_error(message)
	return &Error{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// `if ouch { return error_with_code('an error occurred', 1) }`
[inline]
pub fn error_with_code(message string, code int) IError {
	// trace_error('$message | code: $code')
	return &Error{
		msg: message
		code: code
	}
}
