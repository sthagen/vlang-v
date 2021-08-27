module builtin

// used to generate JS throw statements.
pub fn js_throw(s any) {
	#throw (s instanceof Error ? s : new Error(s))
}

pub fn println(s any) {
	$if js_freestanding {
		#print(s.toString())
	} $else {
		#console.log(s.toString())
	}
}

pub fn print(s any) {
	$if js_node {
		#$process.stdout.write(s.toString())
	} $else {
		panic('Cannot `print` in a browser, use `println` instead')
	}
}

pub fn eprintln(s any) {
	$if js_freestanding {
		#print(s.toString())
	} $else {
		#console.error(s.toString())
	}
}

pub fn eprint(s any) {
	$if js_node {
		#$process.stderr.write(s.toString())
	} $else {
		panic('Cannot `eprint` in a browser, use `println` instead')
	}
}

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
pub fn exit(c int) {
	JS.process.exit(c)
	js_throw('exit($c)')
}
