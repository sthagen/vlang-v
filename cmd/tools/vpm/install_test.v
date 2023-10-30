import os
import v.vmod

// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
const test_path = os.join_path(os.vtmp_dir(), 'test-vmodules')

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_git_url() {
	res := os.execute(@VEXE + ' install https://github.com/vlang/markdown')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
	assert res.output.contains('Installing module "markdown" from "https://github.com/vlang/markdown')
	assert res.output.contains('Relocating module from "vlang/markdown" to "markdown"')
}

fn test_install_from_vpm_ident() {
	res := os.execute(@VEXE + ' install nedpals.args')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'nedpals', 'args', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'nedpals.args'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	res := os.execute(@VEXE + ' install pcre')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
}

fn test_install_already_existant() {
	// Skip on windows for now due permission errors with rmdir.
	$if windows {
		return
	}
	mod_url := 'https://github.com/vlang/markdown'
	mut res := os.execute(@VEXE + ' install ${mod_url}')
	assert res.exit_code == 0, res.output
	res = os.execute(@VEXE + ' install ${mod_url}')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
	assert res.output.contains('already exists')
}

fn test_missing_repo_name_in_url() {
	incomplete_url := 'https://github.com/vlang'
	res := os.execute(@VEXE + ' install ${incomplete_url}')
	assert res.exit_code == 1
	assert res.output.trim_space() == 'Errors while retrieving module name for: "${incomplete_url}"'
}
