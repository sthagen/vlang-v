// vtest retry: 3
module main

import os
import rand
import v.vmod

// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_test_${rand.ulid()}')

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
	os.setenv('VPM_FAIL_ON_PROMPT', '1', true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn get_vmod(path string) vmod.Manifest {
	return vmod.from_file(os.join_path(test_path, path, 'v.mod')) or {
		eprintln('Failed to parse v.mod for `${path}`')
		exit(1)
	}
}

fn test_install_from_vpm_ident() {
	res := os.execute('${vexe} install nedpals.args')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	manifest := get_vmod(os.join_path('nedpals', 'args'))
	assert manifest.name == 'nedpals.args'
	assert manifest.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	res := os.execute('${vexe} install pcre')
	assert res.exit_code == 0, res.str()
	manifest := get_vmod('pcre')
	assert manifest.name == 'pcre'
	assert manifest.description == 'A simple regex library for V.'
}

fn test_install_from_git_url() {
	mut res := os.execute('${vexe} install https://github.com/vlang/markdown')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installing `markdown`'), res.output
	mut manifest := get_vmod('markdown')
	assert manifest.name == 'markdown'
	assert manifest.dependencies == []string{}
	res = os.execute('${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installing `HashMap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	manifest = get_vmod(os.join_path('wertzui123', 'hashmap'))
	res = os.execute('${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `wertzui123.hashmap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	res = os.execute('${vexe} install https://gitlab.com/tobealive/webview')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installed `webview`'), res.output
}

fn test_install_already_existent() {
	mut res := os.execute('${vexe} install https://github.com/vlang/markdown')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `markdown`'), res.output
	manifest := get_vmod('markdown')
	assert manifest.name == 'markdown'
	assert manifest.dependencies == []string{}
	// The same module but with the `.git` extension added.
	res = os.execute('${vexe} install https://github.com/vlang/markdown.git')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `markdown`'), res.output
}

fn test_install_once() {
	// Start with a clean test path.
	$if windows {
		// FIXME: Workaround for failing `rmdir` commands on Windows.
		os.system('rd /s /q ${test_path}')
	} $else {
		os.rmdir_all(test_path) or {}
	}
	os.mkdir_all(test_path) or {}

	// Install markdown module.
	mut res := os.execute('${vexe} install markdown')
	assert res.exit_code == 0, res.str()
	// Keep track of the last modified state of the v.mod file of the installed markdown module.
	md_last_modified := os.file_last_mod_unix(os.join_path(test_path, 'markdown', 'v.mod'))

	install_cmd := '${@VEXE} install https://github.com/vlang/markdown https://github.com/vlang/pcre --once -v'
	// Try installing two modules, one of which is already installed.
	res = os.execute(install_cmd)
	assert res.exit_code == 0, res.str()
	assert res.output.contains("Already installed modules: ['markdown']"), res.output
	manifest := get_vmod('pcre')
	assert manifest.name == 'pcre'
	assert manifest.description == 'A simple regex library for V.'
	// Ensure the before installed markdown module wasn't modified.
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))

	// Try installing two modules that are both already installed.
	res = os.execute(install_cmd)
	assert res.exit_code == 0, res.str()
	assert res.output.contains('All modules are already installed.'), res.output
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))
}

fn test_missing_repo_name_in_url() {
	incomplete_url := 'https://github.com/vlang'
	res := os.execute('${vexe} install ${incomplete_url}')
	assert res.exit_code == 1
	assert res.output.contains('failed to retrieve module name for `${incomplete_url}`'), res.output
}

fn test_manifest_detection() {
	// head branch == `main`.
	mut manifest := fetch_manifest('v-analyzer', 'https://github.com/v-analyzer/v-analyzer',
		'', true) or {
		assert false
		return
	}
	assert manifest.name == 'v-analyzer'
	assert manifest.dependencies == ['https://github.com/v-analyzer/v-tree-sitter']
	// head branch == `master`.
	manifest = fetch_manifest('ui', 'https://github.com/pisaiah/ui', '', true) or {
		assert false
		return
	}
	assert manifest.name == 'iui'
	// not a V module.
	if v := fetch_manifest('octocat', 'https://github.com/octocat/octocat.github.io',
		'', true)
	{
		assert false, v.str()
		return
	}
	mut res := os.execute('${vexe} install https://github.com/octocat/octocat.github.io')
	assert res.exit_code == 1
	assert res.output.contains('failed to find `v.mod` for `https://github.com/octocat/octocat.github.io`'), res.output
	// No error for vpm modules yet.
	res = os.execute('${vexe} install spytheman.regex')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('`spytheman.regex` is missing a manifest file'), res.output
	assert res.output.contains('Installing `spytheman.regex`'), res.output
}

fn test_install_potentially_conflicting() {
	mut res := os.execute('${vexe} install ui')
	assert res.output.contains('Installed `ui`')
	mut manifest := get_vmod('ui')
	assert manifest.name == 'ui'
	res = os.execute('${vexe} install https://github.com/isaiahpatton/ui')
	assert res.output.contains('Installed `iui`')
	manifest = get_vmod('iui')
	assert manifest.name == 'iui'
}

fn test_get_installed_version() {
	test_project_path := os.join_path(test_path, 'test_project')
	os.mkdir_all(test_project_path)!
	os.chdir(test_project_path)!
	os.write_file('v.mod', '')!
	if os.getenv('CI') != '' {
		os.execute_or_exit('git config --global user.email "v@vi.com"')
		os.execute_or_exit('git config --global user.name "V CI"')
	}
	mut res := os.execute('git init')
	assert res.exit_code == 0, res.str()
	res = os.execute('git add .')
	assert res.exit_code == 0, res.str()
	res = os.execute('git commit -m "initial commit"')
	assert res.exit_code == 0, res.str()
	mut mod := Module{
		install_path: test_project_path
	}
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	// Create a tag -> latests commit and tag are at the same state,
	// but it should not be treated as a version installation, when there is another head branch.
	res = os.execute('git tag v0.1.0')
	assert res.exit_code == 0, res.str()
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	os.execute('git checkout v0.1.0')
	assert res.exit_code == 0, res.str()
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	os.execute('git branch -D master')
	assert res.exit_code == 0, res.str()
	os.execute('git reset --hard v0.1.0')
	assert res.exit_code == 0, res.str()
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == 'v0.1.0'
}
