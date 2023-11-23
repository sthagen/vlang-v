module main

import os
import v.vmod
import v.help

enum InstallResult {
	installed
	failed
	skipped
}

fn vpm_install(query []string) {
	if settings.is_help {
		help.print_and_exit('vpm')
	}

	mut vpm_modules, mut external_modules := parse_query(if query.len == 0 {
		if os.exists('./v.mod') {
			// Case: `v install` was run in a directory of another V-module to install its dependencies
			// - without additional module arguments.
			println('Detected v.mod file inside the project directory. Using it...')
			manifest := vmod.from_file('./v.mod') or { panic(err) }
			if manifest.dependencies.len == 0 {
				println('Nothing to install.')
				exit(0)
			}
			manifest.dependencies.clone()
		} else {
			vpm_error('specify at least one module for installation.',
				details: 'example: `v install publisher.package` or `v install https://github.com/owner/repository`'
			)
			exit(2)
		}
	} else {
		query
	})

	installed_modules := get_installed_modules()

	vpm_log(@FILE_LINE, @FN, 'VPM modules: ${vpm_modules}')
	vpm_log(@FILE_LINE, @FN, 'External modules: ${external_modules}')
	vpm_log(@FILE_LINE, @FN, 'Installed modules: ${installed_modules}')

	if installed_modules.len > 0 && settings.is_once {
		num_to_install := vpm_modules.len + external_modules.len
		mut already_installed := []string{}
		if external_modules.len > 0 {
			mut i_deleted := []int{}
			for i, m in external_modules {
				if m.name in installed_modules {
					already_installed << m.name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				external_modules.delete(i)
			}
		}
		if vpm_modules.len > 0 {
			mut i_deleted := []int{}
			for i, m in vpm_modules {
				if m.name in installed_modules {
					already_installed << m.name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				vpm_modules.delete(i)
			}
		}
		if already_installed.len > 0 {
			verbose_println('Already installed modules: ${already_installed}')
			if already_installed.len == num_to_install {
				println('All modules are already installed.')
				exit(0)
			}
		}
	}

	if vpm_modules.len > 0 {
		vpm_install_from_vpm(vpm_modules)
	}
	if external_modules.len > 0 {
		vpm_install_from_vcs(external_modules)
	}
}

fn vpm_install_from_vpm(modules []Module) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	idents := modules.map(it.name)
	mut errors := 0
	for m in modules {
		vpm_log(@FILE_LINE, @FN, 'module: ${m}')
		match m.install() {
			.installed {}
			.failed {
				errors++
				continue
			}
			.skipped {
				continue
			}
		}
		increment_module_download_count(m.name) or {
			vpm_error('failed to increment the download count for `${m.name}`', details: err.msg())
			errors++
		}
		println('Installed `${m.name}`.')
		resolve_dependencies(get_manifest(m.install_path), idents)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_install_from_vcs(modules []Module) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	urls := modules.map(it.url)
	mut errors := 0
	for m in modules {
		vpm_log(@FILE_LINE, @FN, 'module: ${m}')
		match m.install() {
			.installed {}
			.failed {
				errors++
				continue
			}
			.skipped {
				continue
			}
		}
		println('Installed `${m.name}`.')
		resolve_dependencies(m.manifest, urls)
	}
	if errors > 0 {
		exit(1)
	}
}

fn (m Module) install() InstallResult {
	if m.is_installed {
		// Case: installed, but not an explicit version. Update instead of continuing the installation.
		if m.version == '' && m.installed_version == '' {
			if m.is_external && m.url.starts_with('http://') {
				vpm_update([m.install_path.all_after(settings.vmodules_path).trim_left(os.path_separator).replace(os.path_separator,
					'.')])
			} else {
				vpm_update([m.name])
			}
			return .skipped
		}
		// Case: installed, but conflicting. Confirmation or -[-f]orce flag required.
		if settings.is_force || m.confirm_install() {
			m.remove() or {
				vpm_error('failed to remove `${m.name}`.', details: err.msg())
				return .failed
			}
		} else {
			return .skipped
		}
	}
	vcs := m.vcs or { settings.vcs }
	version_opt := if m.version != '' { ' ${vcs.args.version} ${m.version}' } else { '' }
	cmd := '${vcs.cmd} ${vcs.args.install}${version_opt} "${m.url}" "${m.install_path}"'
	vpm_log(@FILE_LINE, @FN, 'command: ${cmd}')
	println('Installing `${m.name}`...')
	verbose_println('  cloning from `${m.url}` to `${m.install_path_fmted}`')
	res := os.execute_opt(cmd) or {
		vpm_error('failed to install `${m.name}`.', details: err.msg())
		return .failed
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output}')
	return .installed
}

fn (m Module) confirm_install() bool {
	if m.installed_version == m.version {
		println('Module `${m.name}${at_version(m.installed_version)}` is already installed, use --force to overwrite.')
		return false
	} else {
		install_version := at_version(if m.version == '' { 'latest' } else { m.version })
		println('Module `${m.name}${at_version(m.installed_version)}` is already installed at `${m.install_path_fmted}`.')
		if settings.fail_on_prompt {
			vpm_error('VPM should not have entered a confirmation prompt.')
			exit(1)
		}
		input := os.input('Replace it with `${m.name}${install_version}`? [Y/n]: ')
		match input.to_lower() {
			'', 'y' {
				return true
			}
			else {
				verbose_println('Skipping `${m.name}`.')
				return false
			}
		}
	}
}

fn (m Module) remove() ! {
	verbose_println('Removing `${m.name}` from `${m.install_path_fmted}`...')
	$if windows {
		os.execute_opt('rd /s /q ${m.install_path}')!
	} $else {
		os.rmdir_all(m.install_path)!
	}
	verbose_println('Removed `${m.name}`.')
}

fn at_version(version string) string {
	return if version != '' { '@${version}' } else { '' }
}
