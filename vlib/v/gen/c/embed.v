module c

import os
import rand
import v.ast
import v.pref

fn (mut g Gen) embed_file_is_prod_mode() bool {
	if g.pref.is_prod || 'debug_embed_file_in_prod' in g.pref.compile_defines {
		return true
	}
	return false
}

// gen_embed_file_struct generates C code for `$embed_file('...')` calls.
fn (mut g Gen) gen_embed_file_init(mut node ast.ComptimeCall) {
	if g.embed_file_is_prod_mode() {
		file_bytes := os.read_bytes(node.embed_file.apath) or {
			panic('unable to read file: "$node.embed_file.rpath')
		}

		if node.embed_file.compression_type == 'none' {
			node.embed_file.bytes = file_bytes
		} else {
			cache_dir := os.join_path(os.vmodules_dir(), 'cache', 'embed_file')
			cache_key := rand.ulid()
			// cache_key := md5.hexhash(node.embed_file.apath)
			if !os.exists(cache_dir) {
				os.mkdir_all(cache_dir) or { panic(err) }
			}
			cache_path := os.join_path(cache_dir, cache_key)

			vexe := pref.vexe_path()
			result := os.execute('"$vexe" compress $node.embed_file.compression_type "$node.embed_file.apath" "$cache_path"')
			if result.exit_code != 0 {
				eprintln('unable to compress file "$node.embed_file.rpath": $result.output')
				node.embed_file.bytes = file_bytes
			} else {
				compressed_bytes := os.read_bytes(cache_path) or {
					eprintln('unable to read compressed file')
					{
					}
					[]byte{}
				}
				os.rm(cache_path) or {} // clean up
				node.embed_file.is_compressed = compressed_bytes.len > 0
					&& compressed_bytes.len < file_bytes.len
				node.embed_file.bytes = if node.embed_file.is_compressed {
					compressed_bytes
				} else {
					file_bytes
				}
			}
		}
		if node.embed_file.bytes.len > 5242880 {
			eprintln('embedding of files >= ~5MB is currently not well supported')
		}
		node.embed_file.len = file_bytes.len
	}

	g.writeln('(v__embed_file__EmbedFileData){')
	g.writeln('\t\t.path = ${ctoslit(node.embed_file.rpath)},')
	if g.embed_file_is_prod_mode() {
		// apath is not needed in production and may leak information
		g.writeln('\t\t.apath = ${ctoslit('')},')
	} else {
		g.writeln('\t\t.apath = ${ctoslit(node.embed_file.apath)},')
	}
	if g.embed_file_is_prod_mode() {
		// use function generated in Gen.gen_embedded_data()
		if node.embed_file.is_compressed {
			g.writeln('\t\t.compression_type = ${ctoslit(node.embed_file.compression_type)},')
			g.writeln('\t\t.compressed = v__embed_file__find_index_entry_by_path((voidptr)_v_embed_file_index, ${ctoslit(node.embed_file.rpath)}, ${ctoslit(node.embed_file.compression_type)})->data,')
			g.writeln('\t\t.uncompressed = NULL,')
		} else {
			g.writeln('\t\t.uncompressed = v__embed_file__find_index_entry_by_path((voidptr)_v_embed_file_index, ${ctoslit(node.embed_file.rpath)}, ${ctoslit(node.embed_file.compression_type)})->data,')
		}
	} else {
		g.writeln('\t\t.uncompressed = NULL,')
	}
	g.writeln('\t\t.free_compressed = 0,')
	g.writeln('\t\t.free_uncompressed = 0,')
	if g.embed_file_is_prod_mode() {
		g.writeln('\t\t.len = $node.embed_file.len')
	} else {
		file_size := os.file_size(node.embed_file.apath)
		if file_size > 5242880 {
			eprintln('Warning: embedding of files >= ~5MB is currently not supported')
		}
		g.writeln('\t\t.len = $file_size')
	}
	g.writeln('} // \$embed_file("$node.embed_file.apath")')

	g.file.embedded_files << node.embed_file
}

// gen_embedded_data embeds data into the V target executable.
fn (mut g Gen) gen_embedded_data() {
	/*
	TODO implement support for large files - right now the setup has problems
	// with even just 10 - 50 MB files - the problem is both in V and C compilers.
	// maybe we need to write to separate files or have an external tool for large files
	// like the `rcc` tool in Qt?
	*/
	for i, emfile in g.embedded_files {
		g.embedded_data.write_string('static const unsigned char _v_embed_blob_$i[$emfile.bytes.len] = {\n    ')
		for j := 0; j < emfile.bytes.len; j++ {
			b := emfile.bytes[j].hex()
			if j < emfile.bytes.len - 1 {
				g.embedded_data.write_string('0x$b,')
			} else {
				g.embedded_data.write_string('0x$b')
			}
			if 0 == ((j + 1) % 16) {
				g.embedded_data.write_string('\n    ')
			}
		}
		g.embedded_data.writeln('\n};')
	}
	g.embedded_data.writeln('')
	g.embedded_data.writeln('const v__embed_file__EmbedFileIndexEntry _v_embed_file_index[] = {')
	for i, emfile in g.embedded_files {
		g.embedded_data.writeln('\t{$i, { .str=(byteptr)("${cestring(emfile.rpath)}"), .len=$emfile.rpath.len, .is_lit=1 }, { .str=(byteptr)("${cestring(emfile.compression_type)}"), .len=$emfile.compression_type.len, .is_lit=1 }, _v_embed_blob_$i},')
	}
	g.embedded_data.writeln('\t{-1, { .str=(byteptr)(""), .len=0, .is_lit=1 }, { .str=(byteptr)(""), .len=0, .is_lit=1 }, NULL}')
	g.embedded_data.writeln('};')
	// see vlib/v/embed_file/embed_file.v, find_index_entry_by_id/2 and find_index_entry_by_path/2
}
