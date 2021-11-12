import os
import toml

const (
	toml_text = '[[a]]
    [[a.b]]
        [a.b.c]
            d = "val0"
    [[a.b]]
        [a.b.c]
            d = "val1"
'
)

fn test_nested_array_of_tables() {
	mut toml_doc := toml.parse(toml_text) or { panic(err) }

	toml_json := toml_doc.to_json()

	eprintln(toml_json)
	assert toml_json == os.read_file(
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out') or { panic(err) }
}
