import encoding.vorbis
import os

fn test_compilation() {
	assert vorbis.wrapper_version() == '1.22'
}

fn test_decode_file() {
	x := vorbis.decode_file(os.join_path(@VROOT, 'examples/sokol/sounds/pickup.ogg'))!
	assert x.path.ends_with('pickup.ogg')
	assert x.channels == 1
	assert x.sample_rate == 44100
	assert x.sample_len == 5478
}
