module rand

import time
// uuid_v4 generates a random (v4) UUID
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)

pub fn uuid_v4() string {
	buflen := 36
	mut buf := unsafe { malloc_noscan(37) }
	mut i_buf := 0
	mut x := u64(0)
	mut d := byte(0)
	for i_buf < buflen {
		mut c := 0
		x = default_rng.u64()
		// do most of the bit manipulation at once:
		x &= 0x0F0F0F0F0F0F0F0F
		x += 0x3030303030303030
		// write the ASCII codes to the buffer:
		for c < 8 && i_buf < buflen {
			d = byte(x)
			unsafe {
				buf[i_buf] = if d > 0x39 { d + 0x27 } else { d }
			}
			i_buf++
			c++
			x = x >> 8
		}
	}
	// there are still some random bits in x:
	x = x >> 8
	d = byte(x)
	unsafe {
		buf[19] = if d > 0x39 { d + 0x27 } else { d }
		buf[8] = `-`
		buf[13] = `-`
		buf[18] = `-`
		buf[23] = `-`
		buf[14] = `4`
		buf[buflen] = 0
		return buf.vstring_with_len(buflen)
	}
}

const (
	ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'
)

// ulid generates an Unique Lexicographically sortable IDentifier.
// See https://github.com/ulid/spec .
// NB: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return ulid_at_millisecond(u64(time.utc().unix_time_milli()))
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_time_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	buflen := 26
	mut buf := unsafe { malloc_noscan(27) }
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = rand.ulid_encoding[t & 0x1F]
		}
		t = t >> 5
		i--
	}
	// first rand set
	mut x := default_rng.u64()
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	// second rand set
	x = default_rng.u64()
	for i < 26 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	unsafe {
		buf[26] = 0
		return buf.vstring_with_len(buflen)
	}
}

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`
pub fn string_from_set(charset string, len int) string {
	if len == 0 {
		return ''
	}
	mut buf := unsafe { malloc_noscan(len + 1) }
	for i in 0 .. len {
		unsafe {
			buf[i] = charset[intn(charset.len)]
		}
	}
	unsafe {
		buf[len] = 0
	}
	return unsafe { buf.vstring_with_len(len) }
}

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn string(len int) string {
	return string_from_set(english_letters, len)
}

// hex returns a hexadecimal number of length `len` containing random characters in range `[a-f0-9]`.
pub fn hex(len int) string {
	return string_from_set(hex_chars, len)
}

// ascii returns a random string of the printable ASCII characters with length `len`.
pub fn ascii(len int) string {
	return string_from_set(ascii_chars, len)
}

fn deinit() {
	unsafe {
		default_rng.free() // free the implementation
		free(default_rng) // free the interface wrapper itself
	}
}

// init initializes the default RNG.
fn init() {
	default_rng = new_default()
	C.atexit(deinit)
}
