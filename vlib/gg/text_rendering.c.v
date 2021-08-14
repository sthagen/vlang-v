// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import sokol.sfons
import sokol.sgl
import gx
import os

struct FT {
pub:
	fons        &C.FONScontext
	font_normal int
	font_bold   int
	font_mono   int
	font_italic int
	scale       f32 = 1.0
}

fn new_ft(c FTConfig) ?&FT {
	if c.font_path == '' {
		if c.bytes_normal.len > 0 {
			fons := sfons.create(512, 512, 1)
			bytes_normal := c.bytes_normal
			bytes_bold := if c.bytes_bold.len > 0 {
				c.bytes_bold
			} else {
				debug_font_println('setting bold variant to normal')
				bytes_normal
			}
			bytes_mono := if c.bytes_mono.len > 0 {
				c.bytes_mono
			} else {
				debug_font_println('setting mono variant to normal')
				bytes_normal
			}
			bytes_italic := if c.bytes_italic.len > 0 {
				c.bytes_italic
			} else {
				debug_font_println('setting italic variant to normal')
				bytes_normal
			}

			return &FT{
				fons: fons
				font_normal: C.fonsAddFontMem(fons, c'sans', bytes_normal.data, bytes_normal.len,
					false)
				font_bold: C.fonsAddFontMem(fons, c'sans', bytes_bold.data, bytes_bold.len,
					false)
				font_mono: C.fonsAddFontMem(fons, c'sans', bytes_mono.data, bytes_mono.len,
					false)
				font_italic: C.fonsAddFontMem(fons, c'sans', bytes_italic.data, bytes_italic.len,
					false)
				scale: c.scale
			}
		} else {
			// Load default font
		}
	}

	if c.font_path == '' || !os.exists(c.font_path) {
		$if !android {
			println('failed to load font "$c.font_path"')
			return none
		}
	}

	mut bytes := []byte{}
	$if android {
		// First try any filesystem paths
		bytes = os.read_bytes(c.font_path) or { []byte{} }
		if bytes.len == 0 {
			// ... then try the APK asset path
			bytes = os.read_apk_asset(c.font_path) or {
				println('failed to load font "$c.font_path"')
				return none
			}
		}
	} $else {
		bytes = os.read_bytes(c.font_path) or {
			println('failed to load font "$c.font_path"')
			return none
		}
	}
	bold_path := if c.custom_bold_font_path != '' {
		c.custom_bold_font_path
	} else {
		get_font_path_variant(c.font_path, .bold)
	}
	bytes_bold := os.read_bytes(bold_path) or {
		debug_font_println('failed to load font "$bold_path"')
		bytes
	}
	mono_path := get_font_path_variant(c.font_path, .mono)
	bytes_mono := os.read_bytes(mono_path) or {
		debug_font_println('failed to load font "$mono_path"')
		bytes
	}
	italic_path := get_font_path_variant(c.font_path, .italic)
	bytes_italic := os.read_bytes(italic_path) or {
		debug_font_println('failed to load font "$italic_path"')
		bytes
	}
	fons := sfons.create(512, 512, 1)
	return &FT{
		fons: fons
		font_normal: C.fonsAddFontMem(fons, c'sans', bytes.data, bytes.len, false)
		font_bold: C.fonsAddFontMem(fons, c'sans', bytes_bold.data, bytes_bold.len, false)
		font_mono: C.fonsAddFontMem(fons, c'sans', bytes_mono.data, bytes_mono.len, false)
		font_italic: C.fonsAddFontMem(fons, c'sans', bytes_italic.data, bytes_italic.len,
			false)
		scale: c.scale
	}
}

pub fn (ctx &Context) set_cfg(cfg gx.TextCfg) {
	if !ctx.font_inited {
		return
	}
	if cfg.bold {
		ctx.ft.fons.set_font(ctx.ft.font_bold)
	} else if cfg.mono {
		ctx.ft.fons.set_font(ctx.ft.font_mono)
	} else if cfg.italic {
		ctx.ft.fons.set_font(ctx.ft.font_italic)
	} else {
		ctx.ft.fons.set_font(ctx.ft.font_normal)
	}
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	size := if cfg.mono { cfg.size - 2 } else { cfg.size }
	ctx.ft.fons.set_size(scale * f32(size))
	C.fonsSetAlign(ctx.ft.fons, int(cfg.align) | int(cfg.vertical_align))
	color := C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, cfg.color.a)
	if cfg.color.a != 255 {
		sgl.load_pipeline(ctx.timage_pip)
	}
	C.fonsSetColor(ctx.ft.fons, color)
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	ctx.ft.fons.vert_metrics(&ascender, &descender, &lh)
}

pub fn (ctx &Context) draw_text(x int, y int, text_ string, cfg gx.TextCfg) {
	$if macos {
		if ctx.native_rendering {
			if cfg.align == gx.align_right {
				width := ctx.text_width(text_)
				C.darwin_draw_string(x - width, ctx.height - y, text_, cfg)
			} else {
				C.darwin_draw_string(x, ctx.height - y, text_, cfg)
			}
			return
		}
	}
	if !ctx.font_inited {
		eprintln('gg: draw_text(): font not initialized')
		return
	}
	// text := text_.trim_space() // TODO remove/optimize
	// mut text := text_
	// if text.contains('\t') {
	// text = text.replace('\t', '    ')
	// }
	ctx.set_cfg(cfg)
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	C.fonsDrawText(ctx.ft.fons, x * scale, y * scale, &char(text_.str), 0) // TODO: check offsets/alignment
}

pub fn (ctx &Context) draw_text_def(x int, y int, text string) {
	ctx.draw_text(x, y, text)
}

/*
pub fn (mut gg FT) init_font() {
}
*/
pub fn (ft &FT) flush() {
	sfons.flush(ft.fons)
}

pub fn (ctx &Context) text_width(s string) int {
	$if macos {
		if ctx.native_rendering {
			return C.darwin_text_width(s)
		}
	}
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, &char(s.str), 0, &buf[0])
	if s.ends_with(' ') {
		return int((buf[2] - buf[0]) / ctx.scale) +
			ctx.text_width('i') // TODO fix this in fontstash?
	}
	res := int((buf[2] - buf[0]) / ctx.scale)
	// println('TW "$s" = $res')
	$if macos {
		if ctx.native_rendering {
			return res * 2
		}
	}
	return int((buf[2] - buf[0]) / ctx.scale)
}

pub fn (ctx &Context) text_height(s string) int {
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, &char(s.str), 0, &buf[0])
	return int((buf[3] - buf[1]) / ctx.scale)
}

pub fn (ctx &Context) text_size(s string) (int, int) {
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0, 0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, &char(s.str), 0, &buf[0])
	return int((buf[2] - buf[0]) / ctx.scale), int((buf[3] - buf[1]) / ctx.scale)
}
