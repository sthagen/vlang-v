// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// TODO Windows version needs to be implemented.
// Will serve as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module termios

type TcFlag = int
type Speed = int
type Cc = u8

// flag provides a termios flag of the correct size
// for the underlying C.termios structure
// It is only implemented for Unix like OSes
pub fn flag(value int) TcFlag {
	return int(value)
}

// invert is a platform dependant way to bitwise NOT (~) TcFlag
// as its length varies across platforms
// It is only implemented for Unix like OSes
pub fn invert(value TcFlag) TcFlag {
	return ~int(value)
}

pub struct Termios {
}

// tcgetattr is an unsafe wrapper around C.termios and keeps its semantic
// It is only implemented for Unix like OSes
pub fn tcgetattr(fd int, mut termios_p Termios) int {
	return -1
}

// tcsetattr is an unsafe wrapper around C.termios and keeps its semantic
// It is only implemented for Unix like OSes
pub fn tcsetattr(fd int, optional_actions int, mut termios_p Termios) int {
	return -1
}

// ioctl is an unsafe wrapper around C.ioctl and keeps its semantic
// It is only implemented for Unix like OSes
[inline]
pub fn ioctl(fd int, request u64, arg voidptr) int {
	return -1
}
