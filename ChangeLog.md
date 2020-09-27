# Revision history for Z-IO

## 0.1.2.0  -- 2020-09-28

* Add file offset interface back, see `FilePtr` and `FilePtrT`.
* Remove `checkFileTClosed` from `Z.IO.FileSystem.Threaded`.
* Take c source file list from libuv Makefile, remove lib requirements on linux and osx.
* Fix `uv_check_t` initiate order in accept loop, which cause a segfault.

## 0.1.1.2  -- 2020-09-25

* Fix macOS build problem caused by missing zconf.h.
* Add more tests(TCP, IPC).

## 0.1.1.0  -- 2020-09-19

* Add stream utilities to `Z.IO.Buffered`.
* Add `Z.Compression.Zlib`.
