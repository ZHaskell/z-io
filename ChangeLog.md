# Revision history for Z-IO

## 0.1.5.1  -- 2020-10-02

* Export `ProcessFlag` from `Z.IO.Process`.
* Fix a bug: when exception raise from server loop an uninitialized uv_check_t is closed.
* Update libuv's version to 1.40.1.
* Change `IOEInfo` type to use `Text` instead of `CBytes`.

## 0.1.5.0  -- 2020-10-02

* Add `Z.IO.Process` module.
* Move many flag type to type alias from newtype, adjust patterns haddock.
* Sync IPC's server API to TCP's.

## 0.1.4.0  -- 2020-10-02

* Add `Z.IO.Environment` module.
* Add various instances to data types in `Z.IO.UV.FFI`.
* Fix a UDP batch receiving bug.
* Remove `UV` prefix in config data types(`Z.IO.FileSystem`, `Z.IO.UDP`).
* Change `TCP`, `IPC` server config, move server worker to start params instead of config.
* `Logger` type rework, colorful logger are possible.

## 0.1.3.0  -- 2020-09-28
* Rename `newBufferedInput/Output` to `newBufferedInput'/Output'`, add default chunk `newBufferedInput/Output`.
* Remove `ghc-pirm` depends.
* Make library works with GHC 8.6 and 8.8 again.

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
