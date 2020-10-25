# Revision history for Z-IO

## 0.1.8.0  -- 2020-10-24

* Remove dictionary from `BufferedInput`, `BufferedOutput`, add `readBufferText`, `sourceBufferText`.

## 0.1.7.0  -- 2020-10-24

* Add `iso8016DateFormat`, change logger's default time format to include time zone.
* Rename `warn` to `warning`, change `Level` to `Int` type alias in `Z.IO.Logger`, add `critical`.
* Export `TimeVal` from `Z.IO.Environment`.
* Add `getCPUInfo`, `getLoadAvg`, `getXXXMem` to `Z.IO.Environment`.

## 0.1.6.1  -- 2020-10-17

* Export `ResUsage` from `Z.IO.Environment`.
* Export `Level` from `Z.IO.Logger`.
* Add linefeed with default logger formattor.

## 0.1.6.0  -- 2020-10-17

* Fix a bug affects udp exception raising(simliar to the one fixed in 0.1.5.2).
* Add `Z.IO.StdStream.Ansi` module, add a default colored logger.
* Add `Z.IO.Time` module, for fast time parsing and formatting.
* Add `Z.IO.FileSystem.FilePath` module for file path manipulations.
* Add `getCWD`, `chDir`, `getHomeDir`, `getTempDir`, `getPassWD` to `Z.IO.Environment`.
* Add `chown`, `fchown`, `lchown` to `Z.IO.FileSystem` and `Z.IO.FileSystem.Threaded`.
* Rename `UVFD` to `FD` accross module.

## 0.1.5.2  -- 2020-10-13

* Fix windows dist(add `fs-fd-hash-inl.h` to other-source-files).

## 0.1.5.1  -- 2020-10-13

* Export `ProcessFlag` from `Z.IO.Process`.
* Add quick read & write functions to fileSystem modules.
* Fix a bug: when exception raise from server loop an uninitialized uv_check_t is closed.
* Update libuv's version to 1.40.1.
* Change `IOEInfo` type to use `Text` instead of `CBytes`.

## 0.1.5.0  -- 2020-10-10

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
