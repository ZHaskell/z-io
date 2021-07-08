# Revision history for Z-IO

## 1.0.0.0  -- 2020-07-08

* Clean function names in `Z.IO.BIO` module, now no `BIO` or `Node` suffix anymore.
* `Z.IO.BIO` is not re-exported from `Z.IO` anymore, user are recommended to import it with qualified name, e.g. `import qualified Z.IO.BIO as BIO`.
* Add `foldl'` and `foldIO'` to `Z.IO.BIO` to use with `Fold` and `FoldM` from `foldl` package.
* Add `INLINABLE` pragmas to many functions.
* Add `printStdLnP` to `Z.IO.StdStream`, a `Parser` debug tool.

## 0.8.1.0  -- 2020-06-12

* Remove `-march=native` flag to improve binary portability.

## 0.8.1.0  -- 2020-04-25

* Add `getInterface` to `Z.IO.Network`.
* `mkstemp` now return opend file, the type changed to `mkstemp :: CBytes -> CBytes -> Bool -> Resource (CBytes, File)`, which has an option for keep file or not.
* `initTempFile` and `initTempDir` now do not need a prefix argument, the prefix is hardcoded as `Z-IO-`.

## 0.8.0.0  -- 2020-04-25

This is an experimental version to test new 'BIO' module.

* Rewrite `Z.IO.BIO` module, now `BIO` is push based.
* Remove `>|>`, `>~>`, `>!>`, now `BIO` nodes can be composed via funtion composition `(.)`!
* Remove `zipSource/zipBIO`, add `stepBIO/stepBIO_/runBIO_`.
* Add `zipBIO` to `Z.IO.BIO.Concurrent`, which run two BIO nodes concurrently.
* Add `ungroupingNode`, change `newGroupingNode` to use `Vector`.
* Rename `EOF` exception to `UnexpectedEOF` to avoid the clash with `EOF` pattern.

## 0.7.1.0  -- 2020-03-16

* Use `CPtr` from Z-Data instead of `ForeignPtr`.

## 0.7.0.0  -- 2020-03-09

* Change resource `Pool` to keyed by default, add `SimplePool`.
* Add `Semigroup` instance to `Logger`.
* Add `clearInputBuffer/clearOutputBuffer` to `Z.IO.Buffered`.
* Add `catchSync/ingoreSync` to `Z.IO.Exception`.
* Add `putStdLn/printStdLn` back.

## 0.6.4.0  -- 2020-02-20

* Add `initProcess'` to kill process while finish using the process resource by default.

## 0.6.3.0  -- 2020-02-20

* Split `Z.IO.UV.FFI` to `Z.IO.UV.FFI` and `Z.IO.UV.FFI_Env`, to make the module buildable when memory is constrained.
* Make functions works on TTY in `Z.IO.StdStream` correctly ignore redirected streams.
* Move `pathSeparator` to `pathSeparators`, now `pathSeparator` return the default path separator.

## 0.6.2.0  -- 2020-02-18

* Hide `Logger` constructor from `Z.IO.Logger`, remove implementation details such as `defaultTSCache`, `pushLogIORef`, `flushLogIORef`, add `loggerFormatter` to `LoggerConfig`.
* Add `newStdLogger/newFileLogger` to make new logger easily.
* Rework `Z.IO.FileSystem.Watch`'s API, change `watchDirs` to accept a recursive param and a callback.
* Hide `Z.IO.UV.Win` module, which should not be directly used by user.
* Fix a bug when stdio is redirected to pipes: https://github.com/ZHaskell/z-io/pull/16

## 0.6.1.0  -- 2020-02-09

* Fix a bug in `newMagicSplitter/newLineSplitter` code.
* Remove `sourceFromInput` and related functions to reduce API surface, use `newBufferedInput` with `sourceFromBuffered` instead.
* Refactor server loop to allow more code sharing between `Z.IO.Network.TCP` and `Z.IO.Network.IPC`.

## 0.6.0.0  -- 2020-02-04

* FileSystem: replace `DEFAULT_MODE` with `DEFAULT_FILE_MODE` & `DEFAULT_DIR_MODE`.
* Ignore exception while `mkdirp` on an exist directory.
* Make `rmrf` more like `rm -rf`, which can be used on files.
* Add `doesPathExist/doesFileExist/doesDirExist` to file system module.
* Add `Z.IO.FileSystem` re-export `Z.IO.FileSystem.Watch` and `Z.IO.FileSystem.FilePath`.
* Add `mkstemp`, `initTempFile/initTempDir` to file system module.

## 0.5.0.0  -- 2020-01-28

* Add `unwrap/unwrap'` to `Z.IO.Exception`.
* Add `readParseChunks` to `Z.IO.Buffered`, Change `readParser`'s type to match `readParseChunks`.
* Add `sourceParseChunksBufferedInput`, `sourceParseChunksInput` to `Z.IO.BIO`.
* Add `newJSONLogger/defaultJSONFmt` to `Z.IO.Logger`, provide simple JSON structured logging.

## 0.3.0.0  -- 2020-12-29

* Add `getSystemTime'` to `Z.IO.Time`.
* Add `shutdownUVStream` to `Z.IO.UV.UVStream`.
* Change `sourceFrom/sinkToFile` to `initSourceFrom/initSinkToFile`.
* Bump `Z-Data` version.

## 0.2.0.0  -- 2020-12-16

* Add `sourceParsedBufferInput` and JSON sources to `Z.IO.BIO`.
* Fix `readLine` and `newLineSplitter`.
* Improve low resolution timer precision.
* Fix a bug in `Z.IO.FileSystem.FilePath.relative`, see [#17](https://github.com/likle/cwalk/issues/17).

## 0.1.9.0  -- 2020-11-23

* Clean up API in `Z.IO.Buffered`, remove `readToMagic'`, `readLine'`, `readExactly'`.
* `readExactly` now throw exception when not reading enough bytes before EOF.
* Add `Show/ShowT` instance to `UVStream`, `StdStream`, `UDP`, `UVManager`.
* Add JSON instance to various types: `SocketAddr` and all configure types.
* Rename `InetAddr` to `IPv4`, `Inet6Addr` to `IPv6`, change `SocketAddr` 's constructor name, and payload order.
* Add `seek` to `Z.IO.FileSystem`.

## 0.1.8.1  -- 2020-11-21

* Export `ZStream` type from `Z.IO.BIO.Zlib`

## 0.1.8.0  -- 2020-11-20

* Remove type index from `BufferedInput`, `BufferedOutput`.
* Add `Z.IO.BIO` module to facilitate streaming process, and `Z.IO.BIO.Concurrent` to facilitate producer-consumer model.
* Remove streamming related functions from `Z.IO.Buffered`, use `Z.IO.BIO` instead.
* Move `Z.IO.Compression.Zlib` to `Z.IO.BIO.Zlib`, change API to `BIO` style.
* Add `Z.IO.FileSystem.Watch` module, provides cross-platform filesystem watching.

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
