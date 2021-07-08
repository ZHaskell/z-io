{-|
Module      : Z.IO.BIO.Base
Description : Composable IO Loops
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'BIO' (block IO) type to facilitate writing streaming programs. A 'BIO' node usually:

  * Process input in unit of block(or item).
  * Running in constant spaces, which means the memory usage won't accumulate.
  * Keep some state in IO, which is sealed in 'BIO' closure.

Some examples of such nodes are:

  * Compressor \/ decompressor, e.g. zlib, etc.
  * Codec, e.g. utf8 codec, base64 codec.
  * Ciphers.
  * Packet parsers.

We use @BIO inp out@ type to represent all the objects above, @BIO Void out@ to represent an 'IO' source,
and @BIO inp Void@ to represent an 'IO' sink, which can all be connected with '>|>' to build a larger 'BIO' node.

@
import Z.Data.CBytes    (CBytes)
import Z.IO
import Z.IO.BIO
import Z.IO.BIO.Zlib

base64AndCompressFile :: HasCallStack => CBytes -> CBytes -> IO ()
base64AndCompressFile origin target = do
    base64Enc <- newBase64Encoder
    (_, zlibCompressor) <- newCompress defaultCompressConfig{compressWindowBits = 31}

    withResource (initSourceFromFile origin) $ \ src ->
        withResource (initSinkToFile target) $ \ sink ->
            run_ $ src . base64Enc . zlibCompressor . sink

> base64AndCompressFile "test" "test.gz"
-- run 'zcat "test.gz" | base64 -d' will give you original file
@

This module is intended to be imported qualified:
@
import           Z.IO.BIO (BIO, Source, Sink)
import qualified Z.IO.BIO as BIO
@

-}
module Z.IO.BIO (
  -- * The BIO type
    BIO, pattern EOF, Source, Sink
  -- ** Basic combinators
  , appendSource, concatSource, concatSource'
  , joinSink, fuseSink
  -- * Run BIO chain
  , discard
  , step, step_
  , run, run_
  , runBlock, runBlock_, unsafeRunBlock
  , runBlocks, runBlocks_, unsafeRunBlocks
  -- * Make new BIO
  , fromPure, fromIO
  , filter, filterIO
  -- * Use with fold
  , fold', foldIO'
  -- ** Source
  , initSourceFromFile
  , initSourceFromFile'
  , sourceFromIO
  , sourceFromList
  , sourceFromBuffered
  , sourceTextFromBuffered
  , sourceJSONFromBuffered
  , sourceParserFromBuffered
  , sourceParseChunkFromBuffered
  -- ** Sink
  , sinkToIO
  , sinkToList
  , initSinkToFile
  , sinkToBuffered
  , sinkBuilderToBuffered
  -- ** Bytes specific
  , newReChunk
  , newUTF8Decoder
  , newParser, newMagicSplitter, newLineSplitter
  , newBase64Encoder, newBase64Decoder
  , hexEncode
  , newHexDecoder
  -- ** Generic BIO
  , counter
  , seqNum
  , newGrouping
  , ungrouping
  , consumed
  -- * Concurrent helpers
  , zip, newTQueuePair, newTBQueuePair, newBroadcastTChanPair
  ) where

import Z.IO.BIO.Base
import Z.IO.BIO.Concurrent
import Z.IO.BIO.Zlib
import           Prelude                hiding (filter, zip)
