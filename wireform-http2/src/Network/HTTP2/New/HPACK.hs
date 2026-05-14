{-# LANGUAGE LambdaCase #-}
-- | HPACK wrapper that adds:
-- * A per-connection mutex so multiple threads can't corrupt the dynamic table.
-- * Header encoding/decoding helpers that return 'ByteString' blobs
--   ready to embed in HEADERS/CONTINUATION frames.
module Network.HTTP2.New.HPACK
    ( newEncoder
    , newDecoder
    , withHpackLock
    , encodeHeaders
    , decodeHeaders
    , module Network.HPACK
    ) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI

import Network.HPACK hiding (encodeHeader)
import qualified Network.HPACK as HPACK

----------------------------------------------------------------

-- | Allocate a fresh HPACK encoder dynamic table (for sending).
newEncoder :: Size -> IO DynamicTable
newEncoder = newDynamicTableForEncoding

-- | Allocate a fresh HPACK decoder dynamic table (for receiving).
newDecoder :: Size -> IO DynamicTable
newDecoder maxSize = newDynamicTableForDecoding maxSize 4096

-- | Run an action while holding the HPACK encoding mutex.
-- All HEADERS frame encoding for a connection must go through this.
withHpackLock :: MVar () -> IO a -> IO a
withHpackLock lock action = withMVar lock (\_ -> action)

----------------------------------------------------------------

-- | Encode a header list to an HPACK block.
-- Uses the stateful encoder; must be called under 'withHpackLock'.
encodeHeaders
    :: DynamicTable
    -> Int                          -- ^ max header-block size (bytes)
    -> [(ByteString, ByteString)]
    -> IO ByteString
encodeHeaders dyntbl maxSize hdrs =
    HPACK.encodeHeader (EncodeStrategy Linear False) maxSize dyntbl
        (map (\(k, v) -> (CI.mk k, v)) hdrs)

-- | Decode an HPACK block to a 'TokenHeaderTable'.
-- The table is used for quick header lookups (e.g. :method, :path).
decodeHeaders :: DynamicTable -> ByteString -> IO TokenHeaderTable
decodeHeaders dyntbl hpackBlock =
    decodeTokenHeader dyntbl hpackBlock
