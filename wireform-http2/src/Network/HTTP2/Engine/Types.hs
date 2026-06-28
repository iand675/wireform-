{-# LANGUAGE RankNTypes #-}
-- | Shared types for the wireform-http2 gRPC-friendly engine.
--
-- These follow the same shape as @http-semantics@'s @InpObj@ /
-- @OutObj@ / @OutBodyIface@ vocabulary. The naming and field layout
-- are deliberately compatible because wireform-grpc was originally
-- written against those types; keeping them aligned lets us drop the
-- @http-semantics@ + @http2@ + @http2-tls@ dependencies without
-- rewriting every grpc-side module.
module Network.HTTP2.Engine.Types
  ( -- * Tokenised headers
    Token (..)
  , TokenHeader
  , TokenHeaderTable
  , tokenKey
  , tokenCIKey
  , tokenFoldedKey
  , tokeniseHeaders
  , detokeniseHeaders
  , lookupToken
    -- * Trailers
  , TrailersMaker
  , NextTrailersMaker (..)
  , defaultTrailersMaker
    -- * Input / output objects
  , InpObj (..)
  , InpBody
  , OutObj (..)
  , OutBody (..)
  , OutBodyIface (..)
    -- * File specs (placeholder)
  , FileSpec (..)
  , FileOffset
  , ByteCount
    -- * Position-read maker (placeholder)
  , PositionReadMaker
  , PositionRead
  , Sentinel (..)
  , defaultPositionReadMaker
    -- * Buffer-size alias
  , BufferSize
    -- * Scheme / Path / Authority aliases
  , Scheme
  , Authority
  , Path
  ) where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Int (Int64)
import Data.IORef (IORef)
import Network.HTTP.Header (Header)

-- | Tokenised header name. Wraps the raw bytes; we don't maintain the
-- static-table integer index that @http2@ does.
newtype Token = Token { unToken :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

type TokenHeader = (Token, ByteString)

-- | HPACK-decoded header block plus a values-table placeholder. The
-- second component is unused — wireform-grpc only pattern-matches on
-- the first component — and is kept as @()@ so the type still parses
-- the @(thl, _) <- ...@ destructuring sites in grpc-side code.
type TokenHeaderTable = ([TokenHeader], ())

tokenKey :: Token -> ByteString
tokenKey = unToken
{-# INLINE tokenKey #-}

-- | Original-case bytes of the header name. Matches http-semantics
-- which preserves the case as received.
tokenCIKey :: Token -> ByteString
tokenCIKey = unToken
{-# INLINE tokenCIKey #-}

-- | Folded (lower-case) bytes of the header name.
tokenFoldedKey :: Token -> ByteString
tokenFoldedKey = CI.foldedCase . CI.mk . unToken
{-# INLINE tokenFoldedKey #-}

tokeniseHeaders :: [(ByteString, ByteString)] -> TokenHeaderTable
tokeniseHeaders hs = (map (\(k, v) -> (Token k, v)) hs, ())

detokeniseHeaders :: TokenHeaderTable -> [(ByteString, ByteString)]
detokeniseHeaders (hs, _) = map (\(Token k, v) -> (k, v)) hs

-- | First value matching the given lowercased pseudo / regular header
-- name. Case-insensitive.
lookupToken :: ByteString -> TokenHeaderTable -> Maybe ByteString
lookupToken name (hs, _) = go hs
  where
    nameCI = CI.mk name
    go [] = Nothing
    go ((Token k, v):rest)
      | CI.mk k == nameCI = Just v
      | otherwise         = go rest

-- | Trailer-block computer for streamed responses (RFC 9113 §8.1).
type TrailersMaker = Maybe ByteString -> IO NextTrailersMaker

data NextTrailersMaker
  = NextTrailersMaker !TrailersMaker
  | Trailers ![Header]

defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = pure (Trailers [])
defaultTrailersMaker (Just _) = pure (NextTrailersMaker defaultTrailersMaker)

-- | Input body: returns @(chunk, isFinal)@. An empty 'ByteString'
-- signals end-of-stream.
type InpBody = IO (ByteString, Bool)

-- | A received message: headers, optional body-size hint, body
-- callback, and a trailer slot the engine fills in once trailers
-- arrive on the wire.
data InpObj = InpObj
  { inpObjHeaders :: !TokenHeaderTable
  , inpObjBodySize :: !(Maybe Int)
  , inpObjBody :: !InpBody
  , inpObjTrailers :: !(IORef (Maybe TokenHeaderTable))
  }

instance Show InpObj where
  show (InpObj (thl, _) _ _ _) = show thl

-- | A message we're about to send: headers, body, and a trailers
-- computer for trailing HEADERS frames.
data OutObj = OutObj
  { outObjHeaders :: ![Header]
  , outObjBody :: !OutBody
  , outObjTrailers :: !TrailersMaker
  }

instance Show OutObj where
  show (OutObj hs _ _) = show hs

-- | Outgoing body shape. Only 'OutBodyNone' and 'OutBodyStreamingIface'
-- are actually consumed by wireform-grpc; 'OutBodyBuilder' is also
-- supported by the engine, 'OutBodyStreaming' is a thin wrapper, and
-- 'OutBodyFile' is unimplemented (wireform-grpc never builds one).
data OutBody
  = OutBodyNone
  | OutBodyStreaming ((BSB.Builder -> IO ()) -> IO () -> IO ())
  | OutBodyStreamingIface (OutBodyIface -> IO ())
  | OutBodyBuilder BSB.Builder
  | OutBodyFile FileSpec

-- | Streaming-body callback bundle (matches http-semantics shape).
data OutBodyIface = OutBodyIface
  { outBodyUnmask :: forall x. IO x -> IO x
  , outBodyPush :: BSB.Builder -> IO ()
  , outBodyPushFinal :: BSB.Builder -> IO ()
  , outBodyCancel :: Maybe SomeException -> IO ()
  , outBodyFlush :: IO ()
  }

-- | File spec for @sendfile@-style responses. wireform-grpc never
-- builds one; the engine errors out if asked.
data FileSpec = FileSpec !FilePath !FileOffset !ByteCount
  deriving stock (Eq, Show)

type FileOffset = Int64
type ByteCount = Int64

type Scheme = ByteString
type Authority = String
type Path = ByteString

type PositionRead = FileOffset -> ByteCount -> IO ByteString
type PositionReadMaker = FilePath -> IO (PositionRead, Sentinel)

data Sentinel
  = Closer !(IO ())
  | Refresher !(IO ())

-- | A 'PositionReadMaker' that returns empty bytes. wireform-grpc
-- never serves files via the HTTP/2 engine, so this is just a typed
-- placeholder for the field slot in @Config@.
defaultPositionReadMaker :: PositionReadMaker
defaultPositionReadMaker _ = pure (\_ _ -> pure BS.empty, Closer (pure ()))

type BufferSize = Int
