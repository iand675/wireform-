{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Types (
    -- * Request/response as input
    InpObj (..),
    InpBody,

    -- * Request/response as output
    OutObj (..),
    OutBody (..),
    OutBodyIface (..),

    -- * Trailers maker
    TrailersMaker,
    defaultTrailersMaker,
    NextTrailersMaker (..),

    -- * File spec
    FileOffset,
    ByteCount,
    FileSpec (..),

    -- * Types
    Scheme,
    Authority,
    Path,
) where

import Control.Exception (SomeException)
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.Int (Int64)
import Network.ByteOrder
import qualified Network.HTTP.Types as H

import Network.HTTP.Semantics.Header
import Network.HTTP.Semantics.Trailer

----------------------------------------------------------------

-- | "http" or "https".
type Scheme = ByteString

-- | Authority.
type Authority = String

-- | Path.
type Path = ByteString

----------------------------------------------------------------

type InpBody = IO (ByteString, Bool)

data OutBody
    = OutBodyNone
    | -- | Streaming body takes a write action and a flush action.
      OutBodyStreaming ((Builder -> IO ()) -> IO () -> IO ())
    | -- | Generalization of 'OutBodyStreaming'.
      OutBodyStreamingIface (OutBodyIface -> IO ())
    | OutBodyBuilder Builder
    | OutBodyFile FileSpec

data OutBodyIface = OutBodyIface
    { outBodyUnmask :: (forall x. IO x -> IO x)
    -- ^ Unmask exceptions in the thread spawned for the request body
    --
    -- This is used in the client: we spawn the new thread for the request body
    -- with exceptions masked, and provide the body of 'OutBodyStreamingIface'
    -- with a callback to unmask them again (typically after installing an
    -- exception handler).
    --
    -- Unmasking in the server is a no-op, as here the scope of the thread that
    -- is spawned for the server is the entire handler, not just the response
    -- streaming body.
    , outBodyPush :: Builder -> IO ()
    -- ^ Push a new chunk
    --
    -- In @http2@, there is no direct correspondence between chunks and the
    -- resulting @DATA@ frames sent: the chunks are collected (written to an
    -- internal write buffer) until we can fill a frame.
    --
    -- See also 'outBodyFlush'.
    , outBodyPushFinal :: Builder -> IO ()
    -- ^ Push the final chunk
    --
    -- Using this function instead of 'outBodyPush' can be used to guarantee
    -- that the final HTTP2 DATA frame is marked end-of-stream; with
    -- 'outBodyPush' it may happen that an additional empty DATA frame is used
    -- for this purpose. Additionally, after calling this function,
    -- 'outBodyCancel' will be a no-op.
    , outBodyCancel :: Maybe SomeException -> IO ()
    -- ^ Cancel the stream
    --
    -- Sends a @RST_STREAM@ to the peer. If cancelling as the result of an
    -- exception, a 'Just' should be provided which specifies the exception
    -- which will be stored locally as the reason for cancelling the stream; in
    -- this case, the error code sent with the @RST_STREAM@ will be
    -- @INTERNAL_ERROR@ (see
    -- <https://datatracker.ietf.org/doc/html/rfc7540#section-7>). If 'Nothing'
    -- is given, the error code will be @CANCEL@.
    --
    -- If there is a partially constructed @DATA@ frame at the time of
    -- cancellation, this frame is discarded. If this is undesirable, you should
    -- call 'outBodyFlush' prior to cancelling.
    , outBodyFlush :: IO ()
    -- ^ Flush
    --
    -- This can be used to emit a DATA frame with the data collected so far
    -- (using 'outBodyPush'), even if that DATA frame has not yet reached the
    -- maximum frame size. Calling 'outBodyFlush' unnecessarily can therefore
    -- result in excessive overhead from frame headers.
    --
    -- If no data is available to send, this is a no-op.
    }

-- | Input object
data InpObj = InpObj
    { inpObjHeaders :: TokenHeaderTable
    -- ^ Accessor for headers.
    , inpObjBodySize :: Maybe Int
    -- ^ Accessor for body length specified in content-length:.
    , inpObjBody :: InpBody
    -- ^ Accessor for body.
    , inpObjTrailers :: IORef (Maybe TokenHeaderTable)
    -- ^ Accessor for trailers.
    }

instance Show InpObj where
    show (InpObj (thl, _) _ _body _tref) = show thl

-- | Output object
data OutObj = OutObj
    { outObjHeaders :: [H.Header]
    -- ^ Accessor for header.
    , outObjBody :: OutBody
    -- ^ Accessor for outObj body.
    , outObjTrailers :: TrailersMaker
    -- ^ Accessor for trailers maker.
    }

instance Show OutObj where
    show (OutObj hdr _ _) = show hdr

----------------------------------------------------------------

-- | Offset for file.
type FileOffset = Int64

-- | How many bytes to read
type ByteCount = Int64

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)
