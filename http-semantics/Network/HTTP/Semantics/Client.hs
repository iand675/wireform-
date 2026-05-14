{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTTP.Semantics.Client (
    -- * HTTP client
    Client,
    SendRequest,

    -- * Request
    Request,

    -- * Creating request
    requestNoBody,
    requestFile,
    requestStreaming,
    requestStreamingUnmask,
    requestBuilder,

    -- ** Generalized streaming interface
    OutBodyIface (..),
    requestStreamingIface,

    -- ** Trailers maker
    TrailersMaker,
    NextTrailersMaker (..),
    defaultTrailersMaker,
    setRequestTrailersMaker,

    -- * Response
    Response,

    -- ** Accessing response
    responseStatus,
    responseHeaders,
    responseBodySize,
    getResponseBodyChunk,
    getResponseBodyChunk',
    getResponseTrailers,

    -- * Aux
    Aux,
    auxPossibleClientStreams,

    -- * Types
    Scheme,
    Authority,
    Method,
    Path,
    FileSpec (..),
    FileOffset,
    ByteCount,
    module Network.HTTP.Semantics.ReadN,
    module Network.HTTP.Semantics.File,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.IORef (readIORef)
import Network.HTTP.Types (Method, RequestHeaders, Status)

import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client.Internal
import Network.HTTP.Semantics.File
import Network.HTTP.Semantics.ReadN
import Network.HTTP.Semantics.Status

----------------------------------------------------------------

-- | Send a request and receive its response.
type SendRequest = forall r. Request -> (Response -> IO r) -> IO r

-- | Client type.
type Client a = SendRequest -> Aux -> IO a

----------------------------------------------------------------

-- | Creating request without body.
requestNoBody :: Method -> Path -> RequestHeaders -> Request
requestNoBody m p hdr = Request $ OutObj hdr' OutBodyNone defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with file.
requestFile :: Method -> Path -> RequestHeaders -> FileSpec -> Request
requestFile m p hdr fileSpec = Request $ OutObj hdr' (OutBodyFile fileSpec) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with builder.
requestBuilder :: Method -> Path -> RequestHeaders -> Builder -> Request
requestBuilder m p hdr builder = Request $ OutObj hdr' (OutBodyBuilder builder) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Creating request with streaming.
requestStreaming
    :: Method
    -> Path
    -> RequestHeaders
    -> ((Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreaming m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreaming strmbdy) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

-- | Like 'requestStreaming', but run the action with exceptions masked
requestStreamingUnmask
    :: Method
    -> Path
    -> RequestHeaders
    -> ((forall x. IO x -> IO x) -> (Builder -> IO ()) -> IO () -> IO ())
    -> Request
requestStreamingUnmask m p hdr strmbdy = requestStreamingIface m p hdr $ \iface ->
    strmbdy (outBodyUnmask iface) (outBodyPush iface) (outBodyFlush iface)

-- | Generalized version of 'requestStreaming',
requestStreamingIface
    :: Method
    -> Path
    -> RequestHeaders
    -> (OutBodyIface -> IO ())
    -> Request
requestStreamingIface m p hdr strmbdy = Request $ OutObj hdr' (OutBodyStreamingIface strmbdy) defaultTrailersMaker
  where
    hdr' = addHeaders m p hdr

addHeaders :: Method -> Path -> RequestHeaders -> RequestHeaders
addHeaders m p hdr = (":method", m) : (":path", p) : hdr

-- | Setting 'TrailersMaker' to 'Response'.
setRequestTrailersMaker :: Request -> TrailersMaker -> Request
setRequestTrailersMaker (Request req) tm = Request req{outObjTrailers = tm}

----------------------------------------------------------------

-- | Getting the status of a response.
responseStatus :: Response -> Maybe Status
responseStatus (Response rsp) = getStatus $ inpObjHeaders rsp

-- | Getting the headers from a response.
responseHeaders :: Response -> TokenHeaderTable
responseHeaders (Response rsp) = inpObjHeaders rsp

-- | Getting the body size from a response.
responseBodySize :: Response -> Maybe Int
responseBodySize (Response rsp) = inpObjBodySize rsp

-- | Reading a chunk of the response body.
--   An empty 'ByteString' returned when finished.
getResponseBodyChunk :: Response -> IO ByteString
getResponseBodyChunk = fmap fst . getResponseBodyChunk'

-- | Generalization of 'getResponseBodyChunk' which also returns if the 'ByteString' is the final one
getResponseBodyChunk' :: Response -> IO (ByteString, Bool)
getResponseBodyChunk' (Response rsp) = inpObjBody rsp

-- | Reading response trailers.
--   This function must be called after 'getResponseBodyChunk'
--   returns an empty.
getResponseTrailers :: Response -> IO (Maybe TokenHeaderTable)
getResponseTrailers (Response rsp) = readIORef (inpObjTrailers rsp)
