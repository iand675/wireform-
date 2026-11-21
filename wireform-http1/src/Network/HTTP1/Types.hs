{-# LANGUAGE CPP #-}
{- | HTTP message types.

This module re-exports the per-aspect modules ('Method', 'Status',
'Version', 'Headers') and adds the high-level 'Request' \/ 'Response'
records used by the client and server APIs.
-}
module Network.HTTP1.Types
  ( -- * Re-exports
    module Network.HTTP1.Method
  , module Network.HTTP1.Version
  , module Network.HTTP1.Status
  , module Network.HTTP1.Headers

    -- * Message bodies
  , Body (..)
  , noBody
  , byteStringBody
  , streamBody

    -- * Pre-encoded responses
  , PreEncoded (..)
  , preEncodedHead

    -- * File-backed responses
  , FileBody (..)
  , FileSource (..)
  , wholeFileBody
  , wholeFileBodyFd

    -- * Request \/ Response
  , Request (..)
  , Response (..)
  , RawTarget
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified System.Posix.Files as Posix
import qualified System.Posix.IO as PosixIO
import System.Posix.Types (Fd (..))

import Network.HTTP1.Headers
import Network.HTTP1.Method
import Network.HTTP1.Status
import Network.HTTP1.Version

-- | The raw HTTP\/1.x request-target. We do not parse this here:
-- depending on the deployment ('OriginForm' for direct origin servers,
-- 'AbsoluteForm' for forward proxies, 'AuthorityForm' for CONNECT,
-- 'AsteriskForm' for OPTIONS *) the right shape differs. Applications
-- that need a parsed URI should layer a URI library on top.
type RawTarget = ByteString

------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------

-- | A request \/ response body.
--
-- Four variants:
--
--   ['BodyEmpty']        No body. Encoded as @Content-Length: 0@ when
--                        an explicit length is required (POST, PUT,
--                        etc.); no framing header otherwise.
--   ['BodyBytes']        A single contiguous strict 'ByteString'. The
--                        encoder knows its length and emits
--                        @Content-Length: n@.
--   ['BodyStream']       A producer @IO (Maybe ByteString)@ that
--                        yields chunks until it returns 'Nothing'.
--                        A fixed-length response checks the supplied
--                        @Content-Length@. An unknown-length response
--                        uses chunked framing on HTTP\/1.1 or closes
--                        the connection on HTTP\/1.0.
--   ['BodyPreEncoded']   Wire-ready response bytes. The server sends the
--                        cached head and data in one operation and writes
--                        any deferred trailer block afterwards. Construct via
--                        'Network.HTTP1.Encode.precomputeResponse'.
--   ['BodyFile']         File-backed body. The server opens the file
--                        (or accepts a caller-opened 'Fd') and
--                        streams it to the socket with @sendfile(2)@
--                        — no userspace buffer allocation, the kernel
--                        DMAs from the file-system cache to the NIC.
--                        Used for static file serving (the equivalent
--                        of @nginx@'s @sendfile on@ /
--                        @h2o@'s @file.dir@).
data Body
  = BodyEmpty
  | BodyBytes !ByteString
  | BodyStream !(IO (Maybe ByteString))
  | BodyPreEncoded !PreEncoded
  | BodyFile !FileBody

instance Show Body where
  show BodyEmpty = "BodyEmpty"
  show (BodyBytes bs) = "BodyBytes " <> show bs
  show (BodyStream _) = "BodyStream <IO>"
  show (BodyPreEncoded pe) = "BodyPreEncoded " <> show pe
  show (BodyFile fb) = "BodyFile " <> show fb

instance NFData Body where
  rnf BodyEmpty = ()
  rnf (BodyBytes bs) = rnf bs
  rnf (BodyStream _) = ()
  rnf (BodyPreEncoded pe) = rnf pe
  rnf (BodyFile fb) = rnf fb

noBody :: Body
noBody = BodyEmpty

byteStringBody :: ByteString -> Body
byteStringBody = BodyBytes

streamBody :: IO (Maybe ByteString) -> Body
streamBody = BodyStream

------------------------------------------------------------------------
-- PreEncoded
------------------------------------------------------------------------

-- | Wire-ready HTTP\/1.x response bytes.
--
-- 'peBytes' holds the head (status line + headers + CRLFCRLF)
-- concatenated with the body. 'peHeadLen' is the byte offset of the
-- first body byte — the server slices to that length to honour HEAD
-- (which MUST emit the same metadata as GET, sans body, per RFC 9110
-- § 9.3.2).
--
-- Construct via 'Network.HTTP1.Encode.precomputeResponse'. The slice
-- for HEAD is zero-copy (it shares 'peBytes'\' 'ForeignPtr').
--
-- 'PreEncoded' holds a complete message. 'PreEncodedWithTrailers'
-- holds a head and optional data chunk. The server appends the
-- terminator and obtains its fields from 'responseTrailers'.
--
-- Keep-alive decisions use the surrounding 'Response' record's
-- 'responseHeaders'. The server can add a per-connection option before
-- the final CRLF of the encoded head. Keep the baked-in headers and the
-- record in sync when constructing these values directly.
data PreEncoded
  = PreEncoded
      { peBytes :: !ByteString
      , peHeadLen :: !Int
      }
  | PreEncodedWithTrailers
      { peBytes :: !ByteString
      , peHeadLen :: !Int
      }
  deriving stock (Eq, Show, Generic)

instance NFData PreEncoded

-- | Zero-copy slice that holds only the head (no body). Used by the
-- server to serve HEAD from a GET-shaped precomputed response.
{-# INLINE preEncodedHead #-}
preEncodedHead :: PreEncoded -> ByteString
preEncodedHead pe = BSU.unsafeTake (peHeadLen pe) (peBytes pe)

------------------------------------------------------------------------
-- FileBody
------------------------------------------------------------------------

-- | A file-backed response body.
--
-- The server sends the extent starting at 'fbOffset'. On plain TCP it
-- uses @sendfile(2)@. A declared trailer selects chunked HTTP/1.1 framing.
data FileBody = FileBody
  { fbSource :: !FileSource
  , fbOffset :: !Word64
    -- ^ Byte offset within the file to start sending from.
  , fbLength :: !Word64
    -- ^ Number of bytes to send.
  }
  deriving stock (Eq, Show, Generic)

instance NFData FileBody

-- | Where the file bytes come from.
--
-- ['FileSourcePath']  A 'FilePath'. The server @open()@s and
--                     @close()@s the file on each request. Simple,
--                     correct, and re-reads if the file is replaced
--                     between requests — but costs three syscalls per
--                     request ('openat', 'sendfile', 'close').
-- ['FileSourceFd']    A pre-opened 'System.Posix.Types.Fd'. The
--                     server uses it directly and does NOT close it.
--                     The application owns the fd's lifetime
--                     (typically opens once at startup, reuses for
--                     every request, closes when the process exits).
--                     This is the @nginx open_file_cache@ /
--                     @h2o file.dir@ shape and the one that
--                     matches their published numbers.
data FileSource
  = FileSourcePath !FilePath
  | FileSourceFd   !Fd
  deriving stock (Eq, Show, Generic)

instance NFData FileSource where
  rnf (FileSourcePath p) = rnf p
  rnf (FileSourceFd (Fd n)) = rnf n

-- | Stat the file at the given path and construct a 'FileBody' that
-- sends its entire contents. The server opens / closes the file on
-- each request.
--
-- @
-- staticHello :: IO Response
-- staticHello = do
--   body <- 'wholeFileBody' \"\/srv\/www\/hello.txt\"
--   pure $ Response OK HTTP_1_1
--            [(\"Content-Type\", \"text\/plain\")]
--            (BodyFile body)
-- @
wholeFileBody :: FilePath -> IO FileBody
wholeFileBody path = do
  st <- Posix.getFileStatus path
  pure FileBody
    { fbSource = FileSourcePath path
    , fbOffset = 0
    , fbLength = fromIntegral (Posix.fileSize st)
    }

-- | Open the file at the given path, stat it, and construct a
-- 'FileBody' carrying the open fd. The fd is kept open after this
-- returns; the application is responsible for closing it (typically:
-- never, the process owns it until exit).
--
-- This is the right shape for static-file servers that want to avoid
-- the per-request @open()@ + @close()@ pair — it's what
-- @nginx open_file_cache@ + @sendfile on@ and @h2o file.dir@ do
-- internally. At ~hello-world response sizes the three saved syscalls
-- per request is roughly a 2-3× throughput improvement.
--
-- @
-- main = do
--   fb <- 'wholeFileBodyFd' \"\/srv\/www\/hello.txt\"
--   let staticResp = Response OK HTTP_1_1
--                      [(\"Content-Type\", \"text\/html\")]
--                      (BodyFile fb)
--   runServer cfg { serverHandler = \\_ -> pure staticResp }
-- @
wholeFileBodyFd :: FilePath -> IO FileBody
wholeFileBodyFd path = do
#if MIN_VERSION_unix(2,8,0)
  fd <- PosixIO.openFd path PosixIO.ReadOnly PosixIO.defaultFileFlags
#else
  fd <- PosixIO.openFd path PosixIO.ReadOnly Nothing PosixIO.defaultFileFlags
#endif
  st <- Posix.getFdStatus fd
  pure FileBody
    { fbSource = FileSourceFd fd
    , fbOffset = 0
    , fbLength = fromIntegral (Posix.fileSize st)
    }

------------------------------------------------------------------------
-- Request / Response
------------------------------------------------------------------------

-- | A parsed (or about-to-be-sent) HTTP request.
--
-- The 'requestBody' is a producer that the request handler can call
-- multiple times. After the first call yields 'Nothing' (end-of-stream),
-- subsequent calls must continue to yield 'Nothing' so that handlers
-- can defensively read more than they need.
data Request = Request
  { requestMethod   :: !Method
  , requestTarget   :: !RawTarget
  , requestVersion  :: !Version
  , requestHeaders  :: !Headers
  , requestBody     :: !Body
    -- ^ For an incoming request this is a 'BodyStream' producer that
    -- reads from the connection's recv buffer. For an outgoing client
    -- request it is whatever the caller supplied.
  , requestTrailers :: !(IO Headers)
    -- ^ Trailer block carried after a chunked request body
    -- (RFC 9112 § 7).  For an incoming request the action blocks
    -- until the body has been fully drained; returns @[]@ when
    -- the framing carried no trailers.
    --
    -- For an outgoing chunked request, the action runs after the body
    -- producer finishes. Declare the anticipated fields in @Trailer@.
  }

instance Show Request where
  show r =
    "Request " <> show (requestMethod r) <> " "
              <> show (requestTarget r) <> " "
              <> show (requestVersion r) <> " "
              <> show (requestHeaders r)

instance NFData Request where
  rnf Request{requestMethod=m,requestTarget=t,requestVersion=v,requestHeaders=h,requestBody=b} =
    rnf m `seq` rnf t `seq` rnf v `seq` rnf h `seq` rnf b
    -- 'requestTrailers' is an IO action; we can't normalise it
    -- without running it, so it's intentionally left out.

data Response = Response
  { responseStatus  :: !Status
  , responseVersion :: !Version
  , responseHeaders :: !Headers
  , responseBody    :: !Body
  , responseTrailers :: !(IO Headers)
    -- ^ Trailer block carried after the body.
    --
    -- /Inbound:/ surfaced by 'sendRequestOn' from the chunked body's
    -- terminator field block (or @[]@ for non-chunked responses).
    -- Returns @[]@ when the framing carried no trailers.
    --
    -- /Outbound:/ servers that want to emit trailers populate this
    -- with a (possibly @IO@-deferred) field list. On a chunked response
    -- the action runs after the body finishes. For an HTTP\/1.1 response,
    -- a nonempty @Trailer@ declaration or an explicit @Transfer-Encoding@
    -- selects chunked framing for an empty, buffered, streaming, or file body.
  }

instance Show Response where
  show r =
    "Response " <> show (responseStatus r) <> " "
                <> show (responseVersion r) <> " "
                <> show (responseHeaders r)

instance NFData Response where
  rnf Response{responseStatus=s,responseVersion=v,responseHeaders=h,responseBody=b} =
    rnf s `seq` rnf v `seq` rnf h `seq` rnf b
    -- 'responseTrailers' is an IO action; we leave it out for
    -- the same reason as 'requestTrailers' above.
