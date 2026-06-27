{-# LANGUAGE TemplateHaskell #-}

{- |
@traceparent@ request\/response header — W3C Trace Context.

The @traceparent@ field carries the incoming distributed-trace
context across service boundaries. For the only currently defined
version (@00@) its value is four hyphen-separated hex fields:

@
traceparent = version \"-\" trace-id \"-\" parent-id \"-\" trace-flags
version     = 2HEXDIG     ; \"00\"
trace-id    = 32HEXDIG    ; 16-byte trace identifier, lowercase hex
parent-id   = 16HEXDIG    ; 8-byte calling-span identifier
trace-flags = 2HEXDIG     ; 8-bit field, e.g. \"01\" == sampled
@

@trace-id@ and @parent-id@ are 128\/64-bit values that overflow a
machine word, so they are surfaced as their verbatim hex text;
@version@ and @trace-flags@ are decoded to octets.

Spec: <https://www.w3.org/TR/trace-context/#traceparent-header>.

See also: "Network.HTTP.Headers.Tracestate", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XRequestID".
-}
module Network.HTTP.Headers.Traceparent (
  Traceparent (..),
  traceparentParser,
  renderTraceparent,
) where

import Control.Monad (replicateM_)
import Data.Char (digitToInt, intToDigit, isHexDigit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTraceparent)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A parsed @traceparent@ value (version @00@ layout).
data Traceparent = Traceparent
  { traceparentVersion :: !Word8
  -- ^ Trace-context version (@00@ at present).
  , traceparentTraceId :: !ST.ShortText
  -- ^ 32 lowercase hex digits identifying the whole trace.
  , traceparentParentId :: !ST.ShortText
  -- ^ 16 lowercase hex digits identifying the calling span.
  , traceparentFlags :: !Word8
  -- ^ 8-bit @trace-flags@ field (bit 0 == @sampled@).
  }
  deriving stock (Eq, Show)


instance KnownHeader Traceparent where
  type ParseFailure Traceparent = String
  type Cardinality Traceparent = 'ZeroOrOne
  type Direction Traceparent = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser traceparentParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Traceparent header: " <> show rest
    Fail -> Left "Failed to parse Traceparent header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTraceparent


  headerName _ = hTraceparent


traceparentParser :: ParserT st String Traceparent
traceparentParser = do
  version <- hexWord8
  $(char '-')
  traceId <- hexDigits 32
  $(char '-')
  parentId <- hexDigits 16
  $(char '-')
  flags <- hexWord8
  pure
    Traceparent
      { traceparentVersion = version
      , traceparentTraceId = traceId
      , traceparentParentId = parentId
      , traceparentFlags = flags
      }


renderTraceparent :: Traceparent -> M.Builder
renderTraceparent (Traceparent version traceId parentId flags) =
  renderHexWord8 version
    <> M.char7 '-'
    <> shortText traceId
    <> M.char7 '-'
    <> shortText parentId
    <> M.char7 '-'
    <> renderHexWord8 flags


-- | Parse exactly @n@ hex digits, kept verbatim.
hexDigits :: Int -> ParserT st e ST.ShortText
hexDigits n = shortASCIIFromParser_ (replicateM_ n (skipSatisfyAscii isHexDigit))


-- | Parse exactly two hex digits as a single octet.
hexWord8 :: ParserT st e Word8
hexWord8 = do
  hi <- hexNibble
  lo <- hexNibble
  pure $! fromIntegral (hi * 16 + lo)
  where
    hexNibble = digitToInt <$> satisfyAscii isHexDigit


-- | Render an octet as two lowercase hex digits.
renderHexWord8 :: Word8 -> M.Builder
renderHexWord8 w = M.char7 (intToDigit hi) <> M.char7 (intToDigit lo)
  where
    (hi, lo) = (fromIntegral w :: Int) `divMod` 16
