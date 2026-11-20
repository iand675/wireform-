{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
@OData-MaxVersion@ — a request header specifying the maximum version of the
OData protocol that the client is able to interpret the response in. The
server selects a version no higher than this and echoes it in @OData-Version@.

== Grammar

@
OData-MaxVersion = major "." minor
major            = 1*DIGIT
minor            = 1*DIGIT
@

Versions are compared as decimals. The fractional digits in @4.01@ and
@4.1@ represent different values, so leading fractional zeros are retained.
Each numeric component is bounded by 'Word'.

Spec: OData Version 4.01 Part 1: Protocol §8.2.7
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataMaxVersion>

See also: "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataIsolation",
"Network.HTTP.Headers.ODataEntityId", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.ODataMaxVersion (
  ODataMaxVersion (ODataMaxVersion, ODataMaxVersionWithLeadingZeros, oDataMaxVersionMajor, oDataMaxVersionMinor),
  oDataMaxVersionLeadingZeros,
  oDataMaxVersionParser,
  renderODataMaxVersion,
) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataMaxVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The maximum acceptable OData protocol version @major.minor@.
data ODataMaxVersion
  = -- | Render the two components in their ordinary decimal spelling.
    ODataMaxVersion
      { oDataMaxVersionMajor :: !Word
      , oDataMaxVersionMinor :: !Word
      }
  | -- | Render the specified number of additional zeros before the minor component.
    ODataMaxVersionWithLeadingZeros
      { oDataMaxVersionMajor :: !Word
      , oDataMaxVersionMinor :: !Word
      , fractionalLeadingZeros :: !Int
      }
  deriving stock (Eq, Show)


-- | The number of zeros before the ordinary decimal spelling of the minor component.
oDataMaxVersionLeadingZeros :: ODataMaxVersion -> Int
oDataMaxVersionLeadingZeros (ODataMaxVersion _ _) = 0
oDataMaxVersionLeadingZeros (ODataMaxVersionWithLeadingZeros _ _ zeros) = zeros


instance KnownHeader ODataMaxVersion where
  type ParseFailure ODataMaxVersion = String
  type Cardinality ODataMaxVersion = 'ZeroOrOne
  type Direction ODataMaxVersion = 'Request


  parseFromHeaders _ ("4.0" :| []) = Right (ODataMaxVersion 4 0)
  parseFromHeaders _ ("4.01" :| []) = Right (ODataMaxVersionWithLeadingZeros 4 1 1)
  parseFromHeaders _ fields = parseSingleFieldLine "OData-MaxVersion" oDataMaxVersionParser fields


  renderToHeaders _ (ODataMaxVersion 4 0) = "4.0"
  renderToHeaders _ (ODataMaxVersionWithLeadingZeros 4 1 1) = "4.01"
  renderToHeaders _ value = M.toStrictByteString (renderODataMaxVersion value)


  headerName _ = hODataMaxVersion


oDataMaxVersionParser :: ParserT st String ODataMaxVersion
oDataMaxVersionParser = do
  major <- boundedDecimalWord maxBound failed
  $(char '.')
  withSpan (boundedDecimalWord maxBound failed) $ \minor (Span start end) -> do
    let zeros = subPos end start - decimalDigits minor
    pure $
      if zeros == 0
        then ODataMaxVersion major minor
        else ODataMaxVersionWithLeadingZeros major minor zeros
  where
    decimalDigits = go 1
    go digits n
      | n < 10 = digits
      | otherwise = go (digits + 1) (n `quot` 10)


renderODataMaxVersion :: ODataMaxVersion -> M.Builder
renderODataMaxVersion (ODataMaxVersion major minor) =
  M.wordDec major <> M.char7 '.' <> M.wordDec minor
renderODataMaxVersion (ODataMaxVersionWithLeadingZeros major minor zeros) =
  M.wordDec major <> M.char7 '.' <> M.byteString (BS.replicate zeros 48) <> M.wordDec minor
