{-# LANGUAGE TemplateHaskell #-}

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

The value is surfaced structurally as @major.minor@ (e.g. @4.0@, @4.01@).

Spec: OData Version 4.01 Part 1: Protocol §8.2.6
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataMaxVersion>

See also: "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataIsolation",
"Network.HTTP.Headers.ODataEntityId", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.ODataMaxVersion (
  ODataMaxVersion (..),
  oDataMaxVersionParser,
  renderODataMaxVersion,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataMaxVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The maximum acceptable OData protocol version @major.minor@.
data ODataMaxVersion = ODataMaxVersion
  { oDataMaxVersionMajor :: !Word
  , oDataMaxVersionMinor :: !Word
  }
  deriving stock (Eq, Show)


instance KnownHeader ODataMaxVersion where
  type ParseFailure ODataMaxVersion = String
  type Cardinality ODataMaxVersion = 'ZeroOrOne
  type Direction ODataMaxVersion = 'Request


  parseFromHeaders _ headers = case runParser oDataMaxVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing OData-MaxVersion header: " <> show rest
    Fail -> Left "Failed to parse OData-MaxVersion header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderODataMaxVersion


  headerName _ = hODataMaxVersion


oDataMaxVersionParser :: ParserT st String ODataMaxVersion
oDataMaxVersionParser = do
  major <- anyAsciiDecimalWord
  $(char '.')
  minor <- anyAsciiDecimalWord
  pure ODataMaxVersion {oDataMaxVersionMajor = major, oDataMaxVersionMinor = minor}


renderODataMaxVersion :: ODataMaxVersion -> M.Builder
renderODataMaxVersion (ODataMaxVersion major minor) =
  M.wordDec major <> M.char7 '.' <> M.wordDec minor
