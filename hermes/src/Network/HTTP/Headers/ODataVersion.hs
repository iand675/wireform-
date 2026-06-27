{-# LANGUAGE TemplateHaskell #-}

{- |
@OData-Version@ — identifies the version of the OData protocol used to
generate a request or response payload. Sent on both requests and responses
(see @OData-MaxVersion@ for the maximum version a client can interpret).

== Grammar

@
OData-Version = major "." minor
major         = 1*DIGIT
minor         = 1*DIGIT
@

The only versions defined by the specification are @4.0@ and @4.01@; the
value is surfaced structurally as @major.minor@.

Spec: OData Version 4.01 Part 1: Protocol §8.1.5
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataVersion>

See also: "Network.HTTP.Headers.ODataMaxVersion", "Network.HTTP.Headers.ODataIsolation",
"Network.HTTP.Headers.ODataEntityId", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.ODataVersion (
  ODataVersion (..),
  oDataVersionParser,
  renderODataVersion,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | An OData protocol version @major.minor@ (e.g. @4.0@, @4.01@).
data ODataVersion = ODataVersion
  { oDataVersionMajor :: !Word
  , oDataVersionMinor :: !Word
  }
  deriving stock (Eq, Show)


instance KnownHeader ODataVersion where
  type ParseFailure ODataVersion = String
  type Cardinality ODataVersion = 'ZeroOrOne
  type Direction ODataVersion = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser oDataVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing OData-Version header: " <> show rest
    Fail -> Left "Failed to parse OData-Version header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderODataVersion


  headerName _ = hODataVersion


oDataVersionParser :: ParserT st String ODataVersion
oDataVersionParser = do
  major <- anyAsciiDecimalWord
  $(char '.')
  minor <- anyAsciiDecimalWord
  pure ODataVersion {oDataVersionMajor = major, oDataVersionMinor = minor}


renderODataVersion :: ODataVersion -> M.Builder
renderODataVersion (ODataVersion major minor) =
  M.wordDec major <> M.char7 '.' <> M.wordDec minor
