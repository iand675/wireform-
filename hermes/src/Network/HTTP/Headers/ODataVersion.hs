{-# LANGUAGE TemplateHaskell #-}

{- |
@OData-Version@ — identifies the version of the OData protocol used to
generate a request or response payload. Sent on both requests and responses
(see @OData-MaxVersion@ for the maximum version a client can interpret).

== Grammar

@
OData-Version = "4.0" [ %x31-39 ]
@

The major number is 4 and the optional final digit is stored as the minor
number. For example, @4.01@ is represented by @ODataVersion 4 1@.

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

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Grammar.Rendering (encodeDecimalVersion)
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


  parseFromHeaders _ ("4.0" :| []) = Right (ODataVersion 4 0)
  parseFromHeaders _ ("4.01" :| []) = Right (ODataVersion 4 1)
  parseFromHeaders _ fields = parseSingleFieldLine "OData-Version" oDataVersionParser fields


  renderToHeaders _ (ODataVersion 4 0) = "4.0"
  renderToHeaders _ (ODataVersion 4 1) = "4.01"
  renderToHeaders _ value = M.toStrictByteString (renderODataVersion value)


  renderToHeadersChecked _ (ODataVersion 4 0) = Just "4.0"
  renderToHeadersChecked _ (ODataVersion 4 1) = Just "4.01"
  renderToHeadersChecked _ (ODataVersion 4 minor)
    | minor <= 9 = Just (encodeDecimalVersion 1 4 minor)
  renderToHeadersChecked _ _ = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hODataVersion


oDataVersionParser :: ParserT st String ODataVersion
oDataVersionParser = do
  $(string "4.0")
  minor <- withSatisfyAscii (\c -> '1' <= c && c <= '9') (pure . fromIntegral . subtract (fromEnum '0') . fromEnum) <|> pure 0
  pure (ODataVersion 4 minor)


renderODataVersion :: ODataVersion -> M.Builder
renderODataVersion (ODataVersion 4 minor)
  | minor == 0 = "4.0"
  | minor <= 9 = "4.0" <> M.wordDec minor
renderODataVersion (ODataVersion major minor) =
  M.wordDec major <> M.char7 '.' <> M.wordDec minor
