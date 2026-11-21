{- |
Module      : Network.HTTP.Headers.RepeatabilityClientID
Description : Representation of the @Repeatability-Client-ID@ request header field

Part of the OData repeatable-requests protocol. @Repeatability-Client-ID@ is an
optional identifier for a client application instance. A server can use it to
group repeatability records for bulk deletion. The codec accepts UUIDs and
other HTTP tokens as opaque identifiers and preserves their spelling.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §3.1.3).

See also: "Network.HTTP.Headers.RepeatabilityRequestID", "Network.HTTP.Headers.RepeatabilityFirstSent",
"Network.HTTP.Headers.RepeatabilityResult".
-}
module Network.HTTP.Headers.RepeatabilityClientID (
  RepeatabilityClientID (..),
  repeatabilityClientIDParser,
  renderRepeatabilityClientID,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityClientID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Repeatability-Client-ID@ value: an opaque client identifier token.
newtype RepeatabilityClientID = RepeatabilityClientID {repeatabilityClientID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityClientID where
  type ParseFailure RepeatabilityClientID = String
  type Cardinality RepeatabilityClientID = 'ZeroOrOne
  type Direction RepeatabilityClientID = 'Request


  parseFromHeaders _ = parseTokenFieldLine "Repeatability-Client-ID" RepeatabilityClientID


  renderToHeaders _ = M.toStrictByteString . renderRepeatabilityClientID


  renderToHeadersChecked _ (RepeatabilityClientID identifier)
    | isHttpTokenText identifier = Just (ST.toByteString identifier)
    | otherwise = Nothing


  headerName _ = hRepeatabilityClientID


repeatabilityClientIDParser :: ParserT st String RepeatabilityClientID
repeatabilityClientIDParser = RepeatabilityClientID <$> rfc9110Token


renderRepeatabilityClientID :: RepeatabilityClientID -> M.Builder
renderRepeatabilityClientID (RepeatabilityClientID identifier) = shortText identifier
