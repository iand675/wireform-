{- |
Module      : Network.HTTP.Headers.RepeatabilityRequestID
Description : Representation of the @Repeatability-Request-ID@ request header field

Part of the OData repeatable-requests protocol. @Repeatability-Request-ID@ is a
request header carrying a client-generated identifier that uniquely
identifies a request. When a request is retried it carries the same
@Repeatability-Request-ID@, allowing the server to recognise the retry and avoid
processing it twice.

Clients generate UUIDs. The codec also accepts other HTTP tokens as opaque
identifiers and preserves their spelling.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §3.1.1).

See also: "Network.HTTP.Headers.RepeatabilityClientID", "Network.HTTP.Headers.RepeatabilityFirstSent",
"Network.HTTP.Headers.RepeatabilityResult".
-}
module Network.HTTP.Headers.RepeatabilityRequestID (
  RepeatabilityRequestID (..),
  repeatabilityRequestIDParser,
  renderRepeatabilityRequestID,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityRequestID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Repeatability-Request-ID@ value: an opaque request identifier token.
newtype RepeatabilityRequestID = RepeatabilityRequestID {repeatabilityRequestID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityRequestID where
  type ParseFailure RepeatabilityRequestID = String
  type Cardinality RepeatabilityRequestID = 'ZeroOrOne
  type Direction RepeatabilityRequestID = 'Request


  parseFromHeaders _ = parseTokenFieldLine "Repeatability-Request-ID" RepeatabilityRequestID


  renderToHeaders _ = M.toStrictByteString . renderRepeatabilityRequestID


  headerName _ = hRepeatabilityRequestID


repeatabilityRequestIDParser :: ParserT st String RepeatabilityRequestID
repeatabilityRequestIDParser = RepeatabilityRequestID <$> rfc9110Token


renderRepeatabilityRequestID :: RepeatabilityRequestID -> M.Builder
renderRepeatabilityRequestID (RepeatabilityRequestID identifier) = shortText identifier
