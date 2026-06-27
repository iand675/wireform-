{- |
Module      : Network.HTTP.Headers.RepeatabilityRequestID
Description : Representation of the @Repeatability-Request-ID@ request header field

Part of the OData repeatable-requests protocol. @Repeatability-Request-ID@ is a
request header carrying a client-generated identifier (a UUID) that uniquely
identifies a request. When a request is retried it carries the same
@Repeatability-Request-ID@, allowing the server to recognise the retry and avoid
processing it twice.

@
  Repeatability-Request-ID = token   ; a UUID, e.g. "8b59...e2"
@

The value is a UUID rendered as a @token@ on the wire; we capture it faithfully
as the token text so it round-trips exactly.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §4.1).

See also: "Network.HTTP.Headers.RepeatabilityClientID", "Network.HTTP.Headers.RepeatabilityFirstSent",
"Network.HTTP.Headers.RepeatabilityResult".
-}
module Network.HTTP.Headers.RepeatabilityRequestID (
  RepeatabilityRequestID (..),
  repeatabilityRequestIDParser,
  renderRepeatabilityRequestID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityRequestID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Repeatability-Request-ID@ value: a client-generated UUID token.
newtype RepeatabilityRequestID = RepeatabilityRequestID {repeatabilityRequestID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityRequestID where
  type ParseFailure RepeatabilityRequestID = String
  type Cardinality RepeatabilityRequestID = 'ZeroOrOne
  type Direction RepeatabilityRequestID = 'Request


  parseFromHeaders _ headers = case runParser repeatabilityRequestIDParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Repeatability-Request-ID header: " <> show rest
    Fail -> Left "Failed to parse Repeatability-Request-ID header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRepeatabilityRequestID


  headerName _ = hRepeatabilityRequestID


repeatabilityRequestIDParser :: ParserT st String RepeatabilityRequestID
repeatabilityRequestIDParser = RepeatabilityRequestID <$> rfc9110Token


renderRepeatabilityRequestID :: RepeatabilityRequestID -> M.Builder
renderRepeatabilityRequestID (RepeatabilityRequestID uuid) = shortText uuid
