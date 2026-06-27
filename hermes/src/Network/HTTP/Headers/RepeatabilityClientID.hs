{- |
Module      : Network.HTTP.Headers.RepeatabilityClientID
Description : Representation of the @Repeatability-Client-ID@ request header field

Part of the OData repeatable-requests protocol. @Repeatability-Client-ID@ is an
optional request header carrying a client-generated identifier (a UUID) that,
together with 'Network.HTTP.Headers.RepeatabilityRequestID.RepeatabilityRequestID',
lets a server detect and de-duplicate retried requests.

@
  Repeatability-Client-ID = token   ; a UUID, e.g. "0e8a7e9f-..."
@

The value is a UUID rendered as a @token@ on the wire; we capture it faithfully
as the token text so it round-trips exactly.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §4.1).

See also: "Network.HTTP.Headers.RepeatabilityRequestID", "Network.HTTP.Headers.RepeatabilityFirstSent",
"Network.HTTP.Headers.RepeatabilityResult".
-}
module Network.HTTP.Headers.RepeatabilityClientID (
  RepeatabilityClientID (..),
  repeatabilityClientIDParser,
  renderRepeatabilityClientID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityClientID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Repeatability-Client-ID@ value: a client-generated UUID token.
newtype RepeatabilityClientID = RepeatabilityClientID {repeatabilityClientID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityClientID where
  type ParseFailure RepeatabilityClientID = String
  type Cardinality RepeatabilityClientID = 'ZeroOrOne
  type Direction RepeatabilityClientID = 'Request


  parseFromHeaders _ headers = case runParser repeatabilityClientIDParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Repeatability-Client-ID header: " <> show rest
    Fail -> Left "Failed to parse Repeatability-Client-ID header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRepeatabilityClientID


  headerName _ = hRepeatabilityClientID


repeatabilityClientIDParser :: ParserT st String RepeatabilityClientID
repeatabilityClientIDParser = RepeatabilityClientID <$> rfc9110Token


renderRepeatabilityClientID :: RepeatabilityClientID -> M.Builder
renderRepeatabilityClientID (RepeatabilityClientID uuid) = shortText uuid
