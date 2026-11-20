{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.RepeatabilityResult
Description : Representation of the @Repeatability-Result@ response header field

Part of the OData repeatable-requests protocol. @Repeatability-Result@ is a
response header by which a server reports the disposition of a repeatable
request: whether the request was @accepted@ for (idempotent) processing or
@rejected@ because the server could not honour the repeatability guarantee.

@
  Repeatability-Result = "accepted" / "rejected"
@

The two values are case-insensitive on the wire but, by convention, lower-case;
we model them as a closed enumeration and render the canonical lower-case form.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §3.2.1).

See also: "Network.HTTP.Headers.RepeatabilityRequestID", "Network.HTTP.Headers.RepeatabilityClientID",
"Network.HTTP.Headers.RepeatabilityFirstSent", "Network.HTTP.Headers.PreferenceApplied".
-}
module Network.HTTP.Headers.RepeatabilityResult (
  RepeatabilityResult (..),
  repeatabilityResultParser,
  renderRepeatabilityResult,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityResult)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Repeatability-Result@ value: the disposition of a repeatable request.
data RepeatabilityResult
  = -- | The request was accepted for repeatable processing.
    Accepted
  | -- | The repeatability guarantee could not be honoured.
    Rejected
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityResult where
  type ParseFailure RepeatabilityResult = String
  type Cardinality RepeatabilityResult = 'ZeroOrOne
  type Direction RepeatabilityResult = 'Response


  parseFromHeaders _ ("accepted" :| []) = Right Accepted
  parseFromHeaders _ ("rejected" :| []) = Right Rejected
  parseFromHeaders _ fields = parseSingleFieldLine "Repeatability-Result" repeatabilityResultParser fields


  renderToHeaders _ = M.toStrictByteString . renderRepeatabilityResult


  headerName _ = hRepeatabilityResult


repeatabilityResultParser :: ParserT st String RepeatabilityResult
repeatabilityResultParser =
  $( switchAsciiCI
       [|
         case _ of
           "accepted" -> pure Accepted
           "rejected" -> pure Rejected
         |]
   )


renderRepeatabilityResult :: RepeatabilityResult -> M.Builder
renderRepeatabilityResult = \case
  Accepted -> "accepted"
  Rejected -> "rejected"
