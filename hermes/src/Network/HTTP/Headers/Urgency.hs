{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Urgency request header (RFC 8030 §5.3): tells the push service how
important a message is to the user agent, letting it conserve resources on
constrained devices by delaying or dropping low-priority messages. Defined
values are @very-low@, @low@, @normal@, and @high@; the default when the header
is absent is 'Normal'.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.3>

See also: "Network.HTTP.Headers.TTL", "Network.HTTP.Headers.Topic",
"Network.HTTP.Headers.Priority".
-}
module Network.HTTP.Headers.Urgency (
  Urgency (..),
  urgencyParser,
  renderUrgency,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hUrgency)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The relative importance of a push message (RFC 8030 §5.3).
data Urgency
  = VeryLow
  | Low
  | Normal
  | High
  deriving stock (Eq, Show, Enum, Bounded)


instance KnownHeader Urgency where
  type ParseFailure Urgency = String
  type Cardinality Urgency = 'ZeroOrOne
  type Direction Urgency = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser urgencyParser header of
      OK urgency "" -> Right urgency
      OK _ rest -> Left $ "Unconsumed input after parsing Urgency header: " <> show rest
      Fail -> Left "Failed to parse Urgency header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderUrgency


  headerName _ = hUrgency


urgencyParser :: ParserT st String Urgency
urgencyParser =
  $( switch
      [|
        case _ of
          "very-low" -> pure VeryLow
          "low" -> pure Low
          "normal" -> pure Normal
          "high" -> pure High
        |]
   )


renderUrgency :: Urgency -> M.Builder
renderUrgency = \case
  VeryLow -> "very-low"
  Low -> "low"
  Normal -> "normal"
  High -> "high"
