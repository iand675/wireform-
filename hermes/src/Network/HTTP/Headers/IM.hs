{-# LANGUAGE TemplateHaskell #-}

{- |
@IM@ (Instance-Manipulation) is a response header that lists, in the
order they were applied, the instance-manipulations — delta encodings
such as @vcdiff@ or @gdiff@, and/or transfer codings like @gzip@ —
used to derive the transmitted entity from its base instance. It is
the response-side counterpart of the client's @A-IM@ preference and
accompanies a @226 IM Used@ delta-encoded response (RFC 3229).

== Grammar

@
IM                    = #( instance-manipulation )
instance-manipulation = \"vcdiff\" / \"diffe\" / \"gzip\" / \"deflate\"
                      / \"gdiff\" / \"range\" / \"identity\"
                      / token
@

A comma list of instance-manipulation tokens (no quality weights,
unlike 'Network.HTTP.Headers.AIM.AIM'). Tokens are surfaced verbatim
as 'ST.ShortText' since extension manipulations are permitted.

Spec: <https://www.rfc-editor.org/rfc/rfc3229#section-10.5.4>

See also: "Network.HTTP.Headers.AIM", "Network.HTTP.Headers.DeltaBase", "Network.HTTP.Headers.ContentEncoding".
-}
module Network.HTTP.Headers.IM (
  IM (..),
  iMParser,
  renderIM,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIM)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The full @IM@ header value: the ordered list of manipulations applied.
newtype IM = IM
  { instanceManipulations :: [ST.ShortText]
  }
  deriving stock (Eq, Show)


instance KnownHeader IM where
  type ParseFailure IM = String
  type Cardinality IM = 'ZeroOrOne
  type Direction IM = 'Response


  parseFromHeaders _ headers = case runParser iMParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing IM header: " <> show rest
    Fail -> Left "Failed to parse IM header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIM


  headerName _ = hIM


iMParser :: ParserT st String IM
iMParser = IM <$> (imToken `sepBy` $(char ','))
  where
    imToken = ows *> rfc9110Token <* ows


renderIM :: IM -> M.Builder
renderIM (IM xs) = M.intersperse ", " (map shortText xs)
