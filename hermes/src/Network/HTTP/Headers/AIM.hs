{-# LANGUAGE TemplateHaskell #-}

{- |
@A-IM@ (Accept-Instance-Manipulation) is a request header by which a
client advertises which instance-manipulations — delta-encoding
algorithms such as @vcdiff@, @gdiff@, or @diffe@, or ordinary transfer
codings like @gzip@ — it is willing to have applied to the response,
each with an optional quality weight. It is the delta-encoding
counterpart of @Accept-Encoding@ (RFC 3229, "Delta encoding in HTTP").

== Grammar

@
A-IM                  = #( instance-manipulation [ \";\" \"q\" \"=\" qvalue ] )
instance-manipulation = \"vcdiff\" / \"diffe\" / \"gzip\" / \"deflate\"
                      / \"gdiff\" / \"range\" / \"identity\"
                      / token
@

This mirrors 'Network.HTTP.Headers.AcceptEncoding': a comma list of
weighted instance-manipulation tokens. The manipulation name is
surfaced verbatim as 'ST.ShortText' because RFC 3229 permits
extension manipulations beyond the registered set.

Spec: <https://www.rfc-editor.org/rfc/rfc3229#section-10.5.3>

See also: "Network.HTTP.Headers.IM", "Network.HTTP.Headers.DeltaBase", "Network.HTTP.Headers.AcceptEncoding".
-}
module Network.HTTP.Headers.AIM (
  AIM (..),
  WeightedIM (..),
  aIMParser,
  renderAIM,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAIM)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | An instance-manipulation token with an optional quality weight (defaults to 1.0).
data WeightedIM = WeightedIM
  { imTag :: !ST.ShortText
  , imWeight :: !Double
  }
  deriving stock (Eq, Show)


-- | The full @A-IM@ header value: a (possibly empty) list of weighted manipulations.
newtype AIM = AIM
  { acceptInstanceManipulations :: [WeightedIM]
  }
  deriving stock (Eq, Show)


instance KnownHeader AIM where
  type ParseFailure AIM = String
  type Cardinality AIM = 'ZeroOrOne
  type Direction AIM = 'Request


  parseFromHeaders _ headers = case runParser aIMParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing A-IM header: " <> show rest
    Fail -> Left "Failed to parse A-IM header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAIM


  headerName _ = hAIM


aIMParser :: ParserT st String AIM
aIMParser = AIM <$> (weightedIMParser `sepBy` $(char ','))
  where
    weightedIMParser = do
      ows
      tag <- rfc9110Token
      w <- weightParser
      ows
      pure $ WeightedIM tag w


{- | Parser for the optional @;q=Q@ tail. Same semantics as
"Network.HTTP.Headers.AcceptEncoding"'s weight parser; duplicated to
avoid an import cycle with the content-negotiation parsers.
-}
weightParser :: ParserT st String Double
weightParser = flip (<|>) (pure 1) $ do
  ows
  $(char ';')
  ows
  $(string "q=")
  qValue
  where
    qValue =
      $( switch
          [|
            case _ of
              "0." -> withSpan anyAsciiDecimalWord $ \d (Span (Pos start) (Pos end)) -> do
                let d' = fromIntegral d
                case end - start of
                  1 -> pure $! d' / 10
                  2 -> pure $! d' / 100
                  3 -> pure $! d' / 1000
                  _ -> err "Too many digits after the decimal point in q-value"
              "0" -> pure 0
              "1.000" -> pure 1
              "1.00" -> pure 1
              "1.0" -> pure 1
              "1" -> pure 1
            |]
       )


renderWeightedIM :: WeightedIM -> M.Builder
renderWeightedIM (WeightedIM tag w) =
  shortText tag <> if w == 1 then mempty else ";q=" <> M.doubleDec (realToFrac w)


renderAIM :: AIM -> M.Builder
renderAIM (AIM xs) = M.intersperse ", " (map renderWeightedIM xs)
