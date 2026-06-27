{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.Prefer
Description : Representation of the HTTP @Prefer@ request header field

The @Prefer@ request header field is used to indicate that particular server
behaviors are preferred by the client but are not required for successful
completion of the request (RFC 7240). The grammar is a comma-separated list of
preferences, each an extension token with an optional @=value@ and an optional
trailing list of parameters.

@
  Prefer     = 1#preference
  preference = token [ BWS "=" BWS word ] *( OWS ";" [ OWS parameter ] )
  parameter  = token [ BWS "=" BWS word ]
  word       = token / quoted-string
@

Each preference's optional value and each parameter value are captured verbatim
(including any surrounding quoting) rather than fabricating semantics for the
open-ended preference space, so values round-trip exactly.

Spec: <https://www.rfc-editor.org/rfc/rfc7240> (RFC 7240).

See also: "Network.HTTP.Headers.PreferenceApplied", "Network.HTTP.Headers.RepeatabilityRequestID",
"Network.HTTP.Headers.RepeatabilityResult", "Network.HTTP.Headers.ODataIsolation".
-}
module Network.HTTP.Headers.Prefer (
  Prefer (..),
  Preference (..),
  preferParser,
  renderPrefer,
) where

import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPrefer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @Prefer@ header value: a non-empty list of preferences.
newtype Prefer = Prefer {preferPreferences :: NE.NonEmpty Preference}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


{- | A single preference: an extension token (e.g. @respond-async@), an optional
@=value@ captured verbatim (token or quoted-string), and an optional list of
parameters.
-}
data Preference = Preference
  { preferenceName :: ST.ShortText
  , preferenceValue :: Maybe ST.ShortText
  , preferenceParameters :: [(ST.ShortText, Maybe ST.ShortText)]
  }
  deriving stock (Eq, Show)


instance KnownHeader Prefer where
  type ParseFailure Prefer = String
  type Cardinality Prefer = 'ZeroOrMore
  type Direction Prefer = 'Request


  parseFromHeaders _ headers = fold1 <$> traverse runPreferParser headers


  renderToHeaders _ = pure . M.toStrictByteString . renderPrefer


  headerName _ = hPrefer


runPreferParser :: B.ByteString -> Either String Prefer
runPreferParser bs = case runParser preferParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Prefer header: " <> show rest
  Fail -> Left "Failed to parse Prefer header"
  Err e -> Left e


preferParser :: ParserT st String Prefer
preferParser = do
  first <- preferenceParser
  rest <- many (ows *> $(char ',') *> ows *> preferenceParser)
  pure $ Prefer (first NE.:| rest)


preferenceParser :: ParserT st String Preference
preferenceParser = do
  name <- rfc9110Token
  val <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
  params <- catMaybes <$> many parameterParser
  pure $ Preference name val params


-- | One iteration of @OWS ";" [ OWS parameter ]@; the parameter itself is optional.
parameterParser :: ParserT st String (Maybe (ST.ShortText, Maybe ST.ShortText))
parameterParser = ows *> $(char ';') *> ows *> optional parameterKV
  where
    parameterKV = do
      pn <- rfc9110Token
      pv <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
      pure (pn, pv)


renderPrefer :: Prefer -> M.Builder
renderPrefer (Prefer ps) = M.intersperse ", " $ map renderPreference $ NE.toList ps


renderPreference :: Preference -> M.Builder
renderPreference (Preference name val params) =
  shortText name
    <> maybe mempty (\v -> M.char7 '=' <> shortText v) val
    <> foldMap renderParameter params


renderParameter :: (ST.ShortText, Maybe ST.ShortText) -> M.Builder
renderParameter (pn, pv) =
  M.char7 ';' <> shortText pn <> maybe mempty (\v -> M.char7 '=' <> shortText v) pv
