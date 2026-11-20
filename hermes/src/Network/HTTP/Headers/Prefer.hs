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

Preference and parameter values retain their token or quoted spelling.
Quoted values represent each wire octet with its Latin-1 code point.
'lookupPreference' applies the first-occurrence rule for preference
names, and 'decodePreferenceValue' gives the unescaped value.

Spec: <https://www.rfc-editor.org/rfc/rfc7240> (RFC 7240).

See also: "Network.HTTP.Headers.PreferenceApplied", "Network.HTTP.Headers.RepeatabilityRequestID",
"Network.HTTP.Headers.RepeatabilityResult", "Network.HTTP.Headers.ODataIsolation".
-}
module Network.HTTP.Headers.Prefer (
  Prefer (..),
  Preference (..),
  lookupPreference,
  decodePreferenceValue,
  preferParser,
  renderPrefer,
) where

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Foldable (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPrefer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderHttpWordValue, renderSeparated, shortText)


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


  parseFromHeaders _ (line NE.:| []) = parseFieldLine "Prefer" preferParser line
  parseFromHeaders _ fields =
    Prefer <$> (parseListFieldLines "Prefer" (httpList preferenceParser) fields >>= requireNonEmptyList "Prefer")


  renderToHeaders _ = pure . M.toStrictByteString . renderPrefer


  headerName _ = hPrefer


-- | Find the first occurrence of a preference name, ignoring ASCII case.
lookupPreference :: ST.ShortText -> Prefer -> Maybe Preference
lookupPreference name = find matches . preferPreferences
  where
    folded = CI.foldCase (ST.toByteString name)
    matches preference =
      let key = preferenceName preference
      in key == name || CI.foldCase (ST.toByteString key) == folded


-- | Decode a retained preference or parameter value. An empty quoted
-- string and an absent value both yield 'Nothing'.
decodePreferenceValue :: Maybe ST.ShortText -> Either String (Maybe B.ByteString)
decodePreferenceValue Nothing = Right Nothing
decodePreferenceValue (Just raw) = do
  let bytes = ST.toByteString raw
      spelling
        | B.isPrefixOf "\"" bytes && B.any (>= 128) bytes = M.toStrictByteString (renderHttpWordValue raw)
        | otherwise = bytes
  value <- parseFieldLine "preference value" (rfc9110TokenBS <|> httpQuotedBytes) spelling
  pure (if B.null value then Nothing else Just value)


preferParser :: ParserT st String Prefer
preferParser = Prefer <$> httpList1 "Prefer" preferenceParser


preferenceParser :: ParserT st String Preference
preferenceParser = do
  name <- rfc9110Token
  value <- optionalHttpWordValue
  Preference name value <$> parameters
  where
    parameters =
      withOption (skipSome (ows *> $(char ';') *> ows)) (const nextParameter) (pure [])
    nextParameter = withOption parameter (\first -> (first :) <$> parameters) (pure [])
    parameter = (,) <$> rfc9110Token <*> optionalHttpWordValue


renderPrefer :: Prefer -> M.Builder
renderPrefer (Prefer ps) = renderSeparated ", " renderPreference (NE.toList ps)


renderPreference :: Preference -> M.Builder
renderPreference (Preference name val params) =
  shortText name
    <> foldMap (\v -> M.char7 '=' <> renderHttpWordValue v) val
    <> foldMap renderParameter params


renderParameter :: (ST.ShortText, Maybe ST.ShortText) -> M.Builder
renderParameter (pn, pv) =
  M.char7 ';' <> shortText pn <> foldMap (\v -> M.char7 '=' <> renderHttpWordValue v) pv
