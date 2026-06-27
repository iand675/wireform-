{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.PreferenceApplied
Description : Representation of the HTTP @Preference-Applied@ response header field

The @Preference-Applied@ response header field allows a server to inform the
client about which preferences from a 'Network.HTTP.Headers.Prefer.Prefer'
request header were applied (RFC 7240 §3). It mirrors the @Prefer@ grammar: a
comma-separated list of applied preferences, each an extension token with an
optional @=value@ and an optional trailing list of parameters.

@
  Preference-Applied = 1#applied-pref
  applied-pref       = token [ BWS "=" BWS word ]
  word               = token / quoted-string
@

In practice servers echo the token (and value) of each honoured preference.
Each preference's optional value is captured verbatim (including any surrounding
quoting) so values round-trip exactly.

Spec: <https://www.rfc-editor.org/rfc/rfc7240#section-3> (RFC 7240 §3).

See also: "Network.HTTP.Headers.Prefer", "Network.HTTP.Headers.RepeatabilityResult",
"Network.HTTP.Headers.ODataVersion".
-}
module Network.HTTP.Headers.PreferenceApplied (
  PreferenceApplied (..),
  AppliedPreference (..),
  preferenceAppliedParser,
  renderPreferenceApplied,
) where

import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPreferenceApplied)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A @Preference-Applied@ header value: a non-empty list of applied preferences.
newtype PreferenceApplied = PreferenceApplied {appliedPreferences :: NE.NonEmpty AppliedPreference}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


{- | A single applied preference: an extension token (e.g. @respond-async@), an
optional @=value@ captured verbatim (token or quoted-string), and an optional
list of parameters.
-}
data AppliedPreference = AppliedPreference
  { appliedPreferenceName :: ST.ShortText
  , appliedPreferenceValue :: Maybe ST.ShortText
  , appliedPreferenceParameters :: [(ST.ShortText, Maybe ST.ShortText)]
  }
  deriving stock (Eq, Show)


instance KnownHeader PreferenceApplied where
  type ParseFailure PreferenceApplied = String
  type Cardinality PreferenceApplied = 'ZeroOrMore
  type Direction PreferenceApplied = 'Response


  parseFromHeaders _ headers = fold1 <$> traverse runPreferenceAppliedParser headers


  renderToHeaders _ = pure . M.toStrictByteString . renderPreferenceApplied


  headerName _ = hPreferenceApplied


runPreferenceAppliedParser :: B.ByteString -> Either String PreferenceApplied
runPreferenceAppliedParser bs = case runParser preferenceAppliedParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Preference-Applied header: " <> show rest
  Fail -> Left "Failed to parse Preference-Applied header"
  Err e -> Left e


preferenceAppliedParser :: ParserT st String PreferenceApplied
preferenceAppliedParser = do
  first <- appliedPreferenceParser
  rest <- many (ows *> $(char ',') *> ows *> appliedPreferenceParser)
  pure $ PreferenceApplied (first NE.:| rest)


appliedPreferenceParser :: ParserT st String AppliedPreference
appliedPreferenceParser = do
  name <- rfc9110Token
  val <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
  params <- catMaybes <$> many parameterParser
  pure $ AppliedPreference name val params


-- | One iteration of @OWS ";" [ OWS parameter ]@; the parameter itself is optional.
parameterParser :: ParserT st String (Maybe (ST.ShortText, Maybe ST.ShortText))
parameterParser = ows *> $(char ';') *> ows *> optional parameterKV
  where
    parameterKV = do
      pn <- rfc9110Token
      pv <- optional ($(char '=') *> shortASCIIFromParser_ (quotedString <|> rfc9110Token))
      pure (pn, pv)


renderPreferenceApplied :: PreferenceApplied -> M.Builder
renderPreferenceApplied (PreferenceApplied ps) =
  M.intersperse ", " $ map renderAppliedPreference $ NE.toList ps


renderAppliedPreference :: AppliedPreference -> M.Builder
renderAppliedPreference (AppliedPreference name val params) =
  shortText name
    <> maybe mempty (\v -> M.char7 '=' <> shortText v) val
    <> foldMap renderParameter params


renderParameter :: (ST.ShortText, Maybe ST.ShortText) -> M.Builder
renderParameter (pn, pv) =
  M.char7 ';' <> shortText pn <> maybe mempty (\v -> M.char7 '=' <> shortText v) pv
