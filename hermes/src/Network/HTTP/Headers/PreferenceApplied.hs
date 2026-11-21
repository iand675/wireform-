{- |
Module      : Network.HTTP.Headers.PreferenceApplied
Description : Representation of the HTTP @Preference-Applied@ response header field

The @Preference-Applied@ response header field allows a server to inform the
client about which preferences from a 'Network.HTTP.Headers.Prefer.Prefer'
request header were applied (RFC 7240 §3). Each applied preference consists of
an extension token and an optional @=value@.

@
  Preference-Applied = 1#applied-pref
  applied-pref       = token [ BWS "=" BWS word ]
  word               = token / quoted-string
@

In practice servers echo the token (and value) of each honoured preference.
Values retain token or quoted spelling. Quoted values use a one-to-one
Latin-1 mapping for wire octets.

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

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPreferenceApplied)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderHttpWordValue, renderSeparated, shortText)


-- | A @Preference-Applied@ header value: a non-empty list of applied preferences.
newtype PreferenceApplied = PreferenceApplied {appliedPreferences :: NE.NonEmpty AppliedPreference}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


-- | An extension token with an optional value in its wire spelling.
data AppliedPreference = AppliedPreference
  { appliedPreferenceName :: ST.ShortText
  , appliedPreferenceValue :: Maybe ST.ShortText
  , appliedPreferenceParameters :: [(ST.ShortText, Maybe ST.ShortText)]
  -- ^ Legacy constructor field. The RFC 7240 response grammar has no
  -- parameters, so decoded records have an empty list and rendering omits it.
  }
  deriving stock (Eq, Show)


instance KnownHeader PreferenceApplied where
  type ParseFailure PreferenceApplied = String
  type Cardinality PreferenceApplied = 'ZeroOrMore
  type Direction PreferenceApplied = 'Response


  parseFromHeaders _ (line NE.:| []) = parseFieldLine "Preference-Applied" preferenceAppliedParser line
  parseFromHeaders _ fields =
    PreferenceApplied <$> (parseListFieldLines "Preference-Applied" (httpList appliedPreferenceParser) fields >>= requireNonEmptyList "Preference-Applied")


  renderToHeaders _ = pure . M.toStrictByteString . renderPreferenceApplied


  renderToHeadersChecked _ value
    | all validAppliedPreference (appliedPreferences value) = Just [M.toStrictByteString (renderPreferenceApplied value)]
    | otherwise = Nothing


  headerName _ = hPreferenceApplied


validAppliedPreference :: AppliedPreference -> Bool
validAppliedPreference (AppliedPreference name word _) =
  isHttpTokenText name && all isHttpWordValue word
{-# INLINE validAppliedPreference #-}


preferenceAppliedParser :: ParserT st String PreferenceApplied
preferenceAppliedParser = PreferenceApplied <$> httpList1 "Preference-Applied" appliedPreferenceParser


appliedPreferenceParser :: ParserT st String AppliedPreference
appliedPreferenceParser = AppliedPreference <$> rfc9110Token <*> optionalHttpWordValue <*> pure []


renderPreferenceApplied :: PreferenceApplied -> M.Builder
renderPreferenceApplied (PreferenceApplied ps) = renderSeparated ", " renderAppliedPreference (NE.toList ps)


renderAppliedPreference :: AppliedPreference -> M.Builder
renderAppliedPreference (AppliedPreference name val _) =
  shortText name <> foldMap (\v -> M.char7 '=' <> renderHttpWordValue v) val
