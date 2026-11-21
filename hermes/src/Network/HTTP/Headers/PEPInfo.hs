{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @PEP-Info@ advertises extension policies that may apply to an HTTP request or response.

Spec: <https://www.w3.org/TR/WD-http-pep-971121>

See also: "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt".
-}
module Network.HTTP.Headers.PEPInfo (
  PEPInfo (PEPInfo, rawPEPInfo),
  pepInfoPolicies,
  pepInfoFromPolicies,
  pepInfoParser,
  renderPEPInfo,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.PEP (PepPolicy, pepPolicyParser, renderPepPolicy)
import Network.HTTP.Grammar.Parser (ParserT, httpList)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPEPInfo)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype PEPInfo = PEPInfoList (R.RetainedList PepPolicy)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'pepInfoFromPolicies' for checked authoring.
pattern PEPInfo :: ST.ShortText -> PEPInfo
pattern PEPInfo {rawPEPInfo} <- (fieldText -> rawPEPInfo)
  where
    PEPInfo text = PEPInfoList (R.RawList text)


{-# COMPLETE PEPInfo #-}


instance Show PEPInfo where
  showsPrec precedence (PEPInfo text) =
    showParen (precedence > 10) $ showString "PEPInfo {rawPEPInfo = " . shows text . showChar '}'


fieldText :: PEPInfo -> ST.ShortText
fieldText (PEPInfoList entries) = R.legacyText entries


pepInfoPolicies :: PEPInfo -> Either String (NE.NonEmpty PepPolicy)
pepInfoPolicies (PEPInfoList entries) = R.listEntries "PEP-Info" (httpList pepPolicyParser) entries


pepInfoFromPolicies :: NE.NonEmpty PepPolicy -> Either String PEPInfo
pepInfoFromPolicies = fmap PEPInfoList . R.listFromEntries "PEP-Info" (httpList pepPolicyParser) renderPepPolicy


instance KnownHeader PEPInfo where
  type ParseFailure PEPInfo = String
  type Cardinality PEPInfo = 'ZeroOrMore
  type Direction PEPInfo = 'RequestAndResponse


  parseFromHeaders _ fields = PEPInfoList <$> R.decodeList "PEP-Info" (httpList pepPolicyParser) fields


  renderToHeaders _ (PEPInfoList entries) = NE.toList (R.fieldLines entries)


  renderToHeadersChecked _ (PEPInfoList entries) = NE.toList <$> R.checkedFieldLines "PEP-Info" (httpList pepPolicyParser) entries


  headerName _ = hPEPInfo


pepInfoParser :: ParserT st String PEPInfo
pepInfoParser = PEPInfoList <$> R.listParser "PEP-Info" (httpList pepPolicyParser)


renderPEPInfo :: PEPInfo -> M.Builder
renderPEPInfo (PEPInfoList entries) = R.renderList entries
