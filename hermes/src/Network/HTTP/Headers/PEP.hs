{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @PEP@ declares which extensions have already been applied to an HTTP request or response.

Spec: <https://www.w3.org/TR/WD-http-pep-971121>

See also: "Network.HTTP.Headers.PEPInfo", "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt".
-}
module Network.HTTP.Headers.PEP (
  PEP (PEP, rawPEP),
  pepDeclarations,
  pepFromDeclarations,
  pepParser,
  renderPEP,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.PEP (PepDeclaration, pepDeclarationParser, renderPepDeclaration)
import Network.HTTP.Grammar.Parser (ParserT, httpList)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPEP)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


newtype PEP = PEPList (R.RetainedList PepDeclaration)
  deriving stock (Eq)


-- | Legacy wire spelling. Use 'pepFromDeclarations' for checked authoring.
pattern PEP :: ST.ShortText -> PEP
pattern PEP {rawPEP} <- (fieldText -> rawPEP)
  where
    PEP text = PEPList (R.RawList text)


{-# COMPLETE PEP #-}


instance Show PEP where
  showsPrec precedence (PEP text) =
    showParen (precedence > 10) $ showString "PEP {rawPEP = " . shows text . showChar '}'


fieldText :: PEP -> ST.ShortText
fieldText (PEPList entries) = R.legacyText entries


pepDeclarations :: PEP -> Either String (NE.NonEmpty PepDeclaration)
pepDeclarations (PEPList entries) = R.listEntries "PEP" (httpList pepDeclarationParser) entries


pepFromDeclarations :: NE.NonEmpty PepDeclaration -> Either String PEP
pepFromDeclarations = fmap PEPList . R.listFromEntries "PEP" (httpList pepDeclarationParser) renderPepDeclaration


instance KnownHeader PEP where
  type ParseFailure PEP = String
  type Cardinality PEP = 'ZeroOrMore
  type Direction PEP = 'RequestAndResponse


  parseFromHeaders _ fields = PEPList <$> R.decodeList "PEP" (httpList pepDeclarationParser) fields


  renderToHeaders _ (PEPList entries) = NE.toList (R.fieldLines entries)


  headerName _ = hPEP


pepParser :: ParserT st String PEP
pepParser = PEPList <$> R.listParser "PEP" (httpList pepDeclarationParser)


renderPEP :: PEP -> M.Builder
renderPEP (PEPList entries) = R.renderList entries
