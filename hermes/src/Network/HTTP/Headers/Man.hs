{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Man@ declares end-to-end extensions in an HTTP request or response.
Each declaration identifies an extension used by the message. The sender
uses the extension and requires the ultimate recipient to honor it.

Spec: <https://www.rfc-editor.org/rfc/rfc2774.html#section-4.1>

See also: "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.Ext", "Network.HTTP.Headers.PEP".
-}
module Network.HTTP.Headers.Man (
  Man (Man, rawMan),
  manDeclarations,
  manFromDeclarations,
  manParser,
  renderMan,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.ExtensionDeclaration
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMan)
import qualified Network.HTTP.Headers.Mason as M


newtype Man = ManDeclarations ExtensionDeclarations
  deriving stock (Eq)


-- | The legacy field spelling. Use 'manFromDeclarations' for checked authoring.
pattern Man :: ST.ShortText -> Man
pattern Man {rawMan} <- (legacyText -> rawMan)
  where
    Man text = ManDeclarations (rawDeclarations text)


{-# COMPLETE Man #-}


instance Show Man where
  showsPrec precedence (Man text) =
    showParen (precedence > 10) $ showString "Man {rawMan = " . shows text . showChar '}'


legacyText :: Man -> ST.ShortText
legacyText (ManDeclarations declarations) = declarationsText declarations


manDeclarations :: Man -> Either String (NE.NonEmpty ExtensionDeclaration)
manDeclarations (ManDeclarations declarations) = declarationsEntries "Man" declarations


manFromDeclarations :: NE.NonEmpty ExtensionDeclaration -> Either String Man
manFromDeclarations = fmap ManDeclarations . declarationsFromEntries "Man"


instance KnownHeader Man where
  type ParseFailure Man = String
  type Cardinality Man = 'ZeroOrMore
  type Direction Man = 'RequestAndResponse


  parseFromHeaders _ fields = ManDeclarations <$> decodeDeclarations "Man" fields


  renderToHeaders _ (ManDeclarations declarations) = NE.toList (declarationsLines declarations)


  renderToHeadersChecked _ (ManDeclarations declarations) = NE.toList <$> checkedDeclarationsLines "Man" declarations


  headerName _ = hMan


manParser :: ParserT st String Man
manParser = ManDeclarations <$> declarationsParser "Man"


renderMan :: Man -> M.Builder
renderMan (ManDeclarations declarations) = renderDeclarations declarations
