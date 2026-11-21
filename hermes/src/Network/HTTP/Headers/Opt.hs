{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @Opt@ declares end-to-end extensions in an HTTP request or response.
Each declaration identifies an extension used by the message. The sender
uses the extension and lets the ultimate recipient decide whether to honor it.

Spec: <https://www.rfc-editor.org/rfc/rfc2774.html#section-4.1>

See also: "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Ext", "Network.HTTP.Headers.PEP".
-}
module Network.HTTP.Headers.Opt (
  Opt (Opt, rawOpt),
  optDeclarations,
  optFromDeclarations,
  optParser,
  renderOpt,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.ExtensionDeclaration
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOpt)
import qualified Network.HTTP.Headers.Mason as M


newtype Opt = OptDeclarations ExtensionDeclarations
  deriving stock (Eq)


-- | The legacy field spelling. Use 'optFromDeclarations' for checked authoring.
pattern Opt :: ST.ShortText -> Opt
pattern Opt {rawOpt} <- (legacyText -> rawOpt)
  where
    Opt text = OptDeclarations (rawDeclarations text)


{-# COMPLETE Opt #-}


instance Show Opt where
  showsPrec precedence (Opt text) =
    showParen (precedence > 10) $ showString "Opt {rawOpt = " . shows text . showChar '}'


legacyText :: Opt -> ST.ShortText
legacyText (OptDeclarations declarations) = declarationsText declarations


optDeclarations :: Opt -> Either String (NE.NonEmpty ExtensionDeclaration)
optDeclarations (OptDeclarations declarations) = declarationsEntries "Opt" declarations


optFromDeclarations :: NE.NonEmpty ExtensionDeclaration -> Either String Opt
optFromDeclarations = fmap OptDeclarations . declarationsFromEntries "Opt"


instance KnownHeader Opt where
  type ParseFailure Opt = String
  type Cardinality Opt = 'ZeroOrMore
  type Direction Opt = 'RequestAndResponse


  parseFromHeaders _ fields = OptDeclarations <$> decodeDeclarations "Opt" fields


  renderToHeaders _ (OptDeclarations declarations) = NE.toList (declarationsLines declarations)


  renderToHeadersChecked _ (OptDeclarations declarations) = NE.toList <$> checkedDeclarationsLines "Opt" declarations


  headerName _ = hOpt


optParser :: ParserT st String Opt
optParser = OptDeclarations <$> declarationsParser "Opt"


renderOpt :: Opt -> M.Builder
renderOpt (OptDeclarations declarations) = renderDeclarations declarations
