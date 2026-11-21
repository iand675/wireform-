{-# LANGUAGE TemplateHaskell #-}

{- | The extension declaration grammar shared by Man and Opt.
See <https://www.rfc-editor.org/rfc/rfc2774.html#section-3>.
-}
module Network.HTTP.Grammar.ExtensionDeclaration (
  ExtensionDeclaration (..),
  ExtensionIdentifier (..),
  ExtensionParameter (..),
  ExtensionArgument (..),
  ExtensionDeclarations,
  rawDeclarations,
  declarationsText,
  declarationsLines,
  checkedDeclarationsLines,
  decodeDeclarations,
  declarationsEntries,
  declarationsFromEntries,
  declarationsParser,
  renderDeclarations,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar (WireGrammar (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes)
import Network.HTTP.Grammar.URIReference (absoluteURIBytes)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.RetainedList as R


data ExtensionDeclaration = ExtensionDeclaration
  { extensionIdentifier :: !ExtensionIdentifier
  , extensionNamespace :: !(Maybe ByteString)
  -- ^ The decimal header prefix as spelled on the wire, including leading zeroes.
  , extensionParameters :: ![ExtensionParameter]
  }
  deriving stock (Eq, Show)


{- | Field-name identifiers are checked for HTTP token syntax. The
application identifies the extension specification that defines the name.
-}
data ExtensionIdentifier
  = ExtensionURI !ByteString
  | ExtensionFieldName !ByteString
  deriving stock (Eq, Show)


data ExtensionParameter = ExtensionParameter
  { extensionParameterName :: !ByteString
  , extensionParameterArgument :: !(Maybe ExtensionArgument)
  }
  deriving stock (Eq, Show)


data ExtensionArgument
  = ExtensionToken !ByteString
  | -- | Opaque octets with quoted-pair escapes interpreted.
    ExtensionQuoted !ByteString
  deriving stock (Eq, Show)


instance WireGrammar ExtensionDeclaration where
  type GrammarErr ExtensionDeclaration = String


  grammarParser = extensionDeclarationParser


  grammarRender = renderExtensionDeclaration


type ExtensionDeclarations = R.RetainedList ExtensionDeclaration


rawDeclarations :: ST.ShortText -> ExtensionDeclarations
rawDeclarations = R.RawList


declarationsText :: ExtensionDeclarations -> ST.ShortText
declarationsText = R.legacyText


declarationsLines :: ExtensionDeclarations -> NonEmpty ByteString
declarationsLines = R.fieldLines


checkedDeclarationsLines :: String -> ExtensionDeclarations -> Maybe (NonEmpty ByteString)
checkedDeclarationsLines name = R.checkedFieldLines name (httpList extensionDeclarationParser)


decodeDeclarations :: String -> NonEmpty ByteString -> Either String ExtensionDeclarations
decodeDeclarations _ wire@(bytes :| [])
  | Just field <- bareFieldName bytes =
      Right (R.DecodedList wire (ExtensionDeclaration (ExtensionFieldName field) Nothing [] :| []))
decodeDeclarations name wire = R.decodeList name (httpList extensionDeclarationParser) wire


-- A single quoted field name forms a complete declaration. Check the
-- original slice directly and leave whitespace, URIs and lists to the parser.
bareFieldName :: ByteString -> Maybe ByteString
bareFieldName bytes
  | size > 2
  , BSU.unsafeHead bytes == 34
  , BSU.unsafeIndex bytes (size - 1) == 34
  , let body = BSU.unsafeTake (size - 2) (BSU.unsafeDrop 1 bytes)
  , BSC.all isTokenChar body =
      Just body
  | otherwise = Nothing
  where
    size = BS.length bytes
{-# INLINE bareFieldName #-}


declarationsEntries :: String -> ExtensionDeclarations -> Either String (NonEmpty ExtensionDeclaration)
declarationsEntries name = R.listEntries name (httpList extensionDeclarationParser)


declarationsFromEntries :: String -> NonEmpty ExtensionDeclaration -> Either String ExtensionDeclarations
declarationsFromEntries name = R.listFromEntries name (httpList extensionDeclarationParser) renderExtensionDeclaration


declarationsParser :: String -> ParserT st String ExtensionDeclarations
declarationsParser name = R.listParser name (httpList extensionDeclarationParser)


renderDeclarations :: ExtensionDeclarations -> M.Builder
renderDeclarations = R.renderList


extensionDeclarationParser :: ParserT st String ExtensionDeclaration
extensionDeclarationParser = do
  $(char '"')
  identifier <-
    (ExtensionURI <$> byteStringOf absoluteURIBytes <* $(char '"'))
      <|> (ExtensionFieldName <$> rfc9110TokenBS <* $(char '"'))
  branch
    separator
    ( do
        name <- rfc9110TokenBS
        if isNamespace name
          then do
            prefix <- assignment namespace
            ExtensionDeclaration identifier (Just prefix) <$> remainingParameters
          else do
            first <- parameterWithName name
            rest <- remainingParameters
            pure (ExtensionDeclaration identifier Nothing (first : rest))
    )
    (pure (ExtensionDeclaration identifier Nothing []))
  where
    namespace = do
      prefix <- rfc9110TokenBS
      if BS.length prefix >= 2 && BSC.all isDigit prefix
        then pure prefix
        else err "Extension namespace requires at least two decimal digits"
    remainingParameters = many $ do
      separator
      name <- rfc9110TokenBS
      if isNamespace name
        then err "Extension namespace must precede declaration parameters and occur once"
        else parameterWithName name
{-# NOINLINE extensionDeclarationParser #-}


isNamespace :: ByteString -> Bool
isNamespace bytes =
  BS.length bytes == 2
    && (BSU.unsafeIndex bytes 0 .|. 0x20) == 0x6e
    && (BSU.unsafeIndex bytes 1 .|. 0x20) == 0x73
{-# INLINE isNamespace #-}


parameterWithName :: ByteString -> ParserT st String ExtensionParameter
parameterWithName name = do
  value <-
    optional $
      assignment $
        (ExtensionQuoted <$> httpQuotedBytes)
          <|> (ExtensionToken <$> rfc9110TokenBS)
  pure (ExtensionParameter name value)
{-# INLINE parameterWithName #-}


separator :: ParserT st e ()
separator = ows *> $(char ';') *> ows
{-# INLINE separator #-}


assignment :: ParserT st e a -> ParserT st e a
assignment value = ows *> $(char '=') *> ows *> value
{-# INLINE assignment #-}


renderExtensionDeclaration :: ExtensionDeclaration -> M.Builder
renderExtensionDeclaration (ExtensionDeclaration ident namespace params) =
  M.char7 '"'
    <> M.byteString identifier
    <> M.char7 '"'
    <> foldMap (("; ns=" <>) . M.byteString) namespace
    <> foldMap renderParameter params
  where
    identifier = case ident of
      ExtensionURI bytes -> bytes
      ExtensionFieldName bytes -> bytes
    renderParameter (ExtensionParameter name value) =
      "; " <> M.byteString name <> foldMap (("=" <>) . argument) value
    argument (ExtensionToken token) = M.byteString token
    argument (ExtensionQuoted bytes) = M.byteString (encodeHttpQuotedBytes (SBS.toShort bytes))
