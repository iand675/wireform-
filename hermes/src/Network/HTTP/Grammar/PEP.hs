{-# LANGUAGE TemplateHaskell #-}

-- | Extension declarations and policies from the November 1997 PEP draft.
module Network.HTTP.Grammar.PEP (
  PepDeclaration (..),
  PepDeclarationAttribute (..),
  PepPolicy (..),
  PepPolicyAttribute (..),
  PepTarget (..),
  PepStrength (..),
  PepBag (..),
  PepBagItem (..),
  pepDeclarationParser,
  pepPolicyParser,
  renderPepDeclaration,
  renderPepPolicy,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiUpper, toLower)
import Network.HTTP.Grammar (WireGrammar (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes, renderSeparated)
import Network.HTTP.Grammar.URIReference (uriReferenceBytes)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)


data PepDeclaration = PepDeclaration
  { pepExtensionURI :: !ByteString
  , pepHeaderPrefixes :: ![ByteString]
  -- ^ Decimal prefixes with their terminating dash and original spelling.
  , pepDeclarationAttributes :: ![PepDeclarationAttribute]
  }
  deriving stock (Eq, Show)


data PepDeclarationAttribute
  = PepDeclarationStrength !PepStrength
  | PepDeclarationExtension !PepBag
  deriving stock (Eq, Show)


data PepPolicy = PepPolicy
  { pepPolicyURI :: !ByteString
  , pepPolicyAttributes :: ![PepPolicyAttribute]
  }
  deriving stock (Eq, Show)


data PepPolicyAttribute
  = PepFor ![PepTarget]
  | PepMaxAge !Natural
  | PepParameters ![PepBagItem]
  | PepPolicyStrength !PepStrength
  | PepPolicyExtension !PepBag
  deriving stock (Eq, Show)


data PepTarget = PepTarget
  { pepTargetURI :: !ByteString
  , pepTargetIsPrefix :: !Bool
  }
  deriving stock (Eq, Show)


data PepStrength = PepMust | PepMay
  deriving stock (Eq, Show)


data PepBag = PepBag
  { pepBagName :: !ByteString
  , pepBagItems :: ![PepBagItem]
  }
  deriving stock (Eq, Show)


data PepBagItem
  = PepNestedBag !PepBag
  | PepToken !ByteString
  | PepQuoted !ByteString
  deriving stock (Eq, Show)


instance WireGrammar PepDeclaration where
  type GrammarErr PepDeclaration = String


  grammarParser = pepDeclarationParser


  grammarRender = renderPepDeclaration


instance WireGrammar PepPolicy where
  type GrammarErr PepPolicy = String


  grammarParser = pepPolicyParser


  grammarRender = renderPepPolicy


pepDeclarationParser :: ParserT st String PepDeclaration
pepDeclarationParser = braces $ do
  mapping <- braces $ do
    exactName "map"
    uri <- ows *> quotedURI
    PepDeclaration uri <$> httpList headerPrefix <*> pure []
  attrs <- many (ows *> namedBag declarationAttribute)
  pure mapping {pepDeclarationAttributes = attrs}
  where
    declarationAttribute name = case asciiLower name of
      "strength" -> PepDeclarationStrength <$> strength
      "map" -> err "PEP declaration already has a map"
      _ -> PepDeclarationExtension . PepBag name <$> bagItems
{-# NOINLINE pepDeclarationParser #-}


pepPolicyParser :: ParserT st String PepPolicy
pepPolicyParser = braces $ do
  uri <- braces (exactName "id" *> ows *> quotedURI)
  PepPolicy uri <$> many (ows *> namedBag policyAttribute)
  where
    policyAttribute name = case asciiLower name of
      "for" -> PepFor <$> httpList target
      "max-age" -> PepMaxAge <$> deltaSeconds
      "params" -> PepParameters <$> bagItems
      "strength" -> PepPolicyStrength <$> strength
      "id" -> err "PEP policy already has an id"
      _ -> PepPolicyExtension . PepBag name <$> bagItems
    target = PepTarget <$> quotedURI <*> withOption (rws *> $(char '*')) (const (pure True)) (pure False)
{-# NOINLINE pepPolicyParser #-}


braces :: ParserT st e a -> ParserT st e a
braces parser = $(char '{') *> ows *> parser <* ows <* $(char '}')
{-# INLINE braces #-}


namedBag :: (ByteString -> ParserT st String a) -> ParserT st String a
namedBag contents = braces (rfc9110TokenBS >>= \name -> ows *> contents name)
{-# INLINE namedBag #-}


exactName :: ByteString -> ParserT st String ()
exactName expected = do
  name <- rfc9110TokenBS
  if asciiLower name == expected
    then pure ()
    else err "Expected required PEP attribute"
{-# INLINE exactName #-}


asciiLower :: ByteString -> ByteString
asciiLower bytes
  | BSC.any isAsciiUpper bytes = BSC.map toLower bytes
  | otherwise = bytes
{-# INLINE asciiLower #-}


quotedURI :: ParserT st String ByteString
quotedURI = $(char '"') *> byteStringOf uriReferenceBytes <* $(char '"')
-- Share the URI scanner between mapping, policy identifier and realm targets.
{-# NOINLINE quotedURI #-}


headerPrefix :: ParserT st String ByteString
headerPrefix = do
  prefix <- rfc9110TokenBS
  if BS.length prefix > 1 && BS.last prefix == 45 && BSC.all isDigit (BS.init prefix)
    then pure prefix
    else err "PEP header prefix requires decimal digits and a dash"


deltaSeconds :: ParserT st String Natural
deltaSeconds = do
  digits <- byteStringOf (skipSome (skipSatisfyAscii isDigit))
  case BSC.readNatural digits of
    Just (value, _) -> pure value
    Nothing -> failed


strength :: ParserT st String PepStrength
strength = do
  value <- rfc9110TokenBS
  case asciiLower value of
    "must" -> pure PepMust
    "may" -> pure PepMay
    _ -> err "Expected must or may in PEP strength"


bagItems :: ParserT st String [PepBagItem]
bagItems = many (ows *> item)
  where
    item =
      (PepToken <$> rfc9110TokenBS)
        <|> (PepQuoted <$> httpQuotedBytes)
        <|> (PepNestedBag <$> namedBag (\name -> PepBag name <$> bagItems))
{-# NOINLINE bagItems #-}


renderPepDeclaration :: PepDeclaration -> M.Builder
renderPepDeclaration (PepDeclaration uri prefixes attrs) =
  wrap ("{map " <> renderURI uri <> suffix (renderSeparated ", " M.byteString) prefixes <> "}" <> attributes renderAttr attrs)
  where
    renderAttr (PepDeclarationStrength value) = renderStrength value
    renderAttr (PepDeclarationExtension bag) = renderBag bag


renderPepPolicy :: PepPolicy -> M.Builder
renderPepPolicy (PepPolicy uri attrs) =
  wrap ("{id " <> renderURI uri <> "}" <> attributes renderAttr attrs)
  where
    renderAttr (PepFor targets) = wrap ("for" <> suffix (renderSeparated ", " renderTarget) targets)
    renderAttr (PepMaxAge seconds) = wrap ("max-age " <> M.integerDec (toInteger seconds))
    renderAttr (PepParameters items) = wrap ("params" <> attributes renderItem items)
    renderAttr (PepPolicyStrength value) = renderStrength value
    renderAttr (PepPolicyExtension bag) = renderBag bag
    renderTarget (PepTarget uriTarget isPrefix) = renderURI uriTarget <> if isPrefix then " *" else mempty


wrap :: M.Builder -> M.Builder
wrap contents = M.char7 '{' <> contents <> M.char7 '}'


attributes :: (a -> M.Builder) -> [a] -> M.Builder
attributes render = foldMap ((" " <>) . render)


suffix :: ([a] -> M.Builder) -> [a] -> M.Builder
suffix _ [] = mempty
suffix render items = " " <> render items


renderURI :: ByteString -> M.Builder
renderURI uri = M.char7 '"' <> M.byteString uri <> M.char7 '"'


renderStrength :: PepStrength -> M.Builder
renderStrength PepMay = "{strength may}"
renderStrength PepMust = "{strength must}"


renderBag :: PepBag -> M.Builder
renderBag (PepBag name items) = wrap (M.byteString name <> attributes renderItem items)


renderItem :: PepBagItem -> M.Builder
renderItem (PepNestedBag bag) = renderBag bag
renderItem (PepToken bytes) = M.byteString bytes
renderItem (PepQuoted bytes) = M.byteString (encodeHttpQuotedBytes (SBS.toShort bytes))
