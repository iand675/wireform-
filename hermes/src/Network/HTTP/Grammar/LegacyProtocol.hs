{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | PICS 1.1 label negotiation and the August 1996 PEP directive grammar.
module Network.HTTP.Grammar.LegacyProtocol (
  ProtocolEntry (..),
  ProtocolRequestEntry (..),
  PicsLabelRequest (..),
  PicsCompleteness (..),
  PicsExtensionValue (..),
  LegacyDirective (..),
  LegacyProtocolIdentifier (..),
  LegacyAttribute (..),
  LegacyScope (..),
  LegacyStrength (..),
  LegacyHeaderPattern (..),
  LegacyTarget (..),
  LegacyItem (..),
  LegacyProtocolMode (..),
  protocolEntryParser,
  protocolRequestEntryParser,
  legacyDirectiveParser,
  legacyProtocolList,
  renderProtocolEntry,
  renderProtocolRequestEntry,
  renderLegacyDirective,
) where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiLower, isAsciiUpper, toLower)
import qualified Data.CharSet.Posix.Ascii as ASCII
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Network.HTTP.Grammar.PICS (picsQuotedName, picsQuotedURL)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes)
import Network.HTTP.Grammar.URIReference (uriReferenceBytes)
import qualified Network.HTTP.Headers.Mason as M


data ProtocolEntry
  = ProtocolPicsLabels
  | ProtocolExtension !LegacyDirective
  deriving stock (Eq, Show)


data ProtocolRequestEntry
  = ProtocolPicsRequest !PicsLabelRequest
  | ProtocolExtensionRequest !LegacyDirective
  deriving stock (Eq, Show)


data PicsLabelRequest = PicsLabelRequest
  { picsCompleteness :: !(Maybe PicsCompleteness)
  , picsRequestExtensions :: ![NonEmpty PicsExtensionValue]
  , picsRatingServices :: !(NonEmpty ByteString)
  }
  deriving stock (Eq, Show)


data PicsCompleteness
  = PicsMinimal
  | PicsShort
  | PicsFull
  | PicsSigned
  | PicsOtherCompleteness !ByteString
  deriving stock (Eq, Show)


-- | Quoted names retain percent escapes as written.
data PicsExtensionValue = PicsToken !ByteString | PicsQuotedName !ByteString
  deriving stock (Eq, Show)


data LegacyDirective = LegacyDirective
  { legacyProtocolIdentifier :: !LegacyProtocolIdentifier
  , legacyProtocolAttributes :: ![LegacyAttribute]
  }
  deriving stock (Eq, Show)


data LegacyProtocolIdentifier
  = LegacyProtocolToken !ByteString
  | LegacyProtocolURI !ByteString
  deriving stock (Eq, Show)


data LegacyAttribute
  = LegacyScope !LegacyScope
  | LegacyStrength !LegacyStrength
  | LegacyHeaders ![LegacyHeaderPattern]
  | LegacyParameters ![LegacyItem]
  | LegacyFor ![LegacyTarget]
  | LegacyVia !LegacyProtocolIdentifier
  | LegacyExtension !LegacyItem
  deriving stock (Eq, Show)


data LegacyScope = LegacyOrigin | LegacyConnection
  deriving stock (Eq, Show)


data LegacyStrength = LegacyOptional | LegacyRequired | LegacyRefused
  deriving stock (Eq, Show)


data LegacyHeaderPattern = LegacyHeaderName !ByteString | LegacyHeaderPrefix !ByteString
  deriving stock (Eq, Show)


data LegacyTarget = LegacyTarget
  { legacyTargetURI :: !ByteString
  , legacyTargetIsPrefix :: !Bool
  }
  deriving stock (Eq, Show)


data LegacyItem
  = LegacyToken !ByteString
  | LegacyQuoted !ByteString
  | LegacyBag !LegacyProtocolIdentifier ![LegacyItem]
  deriving stock (Eq, Show)


data LegacyProtocolMode = LegacyUse | LegacyRequest | LegacyInfo | LegacyQuery


protocolEntryParser :: ParserT st String ProtocolEntry
protocolEntryParser = braces $ do
  ident <- identifier
  if isPics ident
    then ProtocolPicsLabels <$ (ows *> braces (keyword "headers" *> rws *> keyword "pics-label"))
    else ProtocolExtension <$> directiveBody LegacyUse ident


protocolRequestEntryParser :: ParserT st String ProtocolRequestEntry
protocolRequestEntryParser = braces $ do
  ident <- identifier
  if isPics ident
    then ProtocolPicsRequest <$> (ows *> picsRequest)
    else ProtocolExtensionRequest <$> directiveBody LegacyRequest ident


isPics :: LegacyProtocolIdentifier -> Bool
isPics (LegacyProtocolToken token) = lower token == "pics-1.1"
isPics _ = False
{-# INLINE isPics #-}


picsRequest :: ParserT st String PicsLabelRequest
picsRequest = braces $ do
  keyword "params"
  completeness <- optional (rws *> picsCompletenessValue)
  extensions (PicsLabelRequest completeness) []
  where
    extensions request reversed = do
      ows *> $(char '{') *> ows
      first <- picsExtensionValue
      case first of
        PicsToken name | lower name == "services" -> do
          services <- (:|) <$> (rws *> picsQuotedURL) <*> many (rws *> picsQuotedURL)
          ows *> $(char '}')
          pure (request (reverse reversed) services)
        PicsQuotedName "services" -> err "The PICS services name must be a token"
        _ -> do
          rest <- many (rws *> picsExtensionValue)
          ows *> $(char '}')
          extensions request ((first :| rest) : reversed)
{-# NOINLINE picsRequest #-}


picsCompletenessValue :: ParserT st String PicsCompleteness
picsCompletenessValue = do
  token <- picsToken
  case lower token of
    "minimal" -> pure PicsMinimal
    "short" -> pure PicsShort
    "full" -> pure PicsFull
    "signed" -> pure PicsSigned
    _
      | BSC.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) token -> pure (PicsOtherCompleteness token)
      | otherwise -> err "Invalid PICS completeness token"


picsExtensionValue :: ParserT st String PicsExtensionValue
picsExtensionValue = (PicsToken <$> picsToken) <|> (PicsQuotedName <$> picsQuotedName)


picsToken :: ParserT st e ByteString
picsToken = byteStringOf (asciiAlnumSetBytes picsTokenChars)
{-# INLINE picsToken #-}


picsTokenChars :: AsciiSet
picsTokenChars = asciiSet (ASCII.alnum <> "+-")
{-# NOINLINE picsTokenChars #-}


{- | The draft uses comma lists. JEPI also shows several directive bags
separated by whitespace in a single unfolded field.
-}
legacyProtocolList :: ParserT st String a -> ParserT st String [a]
legacyProtocolList item = ows *> entries
  where
    entries = skipMany ($(char ',') *> ows) *> withOption item more (pure [])
    more value =
      (value :)
        <$> branch
          (rws *> lookahead $(char '{'))
          entries
          (ows *> branch $(char ',') (ows *> entries) (pure []))
{-# INLINE legacyProtocolList #-}


legacyDirectiveParser :: LegacyProtocolMode -> ParserT st String LegacyDirective
legacyDirectiveParser mode = braces (identifier >>= directiveBody mode)


directiveBody :: LegacyProtocolMode -> LegacyProtocolIdentifier -> ParserT st String LegacyDirective
directiveBody mode ident = LegacyDirective ident <$> withOption rws (const (attributes 0)) (pure [])
  where
    attributes !seen = withOption (legacyAttribute mode <* ows) (keep seen) (pure [])
    keep seen attr
      | tag .&. seen /= 0 = err "Repeated reserved attribute in PEP directive"
      | otherwise = (attr :) <$> attributes (seen .|. tag)
      where
        tag = attributeTag attr
{-# NOINLINE directiveBody #-}


attributeTag :: LegacyAttribute -> Word8
attributeTag (LegacyScope _) = 1
attributeTag (LegacyStrength _) = 2
attributeTag (LegacyHeaders _) = 4
attributeTag (LegacyParameters _) = 8
attributeTag (LegacyFor _) = 16
attributeTag (LegacyVia _) = 32
attributeTag (LegacyExtension _) = 0
{-# INLINE attributeTag #-}


legacyAttribute :: LegacyProtocolMode -> ParserT st String LegacyAttribute
legacyAttribute mode =
  braces (identifier >>= \name -> withOption rws (const (namedAttribute name)) (namedAttribute name))
    <|> (LegacyExtension <$> legacyWord)
  where
    namedAttribute name@(LegacyProtocolToken token) = case lower token of
      "scope" -> LegacyScope <$> scope
      "str" -> LegacyStrength <$> strength mode
      "headers" -> LegacyHeaders <$> payload headerPattern
      "params" -> LegacyParameters <$> payload legacyItem
      "for" -> LegacyFor <$> payload target
      "via" -> case mode of
        LegacyUse -> LegacyVia <$> identifier
        LegacyInfo -> LegacyVia <$> identifier
        _ -> err "This PEP directive does not allow via"
      _ -> extension name
    namedAttribute name = extension name
    extension name = LegacyExtension . LegacyBag name <$> payload legacyItem
{-# NOINLINE legacyAttribute #-}


scope :: ParserT st String LegacyScope
scope = do
  token <- rfc9110TokenBS
  case lower token of
    "origin" -> pure LegacyOrigin
    "conn" -> pure LegacyConnection
    _ -> err "Expected origin or conn in PEP scope"


strength :: LegacyProtocolMode -> ParserT st String LegacyStrength
strength mode = do
  token <- rfc9110TokenBS
  case lower token of
    "opt" -> pure LegacyOptional
    "req" -> case mode of
      LegacyQuery -> err "PEP queries only allow optional strength"
      _ -> pure LegacyRequired
    "ref" -> case mode of
      LegacyInfo -> pure LegacyRefused
      LegacyRequest -> pure LegacyRefused
      _ -> err "This PEP directive does not allow refused strength"
    _ -> err "Expected opt, req or ref in PEP strength"


headerPattern :: ParserT st String LegacyHeaderPattern
headerPattern = do
  value <- rfc9110TokenBS
  if BS.last value == 42
    then pure (LegacyHeaderPrefix (BS.init value))
    else pure (LegacyHeaderName value)


target :: ParserT st String LegacyTarget
target = do
  uri <- uriWord
  if BS.last uri == 42
    then pure (LegacyTarget (BS.init uri) True)
    else pure (LegacyTarget uri False)


payload :: ParserT st String a -> ParserT st String [a]
payload item = many (item <* ows)
{-# INLINE payload #-}


legacyWord :: ParserT st String LegacyItem
legacyWord = (LegacyToken <$> rfc9110TokenBS) <|> (LegacyQuoted <$> httpQuotedBytes)
{-# INLINE legacyWord #-}


legacyItem :: ParserT st String LegacyItem
legacyItem =
  legacyWord
    <|> braces (LegacyBag <$> identifier <*> withOption rws (const (payload legacyItem)) (pure []))
{-# NOINLINE legacyItem #-}


identifier :: ParserT st String LegacyProtocolIdentifier
identifier =
  (LegacyProtocolToken <$> rfc9110TokenBS <* wordEnd)
    <|> (LegacyProtocolURI <$> uriWord)
{-# NOINLINE identifier #-}


uriWord :: ParserT st String ByteString
uriWord = notFollowedBy wordEnd *> byteStringOf (uriReferenceBytes <* wordEnd)
{-# NOINLINE uriWord #-}


wordEnd :: ParserT st String ()
wordEnd = lookahead (skipSatisfyAscii (\c -> c == '}' || isOWS c))
{-# INLINE wordEnd #-}


keyword :: ByteString -> ParserT st String ()
keyword expected = do
  token <- rfc9110TokenBS
  if lower token == expected then pure () else failed
{-# INLINE keyword #-}


braces :: ParserT st e a -> ParserT st e a
braces parser = $(char '{') *> ows *> parser <* ows <* $(char '}')
{-# INLINE braces #-}


lower :: ByteString -> ByteString
lower bytes
  | BSC.any isAsciiUpper bytes = BSC.map toLower bytes
  | otherwise = bytes
{-# INLINE lower #-}


renderProtocolEntry :: ProtocolEntry -> M.Builder
renderProtocolEntry ProtocolPicsLabels = "{PICS-1.1 {headers PICS-Label}}"
renderProtocolEntry (ProtocolExtension directive) = renderLegacyDirective directive


renderProtocolRequestEntry :: ProtocolRequestEntry -> M.Builder
renderProtocolRequestEntry (ProtocolExtensionRequest directive) = renderLegacyDirective directive
renderProtocolRequestEntry (ProtocolPicsRequest (PicsLabelRequest completeness exts services)) =
  "{PICS-1.1 {params"
    <> foldMap (spaced renderCompleteness) completeness
    <> foldMap (spaced (wrap . renderPicsExtension)) exts
    <> " {services"
    <> foldMap (spaced quoteURI) services
    <> "}}}"
  where
    renderPicsExtension (first :| rest) = renderValue first <> foldMap (spaced renderValue) rest
    renderValue (PicsToken token) = M.byteString token
    renderValue (PicsQuotedName name) = quoteURI name
    renderCompleteness PicsMinimal = "minimal"
    renderCompleteness PicsShort = "short"
    renderCompleteness PicsFull = "full"
    renderCompleteness PicsSigned = "signed"
    renderCompleteness (PicsOtherCompleteness token) = M.byteString token


renderLegacyDirective :: LegacyDirective -> M.Builder
renderLegacyDirective (LegacyDirective ident attrs) =
  wrap (renderIdentifier ident <> foldMap (spaced renderAttribute) attrs)
  where
    renderAttribute (LegacyScope LegacyOrigin) = "{scope origin}"
    renderAttribute (LegacyScope LegacyConnection) = "{scope conn}"
    renderAttribute (LegacyStrength LegacyOptional) = "{str opt}"
    renderAttribute (LegacyStrength LegacyRequired) = "{str req}"
    renderAttribute (LegacyStrength LegacyRefused) = "{str ref}"
    renderAttribute (LegacyHeaders names) = wrap ("headers" <> foldMap (spaced renderPattern) names)
    renderAttribute (LegacyParameters items) = wrap ("params" <> foldMap (spaced renderItem) items)
    renderAttribute (LegacyFor targets) = wrap ("for" <> foldMap (spaced renderTarget) targets)
    renderAttribute (LegacyVia destination) = wrap ("via " <> renderIdentifier destination)
    renderAttribute (LegacyExtension item) = renderItem item
    renderPattern (LegacyHeaderName name) = M.byteString name
    renderPattern (LegacyHeaderPrefix prefix) = M.byteString prefix <> M.char7 '*'
    renderTarget (LegacyTarget uri prefix) = M.byteString uri <> if prefix then M.char7 '*' else mempty


renderIdentifier :: LegacyProtocolIdentifier -> M.Builder
renderIdentifier (LegacyProtocolToken token) = M.byteString token
renderIdentifier (LegacyProtocolURI uri) = M.byteString uri


renderItem :: LegacyItem -> M.Builder
renderItem (LegacyToken token) = M.byteString token
renderItem (LegacyQuoted bytes) = M.byteString (encodeHttpQuotedBytes (SBS.toShort bytes))
renderItem (LegacyBag ident items) = wrap (renderIdentifier ident <> foldMap (spaced renderItem) items)


quoteURI :: ByteString -> M.Builder
quoteURI bytes = M.char7 '"' <> M.byteString bytes <> M.char7 '"'


wrap :: M.Builder -> M.Builder
wrap value = M.char7 '{' <> value <> M.char7 '}'


spaced :: (a -> M.Builder) -> a -> M.Builder
spaced render value = M.char7 ' ' <> render value
