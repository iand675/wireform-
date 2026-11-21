{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 8288 @Link@ — typed Web Linking. The value is a comma-separated
list of @link-value@s, each a target URI reference enclosed in angle
brackets followed by ordered @link-param@s.

== Grammar (RFC 8288 §3)

@
Link       = #link-value
link-value = \"<\" URI-Reference \">\" *( OWS \";\" OWS link-param )
link-param = token BWS [ \"=\" BWS ( token \/ quoted-string ) ]
@

where @#rule@ is the RFC 9110 §5.6.1 comma-separated list operator.

Each 'LinkValue' carries its target (the bytes between @<@ and @>@)
plus an ordered list of 'LinkParam's; a param may be valueless
(a bare token) or carry a 'LinkParamToken' or 'LinkParamQuoted'
value. The codec retains the target and parameter spellings and renders
canonical separators and quoting. Resolve relative targets and anchors
against the representation URL when using a link.

Spec: <https://www.rfc-editor.org/rfc/rfc8288>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.Sunset".
-}
module Network.HTTP.Headers.Link (
  -- * Types
  Link (..),
  LinkValue (..),
  LinkParam (..),
  LinkParamValue (..),
  LinkRelation (..),

  -- * Parameter semantics
  lookupLinkParam,
  linkRelations,
  linkAnchor,
  linkTitle,
  linkLanguages,
  linkMediaType,

  -- * Parsing
  linkParser,
  linkValuesParser,

  -- * Rendering
  renderLink,
  renderLinkValue,
) where

import qualified Control.Applicative as A
import Control.Monad (foldM)
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Either (isRight)
import Data.Foldable (find)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Network.HTTP.ContentNegotiation (MediaType, restrictedMediaTypeParser, sameNegotiationToken)
import Network.HTTP.Grammar.ExtendedValue (ExtendedValue (..), decodeExtendedValueText, parseExtendedValue)
import Network.HTTP.Grammar.LanguageTag (isLanguageTag)
import Network.HTTP.Grammar.Rendering (foldHttpQuotedShortBytes)
import Network.HTTP.Grammar.URIReference (outboundAbsoluteURIWithFragmentBytes, outboundURIReferenceBytes, uriReference, uriWithFragment, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import Network.HTTP.Headers.HeaderFieldName (hLink)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Wireform.Builder as B
import Wireform.Encode.Direct (dWord8, directEncode)


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | A @link-param@ value: a bare token or a quoted-string.
data LinkParamValue
  = -- | @token@ value, e.g. @rel=next@.
    LinkParamToken !ST.ShortText
  | {- | @quoted-string@ value, e.g. @title=\"Next chapter\"@. The
    contained text maps the unescaped wire bytes one-for-one via Latin-1.
    -}
    LinkParamQuoted !ST.ShortText
  deriving stock (Eq, Show)


-- | A single @link-param@: a name with an optional value.
data LinkParam = LinkParam
  { linkParamName :: !ST.ShortText
  , linkParamValue :: !(Maybe LinkParamValue)
  }
  deriving stock (Eq, Show)


{- | A single @link-value@: the target URI reference (the bytes
between @<@ and @>@) plus its params, in document order.
-}
data LinkValue = LinkValue
  { linkTarget :: !ST.ShortText
  , linkParams :: ![LinkParam]
  }
  deriving stock (Eq, Show)


-- | @Link@ header value: a comma-separated list of link-values.
newtype Link = Link {linkValues :: [LinkValue]}
  deriving stock (Eq, Show)


-- | A registered-form relation name or an absolute extension URI.
data LinkRelation
  = RegisteredRelation !ST.ShortText
  | ExtensionRelation !ST.ShortText
  deriving stock (Show)


instance Eq LinkRelation where
  RegisteredRelation a == RegisteredRelation b = sameNegotiationToken a b
  ExtensionRelation a == ExtensionRelation b = sameNegotiationToken a b
  _ == _ = False


instance KnownHeader Link where
  type ParseFailure Link = String
  type Cardinality Link = 'ZeroOrMore
  type Direction Link = 'RequestAndResponse


  parseFromHeaders _ = fmap Link . parseListFieldLines "Link" linkValuesParser


  renderToHeaders _ = pure . encodeLink . linkValues


  renderToHeadersChecked settings value@(Link values)
    | all validLink values = Just (renderToHeaders settings value)
    | otherwise = Nothing


  headerName _ = hLink


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a whole @Link@ value.
linkParser :: ParserT st String Link
linkParser = Link <$> linkValuesParser


-- | Parse the bare comma-separated @#link-value@ list.
linkValuesParser :: ParserT st String [LinkValue]
linkValuesParser = httpList linkValue


-- | @\"<\" URI-Reference \">\" *( OWS \";\" OWS link-param )@.
linkValue :: ParserT st String LinkValue
linkValue = do
  $(char '<')
  target <- uriReference
  $(char '>')
  params <- many linkParam
  pure LinkValue {linkTarget = target, linkParams = params}
  where
    linkParam = do
      ows
      $(char ';')
      ows
      name <- rfc9110Token
      val <- branch (bws *> $(char '=') *> bws) (Just <$> paramValue) (pure Nothing)
      pure LinkParam {linkParamName = name, linkParamValue = val}
    paramValue = (LinkParamToken <$> rfc9110Token) <|> (LinkParamQuoted . ST.fromText <$> httpQuotedString)


-- | Find the first parameter with the given ASCII case-insensitive name.
lookupLinkParam :: ST.ShortText -> LinkValue -> Maybe LinkParam
lookupLinkParam name = find (sameNegotiationToken name . linkParamName) . linkParams
{-# INLINE lookupLinkParam #-}


-- | Validate the first @rel@ value and return its relation types in order.
linkRelations :: LinkValue -> Either String (NonEmpty LinkRelation)
linkRelations value = do
  rel <- maybe (Left "Link requires a rel value") Right (lookupLinkParam "rel" value >>= parameterText)
  parseFieldLine "Link rel" (relation `NE.sepBy1` skipSome $(char ' ')) (ST.toByteString rel)
  where
    relation = (RegisteredRelation <$> shortASCIIFromParser_ registeredRelationBytes) <|> (ExtensionRelation <$> uriWithFragment)


registeredRelationBytes :: ParserT st String ()
registeredRelationBytes =
  skipSatisfyAscii isLowerAscii
    *> skipMany (skipSatisfyAscii registeredChar)
    <* lookahead ($(char ' ') <|> eof)
  where
    isLowerAscii c = c >= 'a' && c <= 'z'
    registeredChar c = isLowerAscii c || isDigit c || c == '.' || c == '-'
{-# INLINE registeredRelationBytes #-}


-- | Validate the first @anchor@ value when the field supplies one.
linkAnchor :: LinkValue -> Either String (Maybe ST.ShortText)
linkAnchor = traverse parseAnchor . lookupLinkParam "anchor"
  where
    parseAnchor param = do
      value <- requireParameterText "anchor" param
      parseFieldLine "Link anchor" uriReference (ST.toByteString value)


-- | Read the title, preferring its decodable RFC 8187 form.
linkTitle :: LinkValue -> Maybe Text
linkTitle value = extended A.<|> (ST.toText <$> (lookupLinkParam "title" value >>= parameterText))
  where
    extended = do
      spelling <- lookupLinkParam "title*" value >>= parameterText
      decodeExtendedValueText (ST.toByteString spelling)


-- | Validate every @hreflang@ hint in wire order.
linkLanguages :: LinkValue -> Either String [LanguageTag]
linkLanguages = collect . linkParams
  where
    collect [] = Right []
    collect (param : rest)
      | sameNegotiationToken "hreflang" (linkParamName param) = do
          tag <- requireParameterText "hreflang" param
          if isLanguageTag (ST.toByteString tag)
            then (LanguageTag tag :) <$> collect rest
            else Left "Invalid Link hreflang language tag"
      | otherwise = collect rest


-- | Validate the first @type@ hint when the field supplies one.
linkMediaType :: LinkValue -> Either String (Maybe MediaType)
linkMediaType = traverse parseType . lookupLinkParam "type"
  where
    parseType param = do
      value <- requireParameterText "type" param
      parseFieldLine "Link type" restrictedMediaTypeParser (ST.toByteString value)


requireParameterText :: String -> LinkParam -> Either String ST.ShortText
requireParameterText name =
  maybe (Left ("Link " <> name <> " requires a value")) Right . parameterText


parameterText :: LinkParam -> Maybe ST.ShortText
parameterText param = case linkParamValue param of
  Just (LinkParamToken value) -> Just value
  Just (LinkParamQuoted value) -> Just value
  Nothing -> Nothing


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

validLink :: LinkValue -> Bool
validLink (LinkValue target params) =
  validURIText outboundURIReferenceBytes target && validateParams 0 params


validateParams :: Word8 -> [LinkParam] -> Bool
validateParams seen [] = testBit seen 0
validateParams seen (param@(LinkParam name payload) : rest) =
  isHttpTokenText name && maybe True validPayload payload && validateParameter
  where
    validateParameter
      | named "rel" = once 0 (required validRelations)
      | named "anchor" = required (validURIText outboundURIReferenceBytes) && next
      | named "hreflang" = required (isLanguageTag . ST.toByteString) && next
      | named "type" = once 1 (required (isRight . parseFieldLine "Link type" restrictedMediaTypeParser . ST.toByteString))
      | named "title" = once 2 (required (const True))
      | named "title*" = once 3 (validExtendedTitle payload)
      | named "media" = once 4 (required (const True))
      | named "rev" = required validRelations && next
      | otherwise = next
    named key = sameNegotiationToken key name
    required predicate = maybe False predicate (parameterText param)
    next = validateParams seen rest
    once bit valid = not (testBit seen bit) && valid && validateParams (setBit seen bit) rest
    validPayload (LinkParamToken token) = isHttpTokenText token
    validPayload (LinkParamQuoted text) = SBS.all allowed (ST.toShortByteString text)
    allowed byte = byte == 9 || byte >= 32 && byte /= 127


validRelations :: ST.ShortText -> Bool
validRelations = validURIText (relation *> skipMany (skipSome $(char ' ') *> relation))
  where
    relation = registeredRelationBytes <|> outboundAbsoluteURIWithFragmentBytes


validExtendedTitle :: Maybe LinkParamValue -> Bool
validExtendedTitle (Just (LinkParamToken spelling)) =
  case parseExtendedValue (ST.toByteString spelling) of
    Right value ->
      sameNegotiationToken "UTF-8" (extendedCharset value) && isRight (TE.decodeUtf8' (extendedOctets value))
    Left _ -> False
validExtendedTitle _ = False


-- | Render a whole @Link@ value.
renderLink :: Link -> M.Builder
renderLink = renderLinkValues . linkValues


-- | Render one @link-value@.
renderLinkValue :: LinkValue -> M.Builder
renderLinkValue value = renderLinkValues [value]


renderLinkValues :: [LinkValue] -> M.Builder
renderLinkValues [] = mempty
renderLinkValues values = B.primBounded (Prim.boundedPrim (linkSize values) write) values
  where
    write entries ptr = (ptr `plusPtr`) <$> writeLinkBytes entries ptr 0


encodeLink :: [LinkValue] -> ByteString
encodeLink [] = BS.empty
encodeLink values = directEncode (linkSize values) (writeLinkBytes values)


linkSize :: [LinkValue] -> Int
linkSize values = runIdentity (foldLinkBytes countByte countBytes (foldHttpQuotedShortBytes countByte) 0 values)
  where
    countByte n _ = Identity $! BSI.checkedAdd "Link field" n 1
    countBytes n bytes = Identity $! BSI.checkedAdd "Link field" n (SBS.length bytes)


writeLinkBytes :: [LinkValue] -> Ptr Word8 -> Int -> IO Int
writeLinkBytes values ptr start = foldLinkBytes (dWord8 ptr) copy (foldHttpQuotedShortBytes (dWord8 ptr)) start values
  where
    copy offset bytes = do
      let len = SBS.length bytes
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len


-- Counting and writing share one traversal so delimiters and escaping
-- contribute exactly the same bytes to both passes.
foldLinkBytes
  :: Monad m
  => (Int -> Word8 -> m Int)
  -> (Int -> SBS.ShortByteString -> m Int)
  -> (Int -> SBS.ShortByteString -> m Int)
  -> Int
  -> [LinkValue]
  -> m Int
foldLinkBytes byte bytes quoted start = \case
  [] -> pure start
  first : rest -> value start first >>= \next -> foldM separated next rest
  where
    separated offset entry = do
      comma <- byte offset 44
      space <- byte comma 32
      value space entry
    value offset (LinkValue target params) = do
      opened <- byte offset 60
      targetEnd <- bytes opened (ST.toShortByteString target)
      closed <- byte targetEnd 62
      foldM param closed params
    param offset (LinkParam name payload) = do
      semicolon <- byte offset 59
      space <- byte semicolon 32
      nameEnd <- bytes space (ST.toShortByteString name)
      case payload of
        Nothing -> pure nameEnd
        Just (LinkParamToken token) -> byte nameEnd 61 >>= \equals -> bytes equals (ST.toShortByteString token)
        Just (LinkParamQuoted text) -> byte nameEnd 61 >>= \equals -> quoted equals (ST.toShortByteString text)
{-# INLINE foldLinkBytes #-}
