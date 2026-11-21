{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
RFC 4918 §10.4 @If@ request header — a sequence of condition lists. A list
is a conjunction of state tokens and entity-tags. Lists are alternatives,
with an optional resource tag identifying the URI to which they apply.
Tagged and untagged lists cannot be mixed in one header.

The typed decoder validates the condition tree and preserves the field's
wire spelling for forwarding. Use 'ifConditionLists' to inspect the tree
and 'fromIfConditionLists' to render a tree canonically.

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.4>

See also: "Network.HTTP.Headers.LockToken", "Network.HTTP.Headers.IfMatch",
"Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.ETag".
-}
module Network.HTTP.Headers.If (
  If (If, ifConditions),
  IfConditionLists (..),
  IfList (..),
  IfResource (..),
  IfCondition (..),
  IfOperand (..),
  ifConditionLists,
  ifConditionsBytes,
  fromIfConditionLists,
  ifParser,
  renderIf,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Network.HTTP.Grammar.URIReference (absolutePathReference, absolutePathReferenceBytes, absoluteURI, codedURL, outboundAbsoluteURIBytes, validURIText)
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag (EntityTag (StrongETagBytes, WeakETagBytes), entityTagParser, isValidOpaqueTag)
import Network.HTTP.Headers.HeaderFieldName (hIf)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Wireform.Builder as Builder
import Wireform.Encode.Direct (dWord8, directEncode)


-- | A WebDAV condition field, retaining its spelling and decoded alternatives.
data If
  = IfHeader !SBS.ShortByteString (Either String IfConditionLists)
  | IfFromLists !IfConditionLists


{- | The original text interface. For opaque non-UTF-8 validators use
'ifConditionsBytes' or 'ifConditionLists'.
-}
pattern If :: ST.ShortText -> If
pattern If {ifConditions} <- (legacyConditionText -> ifConditions)
  where
    If raw =
      IfHeader
        (ST.toShortByteString raw)
        (parseSingleFieldLine "If" listsParser (ST.toByteString raw :| []))


{-# COMPLETE If #-}


legacyConditionText :: If -> ST.ShortText
legacyConditionText value =
  maybe (ST.fromText (TE.decodeLatin1 (SBS.fromShort bytes))) id (ST.fromShortByteString bytes)
  where
    bytes = ifConditionsBytes value


instance Eq If where
  a == b = ifConditionsBytes a == ifConditionsBytes b


instance Show If where
  showsPrec d (If text) =
    showParen (d > 10) (showString "If " . showsPrec 11 text)


-- | Alternatives for the request URI, or groups scoped to explicit resources.
data IfConditionLists
  = IfUntagged !(NonEmpty IfList)
  | IfTagged !(NonEmpty IfResource)
  deriving stock (Eq, Show)


-- | A nonempty conjunction of conditions.
newtype IfList = IfList {ifListConditions :: NonEmpty IfCondition}
  deriving stock (Eq, Show)


-- | A resource tag applies to every following alternative in this group.
data IfResource = IfResource
  { ifResourceURI :: !ST.ShortText
  , ifResourceLists :: !(NonEmpty IfList)
  }
  deriving stock (Eq, Show)


-- | A condition and its optional @Not@ modifier.
data IfCondition = IfCondition
  { ifNegated :: !Bool
  , ifOperand :: !IfOperand
  }
  deriving stock (Eq, Show)


data IfOperand
  = IfStateToken !ST.ShortText
  | IfEntityTag !EntityTag
  deriving stock (Eq, Show)


{- | Read the condition tree. Values built from raw text with 'If' can
report an invalid legacy value here.
-}
ifConditionLists :: If -> Either String IfConditionLists
ifConditionLists (IfHeader _ lists) = lists
ifConditionLists (IfFromLists lists) = Right lists


-- | Condition bytes, including opaque entity-tag octets.
ifConditionsBytes :: If -> SBS.ShortByteString
ifConditionsBytes (IfHeader raw _) = raw
ifConditionsBytes (IfFromLists lists) = SBS.toShort (encodeLists lists)


-- | Build a field with canonical whitespace from an explicit condition tree.
fromIfConditionLists :: IfConditionLists -> If
fromIfConditionLists = IfFromLists


instance KnownHeader If where
  type ParseFailure If = String
  type Cardinality If = 'ZeroOrOne
  type Direction If = 'Request


  parseFromHeaders _ = parseSingleFieldLine "If" ifParser
  renderToHeaders _ (IfHeader raw _) = SBS.fromShort raw
  renderToHeaders _ (IfFromLists lists) = encodeLists lists
  {-# INLINE renderToHeaders #-}
  renderToHeadersChecked settings value
    | Right lists <- ifConditionLists value
    , validLists lists =
        Just (renderToHeaders settings value)
    | otherwise = Nothing
  headerName _ = hIf


validLists :: IfConditionLists -> Bool
validLists (IfUntagged alternatives) = all validList alternatives
validLists (IfTagged resources) = all validResource resources
  where
    validResource (IfResource uri alternatives) =
      validURIText (outboundAbsoluteURIBytes <|> absolutePathReferenceBytes) uri
        && all validList alternatives


validList :: IfList -> Bool
validList (IfList conditions) = all valid conditions
  where
    valid (IfCondition _ operand) = case operand of
      IfStateToken uri -> validURIText outboundAbsoluteURIBytes uri
      IfEntityTag (StrongETagBytes tag) -> isValidOpaqueTag tag
      IfEntityTag (WeakETagBytes tag) -> isValidOpaqueTag tag


ifParser :: ParserT st String If
ifParser =
  withByteString listsParser $ \lists bytes ->
    pure (IfHeader (SBS.toShort bytes) (Right lists))


listsParser :: ParserT st String IfConditionLists
listsParser =
  (IfUntagged <$> oneOrMore ifListParser)
    <|> (IfTagged <$> oneOrMore resourceParser)
  where
    resourceParser =
      IfResource
        <$> ($(char '<') *> (absoluteURI <|> absolutePathReference) <* $(char '>'))
        <*> (ows *> oneOrMore ifListParser)


oneOrMore :: ParserT st e a -> ParserT st e (NonEmpty a)
oneOrMore item = (:|) <$> item <*> many (ows *> item)
{-# INLINE oneOrMore #-}


ifListParser :: ParserT st String IfList
ifListParser =
  IfList <$> ($(char '(') *> ows *> oneOrMore condition <* ows <* $(char ')'))
  where
    condition =
      IfCondition
        <$> branch ($(switchAsciiCI [|case _ of "not" -> pure ()|]) *> ows) (pure True) (pure False)
        <*> ((IfStateToken <$> codedURL) <|> (IfEntityTag <$> ($(char '[') *> entityTagParser <* $(char ']'))))


renderIf :: If -> M.Builder
renderIf (IfHeader raw _) = M.shortByteString raw
renderIf (IfFromLists lists) =
  Builder.primBounded (Prim.boundedPrim (listsSize lists) write) lists
  where
    write value ptr = (ptr `plusPtr`) <$> writeLists value ptr 0


encodeLists :: IfConditionLists -> BS.ByteString
encodeLists lists = directEncode (listsSize lists) (writeLists lists)


listsSize :: IfConditionLists -> Int
listsSize = \case
  IfUntagged alternatives -> separated listSize alternatives
  IfTagged resources -> separated resourceSize resources
  where
    add = BSI.checkedAdd "WebDAV If"
    separated size (first :| rest) =
      foldl' (\n item -> add (add n 1) (size item)) (size first) rest
    resourceSize (IfResource uri alternatives) =
      add (add 3 (SBS.length (ST.toShortByteString uri))) (separated listSize alternatives)
    listSize (IfList conditions) = add 2 (separated conditionSize conditions)
    conditionSize (IfCondition negated operand) =
      let prefix = if negated then 4 else 0
      in case operand of
           IfStateToken uri -> add (prefix + 2) (SBS.length (ST.toShortByteString uri))
           IfEntityTag (StrongETagBytes tag) -> add (prefix + 4) (SBS.length tag)
           IfEntityTag (WeakETagBytes tag) -> add (prefix + 6) (SBS.length tag)


writeLists :: IfConditionLists -> Ptr Word8 -> Int -> IO Int
writeLists lists ptr start = case lists of
  IfUntagged alternatives -> spaced list start alternatives
  IfTagged resources -> spaced resource start resources
  where
    emitChar offset c = dWord8 ptr offset (fromIntegral (ord c))
    put offset bytes = do
      let len = SBS.length bytes
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len

    spaced :: (Int -> a -> IO Int) -> Int -> NonEmpty a -> IO Int
    spaced item offset (first :| rest) = item offset first >>= go rest
      where
        go [] !end = pure end
        go (entry : more) !at = emitChar at ' ' >>= flip item entry >>= go more
    {-# INLINE spaced #-}

    resource offset (IfResource uri alternatives) = do
      opened <- emitChar offset '<'
      named <- put opened (ST.toShortByteString uri)
      next <- emitChar named '>' >>= \closed -> emitChar closed ' '
      spaced list next alternatives
    list offset (IfList conditions) = do
      opened <- emitChar offset '('
      filled <- spaced condition opened conditions
      emitChar filled ')'
    condition offset (IfCondition negated operand) = do
      next <- if negated then emitChar offset 'N' >>= flip emitChar 'o' >>= flip emitChar 't' >>= flip emitChar ' ' else pure offset
      case operand of
        IfStateToken uri -> emitChar next '<' >>= flip put (ST.toShortByteString uri) >>= flip emitChar '>'
        IfEntityTag (StrongETagBytes tag) -> putTag False next tag
        IfEntityTag (WeakETagBytes tag) -> putTag True next tag
    putTag weak offset tag = do
      opened <- emitChar offset '['
      prefix <- if weak then emitChar opened 'W' >>= flip emitChar '/' else pure opened
      quoted <- emitChar prefix '"'
      filled <- put quoted tag
      closed <- emitChar filled '"'
      emitChar closed ']'
