{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@X-UA-Compatible@ is a de-facto response (and @\<meta http-equiv\>@) header,
originally introduced by Microsoft Internet Explorer, used to select the
rendering / document mode of a user agent, e.g. @IE=edge@, @IE=EmulateIE7@, or
@chrome=1@. Its value is a comma-separated list of @engine=mode@ directives,
each side being an RFC 9110 token, surfaced here as a non-empty list of
'UACompatDirective' pairs.

This header is __not__ IANA-registered.

See Microsoft's <https://learn.microsoft.com/en-us/openspecs/ie_standards/ms-iedoco/380e2488-f5eb-4457-a07a-0cb1b6e4b4b5 document mode table>
and <https://learn.microsoft.com/en-us/previous-versions/aa769237(v=vs.85) documentation of comma-delimited modes>.

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.Server".
-}
module Network.HTTP.Headers.XUACompatible (
  XUACompatible (..),
  UACompatDirective (..),
  IEDocumentMode (..),
  ieDocumentMode,
  xUACompatibleParser,
  renderXUACompatible,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (indexWord16Array#, indexWord32Array#, indexWord64Array#, isTrue#, sizeofByteArray#, (<=#), (==#))
import GHC.Word (Word16 (..), Word32 (..), Word64 (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXUACompatible)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderSeparated, shortText)
import Wireform.Encode.Direct (dWord8, directEncode)


-- | A single @engine=mode@ directive, e.g. @IE=edge@.
data UACompatDirective = UACompatDirective
  { uaCompatEngine :: !ST.ShortText
  , uaCompatMode :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | An @X-UA-Compatible@ value: a non-empty list of @engine=mode@ directives.
newtype XUACompatible = XUACompatible {uaCompatDirectives :: NonEmpty UACompatDirective}
  deriving stock (Eq, Show)


{- | A declared Internet Explorer mode. The browser version and document
doctype determine the mode ultimately used for rendering.
-}
data IEDocumentMode
  = IEEdge
  | IEVersion {-# UNPACK #-} !Word
  | IEEmulate {-# UNPACK #-} !Word
  deriving stock (Eq, Show)


-- | Interpret an IE directive without changing its retained spelling.
ieDocumentMode :: UACompatDirective -> Maybe IEDocumentMode
ieDocumentMode (UACompatDirective engine mode)
  | not (matchesIE 32 engineBytes) = Nothing
  | matchesEdge 32 modeBytes = Just IEEdge
  | hasEmulatePrefix modeBytes = readModeVersion IEEmulate 9 modeBytes
  | otherwise = readModeVersion IEVersion 0 modeBytes
  where
    engineBytes = ST.toShortByteString engine
    modeBytes = ST.toShortByteString mode
{-# INLINE ieDocumentMode #-}


-- Check the complete nine-byte prefix before reading either segment.
hasEmulatePrefix :: SBS.ShortByteString -> Bool
hasEmulatePrefix bytes@(SBSI.SBS array) =
  isTrue# (9# <=# sizeofByteArray# array)
    && W64# (indexWord64Array# array 0#) .|. 0x2020_2020_2020_2020 == prefix
    && SBSI.unsafeIndex bytes 8 .|. 32 == 101
  where
    prefix = case targetByteOrder of
      BigEndian -> 0x656d_756c_6174_6569
      LittleEndian -> 0x6965_7461_6c75_6d65
{-# INLINE hasEmulatePrefix #-}


readModeVersion :: (Word -> IEDocumentMode) -> Int -> SBS.ShortByteString -> Maybe IEDocumentMode
readModeVersion make start bytes
  | start >= size = Nothing
  | otherwise = go start 0
  where
    size = SBS.length bytes
    (maxQuotient, maxDigit) = (maxBound :: Word) `quotRem` 10
    go i !n
      | i == size = Just (make n)
      | digit > 9 || n > maxQuotient || (n == maxQuotient && digit > maxDigit) = Nothing
      | otherwise = go (i + 1) (n * 10 + digit)
      where
        digit = fromIntegral (SBSI.unsafeIndex bytes i) - 48
{-# INLINE readModeVersion #-}


instance KnownHeader XUACompatible where
  type ParseFailure XUACompatible = String
  type Cardinality XUACompatible = 'ZeroOrOne
  type Direction XUACompatible = 'Response


  parseFromHeaders _ ("IE=edge" :| []) = Right edgeCompatibility
  parseFromHeaders _ (wire :| []) = parseFieldLine "X-UA-Compatible" xUACompatibleParser wire
  parseFromHeaders _ fields =
    XUACompatible <$> (parseListFieldLines "X-UA-Compatible" (httpList directiveParser) fields >>= requireNonEmptyList "X-UA-Compatible")
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ (XUACompatible (UACompatDirective engine mode :| []))
    | isCanonicalEdge engine mode = "IE=edge"
  renderToHeaders _ (XUACompatible directives) = encodeDirectives directives
  {-# INLINE renderToHeaders #-}


  renderToHeadersChecked _ (XUACompatible (UACompatDirective engine mode :| []))
    | isCanonicalEdge engine mode = Just "IE=edge"
  renderToHeadersChecked _ (XUACompatible directives) = encodeCheckedDirectives directives
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hXUACompatible


xUACompatibleParser :: ParserT st String XUACompatible
xUACompatibleParser = XUACompatible <$> httpList1 "X-UA-Compatible" directiveParser


directiveParser :: ParserT st String UACompatDirective
directiveParser = UACompatDirective <$> rfc9110Token <*> ($(char '=') *> rfc9110Token)


edgeCompatibility :: XUACompatible
edgeCompatibility = XUACompatible (UACompatDirective "IE" "edge" :| [])


isCanonicalEdge :: ST.ShortText -> ST.ShortText -> Bool
isCanonicalEdge engine mode =
  matchesIE 0 (ST.toShortByteString engine) && matchesEdge 0 (ST.toShortByteString mode)
{-# INLINE isCanonicalEdge #-}


-- The length checks bound the word loads. A mask of 32 folds ASCII case.
matchesIE :: Word8 -> SBS.ShortByteString -> Bool
matchesIE mask (SBSI.SBS bytes) =
  isTrue# (sizeofByteArray# bytes ==# 2#)
    && W16# (indexWord16Array# bytes 0#) .|. maskWord == ieWord .|. maskWord
  where
    maskWord = fromIntegral mask * 0x0101
{-# INLINE matchesIE #-}


matchesEdge :: Word8 -> SBS.ShortByteString -> Bool
matchesEdge mask (SBSI.SBS bytes) =
  isTrue# (sizeofByteArray# bytes ==# 4#)
    && W32# (indexWord32Array# bytes 0#) .|. (fromIntegral mask * 0x01010101) == edgeWord
{-# INLINE matchesEdge #-}


ieWord :: Word16
ieWord = case targetByteOrder of
  BigEndian -> 0x4945
  LittleEndian -> 0x4549
{-# INLINE ieWord #-}


edgeWord :: Word32
edgeWord = case targetByteOrder of
  BigEndian -> 0x65646765
  LittleEndian -> 0x65676465
{-# INLINE edgeWord #-}


encodeDirectives :: NonEmpty UACompatDirective -> ByteString
encodeDirectives directives@(first :| rest) =
  encodeDirectivesAtSize size directives
  where
    size = foldl' (\total entry -> addDirectiveSize (addSize total 2) entry) (addDirectiveSize 0 first) rest
-- Keep the fallback's result boxed so the renderer can return the shared
-- ByteString directly on the canonical branch.
{-# OPAQUE encodeDirectives #-}


encodeCheckedDirectives :: NonEmpty UACompatDirective -> Maybe ByteString
encodeCheckedDirectives directives@(first :| rest)
  | valid first = go (addDirectiveSize 0 first) rest
  | otherwise = Nothing
  where
    go !size [] = Just (encodeDirectivesAtSize size directives)
    go !size (entry : more)
      | valid entry = go (addDirectiveSize (addSize size 2) entry) more
      | otherwise = Nothing
    valid (UACompatDirective engine mode) = isHttpTokenText engine && isHttpTokenText mode
-- Keep scans out of callers that inline the cached singleton path.
{-# NOINLINE encodeCheckedDirectives #-}


addDirectiveSize :: Int -> UACompatDirective -> Int
addDirectiveSize total (UACompatDirective engine mode) =
  addSize (addSize (addSize total (textSize engine)) 1) (textSize mode)
  where
    textSize = SBS.length . ST.toShortByteString
{-# INLINE addDirectiveSize #-}


addSize :: Int -> Int -> Int
addSize = BSI.checkedAdd "X-UA-Compatible"
{-# INLINE addSize #-}


encodeDirectivesAtSize :: Int -> NonEmpty UACompatDirective -> ByteString
encodeDirectivesAtSize size (first :| rest) = directEncode size $ \ptr start ->
  writeDirective ptr start first >>= writeRest ptr rest
  where
    writeDirective ptr offset (UACompatDirective engine mode) = do
      engineEnd <- writeText ptr offset engine
      modeStart <- dWord8 ptr engineEnd 61
      writeText ptr modeStart mode
    writeText ptr offset text = do
      let bytes = ST.toShortByteString text
          len = SBS.length bytes
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
      pure $! offset + len
    writeRest _ [] !offset = pure offset
    writeRest ptr (entry : more) !offset = do
      space <- dWord8 ptr offset 44
      start <- dWord8 ptr space 32
      end <- writeDirective ptr start entry
      writeRest ptr more end
{-# INLINE encodeDirectivesAtSize #-}


renderXUACompatible :: XUACompatible -> M.Builder
renderXUACompatible (XUACompatible ds) = renderSeparated ", " renderDirective (NE.toList ds)
  where
    renderDirective (UACompatDirective e m) = shortText e <> M.char7 '=' <> shortText m
