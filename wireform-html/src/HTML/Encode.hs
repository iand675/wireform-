{-# LANGUAGE BangPatterns #-}

{- | HTML5 serializer.

Void elements serialize without closing tag. Boolean attributes
like @checked@ without value serialize as bare attribute names.

Escaping uses SIMD scanning from @cbits\/fast_xml.c@ to bulk-copy
clean byte ranges and only branch on the (rare) escapable bytes.
-}
module HTML.Encode (
  encodeHTML,
  buildNode,
  buildDocument,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable (foldl')
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Foreign.C.Types (CPtrdiff (..))
import Foreign.Ptr (Ptr, castPtr)
import HTML.Value
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Builder qualified as BB


foreign import ccall unsafe "hs_html_find_text_escape"
  c_find_text_escape :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff


foreign import ccall unsafe "hs_html_find_attr_escape"
  c_find_attr_escape :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff


encodeHTML :: HTMLDocument -> ByteString
encodeHTML doc = BL.toStrict (BB.toLazyByteString (buildDocument doc))


buildDocument :: HTMLDocument -> BB.Builder
buildDocument (HTMLDocument mdt root) =
  case mdt of
    Just (Doctype mName _ _) ->
      BB.byteString "<!DOCTYPE "
        <> BB.byteString (TE.encodeUtf8 (maybe "html" id mName))
        <> BB.char7 '>'
        <> BB.char7 '\n'
        <> buildNode root
    Nothing -> buildNode root


buildNode :: HTMLNode -> BB.Builder
buildNode = \case
  HTMLText t -> escapeText (TE.encodeUtf8 t)
  HTMLComment t ->
    BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->"
  HTMLDoctype t _ _ ->
    BB.byteString "<!DOCTYPE " <> BB.byteString (TE.encodeUtf8 t) <> BB.char7 '>'
  HTMLElement tag attrs children
    | isVoidElement tag ->
        BB.char7 '<' <> BB.byteString (TE.encodeUtf8 tag) <> buildAttrs attrs <> BB.char7 '>'
    | sizeofSmallArray children == 0 ->
        BB.char7 '<'
          <> tagBS
          <> buildAttrs attrs
          <> BB.char7 '>'
          <> BB.byteString "</"
          <> tagBS
          <> BB.char7 '>'
    | otherwise ->
        BB.char7 '<'
          <> tagBS
          <> buildAttrs attrs
          <> BB.char7 '>'
          <> foldl' (\acc c -> acc <> buildNode c) mempty children
          <> BB.byteString "</"
          <> tagBS
          <> BB.char7 '>'
    where
      !tagBS = BB.byteString (TE.encodeUtf8 tag)


buildAttrs :: SmallArray HTMLAttribute -> BB.Builder
buildAttrs = foldl' (\acc a -> acc <> buildAttr a) mempty


buildAttr :: HTMLAttribute -> BB.Builder
buildAttr (HTMLAttribute name val)
  | T.null val && isBooleanAttr name =
      BB.char7 ' ' <> BB.byteString (TE.encodeUtf8 name)
  | otherwise =
      BB.char7 ' '
        <> BB.byteString (TE.encodeUtf8 name)
        <> BB.byteString "=\""
        <> escapeAttrValue (TE.encodeUtf8 val)
        <> BB.char7 '"'


{-# INLINE isBooleanAttr #-}
isBooleanAttr :: Text -> Bool
isBooleanAttr t = case t of
  "checked" -> True
  "disabled" -> True
  "readonly" -> True
  "selected" -> True
  "autofocus" -> True
  "autoplay" -> True
  "controls" -> True
  "defer" -> True
  "formnovalidate" -> True
  "hidden" -> True
  "ismap" -> True
  "loop" -> True
  "multiple" -> True
  "muted" -> True
  "nomodule" -> True
  "novalidate" -> True
  "open" -> True
  "required" -> True
  "reversed" -> True
  "allowfullscreen" -> True
  "async" -> True
  "default" -> True
  "inert" -> True
  "itemscope" -> True
  _ -> False


{- | Escape text content: replace <, >, & with entities.
Uses SIMD scanning to bulk-copy clean byte ranges.
-}
escapeText :: ByteString -> BB.Builder
escapeText bs
  | BS.null bs = mempty
  | otherwise = unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        let !ptr = castPtr cstr :: Ptr Word8
        let go !off
              | off >= len = pure mempty
              | otherwise = do
                  CPtrdiff pos <- c_find_text_escape ptr (fromIntegral off) (fromIntegral len)
                  let !absPos = fromIntegral pos
                  if absPos >= len
                    then pure $! BB.byteString (bsDrop off bs)
                    else do
                      let !b = BSU.unsafeIndex bs absPos
                          !clean = bsSlice off (absPos - off) bs
                          !esc = textEscEntity b
                      rest <- go (absPos + 1)
                      pure $! BB.byteString clean <> esc <> rest
        go 0
{-# INLINE escapeText #-}


-- | Escape attribute values: replace ", &, <, > with entities.
escapeAttrValue :: ByteString -> BB.Builder
escapeAttrValue bs
  | BS.null bs = mempty
  | otherwise = unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        let !ptr = castPtr cstr :: Ptr Word8
        let go !off
              | off >= len = pure mempty
              | otherwise = do
                  CPtrdiff pos <- c_find_attr_escape ptr (fromIntegral off) (fromIntegral len)
                  let !absPos = fromIntegral pos
                  if absPos >= len
                    then pure $! BB.byteString (bsDrop off bs)
                    else do
                      let !b = BSU.unsafeIndex bs absPos
                          !clean = bsSlice off (absPos - off) bs
                          !esc = attrEscEntity b
                      rest <- go (absPos + 1)
                      pure $! BB.byteString clean <> esc <> rest
        go 0
{-# INLINE escapeAttrValue #-}


{-# INLINE textEscEntity #-}
textEscEntity :: Word8 -> BB.Builder
textEscEntity 0x3C = BB.byteString "&lt;"
textEscEntity 0x3E = BB.byteString "&gt;"
textEscEntity 0x26 = BB.byteString "&amp;"
textEscEntity b = BB.word8 b


{-# INLINE attrEscEntity #-}
attrEscEntity :: Word8 -> BB.Builder
attrEscEntity 0x22 = BB.byteString "&quot;"
attrEscEntity 0x26 = BB.byteString "&amp;"
attrEscEntity 0x3C = BB.byteString "&lt;"
attrEscEntity 0x3E = BB.byteString "&gt;"
attrEscEntity b = BB.word8 b


{-# INLINE bsSlice #-}
bsSlice :: Int -> Int -> ByteString -> ByteString
bsSlice off n bs = BS.take n (BS.drop off bs)


{-# INLINE bsDrop #-}
bsDrop :: Int -> ByteString -> ByteString
bsDrop = BS.drop
