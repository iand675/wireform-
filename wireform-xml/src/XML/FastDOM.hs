{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

{- | Zero-copy XML DOM. All strings are (offset, length) pairs into the
original ByteString. No Text allocation during parse. ~hexml speed.
-}
module XML.FastDOM (
  FastDoc (..),
  FastNode (..),
  FastAttr (..),
  Span (..),
  parseFast,
  nodeTag,
  nodeTagBS,
  attrNameBS,
  attrValueBS,
  attrName,
  attrValue,
  nodeChildren,
  nodeAttrs,
  nodeTextBS,
  nodeText,
  toDocument,
) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (chr, digitToInt, isDigit, isHexDigit, ord)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import Foreign.C.Types (CPtrdiff (..), CUChar (..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import XML.Value (
  Attribute (..),
  Document (..),
  Name (..),
  Node (..),
  qualifiedName,
  simpleName,
 )


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

data Span = Span {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Show, Eq)


instance NFData Span where
  rnf (Span _ _) = ()


data FastNode
  = FElement !Span !(Vector FastAttr) !(Vector FastNode)
  | FText !Span
  | FCData !Span
  | FComment !Span
  | FPI !Span !Span
  deriving stock (Show, Eq)


instance NFData FastNode where
  rnf (FElement s as cs) = rnf s `seq` rnf as `seq` rnf cs
  rnf (FText s) = rnf s
  rnf (FCData s) = rnf s
  rnf (FComment s) = rnf s
  rnf (FPI s1 s2) = rnf s1 `seq` rnf s2


data FastAttr = FastAttr !Span !Span
  deriving stock (Show, Eq)


instance NFData FastAttr where
  rnf (FastAttr n v) = rnf n `seq` rnf v


data FastDoc = FastDoc
  { fdSource :: !ByteString
  , fdRoot :: !FastNode
  }
  deriving stock (Show, Eq)


instance NFData FastDoc where
  rnf (FastDoc bs r) = rnf bs `seq` rnf r


------------------------------------------------------------------------
-- FFI imports
------------------------------------------------------------------------

-- The scanners take and return @CPtrdiff@ (C @ptrdiff_t@): @off@ is the
-- parser position, @len@ the document length, and every scanner reports a
-- miss as @-1@. Both used to be @CInt@, which truncated offsets and
-- lengths on documents longer than @INT_MAX@ bytes. @fromIntegral@ on the
-- way in and out is exact (never negative in, and the result is at most
-- the buffer length on the way out).
foreign import ccall unsafe "hs_xml_find_lt"
  c_find_lt :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff


foreign import ccall unsafe "hs_xml_find_byte"
  c_find_byte :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> CUChar -> IO CPtrdiff


foreign import ccall unsafe "hs_xml_find_attr_end"
  c_find_attr_end :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> CUChar -> IO CPtrdiff


foreign import ccall unsafe "hs_xml_find_cdata_end"
  c_find_cdata_end :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff


foreign import ccall unsafe "hs_xml_find_comment_end"
  c_find_comment_end :: Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff


------------------------------------------------------------------------
-- Inline SIMD wrappers
------------------------------------------------------------------------

findLtP :: Ptr Word8 -> Int -> Int -> IO Int
findLtP ptr off len =
  fromIntegral <$> c_find_lt ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findLtP #-}


findByteP :: Ptr Word8 -> Int -> Int -> Word8 -> IO Int
findByteP ptr off len target =
  fromIntegral <$> c_find_byte ptr (fromIntegral off) (fromIntegral len) (CUChar target)
{-# INLINE findByteP #-}


findAttrEndP :: Ptr Word8 -> Int -> Int -> Word8 -> IO Int
findAttrEndP ptr off len q =
  fromIntegral <$> c_find_attr_end ptr (fromIntegral off) (fromIntegral len) (CUChar q)
{-# INLINE findAttrEndP #-}


findCDataEndP :: Ptr Word8 -> Int -> Int -> IO Int
findCDataEndP ptr off len =
  fromIntegral <$> c_find_cdata_end ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findCDataEndP #-}


findCommentEndP :: Ptr Word8 -> Int -> Int -> IO Int
findCommentEndP ptr off len =
  fromIntegral <$> c_find_comment_end ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findCommentEndP #-}


------------------------------------------------------------------------
-- Zero-copy slicing
------------------------------------------------------------------------

sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs off count
  | count <= 0 = BS.empty
  | otherwise = BSU.unsafeTake count (BSU.unsafeDrop off bs)
{-# INLINE sliceBS #-}


decodeSlice :: ByteString -> Int -> Int -> Text
decodeSlice bs off count = TE.decodeUtf8Lenient (sliceBS bs off count)
{-# INLINE decodeSlice #-}


------------------------------------------------------------------------
-- Accessor functions (decode on demand)
------------------------------------------------------------------------

nodeTag :: FastNode -> ByteString -> Text
nodeTag (FElement (Span off len) _ _) bs = decodeSlice bs off len
nodeTag _ _ = T.empty


nodeTagBS :: FastNode -> ByteString -> ByteString
nodeTagBS (FElement (Span off len) _ _) bs = sliceBS bs off len
nodeTagBS _ _ = BS.empty


attrNameBS :: FastAttr -> ByteString -> ByteString
attrNameBS (FastAttr (Span off len) _) bs = sliceBS bs off len


attrValueBS :: FastAttr -> ByteString -> ByteString
attrValueBS (FastAttr _ (Span off len)) bs = sliceBS bs off len


attrName :: FastAttr -> ByteString -> Text
attrName a bs = TE.decodeUtf8Lenient (attrNameBS a bs)


attrValue :: FastAttr -> ByteString -> Text
attrValue a bs = TE.decodeUtf8Lenient (attrValueBS a bs)


nodeChildren :: FastNode -> Vector FastNode
nodeChildren (FElement _ _ cs) = cs
nodeChildren _ = V.empty


nodeAttrs :: FastNode -> Vector FastAttr
nodeAttrs (FElement _ as _) = as
nodeAttrs _ = V.empty


nodeTextBS :: FastNode -> ByteString -> ByteString
nodeTextBS (FText (Span off len)) bs = sliceBS bs off len
nodeTextBS (FCData (Span off len)) bs = sliceBS bs off len
nodeTextBS _ _ = BS.empty


nodeText :: FastNode -> ByteString -> Text
nodeText n bs = TE.decodeUtf8Lenient (nodeTextBS n bs)


------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------

parseFast :: ByteString -> Either String FastDoc
parseFast bs = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    let !ptr = castPtr cstr :: Ptr Word8
    let !off0 = skipSpaces bs 0 len
    -- Skip XML declaration if present
    off1 <- skipXMLDecl bs ptr off0 len
    let !off2 = skipSpaces bs off1 len
    -- Skip DOCTYPE if present
    let !off3 = skipDoctype bs off2 len
    let !off4 = skipSpaces bs off3 len
    result <- parseNode bs ptr off4 len
    case result of
      Left err -> pure (Left err)
      Right (node, _) -> pure (Right (FastDoc bs node))


skipXMLDecl :: ByteString -> Ptr Word8 -> Int -> Int -> IO Int
skipXMLDecl !bs !_ptr !off !len
  | off + 5 < len
  , BSU.unsafeIndex bs off == 0x3C -- <
  , BSU.unsafeIndex bs (off + 1) == 0x3F -- ?
  , BSU.unsafeIndex bs (off + 2) == 0x78 -- x
  , BSU.unsafeIndex bs (off + 3) == 0x6D -- m
  , BSU.unsafeIndex bs (off + 4) == 0x6C -- l
    =
      do
        -- Find ?>
        let go !i
              | i + 1 >= len = len
              | BSU.unsafeIndex bs i == 0x3F && BSU.unsafeIndex bs (i + 1) == 0x3E = i + 2
              | otherwise = go (i + 1)
        pure (go (off + 5))
  | otherwise = pure off


skipDoctype :: ByteString -> Int -> Int -> Int
skipDoctype !bs !off !len
  | off + 8 < len
  , BSU.unsafeIndex bs off == 0x3C -- <
  , BSU.unsafeIndex bs (off + 1) == 0x21 -- !
  , BSU.unsafeIndex bs (off + 2) == 0x44 -- D
  , BSU.unsafeIndex bs (off + 3) == 0x4F -- O
  , BSU.unsafeIndex bs (off + 4) == 0x43 -- C
  , BSU.unsafeIndex bs (off + 5) == 0x54 -- T
  , BSU.unsafeIndex bs (off + 6) == 0x59 -- Y
  , BSU.unsafeIndex bs (off + 7) == 0x50 -- P
  , BSU.unsafeIndex bs (off + 8) == 0x45 -- E
    =
      go (off + 9) (0 :: Int)
  | otherwise = off
  where
    go !i !depth
      | i >= len = i
      | BSU.unsafeIndex bs i == 0x3E && depth == 0 = i + 1
      | BSU.unsafeIndex bs i == 0x5B = go (i + 1) (depth + 1)
      | BSU.unsafeIndex bs i == 0x5D = go (i + 1) (depth - 1)
      | otherwise = go (i + 1) depth


------------------------------------------------------------------------
-- Core node parser
------------------------------------------------------------------------

parseNode :: ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (FastNode, Int))
parseNode !bs !ptr !off !len
  | off >= len = pure (Left "Unexpected end of input")
  | BSU.unsafeIndex bs off /= 0x3C = do
      -- Text node: find next '<' using SIMD
      endPos <- findLtP ptr off len
      let !textEnd = if endPos < 0 then len else endPos
      if textEnd <= off
        then pure (Left "Empty text node at unexpected position")
        else pure (Right (FText (Span off (textEnd - off)), textEnd))
  | off + 1 >= len = pure (Left "Unexpected end after '<'")
  | otherwise =
      case BSU.unsafeIndex bs (off + 1) of
        0x21 -> parseBang bs ptr off len
        0x3F -> parsePINode bs ptr off len
        _ -> parseElement bs ptr off len


------------------------------------------------------------------------
-- Element parser
------------------------------------------------------------------------

parseElement :: ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (FastNode, Int))
parseElement !bs !ptr !off !len = do
  let !nameStart = off + 1
      !nameEnd = skipNameChars bs nameStart len
      !tagSpan = Span nameStart (nameEnd - nameStart)
  attrResult <- parseAttrs bs ptr nameEnd len
  case attrResult of
    Left err -> pure (Left err)
    Right (attrs, endOff, selfClose) ->
      if selfClose
        then pure (Right (FElement tagSpan attrs V.empty, endOff))
        else do
          childrenResult <- parseChildren bs ptr endOff len nameStart (nameEnd - nameStart)
          case childrenResult of
            Left err -> pure (Left err)
            Right (cs, afterClose) ->
              pure (Right (FElement tagSpan attrs cs, afterClose))


------------------------------------------------------------------------
-- Children parser (collects siblings until end tag)
------------------------------------------------------------------------

parseChildren
  :: ByteString
  -> Ptr Word8
  -> Int
  -> Int
  -> Int
  -> Int
  -> IO (Either String (Vector FastNode, Int))
parseChildren !bs !ptr !off !len !tagOff !tagLen = do
  mvRef <- newIORef =<< MV.unsafeNew 16
  nRef <- newIORef (0 :: Int)
  let loop !i
        | i >= len = pure (Left "Unterminated element")
        | otherwise = do
            -- Check for end tag
            if i + 1 < len && BSU.unsafeIndex bs i == 0x3C && BSU.unsafeIndex bs (i + 1) == 0x2F
              then do
                -- Verify tag name matches
                let !closeNameStart = i + 2
                    !closeNameEnd = skipNameChars bs closeNameStart len
                if closeNameEnd - closeNameStart == tagLen
                  && sliceBS bs closeNameStart (closeNameEnd - closeNameStart) == sliceBS bs tagOff tagLen
                  then do
                    gtPos <- findByteP ptr closeNameEnd len 0x3E
                    if gtPos < 0
                      then pure (Left "Unterminated end tag")
                      else do
                        n <- readIORef nRef
                        mv <- readIORef mvRef
                        v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
                        pure (Right (v, gtPos + 1))
                  else do
                    -- Not our end tag; parse as a node (will error in nested context)
                    result <- parseNode bs ptr i len
                    case result of
                      Left err -> pure (Left err)
                      Right (node, nextOff) -> addChild node nextOff
              else
                if BSU.unsafeIndex bs i == 0x3C
                  then do
                    result <- parseNode bs ptr i len
                    case result of
                      Left err -> pure (Left err)
                      Right (node, nextOff) -> addChild node nextOff
                  else do
                    -- Text content: find '<' using SIMD
                    textEnd <- findLtP ptr i len
                    let !te = if textEnd < 0 then len else textEnd
                    if te > i
                      then addChild (FText (Span i (te - i))) te
                      else loop (i + 1)
        where
          addChild !node !nextOff = do
            n <- readIORef nRef
            mv <- readIORef mvRef
            mv' <-
              if n >= MV.length mv
                then MV.grow mv (MV.length mv)
                else pure mv
            MV.unsafeWrite mv' n node
            writeIORef mvRef mv'
            writeIORef nRef (n + 1)
            loop nextOff
  loop off


------------------------------------------------------------------------
-- Attribute parser
------------------------------------------------------------------------

parseAttrs
  :: ByteString
  -> Ptr Word8
  -> Int
  -> Int
  -> IO (Either String (Vector FastAttr, Int, Bool))
parseAttrs !bs !ptr !off !len = do
  mv0 <- MV.unsafeNew 8
  go mv0 off 0
  where
    go !mv !i !n = do
      let !j = skipSpaces bs i len
      if j >= len
        then pure (Left "Unterminated start tag")
        else do
          let !b = BSU.unsafeIndex bs j
          if b == 0x3E -- '>'
            then do
              v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
              pure (Right (v, j + 1, False))
            else
              if b == 0x2F && j + 1 < len && BSU.unsafeIndex bs (j + 1) == 0x3E -- '/>'
                then do
                  v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
                  pure (Right (v, j + 2, True))
                else do
                  let !nameEnd = skipNameChars bs j len
                      !nameSpan = Span j (nameEnd - j)
                      !eqPos = skipSpaces bs nameEnd len
                  if eqPos >= len || BSU.unsafeIndex bs eqPos /= 0x3D
                    then pure (Left "Expected '=' after attribute name")
                    else do
                      let !afterEq = skipSpaces bs (eqPos + 1) len
                      if afterEq >= len
                        then pure (Left "Unterminated attribute value")
                        else do
                          let !q = BSU.unsafeIndex bs afterEq
                          if q /= 0x22 && q /= 0x27
                            then pure (Left "Expected quote for attribute value")
                            else do
                              let !valStart = afterEq + 1
                              valEnd <- findAttrEndP ptr valStart len q
                              if valEnd < 0
                                then pure (Left "Unterminated attribute value")
                                else do
                                  let !valSpan = Span valStart (valEnd - valStart)
                                      !attr = FastAttr nameSpan valSpan
                                  mv' <-
                                    if n >= MV.length mv
                                      then MV.grow mv (MV.length mv)
                                      else pure mv
                                  MV.unsafeWrite mv' n attr
                                  go mv' (valEnd + 1) (n + 1)


------------------------------------------------------------------------
-- Bang markup (comment, CDATA)
------------------------------------------------------------------------

parseBang :: ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (FastNode, Int))
parseBang !bs !ptr !off !len
  -- Comment: <!--
  | off + 3 < len
  , BSU.unsafeIndex bs (off + 2) == 0x2D -- -
  , BSU.unsafeIndex bs (off + 3) == 0x2D -- -
    =
      do
        let !contentStart = off + 4
        endPos <- findCommentEndP ptr contentStart len
        if endPos < 0
          then pure (Left "Unterminated comment")
          else pure (Right (FComment (Span contentStart (endPos - contentStart)), endPos + 3))
  -- CDATA: <![CDATA[
  | off + 8 < len
  , BSU.unsafeIndex bs (off + 2) == 0x5B -- [
  , BSU.unsafeIndex bs (off + 3) == 0x43 -- C
  , BSU.unsafeIndex bs (off + 4) == 0x44 -- D
  , BSU.unsafeIndex bs (off + 5) == 0x41 -- A
  , BSU.unsafeIndex bs (off + 6) == 0x54 -- T
  , BSU.unsafeIndex bs (off + 7) == 0x41 -- A
  , BSU.unsafeIndex bs (off + 8) == 0x5B -- [
    =
      do
        let !contentStart = off + 9
        endPos <- findCDataEndP ptr contentStart len
        if endPos < 0
          then pure (Left "Unterminated CDATA section")
          else pure (Right (FCData (Span contentStart (endPos - contentStart)), endPos + 3))
  | otherwise = pure (Left $ "Unknown markup at offset " ++ show off)


------------------------------------------------------------------------
-- Processing instruction
------------------------------------------------------------------------

parsePINode :: ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (FastNode, Int))
parsePINode !bs !_ptr !off !len = do
  let !nameStart = off + 2
      !nameEnd = skipNameChars bs nameStart len
      !targetSpan = Span nameStart (nameEnd - nameStart)
  -- Find ?>
  let !dataStart = skipSpaces bs nameEnd len
  let findEnd !i
        | i + 1 >= len = Nothing
        | BSU.unsafeIndex bs i == 0x3F && BSU.unsafeIndex bs (i + 1) == 0x3E = Just i
        | otherwise = findEnd (i + 1)
  case findEnd dataStart of
    Nothing -> pure (Left "Unterminated processing instruction")
    Just endPos -> do
      let !dataSpan = Span dataStart (endPos - dataStart)
      pure (Right (FPI targetSpan dataSpan, endPos + 2))


------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

skipSpaces :: ByteString -> Int -> Int -> Int
skipSpaces !bs !off !len
  | off >= len = off
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in if b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D
           then skipSpaces bs (off + 1) len
           else off


skipNameChars :: ByteString -> Int -> Int -> Int
skipNameChars !bs !off !len = go off
  where
    go !i
      | i >= len = i
      | isNameByte (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = i


isNameByte :: Word8 -> Bool
isNameByte !b =
  (b >= 0x61 && b <= 0x7A)
    || (b >= 0x41 && b <= 0x5A) -- a-z
    || (b >= 0x30 && b <= 0x39) -- A-Z
    || b == 0x3A -- 0-9
    || b == 0x5F
    || b == 0x2D
    || b == 0x2E
    || b >= 0x80 -- : _ - .
{-# INLINE isNameByte #-}


------------------------------------------------------------------------
-- toDocument: full materialization
------------------------------------------------------------------------

toDocument :: FastDoc -> Document
toDocument (FastDoc bs root) = Document Nothing (materializeNode bs root)


materializeNode :: ByteString -> FastNode -> Node
materializeNode !bs = go
  where
    go (FElement nameSpan attrs children) =
      let !name = materializeName bs nameSpan
          !as = V.map (materializeAttr bs) attrs
          !cs = V.map go children
      in Element name as cs
    go (FText (Span off len)) =
      let !raw = decodeSlice bs off len
      in Text (resolveEntitiesText raw)
    go (FCData (Span off len)) =
      CData (decodeSlice bs off len)
    go (FComment (Span off len)) =
      Comment (decodeSlice bs off len)
    go (FPI (Span tOff tLen) (Span dOff dLen)) =
      ProcessingInstruction (decodeSlice bs tOff tLen) (decodeSlice bs dOff dLen)


materializeName :: ByteString -> Span -> Name
materializeName !bs (Span off len) =
  let !raw = decodeSlice bs off len
  in case T.breakOn ":" raw of
       (local, rest)
         | T.null rest -> simpleName local
         | otherwise -> qualifiedName local (T.drop 1 rest)


materializeAttr :: ByteString -> FastAttr -> Attribute
materializeAttr !bs (FastAttr (Span nOff nLen) (Span vOff vLen)) =
  let !rawName = decodeSlice bs nOff nLen
      !rawVal = decodeSlice bs vOff vLen
      !name = case T.breakOn ":" rawName of
        (local, rest)
          | T.null rest -> simpleName local
          | otherwise -> qualifiedName local (T.drop 1 rest)
      !val = resolveEntitiesText rawVal
  in Attribute name val


------------------------------------------------------------------------
-- Entity resolution (for toDocument materialization)
------------------------------------------------------------------------

resolveEntitiesText :: Text -> Text
resolveEntitiesText txt
  | not (T.any (== '&') txt) = txt
  | otherwise = case resolveLoop txt of
      Left _ -> txt
      Right r -> r
  where
    resolveLoop t =
      case T.breakOn "&" t of
        (before, rest)
          | T.null rest -> Right before
          | otherwise ->
              let !afterAmp = T.drop 1 rest
              in case T.breakOn ";" afterAmp of
                   (_, semicRest)
                     | T.null semicRest -> Right (before <> rest)
                     | otherwise ->
                         let !entityName = T.takeWhile (/= ';') afterAmp
                             !remaining = T.drop 1 (T.dropWhile (/= ';') afterAmp)
                         in case resolveEntity entityName of
                              Nothing -> do
                                rest' <- resolveLoop remaining
                                Right (before <> "&" <> entityName <> ";" <> rest')
                              Just replacement -> do
                                rest' <- resolveLoop remaining
                                Right (before <> replacement <> rest')


resolveEntity :: Text -> Maybe Text
resolveEntity "amp" = Just "&"
resolveEntity "lt" = Just "<"
resolveEntity "gt" = Just ">"
resolveEntity "apos" = Just "'"
resolveEntity "quot" = Just "\""
resolveEntity t
  | T.isPrefixOf "#x" t || T.isPrefixOf "#X" t =
      let hex = T.drop 2 t
      in if T.null hex || not (T.all isHexDigit hex)
           then Nothing
           else
             let !n = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 hex
             in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | T.isPrefixOf "#" t =
      let dec = T.drop 1 t
      in if T.null dec || not (T.all isDigit dec)
           then Nothing
           else
             let !n = T.foldl' (\acc c -> acc * 10 + (ord c - ord '0')) 0 dec
             in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | otherwise = Nothing
