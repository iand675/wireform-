{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Zero-copy XML DOM. All strings are (offset, length) pairs into the
original ByteString. No Text allocation during parse. ~hexml speed.
-}
module XML.FastDOM (
  FastDoc (FastDoc, fdSource, fdRoot),
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
import Data.Char (chr)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Read qualified as TR
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


-- Replacement text has different whitespace treatment in attributes (XML 3.3.3).
data EntityExpansion = EntityExpansion
  { entityText :: !Text
  , entityAttribute :: !Text
  }
  deriving stock (Show, Eq)


instance NFData EntityExpansion where
  rnf (EntityExpansion text attribute) = rnf text `seq` rnf attribute


type EntityMap = Map Text EntityExpansion


data FastDoc = FastDocInternal
  { fdSource :: !ByteString
  , fdRoot :: !FastNode
  , fdEntities :: !EntityMap
  }
  deriving stock (Show, Eq)


-- Compatibility constructor for documents without an internal subset.
pattern FastDoc :: ByteString -> FastNode -> FastDoc
pattern FastDoc bs root <- FastDocInternal bs root _
  where
    FastDoc bs root = FastDocInternal bs root Map.empty


{-# COMPLETE FastDoc #-}


instance NFData FastDoc where
  rnf (FastDocInternal bs r entities) = rnf bs `seq` rnf r `seq` rnf entities


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
    -- Keep declaration parsing (and its result allocation) off the common path.
    let parseRoot entities offset = do
          result <- parseNode bs ptr offset len
          pure $ case result of
            Left err -> Left err
            Right (node, _) -> do
              if Map.null entities then Right () else checkEntityReferences bs entities node
              Right (FastDocInternal bs node entities)
    if hasDoctype bs off2 len
      then case readDoctype bs off2 len of
        Left err -> pure (Left err)
        Right (Doctype entities off3) -> parseRoot entities (skipSpaces bs off3 len)
      else parseRoot Map.empty off2


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


-- Internal subsets are uncommon: all declaration work stays off the no-DTD
-- parser path. No external resources are opened by this parser.
data Doctype = Doctype !EntityMap {-# UNPACK #-} !Int


entityLimit :: Int
entityLimit = 1048576


hasDoctype :: ByteString -> Int -> Int -> Bool
hasDoctype bs off len =
  off + 9 <= len
    && at off == 60
    && at (off + 1) == 33
    && at (off + 2) == 68
    && at (off + 3) == 79
    && at (off + 4) == 67
    && at (off + 5) == 84
    && at (off + 6) == 89
    && at (off + 7) == 80
    && at (off + 8) == 69
  where
    at = BSU.unsafeIndex bs
{-# INLINE hasDoctype #-}


readDoctype :: ByteString -> Int -> Int -> Either String Doctype
readDoctype bs off len
  | not (hasDoctype bs off len) = Right (Doctype Map.empty off)
  | otherwise = header (off + 9) Nothing
  where
    at = BSU.unsafeIndex bs
    header i quote
      | i >= len = Left "XML.FastDOM: unterminated DOCTYPE"
      | otherwise = case quote of
          Just q
            | at i == q -> header (i + 1) Nothing
            | otherwise -> header (i + 1) quote
          Nothing
            | at i == 34 || at i == 39 -> header (i + 1) (Just (at i))
            | at i == 62 -> Right (Doctype Map.empty (i + 1))
            | at i == 91 -> subset (i + 1) Map.empty
            | otherwise -> header (i + 1) Nothing
    subset i declarations
      | i >= len = Left "XML.FastDOM: unterminated internal subset"
      | at i == 93 = do
          let end = skipSpaces bs (i + 1) len
          if end >= len || at end /= 62
            then Left "XML.FastDOM: expected DOCTYPE terminator"
            else Doctype <$> resolveDeclarations declarations <*> pure (end + 1)
      | "<!--" `BS.isPrefixOf` BS.drop i bs = do
          let (comment, rest) = BS.breakSubstring "-->" (BS.drop (i + 4) bs)
          if BS.null rest
            then Left "XML.FastDOM: unterminated DTD comment"
            else subset (i + 7 + BS.length comment) declarations
      | "<!ENTITY" `BS.isPrefixOf` BS.drop i bs = do
          let nameStart = skipSpaces bs (i + 8) len
          if nameStart <= i + 8 || nameStart >= len || at nameStart == 37
            then Left "XML.FastDOM: parameter entities are unsupported"
            else do
              let nameEnd = skipNameChars bs nameStart len
                  valueStart = skipSpaces bs nameEnd len
              if nameEnd == nameStart || valueStart <= nameEnd || valueStart >= len || not (at valueStart == 34 || at valueStart == 39)
                then Left "XML.FastDOM: expected an internal quoted entity value"
                else do
                  let q = at valueStart
                      (valueBytes, rest) = BS.break (== q) (BS.drop (valueStart + 1) bs)
                      after = skipSpaces bs (valueStart + 2 + BS.length valueBytes) len
                  if BS.null rest || after >= len || at after /= 62
                    then Left "XML.FastDOM: unterminated entity declaration"
                    else
                      if BS.elem 60 valueBytes || BS.elem 37 valueBytes
                        then Left "XML.FastDOM: markup and parameter references in entity values are unsupported"
                        else do
                          name <- strictText (sliceBS bs nameStart (nameEnd - nameStart))
                          rawValue <- strictText valueBytes
                          value <- cookEntityValue rawValue
                          let next = Map.insertWith (\_ first -> first) name value declarations
                          if Map.size next > 4096
                            then Left "XML.FastDOM: too many entity declarations"
                            else subset (after + 1) next
      | at i == 60 = skipDeclaration (i + 1) Nothing >>= \next -> subset next declarations
      | at i == 37 = Left "XML.FastDOM: parameter entity references are unsupported"
      | otherwise = subset (i + 1) declarations
    skipDeclaration i quote
      | i >= len = Left "XML.FastDOM: unterminated DTD declaration"
      | otherwise = case quote of
          Just q
            | at i == q -> skipDeclaration (i + 1) Nothing
            | otherwise -> skipDeclaration (i + 1) quote
          Nothing
            | at i == 34 || at i == 39 -> skipDeclaration (i + 1) (Just (at i))
            | at i == 62 -> Right (i + 1)
            | otherwise -> skipDeclaration (i + 1) Nothing
    strictText = either (const (Left "XML.FastDOM: invalid UTF-8 in DTD")) Right . TE.decodeUtf8'


-- XML 4.5: numeric references are processed while constructing replacement
-- text; general references are processed when that replacement is included.
-- For example, &#38;lt; becomes <, whereas &amp;lt; stays the literal &lt;.
cookEntityValue :: Text -> Either String Text
cookEntityValue raw = do
  value <- TL.toStrict . TB.toLazyText <$> go (normalizeLines raw)
  if T.any (== '<') value
    then Left "XML.FastDOM: markup in entity replacement text is unsupported"
    else Right value
  where
    go text =
      let (literal, tailText) = T.breakOn "&" text
      in if T.null tailText
           then Right (TB.fromText literal)
           else do
             let (name, semi) = T.breakOn ";" (T.drop 1 tailText)
             if T.null semi
               then Left "XML.FastDOM: unterminated entity value reference"
               else do
                 replacement <-
                   if "#" `T.isPrefixOf` name
                     then maybe (Left "XML.FastDOM: invalid numeric entity reference") Right (resolveEntity name)
                     else Right ("&" <> name <> ";")
                 rest <- go (T.drop 1 semi)
                 Right (TB.fromText literal <> TB.fromText replacement <> rest)


normalizeLines :: Text -> Text
normalizeLines = T.replace "\r" "\n" . T.replace "\r\n" "\n"


normalizeAttributeLiteral :: Text -> Text
normalizeAttributeLiteral text
  | T.any whitespace text = T.map (\c -> if whitespace c then ' ' else c) text
  | otherwise = text
  where
    whitespace c = c == '\t' || c == '\n' || c == '\r'


resolveDeclarations :: Map Text Text -> Either String EntityMap
resolveDeclarations raw = Map.foldlWithKey' step (Right Map.empty) raw
  where
    step result name _ = do
      done <- result
      text <- resolve False Set.empty 0 entityLimit name
      attribute <- resolve True Set.empty 0 entityLimit name
      let used = Map.foldl' (\n value -> n + T.length (entityText value)) (T.length text) done
      if used > entityLimit
        then Left "XML.FastDOM: expanded entity table exceeds limit"
        else Right (Map.insert name (EntityExpansion text attribute) done)
    resolve :: Bool -> Set.Set Text -> Int -> Int -> Text -> Either String Text
    resolve attribute seen depth budget name
      | Set.member name seen = Left "XML.FastDOM: recursive entity declaration"
      | depth >= 32 = Left "XML.FastDOM: entity nesting exceeds limit"
      | otherwise = case Map.lookup name raw of
          Nothing -> Left "XML.FastDOM: undefined entity in declaration"
          Just value -> expandReferences attribute budget (resolve attribute (Set.insert name seen) (depth + 1)) value


-- The callback receives the remaining budget before expansion, so even a
-- compact exponential declaration is rejected before its output is allocated.
expandReferences :: Bool -> Int -> (Int -> Text -> Either String Text) -> Text -> Either String Text
expandReferences attribute limit lookupName input = TL.toStrict . TB.toLazyText <$> go limit input
  where
    go budget text =
      let (rawLiteral, tailText) = T.breakOn "&" text
          literal = if attribute then normalizeAttributeLiteral rawLiteral else rawLiteral
          remainingBudget = budget - T.length literal
      in if remainingBudget < 0
           then Left "XML.FastDOM: entity expansion exceeds limit"
           else
             if T.null tailText
               then Right (TB.fromText literal)
               else do
                 let (name, semi) = T.breakOn ";" (T.drop 1 tailText)
                 if T.null semi
                   then Left "XML.FastDOM: unterminated entity reference"
                   else do
                     replacement <- case resolveEntity name of
                       Just value -> Right value
                       Nothing -> lookupName remainingBudget name
                     let nextBudget = remainingBudget - T.length replacement
                     if nextBudget < 0
                       then Left "XML.FastDOM: entity expansion exceeds limit"
                       else do
                         rest <- go nextBudget (T.drop 1 semi)
                         Right (TB.fromText literal <> TB.fromText replacement <> rest)


checkEntityReferences :: ByteString -> EntityMap -> FastNode -> Either String ()
checkEntityReferences bs entities root = () <$ visit entityLimit root
  where
    visit budget (FElement _ attrs children) = do
      afterAttrs <- V.foldM checkAttr budget attrs
      V.foldM visit afterAttrs children
    visit budget (FText span') = checkSpan False budget span'
    visit budget _ = Right budget
    checkAttr budget (FastAttr _ valueSpan) = checkSpan True budget valueSpan
    checkSpan attribute budget (Span off len) = do
      let raw = normalizeLines (decodeSlice bs off len)
      if T.any (== '&') raw
        then do
          expanded <- expandReferences attribute entityLimit (lookupName attribute) raw
          -- Count materialized output only for spans with entity references.
          let remaining = budget - T.length expanded
          if remaining < 0 then Left "XML.FastDOM: document entity expansion exceeds limit" else Right remaining
        else Right budget
    lookupName attribute _ name = maybe (Left "XML.FastDOM: undefined entity reference") (Right . if attribute then entityAttribute else entityText) (Map.lookup name entities)


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
toDocument (FastDocInternal bs root entities) = Document Nothing (materializeNode entities bs root)


materializeNode :: EntityMap -> ByteString -> FastNode -> Node
materializeNode !entities !bs = go
  where
    go (FElement nameSpan attrs children) =
      let !name = materializeName bs nameSpan
          !as = V.map (materializeAttr entities bs) attrs
          !cs = V.map go children
      in Element name as cs
    go (FText (Span off len)) =
      let !raw = decodeSlice bs off len
      in Text (resolveDocumentEntities False entities raw)
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


materializeAttr :: EntityMap -> ByteString -> FastAttr -> Attribute
materializeAttr !entities !bs (FastAttr (Span nOff nLen) (Span vOff vLen)) =
  let !rawName = decodeSlice bs nOff nLen
      !rawVal = decodeSlice bs vOff vLen
      !name = case T.breakOn ":" rawName of
        (local, rest)
          | T.null rest -> simpleName local
          | otherwise -> qualifiedName local (T.drop 1 rest)
      !val = resolveDocumentEntities True entities rawVal
  in Attribute name val


------------------------------------------------------------------------
-- Entity resolution (for toDocument materialization)
------------------------------------------------------------------------

resolveDocumentEntities :: Bool -> EntityMap -> Text -> Text
resolveDocumentEntities attribute entities input
  | Map.null entities = resolveEntitiesText (if attribute then normalizeAttributeLiteral text else text)
  | otherwise = either (const text) id (expandReferences attribute entityLimit lookupName text)
  where
    text = normalizeLines input
    lookupName _ name = maybe (Left "undefined entity") (Right . if attribute then entityAttribute else entityText) (Map.lookup name entities)


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
  | Just digits <- T.stripPrefix "#x" t = numeric TR.hexadecimal digits
  | Just digits <- T.stripPrefix "#" t = numeric TR.decimal digits
  | otherwise = Nothing
  where
    numeric parser digits = case parser digits :: Either String (Integer, Text) of
      Right (n, rest) | T.null rest && valid n -> Just (T.singleton (chr (fromInteger n)))
      _ -> Nothing
    valid n =
      n == 9
        || n == 10
        || n == 13
        || (n >= 32 && n <= 0xD7FF)
        || (n >= 0xE000 && n <= 0xFFFD)
        || (n >= 0x10000 && n <= 0x10FFFF)
