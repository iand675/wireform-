{-# LANGUAGE BangPatterns #-}
-- | SAX (Simple API for XML) event-based parser.
--
-- Uses SIMD C primitives from @cbits\/fast_xml.c@ for bulk character
-- scanning, giving significant speedups on text-heavy documents.
module XML.SAX
  ( SAXEvent(..)
  , parseSAX
  , parseSAXWith
  , parseSAXStream
  , foldSAX
  , SAXConfig(..)
  , defaultSAXConfig
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, isDigit, isHexDigit, digitToInt, ord)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CUChar(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import XML.Value (Name(..), Attribute(..), XMLDecl(..), simpleName, qualifiedName)

data SAXEvent
  = StartElement !Name !(Vector Attribute)
  | EndElement !Name
  | Characters !Text
  | CDATASection !Text
  | CommentEvent !Text
  | PI !Text !Text
  | StartDocument !(Maybe XMLDecl)
  | EndDocument
  deriving stock (Show, Eq)

instance NFData SAXEvent where
  rnf (StartElement n as) = rnf n `seq` rnf as
  rnf (EndElement n) = rnf n
  rnf (Characters t) = rnf t
  rnf (CDATASection t) = rnf t
  rnf (CommentEvent t) = rnf t
  rnf (PI t1 t2) = rnf t1 `seq` rnf t2
  rnf (StartDocument md) = rnf md
  rnf EndDocument = ()

data SAXConfig = SAXConfig
  { saxMaxEntityExpansion :: !Int
  , saxAllowDTD :: !Bool
  , saxAllowExternalEntities :: !Bool
  }

defaultSAXConfig :: SAXConfig
defaultSAXConfig = SAXConfig
  { saxMaxEntityExpansion = 10000
  , saxAllowDTD = True
  , saxAllowExternalEntities = False
  }

------------------------------------------------------------------------
-- FFI imports for SIMD scanning
------------------------------------------------------------------------

foreign import ccall unsafe "hs_xml_find_byte"
  c_find_byte :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt

foreign import ccall unsafe "hs_xml_find_text_end"
  c_find_text_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_cdata_end"
  c_find_cdata_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_comment_end"
  c_find_comment_end :: Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_xml_find_attr_end"
  c_find_attr_end :: Ptr Word8 -> CInt -> CInt -> CUChar -> IO CInt

-- hs_xml_find_lt available via findTextEndP (which finds both '<' and '&')

------------------------------------------------------------------------
-- Ptr-based scanner wrappers (no per-call unsafePerformIO)
------------------------------------------------------------------------

findByteP :: Ptr Word8 -> Int -> Int -> Word8 -> IO Int
findByteP ptr off len target =
  fromIntegral <$> c_find_byte ptr (fromIntegral off) (fromIntegral len) (CUChar target)
{-# INLINE findByteP #-}

findTextEndP :: Ptr Word8 -> Int -> Int -> IO Int
findTextEndP ptr off len =
  fromIntegral <$> c_find_text_end ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findTextEndP #-}

findCDataEndP :: Ptr Word8 -> Int -> Int -> IO Int
findCDataEndP ptr off len =
  fromIntegral <$> c_find_cdata_end ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findCDataEndP #-}

findCommentEndP :: Ptr Word8 -> Int -> Int -> IO Int
findCommentEndP ptr off len =
  fromIntegral <$> c_find_comment_end ptr (fromIntegral off) (fromIntegral len)
{-# INLINE findCommentEndP #-}

findAttrEndP :: Ptr Word8 -> Int -> Int -> Word8 -> IO Int
findAttrEndP ptr off len q =
  fromIntegral <$> c_find_attr_end ptr (fromIntegral off) (fromIntegral len) (CUChar q)
{-# INLINE findAttrEndP #-}


------------------------------------------------------------------------
-- Parser state — carries the raw Ptr pinned once at parse start
------------------------------------------------------------------------

data PState = PState
  { psBS     :: !ByteString
  , psPtr    :: !(Ptr Word8)
  , psOffset :: !Int
  , psLen    :: !Int
  , psNsStack :: ![(Text, Text)]
  , psEntities :: !(Map Text Text)
  , psEntityBudget :: !(Maybe (IORef Int))
  }

------------------------------------------------------------------------
-- Public entry points
------------------------------------------------------------------------

-- | Parse XML, emitting SAX events. Uses SIMD for character scanning.
-- Accumulates events in a growing mutable vector (no list reverse).
parseSAX :: ByteString -> Either String (Vector SAXEvent)
parseSAX = parseSAXWith defaultSAXConfig

-- | Parse XML with explicit configuration.
parseSAXWith :: SAXConfig -> ByteString -> Either String (Vector SAXEvent)
parseSAXWith cfg bs = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    let !ptr = castPtr cstr :: Ptr Word8
    mvRef <- newIORef =<< MV.unsafeNew 256
    nRef  <- newIORef (0 :: Int)
    let emit !ev = do
          n  <- readIORef nRef
          mv <- readIORef mvRef
          mv' <- if n >= MV.length mv
                   then MV.grow mv (MV.length mv)
                   else pure mv
          MV.unsafeWrite mv' n ev
          writeIORef mvRef mv'
          writeIORef nRef (n + 1)
    result <- parseSAXImplWith cfg bs ptr len emit
    case result of
      Left err -> pure (Left err)
      Right () -> do
        n  <- readIORef nRef
        mv <- readIORef mvRef
        v  <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
        pure (Right v)

-- | Streaming SAX: process events one at a time with a callback.
parseSAXStream :: ByteString -> (SAXEvent -> IO ()) -> IO (Either String ())
parseSAXStream bs emit =
  BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    let !ptr = castPtr cstr :: Ptr Word8
    parseSAXImpl bs ptr len emit

-- | Fold over SAX events (pure).
foldSAX :: (a -> SAXEvent -> a) -> a -> ByteString -> Either String a
foldSAX f z bs = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    let !ptr = castPtr cstr :: Ptr Word8
    ref <- newIORef z
    result <- parseSAXImpl bs ptr len (\ev -> modifyIORef' ref (\acc -> f acc ev))
    case result of
      Left err -> pure (Left err)
      Right () -> Right <$> readIORef ref

------------------------------------------------------------------------
-- Core parser implementation
------------------------------------------------------------------------

parseSAXImpl :: ByteString -> Ptr Word8 -> Int -> (SAXEvent -> IO ()) -> IO (Either String ())
parseSAXImpl = parseSAXImplWith defaultSAXConfig

parseSAXImplWith :: SAXConfig -> ByteString -> Ptr Word8 -> Int -> (SAXEvent -> IO ()) -> IO (Either String ())
parseSAXImplWith cfg bs ptr len emit = do
  entityBudgetRef <- newIORef (saxMaxEntityExpansion cfg)
  let initState = PState bs ptr 0 len [] Map.empty Nothing
  result <- parsePrologWith cfg initState emit
  case result of
    Left err -> pure (Left err)
    Right (st', _mDecl) -> do
      let st'' = st' { psEntityBudget = Just entityBudgetRef }
      contentResult <- parseContent st'' emit []
      case contentResult of
        Left err -> pure (Left err)
        Right (_st, []) -> do
          emit EndDocument
          pure (Right ())
        Right (_st, stack) ->
          pure (Left $ "Unclosed elements: " ++ show (map (T.unpack . nameLocal) stack))

type TagStack = [Name]

parsePrologWith :: SAXConfig -> PState -> (SAXEvent -> IO ()) -> IO (Either String (PState, Maybe XMLDecl))
parsePrologWith !cfg !st emit = do
  let !bs = psBS st
      !ptr = psPtr st
      !len = psLen st
      !off = skipSpaces bs 0 len
  if off + 5 < len &&
     BSU.unsafeIndex bs off == 0x3C &&
     BSU.unsafeIndex bs (off+1) == 0x3F &&
     BSU.unsafeIndex bs (off+2) == 0x78 &&
     BSU.unsafeIndex bs (off+3) == 0x6D &&
     BSU.unsafeIndex bs (off+4) == 0x6C &&
     isSpaceByte (BSU.unsafeIndex bs (off+5))
    then do
      let !nameEnd = off + 5
      result <- parseXMLDeclAttrs bs ptr nameEnd len
      case result of
        Left err -> pure (Left err)
        Right (decl, endOff) -> do
          emit (StartDocument (Just decl))
          let !st' = st { psOffset = skipSpaces bs endOff len }
          dtdResult <- skipDoctypeIfPresentWith cfg st' emit
          case dtdResult of
            Left err -> pure (Left err)
            Right st'' -> pure (Right (st'', Just decl))
    else do
      emit (StartDocument Nothing)
      let !st' = st { psOffset = off }
      dtdResult <- skipDoctypeIfPresentWith cfg st' emit
      case dtdResult of
        Left err -> pure (Left err)
        Right st'' -> pure (Right (st'', Nothing))

skipDoctypeIfPresentWith :: SAXConfig -> PState -> (SAXEvent -> IO ()) -> IO (Either String PState)
skipDoctypeIfPresentWith cfg !st _emit = do
  let !bs = psBS st
      !len = psLen st
      !off = skipSpaces bs (psOffset st) len
  if off + 8 < len &&
     BSU.unsafeIndex bs off == 0x3C &&
     BSU.unsafeIndex bs (off+1) == 0x21 &&
     matchBytes bs (off + 2) [0x44, 0x4F, 0x43, 0x54, 0x59, 0x50, 0x45]
    then do
      if not (saxAllowDTD cfg)
        then pure (Left "DOCTYPE declarations are not allowed (pcAllowDTD=False)")
        else do
          let findBracketOrGt !i
                | i >= len = (i, False)
                | BSU.unsafeIndex bs i == 0x5B = (i, True)
                | BSU.unsafeIndex bs i == 0x3E = (i + 1, False)
                | otherwise = findBracketOrGt (i + 1)
              (!pos, !hasSubset) = findBracketOrGt (off + 9)
          if hasSubset
            then do
              let (entities, endOff) = parseInternalSubset bs (pos + 1) len
                  closeBracket = skipSpaces bs endOff len
                  !finalOff = if closeBracket < len && BSU.unsafeIndex bs closeBracket == 0x5D
                                then let !afterBracket = skipSpaces bs (closeBracket + 1) len
                                     in if afterBracket < len && BSU.unsafeIndex bs afterBracket == 0x3E
                                          then afterBracket + 1
                                          else afterBracket
                                else closeBracket
              pure (Right (st { psOffset = finalOff, psEntities = Map.union entities (psEntities st) }))
            else pure (Right (st { psOffset = pos }))
    else pure (Right (st { psOffset = off }))

parseInternalSubset :: ByteString -> Int -> Int -> (Map Text Text, Int)
parseInternalSubset !bs !off !len = go off Map.empty
  where
    go !i !ents
      | i >= len = (ents, i)
      | BSU.unsafeIndex bs i == 0x5D = (ents, i)
      | i + 8 < len &&
        BSU.unsafeIndex bs i == 0x3C &&
        BSU.unsafeIndex bs (i+1) == 0x21 &&
        matchBytes bs (i + 2) [0x45, 0x4E, 0x54, 0x49, 0x54, 0x59] &&
        isSpaceByte (BSU.unsafeIndex bs (i + 8)) =
          case parseEntityDecl bs (i + 9) len of
            Just (name, val, nextOff) -> go nextOff (Map.insert name val ents)
            Nothing -> skipToGt i ents
      | BSU.unsafeIndex bs i == 0x3C = skipToGt i ents
      | otherwise = go (i + 1) ents
    skipToGt !i !ents =
      let findGt !j
            | j >= len = j
            | BSU.unsafeIndex bs j == 0x3E = j + 1
            | otherwise = findGt (j + 1)
      in go (findGt (i + 1)) ents

parseEntityDecl :: ByteString -> Int -> Int -> Maybe (Text, Text, Int)
parseEntityDecl !bs !off !len =
  let !nameStart = skipSpaces bs off len
      !nameEnd = skipNameChars bs nameStart len
  in if nameEnd <= nameStart
       then Nothing
       else let !name = decodeSlice bs nameStart (nameEnd - nameStart)
                !afterName = skipSpaces bs nameEnd len
            in if afterName >= len
                 then Nothing
                 else let !q = BSU.unsafeIndex bs afterName
                      in if q == 0x22 || q == 0x27
                           then let !valStart = afterName + 1
                                    findQuote !j
                                      | j >= len = Nothing
                                      | BSU.unsafeIndex bs j == q = Just j
                                      | otherwise = findQuote (j + 1)
                                in case findQuote valStart of
                                     Nothing -> Nothing
                                     Just valEnd ->
                                       let !val = decodeSlice bs valStart (valEnd - valStart)
                                           !afterVal = skipSpaces bs (valEnd + 1) len
                                           !nextOff = if afterVal < len && BSU.unsafeIndex bs afterVal == 0x3E
                                                        then afterVal + 1
                                                        else afterVal
                                       in Just (name, val, nextOff)
                           else Nothing

parseXMLDeclAttrs :: ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (XMLDecl, Int))
parseXMLDeclAttrs !bs !ptr !off !len = go off Nothing Nothing Nothing
  where
    go !i !ver !enc !sa = do
      let !j = skipSpaces bs i len
      if j + 1 < len && BSU.unsafeIndex bs j == 0x3F && BSU.unsafeIndex bs (j+1) == 0x3E
        then do
          let !decl = XMLDecl (maybe "1.0" id ver) enc sa
          pure (Right (decl, j + 2))
        else if j >= len
          then pure (Left "Unterminated XML declaration")
          else do
            let !attrNameEnd = skipNameChars bs j len
                !attrName = decodeSlice bs j (attrNameEnd - j)
                !eqPos = skipSpaces bs attrNameEnd len
            if eqPos >= len || BSU.unsafeIndex bs eqPos /= 0x3D
              then pure (Left $ "Expected '=' in XML declaration after " ++ T.unpack attrName)
              else do
                let !afterEq = skipSpaces bs (eqPos + 1) len
                if afterEq >= len
                  then pure (Left "Unterminated XML declaration attribute")
                  else do
                    let !q = BSU.unsafeIndex bs afterEq
                    if q /= 0x22 && q /= 0x27
                      then pure (Left "Expected quote in XML declaration")
                      else do
                        let !valStart = afterEq + 1
                        valEnd <- findAttrEndP ptr valStart len q
                        if valEnd < 0
                          then pure (Left "Unterminated attribute in XML declaration")
                          else do
                            let !val = decodeSlice bs valStart (valEnd - valStart)
                            case attrName of
                              "version"    -> go (valEnd + 1) (Just val) enc sa
                              "encoding"   -> go (valEnd + 1) ver (Just val) sa
                              "standalone" -> go (valEnd + 1) ver enc (Just (val == "yes"))
                              _ -> go (valEnd + 1) ver enc sa

------------------------------------------------------------------------
-- Content parsing
------------------------------------------------------------------------

parseContent :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseContent !st emit !stack
  | psOffset st >= psLen st = pure (Right (st, stack))
  | otherwise = do
      let !off = psOffset st
          !bs = psBS st
      case BSU.unsafeIndex bs off of
        0x3C -> do
          if off + 1 >= psLen st
            then pure (Left "Unexpected end of input after '<'")
            else case BSU.unsafeIndex bs (off + 1) of
              0x2F -> parseEndTag st emit stack
              0x21 -> parseBangMarkup st emit stack
              0x3F -> parsePITag st emit stack
              _    -> parseStartTag st emit stack
        _ -> parseTextContent st emit stack

------------------------------------------------------------------------
-- Text content with fast path (Fix 5 + Fix 7: hs_xml_find_lt + no-entity fast path)
------------------------------------------------------------------------

parseTextContent :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseTextContent !st emit !stack = do
  let !off = psOffset st
      !bs  = psBS st
      !ptr = psPtr st
      !len = psLen st
      !ents = psEntities st
  result <- collectText ents (psEntityBudget st) bs ptr off len
  case result of
    Left err -> pure (Left err)
    Right (txt, newOff) -> do
      when (not (T.null txt)) $
        emit (Characters txt)
      parseContent (st { psOffset = newOff }) emit stack

collectText :: Map Text Text -> Maybe (IORef Int) -> ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String (Text, Int))
collectText !ents !budgetRef !bs !ptr !off !len
  | off >= len = pure (Right (T.empty, off))
  | BSU.unsafeIndex bs off == 0x3C = pure (Right (T.empty, off))
  | otherwise = do
      endPos <- findTextEndP ptr off len
      if endPos >= len || BSU.unsafeIndex bs endPos == 0x3C
        then do
          let !txt = decodeSlice bs off (endPos - off)
          pure (Right (txt, endPos))
        else
          goEntities off []
  where
    goEntities !i !acc
      | i >= len =
          pure (Right (T.concat (reverse acc), i))
      | BSU.unsafeIndex bs i == 0x3C =
          pure (Right (T.concat (reverse acc), i))
      | BSU.unsafeIndex bs i == 0x26 = do
          semicPos <- findByteP ptr (i + 1) len 0x3B
          if semicPos < 0
            then pure (Left "Unterminated entity reference")
            else do
              let !entityName = decodeSlice bs (i + 1) (semicPos - i - 1)
              case resolveEntityWith ents entityName of
                Nothing -> pure (Left $ "Unknown entity: &" ++ T.unpack entityName ++ ";")
                Just replacement -> do
                  budgetOk <- checkEntityBudget budgetRef (T.length replacement)
                  if not budgetOk
                    then pure (Left "Entity expansion limit exceeded")
                    else goEntities (semicPos + 1) (replacement : acc)
      | otherwise = do
          endPos <- findTextEndP ptr i len
          let !chunk = decodeSlice bs i (endPos - i)
          goEntities endPos (chunk : acc)

checkEntityBudget :: Maybe (IORef Int) -> Int -> IO Bool
checkEntityBudget Nothing _ = pure True
checkEntityBudget (Just ref) cost = do
  remaining <- readIORef ref
  let !remaining' = remaining - cost
  if remaining' < 0
    then pure False
    else do
      writeIORef ref remaining'
      pure True

------------------------------------------------------------------------
-- Markup parsing
------------------------------------------------------------------------

parseBangMarkup :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseBangMarkup !st emit !stack = do
  let !off = psOffset st
      !bs = psBS st
      !len = psLen st
  if off + 3 >= len
    then pure (Left "Unexpected end of input in markup")
    else if off + 3 < len && matchBytes bs (off + 2) [0x2D, 0x2D]
      then parseCommentTag st emit stack
      else if off + 9 <= len && matchBytes bs (off + 2) [0x5B, 0x43, 0x44, 0x41, 0x54, 0x41, 0x5B]
        then parseCDataTag st emit stack
        else if off + 8 <= len && matchBytes bs (off + 2) [0x44, 0x4F, 0x43, 0x54, 0x59, 0x50, 0x45]
          then skipDoctypeTag st emit stack
          else pure (Left $ "Unknown markup at offset " ++ show off)

parseCommentTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseCommentTag !st emit !stack = do
  let !off = psOffset st
      !bs  = psBS st
      !ptr = psPtr st
      !len = psLen st
      !startContent = off + 4
  endPos <- findCommentEndP ptr startContent len
  if endPos < 0
    then pure (Left "Unterminated comment")
    else do
      let !txt = decodeSlice bs startContent (endPos - startContent)
      emit (CommentEvent txt)
      parseContent (st { psOffset = endPos + 3 }) emit stack

parseCDataTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseCDataTag !st emit !stack = do
  let !off = psOffset st
      !bs  = psBS st
      !ptr = psPtr st
      !len = psLen st
      !startContent = off + 9
  endPos <- findCDataEndP ptr startContent len
  if endPos < 0
    then pure (Left "Unterminated CDATA section")
    else do
      let !txt = decodeSlice bs startContent (endPos - startContent)
      emit (CDATASection txt)
      parseContent (st { psOffset = endPos + 3 }) emit stack

skipDoctypeTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
skipDoctypeTag !st emit !stack = do
  let !off = psOffset st
      !bs = psBS st
      !len = psLen st
      go !i !depth
        | i >= len = Left "Unterminated DOCTYPE"
        | BSU.unsafeIndex bs i == 0x3E && depth == 0 = Right (i + 1)
        | BSU.unsafeIndex bs i == 0x5B = go (i + 1) (depth + 1 :: Int)
        | BSU.unsafeIndex bs i == 0x5D = go (i + 1) (depth - 1)
        | otherwise = go (i + 1) depth
  case go (off + 2) 0 of
    Left err -> pure (Left err)
    Right newOff -> parseContent (st { psOffset = newOff }) emit stack

parsePITag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parsePITag !st emit !stack = do
  let !off = psOffset st
      !bs = psBS st
      !len = psLen st
      !nameStart = off + 2
  if nameStart >= len
    then pure (Left "Unexpected end in processing instruction")
    else do
      let !nameEnd = skipNameChars bs nameStart len
          !piTarget = decodeSlice bs nameStart (nameEnd - nameStart)
          !qEnd = findPIEnd bs nameEnd len
      case qEnd of
        Nothing -> pure (Left "Unterminated processing instruction")
        Just end -> do
          let !contentStart = skipSpaces bs nameEnd len
              !content = if contentStart < end
                           then decodeSlice bs contentStart (end - contentStart)
                           else T.empty
          emit (PI piTarget content)
          parseContent (st { psOffset = end + 2 }) emit stack

findPIEnd :: ByteString -> Int -> Int -> Maybe Int
findPIEnd bs off len = go off
  where
    go !i
      | i + 1 >= len = Nothing
      | BSU.unsafeIndex bs i == 0x3F && BSU.unsafeIndex bs (i+1) == 0x3E = Just i
      | otherwise = go (i + 1)

------------------------------------------------------------------------
-- Start / end tag parsing
------------------------------------------------------------------------

parseStartTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseStartTag !st emit !stack = do
  let !off = psOffset st
      !bs  = psBS st
      !ptr = psPtr st
      !len = psLen st
      !nameStart = off + 1
  if nameStart >= len || not (isNameStartByte (BSU.unsafeIndex bs nameStart))
    then pure (Left "Invalid element name (must start with letter, underscore, or colon)")
    else do
      let !nameEnd = skipNameChars bs nameStart len
          !rawName = decodeSlice bs nameStart (nameEnd - nameStart)
      attrResult <- parseAttributes (psEntities st) bs ptr nameEnd len
      case attrResult of
        Left err -> pure (Left err)
        Right (attrs, endOff, selfClose) -> do
          let !nsStack' = V.foldl' addNs (psNsStack st) attrs
              !st' = st { psOffset = endOff, psNsStack = nsStack' }
              !resolvedName = resolveNameNs rawName nsStack'
              !resolvedAttrs = V.map (resolveAttrNs nsStack') attrs
          emit (StartElement resolvedName resolvedAttrs)
          if selfClose
            then do
              emit (EndElement resolvedName)
              parseContent st' emit stack
            else
              parseContent st' emit (resolvedName : stack)

parseEndTag :: PState -> (SAXEvent -> IO ()) -> TagStack -> IO (Either String (PState, TagStack))
parseEndTag !st emit !stack = do
  let !off = psOffset st
      !bs  = psBS st
      !ptr = psPtr st
      !len = psLen st
      !nameStart = off + 2
      !nameEnd = skipNameChars bs nameStart len
      !rawName = decodeSlice bs nameStart (nameEnd - nameStart)
  gtPos <- findByteP ptr nameEnd len 0x3E
  if gtPos < 0
    then pure (Left "Unterminated end tag")
    else do
      let !name = resolveNameNs rawName (psNsStack st)
      case stack of
        [] -> pure (Left $ "Unexpected end tag: </" ++ T.unpack rawName ++ ">")
        (top : rest) ->
          if nameLocal top == nameLocal name &&
             namePrefix top == namePrefix name
            then do
              emit (EndElement name)
              parseContent (st { psOffset = gtPos + 1 }) emit rest
            else pure (Left $ "Mismatched tags: expected </" ++
                       T.unpack (nameLocal top) ++ ">, got </" ++
                       T.unpack rawName ++ ">")

------------------------------------------------------------------------
-- Attribute parsing with mutable vector (Fix 4)
------------------------------------------------------------------------

parseAttributes :: Map Text Text -> ByteString -> Ptr Word8 -> Int -> Int
                -> IO (Either String (Vector Attribute, Int, Bool))
parseAttributes !ents !bs !ptr !off !len = do
  mv0 <- MV.unsafeNew 8
  go mv0 off 0
  where
    go !mv !i !n = do
      let !j = skipSpaces bs i len
      if j >= len
        then pure (Left "Unterminated start tag")
        else do
          let !b = BSU.unsafeIndex bs j
          if b == 0x3E
            then do
              v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
              pure (Right (v, j + 1, False))
            else if b == 0x2F && j + 1 < len && BSU.unsafeIndex bs (j+1) == 0x3E
              then do
                v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
                pure (Right (v, j + 2, True))
              else do
                let !nameEnd = skipNameChars bs j len
                    !rawAttrName = decodeSlice bs j (nameEnd - j)
                    !eqPos = skipSpaces bs nameEnd len
                if eqPos >= len || BSU.unsafeIndex bs eqPos /= 0x3D
                  then pure (Left $ "Expected '=' after attribute name '" ++ T.unpack rawAttrName ++ "'")
                  else do
                    let !afterEq = skipSpaces bs (eqPos + 1) len
                    if afterEq >= len
                      then pure (Left "Unterminated attribute value")
                      else do
                        let !q = BSU.unsafeIndex bs afterEq
                        if q /= 0x22 && q /= 0x27
                          then pure (Left $ "Expected quote for attribute '" ++ T.unpack rawAttrName ++ "'")
                          else do
                            let !valStart = afterEq + 1
                            valEnd <- findAttrEndP ptr valStart len q
                            if valEnd < 0
                              then pure (Left "Unterminated attribute value")
                              else do
                                if containsLtByte bs valStart valEnd
                                  then pure (Left "Illegal '<' character in attribute value")
                                  else do
                                    valResult <- resolveAttrValue ents bs ptr valStart (valEnd - valStart)
                                    case valResult of
                                      Left err -> pure (Left err)
                                      Right val -> do
                                        let !attrName = parseAttrName rawAttrName
                                            !attr = Attribute attrName val
                                        if hasDuplicateAttr mv n attrName
                                          then pure (Left $ "Duplicate attribute: " ++ T.unpack rawAttrName)
                                          else do
                                            mv' <- if n >= MV.length mv
                                                     then MV.grow mv (MV.length mv)
                                                     else pure mv
                                            MV.unsafeWrite mv' n attr
                                            go mv' (valEnd + 1) (n + 1)
    hasDuplicateAttr !mv !n !attrName = unsafeDupablePerformIO $ do
      let go_ !k
            | k >= n = pure False
            | otherwise = do
                Attribute existingName _ <- MV.unsafeRead mv k
                if nameLocal existingName == nameLocal attrName &&
                   namePrefix existingName == namePrefix attrName
                  then pure True
                  else go_ (k + 1)
      go_ 0

containsLtByte :: ByteString -> Int -> Int -> Bool
containsLtByte !bs !start !end = go start
  where
    go !i
      | i >= end = False
      | BSU.unsafeIndex bs i == 0x3C = True
      | otherwise = go (i + 1)

resolveAttrValue :: Map Text Text -> ByteString -> Ptr Word8 -> Int -> Int -> IO (Either String Text)
resolveAttrValue !ents !bs !ptr !valStart !valLen = do
  let !valEnd = valStart + valLen
  ampPos <- findByteP ptr valStart valEnd 0x26
  if ampPos < 0
    then pure (Right (decodeSlice bs valStart valLen))
    else pure (resolveEntities ents (decodeSlice bs valStart valLen))
{-# INLINE resolveAttrValue #-}

------------------------------------------------------------------------
-- Name parsing and namespace resolution
------------------------------------------------------------------------

parseAttrName :: Text -> Name
parseAttrName raw =
  case T.breakOn ":" raw of
    (pfx, rest)
      | T.null rest -> simpleName pfx
      | otherwise -> qualifiedName pfx (T.drop 1 rest)

resolveNameNs :: Text -> [(Text, Text)] -> Name
resolveNameNs raw nsStack =
  case T.breakOn ":" raw of
    (local, rest)
      | T.null rest -> Name local Nothing (lookup "" nsStack)
      | otherwise ->
          let !pfx = local
              !localPart = T.drop 1 rest
          in Name localPart (Just pfx) (lookup pfx nsStack)

resolveAttrNs :: [(Text, Text)] -> Attribute -> Attribute
resolveAttrNs nsStack (Attribute name val) =
  let !resolved = case namePrefix name of
        Nothing -> name
        Just pfx -> name { nameNamespace = lookup pfx nsStack }
  in Attribute resolved val

addNs :: [(Text, Text)] -> Attribute -> [(Text, Text)]
addNs stack (Attribute name val)
  | nameLocal name == "xmlns" && namePrefix name == Nothing =
      ("", val) : stack
  | namePrefix name == Just "xmlns" =
      (nameLocal name, val) : stack
  | otherwise = stack

------------------------------------------------------------------------
-- Entity resolution
------------------------------------------------------------------------

resolveEntities :: Map Text Text -> Text -> Either String Text
resolveEntities ents txt
  | T.null txt = Right T.empty
  | not (T.any (== '&') txt) = Right txt
  | otherwise = resolveLoop txt
  where
    resolveLoop t =
      case T.breakOn "&" t of
        (before, rest)
          | T.null rest -> Right before
          | otherwise ->
              let !afterAmp = T.drop 1 rest
              in case T.breakOn ";" afterAmp of
                (_, semicRest)
                  | T.null semicRest ->
                      Left $ "Unterminated entity reference: &" ++ T.unpack (T.take 10 afterAmp)
                  | otherwise ->
                      let !entityName = T.takeWhile (/= ';') afterAmp
                          !remaining = T.drop 1 (T.dropWhile (/= ';') afterAmp)
                      in case resolveEntityWith ents entityName of
                        Nothing -> Left $ "Unknown entity: &" ++ T.unpack entityName ++ ";"
                        Just replacement -> do
                          rest' <- resolveLoop remaining
                          Right (before <> replacement <> rest')

resolveEntityWith :: Map Text Text -> Text -> Maybe Text
resolveEntityWith ents name = case resolveEntity name of
  Just t  -> Just t
  Nothing -> Map.lookup name ents

resolveEntity :: Text -> Maybe Text
resolveEntity "amp"  = Just "&"
resolveEntity "lt"   = Just "<"
resolveEntity "gt"   = Just ">"
resolveEntity "apos" = Just "'"
resolveEntity "quot" = Just "\""
resolveEntity t
  | T.isPrefixOf "#x" t || T.isPrefixOf "#X" t =
      let hex = T.drop 2 t
      in if T.null hex || not (T.all isHexDigit hex)
           then Nothing
           else let !n = T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 hex
                in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | T.isPrefixOf "#" t =
      let dec = T.drop 1 t
      in if T.null dec || not (T.all isDigit dec)
           then Nothing
           else let !n = T.foldl' (\acc c -> acc * 10 + (ord c - ord '0')) 0 dec
                in if n > 0x10FFFF then Nothing else Just (T.singleton (chr n))
  | otherwise = Nothing

------------------------------------------------------------------------
-- Low-level utilities
------------------------------------------------------------------------

skipSpaces :: ByteString -> Int -> Int -> Int
skipSpaces !bs !off !len
  | off >= len = off
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in if isSpaceByte b
           then skipSpaces bs (off + 1) len
           else off

isSpaceByte :: Word8 -> Bool
isSpaceByte b = b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D
{-# INLINE isSpaceByte #-}

skipNameChars :: ByteString -> Int -> Int -> Int
skipNameChars !bs !off !len = go off
  where
    go !i
      | i >= len = i
      | isNameByte (BSU.unsafeIndex bs i) = go (i + 1)
      | otherwise = i

isNameByte :: Word8 -> Bool
isNameByte !b =
  (b >= 0x61 && b <= 0x7A) ||
  (b >= 0x41 && b <= 0x5A) ||
  (b >= 0x30 && b <= 0x39) ||
  b == 0x3A || b == 0x5F || b == 0x2D || b == 0x2E ||
  b >= 0x80
{-# INLINE isNameByte #-}

isNameStartByte :: Word8 -> Bool
isNameStartByte !b =
  (b >= 0x61 && b <= 0x7A) ||
  (b >= 0x41 && b <= 0x5A) ||
  b == 0x3A || b == 0x5F ||
  b >= 0x80
{-# INLINE isNameStartByte #-}

matchBytes :: ByteString -> Int -> [Word8] -> Bool
matchBytes bs off expected = go off expected
  where
    !len = BS.length bs
    go !_ [] = True
    go !i (b:rest)
      | i >= len = False
      | BSU.unsafeIndex bs i == b = go (i + 1) rest
      | otherwise = False

sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs off count
  | count <= 0 = BS.empty
  | otherwise = BSU.unsafeTake count (BSU.unsafeDrop off bs)
{-# INLINE sliceBS #-}

decodeSlice :: ByteString -> Int -> Int -> Text
decodeSlice bs off count = TE.decodeUtf8 (sliceBS bs off count)
{-# INLINE decodeSlice #-}
