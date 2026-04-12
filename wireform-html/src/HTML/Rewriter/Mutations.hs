{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module HTML.Rewriter.Mutations where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (BS))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray (MutablePrimArray, readPrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, newSmallArray, sizeofSmallArray, runSmallArray, thawSmallArray, copySmallArray, writeSmallArray)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Array.Byte (ByteArray (ByteArray))
import Data.Text.Internal (Text (..))
import GHC.Exts (ByteArray#, Int (..), RealWorld, indexWord8Array#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word8 (W8#))
import HTML.Parse (isWSByte, readByteOff, readTagAttrsBS)
import HTML.Value (HTMLAttribute (..))
import Data.ByteString.Builder qualified as BB

data ContentType = AsText | AsHTML
  deriving (Show, Eq)


-- ---------------------------------------------------------------------------
-- Mutations – intrusive sum, no extra Maybe box
-- ---------------------------------------------------------------------------

-- | Shared mutation state for before/after/replace/remove.
-- 'MutNone' is the unmodified state; checking is a single pattern match.
data Mutations
  = MutNone
  | Mut !BB.Builder !BB.Builder !(Maybe BB.Builder) !Bool
  -- ^ before, after, replacement (builder), removed
  | MutText !(Maybe BB.Builder) !(Maybe BB.Builder) !Text !ContentType !Bool
  -- ^ before, after, replacement text, content type, removed

-- | Apply f to the before/after/repl/removed fields. Allocates on first use.
withMut :: IORef Mutations -> (BB.Builder -> BB.Builder -> Maybe BB.Builder -> Bool -> Mutations) -> IO ()
withMut ref f = do
  m <- readIORef ref
  case m of
    MutNone -> writeIORef ref (f mempty mempty Nothing False)
    Mut b a r d -> writeIORef ref (f b a r d)
    MutText mb ma _ _ d -> writeIORef ref (f (fromMaybe mempty mb) (fromMaybe mempty ma) Nothing d)
{-# INLINE withMut #-}


-- ---------------------------------------------------------------------------
-- Mutable handles
-- ---------------------------------------------------------------------------

-- | Element-specific mutations beyond base before/after/replace/remove.
data ElemMut = ElemMut
  { emTag :: !(Maybe Text)
  , emAttrs :: !(Maybe (SmallArray HTMLAttribute))
  , emNewAttrs :: ![(Text, Text)]
  , emPrepend :: !(Maybe BB.Builder)
  , emAppend :: !(Maybe BB.Builder)
  , emRmChildren :: !Bool
  , emInnerContent :: !(Maybe BB.Builder)
  , emEndTagHandler :: !(Maybe (EndTagRef -> IO ()))
  }

emptyElemMut :: ElemMut
emptyElemMut = ElemMut Nothing Nothing [] Nothing Nothing False Nothing Nothing
{-# INLINE emptyElemMut #-}

-- | Compact representation of element modifications.
-- Common cases (tag rename, new attrs) avoid the 72-byte ElemMut allocation.
data ElemMod
  = EMNone
  | EMTag !Text
  | EMNewAttrs ![(Text, Text)]
  | EMTagAndAttrs !Text ![(Text, Text)]
  | EMFull !ElemMut

elemModToMut :: ElemMod -> Maybe ElemMut
elemModToMut EMNone = Nothing
elemModToMut (EMTag t) = Just (emptyElemMut {emTag = Just t})
elemModToMut (EMNewAttrs as) = Just (emptyElemMut {emNewAttrs = as})
elemModToMut (EMTagAndAttrs t as) = Just (emptyElemMut {emTag = Just t, emNewAttrs = as})
elemModToMut (EMFull m) = Just m
{-# INLINE elemModToMut #-}

elemModTag :: ElemMod -> Maybe Text
elemModTag (EMTag t) = Just t
elemModTag (EMTagAndAttrs t _) = Just t
elemModTag (EMFull m) = emTag m
elemModTag _ = Nothing
{-# INLINE elemModTag #-}

modifyElemMut :: ElementRef -> (ElemMut -> ElemMut) -> IO ()
modifyElemMut er f = do
  m <- readIORef (_erElem er)
  let !base = case m of
        EMNone -> emptyElemMut
        EMTag t -> emptyElemMut {emTag = Just t}
        EMNewAttrs as -> emptyElemMut {emNewAttrs = as}
        EMTagAndAttrs t as -> emptyElemMut {emTag = Just t, emNewAttrs = as}
        EMFull em -> em
  writeIORef (_erElem er) $! EMFull $! f base
{-# INLINE modifyElemMut #-}

data ElementRef = ElementRef
  { _erOrigTag :: !(IORef Text)
  , _erOrigAttrs :: !(IORef (SmallArray HTMLAttribute))
  , _erSelfClose :: !Bool
  , _erMut :: !(IORef Mutations)
  , _erElem :: !(IORef ElemMod)
  , _erInts :: !(MutablePrimArray RealWorld Int)
    -- ^ slot 0 = valid (0/1), slot 1 = attrOff, slot 2 = srcLen
  , _erSharedBA :: !(IORef ByteArray)
  , _erSrcBS :: !(IORef ByteString)
  }


data TextChunkRef = TextChunkRef
  { _trContent :: !(IORef Text)
  , _trMut :: !(IORef Mutations)
  , _trIsLast :: !Bool
  , _trValid :: !(IORef Bool)
  }


data CommentRef = CommentRef
  { _crText :: !(IORef Text)
  , _crMut :: !(IORef Mutations)
  , _crValid :: !(IORef Bool)
  }


data DoctypeRef = DoctypeRef
  { _drName :: !Text
  , _drPubId :: !(Maybe Text)
  , _drSysId :: !(Maybe Text)
  , _drValid :: !(IORef Bool)
  }


data EndTagRef = EndTagRef
  { _etrTag :: !(IORef Text)
  , _etrMut :: !(IORef Mutations)
  , _etrValid :: !(IORef Bool)
  }


newtype ExpiredRefError = ExpiredRefError Text
  deriving (Show)


instance Exception ExpiredRefError


checkValidER :: ElementRef -> IO ()
checkValidER er = do
  v <- readPrimArray (_erInts er) 0
  when ((v :: Int) == 0) $ throwIO (ExpiredRefError "ElementRef used outside its callback scope")
{-# INLINE checkValidER #-}

checkValid :: IORef Bool -> Text -> IO ()
checkValid ref what = do
  v <- readIORef ref
  unless v $ throwIO (ExpiredRefError (what <> " used outside its callback scope"))
{-# INLINE checkValid #-}

encodeContent :: Text -> ContentType -> BB.Builder
encodeContent text AsHTML = BB.byteString (TE.encodeUtf8 text)
encodeContent text AsText = escapeTextBuilder text


escapeTextBuilder :: Text -> BB.Builder
escapeTextBuilder t =
  let !bs = TE.encodeUtf8 t
  in escapeBS bs 0 (BS.length bs)
  where
    escapeBS !bs !off !len
      | off >= len = mempty
      | otherwise =
          let !b = BS.index bs off
          in case b of
              0x3C -> BB.byteString (BS.take off BS.empty) <> BB.byteString "&lt;" <> escapeBS bs (off + 1) len
              _ -> scanClean bs off off len

    scanClean !bs !start !off !len
      | off >= len = BB.byteString (BS.take (off - start) (BS.drop start bs))
      | otherwise =
          let !b = BS.index bs off
          in case b of
              0x3C ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&lt;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x3E ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&gt;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x26 ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&amp;"
                  <> scanClean bs (off + 1) (off + 1) len
              _ -> scanClean bs start (off + 1) len
lookupAttrArr :: Text -> SmallArray HTMLAttribute -> Maybe Text
lookupAttrArr name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = Nothing
      | otherwise =
          let !(HTMLAttribute k v) = indexSmallArray arr i
          in if k == name then Just v else go (i + 1)
{-# INLINE lookupAttrArr #-}


hasAttrArr :: Text -> SmallArray HTMLAttribute -> Bool
hasAttrArr name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | otherwise =
          let !(HTMLAttribute k _) = indexSmallArray arr i
          in k == name || go (i + 1)
{-# INLINE hasAttrArr #-}


setAttrArr :: Text -> Text -> SmallArray HTMLAttribute -> SmallArray HTMLAttribute
setAttrArr name val arr =
  let !n = sizeofSmallArray arr
      !idx = findAttrIdx name arr
  in if idx >= 0
    then runSmallArray $ do
      ma <- thawSmallArray arr 0 n
      writeSmallArray ma idx (HTMLAttribute name val)
      pure ma
    else runSmallArray $ do
      ma <- newSmallArray (n + 1) (HTMLAttribute name val)
      copySmallArray ma 0 arr 0 n
      pure ma
{-# INLINE setAttrArr #-}


removeAttrArr :: Text -> SmallArray HTMLAttribute -> SmallArray HTMLAttribute
removeAttrArr name arr =
  let !n = sizeofSmallArray arr
      !idx = findAttrIdx name arr
  in if idx < 0
    then arr
    else runSmallArray $ do
      ma <- newSmallArray (n - 1) (HTMLAttribute "" "")
      copySmallArray ma 0 arr 0 idx
      copySmallArray ma idx arr (idx + 1) (n - idx - 1)
      pure ma
{-# INLINE removeAttrArr #-}


findAttrIdx :: Text -> SmallArray HTMLAttribute -> Int
findAttrIdx name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = -1
      | otherwise = let !(HTMLAttribute k _) = indexSmallArray arr i
                    in if k == name then i else go (i + 1)
{-# INLINE findAttrIdx #-}


indexBA :: ByteArray# -> Int -> Word8
indexBA ba# (I# i#) = W8# (indexWord8Array# ba# i#)
{-# INLINE indexBA #-}


forceAttrs :: ElementRef -> IO (SmallArray HTMLAttribute)
forceAttrs er = do
  let !ints = _erInts er
  off <- readPrimArray ints 1
  if (off :: Int) < 0
    then readIORef (_erOrigAttrs er)
    else do
      ba <- readIORef (_erSharedBA er)
      srcBS <- readIORef (_erSrcBS er)
      srcLen <- readPrimArray ints 2
      let (!attrs, !_, !_) = readTagAttrsBS ba srcBS off (srcLen :: Int)
      writeIORef (_erOrigAttrs er) attrs
      writePrimArray ints 1 (-1 :: Int)
      pure attrs
{-# INLINE forceAttrs #-}


lookupPending :: Text -> [(Text, Text)] -> Maybe Text
lookupPending _ [] = Nothing
lookupPending name ((n, v) : rest)
  | n == name = Just v
  | otherwise = lookupPending name rest
{-# INLINE lookupPending #-}


hasAttrNameInTag :: ByteString -> Int -> Int -> Text -> Bool
hasAttrNameInTag (BS (ForeignPtr addr# _) _) !off !end name = go off
  where
    !(Text (ByteArray nameBA#) nameOff nameLen) = name
    rd :: Int -> Word8
    rd = readByteOff addr#
    {-# INLINE rd #-}

    go !i
      | i >= end = False
      | otherwise = case rd i of
          0x3E -> False
          0x2F | i + 1 < end, rd (i + 1) == 0x3E -> False
          b | isWSByte b -> go (i + 1)
          _ -> checkName i

    checkName !nameStart =
      let !nameEnd' = scanToDelim nameStart
          !attrNameLen = nameEnd' - nameStart
      in (attrNameLen == nameLen && matchBytes nameStart) || skipValue nameEnd'

    scanToDelim !j
      | j >= end = j
      | otherwise = case rd j of
          0x3D -> j
          0x3E -> j
          0x2F -> j
          b | isWSByte b -> j
          _ -> scanToDelim (j + 1)

    matchBytes !start = matchLoop 0
      where
        matchLoop !k
          | k >= nameLen = True
          | otherwise =
              let !a = toLowerByte (rd (start + k))
                  !b = indexBA nameBA# (nameOff + k)
              in a == b && matchLoop (k + 1)

    toLowerByte :: Word8 -> Word8
    toLowerByte w = if w >= 0x41 && w <= 0x5A then w + 32 else w

    skipValue !j
      | j >= end = go j
      | otherwise =
          let !j1 = skipWS j
          in if j1 >= end || rd j1 /= 0x3D
               then go j1
               else
                 let !j2 = skipWS (j1 + 1)
                 in if j2 >= end then go j2
                    else case rd j2 of
                      0x22 -> go (scanPast 0x22 (j2 + 1))
                      0x27 -> go (scanPast 0x27 (j2 + 1))
                      _    -> go (scanUnq j2)

    scanPast !delim !j
      | j >= end = j
      | rd j == delim = j + 1
      | otherwise = scanPast delim (j + 1)

    scanUnq !j
      | j >= end = j
      | isWSByte (rd j) || rd j == 0x3E = j
      | otherwise = scanUnq (j + 1)

    skipWS !j
      | j >= end = j
      | isWSByte (rd j) = skipWS (j + 1)
      | otherwise = j
{-# INLINE hasAttrNameInTag #-}
getTagName :: ElementRef -> IO Text
getTagName er = do
  checkValidER er
  em <- readIORef (_erElem er)
  case elemModTag em of
    Just t -> pure t
    Nothing -> readIORef (_erOrigTag er)


setTagName :: ElementRef -> Text -> IO ()
setTagName er t = do
  checkValidER er
  em <- readIORef (_erElem er)
  writeIORef (_erElem er) $! case em of
    EMNone -> EMTag t
    EMTag _ -> EMTag t
    EMNewAttrs as -> EMTagAndAttrs t as
    EMTagAndAttrs _ as -> EMTagAndAttrs t as
    EMFull m -> EMFull (m {emTag = Just t})


getElemAttr :: ElementRef -> Text -> IO (Maybe Text)
getElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMNewAttrs pending
      | Just v <- lookupPending name pending -> pure (Just v)
    EMTagAndAttrs _ pending
      | Just v <- lookupPending name pending -> pure (Just v)
    EMFull (ElemMut {emNewAttrs = pending})
      | Just v <- lookupPending name pending -> pure (Just v)
    EMFull (ElemMut {emAttrs = Just attrs}) -> pure $ lookupAttrArr name attrs
    _ -> do
      attrs <- forceAttrs er
      pure $ lookupAttrArr name attrs


setElemAttr :: ElementRef -> Text -> Text -> IO ()
setElemAttr er name val = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull e@(ElemMut {emAttrs = Just attrs}) ->
      writeIORef (_erElem er) $! EMFull $! e {emAttrs = Just (setAttrArr name val attrs)}
    EMFull e@(ElemMut {emNewAttrs = pending}) | not (null pending) ->
      writeIORef (_erElem er) $! EMFull $! e {emNewAttrs = (name, val) : pending}
    _ -> do
      attrOff <- readPrimArray (_erInts er) 1
      if (attrOff :: Int) >= 0
        then do
          srcBS <- readIORef (_erSrcBS er)
          srcLen <- readPrimArray (_erInts er) 2
          if hasAttrNameInTag srcBS attrOff (srcLen :: Int) name
            then do
              origArr <- forceAttrs er
              modifyElemMut er (\e -> e {emAttrs = Just (setAttrArr name val origArr)})
            else do
              writeIORef (_erElem er) $! case em of
                EMNone -> EMNewAttrs [(name, val)]
                EMTag t -> EMTagAndAttrs t [(name, val)]
                EMNewAttrs as -> EMNewAttrs ((name, val) : as)
                EMTagAndAttrs t as -> EMTagAndAttrs t ((name, val) : as)
                EMFull m -> EMFull (m {emNewAttrs = (name, val) : emNewAttrs m})
        else do
          origArr <- forceAttrs er
          modifyElemMut er (\e -> e {emAttrs = Just (setAttrArr name val origArr)})


removeElemAttr :: ElementRef -> Text -> IO ()
removeElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull e@(ElemMut {emAttrs = Just attrs}) ->
      writeIORef (_erElem er) $! EMFull $! e {emAttrs = Just (removeAttrArr name attrs)}
    _ -> do
      origArr <- forceAttrs er
      modifyElemMut er (\e -> e {emAttrs = Just (removeAttrArr name origArr)})


hasElemAttr :: ElementRef -> Text -> IO Bool
hasElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMNewAttrs pending | any (\(n, _) -> n == name) pending -> pure True
    EMTagAndAttrs _ pending | any (\(n, _) -> n == name) pending -> pure True
    EMFull (ElemMut {emNewAttrs = pending}) | any (\(n, _) -> n == name) pending -> pure True
    EMFull (ElemMut {emAttrs = Just attrs}) -> pure $ hasAttrArr name attrs
    _ -> do
      origArr <- forceAttrs er
      pure $ hasAttrArr name origArr


getElemAttrs :: ElementRef -> IO [(Text, Text)]
getElemAttrs er = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull (ElemMut {emAttrs = Just a}) -> pure $
      let !sz = sizeofSmallArray a
          go !i acc
            | i < 0 = acc
            | otherwise = let !(HTMLAttribute n v) = indexSmallArray a i in go (i - 1) ((n, v) : acc)
      in go (sz - 1) []
    _ -> do
      origArr <- forceAttrs er
      let !sz = sizeofSmallArray origArr
          go2 !i acc
            | i < 0 = acc
            | otherwise = let !(HTMLAttribute n v) = indexSmallArray origArr i in go2 (i - 1) ((n, v) : acc)
      pure $ go2 (sz - 1) []


beforeElement :: ElementRef -> Text -> ContentType -> IO ()
beforeElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


prependToElement :: ElementRef -> Text -> ContentType -> IO ()
prependToElement er content ct = do
  checkValidER er
  let !b = encodeContent content ct
  modifyElemMut er (\em -> em {emPrepend = Just $ maybe b (<> b) (emPrepend em)})


appendToElement :: ElementRef -> Text -> ContentType -> IO ()
appendToElement er content ct = do
  checkValidER er
  let !b = encodeContent content ct
  modifyElemMut er (\em -> em {emAppend = Just $ maybe b (<> b) (emAppend em)})


afterElement :: ElementRef -> Text -> ContentType -> IO ()
afterElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


replaceElement :: ElementRef -> Text -> ContentType -> IO ()
replaceElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a _ _ -> Mut b a (Just (encodeContent content ct)) True)


removeElement :: ElementRef -> IO ()
removeElement er = do
  checkValidER er
  withMut (_erMut er) (\b a r _ -> Mut b a r True)


removeChildren :: ElementRef -> IO ()
removeChildren er = do
  checkValidER er
  modifyElemMut er (\em -> em {emRmChildren = True})


setInnerContent :: ElementRef -> Text -> ContentType -> IO ()
setInnerContent er content ct = do
  checkValidER er
  modifyElemMut er (\em -> em {emInnerContent = Just (encodeContent content ct)})


onElementEndTag :: ElementRef -> (EndTagRef -> IO ()) -> IO ()
onElementEndTag er handler = do
  checkValidER er
  modifyElemMut er (\em -> em {emEndTagHandler = Just handler})


-- ---------------------------------------------------------------------------
-- Text chunk mutation API
-- ---------------------------------------------------------------------------

getTextContent :: TextChunkRef -> IO Text
getTextContent tr = checkValid (_trValid tr) "TextChunkRef" >> readIORef (_trContent tr)


replaceTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
replaceTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  m <- readIORef (_trMut tr)
  case m of
    MutNone -> writeIORef (_trMut tr) $! MutText Nothing Nothing content ct True
    Mut b a _ _ -> writeIORef (_trMut tr) $! MutText (Just b) (Just a) content ct True
    MutText b a _ _ _ -> writeIORef (_trMut tr) $! MutText b a content ct True


beforeTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
beforeTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
afterTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


removeTextChunk :: TextChunkRef -> IO ()
removeTextChunk tr = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r _ -> Mut b a r True)


isLastInTextNode :: TextChunkRef -> IO Bool
isLastInTextNode tr = checkValid (_trValid tr) "TextChunkRef" >> pure (_trIsLast tr)


-- ---------------------------------------------------------------------------
-- Comment mutation API
-- ---------------------------------------------------------------------------

getCommentText :: CommentRef -> IO Text
getCommentText cr = checkValid (_crValid cr) "CommentRef" >> readIORef (_crText cr)


setCommentText :: CommentRef -> Text -> IO ()
setCommentText cr t = checkValid (_crValid cr) "CommentRef" >> writeIORef (_crText cr) t


replaceComment :: CommentRef -> Text -> ContentType -> IO ()
replaceComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a _ _ -> Mut b a (Just (encodeContent content ct)) True)


beforeComment :: CommentRef -> Text -> ContentType -> IO ()
beforeComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterComment :: CommentRef -> Text -> ContentType -> IO ()
afterComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


removeComment :: CommentRef -> IO ()
removeComment cr = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r _ -> Mut b a r True)


-- ---------------------------------------------------------------------------
-- Doctype access
-- ---------------------------------------------------------------------------

getDoctypeName :: DoctypeRef -> IO Text
getDoctypeName dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drName dr)


getDoctypePublicId :: DoctypeRef -> IO (Maybe Text)
getDoctypePublicId dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drPubId dr)


getDoctypeSystemId :: DoctypeRef -> IO (Maybe Text)
getDoctypeSystemId dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drSysId dr)


-- ---------------------------------------------------------------------------
-- End tag mutation
-- ---------------------------------------------------------------------------

getEndTagName :: EndTagRef -> IO Text
getEndTagName etr = checkValid (_etrValid etr) "EndTagRef" >> readIORef (_etrTag etr)


setEndTagName :: EndTagRef -> Text -> IO ()
setEndTagName etr t = checkValid (_etrValid etr) "EndTagRef" >> writeIORef (_etrTag etr) t


beforeEndTag :: EndTagRef -> Text -> ContentType -> IO ()
beforeEndTag etr content ct = do
  checkValid (_etrValid etr) "EndTagRef"
  withMut (_etrMut etr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterEndTag :: EndTagRef -> Text -> ContentType -> IO ()
afterEndTag etr content ct = do
  checkValid (_etrValid etr) "EndTagRef"
  withMut (_etrMut etr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)
