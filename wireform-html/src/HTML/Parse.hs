{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Full HTML5 tree construction algorithm with mutable tree nodes.
module HTML.Parse (
  parseHTML,
  parseHTMLFragment,
  parseHTMLNodes,
  tokenizeOnlyIO,
  tokenizeCountChunk,
  treeBuildOnlyIO,

  -- * Low-level token API (for rewriter)
  Token (..),
  tokenizeBS,
  tokenizeCallbackIO,
  tokenizeCallbackIOWith,

  -- * Incremental / streaming tree builder
  TreeBuilder,
  newTreeBuilder,
  newTreeBuilderWith,
  -- newTreeBuilderRaw (unused; raw events use tokenizeRawEventsIO)
  processToken,
  buildDocument,
  tokenizeBSIO,
  freeTreeBuilder,
  drainTreeBuilderStack,
  tbGetEvents,
  tbResetEvents,

  -- * Raw event tokenizer (no tree builder)
  tokenizeRawEventsIO,

  -- * Low-level scanning (direct rewriter)
  byteStringPinnedByteArray,
  decodeTextSlice,
  decodeTextSliceKnown,
  scanTagNameFast,
  scanTextFast,
  scanTextAscii,
  readTagAttrsBS,
  scanClassAndSkip,
  skipTagBS,
  skipToGtBS,
  skipAttrsBS,
  readByteOff,
  isAlphaByte,
  isRawTextTag,
  isRCDataTag,
  isSvgHtmlIntegPoint,
  markupDeclRemaining,
  rawTextRemainingString,
  utf8AdvanceNChars,
  tokenizeMarkupDeclCtx,
  ScanTextResult (..),
  parseEntityRef,
  isWSByte,
  skipWSAddr,
) where

import Control.Monad (when)
import Data.Array.Byte (ByteArray (ByteArray))
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (BS))
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, toLower)
import Data.Foldable (toList)
import Data.IORef
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Primitive.ByteArray (
  MutableByteArray,
  newByteArray,
  readByteArray,
  writeByteArray,
 )
import Data.Primitive.SmallArray (
  SmallArray,
  SmallMutableArray,
  copySmallMutableArray,
  createSmallArray,
  emptySmallArray,
  indexSmallArray,
  mapSmallArray',
  newSmallArray,
  readSmallArray,
  shrinkSmallMutableArray,
  sizeofSmallArray,
  sizeofSmallMutableArray,
  getSizeofSmallMutableArray,
  smallArrayFromList,
  unsafeFreezeSmallArray,
  freezeSmallArray,
  writeSmallArray,
 )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Internal (Text (Text))
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CPtrdiff (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Exts (Addr#, Int (I#), Int#, RealWorld, Word (W#), copyAddrToByteArray#, dataToTag#, indexWord8OffAddr#, indexWordOffAddr#, newByteArray#, plusAddr#, runRW#, tagToEnum#, unsafeFreezeByteArray#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word8 (..))
import HTML.TagId
import HTML.Value
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)


foreign import ccall unsafe "wireform_scan_text"
  c_scan_text :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_text_ascii"
  c_scan_text_ascii :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_is_all_ascii"
  c_is_all_ascii :: Addr# -> CPtrdiff -> CPtrdiff -> CInt


foreign import ccall unsafe "wireform_scan_tagname"
  c_scan_tagname :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_dquote"
  c_scan_dquote :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_dquote_ascii"
  c_scan_dquote_ascii :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_squote"
  c_scan_squote :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_squote_ascii"
  c_scan_squote_ascii :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_skip_to_dquote"
  c_skip_to_dquote :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_skip_to_squote"
  c_skip_to_squote :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_scan_unquoted"
  c_scan_unquoted :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


foreign import ccall unsafe "wireform_skip_attrs"
  c_skip_attrs :: Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff


------------------------------------------------------------------------
-- Fast TagId classification for modeInBody dispatch
------------------------------------------------------------------------

{-# INLINE endTagCanFastPop #-}
endTagCanFastPop :: TagId -> Bool
endTagCanFastPop !t = case t of
  TagBody -> False
  TagHtml -> False
  TagForm -> False
  TagBr -> False
  TagTemplate -> False
  t' | tagIdIsFormatting t' -> False
  TagA -> False
  _ -> True


{-# INLINE isBodyBlockTag #-}
isBodyBlockTag :: TagId -> Bool
isBodyBlockTag !t = case t of
  TagAddress -> True
  TagArticle -> True
  TagAside -> True
  TagBlockquote -> True
  TagCenter -> True
  TagDetails -> True
  TagDialog -> True
  TagDir -> True
  TagDiv -> True
  TagDl -> True
  TagFieldset -> True
  TagFigcaption -> True
  TagFigure -> True
  TagFooter -> True
  TagHeader -> True
  TagHgroup -> True
  TagMain -> True
  TagMenu -> True
  TagNav -> True
  TagOl -> True
  TagP -> True
  TagSearch -> True
  TagSection -> True
  TagSummary -> True
  TagUl -> True
  _ -> False


{-# INLINE isBodyEndBlockTag #-}
isBodyEndBlockTag :: TagId -> Bool
isBodyEndBlockTag !t = case t of
  TagAddress -> True
  TagArticle -> True
  TagAside -> True
  TagBlockquote -> True
  TagButton -> True
  TagCenter -> True
  TagDetails -> True
  TagDialog -> True
  TagDir -> True
  TagDiv -> True
  TagDl -> True
  TagFieldset -> True
  TagFigcaption -> True
  TagFigure -> True
  TagFooter -> True
  TagHeader -> True
  TagHgroup -> True
  TagListing -> True
  TagMain -> True
  TagMenu -> True
  TagNav -> True
  TagOl -> True
  TagPre -> True
  TagSearch -> True
  TagSection -> True
  TagSummary -> True
  TagUl -> True
  _ -> False


{-# INLINE isVoidInsertTag #-}
isVoidInsertTag :: TagId -> Bool
isVoidInsertTag !t = case t of
  TagArea -> True
  TagBr -> True
  TagEmbed -> True
  TagImg -> True
  TagKeygen -> True
  TagWbr -> True
  _ -> False


{-# INLINE isIgnoredStartTag #-}
isIgnoredStartTag :: TagId -> Bool
isIgnoredStartTag !t = case t of
  TagCaption -> True
  TagCol -> True
  TagColgroup -> True
  TagFrame -> True
  TagHead -> True
  TagTbody -> True
  TagTd -> True
  TagTfoot -> True
  TagTh -> True
  TagThead -> True
  TagTr -> True
  _ -> False


{-# INLINE isHeadDelegateTag #-}
isHeadDelegateTag :: TagId -> Bool
isHeadDelegateTag !t = case t of
  TagBase -> True
  TagBasefont -> True
  TagBgsound -> True
  TagLink -> True
  TagMeta -> True
  TagTemplate -> True
  TagTitle -> True
  TagNoframes -> True
  TagScript -> True
  TagStyle -> True
  _ -> False


{-# INLINE isRawTextTag #-}
isRawTextTag :: TagId -> Bool
isRawTextTag !t = case t of
  TagScript -> True
  TagStyle -> True
  TagXmp -> True
  TagIframe -> True
  TagNoembed -> True
  TagNoframes -> True
  TagNoscript -> True
  _ -> False


{-# INLINE isRCDataTag #-}
isRCDataTag :: TagId -> Bool
isRCDataTag !t = case t of
  TagTextarea -> True
  TagTitle -> True
  _ -> False


{-# INLINE isSvgHtmlIntegPoint #-}
isSvgHtmlIntegPoint :: TagId -> Text -> Bool
isSvgHtmlIntegPoint !t !name = case t of
  TagTitle -> True
  TagUnknown -> name == "foreignobject" || name == "desc"
  _ -> False


------------------------------------------------------------------------
-- Token types
------------------------------------------------------------------------

data Token
  = TDoctype !Text !(Maybe Text) !(Maybe Text) !Bool
  | TStartTag !Text !(SmallArray HTMLAttribute) !Bool !TagId
  | TEndTag !Text !TagId
  | TChar !Char
  | TString !Text
  | TComment !Text
  | TEOF
  deriving (Show)


------------------------------------------------------------------------
-- Insertion modes
------------------------------------------------------------------------

data InsertionMode
  = MInitial
  | MBeforeHtml
  | MBeforeHead
  | MInHead
  | MInHeadNoscript
  | MAfterHead
  | MInBody
  | MText
  | MInTable
  | MInTableText
  | MInCaption
  | MInColumnGroup
  | MInTableBody
  | MInRow
  | MInCell
  | MInSelect
  | MInSelectInTable
  | MInTemplate
  | MAfterBody
  | MInFrameset
  | MAfterFrameset
  | MAfterAfterBody
  | MAfterAfterFrameset
  deriving (Show, Eq, Enum)


------------------------------------------------------------------------
-- Mutable tree nodes
------------------------------------------------------------------------

data TBChild
  = TBCElement !TBNode
  | TBCText !Text
  | TBCComment !Text


data ChildVec
  = ChildVec
      !(MutableByteArray RealWorld)
      !(SmallMutableArray RealWorld TBChild)


data TBNode = TBNode
  { nodeName :: !Text
  , nodeTagId :: !TagId
  , nodeAttrs :: {-# UNPACK #-} !(IORef (SmallArray HTMLAttribute))
  , nodeNs :: !(Maybe Text)
  , nodeIsHTMLNs :: !Bool
  , nodeIsTemplate :: !Bool
  , nodeChildren :: {-# UNPACK #-} !(IORef ChildVec)
  , nodeParent :: {-# UNPACK #-} !(IORef (Maybe TBNode))
  , nodeTemplateContents :: {-# UNPACK #-} !(IORef ChildVec)
  }


instance Eq TBNode where
  a == b = nodeChildren a == nodeChildren b


data ChildNode
  = CElement !TBNode
  | CText !Text
  | CComment !Text
  | CDoctype !Text !(Maybe Text) !(Maybe Text)


------------------------------------------------------------------------
-- ChildVec operations
------------------------------------------------------------------------

uninitChild :: TBChild
uninitChild = TBCText T.empty
{-# NOINLINE uninitChild #-}


smallArrayFromListN_Rev :: Int -> [a] -> SmallArray a
smallArrayFromListN_Rev !n xs = createSmallArray n (error "smallArrayFromListN_Rev") $ \marr ->
  let fill !_ [] = pure ()
      fill !i (a : as) = writeSmallArray marr i a >> fill (i - 1) as
  in fill (n - 1) xs
{-# INLINE smallArrayFromListN_Rev #-}


newChildVec :: Int -> IO (IORef ChildVec)
newChildVec cap = do
  countBuf <- newByteArray 16
  writeByteArray countBuf 0 (0 :: Int)
  writeByteArray countBuf 1 (0 :: Int)
  arr <- newSmallArray cap uninitChild
  newIORef (ChildVec countBuf arr)


emptyChildVecPool :: MutableByteArray RealWorld
emptyChildVecPool = unsafePerformIO $ do
  buf <- newByteArray 16
  writeByteArray buf 0 (0 :: Int)
  writeByteArray buf 1 (0 :: Int)
  pure buf
{-# NOINLINE emptyChildVecPool #-}


emptyChildVecRef :: IORef ChildVec
emptyChildVecRef = unsafePerformIO $ do
  arr <- newSmallArray 0 uninitChild
  newIORef (ChildVec emptyChildVecPool arr)
{-# NOINLINE emptyChildVecRef #-}

dummyParentRef :: IORef (Maybe TBNode)
dummyParentRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dummyParentRef #-}

dummyAttrsRef :: IORef (SmallArray HTMLAttribute)
dummyAttrsRef = unsafePerformIO $ newIORef emptySmallArray
{-# NOINLINE dummyAttrsRef #-}


{-# INLINE pushChild #-}
pushChild :: IORef ChildVec -> TBChild -> IO ()
pushChild ref child = do
  ChildVec countBuf arr <- readIORef ref
  n <- readByteArray countBuf 0
  let cap = sizeofSmallMutableArray arr
  if n >= cap
    then do
      let !newCap = max 4 (cap * 2)
      new <- newSmallArray newCap uninitChild
      copySmallMutableArray new 0 arr 0 n
      writeSmallArray new n child
      writeByteArray countBuf 0 (n + 1)
      writeIORef ref (ChildVec countBuf new)
    else do
      writeSmallArray arr n child
      writeByteArray countBuf 0 (n + 1)
  case child of
    TBCElement _ -> do
      ec <- readByteArray countBuf 1 :: IO Int
      writeByteArray countBuf 1 (ec + 1)
    _ -> pure ()


{-# INLINE pushText #-}
pushText :: IORef ChildVec -> Text -> IO ()
pushText ref txt = do
  ChildVec countBuf arr <- readIORef ref
  n <- readByteArray countBuf 0
  if n > 0
    then do
      lastChild <- readSmallArray arr (n - 1)
      case lastChild of
        TBCText old -> writeSmallArray arr (n - 1) $! TBCText (old <> txt)
        _ -> do
          let cap = sizeofSmallMutableArray arr
          if n >= cap
            then do
              let !newCap = max 4 (cap * 2)
              new <- newSmallArray newCap uninitChild
              copySmallMutableArray new 0 arr 0 n
              writeSmallArray new n (TBCText txt)
              writeByteArray countBuf 0 (n + 1)
              writeIORef ref (ChildVec countBuf new)
            else do
              writeSmallArray arr n (TBCText txt)
              writeByteArray countBuf 0 (n + 1)
    else do
      let cap = sizeofSmallMutableArray arr
      if cap == 0
        then do
          new <- newSmallArray 4 uninitChild
          writeSmallArray new 0 (TBCText txt)
          writeByteArray countBuf 0 (1 :: Int)
          writeIORef ref (ChildVec countBuf new)
        else do
          writeSmallArray arr 0 (TBCText txt)
          writeByteArray countBuf 0 (1 :: Int)


findChildIndex :: SmallMutableArray RealWorld TBChild -> Int -> TBNode -> IO (Maybe Int)
findChildIndex arr n target = go 0
  where
    go !i
      | i >= n = pure Nothing
      | otherwise = do
          child <- readSmallArray arr i
          case child of
            TBCElement node | node == target -> pure (Just i)
            _ -> go (i + 1)


removeChildFromVec :: IORef ChildVec -> TBNode -> IO ()
removeChildFromVec ref target = do
  ChildVec countBuf arr <- readIORef ref
  n <- readByteArray countBuf 0
  mIdx <- findChildIndex arr n target
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      let shift !i
            | i >= n - 1 = pure ()
            | otherwise = do
                next <- readSmallArray arr (i + 1)
                writeSmallArray arr i next
                shift (i + 1)
      shift idx
      writeSmallArray arr (n - 1) uninitChild
      writeByteArray countBuf 0 (n - 1)


insertChildBefore :: IORef ChildVec -> TBNode -> TBChild -> IO ()
insertChildBefore ref refNode newChild = do
  ChildVec countBuf arr <- readIORef ref
  n <- readByteArray countBuf 0
  mIdx <- findChildIndex arr n refNode
  let !idx = case mIdx of Just i -> i; Nothing -> n
  let cap = sizeofSmallMutableArray arr
  if n >= cap
    then do
      let !newCap = max 4 (cap + (cap `quot` 2))
      new <- newSmallArray newCap uninitChild
      copySmallMutableArray new 0 arr 0 n
      let shift !i
            | i <= idx = pure ()
            | otherwise = do
                prev <- readSmallArray new (i - 1)
                writeSmallArray new i prev
                shift (i - 1)
      shift n
      writeSmallArray new idx newChild
      writeByteArray countBuf 0 (n + 1)
      writeIORef ref (ChildVec countBuf new)
    else do
      let shift !i
            | i <= idx = pure ()
            | otherwise = do
                prev <- readSmallArray arr (i - 1)
                writeSmallArray arr i prev
                shift (i - 1)
      shift n
      writeSmallArray arr idx newChild
      writeByteArray countBuf 0 (n + 1)


transferChildren :: IORef ChildVec -> TBNode -> IORef ChildVec -> IO ()
transferChildren srcRef newParent dstRef = do
  ChildVec countBuf srcArr <- readIORef srcRef
  n <- readByteArray countBuf 0
  if n == 0
    then pure ()
    else do
      let go !i
            | i >= n = pure ()
            | otherwise = do
                child <- readSmallArray srcArr i
                case child of
                  TBCElement node -> writeIORef (nodeParent node) (Just newParent)
                  _ -> pure ()
                pushChild dstRef child
                go (i + 1)
      go 0
      writeByteArray countBuf 0 (0 :: Int)
      emptyArr <- newSmallArray 0 uninitChild
      writeIORef srcRef (ChildVec countBuf emptyArr)


childVecToList :: IORef ChildVec -> IO [TBChild]
childVecToList ref = do
  ChildVec countBuf arr <- readIORef ref
  n <- readByteArray countBuf 0
  let go !i !acc
        | i < 0 = pure acc
        | otherwise = do
            child <- readSmallArray arr i
            go (i - 1) (child : acc)
  go (n - 1) []


------------------------------------------------------------------------
-- Open element stack (SmallMutableArray; index 0 = bottom, count-1 = top)
------------------------------------------------------------------------

data ElementStack = ElementStack
  { esArr :: !(SmallMutableArray RealWorld TBNode)
  , esCount :: !(MutableByteArray RealWorld)
  , esTids :: !(MutableByteArray RealWorld)
  }


{-# INLINE packTidInfo #-}
packTidInfo :: TagId -> Bool -> Int
packTidInfo tid isHTML = unsafeShiftL (I# (dataToTag# tid)) 1 .|. (if isHTML then 1 else 0)


{-# INLINE tidFromPacked #-}
tidFromPacked :: Int -> TagId
tidFromPacked !packed = let !(I# i) = unsafeShiftR packed 1 in tagToEnum# i


{-# INLINE isHTMLFromPacked #-}
isHTMLFromPacked :: Int -> Bool
isHTMLFromPacked !packed = packed .&. 1 == 1


{-# INLINE packedTidIs #-}
packedTidIs :: Int -> TagId -> Bool
packedTidIs !packed tid = unsafeShiftR packed 1 == I# (dataToTag# tid)


{-# INLINE packedTidSatisfies #-}
packedTidSatisfies :: Int -> (TagId -> Bool) -> Bool
packedTidSatisfies !packed f = f (tidFromPacked packed)


{-# INLINE esReadTid #-}
esReadTid :: ElementStack -> Int -> IO Int
esReadTid (ElementStack _ _ tidsBuf) i = readByteArray tidsBuf i


newElementStack :: Int -> IO ElementStack
newElementStack !cap = do
  arr <- newSmallArray cap (error "uninit")
  countBuf <- newByteArray 8
  writeByteArray countBuf 0 (0 :: Int)
  tidsBuf <- newByteArray (cap * 8)
  pure (ElementStack arr countBuf tidsBuf)


{-# INLINE esPush #-}
esPush :: ElementStack -> TBNode -> IO ()
esPush (ElementStack arr countBuf tidsBuf) node = do
  n <- readByteArray countBuf 0
  let cap = sizeofSmallMutableArray arr
  if n >= cap
    then error "HTML.Parse: element stack overflow"
    else do
      writeSmallArray arr n node
      writeByteArray tidsBuf n (packTidInfo (nodeTagId node) (nodeIsHTMLNs node))
      writeByteArray countBuf 0 (n + 1)


{-# INLINE esPop #-}
esPop :: ElementStack -> IO ()
esPop (ElementStack _ countBuf _) = do
  n <- readByteArray countBuf 0 :: IO Int
  if n > 0
    then writeByteArray countBuf 0 (n - 1 :: Int)
    else pure ()


{-# INLINE esTop #-}
esTop :: ElementStack -> IO (Maybe TBNode)
esTop (ElementStack arr countBuf _) = do
  n <- readByteArray countBuf 0 :: IO Int
  if n > 0 then Just <$> readSmallArray arr (n - 1) else pure Nothing


{-# INLINE esTopUnsafe #-}
esTopUnsafe :: ElementStack -> IO TBNode
esTopUnsafe (ElementStack arr countBuf _) = do
  n <- readByteArray countBuf 0 :: IO Int
  readSmallArray arr (n - 1)


{-# INLINE esSize #-}
esSize :: ElementStack -> IO Int
esSize (ElementStack _ countBuf _) = readByteArray countBuf 0 :: IO Int


{-# INLINE esRead #-}
esRead :: ElementStack -> Int -> IO TBNode
esRead (ElementStack arr _ _) i = readSmallArray arr i


{-# INLINE esSetSize #-}
esSetSize :: ElementStack -> Int -> IO ()
esSetSize (ElementStack _ countBuf _) n = writeByteArray countBuf 0 n


esReadAll :: ElementStack -> IO [TBNode]
esReadAll (ElementStack arr countBuf _) = do
  n <- readByteArray countBuf 0 :: IO Int
  let go !i !acc
        | i >= n = pure acc
        | otherwise = do
            node <- readSmallArray arr i
            go (i + 1) (node : acc)
  go 0 []


esPopUntilInclusive :: ElementStack -> Text -> IO ()
esPopUntilInclusive es target = do
  let !targetTid = tagIdFromText target
      matchNode !tid node
        | tid /= TagUnknown = nodeTagId node == tid
        | otherwise = nodeName node == target
      loop = do
        n <- esSize es
        if n <= 0
          then pure ()
          else do
            top <- esRead es (n - 1)
            if nodeIsHTMLNs top && matchNode targetTid top
              then esPop es
              else do esPop es; loop
  loop


esRemoveNode :: ElementStack -> TBNode -> IO ()
esRemoveNode (ElementStack arr countBuf tidsBuf) target = do
  n <- readByteArray countBuf 0 :: IO Int
  go 0 n
  where
    go !i !n'
      | i >= n' = pure ()
      | otherwise = do
          node <- readSmallArray arr i
          if node == target then shift i n' else go (i + 1) n'
    shift !i !n'
      | i + 1 >= n' = writeByteArray countBuf 0 (n' - 1)
      | otherwise = do
          next <- readSmallArray arr (i + 1)
          writeSmallArray arr i next
          nextTid <- readByteArray tidsBuf (i + 1) :: IO Int
          writeByteArray tidsBuf i nextTid
          shift (i + 1) n'


esRemoveByName :: ElementStack -> Text -> IO ()
esRemoveByName (ElementStack arr countBuf tidsBuf) name = do
  n <- readByteArray countBuf 0 :: IO Int
  goFromTop (n - 1) n
  where
    goFromTop !i !n'
      | i < 0 = pure ()
      | otherwise = do
          node <- readSmallArray arr i
          if nodeName node == name
            then shiftDown i n'
            else goFromTop (i - 1) n'
    shiftDown !i !n' = do
      let go' !j
            | j + 1 >= n' = writeByteArray countBuf 0 (n' - 1)
            | otherwise = do
                next <- readSmallArray arr (j + 1)
                writeSmallArray arr j next
                nextTid <- readByteArray tidsBuf (j + 1) :: IO Int
                writeByteArray tidsBuf j nextTid
                go' (j + 1)
      go' i


esWriteList :: ElementStack -> [TBNode] -> IO ()
esWriteList (ElementStack arr countBuf tidsBuf) nodes = do
  let reversed = reverse nodes
      !n = length reversed
  go 0 reversed
  writeByteArray countBuf 0 n
  where
    go !_ [] = pure ()
    go !i (node : rest) = do
      writeSmallArray arr i node
      writeByteArray tidsBuf i (packTidInfo (nodeTagId node) (nodeIsHTMLNs node))
      go (i + 1) rest


{-# INLINE esElemInStack #-}
esElemInStack :: ElementStack -> TBNode -> IO Bool
esElemInStack es target = do
  n <- esSize es
  let go !i
        | i >= n = pure False
        | otherwise = do
            x <- esRead es i
            if x == target then pure True else go (i + 1)
  go 0


------------------------------------------------------------------------
-- Off-heap scalar store
------------------------------------------------------------------------

sMode, sOriginalMode, sFramesetOk, sInsertFromTable, sIgnoreLF, sHasSelect, sHasAF, sPOnStack :: Int
sMode = 0
sOriginalMode = 1
sFramesetOk = 2
sInsertFromTable = 3
sIgnoreLF = 4
sHasSelect = 5
sHasAF = 6
sPOnStack = 7


sTotalSlots :: Int
sTotalSlots = 8


{-# INLINE readScalar #-}
readScalar :: Ptr Int -> Int -> IO Int
readScalar = peekElemOff


{-# INLINE writeScalar #-}
writeScalar :: Ptr Int -> Int -> Int -> IO ()
writeScalar = pokeElemOff


{-# INLINE readMode #-}
readMode :: Ptr Int -> IO InsertionMode
readMode p = do
  I# i <- readScalar p sMode
  pure (tagToEnum# i)


{-# INLINE writeMode #-}
writeMode :: Ptr Int -> InsertionMode -> IO ()
writeMode p m = writeScalar p sMode (I# (dataToTag# m))


{-# INLINE readBoolSlot #-}
readBoolSlot :: Ptr Int -> Int -> IO Bool
readBoolSlot p slot = (/= 0) <$> readScalar p slot


{-# INLINE writeBoolSlot #-}
writeBoolSlot :: Ptr Int -> Int -> Bool -> IO ()
writeBoolSlot p slot b = writeScalar p slot (if b then 1 else 0)


------------------------------------------------------------------------
-- Tree builder state
------------------------------------------------------------------------

data TreeBuilder = TreeBuilder
  { tbScalars :: !(Ptr Int)
  , tbStack :: !ElementStack
  , tbActiveFormatting :: !(IORef [AFEntry])
  , tbHeadElement :: !(IORef (Maybe TBNode))
  , tbFormElement :: !(IORef (Maybe TBNode))
  , tbPendingTableText :: !(IORef [Text])
  , tbTemplateModes :: !(IORef [InsertionMode])
  , tbDocument :: !(IORef [ChildNode])
  , tbQuirksMode :: !(IORef Text)
  , tbScriptingEnabled :: !Bool
  , tbFragmentContext :: !(Maybe (Text, Maybe Text))
  , tbFragmentContextElement :: !(Maybe TBNode)
  , tbEmitEvents :: !Bool
  , tbBuildDOM :: !Bool
  , tbEventArr :: !(IORef (SmallMutableArray RealWorld TreeEvent))
  , tbEventCntBA :: !(MutableByteArray RealWorld)
  }


tbEmitEvent :: TreeBuilder -> TreeEvent -> IO ()
tbEmitEvent tb evt
  | not (tbEmitEvents tb) = pure ()
  | otherwise = tbPushEvent tb evt

tbPushEvent :: TreeBuilder -> TreeEvent -> IO ()
tbPushEvent tb evt = do
  n <- readByteArray (tbEventCntBA tb) 0 :: IO Int
  cap <- readByteArray (tbEventCntBA tb) 1 :: IO Int
  arr <- readIORef (tbEventArr tb)
  if n < cap
    then do
      writeSmallArray arr n evt
      writeByteArray (tbEventCntBA tb) 0 (n + 1 :: Int)
    else do
      let !newCap = cap * 2
      bigger <- newSmallArray newCap evt
      copySmallMutableArray bigger 0 arr 0 n
      writeIORef (tbEventArr tb) bigger
      writeSmallArray bigger n evt
      writeByteArray (tbEventCntBA tb) 0 (n + 1 :: Int)
      writeByteArray (tbEventCntBA tb) 1 newCap
{-# NOINLINE tbPushEvent #-}


tbGetEvents :: TreeBuilder -> IO (SmallArray TreeEvent)
tbGetEvents tb = do
  n <- readByteArray (tbEventCntBA tb) 0 :: IO Int
  arr <- readIORef (tbEventArr tb)
  freezeSmallArray arr 0 n


tbResetEvents :: TreeBuilder -> IO ()
tbResetEvents tb = writeByteArray (tbEventCntBA tb) 0 (0 :: Int)


{-# INLINE tbMode #-}
tbMode :: TreeBuilder -> IO InsertionMode
tbMode tb = readMode (tbScalars tb)


{-# INLINE tbSetMode #-}
tbSetMode :: TreeBuilder -> InsertionMode -> IO ()
tbSetMode tb = writeMode (tbScalars tb)


{-# INLINE tbOriginalMode #-}
tbOriginalMode :: TreeBuilder -> IO InsertionMode
tbOriginalMode tb = do
  I# i <- readScalar (tbScalars tb) sOriginalMode
  pure (tagToEnum# i)


{-# INLINE tbSetOriginalMode #-}
tbSetOriginalMode :: TreeBuilder -> InsertionMode -> IO ()
tbSetOriginalMode tb m = writeScalar (tbScalars tb) sOriginalMode (I# (dataToTag# m))


{-# INLINE tbFramesetOk #-}
tbFramesetOk :: TreeBuilder -> IO Bool
tbFramesetOk tb = readBoolSlot (tbScalars tb) sFramesetOk


{-# INLINE tbSetFramesetOk #-}
tbSetFramesetOk :: TreeBuilder -> Bool -> IO ()
tbSetFramesetOk tb = writeBoolSlot (tbScalars tb) sFramesetOk


{-# INLINE tbInsertFromTable #-}
tbInsertFromTable :: TreeBuilder -> IO Bool
tbInsertFromTable tb = readBoolSlot (tbScalars tb) sInsertFromTable


{-# INLINE tbSetInsertFromTable #-}
tbSetInsertFromTable :: TreeBuilder -> Bool -> IO ()
tbSetInsertFromTable tb = writeBoolSlot (tbScalars tb) sInsertFromTable


{-# INLINE tbIgnoreLF #-}
tbIgnoreLF :: TreeBuilder -> IO Bool
tbIgnoreLF tb = readBoolSlot (tbScalars tb) sIgnoreLF


{-# INLINE tbSetIgnoreLF #-}
tbSetIgnoreLF :: TreeBuilder -> Bool -> IO ()
tbSetIgnoreLF tb = writeBoolSlot (tbScalars tb) sIgnoreLF


{-# INLINE tbHasSelect #-}
tbHasSelect :: TreeBuilder -> IO Bool
tbHasSelect tb = readBoolSlot (tbScalars tb) sHasSelect


{-# INLINE tbSetHasSelect #-}
tbSetHasSelect :: TreeBuilder -> Bool -> IO ()
tbSetHasSelect tb = writeBoolSlot (tbScalars tb) sHasSelect


data AFEntry
  = AFMarker
  | AFEntry !Text !(SmallArray HTMLAttribute) !TBNode
  deriving (Eq)


------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

parseHTML :: ByteString -> HTMLDocument
parseHTML bs = unsafePerformIO $ do
  tb <- newTreeBuilder Nothing
  tokenizeBSIO bs 0 (BS.length bs) 0 False tb
  processToken tb TEOF
  !doc <- buildDocument tb
  free (tbScalars tb)
  pure doc


parseHTMLNodes :: ByteString -> [HTMLNode]
parseHTMLNodes bs = unsafePerformIO $ do
  tb <- newTreeBuilder Nothing
  tokenizeBSIO bs 0 (BS.length bs) 0 False tb
  processToken tb TEOF
  nodes <- buildAllNodes tb
  free (tbScalars tb)
  if any hasSelectElement nodes
    then pure (map populateSelectedContent nodes)
    else pure nodes


treeBuildOnlyIO :: ByteString -> IO ()
treeBuildOnlyIO bs = do
  tb <- newTreeBuilder Nothing
  tokenizeBSIO bs 0 (BS.length bs) 0 False tb
  processToken tb TEOF
  free (tbScalars tb)
{-# NOINLINE treeBuildOnlyIO #-}


freeTreeBuilder :: TreeBuilder -> IO ()
freeTreeBuilder tb = free (tbScalars tb)


drainTreeBuilderStack :: TreeBuilder -> IO ()
drainTreeBuilderStack tb = do
  let es@(ElementStack esArr esCnt _) = tbStack tb
  n <- readByteArray esCnt 0 :: IO Int
  go esArr (n - 1)
  where
    go _ i | i < 0 = pure ()
    go arr i = do
      node <- readSmallArray arr i
      tbEmitEvent tb (TreeClose (nodeName node))
      go arr (i - 1)


tokenizeOnlyIO :: ByteString -> IO Int
tokenizeOnlyIO bs = do
  let !len = BS.length bs
      !(BS (ForeignPtr addr# _) _) = bs
  countRef <- newIORef (0 :: Int)
  let !sharedBA = case runRW#
        ( \s0 ->
            case newByteArray# len# s0 of
              (# s1, mba# #) ->
                case copyAddrToByteArray# addr# mba# 0# len# s1 of
                  s2 ->
                    case unsafeFreezeByteArray# mba# s2 of
                      (# s3, ba# #) ->
                        (# s3, ba# #)
        ) of
        (# _, ba# #) -> ByteArray ba#
        where
          !(I# len#) = len
  go sharedBA addr# countRef bs 0 len
  readIORef countRef
  where
    go !_ba addr# !countRef !bs !off !len
      | off >= len = pure ()
      | otherwise =
          let !b = readByteOff addr# off
          in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
              then do
                let !end = scanTextFast addr# (off + 1) len
                modifyIORef' countRef (+ 1)
                go _ba addr# countRef bs end len
              else case b of
                0x3C
                  | off + 1 < len ->
                      let !b2 = readByteOff addr# (off + 1)
                      in if isAlphaByte b2
                          then do
                            let !nameEnd = scanTagName bs (off + 1) len
                                !afterTag = skipAttrsBS bs nameEnd len
                            modifyIORef' countRef (+ 1)
                            go _ba addr# countRef bs afterTag len
                          else
                            if b2 == 0x2F && off + 2 < len && isAlphaByte (readByteOff addr# (off + 2))
                              then do
                                let !nameEnd = scanTagName bs (off + 2) len
                                    !afterGt = skipToGtBS bs nameEnd len
                                modifyIORef' countRef (+ 1)
                                go _ba addr# countRef bs afterGt len
                              else do
                                modifyIORef' countRef (+ 1)
                                go _ba addr# countRef bs (off + 1) len
                _ -> do
                  modifyIORef' countRef (+ 1)
                  go _ba addr# countRef bs (off + 1) len
{-# NOINLINE tokenizeOnlyIO #-}


-- | Raw event tokenizer: scans HTML bytes and emits TreeEvents directly
-- without running the HTML5 tree construction algorithm. No mode tracking,
-- no element stack, no implicit elements, no adoption agency.
tokenizeRawEventsIO :: ByteString -> IO (SmallArray TreeEvent)
tokenizeRawEventsIO !bs = do
  let !len = BS.length bs
      !(BS (ForeignPtr addr# _) _) = bs
      !sharedBA = case runRW#
        ( \s0 ->
            case newByteArray# len# s0 of
              (# s1, mba# #) ->
                case copyAddrToByteArray# addr# mba# 0# len# s1 of
                  s2 ->
                    case unsafeFreezeByteArray# mba# s2 of
                      (# s3, ba# #) ->
                        (# s3, ba# #)
        ) of
        (# _, ba# #) -> ByteArray ba#
        where
          !(I# len#) = len
  let !initCap = max 256 (len `quot` 10)
  arr0 <- newSmallArray initCap (TreeClose "")
  evtArrRef <- newIORef arr0
  let
    {-# INLINE push #-}
    push :: Int -> TreeEvent -> IO Int
    push !n !evt
      | n < initCap = do
          writeSmallArray arr0 n evt
          pure (n + 1)
      | otherwise = pushOverflow n evt
    pushOverflow :: Int -> TreeEvent -> IO Int
    pushOverflow !n !evt = do
      arr <- readIORef evtArrRef
      cap <- getSizeofSmallMutableArray arr
      arr' <- if n < cap
        then pure arr
        else do
          bigger <- newSmallArray (cap * 2) evt
          copySmallMutableArray bigger 0 arr 0 n
          writeIORef evtArrRef bigger
          pure bigger
      writeSmallArray arr' n evt
      pure (n + 1)
    {-# NOINLINE pushOverflow #-}

    pushMany :: Int -> [Token] -> IO Int
    pushMany !n [] = pure n
    pushMany !n (tok : rest) = do
      n' <- case tok of
        TStartTag name attrs sc tid -> do
          n1 <- push n (TreeOpen name attrs)
          if sc || tagIdIsVoid tid then push n1 (TreeClose name) else pure n1
        TEndTag name _ -> push n (TreeClose name)
        TString txt -> push n (TreeText txt)
        TChar c -> push n (TreeText (T.singleton c))
        TComment txt -> push n (TreeComment txt)
        TDoctype name pub sys _ -> push n (TreeDoctype name pub sys)
        TEOF -> pure n
      pushMany n' rest

    go :: Int -> Int -> IO Int
    go !n !off
      | off >= len = pure n
      | otherwise =
          let !b = readByteOff addr# off
          in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
              then do
                let !(ScanTextResult end ascii) = scanTextAscii addr# (off + 1) len
                    !firstByteAscii = b < 0x80
                    !allAscii = firstByteAscii && ascii
                    !t = decodeTextSliceKnown sharedBA off (end - off) bs allAscii
                n' <- push n (TreeText t)
                go n' end
              else case b of
                0x3C -> goTag n (off + 1)
                0x26 -> do
                  let !windowEnd = min len (off + 65)
                      !input = toStringFrom bs (off + 1) windowEnd
                      (ent, rest) = parseEntityRef input
                      !consumed = length input - length rest
                  n' <- pushMany n (map TChar ent)
                  go n' (off + 1 + consumed)
                0x00 -> do
                  n' <- push n (TreeText (T.singleton '\0'))
                  go n' (off + 1)
                0x0D -> do
                  n' <- push n (TreeText (T.singleton '\n'))
                  let !next = off + 1
                  go n' (if next < len && readByteOff addr# next == 0x0A then next + 1 else next)
                _ -> go n (off + 1)

    goTag :: Int -> Int -> IO Int
    goTag !n !off
      | off >= len = push n (TreeText "<")
      | otherwise = case readByteOff addr# off of
          0x21 -> do
            let toks = tokenizeMarkupDeclCtx 0 False (toStringFrom bs (off + 1) len)
            pushMany n toks
          0x2F -> goEndTag n (off + 1)
          0x3F -> do
            let (comment, remaining) = readUntilStr ">" (toStringFrom bs (off + 1) len)
            n' <- push n (TreeComment (T.pack ('?' : comment)))
            let toks = tokenizeCtx 0 False remaining
            pushMany n' toks
          b | isAlphaByte b -> goStartTag n off
          _ -> do
            n' <- push n (TreeText "<")
            go n' off

    goStartTag :: Int -> Int -> IO Int
    goStartTag !n !off =
      let !nameEnd = scanTagNameFast addr# off len
          !tagLen = nameEnd - off
          (# lcName, tid #) = internTagAddrU addr# off tagLen bs
          (!attrs, !selfClose, !afterTag) = readTagAttrsBS sharedBA bs nameEnd len
      in if afterTag > len
          then pure n
          else do
            n' <- push n (TreeOpen lcName attrs)
            n'' <- if selfClose || tagIdIsVoid tid
              then push n' (TreeClose lcName)
              else pure n'
            if isRawTextTag tid && not selfClose
              then pushMany n'' (tokenizeRawText (toStringFrom bs afterTag len) lcName)
              else
                if isRCDataTag tid && not selfClose
                  then pushMany n'' (tokenizeRCData (toStringFrom bs afterTag len) lcName)
                  else
                    if tid == TagPlaintext
                      then pushMany n'' (tokenizePlaintext (toStringFrom bs afterTag len))
                      else go n'' afterTag

    goEndTag :: Int -> Int -> IO Int
    goEndTag !n !off
      | off >= len = do n' <- push n (TreeText "<"); push n' (TreeText "/")
      | isAlphaByte (readByteOff addr# off) =
          let !nameEnd = scanTagNameFast addr# off len
              !tagLen = nameEnd - off
              (# lcName, _tid #) = internTagAddrU addr# off tagLen bs
              !afterGt = skipToGtBS bs nameEnd len
          in do
            n' <- push n (TreeClose lcName)
            go n' afterGt
      | readByteOff addr# off == 0x3E = do
          n' <- push n (TreeComment "")
          go n' (off + 1)
      | otherwise = do
          let (comment, remaining) = readUntilStr ">" (toStringFrom bs off len)
          n' <- push n (TreeComment (T.pack comment))
          pushMany n' (tokenizeCtx 0 False remaining)
  finalN <- go 0 0
  arr <- readIORef evtArrRef
  freezeSmallArray arr 0 finalN
{-# NOINLINE tokenizeRawEventsIO #-}


-- | Count tokens in a chunk without allocating Token objects.
-- Uses the same fast-scan approach as tokenizeOnlyIO.
tokenizeCountChunk :: ByteString -> IO Int
tokenizeCountChunk bs = do
  let !len = BS.length bs
      !(BS (ForeignPtr addr# _) _) = bs
  countRef <- newIORef (0 :: Int)
  let go !off
        | off >= len = pure ()
        | otherwise =
            let !b = readByteOff addr# off
            in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
                then do
                  let !end = scanTextFast addr# (off + 1) len
                  modifyIORef' countRef (+ 1)
                  go end
                else case b of
                  0x3C
                    | off + 1 < len ->
                        let !b2 = readByteOff addr# (off + 1)
                        in if isAlphaByte b2
                            then do
                              let !nameEnd = scanTagName bs (off + 1) len
                                  !afterTag = skipAttrsBS bs nameEnd len
                              modifyIORef' countRef (+ 1)
                              go afterTag
                            else
                              if b2 == 0x2F && off + 2 < len && isAlphaByte (readByteOff addr# (off + 2))
                                then do
                                  let !nameEnd = scanTagName bs (off + 2) len
                                      !afterGt = skipToGtBS bs nameEnd len
                                  modifyIORef' countRef (+ 1)
                                  go afterGt
                                else do
                                  modifyIORef' countRef (+ 1)
                                  go (off + 1)
                  _ -> do
                    modifyIORef' countRef (+ 1)
                    go (off + 1)
  go 0
  readIORef countRef
{-# NOINLINE tokenizeCountChunk #-}


parseHTMLFragment :: Text -> Maybe Text -> ByteString -> [HTMLNode]
parseHTMLFragment contextTag contextNs bs = unsafePerformIO $ do
  let txt = TE.decodeUtf8Lenient bs
      tokens = coalesceTokens (fragmentTokenize contextTag contextNs txt)
  tb <- newTreeBuilder (Just (contextTag, contextNs))
  mapM_ (processToken tb) tokens
  processToken tb TEOF
  buildFragmentResult tb


coalesceTokens :: [Token] -> [Token]
coalesceTokens [] = []
coalesceTokens (TChar '\0' : rest) = TChar '\0' : coalesceTokens rest
coalesceTokens (TChar c : rest) = gatherChars [c] rest
  where
    gatherChars !acc [] = [TString (T.pack (reverse acc))]
    gatherChars !acc (TChar '\0' : rs) = TString (T.pack (reverse acc)) : TChar '\0' : coalesceTokens rs
    gatherChars !acc (TChar c' : rs) = gatherChars (c' : acc) rs
    gatherChars !acc (TString t : rs) = TString (T.pack (reverse acc) <> t) : coalesceTokens rs
    gatherChars !acc rs = TString (T.pack (reverse acc)) : coalesceTokens rs
coalesceTokens (TString t1 : TString t2 : rest) = coalesceTokens (TString (t1 <> t2) : rest)
coalesceTokens (TString t1 : TChar '\0' : rest) = TString t1 : TChar '\0' : coalesceTokens rest
coalesceTokens (TString t1 : TChar c : rest) = coalesceTokens (TString (T.snoc t1 c) : rest)
coalesceTokens (t : rest) = t : coalesceTokens rest


fragmentTokenize :: Text -> Maybe Text -> Text -> [Token]
fragmentTokenize ctx ctxNs txt
  | isForeignNs = tokenize txt
  | ctx `elem` ["title", "textarea"] = tokenizeRCData (T.unpack txt) ctx
  | ctx `elem` ["style", "xmp", "iframe", "noembed", "noframes", "noscript"] =
      tokenizePlaintext (T.unpack (normCR txt))
  | ctx == "script" =
      tokenizePlaintext (T.unpack (normCR txt))
  | ctx == "plaintext" = tokenizePlaintext (T.unpack txt)
  | otherwise = tokenize txt
  where
    isForeignNs = case ctxNs of
      Just ns -> ns == "svg" || ns == "math"
      Nothing -> False
    normCR t = T.replace "\r\n" "\n" (T.replace "\r" "\n" t)


------------------------------------------------------------------------
-- Initialize tree builder
------------------------------------------------------------------------

newTreeBuilder :: Maybe (Text, Maybe Text) -> IO TreeBuilder
newTreeBuilder = newTreeBuilderWith False True 0



newTreeBuilderWith :: Bool -> Bool -> Int -> Maybe (Text, Maybe Text) -> IO TreeBuilder
newTreeBuilderWith emitting buildDOM initCap mCtx = do
  scalars <- mallocBytes (sTotalSlots * 8)
  writeScalar scalars sMode (fromEnum MInitial)
  writeScalar scalars sOriginalMode (fromEnum MInitial)
  writeScalar scalars sFramesetOk 1
  writeScalar scalars sInsertFromTable 0
  writeScalar scalars sIgnoreLF 0
  writeScalar scalars sHasSelect 0
  writeScalar scalars sHasAF 0
  writeScalar scalars sPOnStack 0
  stack <- newElementStack 256
  afRef <- newIORef []
  headRef <- newIORef Nothing
  formRef <- newIORef Nothing
  pendRef <- newIORef []
  tmRef <- newIORef []
  docRef <- newIORef []
  qmRef <- newIORef "no-quirks"
  let !arrCap = max 1 initCap
  evtArr <- newSmallArray arrCap (TreeClose "") >>= newIORef
  evtCntBA <- newByteArray 16
  writeByteArray evtCntBA 0 (0 :: Int)
  writeByteArray evtCntBA 1 arrCap
  let tb0 =
        TreeBuilder
          scalars
          stack
          afRef
          headRef
          formRef
          pendRef
          tmRef
          docRef
          qmRef
          True
          mCtx
          Nothing
          emitting
          buildDOM
          evtArr
          evtCntBA
  case mCtx of
    Nothing -> pure tb0
    Just (ctxTag, ctxNs) -> do
      htmlNode <- newTBNode tb0 "html" TagHtml emptySmallArray Nothing False
      when (tbBuildDOM tb0) $ modifyIORef' docRef (++ [CElement htmlNode])
      esPush stack htmlNode
      mCtxElem <- case ctxNs of
        Just ns | ns == "svg" || ns == "math" -> do
          let adjustedName = if ns == "svg" then adjustSVGTagName ctxTag else ctxTag
          ctxNode <- newTBNode tb0 adjustedName (tagIdFromText adjustedName) emptySmallArray ctxNs False
          when (tbBuildDOM tb0) $ appendChild htmlNode ctxNode
          esWriteList stack [ctxNode, htmlNode]
          tbSetMode tb0 MInBody
          pure (Just ctxNode)
        _ -> do
          let mode0 = resetInsertionModeForContext ctxTag ctxNs
          tbSetMode tb0 mode0
          pure Nothing
      tbSetFramesetOk tb0 False
      let tb = tb0 {tbFragmentContextElement = mCtxElem}
      pure tb


resetInsertionModeForContext :: Text -> Maybe Text -> InsertionMode
resetInsertionModeForContext name _ns =
  let !tid = tagIdFromText name
  in case tid of
      TagTd -> MInCell
      TagTh -> MInCell
      TagTr -> MInRow
      TagTbody -> MInTableBody
      TagThead -> MInTableBody
      TagTfoot -> MInTableBody
      TagCaption -> MInCaption
      TagColgroup -> MInColumnGroup
      TagTable -> MInTable
      TagTemplate -> MInTemplate
      TagHead -> MInBody
      TagBody -> MInBody
      TagFrameset -> MInFrameset
      TagHtml -> MBeforeHead
      TagSelect -> MInSelect
      _ -> MInBody


{-# INLINE newTBNode #-}
newTBNode :: TreeBuilder -> Text -> TagId -> SmallArray HTMLAttribute -> Maybe Text -> Bool -> IO TBNode
newTBNode tb name tid attrs ns isTmpl = do
  let !htmlNs = ns == Nothing || ns == Just "" || ns == Just "html"
  if tbBuildDOM tb
    then do
      attrRef <- newIORef attrs
      childVecRef <- if tagIdIsVoid tid then pure emptyChildVecRef else newChildVec (initialChildCap tid)
      parentRef <- newIORef Nothing
      tmplRef <- if isTmpl then newChildVec 2 else pure emptyChildVecRef
      pure $!
        TBNode
          { nodeName = name
          , nodeTagId = tid
          , nodeAttrs = attrRef
          , nodeNs = ns
          , nodeIsHTMLNs = htmlNs
          , nodeIsTemplate = isTmpl
          , nodeChildren = childVecRef
          , nodeParent = parentRef
          , nodeTemplateContents = tmplRef
          }
    else
      pure $!
        TBNode
          { nodeName = name
          , nodeTagId = tid
          , nodeAttrs = dummyAttrsRef
          , nodeNs = ns
          , nodeIsHTMLNs = htmlNs
          , nodeIsTemplate = isTmpl
          , nodeChildren = emptyChildVecRef
          , nodeParent = dummyParentRef
          , nodeTemplateContents = emptyChildVecRef
          }


initialChildCap :: TagId -> Int
initialChildCap tid = case tid of
  TagBody -> 32
  TagHead -> 16
  TagHtml -> 3
  TagUl -> 16
  TagOl -> 16
  TagDl -> 8
  TagSelect -> 16
  TagOptgroup -> 8
  TagTable -> 8
  TagTbody -> 8
  TagThead -> 4
  TagTfoot -> 4
  TagTr -> 8
  TagDiv -> 16
  TagSpan -> 2
  TagP -> 2
  TagLi -> 4
  TagNav -> 8
  TagMain -> 8
  TagSection -> 8
  TagArticle -> 8
  TagForm -> 8
  TagFieldset -> 8
  _ -> 4
{-# INLINE initialChildCap #-}


{-# INLINE readNodeAttrs #-}
readNodeAttrs :: TBNode -> IO (SmallArray HTMLAttribute)
readNodeAttrs node = readIORef (nodeAttrs node)


{-# INLINE attrLookup #-}
attrLookup :: Text -> SmallArray HTMLAttribute -> Maybe Text
attrLookup name vec = go 0
  where
    !len = sizeofSmallArray vec
    go !i
      | i >= len = Nothing
      | otherwise =
          let !(HTMLAttribute n v) = indexSmallArray vec i
          in if n == name then Just v else go (i + 1)


{-# INLINE attrsFromList #-}
attrsFromList :: [(Text, Text)] -> SmallArray HTMLAttribute
attrsFromList [] = emptySmallArray
attrsFromList [(n, v)] = createSmallArray 1 (HTMLAttribute n v) (\_ -> pure ())
attrsFromList xs = smallArrayFromList (map (\(n, v) -> HTMLAttribute n v) xs)


------------------------------------------------------------------------
-- Build final document
------------------------------------------------------------------------

buildDocument :: TreeBuilder -> IO HTMLDocument
buildDocument tb = do
  allNodes <- buildAllNodes tb
  let mdt = extractDoctype allNodes
      root = findOrCreateRoot allNodes
  pure (HTMLDocument mdt root)


buildAllNodes :: TreeBuilder -> IO [HTMLNode]
buildAllNodes tb = do
  docNodes <- readIORef (tbDocument tb)
  sz <- esSize (tbStack tb)
  rootFromStack <- if sz > 0 then Just <$> esRead (tbStack tb) 0 else pure Nothing
  flatResult <- mapM (buildDocChild rootFromStack) docNodes
  case rootFromStack of
    Just root | not (hasElement docNodes) -> do
      r <- tbNodeToHTMLNode root
      pure (flatResult ++ [r])
    _ -> pure flatResult
  where
    hasElement [] = False
    hasElement (CElement _ : _) = True
    hasElement (_ : rest) = hasElement rest
    buildDocChild _mRoot (CElement node) = tbNodeToHTMLNode node
    buildDocChild _ cn = childToHTMLNode cn


buildFragmentResult :: TreeBuilder -> IO [HTMLNode]
buildFragmentResult tb = do
  docNodes <- readIORef (tbDocument tb)
  let htmlRoot = findHtmlInDoc docNodes
  case htmlRoot of
    Nothing -> mapM childToHTMLNode docNodes
    Just root -> do
      case tbFragmentContextElement tb of
        Just ctxElem -> do
          ctxCs <- childVecToList (nodeChildren ctxElem)
          rootCs <- childVecToList (nodeChildren root)
          let isCtx (TBCElement n) = n == ctxElem
              isCtx _ = False
              allChildren = ctxCs ++ filter (not . isCtx) rootCs
          mapM tbChildToHTMLNode allChildren
        Nothing -> do
          cs <- childVecToList (nodeChildren root)
          mapM tbChildToHTMLNode cs
  where
    findHtmlInDoc [] = Nothing
    findHtmlInDoc (CElement node : _) | nodeTagId node == TagHtml = Just node
    findHtmlInDoc (_ : rest) = findHtmlInDoc rest


{-# INLINE tbChildToHTMLNode #-}
tbChildToHTMLNode :: TBChild -> IO HTMLNode
tbChildToHTMLNode (TBCElement node) = tbNodeToHTMLNode node
tbChildToHTMLNode (TBCText t) = pure (HTMLText t)
tbChildToHTMLNode (TBCComment t) = pure (HTMLComment t)


{- | Convert a mutable TBNode to an immutable HTMLNode.

Uses in-place conversion: TBCText\/TBCComment have identical memory layout
to HTMLText\/HTMLComment (same constructor tag and arity), so they are
reinterpreted via unsafeCoerce without allocation.  TBCElement entries are
recursively converted and overwritten.  The mutable array is then shrunk to
the live count and frozen in place — zero SmallArray body allocation.
-}
tbNodeToHTMLNode :: TBNode -> IO HTMLNode
tbNodeToHTMLNode node
  | nodeIsTemplate node = do
      ChildVec tcb tarr <- readIORef (nodeTemplateContents node)
      tn <- readByteArray tcb 0
      if tn > 0
        then buildElementInPlace tcb tarr tn
        else do
          ChildVec ccb carr <- readIORef (nodeChildren node)
          cn <- readByteArray ccb 0
          buildElementInPlace ccb carr cn
  | otherwise = do
      ChildVec ccb carr <- readIORef (nodeChildren node)
      cn <- readByteArray ccb 0
      buildElementInPlace ccb carr cn
  where
    buildElementInPlace !ccb !carr !cn = do
      attrs <- readIORef (nodeAttrs node)
      childArr <-
        if cn == 0
          then pure mempty
          else do
            ec <- readByteArray ccb 1 :: IO Int
            let !htmlArr = unsafeCoerce carr :: SmallMutableArray RealWorld HTMLNode
            if ec == 0
              then do
                shrinkSmallMutableArray htmlArr cn
                unsafeFreezeSmallArray htmlArr
              else do
                let go !i
                      | i >= cn = do
                          shrinkSmallMutableArray htmlArr cn
                          unsafeFreezeSmallArray htmlArr
                      | otherwise = do
                          child <- readSmallArray carr i
                          case child of
                            TBCElement n -> do
                              h <- tbNodeToHTMLNode n
                              writeSmallArray htmlArr i h
                            _ -> pure ()
                          go (i + 1)
                go 0
      let !displayName =
            if nodeIsHTMLNs node
              then nodeName node
              else nameWithNs (nodeName node) (nodeNs node)
      pure $! HTMLElement displayName attrs childArr


childToHTMLNode :: ChildNode -> IO HTMLNode
childToHTMLNode (CElement node) = tbNodeToHTMLNode node
childToHTMLNode (CText t) = pure (HTMLText t)
childToHTMLNode (CComment t) = pure (HTMLComment t)
childToHTMLNode (CDoctype n p s) = pure (HTMLDoctype n p s)


populateSelectedContent :: HTMLNode -> HTMLNode
populateSelectedContent node@(HTMLElement tag attrs children)
  | tag == "select" =
      let children' = mapSmallArray' populateSelectedContent children
          mSelectedContent = findSelectedContentInSelect children'
          options = findOptionsList children'
          selectedOpt = case filter hasSelectedAttr options of
            (opt : _) -> opt
            [] -> case options of
              (opt : _) -> opt
              [] -> HTMLText ""
      in case mSelectedContent of
          Just _ ->
            let newChildren = mapSmallArray' (fillSelectedContent (getOptionChildren selectedOpt)) children'
            in HTMLElement tag attrs newChildren
          Nothing -> HTMLElement tag attrs children'
  | sizeofSmallArray children == 0 = node
  | otherwise =
      let !children' = mapSmallArray' populateSelectedContent children
      in HTMLElement tag attrs children'
populateSelectedContent other = other


hasSelectElement :: HTMLNode -> Bool
hasSelectElement (HTMLElement "select" _ _) = True
hasSelectElement (HTMLElement _ _ cs) = any hasSelectElement cs
hasSelectElement _ = False


findSelectedContentInSelect :: SmallArray HTMLNode -> Maybe HTMLNode
findSelectedContentInSelect children =
  let found = concatMap findSC (toList children)
  in case found of [] -> Nothing; (x : _) -> Just x
  where
    findSC (HTMLElement t _ cs)
      | t == "selectedcontent" = [HTMLElement t mempty cs]
      | otherwise = concatMap findSC (toList cs)
    findSC _ = []


findOptionsList :: SmallArray HTMLNode -> [HTMLNode]
findOptionsList children = concatMap findOpt (toList children)
  where
    findOpt node@(HTMLElement t _ cs)
      | t == "option" = [node]
      | otherwise = concatMap findOpt (toList cs)
    findOpt _ = []


hasSelectedAttr :: HTMLNode -> Bool
hasSelectedAttr (HTMLElement _ attrs _) =
  any (\(HTMLAttribute n _) -> n == "selected") attrs
hasSelectedAttr _ = False


getOptionChildren :: HTMLNode -> SmallArray HTMLNode
getOptionChildren (HTMLElement _ _ cs) = cs
getOptionChildren _ = mempty


fillSelectedContent :: SmallArray HTMLNode -> HTMLNode -> HTMLNode
fillSelectedContent optChildren (HTMLElement tag attrs children)
  | tag == "selectedcontent" = HTMLElement tag attrs optChildren
  | otherwise = HTMLElement tag attrs (mapSmallArray' (fillSelectedContent optChildren) children)
fillSelectedContent _ other = other


extractDoctype :: [HTMLNode] -> Maybe Doctype
extractDoctype [] = Nothing
extractDoctype (HTMLDoctype n p s : _) = Just (Doctype (Just n) p s)
extractDoctype (_ : rest) = extractDoctype rest


findOrCreateRoot :: [HTMLNode] -> HTMLNode
findOrCreateRoot = go
  where
    go [] = HTMLElement "html" mempty mempty
    go (n@(HTMLElement "html" _ _) : _) = n
    go (_ : rest) = go rest


------------------------------------------------------------------------
-- Process a single token
------------------------------------------------------------------------

processToken :: TreeBuilder -> Token -> IO ()
processToken !tb tok = do
  ignoreLF <- tbIgnoreLF tb
  if ignoreLF
    then do
      tbSetIgnoreLF tb False
      case tok of
        TChar '\n' -> pure ()
        TString t -> case T.uncons t of
          Just ('\n', rest) | T.null rest -> pure ()
          Just ('\n', rest) -> dispatchToken tb (TString rest)
          _ -> dispatchToken tb tok
        _ -> dispatchToken tb tok
    else dispatchToken tb tok


dispatchToken :: TreeBuilder -> Token -> IO ()
dispatchToken tb tok = do
  mCurrent <- esTop (tbStack tb)
  case mCurrent of
    Nothing -> processInMode tb tok
    Just current ->
      let ns = nodeNs current
      in if ns == Nothing || ns == Just "" || ns == Just "html"
          then processInMode tb tok
          else do
            useForeign <- shouldUseForeignContent tb tok current
            if useForeign
              then processForeignContent tb tok
              else processInMode tb tok


shouldUseForeignContent :: TreeBuilder -> Token -> TBNode -> IO Bool
shouldUseForeignContent tb tok current = do
  let ns = nodeNs current
      name = nodeName current
  case ns of
    Nothing -> pure False
    Just "html" -> pure False
    Just "" -> pure False
    Just nsVal -> case tok of
      TEOF -> pure False
      TChar _ -> do
        if isMathMLTIP name nsVal
          then pure False
          else do
            hip <- isHTMLIP current nsVal
            if hip then pure False else pure True
      TString _ -> do
        if isMathMLTIP name nsVal
          then pure False
          else do
            hip <- isHTMLIP current nsVal
            if hip then pure False else pure True
      TStartTag tname _ _ _ -> do
        if isMathMLTIP name nsVal && tname /= "mglyph" && tname /= "malignmark"
          then pure False
          else
            if name == "annotation-xml" && nsVal == "math" && tname == "svg"
              then pure False
              else do
                hip <- isHTMLIP current nsVal
                if hip then pure False else pure True
      TComment _ -> pure True
      TEndTag _ _ -> pure True
      _ -> pure True
  where
    isMathMLTIP n ns = ns == "math" && n `elem` ["mi", "mo", "mn", "ms", "mtext"]
    isHTMLIP node ns
      | ns == "svg" = pure $ nodeName node `elem` ["foreignObject", "desc", "title"]
      | ns == "math" && nodeName node == "annotation-xml" = do
          attrs <- readNodeAttrs node
          pure $ case attrLookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html", "application/xhtml+xml"]
            Nothing -> False
      | otherwise = pure False


processInMode :: TreeBuilder -> Token -> IO ()
processInMode tb tok = do
  mode <- tbMode tb
  case tok of
    TString _ -> case mode of
      MInBody -> modeInBody tb tok
      MText -> modeText tb tok
      MInCell -> modeInCell tb tok
      MInTableText -> modeInTableText tb tok
      _ -> expandStringToken tb tok
    _ -> dispatchToMode mode tb tok


dispatchToMode :: InsertionMode -> TreeBuilder -> Token -> IO ()
dispatchToMode mode tb tok = case mode of
  MInitial -> modeInitial tb tok
  MBeforeHtml -> modeBeforeHtml tb tok
  MBeforeHead -> modeBeforeHead tb tok
  MInHead -> modeInHead tb tok
  MInHeadNoscript -> modeInHeadNoscript tb tok
  MAfterHead -> modeAfterHead tb tok
  MInBody -> modeInBody tb tok
  MText -> modeText tb tok
  MInTable -> modeInTable tb tok
  MInTableText -> modeInTableText tb tok
  MInCaption -> modeInCaption tb tok
  MInColumnGroup -> modeInColumnGroup tb tok
  MInTableBody -> modeInTableBody tb tok
  MInRow -> modeInRow tb tok
  MInCell -> modeInCell tb tok
  MInSelect -> modeInSelect tb tok
  MInSelectInTable -> modeInSelectInTable tb tok
  MInTemplate -> modeInTemplate tb tok
  MAfterBody -> modeAfterBody tb tok
  MInFrameset -> modeInFrameset tb tok
  MAfterFrameset -> modeAfterFrameset tb tok
  MAfterAfterBody -> modeAfterAfterBody tb tok
  MAfterAfterFrameset -> modeAfterAfterFrameset tb tok


expandStringToken :: TreeBuilder -> Token -> IO ()
expandStringToken tb (TString t) = mapM_ (\c -> processToken tb (TChar c)) (T.unpack t)
expandStringToken _ _ = pure ()


------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Scope checking
------------------------------------------------------------------------

hasElementInScopeT :: TagId -> Text -> (TagId -> Bool) -> Bool -> TreeBuilder -> IO Bool
hasElementInScopeT !targetTid target termCheck checkIntegrationPoints tb = do
  let es = tbStack tb
  sz <- esSize es
  go es (sz - 1)
  where
    go _es !i | i < 0 = pure False
    go es !i = do
      packed <- esReadTid es i
      let !tid = tidFromPacked packed
          !isHTML = isHTMLFromPacked packed
      if isHTML
        then
          if not (packedTidIs packed TagUnknown)
            then
              if packedTidIs packed targetTid
                then pure True
                else
                  if termCheck tid
                    then pure False
                    else go es (i - 1)
            else do
              node <- esRead es i
              if nodeName node == target
                then pure True
                else go es (i - 1)
        else
          if checkIntegrationPoints
            then do
              node <- esRead es i
              isIP <- isForeignScopeTerminator node
              if isIP then pure False else go es (i - 1)
            else go es (i - 1)
    isForeignScopeTerminator node = do
      let ns = nodeNs node
          name = nodeName node
      case ns of
        Just "math" | name `elem` ["mi", "mo", "mn", "ms", "mtext"] -> pure True
        Just "math" | name == "annotation-xml" -> do
          attrs <- readNodeAttrs node
          pure $ case attrLookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html", "application/xhtml+xml"]
            Nothing -> False
        Just "svg" | name `elem` ["foreignObject", "desc", "title"] -> pure True
        _ -> pure False


hasInScope :: Text -> TreeBuilder -> IO Bool
hasInScope t tb = do
  let !targetTid = tagIdFromText t
  hasElementInScopeT targetTid t tagIdIsDefaultScopeTerminator True tb


{-# INLINE hasInScopeT #-}
hasInScopeT :: TagId -> Text -> TreeBuilder -> IO Bool
hasInScopeT !tid t tb = hasElementInScopeT tid t tagIdIsDefaultScopeTerminator True tb


hasInButtonScope :: Text -> TreeBuilder -> IO Bool
hasInButtonScope t tb = do
  let !targetTid = tagIdFromText t
  hasElementInScopeT targetTid t tagIdIsButtonScopeTerminator True tb


{-# INLINE hasInButtonScopeT #-}
hasInButtonScopeT :: TagId -> Text -> TreeBuilder -> IO Bool
hasInButtonScopeT !tid t tb = hasElementInScopeT tid t tagIdIsButtonScopeTerminator True tb


hasInListItemScope :: Text -> TreeBuilder -> IO Bool
hasInListItemScope t tb = do
  let !targetTid = tagIdFromText t
  hasElementInScopeT targetTid t tagIdIsListItemScopeTerminator True tb


hasInDefinitionScope :: Text -> TreeBuilder -> IO Bool
hasInDefinitionScope t tb = do
  let !targetTid = tagIdFromText t
  hasElementInScopeT targetTid t (\tid -> tid == TagDl || tagIdIsDefaultScopeTerminator tid) True tb


hasInTableScope :: Text -> TreeBuilder -> IO Bool
hasInTableScope t tb = do
  let !targetTid = tagIdFromText t
  hasElementInScopeT targetTid t tagIdIsTableScopeTerminator False tb


hasAnyHeadingInScope :: TreeBuilder -> IO Bool
hasAnyHeadingInScope tb = do
  let es = tbStack tb
  sz <- esSize es
  let go !i
        | i < 0 = pure False
        | otherwise = do
            packed <- esReadTid es i
            let !tid = tidFromPacked packed
                !isHTML = isHTMLFromPacked packed
            if isHTML
              then
                if tagIdIsHeading tid
                  then pure True
                  else
                    if tagIdIsDefaultScopeTerminator tid
                      then pure False
                      else go (i - 1)
              else go (i - 1)
  go (sz - 1)


------------------------------------------------------------------------
-- Stack helpers
------------------------------------------------------------------------

currentNode :: TreeBuilder -> IO (Maybe TBNode)
currentNode tb = esTop (tbStack tb)


{-# INLINE currentNodeName #-}
currentNodeName :: TreeBuilder -> IO Text
currentNodeName tb = do
  sz <- esSize (tbStack tb)
  if sz > 0 then nodeName <$> esTopUnsafe (tbStack tb) else pure ""


{-# INLINE currentNodeTagId #-}
currentNodeTagId :: TreeBuilder -> IO TagId
currentNodeTagId tb = do
  sz <- esSize (tbStack tb)
  if sz > 0 then nodeTagId <$> esTopUnsafe (tbStack tb) else pure TagUnknown


popElement :: TreeBuilder -> IO ()
popElement tb = do
  when (tbEmitEvents tb) $ do
    let !(ElementStack esArr esCnt _) = tbStack tb
    n <- readByteArray esCnt 0 :: IO Int
    when (n > 0) $ do
      top <- readSmallArray esArr (n - 1)
      tbEmitEvent tb (TreeClose (nodeName top))
  esPop (tbStack tb)


popUntilInclusive :: Text -> TreeBuilder -> IO ()
popUntilInclusive target tb = popUntilInclusiveT (tagIdFromText target) target tb


{-# INLINE popUntilInclusiveT #-}
popUntilInclusiveT :: TagId -> Text -> TreeBuilder -> IO ()
popUntilInclusiveT !targetTid target tb = do
  let es@(ElementStack esArr esCnt tidsBuf) = tbStack tb
      !emitting = tbEmitEvents tb
      loop = do
        n <- readByteArray esCnt 0 :: IO Int
        if n <= 0
          then pure ()
          else do
            packed <- readByteArray tidsBuf (n - 1) :: IO Int
            let !isHTML = isHTMLFromPacked packed
            if packedTidIs packed TagP
              then do
                pc <- readScalar (tbScalars tb) sPOnStack
                writeScalar (tbScalars tb) sPOnStack (max 0 (pc - 1))
              else pure ()
            matches <-
              if not isHTML
                then pure False
                else
                  if targetTid /= TagUnknown
                    then pure (packedTidIs packed targetTid)
                    else
                      if not (packedTidIs packed TagUnknown)
                        then pure False
                        else do
                          node <- esRead es (n - 1)
                          pure (nodeName node == target)
            when emitting $ do
              top <- readSmallArray esArr (n - 1)
              tbEmitEvent tb (TreeClose (nodeName top))
            writeByteArray esCnt 0 (n - 1 :: Int)
            if matches then pure () else loop
  loop


popUntilPred :: (TagId -> Bool) -> TreeBuilder -> IO ()
popUntilPred predicate tb = do
  let es@(ElementStack esArr _ _) = tbStack tb
      !emitting = tbEmitEvents tb
      loop = do
        n <- esSize es
        if n <= 0
          then pure ()
          else do
            packed <- esReadTid es (n - 1)
            if predicate (tidFromPacked packed)
              then pure ()
              else do
                when emitting $ do
                  top <- readSmallArray esArr (n - 1)
                  tbEmitEvent tb (TreeClose (nodeName top))
                esPop es
                loop
  loop


isOnStack :: Text -> TreeBuilder -> IO Bool
isOnStack name tb = do
  let !tid = tagIdFromText name
      es = tbStack tb
  n <- esSize es
  let go !i
        | i >= n = pure False
        | otherwise = do
            packed <- esReadTid es i
            let !t = tidFromPacked packed
            if tid /= TagUnknown
              then if t == tid then pure True else go (i + 1)
              else do
                node <- esRead es i
                if nodeName node == name then pure True else go (i + 1)
  go 0


------------------------------------------------------------------------
-- Insert operations
------------------------------------------------------------------------

{-# INLINE appendChild #-}
appendChild :: TBNode -> TBNode -> IO ()
appendChild parent child = do
  writeIORef (nodeParent child) (Just parent)
  let ref = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
  pushChild ref (TBCElement child)


removeChild :: TBNode -> TBNode -> IO ()
removeChild parent child = do
  let ref = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
  removeChildFromVec ref child
  writeIORef (nodeParent child) Nothing


insertElement :: TreeBuilder -> Text -> [(Text, Text)] -> Maybe Text -> IO TBNode
insertElement tb name attrs ns = insertElementT tb name (tagIdFromText name) (attrsFromList attrs) ns


{-# INLINE insertElementT #-}
insertElementT :: TreeBuilder -> Text -> TagId -> SmallArray HTMLAttribute -> Maybe Text -> IO TBNode
insertElementT tb name tid attrs ns = do
  let isTmpl = tid == TagTemplate && (ns == Nothing || ns == Just "" || ns == Just "html")
  node <- newTBNode tb name tid attrs ns isTmpl
  insertFromTable <- tbInsertFromTable tb
  let !(ElementStack esArr esCnt _) = tbStack tb
  n <- readByteArray esCnt 0 :: IO Int
  if n > 0
    then do
      current <- readSmallArray esArr (n - 1)
      if insertFromTable && isFosterTarget (nodeTagId current)
        then fosterParentNode tb node
        else when (tbBuildDOM tb) $ appendChild current node
    else insertIntoFragmentOrDoc tb node
  esPush (tbStack tb) node
  tbEmitEvent tb (TreeOpen name attrs)
  if tid == TagP
    then do
      pc <- readScalar (tbScalars tb) sPOnStack
      writeScalar (tbScalars tb) sPOnStack (pc + 1)
    else pure ()
  pure node


insertIntoFragmentOrDoc :: TreeBuilder -> TBNode -> IO ()
insertIntoFragmentOrDoc tb node =
  when (tbBuildDOM tb) $ do
    docNodes <- readIORef (tbDocument tb)
    case findHtmlRoot docNodes of
      Just htmlRoot -> appendChild htmlRoot node
      Nothing -> modifyIORef' (tbDocument tb) (++ [CElement node])
  where
    findHtmlRoot [] = Nothing
    findHtmlRoot (CElement n : _) | nodeTagId n == TagHtml = Just n
    findHtmlRoot (_ : rest) = findHtmlRoot rest


insertVoidElement :: TreeBuilder -> Text -> [(Text, Text)] -> Maybe Text -> IO TBNode
insertVoidElement tb name attrs ns = do
  node <- insertElement tb name attrs ns
  popElement tb
  pure node


insertVoidElementT :: TreeBuilder -> Text -> TagId -> SmallArray HTMLAttribute -> Maybe Text -> IO TBNode
insertVoidElementT tb name tid attrs ns = do
  node <- insertElementT tb name tid attrs ns
  popElement tb
  pure node


insertComment :: TreeBuilder -> Text -> IO ()
insertComment tb txt = do
  let commentText = fixCDATAComment txt
  sz <- esSize (tbStack tb)
  if sz > 0
    then do
      current <- esTopUnsafe (tbStack tb)
      let ref = if nodeIsTemplate current then nodeTemplateContents current else nodeChildren current
      when (tbBuildDOM tb) $ pushChild ref (TBCComment commentText)
    else when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CComment commentText])
  tbEmitEvent tb (TreeComment commentText)


cdataMarker :: Text
cdataMarker = T.pack ['\xFFFE', 'C', 'D']


isCDATA :: Text -> Bool
isCDATA t = cdataMarker `T.isPrefixOf` t


cdataContent :: Text -> Text
cdataContent t = T.drop (T.length cdataMarker) t


fixCDATAComment :: Text -> Text
fixCDATAComment t
  | isCDATA t = "[CDATA[" <> cdataContent t <> "]]"
  | otherwise = t


insertCommentToDocument :: TreeBuilder -> Text -> IO ()
insertCommentToDocument tb txt = case tbFragmentContext tb of
  Just _ -> do
    docNodes <- readIORef (tbDocument tb)
    case findHtmlRootInDoc docNodes of
      Just htmlRoot ->
        when (tbBuildDOM tb) $ pushChild (nodeChildren htmlRoot) (TBCComment (fixCDATAComment txt))
      Nothing ->
        when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CComment (fixCDATAComment txt)])
  Nothing ->
    when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CComment (fixCDATAComment txt)])
  where
    findHtmlRootInDoc [] = Nothing
    findHtmlRootInDoc (CElement n : _) | nodeTagId n == TagHtml = Just n
    findHtmlRootInDoc (_ : rest) = findHtmlRootInDoc rest


insertDoctype :: TreeBuilder -> Text -> Maybe Text -> Maybe Text -> IO ()
insertDoctype tb name pub sys = do
  when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CDoctype name pub sys])
  tbEmitEvent tb (TreeDoctype name pub sys)


------------------------------------------------------------------------
-- Foster parenting
------------------------------------------------------------------------

fosterParentText :: TreeBuilder -> Text -> IO ()
fosterParentText tb txt = do
  elems <- esReadAll (tbStack tb)
  let mLastTable = findLastOnStack "table" elems
      mLastTemplate = findLastTemplate elems
  case (mLastTemplate, mLastTable) of
    (Just (tmplIdx, tmplNode), Just (tblIdx, _))
      | tmplIdx < tblIdx -> appendTextInto tb tmplNode txt
    (Just (_, tmplNode), Nothing) -> appendTextInto tb tmplNode txt
    (_, Just (_, tableNode)) -> do
      mpar <- readIORef (nodeParent tableNode)
      case mpar of
        Just parent -> do
          let ref = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
          ChildVec fcb arr <- readIORef ref
          n <- readByteArray fcb 0
          mIdx <- findChildIndex arr n tableNode
          case mIdx of
            Just idx | idx > 0 -> do
              prev <- readSmallArray arr (idx - 1)
              when (tbBuildDOM tb) $ case prev of
                TBCText old -> writeSmallArray arr (idx - 1) $! TBCText (old <> txt)
                _ -> insertChildBefore ref tableNode (TBCText txt)
            _ -> when (tbBuildDOM tb) $ insertChildBefore ref tableNode (TBCText txt)
        Nothing ->
          appendTextToCurrentNode tb txt
    (Nothing, Nothing) -> appendTextToCurrentNode tb txt


{-# INLINE appendTextInto #-}
appendTextInto :: TreeBuilder -> TBNode -> Text -> IO ()
appendTextInto tb parent txt = do
  when (tbBuildDOM tb) $ do
    let ref = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
    pushText ref txt


fosterParentNode :: TreeBuilder -> TBNode -> IO ()
fosterParentNode tb node =
  when (tbBuildDOM tb) $ do
    elems <- esReadAll (tbStack tb)
    let mLastTable = findLastOnStack "table" elems
        mLastTemplate = findLastTemplate elems
    case (mLastTemplate, mLastTable) of
      (Just (tmplIdx, tmplNode), Just (tblIdx, _))
        | tmplIdx < tblIdx -> appendChild tmplNode node
      (Just (_, tmplNode), Nothing) -> appendChild tmplNode node
      (_, Just (_, tableNode)) -> do
        mPar <- readIORef (nodeParent tableNode)
        case mPar of
          Just parent -> insertBefore parent tableNode node
          Nothing ->
            let above = dropWhile (/= tableNode) elems
            in case drop 1 above of
                (a : _) -> appendChild a node
                [] -> pure ()
      (Nothing, Nothing) -> case elems of
        (current : _) -> appendChild current node
        [] -> pure ()


findLastOnStack :: Text -> [TBNode] -> Maybe (Int, TBNode)
findLastOnStack name elems =
  let !tid = tagIdFromText name
  in go tid 0 elems
  where
    go _ _ [] = Nothing
    go !tid i (n : rest)
      | tid /= TagUnknown && nodeTagId n == tid = Just (i, n)
      | tid == TagUnknown && nodeName n == name = Just (i, n)
      | otherwise = go tid (i + 1) rest


findLastTemplate :: [TBNode] -> Maybe (Int, TBNode)
findLastTemplate elems = go 0 elems
  where
    go _ [] = Nothing
    go i (n : rest)
      | nodeIsTemplate n = Just (i, n)
      | otherwise = go (i + 1) rest


insertBefore :: TBNode -> TBNode -> TBNode -> IO ()
insertBefore parent refNode newNode = do
  writeIORef (nodeParent newNode) (Just parent)
  let ref = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
  insertChildBefore ref refNode (TBCElement newNode)


------------------------------------------------------------------------
-- More helpers
------------------------------------------------------------------------

generateImpliedEndTags :: Maybe Text -> TreeBuilder -> IO ()
generateImpliedEndTags mexclude tb = do
  let mexcludeTid = case mexclude of
        Nothing -> Nothing
        Just t -> Just (tagIdFromText t)
  generateImpliedEndTagsT mexcludeTid tb


{-# INLINE generateImpliedEndTagsT #-}
generateImpliedEndTagsT :: Maybe TagId -> TreeBuilder -> IO ()
generateImpliedEndTagsT mexclude tb = do
  let (ElementStack _ esCnt tidsBuf) = tbStack tb
  go esCnt tidsBuf
  where
    go esCnt tidsBuf = do
      sz <- readByteArray esCnt 0 :: IO Int
      if sz <= 0
        then pure ()
        else do
          packed <- readByteArray tidsBuf (sz - 1) :: IO Int
          let !tid = tidFromPacked packed
              !excluded = case mexclude of
                Nothing -> False
                Just ex -> packedTidIs packed ex
          if tagIdIsImpliedEndTag tid && not excluded
            then do
              writeByteArray esCnt 0 (sz - 1 :: Int)
              go esCnt tidsBuf
            else pure ()


closePElement :: TreeBuilder -> IO ()
closePElement tb = do
  pc <- readScalar (tbScalars tb) sPOnStack
  if pc <= 0
    then pure ()
    else do
      inScope <- hasInButtonScopeT TagP "p" tb
      if inScope
        then do
          generateImpliedEndTagsT (Just TagP) tb
          popUntilInclusive "p" tb
        else pure ()


clearStackToTableContext :: TreeBuilder -> IO ()
clearStackToTableContext tb = popUntilPred (\tid -> tid == TagTable || tid == TagTemplate || tid == TagHtml) tb


clearStackToTableBodyContext :: TreeBuilder -> IO ()
clearStackToTableBodyContext tb = popUntilPred (\tid -> tid == TagTbody || tid == TagTfoot || tid == TagThead || tid == TagTemplate || tid == TagHtml) tb


clearStackToTableRowContext :: TreeBuilder -> IO ()
clearStackToTableRowContext tb = popUntilPred (\tid -> tid == TagTr || tid == TagTemplate || tid == TagHtml) tb


------------------------------------------------------------------------
-- Active formatting
------------------------------------------------------------------------

reconstructActiveFormatting :: TreeBuilder -> IO ()
reconstructActiveFormatting tb = do
  hasAF <- readScalar (tbScalars tb) sHasAF
  if hasAF == 0
    then pure ()
    else do
      af <- readIORef (tbActiveFormatting tb)
      case af of
        [] -> pure ()
        (AFMarker : _) -> pure ()
        (AFEntry _name _ node : _) -> do
          onStack <- esElemInStack (tbStack tb) node
          if onStack
            then pure ()
            else doReconstruct tb
        _ -> pure ()


doReconstruct :: TreeBuilder -> IO ()
doReconstruct tb = do
  af <- readIORef (tbActiveFormatting tb)
  indexedEntries <- collectEntries af 0
  mapM_ reinsertAtIndex indexedEntries
  where
    collectEntries [] _ = pure []
    collectEntries (AFMarker : _) _ = pure []
    collectEntries (AFEntry _ _ node : rest) idx = do
      onStack <- esElemInStack (tbStack tb) node
      if onStack
        then pure []
        else do
          further <- collectEntries rest (idx + 1)
          pure (further ++ [idx])

    reinsertAtIndex idx = do
      af <- readIORef (tbActiveFormatting tb)
      case safeIdx idx af of
        Just (AFEntry name attrs _) -> do
          node <- insertElementT tb name (tagIdFromText name) attrs Nothing
          modifyIORef' (tbActiveFormatting tb) (replaceAtIdx idx (AFEntry name attrs node))
        _ -> pure ()

    safeIdx _ [] = Nothing
    safeIdx 0 (x : _) = Just x
    safeIdx n (_ : xs) = safeIdx (n - 1) xs

    replaceAtIdx 0 new (_ : rest) = new : rest
    replaceAtIdx n new (x : rest) = x : replaceAtIdx (n - 1) new rest
    replaceAtIdx _ _ [] = []


pushFormattingMarker :: TreeBuilder -> IO ()
pushFormattingMarker tb = do
  modifyIORef' (tbActiveFormatting tb) (AFMarker :)
  writeScalar (tbScalars tb) sHasAF 1


pushFormattingEntry :: Text -> SmallArray HTMLAttribute -> TBNode -> TreeBuilder -> IO ()
pushFormattingEntry name attrs node tb = do
  af <- readIORef (tbActiveFormatting tb)
  let cleaned = removeExcess name attrs af
  writeIORef (tbActiveFormatting tb) (AFEntry name attrs node : cleaned)
  writeScalar (tbScalars tb) sHasAF 1


removeExcess :: Text -> SmallArray HTMLAttribute -> [AFEntry] -> [AFEntry]
removeExcess name attrs entries =
  let (beforeMarker, _) = break isMarker entries
      matching = findMatching 0 beforeMarker
  in if length matching >= 3
      then removeAt (last matching) entries
      else entries
  where
    isMarker AFMarker = True
    isMarker _ = False
    findMatching _ [] = []
    findMatching !i (AFEntry n a _ : rest)
      | n == name, sameAttrs a attrs = i : findMatching (i + 1) rest
      | otherwise = findMatching (i + 1) rest
    findMatching !i (AFMarker : rest) = findMatching (i + 1) rest
    sameAttrs a b = sizeofSmallArray a == sizeofSmallArray b && sortedEq (sortAttrs a) (sortAttrs b)
    sortAttrs v = sortBy (comparing (\(HTMLAttribute n _) -> n)) (toList v)
    sortedEq [] [] = True
    sortedEq (HTMLAttribute n1 v1 : xs) (HTMLAttribute n2 v2 : ys) = n1 == n2 && v1 == v2 && sortedEq xs ys
    sortedEq _ _ = False


removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_ : xs) = xs
removeAt n (x : xs) = x : removeAt (n - 1) xs


clearActiveFormattingToMarker :: TreeBuilder -> IO ()
clearActiveFormattingToMarker tb =
  modifyIORef' (tbActiveFormatting tb) go
  where
    go [] = []
    go (AFMarker : rest) = rest
    go (_ : rest) = go rest


hasActiveFormattingEntry :: Text -> TreeBuilder -> IO Bool
hasActiveFormattingEntry name tb = do
  af <- readIORef (tbActiveFormatting tb)
  pure (go af)
  where
    go [] = False
    go (AFMarker : _) = False
    go (AFEntry n _ _ : rest)
      | n == name = True
      | otherwise = go rest


------------------------------------------------------------------------
-- Adoption agency algorithm
------------------------------------------------------------------------

adoptionAgency :: Text -> TreeBuilder -> IO ()
adoptionAgency subject tb = do
  cn <- currentNodeName tb
  hasAF <- hasActiveFormattingEntry subject tb
  if cn == subject && not hasAF
    then popUntilInclusive subject tb
    else outerLoop 0
  where
    outerLoop :: Int -> IO ()
    outerLoop !iter
      | iter >= 8 = pure ()
      | otherwise = do
          af <- readIORef (tbActiveFormatting tb)
          case findFormattingElement subject af of
            Nothing -> anyOtherEndTag subject tb
            Just (fmtIdx, AFEntry _ fmtAttrs fmtNode) -> do
              onStack <- esElemInStack (tbStack tb) fmtNode
              if not onStack
                then do
                  modifyIORef' (tbActiveFormatting tb) (removeAtIdx fmtIdx)
                else do
                  inScope <- hasInScope subject tb
                  if not inScope
                    then pure ()
                    else do
                      openElems <- esReadAll (tbStack tb)
                      let mFb = findFurthestBlock fmtNode openElems
                      case mFb of
                        Nothing -> do
                          elems2 <- esReadAll (tbStack tb)
                          esWriteList (tbStack tb) (dropThrough' fmtNode elems2)
                          modifyIORef' (tbActiveFormatting tb) (removeAtIdx fmtIdx)
                        Just furthestBlock -> do
                          doAdoption fmtNode fmtIdx fmtAttrs furthestBlock
                          outerLoop (iter + 1)
            _ -> pure ()

    doAdoption :: TBNode -> Int -> SmallArray HTMLAttribute -> TBNode -> IO ()
    doAdoption fmtNode fmtIdx fmtAttrs furthestBlock = do
      openElems <- esReadAll (tbStack tb)
      let fmtStackIdx = case elemIndex fmtNode openElems of Just i -> i; Nothing -> 0
          commonAncestor =
            if fmtStackIdx + 1 < length openElems
              then openElems !! (fmtStackIdx + 1)
              else fmtNode

      let bookmark0 = fmtIdx + 1
      let fbStackIdx = case elemIndex furthestBlock openElems of Just i -> i; Nothing -> 0

      (lastNode, finalBookmark) <- innerLoop 0 furthestBlock fmtNode bookmark0 furthestBlock

      mp <- readIORef (nodeParent lastNode)
      case mp of
        Just parent -> when (tbBuildDOM tb) $ removeChild parent lastNode
        Nothing -> pure ()

      insertFromTable <- tbInsertFromTable tb
      let shouldFoster = insertFromTable && nodeName commonAncestor `elem` ["table", "tbody", "tfoot", "thead", "tr"]
      if shouldFoster
        then fosterParentNode tb lastNode
        else when (tbBuildDOM tb) $ appendChild commonAncestor lastNode

      newFmtNode <- newTBNode tb (nodeName fmtNode) (nodeTagId fmtNode) fmtAttrs (nodeNs fmtNode) False

      when (tbBuildDOM tb) $ transferChildren (nodeChildren furthestBlock) newFmtNode (nodeChildren newFmtNode)
      when (tbBuildDOM tb) $ appendChild furthestBlock newFmtNode

      af2 <- readIORef (tbActiveFormatting tb)
      let updatedEntry = AFEntry (nodeName fmtNode) fmtAttrs newFmtNode
          af3 = removeAtIdx fmtIdx af2
          adjBookmark = finalBookmark - 1
          insertPos = max 0 (min adjBookmark (length af3))
          af4 = insertAtIdx insertPos updatedEntry af3
      writeIORef (tbActiveFormatting tb) af4

      openElems2 <- esReadAll (tbStack tb)
      let elems3 = filter (/= fmtNode) openElems2
      case elemIndex furthestBlock elems3 of
        Just idx -> do
          esWriteList
            (tbStack tb)
            (take idx elems3 ++ [newFmtNode] ++ drop idx elems3)
        Nothing -> do
          esWriteList (tbStack tb) (newFmtNode : elems3)

    innerLoop :: Int -> TBNode -> TBNode -> Int -> TBNode -> IO (TBNode, Int)
    innerLoop !count nodeRef fmtNode' bookmark fb = do
      openElems <- esReadAll (tbStack tb)
      let nodeRefIdx = case elemIndex nodeRef openElems of Just i -> i; Nothing -> 0
          nextIdx = nodeRefIdx + 1
      if nextIdx >= length openElems
        then pure (nodeRef, bookmark)
        else do
          let node = openElems !! nextIdx
              newCount = count + 1
          if node == fmtNode'
            then pure (nodeRef, bookmark)
            else do
              af <- readIORef (tbActiveFormatting tb)
              let mAfIdx = findAFIndex node af
              case mAfIdx of
                Just afIdx | newCount > 3 -> do
                  modifyIORef' (tbActiveFormatting tb) (removeAtIdx afIdx)
                  let newBookmark = if afIdx < bookmark then bookmark - 1 else bookmark
                  removeNodeFromStack node tb
                  innerLoop newCount nodeRef fmtNode' newBookmark fb
                Nothing -> do
                  removeNodeFromStack node tb
                  innerLoop newCount nodeRef fmtNode' bookmark fb
                Just afIdx -> do
                  af2 <- readIORef (tbActiveFormatting tb)
                  let AFEntry eName eAttrs _ = af2 !! afIdx
                  newElem <- newTBNode tb eName (tagIdFromText eName) eAttrs (nodeNs node) False
                  modifyIORef'
                    (tbActiveFormatting tb)
                    ( \afs ->
                        take afIdx afs ++ [AFEntry eName eAttrs newElem] ++ drop (afIdx + 1) afs
                    )
                  openElems2 <- esReadAll (tbStack tb)
                  let idx2 = case elemIndex node openElems2 of Just i -> i; Nothing -> nextIdx
                  esWriteList
                    (tbStack tb)
                    (take idx2 openElems2 ++ [newElem] ++ drop (idx2 + 1) openElems2)
                  let newBookmark = if nodeRef == fb then afIdx + 1 else bookmark
                  mpLast <- readIORef (nodeParent nodeRef)
                  case mpLast of
                    Just p -> when (tbBuildDOM tb) $ removeChild p nodeRef
                    Nothing -> pure ()
                  when (tbBuildDOM tb) $ appendChild newElem nodeRef
                  innerLoop newCount newElem fmtNode' newBookmark fb

    dropThrough' :: TBNode -> [TBNode] -> [TBNode]
    dropThrough' target (x : xs) | x == target = xs
    dropThrough' target (_ : xs) = dropThrough' target xs
    dropThrough' _ [] = []


findFormattingElement :: Text -> [AFEntry] -> Maybe (Int, AFEntry)
findFormattingElement _ [] = Nothing
findFormattingElement subject (AFMarker : _) = Nothing
findFormattingElement subject entries = go 0 entries
  where
    go _ [] = Nothing
    go _ (AFMarker : _) = Nothing
    go i (e@(AFEntry n _ _) : rest)
      | n == subject = Just (i, e)
      | otherwise = go (i + 1) rest


findFurthestBlock :: TBNode -> [TBNode] -> Maybe TBNode
findFurthestBlock fmtNode openElems =
  let aboveFmt = takeWhile (/= fmtNode) openElems
      specialOnes = filter isSpecial aboveFmt
  in case specialOnes of
      [] -> Nothing
      _ -> Just (last specialOnes)
  where
    isSpecial n = nodeIsHTMLNs n && tagIsSpecial n
    tagIsSpecial n = tagIdIsSpecial (nodeTagId n)


removeAtIdx :: Int -> [a] -> [a]
removeAtIdx _ [] = []
removeAtIdx 0 (_ : xs) = xs
removeAtIdx n (x : xs) = x : removeAtIdx (n - 1) xs


runAdoptionInner :: TBNode -> Int -> SmallArray HTMLAttribute -> TBNode -> TreeBuilder -> IO ()
runAdoptionInner fmtNode fmtIdx fmtAttrs furthestBlock tb = do
  openElems <- esReadAll (tbStack tb)
  let fmtStackIdx = elemIndex fmtNode openElems
      fbStackIdx = elemIndex furthestBlock openElems
  case (fmtStackIdx, fbStackIdx) of
    (Just fi, Just fbi) -> do
      let commonAncestor = openElems !! (fi + 1)
      af <- readIORef (tbActiveFormatting tb)
      let bookmark = fmtIdx
      innerLoop 0 (fbi - 1) bookmark fi fbi commonAncestor
    _ -> pure ()
  where
    innerLoop :: Int -> Int -> Int -> Int -> Int -> TBNode -> IO ()
    innerLoop !innerCount !nodeIdx !bookmark !fmtSIdx !fbSIdx !commonAnc
      | innerCount >= 3 = finishAdoption bookmark fmtSIdx commonAnc
      | otherwise = do
          openElems <- esReadAll (tbStack tb)
          if nodeIdx <= fmtSIdx || nodeIdx >= length openElems
            then finishAdoption bookmark fmtSIdx commonAnc
            else do
              let node = openElems !! nodeIdx
              af <- readIORef (tbActiveFormatting tb)
              let mafIdx = findAFIndex node af
              case mafIdx of
                Nothing -> do
                  removeNodeFromStack node tb
                  innerLoop innerCount (nodeIdx - 1) bookmark fmtSIdx fbSIdx commonAnc
                Just afIdx -> do
                  finishAdoption bookmark fmtSIdx commonAnc
    finishAdoption :: Int -> Int -> TBNode -> IO ()
    finishAdoption bookmark fmtSIdx commonAnc = do
      newFmtNode <- newTBNode tb (nodeName fmtNode) (nodeTagId fmtNode) fmtAttrs (nodeNs fmtNode) False
      when (tbBuildDOM tb) $ transferChildren (nodeChildren furthestBlock) newFmtNode (nodeChildren newFmtNode)
      when (tbBuildDOM tb) $ appendChild furthestBlock newFmtNode
      modifyIORef'
        (tbActiveFormatting tb)
        ( \af ->
            let af1 = removeAtIdx fmtIdx af
                newEntry = AFEntry (nodeName fmtNode) fmtAttrs newFmtNode
            in insertAtIdx (min bookmark (length af1)) newEntry af1
        )
      removeNodeFromStack fmtNode tb
      openElems2 <- esReadAll (tbStack tb)
      let fbIdx2 = elemIndex furthestBlock openElems2
      case fbIdx2 of
        Just idx -> do
          esWriteList
            (tbStack tb)
            (take idx openElems2 ++ [newFmtNode] ++ drop idx openElems2)
        Nothing -> pure ()
      -- Reparent fmtNode's later children into commonAncestor
      mp <- readIORef (nodeParent fmtNode)
      case mp of
        Just parent -> when (tbBuildDOM tb) $ removeChild parent fmtNode
        Nothing -> pure ()
      when (tbBuildDOM tb) $ appendChild commonAnc furthestBlock
      mp2 <- readIORef (nodeParent furthestBlock)
      case mp2 of
        Just oldParent | oldParent /= commonAnc -> when (tbBuildDOM tb) $ removeChild oldParent furthestBlock
        _ -> pure ()


findAFIndex :: TBNode -> [AFEntry] -> Maybe Int
findAFIndex _ [] = Nothing
findAFIndex node entries = go 0 entries
  where
    go _ [] = Nothing
    go _ (AFMarker : _) = Nothing
    go i (AFEntry _ _ n : rest)
      | n == node = Just i
      | otherwise = go (i + 1) rest


insertAtIdx :: Int -> a -> [a] -> [a]
insertAtIdx 0 x xs = x : xs
insertAtIdx n x (y : ys) = y : insertAtIdx (n - 1) x ys
insertAtIdx _ x [] = [x]


removeNodeFromStack :: TBNode -> TreeBuilder -> IO ()
removeNodeFromStack node tb = do
  esRemoveNode (tbStack tb) node


elemIndex :: TBNode -> [TBNode] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex target xs = go 0 xs
  where
    go _ [] = Nothing
    go i (x : rest)
      | x == target = Just i
      | otherwise = go (i + 1) rest


anyOtherEndTag :: Text -> TreeBuilder -> IO ()
anyOtherEndTag name tb = anyOtherEndTagT (tagIdFromText name) name tb


{-# INLINE anyOtherEndTagT #-}
anyOtherEndTagT :: TagId -> Text -> TreeBuilder -> IO ()
anyOtherEndTagT !ntid name tb = do
  let es = tbStack tb
  sz <- esSize es
  go es (sz - 1)
  where
    !mexcludeTid = Just ntid
    go _es !i | i < 0 = pure ()
    go es !i = do
      packed <- esReadTid es i
      let !tid = tidFromPacked packed
          !isHTML = isHTMLFromPacked packed
      if isHTML && tid /= TagUnknown
        then
          if tid == ntid
            then do
              generateImpliedEndTagsT mexcludeTid tb
              esSetSize es i
            else
              if tagIdIsSpecial tid
                then pure ()
                else go es (i - 1)
        else do
          node <- esRead es i
          if ntid == TagUnknown && nodeName node == name
            then do
              generateImpliedEndTagsT mexcludeTid tb
              esSetSize es i
            else
              if nodeIsHTMLNs node && tagIdIsSpecial (nodeTagId node)
                then pure ()
                else go es (i - 1)


------------------------------------------------------------------------
-- Reset insertion mode
------------------------------------------------------------------------

resetInsertionMode :: TreeBuilder -> IO ()
resetInsertionMode tb = do
  let es = tbStack tb
  sz <- esSize es
  go es (sz - 1)
  where
    go _es !i | i < 0 = tbSetMode tb MInBody
    go es !i = do
      node <- esRead es i
      let !tid = nodeTagId node
      case tid of
        TagSelect -> tbSetMode tb MInSelect
        TagTd -> tbSetMode tb MInCell
        TagTh -> tbSetMode tb MInCell
        TagTr -> tbSetMode tb MInRow
        TagTbody -> tbSetMode tb MInTableBody
        TagThead -> tbSetMode tb MInTableBody
        TagTfoot -> tbSetMode tb MInTableBody
        TagCaption -> tbSetMode tb MInCaption
        TagTable -> tbSetMode tb MInTable
        TagTemplate -> do
          tms <- readIORef (tbTemplateModes tb)
          case tms of
            (m : _) -> tbSetMode tb m
            [] -> tbSetMode tb MInTemplate
        TagHead -> tbSetMode tb MInHead
        TagBody -> tbSetMode tb MInBody
        TagFrameset -> tbSetMode tb MInFrameset
        TagHtml -> tbSetMode tb MInBody
        _ -> go es (i - 1)


------------------------------------------------------------------------
-- Insertion mode implementations
------------------------------------------------------------------------

isWS :: Char -> Bool
isWS c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'


addMissingAttrs :: TBNode -> SmallArray HTMLAttribute -> IO ()
addMissingAttrs node newAttrs = do
  existingVec <- readNodeAttrs node
  let toAdd = filter (\(HTMLAttribute n _) -> not (any (\(HTMLAttribute en _) -> en == n) existingVec)) (toList newAttrs)
  if null toAdd
    then pure ()
    else writeIORef (nodeAttrs node) (smallArrayFromList (toList existingVec ++ toAdd))


modeInitial :: TreeBuilder -> Token -> IO ()
modeInitial tb tok = case tok of
  TChar c | isWS c -> pure ()
  TComment t -> insertCommentToDocument tb t
  TDoctype name pub sys fq -> do
    insertDoctype tb (T.toLower name) pub sys
    let qm = determineQuirksMode name pub sys fq
    writeIORef (tbQuirksMode tb) qm
    tbSetMode tb MBeforeHtml
  _ -> do
    writeIORef (tbQuirksMode tb) "quirks"
    tbSetMode tb MBeforeHtml
    modeBeforeHtml tb tok


modeBeforeHtml :: TreeBuilder -> Token -> IO ()
modeBeforeHtml tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> pure ()
  TChar c | isWS c -> pure ()
  TStartTag "html" attrs _ _ -> do
    node <- newTBNode tb "html" TagHtml attrs Nothing False
    when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CElement node])
    esPush (tbStack tb) node
    tbEmitEvent tb (TreeOpen "html" attrs)
    tbSetMode tb MBeforeHead
  TEndTag name _ | name `elem` ["head", "body", "html", "br"] -> do
    node <- newTBNode tb "html" TagHtml emptySmallArray Nothing False
    when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CElement node])
    esPush (tbStack tb) node
    tbEmitEvent tb (TreeOpen "html" emptySmallArray)
    tbSetMode tb MBeforeHead
    processInMode tb tok
  TEndTag _ _ -> pure ()
  _ -> do
    node <- newTBNode tb "html" TagHtml emptySmallArray Nothing False
    when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (++ [CElement node])
    esPush (tbStack tb) node
    tbEmitEvent tb (TreeOpen "html" emptySmallArray)
    tbSetMode tb MBeforeHead
    processInMode tb tok


modeBeforeHead :: TreeBuilder -> Token -> IO ()
modeBeforeHead tb tok = case tok of
  TChar c | isWS c -> pure ()
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" attrs _ _ -> modeInBody tb tok
  TStartTag "head" attrs _ _ -> do
    node <- insertElementT tb "head" TagHead attrs Nothing
    writeIORef (tbHeadElement tb) (Just node)
    tbSetMode tb MInHead
  TEndTag name _ | name `elem` ["head", "body", "html", "br"] -> do
    node <- insertElementT tb "head" TagHead emptySmallArray Nothing
    writeIORef (tbHeadElement tb) (Just node)
    tbSetMode tb MInHead
    modeInHead tb tok
  TEndTag _ _ -> pure ()
  _ -> do
    node <- insertElementT tb "head" TagHead emptySmallArray Nothing
    writeIORef (tbHeadElement tb) (Just node)
    tbSetMode tb MInHead
    modeInHead tb tok


modeInHead :: TreeBuilder -> Token -> IO ()
modeInHead tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag name attrs _ tid
    | name `elem` ["base", "basefont", "bgsound", "link", "meta"] ->
        void $ insertVoidElementT tb name tid attrs Nothing
  TStartTag "title" attrs _ _ -> do
    void $ insertElementT tb "title" TagTitle attrs Nothing
    curMode <- tbMode tb
    tbSetOriginalMode tb curMode
    tbSetMode tb MText
  TStartTag "noscript" attrs _ _ -> do
    void $ insertElementT tb "noscript" TagNoscript attrs Nothing
    curMode <- tbMode tb
    tbSetOriginalMode tb curMode
    tbSetMode tb MText
  TStartTag "noframes" attrs _ _ -> do
    void $ insertElementT tb "noframes" TagNoframes attrs Nothing
    curMode <- tbMode tb
    tbSetOriginalMode tb curMode
    tbSetMode tb MText
  TStartTag "style" attrs _ _ -> do
    void $ insertElementT tb "style" TagStyle attrs Nothing
    curMode <- tbMode tb
    tbSetOriginalMode tb curMode
    tbSetMode tb MText
  TStartTag "script" attrs _ _ -> do
    void $ insertElementT tb "script" TagScript attrs Nothing
    curMode <- tbMode tb
    tbSetOriginalMode tb curMode
    tbSetMode tb MText
  TStartTag "template" attrs _ _ -> do
    void $ insertElementT tb "template" TagTemplate attrs Nothing
    pushFormattingMarker tb
    tbSetFramesetOk tb False
    tbSetMode tb MInTemplate
    modifyIORef' (tbTemplateModes tb) (MInTemplate :)
  TEndTag "template" _ -> do
    onStack <- isOnStack "template" tb
    if not onStack
      then pure ()
      else do
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive "template" tb
        clearActiveFormattingToMarker tb
        modifyIORef' (tbTemplateModes tb) safeTail
        resetInsertionMode tb
  TStartTag "head" _ _ _ -> pure ()
  TEndTag name _ | name `elem` ["body", "html", "br"] -> do
    popElement tb
    tbSetMode tb MAfterHead
    processInMode tb tok
  TEndTag "head" _ -> do
    popElement tb
    tbSetMode tb MAfterHead
  TEndTag _ _ -> pure ()
  _ -> do
    popElement tb
    tbSetMode tb MAfterHead
    processInMode tb tok


modeInHeadNoscript :: TreeBuilder -> Token -> IO ()
modeInHeadNoscript tb tok = case tok of
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TEndTag "noscript" _ -> do
    popElement tb
    tbSetMode tb MInHead
  TChar c | isWS c -> modeInHead tb tok
  TComment _ -> modeInHead tb tok
  TStartTag name _ _ _
    | name `elem` ["basefont", "bgsound", "link", "meta", "noframes", "style"] ->
        modeInHead tb tok
  TEndTag "br" _ -> do
    popElement tb
    tbSetMode tb MInHead
    processInMode tb tok
  TStartTag name _ _ _ | name == "head" || name == "noscript" -> pure ()
  TEndTag _ _ -> pure ()
  _ -> do
    popElement tb
    tbSetMode tb MInHead
    processInMode tb tok


modeAfterHead :: TreeBuilder -> Token -> IO ()
modeAfterHead tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag "body" attrs _ _ -> do
    void $ insertElementT tb "body" TagBody attrs Nothing
    tbSetFramesetOk tb False
    tbSetMode tb MInBody
  TStartTag "frameset" attrs _ _ -> do
    void $ insertElementT tb "frameset" TagFrameset attrs Nothing
    tbSetMode tb MInFrameset
  TStartTag name _ _ _ | name `elem` ["base", "basefont", "bgsound", "link", "meta", "noframes", "script", "style", "title"] -> do
    mHead <- readIORef (tbHeadElement tb)
    case mHead of
      Just headNode -> do
        esPush (tbStack tb) headNode
        modeInHead tb tok
        esRemoveNode (tbStack tb) headNode
      Nothing -> modeInHead tb tok
  TStartTag "template" _ _ _ -> do
    mHead <- readIORef (tbHeadElement tb)
    case mHead of
      Just headNode -> do
        esPush (tbStack tb) headNode
        tbSetMode tb MInHead
        processInMode tb tok
      Nothing -> modeInHead tb tok
  TEndTag "template" _ -> modeInHead tb tok
  TEndTag name _ | name `elem` ["body", "html", "br"] -> do
    void $ insertElementT tb "body" TagBody emptySmallArray Nothing
    tbSetMode tb MInBody
    processInMode tb tok
  TStartTag "head" _ _ _ -> pure ()
  TEndTag _ _ -> pure ()
  _ -> do
    void $ insertElementT tb "body" TagBody emptySmallArray Nothing
    tbSetMode tb MInBody
    processInMode tb tok


modeInBody :: TreeBuilder -> Token -> IO ()
modeInBody tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c | isWS c -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb (T.singleton c)
  TChar c -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb (T.singleton c)
    tbSetFramesetOk tb False
  TString t -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb t
    if not (T.all isWS t)
      then tbSetFramesetOk tb False
      else pure ()
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TEOF -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms)
      then modeInTemplate tb tok
      else pure ()
  TStartTag name attrs _sc tid ->
    modeInBodyStartTag tb name attrs _sc tid
  TEndTag name tid ->
    modeInBodyEndTag tb name tid
  _ -> pure ()


modeInBodyStartTag :: TreeBuilder -> Text -> SmallArray HTMLAttribute -> Bool -> TagId -> IO ()
modeInBodyStartTag !tb !name attrs !_sc !tid = do
  case tid of
    TagHtml -> do
      tms <- readIORef (tbTemplateModes tb)
      if not (null tms)
        then pure ()
        else do
          sz <- esSize (tbStack tb)
          if sz > 0
            then do
              when (tbBuildDOM tb) $ do
                htmlNode <- esRead (tbStack tb) 0; addMissingAttrs htmlNode attrs
            else pure ()
    TagBody -> do
      tms <- readIORef (tbTemplateModes tb)
      if not (null tms)
        then pure ()
        else do
          sz <- esSize (tbStack tb)
          if sz >= 2
            then do
              bodyNode <- esRead (tbStack tb) 1
              if nodeTagId bodyNode == TagBody
                then do
                  when (tbBuildDOM tb) $ addMissingAttrs bodyNode attrs
                  tbSetFramesetOk tb False
                else pure ()
            else pure ()
    TagFrameset -> do
      fo <- tbFramesetOk tb
      if not fo
        then pure ()
        else do
          sz <- esSize (tbStack tb)
          if sz >= 2
            then do
              htmlNode <- esRead (tbStack tb) 0
              bodyNode <- esRead (tbStack tb) 1
              if nodeTagId bodyNode == TagBody
                then do
                  when (tbBuildDOM tb) $ removeChild htmlNode bodyNode
                  esSetSize (tbStack tb) 1
                  void $ insertElementT tb "frameset" TagFrameset attrs Nothing
                  tbSetMode tb MInFrameset
                else do
                  popUntilPred (== TagHtml) tb
                  void $ insertElementT tb "frameset" TagFrameset attrs Nothing
                  tbSetMode tb MInFrameset
            else do
              popUntilPred (== TagHtml) tb
              void $ insertElementT tb "frameset" TagFrameset attrs Nothing
              tbSetMode tb MInFrameset
    -- Block-level tags: closePElement + insert
    TagAddress -> blockInsert
    TagArticle -> blockInsert
    TagAside -> blockInsert
    TagBlockquote -> blockInsert
    TagCenter -> blockInsert
    TagDetails -> blockInsert
    TagDialog -> blockInsert
    TagDir -> blockInsert
    TagDiv -> blockInsert
    TagDl -> blockInsert
    TagFieldset -> blockInsert
    TagFigcaption -> blockInsert
    TagFigure -> blockInsert
    TagFooter -> blockInsert
    TagHeader -> blockInsert
    TagHgroup -> blockInsert
    TagMain -> blockInsert
    TagMenu -> blockInsert
    TagNav -> blockInsert
    TagOl -> blockInsert
    TagP -> blockInsert
    TagSearch -> blockInsert
    TagSection -> blockInsert
    TagSummary -> blockInsert
    TagUl -> blockInsert
    -- Heading tags
    TagH1 -> headingInsert
    TagH2 -> headingInsert
    TagH3 -> headingInsert
    TagH4 -> headingInsert
    TagH5 -> headingInsert
    TagH6 -> headingInsert
    TagPre -> do
      closePElement tb
      void $ insertElementT tb name TagPre attrs Nothing
      tbSetFramesetOk tb False
      tbSetIgnoreLF tb True
    TagListing -> do
      closePElement tb
      void $ insertElementT tb name TagListing attrs Nothing
      tbSetFramesetOk tb False
      tbSetIgnoreLF tb True
    TagForm -> do
      mForm <- readIORef (tbFormElement tb)
      onStack <- isOnStack "template" tb
      if mForm /= Nothing && not onStack
        then pure ()
        else do
          closePElement tb
          node <- insertElementT tb "form" TagForm attrs Nothing
          if not onStack then writeIORef (tbFormElement tb) (Just node) else pure ()
    TagLi -> do
      tbSetFramesetOk tb False
      closeLiElements tb
      closePElement tb
      void $ insertElementT tb "li" TagLi attrs Nothing
    TagDd -> do
      tbSetFramesetOk tb False
      closeDdDtElements tb
      closePElement tb
      void $ insertElementT tb name tid attrs Nothing
    TagDt -> do
      tbSetFramesetOk tb False
      closeDdDtElements tb
      closePElement tb
      void $ insertElementT tb name tid attrs Nothing
    TagPlaintext -> do
      closePElement tb
      void $ insertElementT tb "plaintext" TagPlaintext attrs Nothing
    TagButton -> do
      inScope <- hasInScopeT TagButton "button" tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilInclusive "button" tb
          reconstructActiveFormatting tb
          _ <- insertElementT tb "button" TagButton attrs Nothing
          tbSetFramesetOk tb False
        else do
          reconstructActiveFormatting tb
          void $ insertElementT tb "button" TagButton attrs Nothing
          tbSetFramesetOk tb False
    TagA -> do
      hasA <- hasActiveFormattingEntry "a" tb
      if hasA
        then do
          adoptionAgency "a" tb
          removeActiveFormattingByName "a" tb
          removeNameFromStack "a" tb
        else pure ()
      reconstructActiveFormatting tb
      node <- insertElementT tb "a" TagA attrs Nothing
      pushFormattingEntry "a" attrs node tb
    TagNobr -> do
      reconstructActiveFormatting tb
      inScope <- hasInScope "nobr" tb
      if inScope
        then do
          adoptionAgency "nobr" tb
          removeActiveFormattingByName "nobr" tb
          removeNameFromStack "nobr" tb
        else pure ()
      reconstructActiveFormatting tb
      node <- insertElementT tb "nobr" TagNobr attrs Nothing
      pushFormattingEntry "nobr" attrs node tb
    -- Formatting tags: reconstruct + insert + push formatting entry
    TagB -> formattingInsert
    TagBig -> formattingInsert
    TagCode -> formattingInsert
    TagEm -> formattingInsert
    TagFont -> formattingInsert
    TagI -> formattingInsert
    TagS -> formattingInsert
    TagSmall -> formattingInsert
    TagStrike -> formattingInsert
    TagStrong -> formattingInsert
    TagTt -> formattingInsert
    TagU -> formattingInsert
    TagApplet -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb name TagApplet attrs Nothing
      pushFormattingMarker tb
      tbSetFramesetOk tb False
    TagMarquee -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb name TagMarquee attrs Nothing
      pushFormattingMarker tb
      tbSetFramesetOk tb False
    TagObject -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb name TagObject attrs Nothing
      pushFormattingMarker tb
      tbSetFramesetOk tb False
    TagTable -> do
      qm <- readIORef (tbQuirksMode tb)
      if qm /= "quirks" then closePElement tb else pure ()
      void $ insertElementT tb "table" TagTable attrs Nothing
      tbSetFramesetOk tb False
      tbSetMode tb MInTable
    -- Void insert tags
    TagArea -> voidInsert
    TagBr -> voidInsert
    TagEmbed -> voidInsert
    TagImg -> voidInsert
    TagKeygen -> voidInsert
    TagWbr -> voidInsert
    TagInput -> do
      reconstructActiveFormatting tb
      void $ insertVoidElementT tb "input" TagInput attrs Nothing
      let isHidden = case attrLookup "type" attrs of
            Just v -> T.toLower v == "hidden"
            Nothing -> False
      if not isHidden then tbSetFramesetOk tb False else pure ()
    TagParam ->
      void $ insertVoidElementT tb name TagParam attrs Nothing
    TagSource ->
      void $ insertVoidElementT tb name TagSource attrs Nothing
    TagTrack ->
      void $ insertVoidElementT tb name TagTrack attrs Nothing
    TagHr -> do
      closePElement tb
      void $ insertVoidElementT tb "hr" TagHr attrs Nothing
      tbSetFramesetOk tb False
    TagImage ->
      modeInBodyStartTag tb "img" attrs _sc TagImg
    TagTextarea -> do
      void $ insertElementT tb "textarea" TagTextarea attrs Nothing
      tbSetFramesetOk tb False
      tbSetIgnoreLF tb True
    TagXmp -> do
      closePElement tb
      reconstructActiveFormatting tb
      void $ insertElementT tb "xmp" TagXmp attrs Nothing
      tbSetFramesetOk tb False
      tbSetOriginalMode tb MInBody
      tbSetMode tb MText
    TagIframe -> do
      void $ insertElementT tb "iframe" TagIframe attrs Nothing
      tbSetFramesetOk tb False
      tbSetOriginalMode tb MInBody
      tbSetMode tb MText
    TagNoembed -> do
      void $ insertElementT tb "noembed" TagNoembed attrs Nothing
      tbSetOriginalMode tb MInBody
      tbSetMode tb MText
    TagSelect -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb "select" TagSelect attrs Nothing
      tbSetFramesetOk tb False
      resetInsertionMode tb
    TagOptgroup -> do
      ctid <- currentNodeTagId tb
      if ctid == TagOption then popElement tb else pure ()
      reconstructActiveFormatting tb
      void $ insertElementT tb name tid attrs Nothing
    TagOption -> do
      ctid <- currentNodeTagId tb
      if ctid == TagOption then popElement tb else pure ()
      reconstructActiveFormatting tb
      void $ insertElementT tb name tid attrs Nothing
    TagRb -> do
      inScope <- hasInScope "ruby" tb
      if inScope then generateImpliedEndTagsT Nothing tb else pure ()
      void $ insertElementT tb name tid attrs Nothing
    TagRtc -> do
      inScope <- hasInScope "ruby" tb
      if inScope then generateImpliedEndTagsT Nothing tb else pure ()
      void $ insertElementT tb name tid attrs Nothing
    TagRp -> do
      inScope <- hasInScope "ruby" tb
      if inScope then generateImpliedEndTagsT (Just TagRtc) tb else pure ()
      void $ insertElementT tb name tid attrs Nothing
    TagRt -> do
      inScope <- hasInScope "ruby" tb
      if inScope then generateImpliedEndTagsT (Just TagRtc) tb else pure ()
      void $ insertElementT tb name tid attrs Nothing
    TagMath -> do
      reconstructActiveFormatting tb
      let !fAttrs = adjustForeignAttrs (adjustMathMLAttrs attrs)
      if _sc
        then void $ insertVoidElementT tb name TagMath fAttrs (Just "math")
        else void $ insertElementT tb name TagMath fAttrs (Just "math")
    TagSvg -> do
      reconstructActiveFormatting tb
      let !fAttrs = adjustForeignAttrs (adjustSVGAttrs attrs)
      if _sc
        then void $ insertVoidElementT tb "svg" TagSvg fAttrs (Just "svg")
        else void $ insertElementT tb "svg" TagSvg fAttrs (Just "svg")
    -- Ignored start tags in body
    TagCaption -> pure ()
    TagCol -> pure ()
    TagColgroup -> pure ()
    TagFrame -> pure ()
    TagHead -> pure ()
    TagTbody -> pure ()
    TagTd -> pure ()
    TagTfoot -> pure ()
    TagTh -> pure ()
    TagThead -> pure ()
    TagTr -> pure ()
    -- Head delegate tags
    TagBase -> modeInHead tb tok
    TagBasefont -> modeInHead tb tok
    TagBgsound -> modeInHead tb tok
    TagLink -> modeInHead tb tok
    TagMeta -> modeInHead tb tok
    TagTemplate -> modeInHead tb tok
    TagTitle -> modeInHead tb tok
    TagNoframes -> modeInHead tb tok
    TagScript -> modeInHead tb tok
    TagStyle -> modeInHead tb tok
    TagNoscript -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb name tid attrs Nothing
      tbSetFramesetOk tb False
    _ -> do
      reconstructActiveFormatting tb
      void $ insertElementT tb name tid attrs Nothing
      tbSetFramesetOk tb False
  where
    tok = TStartTag name attrs _sc tid
    blockInsert = do
      closePElement tb
      void $ insertElementT tb name tid attrs Nothing
    {-# INLINE blockInsert #-}
    headingInsert = do
      closePElement tb
      ctid <- currentNodeTagId tb
      if tagIdIsHeading ctid
        then popElement tb >> void (insertElementT tb name tid attrs Nothing)
        else void $ insertElementT tb name tid attrs Nothing
    {-# INLINE headingInsert #-}
    formattingInsert = do
      reconstructActiveFormatting tb
      node <- insertElementT tb name tid attrs Nothing
      pushFormattingEntry name attrs node tb
    {-# INLINE formattingInsert #-}
    voidInsert = do
      reconstructActiveFormatting tb
      void $ insertVoidElementT tb name tid attrs Nothing
      tbSetFramesetOk tb False
    {-# INLINE voidInsert #-}


modeInBodyEndTag :: TreeBuilder -> Text -> TagId -> IO ()
modeInBodyEndTag !tb !name !tid =
  case tid of
    TagBody -> do
      inScope <- hasInScopeT TagBody "body" tb
      if inScope then tbSetMode tb MAfterBody else pure ()
    TagHtml -> do
      inScope <- hasInScopeT TagBody "body" tb
      if inScope
        then do
          tbSetMode tb MAfterBody
          processInMode tb (TEndTag name tid)
        else pure ()
    TagAddress -> endBlockInsert
    TagArticle -> endBlockInsert
    TagAside -> endBlockInsert
    TagBlockquote -> endBlockInsert
    TagButton -> endBlockInsert
    TagCenter -> endBlockInsert
    TagDetails -> endBlockInsert
    TagDialog -> endBlockInsert
    TagDir -> endBlockInsert
    TagDiv -> endBlockInsert
    TagDl -> endBlockInsert
    TagFieldset -> endBlockInsert
    TagFigcaption -> endBlockInsert
    TagFigure -> endBlockInsert
    TagFooter -> endBlockInsert
    TagHeader -> endBlockInsert
    TagHgroup -> endBlockInsert
    TagListing -> endBlockInsert
    TagMain -> endBlockInsert
    TagMenu -> endBlockInsert
    TagNav -> endBlockInsert
    TagOl -> endBlockInsert
    TagPre -> endBlockInsert
    TagSearch -> endBlockInsert
    TagSection -> endBlockInsert
    TagSummary -> endBlockInsert
    TagUl -> endBlockInsert
    TagForm -> do
      onStack <- isOnStack "template" tb
      mForm <- readIORef (tbFormElement tb)
      if not onStack && mForm /= Nothing
        then do
          writeIORef (tbFormElement tb) Nothing
          formOnStack <- isOnStack "form" tb
          if formOnStack
            then do
              generateImpliedEndTagsT Nothing tb
              removeNameFromStack "form" tb
            else pure ()
        else
          if onStack
            then do
              inScope <- hasInScopeT TagForm "form" tb
              if inScope
                then do
                  generateImpliedEndTagsT Nothing tb
                  popUntilInclusive "form" tb
                else pure ()
            else pure ()
    TagP -> do
      inScope <- hasInButtonScopeT TagP "p" tb
      if inScope
        then do
          generateImpliedEndTagsT (Just TagP) tb
          popUntilInclusive "p" tb
        else do
          void $ insertElementT tb "p" TagP emptySmallArray Nothing
          popUntilInclusive "p" tb
    TagLi -> do
      inScope <- hasInListItemScope "li" tb
      if inScope
        then do
          generateImpliedEndTagsT (Just TagLi) tb
          popUntilInclusive "li" tb
        else pure ()
    TagDd -> do
      inScope <- hasInDefinitionScope name tb
      if inScope
        then do
          generateImpliedEndTagsT (Just TagDd) tb
          popUntilInclusive name tb
        else pure ()
    TagDt -> do
      inScope <- hasInDefinitionScope name tb
      if inScope
        then do
          generateImpliedEndTagsT (Just TagDt) tb
          popUntilInclusive name tb
        else pure ()
    TagH1 -> endHeadingInsert
    TagH2 -> endHeadingInsert
    TagH3 -> endHeadingInsert
    TagH4 -> endHeadingInsert
    TagH5 -> endHeadingInsert
    TagH6 -> endHeadingInsert
    TagA -> adoptionAgency name tb
    TagB -> adoptionAgency name tb
    TagBig -> adoptionAgency name tb
    TagCode -> adoptionAgency name tb
    TagEm -> adoptionAgency name tb
    TagFont -> adoptionAgency name tb
    TagI -> adoptionAgency name tb
    TagNobr -> adoptionAgency name tb
    TagS -> adoptionAgency name tb
    TagSmall -> adoptionAgency name tb
    TagStrike -> adoptionAgency name tb
    TagStrong -> adoptionAgency name tb
    TagTt -> adoptionAgency name tb
    TagU -> adoptionAgency name tb
    TagApplet -> do
      inScope <- hasInScopeT TagApplet "applet" tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilInclusive name tb
          clearActiveFormattingToMarker tb
        else pure ()
    TagMarquee -> do
      inScope <- hasInScopeT TagMarquee "marquee" tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilInclusive name tb
          clearActiveFormattingToMarker tb
        else pure ()
    TagObject -> do
      inScope <- hasInScopeT TagObject "object" tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilInclusive name tb
          clearActiveFormattingToMarker tb
        else pure ()
    TagBr -> do
      reconstructActiveFormatting tb
      void $ insertVoidElementT tb "br" TagBr emptySmallArray Nothing
      tbSetFramesetOk tb False
    TagTemplate -> modeInHead tb (TEndTag name tid)
    _ -> anyOtherEndTagT tid name tb
  where
    endBlockInsert = do
      inScope <- hasInScopeT tid name tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilInclusive name tb
        else pure ()
    {-# INLINE endBlockInsert #-}
    endHeadingInsert = do
      inScope <- hasAnyHeadingInScope tb
      if inScope
        then do
          generateImpliedEndTagsT Nothing tb
          popUntilHeading tb
        else pure ()
    {-# INLINE endHeadingInsert #-}


popUntilHeading :: TreeBuilder -> IO ()
popUntilHeading tb = do
  ctid <- currentNodeTagId tb
  if tagIdIsHeading ctid
    then popElement tb
    else do
      popElement tb
      ctid2 <- currentNodeTagId tb
      if ctid2 == TagUnknown
        then do
          cn2 <- currentNodeName tb
          if cn2 == "" then pure () else popUntilHeading tb
        else popUntilHeading tb


closeLiElements :: TreeBuilder -> IO ()
closeLiElements tb = do
  inScope <- hasInListItemScope "li" tb
  if inScope
    then do
      generateImpliedEndTagsT (Just TagLi) tb
      popUntilInclusiveT TagLi "li" tb
    else pure ()


closeDdDtElements :: TreeBuilder -> IO ()
closeDdDtElements tb = do
  let es = tbStack tb
  sz <- esSize es
  go es (sz - 1)
  where
    go _es !i | i < 0 = pure ()
    go es !i = do
      packed <- esReadTid es i
      let !tid = tidFromPacked packed
          !isHTML = isHTMLFromPacked packed
      if tid == TagDd
        then do
          generateImpliedEndTagsT (Just TagDd) tb
          popUntilInclusiveT TagDd "dd" tb
        else
          if tid == TagDt
            then do
              generateImpliedEndTagsT (Just TagDt) tb
              popUntilInclusiveT TagDt "dt" tb
            else
              if isHTML && tid /= TagUnknown && tagIdIsSpecial tid && tid /= TagAddress && tid /= TagDiv && tid /= TagP
                then pure ()
                else
                  if not isHTML
                    then go es (i - 1)
                    else do
                      node <- esRead es i
                      let !ntid = nodeTagId node
                      if nodeIsHTMLNs node && tagIdIsSpecial ntid && ntid /= TagAddress && ntid /= TagDiv && ntid /= TagP
                        then pure ()
                        else go es (i - 1)


removeActiveFormattingByName :: Text -> TreeBuilder -> IO ()
removeActiveFormattingByName name tb =
  modifyIORef' (tbActiveFormatting tb) go
  where
    go [] = []
    go (AFMarker : rest) = AFMarker : rest
    go (AFEntry n a nd : rest)
      | n == name = rest
      | otherwise = AFEntry n a nd : go rest


removeNameFromStack :: Text -> TreeBuilder -> IO ()
removeNameFromStack name tb =
  esRemoveByName (tbStack tb) name


modeText :: TreeBuilder -> Token -> IO ()
modeText tb tok = case tok of
  TChar c -> appendTextToCurrentNode tb (T.singleton c)
  TString t -> appendTextToCurrentNode tb t
  TEOF -> do
    popElement tb
    origMode <- tbOriginalMode tb
    tbSetMode tb origMode
    processInMode tb tok
  TEndTag _ _ -> do
    popElement tb
    origMode <- tbOriginalMode tb
    tbSetMode tb origMode
  _ -> pure ()


modeInTable :: TreeBuilder -> Token -> IO ()
modeInTable tb tok = case tok of
  TChar _ -> do
    cn <- currentNodeName tb
    if cn `elem` ["table", "tbody", "tfoot", "thead", "tr"]
      then do
        writeIORef (tbPendingTableText tb) []
        origMode <- tbMode tb
        tbSetOriginalMode tb origMode
        tbSetMode tb MInTableText
        modeInTableText tb tok
      else fosterParentToken tb tok
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "caption" attrs _ _ -> do
    clearStackToTableContext tb
    pushFormattingMarker tb
    void $ insertElementT tb "caption" TagCaption attrs Nothing
    tbSetMode tb MInCaption
  TStartTag "colgroup" attrs _ _ -> do
    clearStackToTableContext tb
    void $ insertElementT tb "colgroup" TagColgroup attrs Nothing
    tbSetMode tb MInColumnGroup
  TStartTag "col" _ _ _ -> do
    clearStackToTableContext tb
    void $ insertElementT tb "colgroup" TagColgroup emptySmallArray Nothing
    tbSetMode tb MInColumnGroup
    processInMode tb tok
  TStartTag name attrs _ tid | name `elem` ["tbody", "tfoot", "thead"] -> do
    clearStackToTableContext tb
    void $ insertElementT tb name tid attrs Nothing
    tbSetMode tb MInTableBody
  TStartTag name _ _ _ | name `elem` ["td", "th", "tr"] -> do
    clearStackToTableContext tb
    void $ insertElementT tb "tbody" TagTbody emptySmallArray Nothing
    tbSetMode tb MInTableBody
    processInMode tb tok
  TStartTag "table" _ _ _ -> do
    inScope <- hasInTableScope "table" tb
    if inScope
      then do
        popUntilInclusive "table" tb
        resetInsertionMode tb
        processInMode tb tok
      else pure ()
  TEndTag "table" _ -> do
    inScope <- hasInTableScope "table" tb
    if inScope
      then do
        popUntilInclusive "table" tb
        resetInsertionMode tb
      else pure ()
  TEndTag name _
    | name `elem` ["body", "caption", "col", "colgroup", "html", "tbody", "td", "tfoot", "th", "thead", "tr"] ->
        pure ()
  TStartTag name _ _ _
    | name `elem` ["style", "script", "template"] ->
        modeInHead tb tok
  TEndTag "template" _ -> modeInHead tb tok
  TStartTag "input" attrs _ _ ->
    case attrLookup "type" attrs of
      Just v
        | T.toLower v == "hidden" ->
            void $ insertVoidElementT tb "input" TagInput attrs Nothing
      _ -> fosterParentToken tb tok
  TStartTag "form" attrs _ _ -> do
    mForm <- readIORef (tbFormElement tb)
    onStack <- isOnStack "template" tb
    if mForm == Nothing && not onStack
      then do
        node <- insertElementT tb "form" TagForm attrs Nothing
        writeIORef (tbFormElement tb) (Just node)
        popElement tb
      else pure ()
  TEOF -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms)
      then modeInTemplate tb tok
      else pure ()
  _ -> fosterParentToken tb tok


fosterParentToken :: TreeBuilder -> Token -> IO ()
fosterParentToken tb tok = do
  tbSetInsertFromTable tb True
  modeInBody tb tok
  tbSetInsertFromTable tb False


modeInTableText :: TreeBuilder -> Token -> IO ()
modeInTableText tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c ->
    modifyIORef' (tbPendingTableText tb) (++ [T.singleton c])
  TString t ->
    modifyIORef' (tbPendingTableText tb) (++ [t])
  _ -> do
    pending <- readIORef (tbPendingTableText tb)
    origMode <- tbOriginalMode tb
    tbSetMode tb origMode
    writeIORef (tbPendingTableText tb) []
    let combined = T.concat pending
    if T.all isWS combined
      then do
        appendTextToCurrentNode tb combined
        processInMode tb tok
      else do
        tbSetInsertFromTable tb True
        mapM_
          ( \chunk -> do
              reconstructActiveFormatting tb
              appendTextToCurrentNode tb chunk
              tbSetFramesetOk tb False
          )
          pending
        tbSetInsertFromTable tb False
        processInMode tb tok


modeInCaption :: TreeBuilder -> Token -> IO ()
modeInCaption tb tok = case tok of
  TEndTag "caption" _ -> do
    inScope <- hasInTableScope "caption" tb
    if inScope
      then do
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive "caption" tb
        clearActiveFormattingToMarker tb
        tbSetMode tb MInTable
      else pure ()
  TStartTag name _ _ _ | name `elem` ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"] -> do
    inScope <- hasInTableScope "caption" tb
    if inScope
      then do
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive "caption" tb
        clearActiveFormattingToMarker tb
        tbSetMode tb MInTable
        processInMode tb tok
      else pure ()
  TEndTag "table" _ -> do
    inScope <- hasInTableScope "caption" tb
    if inScope
      then do
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive "caption" tb
        clearActiveFormattingToMarker tb
        tbSetMode tb MInTable
        processInMode tb tok
      else pure ()
  TEndTag name _
    | name `elem` ["body", "col", "colgroup", "html", "tbody", "td", "tfoot", "th", "thead", "tr"] ->
        pure ()
  _ -> modeInBody tb tok


modeInColumnGroup :: TreeBuilder -> Token -> IO ()
modeInColumnGroup tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag "col" attrs _ _ -> void $ insertVoidElementT tb "col" TagCol attrs Nothing
  TEndTag "colgroup" _ -> do
    cn <- currentNodeName tb
    if cn == "colgroup"
      then do
        popElement tb
        tbSetMode tb MInTable
      else pure ()
  TEndTag "col" _ -> pure ()
  TStartTag "template" _ _ _ -> modeInHead tb tok
  TEndTag "template" _ -> modeInHead tb tok
  TEOF -> modeInBody tb tok
  _ -> do
    cn <- currentNodeName tb
    if cn == "colgroup"
      then do
        popElement tb
        tbSetMode tb MInTable
        processInMode tb tok
      else pure ()


modeInTableBody :: TreeBuilder -> Token -> IO ()
modeInTableBody tb tok = case tok of
  TStartTag "tr" attrs _ _ -> do
    clearStackToTableBodyContext tb
    void $ insertElementT tb "tr" TagTr attrs Nothing
    tbSetMode tb MInRow
  TStartTag name attrs _ _ | name == "th" || name == "td" -> do
    clearStackToTableBodyContext tb
    void $ insertElementT tb "tr" TagTr emptySmallArray Nothing
    tbSetMode tb MInRow
    processInMode tb tok
  TEndTag name _ | name `elem` ["tbody", "tfoot", "thead"] -> do
    inScope <- hasInTableScope name tb
    if inScope
      then do
        clearStackToTableBodyContext tb
        popElement tb
        tbSetMode tb MInTable
      else pure ()
  TStartTag name _ _ _ | name `elem` ["caption", "col", "colgroup", "tbody", "tfoot", "thead"] -> do
    tb1 <- hasInTableScope "tbody" tb
    tb2 <- hasInTableScope "thead" tb
    tb3 <- hasInTableScope "tfoot" tb
    if tb1 || tb2 || tb3
      then do
        clearStackToTableBodyContext tb
        popElement tb
        tbSetMode tb MInTable
        processInMode tb tok
      else pure ()
  TEndTag "table" _ -> do
    tb1 <- hasInTableScope "tbody" tb
    tb2 <- hasInTableScope "thead" tb
    tb3 <- hasInTableScope "tfoot" tb
    if tb1 || tb2 || tb3
      then do
        clearStackToTableBodyContext tb
        popElement tb
        tbSetMode tb MInTable
        processInMode tb tok
      else pure ()
  TEndTag name _
    | name `elem` ["body", "caption", "col", "colgroup", "html", "td", "th", "tr"] ->
        pure ()
  _ -> modeInTable tb tok


modeInRow :: TreeBuilder -> Token -> IO ()
modeInRow tb tok = case tok of
  TStartTag name attrs _ tid | name == "th" || name == "td" -> do
    clearStackToTableRowContext tb
    void $ insertElementT tb name tid attrs Nothing
    pushFormattingMarker tb
    tbSetMode tb MInCell
  TEndTag "tr" _ -> do
    inScope <- hasInTableScope "tr" tb
    if inScope
      then do
        clearStackToTableRowContext tb
        popElement tb
        tbSetMode tb MInTableBody
      else pure ()
  TStartTag name _ _ _ | name `elem` ["caption", "col", "colgroup", "tbody", "tfoot", "thead", "tr"] -> do
    inScope <- hasInTableScope "tr" tb
    if inScope
      then do
        clearStackToTableRowContext tb
        popElement tb
        tbSetMode tb MInTableBody
        processInMode tb tok
      else pure ()
  TEndTag "table" _ -> do
    inScope <- hasInTableScope "tr" tb
    if inScope
      then do
        clearStackToTableRowContext tb
        popElement tb
        tbSetMode tb MInTableBody
        processInMode tb tok
      else pure ()
  TEndTag name _ | name `elem` ["tbody", "tfoot", "thead"] -> do
    inScope <- hasInTableScope name tb
    if inScope
      then do
        trScope <- hasInTableScope "tr" tb
        if trScope
          then do
            clearStackToTableRowContext tb
            popElement tb
            tbSetMode tb MInTableBody
            processInMode tb tok
          else pure ()
      else pure ()
  TEndTag name _
    | name `elem` ["body", "caption", "col", "colgroup", "html", "td", "th"] ->
        pure ()
  _ -> modeInTable tb tok


modeInCell :: TreeBuilder -> Token -> IO ()
modeInCell tb tok = case tok of
  TEndTag name _ | name == "td" || name == "th" -> do
    inScope <- hasInTableScope name tb
    if inScope
      then do
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive name tb
        clearActiveFormattingToMarker tb
        tbSetMode tb MInRow
      else pure ()
  TStartTag name _ _ _ | name `elem` ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"] -> do
    tdScope <- hasInTableScope "td" tb
    thScope <- hasInTableScope "th" tb
    if tdScope || thScope
      then do
        let cellName = if tdScope then "td" else "th"
        generateImpliedEndTagsT Nothing tb
        popUntilInclusive cellName tb
        clearActiveFormattingToMarker tb
        tbSetMode tb MInRow
        processInMode tb tok
      else pure ()
  TEndTag name _ | name `elem` ["body", "caption", "col", "colgroup", "html"] -> pure ()
  TEndTag name _ | name `elem` ["table", "tbody", "tfoot", "thead", "tr"] -> do
    inScope <- hasInTableScope name tb
    if inScope
      then do
        tdScope <- hasInTableScope "td" tb
        thScope <- hasInTableScope "th" tb
        if tdScope || thScope
          then do
            let cellName = if tdScope then "td" else "th"
            generateImpliedEndTagsT Nothing tb
            popUntilInclusive cellName tb
            clearActiveFormattingToMarker tb
            tbSetMode tb MInRow
            processInMode tb tok
          else pure ()
      else pure ()
  _ -> modeInBody tb tok


modeInSelect :: TreeBuilder -> Token -> IO ()
modeInSelect tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag "option" attrs _ _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    reconstructActiveFormatting tb
    void $ insertElementT tb "option" TagOption attrs Nothing
  TStartTag "optgroup" attrs _ _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    cn2 <- currentNodeName tb
    if cn2 == "optgroup" then popElement tb else pure ()
    reconstructActiveFormatting tb
    void $ insertElementT tb "optgroup" TagOptgroup attrs Nothing
  TStartTag "hr" attrs _ _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    cn2 <- currentNodeName tb
    if cn2 == "optgroup" then popElement tb else pure ()
    reconstructActiveFormatting tb
    void $ insertVoidElementT tb "hr" TagHr attrs Nothing
  TStartTag "select" _ _ _ -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
  TStartTag name _ _ _ | name `elem` ["input", "textarea"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TStartTag "keygen" attrs _ _ -> do
    reconstructActiveFormatting tb
    void $ insertVoidElementT tb "keygen" TagKeygen attrs Nothing
  TStartTag name _ _ _ | name `elem` ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr", "table"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TStartTag "script" _ _ _ -> modeInHead tb tok
  TStartTag "template" _ _ _ -> modeInHead tb tok
  TStartTag name attrs sc _ | name == "svg" || name == "math" -> do
    reconstructActiveFormatting tb
    let ns = if name == "svg" then Just "svg" else Just "math"
        tidForeign = if name == "svg" then TagSvg else TagMath
    if sc
      then void $ insertVoidElementT tb name tidForeign attrs ns
      else void $ insertElementT tb name (tagIdFromText name) attrs ns
  TStartTag name attrs _ tid | tagIdIsFormatting tid -> do
    reconstructActiveFormatting tb
    node <- insertElementT tb name tid attrs Nothing
    pushFormattingEntry name attrs node tb
  TStartTag "menuitem" attrs _ _ -> do
    reconstructActiveFormatting tb
    void $ insertElementT tb "menuitem" TagMenuitem attrs Nothing
  TStartTag name attrs sc tid | name `elem` ["p", "div", "span", "button", "datalist", "selectedcontent"] -> do
    reconstructActiveFormatting tb
    if sc
      then void $ insertVoidElementT tb name tid attrs Nothing
      else void $ insertElementT tb name tid attrs Nothing
  TStartTag name attrs _ tid | name `elem` ["br", "img"] -> do
    reconstructActiveFormatting tb
    void $ insertVoidElementT tb name tid attrs Nothing
  TStartTag "plaintext" attrs _ _ -> do
    reconstructActiveFormatting tb
    void $ insertElementT tb "plaintext" TagPlaintext attrs Nothing
  TEndTag "optgroup" _ -> do
    cn <- currentNodeName tb
    if cn == "option"
      then do
        elems <- esReadAll (tbStack tb)
        case elems of
          (_ : node2 : _) | nodeName node2 == "optgroup" -> do
            popElement tb
            popElement tb
          _ -> pure ()
      else
        if cn == "optgroup"
          then popElement tb
          else pure ()
  TEndTag "option" _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
  TEndTag "select" _ -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
  TEndTag name tid | tagIdIsFormatting tid || tid == TagA -> do
    selectAdoptionAgency name tb
  TEndTag name _ | name `elem` ["p", "div", "span", "button", "datalist", "selectedcontent"] -> do
    selectCloseElement name tb
  TEndTag name _ | name `elem` ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr", "table"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TEndTag "template" _ -> modeInHead tb tok
  TEOF -> modeInBody tb tok
  TStartTag name attrs sc tid -> do
    reconstructActiveFormatting tb
    if sc
      then void $ insertVoidElementT tb name tid attrs Nothing
      else void $ insertElementT tb name tid attrs Nothing
  _ -> pure ()


selectAdoptionAgency :: Text -> TreeBuilder -> IO ()
selectAdoptionAgency name tb = do
  elems <- esReadAll (tbStack tb)
  let selectIdx = findSelectIdx elems 0
      targetIdx = findTargetIdx name elems 0
  case (selectIdx, targetIdx) of
    (Just si, Just ti)
      | ti > si -> pure ()
    _ -> adoptionAgency name tb
  where
    findSelectIdx [] _ = Nothing
    findSelectIdx (n : ns) i
      | nodeName n == "select" = Just i
      | otherwise = findSelectIdx ns (i + 1)
    findTargetIdx _ [] _ = Nothing
    findTargetIdx tgt (n : ns) i
      | nodeName n == tgt = Just i
      | otherwise = findTargetIdx tgt ns (i + 1)


selectCloseElement :: Text -> TreeBuilder -> IO ()
selectCloseElement name tb = do
  elems <- esReadAll (tbStack tb)
  let selectIdx = findIdx "select" elems 0
      targetIdx = findLastIdx name elems 0 Nothing
  case (selectIdx, targetIdx) of
    (_, Nothing) -> pure ()
    (Nothing, Just _) -> popUntilInclusive name tb
    (Just si, Just ti)
      | ti <= si -> popUntilInclusive name tb
      | otherwise -> pure ()
  where
    findIdx _ [] _ = Nothing
    findIdx tgt (n : ns) i
      | nodeName n == tgt = Just i
      | otherwise = findIdx tgt ns (i + 1)
    findLastIdx _ [] _ acc = acc
    findLastIdx tgt (n : ns) i acc
      | nodeName n == tgt = findLastIdx tgt ns (i + 1) (Just i)
      | otherwise = findLastIdx tgt ns (i + 1) acc


modeInSelectInTable :: TreeBuilder -> Token -> IO ()
modeInSelectInTable tb tok = case tok of
  TStartTag name _ _ _ | name `elem` ["caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TEndTag name _ | name `elem` ["caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th"] -> do
    inScope <- hasInTableScope name tb
    if inScope
      then do
        popUntilInclusive "select" tb
        resetInsertionMode tb
        processInMode tb tok
      else pure ()
  _ -> modeInSelect tb tok


modeInTemplate :: TreeBuilder -> Token -> IO ()
modeInTemplate tb tok = case tok of
  TChar _ -> modeInBody tb tok
  TComment _ -> modeInBody tb tok
  TDoctype _ _ _ _ -> modeInBody tb tok
  TStartTag name _ _ _
    | name `elem` ["base", "basefont", "bgsound", "link", "meta", "noframes", "script", "style", "template", "title"] ->
        modeInHead tb tok
  TEndTag "template" _ -> modeInHead tb tok
  TStartTag name _ _ _ | name `elem` ["caption", "colgroup", "tbody", "tfoot", "thead"] -> do
    replaceTemplateMode MInTable tb
    tbSetMode tb MInTable
    processInMode tb tok
  TStartTag "col" _ _ _ -> do
    replaceTemplateMode MInColumnGroup tb
    tbSetMode tb MInColumnGroup
    processInMode tb tok
  TStartTag "tr" _ _ _ -> do
    replaceTemplateMode MInTableBody tb
    tbSetMode tb MInTableBody
    processInMode tb tok
  TStartTag name _ _ _ | name == "td" || name == "th" -> do
    replaceTemplateMode MInRow tb
    tbSetMode tb MInRow
    processInMode tb tok
  TEOF -> do
    onStack <- isOnStack "template" tb
    if not onStack
      then pure ()
      else do
        popUntilInclusive "template" tb
        clearActiveFormattingToMarker tb
        modifyIORef' (tbTemplateModes tb) safeTail
        resetInsertionMode tb
        processInMode tb tok
  TStartTag _ _ _ _ -> do
    replaceTemplateMode MInBody tb
    tbSetMode tb MInBody
    processInMode tb tok
  _ -> pure ()


replaceTemplateMode :: InsertionMode -> TreeBuilder -> IO ()
replaceTemplateMode newMode tb =
  modifyIORef' (tbTemplateModes tb) (\case (_ : rest) -> newMode : rest; [] -> [newMode])


modeAfterBody :: TreeBuilder -> Token -> IO ()
modeAfterBody tb tok = case tok of
  TChar c | isWS c -> modeInBody tb tok
  TComment t -> do
    elems <- esReadAll (tbStack tb)
    case reverse elems of
      (htmlNode : _) ->
        when (tbBuildDOM tb) $ pushChild (nodeChildren htmlNode) (TBCComment t)
      [] -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TEndTag "html" _ -> tbSetMode tb MAfterAfterBody
  TEOF -> pure ()
  _ -> do
    tbSetMode tb MInBody
    processInMode tb tok


modeInFrameset :: TreeBuilder -> Token -> IO ()
modeInFrameset tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag "frameset" attrs _ _ -> void $ insertElementT tb "frameset" TagFrameset attrs Nothing
  TEndTag "frameset" _ -> do
    cn <- currentNodeName tb
    if cn == "html"
      then pure ()
      else do
        popElement tb
        cn2 <- currentNodeName tb
        if cn2 /= "frameset"
          then tbSetMode tb MAfterFrameset
          else pure ()
  TStartTag "frame" attrs _ _ -> void $ insertVoidElementT tb "frame" TagFrame attrs Nothing
  TStartTag "noframes" attrs _ _ -> do
    void $ insertElementT tb "noframes" TagNoframes attrs Nothing
    tbSetOriginalMode tb MInFrameset
    tbSetMode tb MText
  TEOF -> pure ()
  _ -> pure ()


modeAfterFrameset :: TreeBuilder -> Token -> IO ()
modeAfterFrameset tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TEndTag "html" _ -> tbSetMode tb MAfterAfterFrameset
  TStartTag "noframes" attrs _ _ -> do
    void $ insertElementT tb "noframes" TagNoframes attrs Nothing
    tbSetOriginalMode tb MAfterFrameset
    tbSetMode tb MText
  TEOF -> pure ()
  _ -> pure ()


modeAfterAfterBody :: TreeBuilder -> Token -> IO ()
modeAfterAfterBody tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> modeInBody tb tok
  TChar c | isWS c -> modeInBody tb tok
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TEOF -> pure ()
  _ -> do
    tbSetMode tb MInBody
    processInMode tb tok


modeAfterAfterFrameset :: TreeBuilder -> Token -> IO ()
modeAfterAfterFrameset tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> modeInBody tb tok
  TChar c | isWS c -> modeInBody tb tok
  TStartTag "html" _ _ _ -> modeInBody tb tok
  TStartTag "noframes" _ _ _ -> modeInHead tb tok
  TEOF -> pure ()
  _ -> pure ()


------------------------------------------------------------------------
-- Foreign content
------------------------------------------------------------------------

processForeignContent :: TreeBuilder -> Token -> IO ()
processForeignContent tb tok = case tok of
  TChar '\0' -> appendTextToCurrentNode tb "\xFFFD"
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TChar c -> do
    appendTextToCurrentNode tb (T.singleton c)
    tbSetFramesetOk tb False
  TString t -> do
    appendTextToCurrentNode tb t
    if not (T.all isWS t) then tbSetFramesetOk tb False else pure ()
  TComment t
    | isCDATA t ->
        let content = T.replace "\0" "\xFFFD" (cdataContent t)
        in if T.null content
            then pure ()
            else appendTextToCurrentNode tb content
    | otherwise -> insertComment tb t
  TStartTag name attrs sc _tid -> do
    let !breakoutTid = tagIdFromText (T.toLower name)
    if tagIdIsForeignBreakout breakoutTid
      || (breakoutTid == TagFont && hasFontBreakoutAttr attrs)
      then do
        popUntilHTMLOrIntegrationPoint tb
        resetInsertionMode tb
        processInMode tb tok
      else do
        mn <- currentNode tb
        let ns = maybe Nothing nodeNs mn
            adjustedName = case ns of
              Just "svg" -> adjustSVGTagName name
              _ -> name
            !adjustedAttrs = case ns of
              Just "svg" -> adjustForeignAttrs (adjustSVGAttrs attrs)
              Just "math" -> adjustForeignAttrs (adjustMathMLAttrs attrs)
              _ -> adjustForeignAttrs attrs
        if sc
          then void $ insertVoidElementT tb adjustedName (tagIdFromText adjustedName) adjustedAttrs ns
          else void $ insertElementT tb adjustedName (tagIdFromText adjustedName) adjustedAttrs ns
  TEndTag name tid -> do
    let nameLower = T.toLower name
    if nameLower == "br" || nameLower == "p"
      then do
        popUntilHTMLOrIntegrationPoint tb
        resetInsertionMode tb
        processInMode tb tok
      else foreignEndTag nameLower tb
  _ -> pure ()


foreignEndTag :: Text -> TreeBuilder -> IO ()
foreignEndTag name tb = do
  elems <- esReadAll (tbStack tb)
  case elems of
    [] -> pure ()
    _ -> go 0 elems
  where
    go _ [] = pure ()
    go idx (node : rest)
      | T.toLower (nodeName node) == name = do
          case tbFragmentContextElement tb of
            Just fce | node == fce -> pure ()
            _ -> do
              elems2 <- esReadAll (tbStack tb)
              esWriteList (tbStack tb) (dropThrough node elems2)
      | otherwise =
          case rest of
            [] -> pure ()
            (nextNode : _)
              | nodeIsHTMLNs nextNode ->
                  processInMode tb (TEndTag name (tagIdFromText name))
              | otherwise -> go (idx + 1) rest
    dropThrough target (x : xs) | x == target = xs
    dropThrough target (_ : xs) = dropThrough target xs
    dropThrough _ [] = []


hasFontBreakoutAttr :: SmallArray HTMLAttribute -> Bool
hasFontBreakoutAttr attrs =
  any (\(HTMLAttribute n _) -> let !ln = T.toLower n in ln == "color" || ln == "face" || ln == "size") attrs


popUntilHTMLOrIntegrationPoint :: TreeBuilder -> IO ()
popUntilHTMLOrIntegrationPoint tb = do
  elems <- esReadAll (tbStack tb)
  case elems of
    [] -> pure ()
    (node : _)
      | nodeIsHTMLNs node -> pure ()
      | isFragCtxElem node -> pure ()
      | otherwise -> do
          isHIP <- isHTMLIntegrationPoint node
          isMTIP <- isMathMLTIP node
          if isHIP || isMTIP
            then pure ()
            else do
              popElement tb
              popUntilHTMLOrIntegrationPoint tb
  where
    isFragCtxElem node = case tbFragmentContextElement tb of
      Just fce -> node == fce
      Nothing -> False
    isMathMLTIP node = pure $ nodeNs node == Just "math" && nodeName node `elem` ["mi", "mo", "mn", "ms", "mtext"]
    isHTMLIntegrationPoint node
      | nodeNs node == Just "svg" = pure $ nodeName node `elem` ["foreignObject", "desc", "title"]
      | nodeNs node == Just "math" && nodeName node == "annotation-xml" = do
          attrs <- readNodeAttrs node
          pure $ case attrLookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html", "application/xhtml+xml"]
            Nothing -> False
      | otherwise = pure False


------------------------------------------------------------------------
-- Text node helpers
------------------------------------------------------------------------

appendTextToCurrentNode :: TreeBuilder -> Text -> IO ()
appendTextToCurrentNode tb txt = do
  sz <- esSize (tbStack tb)
  if sz > 0
    then do
      current <- esTopUnsafe (tbStack tb)
      insertFromTable <- tbInsertFromTable tb
      if insertFromTable && isFosterTarget (nodeTagId current)
        then fosterParentText tb txt
        else do
          let ref = if nodeIsTemplate current then nodeTemplateContents current else nodeChildren current
          when (tbBuildDOM tb) $ pushText ref txt
    else do
      docNodes <- readIORef (tbDocument tb)
      case findHtmlRootDoc docNodes of
        Just htmlRoot -> when (tbBuildDOM tb) $ pushText (nodeChildren htmlRoot) txt
        Nothing -> when (tbBuildDOM tb) $ modifyIORef' (tbDocument tb) (appendTextToDocChildren txt)
  tbEmitEvent tb (TreeText txt)
  where
    findHtmlRootDoc [] = Nothing
    findHtmlRootDoc (CElement n : _) | nodeTagId n == TagHtml = Just n
    findHtmlRootDoc (_ : rest) = findHtmlRootDoc rest


appendTextToDocChildren :: Text -> [ChildNode] -> [ChildNode]
appendTextToDocChildren txt [] = [CText txt]
appendTextToDocChildren txt children =
  case last children of
    CText prev -> init children ++ [CText (prev <> txt)]
    _ -> children ++ [CText txt]


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINE isFosterTarget #-}
isFosterTarget :: TagId -> Bool
isFosterTarget !tid = case tid of
  TagTable -> True
  TagTbody -> True
  TagTfoot -> True
  TagThead -> True
  TagTr -> True
  _ -> False


nameWithNs :: Text -> Maybe Text -> Text
nameWithNs name Nothing = name
nameWithNs name (Just "") = name
nameWithNs name (Just "html") = name
nameWithNs name (Just ns) = ns <> " " <> name


safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs


void :: IO a -> IO ()
void m = m >> pure ()


------------------------------------------------------------------------
-- Quirks mode
------------------------------------------------------------------------

determineQuirksMode :: Text -> Maybe Text -> Maybe Text -> Bool -> Text
determineQuirksMode name pub sys fq
  | fq = "quirks"
  | T.toLower name /= "html" = "quirks"
  | matchesQuirkyPublic (fmap T.toLower pub) = "quirks"
  | matchesQuirkySystem (fmap T.toLower sys) = "quirks"
  | matchesLimitedQuirky (fmap T.toLower pub) sys = "limited-quirks"
  | otherwise = "no-quirks"


matchesQuirkyPublic :: Maybe Text -> Bool
matchesQuirkyPublic Nothing = False
matchesQuirkyPublic (Just p) =
  p `elem` ["-//w3o//dtd w3 html strict 3.0//en//", "-/w3c/dtd html 4.0 transitional/en", "html"]
    || any (`T.isPrefixOf` p) quirkyPublicPrefixes


matchesQuirkySystem :: Maybe Text -> Bool
matchesQuirkySystem Nothing = False
matchesQuirkySystem (Just s) = s == "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd"


matchesLimitedQuirky :: Maybe Text -> Maybe Text -> Bool
matchesLimitedQuirky pub _sys = case pub of
  Nothing -> False
  Just p ->
    any
      (`T.isPrefixOf` p)
      ["-//w3c//dtd xhtml 1.0 frameset//", "-//w3c//dtd xhtml 1.0 transitional//"]
      || ( any
            (`T.isPrefixOf` p)
            ["-//w3c//dtd html 4.01 frameset//", "-//w3c//dtd html 4.01 transitional//"]
            && _sys /= Nothing
         )


quirkyPublicPrefixes :: [Text]
quirkyPublicPrefixes =
  [ "-//advasoft ltd//dtd html 3.0 aswedit + extensions//"
  , "-//as//dtd html 3.0 aswedit + extensions//"
  , "-//ietf//dtd html 2.0 level 1//"
  , "-//ietf//dtd html 2.0 level 2//"
  , "-//ietf//dtd html 2.0 strict level 1//"
  , "-//ietf//dtd html 2.0 strict level 2//"
  , "-//ietf//dtd html 2.0 strict//"
  , "-//ietf//dtd html 2.0//"
  , "-//ietf//dtd html 2.1e//"
  , "-//ietf//dtd html 3.0//"
  , "-//ietf//dtd html 3.2 final//"
  , "-//ietf//dtd html 3.2//"
  , "-//ietf//dtd html 3//"
  , "-//ietf//dtd html level 0//"
  , "-//ietf//dtd html level 1//"
  , "-//ietf//dtd html level 2//"
  , "-//ietf//dtd html level 3//"
  , "-//ietf//dtd html strict level 0//"
  , "-//ietf//dtd html strict level 1//"
  , "-//ietf//dtd html strict level 2//"
  , "-//ietf//dtd html strict level 3//"
  , "-//ietf//dtd html strict//"
  , "-//ietf//dtd html//"
  , "-//metrius//dtd metrius presentational//"
  , "-//microsoft//dtd internet explorer 2.0 html strict//"
  , "-//microsoft//dtd internet explorer 2.0 html//"
  , "-//microsoft//dtd internet explorer 2.0 tables//"
  , "-//microsoft//dtd internet explorer 3.0 html strict//"
  , "-//microsoft//dtd internet explorer 3.0 html//"
  , "-//microsoft//dtd internet explorer 3.0 tables//"
  , "-//netscape comm. corp.//dtd html//"
  , "-//netscape comm. corp.//dtd strict html//"
  , "-//o'reilly and associates//dtd html 2.0//"
  , "-//o'reilly and associates//dtd html extended 1.0//"
  , "-//o'reilly and associates//dtd html extended relaxed 1.0//"
  , "-//softquad software//dtd hotmetal pro 6.0::19990601::extensions to html 4.0//"
  , "-//softquad//dtd hotmetal pro 4.0::19971010::extensions to html 4.0//"
  , "-//spyglass//dtd html 2.0 extended//"
  , "-//sq//dtd html 2.0 hotmetal + extensions//"
  , "-//sun microsystems corp.//dtd hotjava html//"
  , "-//sun microsystems corp.//dtd hotjava strict html//"
  , "-//w3c//dtd html 3 1995-03-24//"
  , "-//w3c//dtd html 3.2 draft//"
  , "-//w3c//dtd html 3.2 final//"
  , "-//w3c//dtd html 3.2//"
  , "-//w3c//dtd html 3.2s draft//"
  , "-//w3c//dtd html 4.0 frameset//"
  , "-//w3c//dtd html 4.0 transitional//"
  , "-//w3c//dtd html experimental 19960712//"
  , "-//w3c//dtd html experimental 970421//"
  , "-//w3c//dtd w3 html//"
  , "-//w3o//dtd w3 html 3.0//"
  , "-//webtechs//dtd mozilla html 2.0//"
  , "-//webtechs//dtd mozilla html//"
  ]


------------------------------------------------------------------------
-- SVG/MathML/Foreign attribute adjustments
------------------------------------------------------------------------

svgTagNameAdjustments :: [(Text, Text)]
svgTagNameAdjustments =
  [ ("altglyph", "altGlyph")
  , ("altglyphdef", "altGlyphDef")
  , ("altglyphitem", "altGlyphItem")
  , ("animatecolor", "animateColor")
  , ("animatemotion", "animateMotion")
  , ("animatetransform", "animateTransform")
  , ("clippath", "clipPath")
  , ("feblend", "feBlend")
  , ("fecolormatrix", "feColorMatrix")
  , ("fecomponenttransfer", "feComponentTransfer")
  , ("fecomposite", "feComposite")
  , ("feconvolvematrix", "feConvolveMatrix")
  , ("fediffuselighting", "feDiffuseLighting")
  , ("fedisplacementmap", "feDisplacementMap")
  , ("fedistantlight", "feDistantLight")
  , ("feflood", "feFlood")
  , ("fefunca", "feFuncA")
  , ("fefuncb", "feFuncB")
  , ("fefuncg", "feFuncG")
  , ("fefuncr", "feFuncR")
  , ("fegaussianblur", "feGaussianBlur")
  , ("feimage", "feImage")
  , ("femerge", "feMerge")
  , ("femergenode", "feMergeNode")
  , ("femorphology", "feMorphology")
  , ("feoffset", "feOffset")
  , ("fepointlight", "fePointLight")
  , ("fespecularlighting", "feSpecularLighting")
  , ("fespotlight", "feSpotLight")
  , ("fetile", "feTile")
  , ("feturbulence", "feTurbulence")
  , ("foreignobject", "foreignObject")
  , ("glyphref", "glyphRef")
  , ("lineargradient", "linearGradient")
  , ("radialgradient", "radialGradient")
  , ("textpath", "textPath")
  ]


adjustSVGTagName :: Text -> Text
adjustSVGTagName name = case lookup (T.toLower name) svgTagNameAdjustments of
  Just adj -> adj
  Nothing -> name


adjustSVGAttrs :: SmallArray HTMLAttribute -> SmallArray HTMLAttribute
adjustSVGAttrs = fmap (\(HTMLAttribute n v) -> HTMLAttribute (lookupDef n (T.toLower n) svgAttrAdjustments) v)


svgAttrAdjustments :: [(Text, Text)]
svgAttrAdjustments =
  [ ("attributename", "attributeName")
  , ("attributetype", "attributeType")
  , ("basefrequency", "baseFrequency")
  , ("baseprofile", "baseProfile")
  , ("calcmode", "calcMode")
  , ("clippathunits", "clipPathUnits")
  , ("diffuseconstant", "diffuseConstant")
  , ("edgemode", "edgeMode")
  , ("filterunits", "filterUnits")
  , ("glyphref", "glyphRef")
  , ("gradienttransform", "gradientTransform")
  , ("gradientunits", "gradientUnits")
  , ("kernelmatrix", "kernelMatrix")
  , ("kernelunitlength", "kernelUnitLength")
  , ("keypoints", "keyPoints")
  , ("keysplines", "keySplines")
  , ("keytimes", "keyTimes")
  , ("lengthadjust", "lengthAdjust")
  , ("limitingconeangle", "limitingConeAngle")
  , ("markerheight", "markerHeight")
  , ("markerunits", "markerUnits")
  , ("markerwidth", "markerWidth")
  , ("maskcontentunits", "maskContentUnits")
  , ("maskunits", "maskUnits")
  , ("numoctaves", "numOctaves")
  , ("pathlength", "pathLength")
  , ("patterncontentunits", "patternContentUnits")
  , ("patterntransform", "patternTransform")
  , ("patternunits", "patternUnits")
  , ("pointsatx", "pointsAtX")
  , ("pointsaty", "pointsAtY")
  , ("pointsatz", "pointsAtZ")
  , ("preservealpha", "preserveAlpha")
  , ("preserveaspectratio", "preserveAspectRatio")
  , ("primitiveunits", "primitiveUnits")
  , ("refx", "refX")
  , ("refy", "refY")
  , ("repeatcount", "repeatCount")
  , ("repeatdur", "repeatDur")
  , ("requiredextensions", "requiredExtensions")
  , ("requiredfeatures", "requiredFeatures")
  , ("specularconstant", "specularConstant")
  , ("specularexponent", "specularExponent")
  , ("spreadmethod", "spreadMethod")
  , ("startoffset", "startOffset")
  , ("stddeviation", "stdDeviation")
  , ("stitchtiles", "stitchTiles")
  , ("surfacescale", "surfaceScale")
  , ("systemlanguage", "systemLanguage")
  , ("tablevalues", "tableValues")
  , ("targetx", "targetX")
  , ("targety", "targetY")
  , ("textlength", "textLength")
  , ("viewbox", "viewBox")
  , ("viewtarget", "viewTarget")
  , ("xchannelselector", "xChannelSelector")
  , ("ychannelselector", "yChannelSelector")
  , ("zoomandpan", "zoomAndPan")
  ]


adjustMathMLAttrs :: SmallArray HTMLAttribute -> SmallArray HTMLAttribute
adjustMathMLAttrs = fmap (\(HTMLAttribute n v) -> HTMLAttribute (lookupDef n (T.toLower n) [("definitionurl", "definitionURL")]) v)


adjustForeignAttrs :: SmallArray HTMLAttribute -> SmallArray HTMLAttribute
adjustForeignAttrs =
  fmap
    ( \(HTMLAttribute n v) -> case lookup (T.toLower n) foreignAttrAdj of
        Just (prefix, local) -> if T.null prefix then HTMLAttribute local v else HTMLAttribute (prefix <> ":" <> local) v
        Nothing -> HTMLAttribute n v
    )


foreignAttrAdj :: [(Text, (Text, Text))]
foreignAttrAdj =
  [ ("xlink:actuate", ("xlink", "actuate"))
  , ("xlink:arcrole", ("xlink", "arcrole"))
  , ("xlink:href", ("xlink", "href"))
  , ("xlink:role", ("xlink", "role"))
  , ("xlink:show", ("xlink", "show"))
  , ("xlink:title", ("xlink", "title"))
  , ("xlink:type", ("xlink", "type"))
  , ("xml:lang", ("xml", "lang"))
  , ("xml:space", ("xml", "space"))
  , ("xmlns", ("", "xmlns"))
  , ("xmlns:xlink", ("xmlns", "xlink"))
  ]


lookupDef :: Text -> Text -> [(Text, Text)] -> Text
lookupDef def key table = case lookup key table of Just v -> v; Nothing -> def


------------------------------------------------------------------------
-- tbNodeToHTMLNode (for building final output)
------------------------------------------------------------------------
-- (already defined above in the "Build final document" section)

------------------------------------------------------------------------
-- Tokenizer
------------------------------------------------------------------------

tokenize :: Text -> [Token]
tokenize txt = let !bs = TE.encodeUtf8 txt in tokenizeBS bs 0 (BS.length bs) 0 False


-- ByteString-based hot-path tokenizer

tokenizeBS :: ByteString -> Int -> Int -> Int -> Bool -> [Token]
tokenizeBS !bs !off !len !svgD !svgH
  | off >= len = []
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
          then
            let !end = scanText bs (off + 1) len
                !t = TE.decodeUtf8Lenient (BSU.unsafeTake (end - off) (BSU.unsafeDrop off bs))
            in TString t : tokenizeBS bs end len svgD svgH
          else case b of
            0x3C -> tokenizeTagBS bs (off + 1) len svgD svgH
            0x26 ->
              let !windowEnd = min len (off + 65)
                  !input = toStringFrom bs (off + 1) windowEnd
                  (ent, rest) = parseEntityRef input
                  !consumed = length input - length rest
              in map TChar ent ++ tokenizeBS bs (off + 1 + consumed) len svgD svgH
            0x00 -> TChar '\0' : tokenizeBS bs (off + 1) len svgD svgH
            0x0D ->
              let !next = off + 1
              in TChar '\n'
                  : tokenizeBS
                    bs
                    (if next < len && BSU.unsafeIndex bs next == 0x0A then next + 1 else next)
                    len
                    svgD
                    svgH
            _ -> tokenizeBS bs (off + 1) len svgD svgH


-- IO-based fused tokenizer: calls processToken directly, no intermediate list
tokenizeBSIO :: ByteString -> Int -> Int -> Int -> Bool -> TreeBuilder -> IO ()
tokenizeBSIO !bs !off0 !len !svgD0 !svgH0 !tb = go off0 svgD0 svgH0
  where
    !(BS (ForeignPtr addr# _) _) = bs
    !sharedBA = case runRW#
      ( \s0 ->
          case newByteArray# len# s0 of
            (# s1, mba# #) ->
              case copyAddrToByteArray# addr# mba# 0# len# s1 of
                s2 ->
                  case unsafeFreezeByteArray# mba# s2 of
                    (# s3, ba# #) ->
                      (# s3, ba# #)
      ) of
      (# _, ba# #) -> ByteArray ba#
      where
        !(I# len#) = len
    !scalars = tbScalars tb

    emit !tok = processToken tb tok
    {-# INLINE emit #-}

    {-# INLINE emitText #-}
    emitText !t !firstByte = do
          mode <- readMode scalars
          case mode of
            MInBody -> do
              ignoreLF <- readBoolSlot scalars sIgnoreLF
              if ignoreLF
                then emit (TString t)
                else do
                  hasAF <- readScalar scalars sHasAF
                  if hasAF /= 0 then reconstructActiveFormatting tb else pure ()
                  let !(ElementStack esArr esCnt _) = tbStack tb
                  n <- readByteArray esCnt 0 :: IO Int
                  cur <- readSmallArray esArr (n - 1)
                  when (tbBuildDOM tb) $ pushText (nodeChildren cur) t
                  tbEmitEvent tb (TreeText t)
                  if not (isWSByte firstByte)
                    then writeScalar scalars sFramesetOk 0
                    else do
                      fo <- readScalar scalars sFramesetOk
                      if fo == 0
                        then pure ()
                        else
                          if not (T.all isWS t)
                            then writeScalar scalars sFramesetOk 0
                            else pure ()
            MText -> appendTextToCurrentNode tb t
            _ -> emit (TString t)

    {-# INLINE emitStartTag #-}
    emitStartTag !lcName attrs !selfClose !tid = do
          ignoreLF <- readBoolSlot scalars sIgnoreLF
          if ignoreLF then tbSetIgnoreLF tb False else pure ()
          mode <- readMode scalars
          case mode of
            MInBody -> do
              let ElementStack _ esCnt tidsBuf = tbStack tb
              n <- readByteArray esCnt 0 :: IO Int
              if n > 0
                then do
                  packed <- readByteArray tidsBuf (n - 1) :: IO Int
                  if isHTMLFromPacked packed
                    then modeInBodyStartTag tb lcName attrs selfClose tid
                    else emit (TStartTag lcName attrs selfClose tid)
                else emit (TStartTag lcName attrs selfClose tid)
            _ -> emit (TStartTag lcName attrs selfClose tid)

    {-# INLINE emitEndTag #-}
    emitEndTag !lcName !tid = do
          ignoreLF <- readBoolSlot scalars sIgnoreLF
          if ignoreLF then tbSetIgnoreLF tb False else pure ()
          mode <- readMode scalars
          case mode of
            MInBody -> do
              let ElementStack _ esCnt tidsBuf = tbStack tb
              n <- readByteArray esCnt 0 :: IO Int
              if n > 0
                then do
                  packed <- readByteArray tidsBuf (n - 1) :: IO Int
                  if isHTMLFromPacked packed
                    then do
                      let !topTid = tidFromPacked packed
                      if topTid == tid && endTagCanFastPop tid
                        then do
                          tbEmitEvent tb (TreeClose lcName)
                          writeByteArray esCnt 0 (n - 1 :: Int)
                          if tid == TagP
                            then do
                              pc <- readScalar scalars sPOnStack
                              writeScalar scalars sPOnStack (max 0 (pc - 1))
                            else pure ()
                        else modeInBodyEndTag tb lcName tid
                    else emit (TEndTag lcName tid)
                else emit (TEndTag lcName tid)
            _ -> emit (TEndTag lcName tid)

    go !off !svgD !svgH
      | off >= len = pure ()
      | otherwise =
          let !b = readByteOff addr# off
          in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
              then do
                let !(ScanTextResult end ascii) = scanTextAscii addr# (off + 1) len
                    !firstByteAscii = b < 0x80
                    !allAscii = firstByteAscii && ascii
                    !t = decodeTextSliceKnown sharedBA off (end - off) bs allAscii
                emitText t b
                go end svgD svgH
              else case b of
                0x3C -> goTag (off + 1) svgD svgH
                0x26 -> do
                  let !windowEnd = min len (off + 65)
                      !input = toStringFrom bs (off + 1) windowEnd
                      (ent, rest) = parseEntityRef input
                      !consumed = length input - length rest
                  mapM_ (emit . TChar) ent
                  go (off + 1 + consumed) svgD svgH
                0x00 -> do
                  emit (TChar '\0')
                  go (off + 1) svgD svgH
                0x0D -> do
                  emit (TChar '\n')
                  let !next = off + 1
                  go (if next < len && readByteOff addr# next == 0x0A then next + 1 else next) svgD svgH
                _ -> go (off + 1) svgD svgH

    goTag !off !svgD !svgH
      | off >= len = emit (TChar '<')
      | otherwise = case readByteOff addr# off of
          0x21 -> do
            let toks = tokenizeMarkupDeclCtx svgD svgH (toStringFrom bs (off + 1) len)
            mapM_ emit toks
          0x2F -> goEndTag (off + 1) svgD svgH
          0x3F -> do
            let (comment, remaining) = readUntilStr ">" (toStringFrom bs (off + 1) len)
            emit (TComment (T.pack ('?' : comment)))
            let toks = tokenizeCtx svgD svgH remaining
            mapM_ emit toks
          b | isAlphaByte b -> goStartTag off svgD svgH
          _ -> do
            emit (TChar '<')
            go off svgD svgH

    goStartTag !off !svgD !svgH =
      let !nameEnd = scanTagNameFast addr# off len
          !tagLen = nameEnd - off
          (!lcName, !tid) = internTagAddr addr# off tagLen bs
          (!attrs, !selfClose, !afterTag) = readTagAttrsBS sharedBA bs nameEnd len
          !newSvgD = if tid == TagSvg then svgD + 1 else svgD
          !inSvg = newSvgD > 0
          !newSvgH =
            if inSvg && isSvgHtmlIntegPoint tid lcName
              then True
              else if tid == TagSvg then False else svgH
      in if afterTag > len
          then pure ()
          else
            if isRawTextTag tid
              then do
                if selfClose
                  then do emitStartTag lcName attrs selfClose tid; go afterTag newSvgD newSvgH
                  else do
                    emitStartTag lcName attrs selfClose tid
                    let toks = tokenizeRawText (toStringFrom bs afterTag len) lcName
                    mapM_ emit toks
              else
                if isRCDataTag tid
                  then do
                    if selfClose || inSvg
                      then do emitStartTag lcName attrs selfClose tid; go afterTag newSvgD newSvgH
                      else do
                        emitStartTag lcName attrs selfClose tid
                        let toks = tokenizeRCData (toStringFrom bs afterTag len) lcName
                        mapM_ emit toks
                  else
                    if tid == TagPlaintext
                      then do
                        if inSvg && not newSvgH
                          then do emitStartTag lcName attrs selfClose tid; go afterTag newSvgD newSvgH
                          else do
                            emitStartTag lcName attrs selfClose tid
                            let toks = tokenizePlaintext (toStringFrom bs afterTag len)
                            mapM_ emit toks
                      else do
                        emitStartTag lcName attrs selfClose tid
                        go afterTag newSvgD newSvgH

    goEndTag !off !svgD !svgH
      | off >= len = do emit (TChar '<'); emit (TChar '/')
      | isAlphaByte (readByteOff addr# off) =
          let !nameEnd = scanTagNameFast addr# off len
              !tagLen = nameEnd - off
              (!lcName, !tid) = internTagAddr addr# off tagLen bs
              !afterGt = skipToGtBS bs nameEnd len
              !newSvgD = if tid == TagSvg && svgD > 0 then svgD - 1 else svgD
              !newSvgH = if tid == TagSvg && svgD > 0 then False else svgH
          in if nameEnd >= len
              then pure ()
              else do
                emitEndTag lcName tid
                go afterGt newSvgD newSvgH
      | readByteOff addr# off == 0x3E = do
          emit (TComment "")
          go (off + 1) svgD svgH
      | otherwise = do
          let (comment, remaining) = readUntilStr ">" (toStringFrom bs off len)
          emit (TComment (T.pack comment))
          let toks = tokenizeCtx svgD svgH remaining
          mapM_ emit toks


{- | Tokenize a ByteString using the fast IO tokenizer, calling the
provided callback for each token with its byte range in the input.
Uses the same Addr#-based scanning as the tree-builder tokenizer.

The callback receives @(token, startOffset, endOffset)@ where the
offsets delimit the raw bytes of that token in the input. For tokens
produced by String-based sub-parsers (comments, raw text content),
offsets are @(-1, -1)@.

When @parseAttrs@ is False, start tags carry an empty attribute array
and the caller should use the raw byte range instead. This avoids
allocating attribute arrays when they won't be inspected.
-}
tokenizeCallbackIO :: ByteString -> (Token -> Int -> Int -> IO ()) -> IO ()
tokenizeCallbackIO = tokenizeCallbackIOWith True
{-# INLINE tokenizeCallbackIO #-}


tokenizeCallbackIOWith :: Bool -> ByteString -> (Token -> Int -> Int -> IO ()) -> IO ()
tokenizeCallbackIOWith !parseAttrs !bs emit = go 0 0 False
  where
    !len = BS.length bs
    !(BS (ForeignPtr addr# _) _) = bs
    !sharedBA = case runRW#
      ( \s0 ->
          case newByteArray# len# s0 of
            (# s1, mba# #) ->
              case copyAddrToByteArray# addr# mba# 0# len# s1 of
                s2 ->
                  case unsafeFreezeByteArray# mba# s2 of
                    (# s3, ba# #) ->
                      (# s3, ba# #)
      ) of
      (# _, ba# #) -> ByteArray ba#
      where
        !(I# len#) = len

    go !off !svgD !svgH
      | off >= len = pure ()
      | otherwise =
          let !b = readByteOff addr# off
          in if b /= 0x3C && b /= 0x26 && b /= 0x00 && b /= 0x0D
              then do
                let !(ScanTextResult end ascii) = scanTextAscii addr# (off + 1) len
                    !firstByteAscii = b < 0x80
                    !allAscii = firstByteAscii && ascii
                    !t = decodeTextSliceKnown sharedBA off (end - off) bs allAscii
                emit (TString t) off end
                go end svgD svgH
              else case b of
                0x3C -> goTag (off + 1) off svgD svgH
                0x26 -> do
                  let !windowEnd = min len (off + 65)
                      !input = toStringFrom bs (off + 1) windowEnd
                      (ent, rest) = parseEntityRef input
                      !consumed = length input - length rest
                      !entEnd = off + 1 + consumed
                  mapM_ (\c -> emit (TChar c) off entEnd) ent
                  go entEnd svgD svgH
                0x00 -> do
                  emit (TChar '\0') off (off + 1)
                  go (off + 1) svgD svgH
                0x0D -> do
                  let !next = off + 1
                      !end = if next < len && readByteOff addr# next == 0x0A then next + 1 else next
                  emit (TChar '\n') off end
                  go end svgD svgH
                _ -> go (off + 1) svgD svgH

    goTag !off !ltOff !svgD !svgH
      | off >= len = emit (TChar '<') ltOff len
      | otherwise = case readByteOff addr# off of
          0x21 -> do
            let toks = tokenizeMarkupDeclCtx svgD svgH (toStringFrom bs (off + 1) len)
            mapM_ (\tok -> emit tok (-1) (-1)) toks
          0x2F -> goEndTag (off + 1) ltOff svgD svgH
          0x3F -> do
            let (comment, remaining) = readUntilStr ">" (toStringFrom bs (off + 1) len)
            emit (TComment (T.pack ('?' : comment))) (-1) (-1)
            let toks = tokenizeCtx svgD svgH remaining
            mapM_ (\tok -> emit tok (-1) (-1)) toks
          b | isAlphaByte b -> goStartTag off ltOff svgD svgH
          _ -> do
            emit (TChar '<') ltOff (ltOff + 1)
            go off svgD svgH

    goStartTag !off !ltOff !svgD !svgH =
      let !nameEnd = scanTagNameFast addr# off len
          !tagLen = nameEnd - off
          (!lcName, !tid) = internTagAddr addr# off tagLen bs
          (!attrs, !selfClose, !afterTag)
            | parseAttrs = readTagAttrsBS sharedBA bs nameEnd len
            | otherwise = case skipTagBS addr# nameEnd len of
                (# sc, at #) -> (emptySmallArray, sc, at)
          !newSvgD = if tid == TagSvg then svgD + 1 else svgD
          !inSvg = newSvgD > 0
          !newSvgH =
            if inSvg && isSvgHtmlIntegPoint tid lcName
              then True
              else if tid == TagSvg then False else svgH
      in if afterTag > len
          then pure ()
          else
            if isRawTextTag tid
              then do
                if selfClose
                  then do emit (TStartTag lcName attrs selfClose tid) ltOff afterTag; go afterTag newSvgD newSvgH
                  else do
                    emit (TStartTag lcName attrs selfClose tid) ltOff afterTag
                    let toks = tokenizeRawText (toStringFrom bs afterTag len) lcName
                    mapM_ (\tok -> emit tok (-1) (-1)) toks
              else
                if isRCDataTag tid
                  then do
                    if selfClose || inSvg
                      then do emit (TStartTag lcName attrs selfClose tid) ltOff afterTag; go afterTag newSvgD newSvgH
                      else do
                        emit (TStartTag lcName attrs selfClose tid) ltOff afterTag
                        let toks = tokenizeRCData (toStringFrom bs afterTag len) lcName
                        mapM_ (\tok -> emit tok (-1) (-1)) toks
                  else
                    if tid == TagPlaintext
                      then do
                        if inSvg && not newSvgH
                          then do emit (TStartTag lcName attrs selfClose tid) ltOff afterTag; go afterTag newSvgD newSvgH
                          else do
                            emit (TStartTag lcName attrs selfClose tid) ltOff afterTag
                            let toks = tokenizePlaintext (toStringFrom bs afterTag len)
                            mapM_ (\tok -> emit tok (-1) (-1)) toks
                      else do
                        emit (TStartTag lcName attrs selfClose tid) ltOff afterTag
                        go afterTag newSvgD newSvgH

    goEndTag !off !ltOff !svgD !svgH
      | off >= len = do emit (TChar '<') ltOff (ltOff + 1); emit (TChar '/') (ltOff + 1) (ltOff + 2)
      | isAlphaByte (readByteOff addr# off) =
          let !nameEnd = scanTagNameFast addr# off len
              !tagLen = nameEnd - off
              (!lcName, !tid) = internTagAddr addr# off tagLen bs
              !afterGt = skipToGtBS bs nameEnd len
              !newSvgD = if tid == TagSvg && svgD > 0 then svgD - 1 else svgD
              !newSvgH = if tid == TagSvg && svgD > 0 then False else svgH
          in if nameEnd >= len
              then pure ()
              else do
                emit (TEndTag lcName tid) ltOff afterGt
                go afterGt newSvgD newSvgH
      | readByteOff addr# off == 0x3E = do
          emit (TComment "") ltOff (off + 1)
          go (off + 1) svgD svgH
      | otherwise = do
          let (comment, remaining) = readUntilStr ">" (toStringFrom bs off len)
          emit (TComment (T.pack comment)) (-1) (-1)
          let toks = tokenizeCtx svgD svgH remaining
          mapM_ (\tok -> emit tok (-1) (-1)) toks
{-# NOINLINE tokenizeCallbackIOWith #-}


scanText :: ByteString -> Int -> Int -> Int
scanText !bs !off !len
  | off >= len = len
  | otherwise =
      let !b = BSU.unsafeIndex bs off
      in if b == 0x3C || b == 0x26 || b == 0x00 || b == 0x0D
          then off
          else scanText bs (off + 1) len


{-# INLINE scanTextFast #-}
scanTextFast :: Addr# -> Int -> Int -> Int
scanTextFast addr# !off !end =
  let !(CPtrdiff r) = c_scan_text addr# (fromIntegral off) (fromIntegral end)
  in fromIntegral r


data ScanTextResult = ScanTextResult {-# UNPACK #-} !Int !Bool


{-# INLINE scanTextAscii #-}
scanTextAscii :: Addr# -> Int -> Int -> ScanTextResult
scanTextAscii addr# !off !end =
  let !(CPtrdiff packed) = c_scan_text_ascii addr# (fromIntegral off) (fromIntegral end)
      !pos = fromIntegral (unsafeShiftR packed 1)
      !ascii = packed .&. 1 /= 0
  in ScanTextResult pos ascii


tokenizeTagBS :: ByteString -> Int -> Int -> Int -> Bool -> [Token]
tokenizeTagBS !bs !off !len !svgD !svgH
  | off >= len = [TChar '<']
  | otherwise = case BSU.unsafeIndex bs off of
      0x21 -> tokenizeMarkupDeclCtx svgD svgH (toStringFrom bs (off + 1) len)
      0x2F -> tokenizeEndTagBS bs (off + 1) len svgD svgH
      0x3F ->
        let (comment, remaining) = readUntilStr ">" (toStringFrom bs (off + 1) len)
        in TComment (T.pack ('?' : comment)) : tokenizeCtx svgD svgH remaining
      b | isAlphaByte b -> tokenizeStartTagBS bs off len svgD svgH
      _ -> TChar '<' : tokenizeBS bs off len svgD svgH


{-# INLINE isAlphaByte #-}
isAlphaByte :: Word8 -> Bool
isAlphaByte b = (b >= 0x41 && b <= 0x5A) || (b >= 0x61 && b <= 0x7A)


tokenizeStartTagBS :: ByteString -> Int -> Int -> Int -> Bool -> [Token]
tokenizeStartTagBS !bs !off !len !svgD !svgH =
  let !nameEnd = scanTagName bs off len
      !nameBS = BSU.unsafeTake (nameEnd - off) (BSU.unsafeDrop off bs)
      (!lcName, !tid) = internTagBS nameBS
      !ba = bsToByteArray bs
      (!attrsV, !selfClose, !afterTag) = readTagAttrsBS ba bs nameEnd len
      !tok = TStartTag lcName attrsV selfClose tid
      !newSvgD = if lcName == "svg" then svgD + 1 else svgD
      !inSvg = newSvgD > 0
      !newSvgH =
        if inSvg && lcName `elem` ["foreignobject", "desc", "title"]
          then True
          else if lcName == "svg" then False else svgH
  in if afterTag > len
      then []
      else case () of
        _
          | lcName `elem` ["script", "style", "xmp", "iframe", "noembed", "noframes", "noscript"] ->
              if selfClose
                then tok : tokenizeBS bs afterTag len newSvgD newSvgH
                else tok : tokenizeRawText (toStringFrom bs afterTag len) lcName
          | lcName == "textarea" || lcName == "title" ->
              if selfClose || inSvg
                then tok : tokenizeBS bs afterTag len newSvgD newSvgH
                else tok : tokenizeRCData (toStringFrom bs afterTag len) lcName
          | lcName == "plaintext" ->
              if inSvg && not newSvgH
                then tok : tokenizeBS bs afterTag len newSvgD newSvgH
                else tok : tokenizePlaintext (toStringFrom bs afterTag len)
          | otherwise -> tok : tokenizeBS bs afterTag len newSvgD newSvgH


scanTagName :: ByteString -> Int -> Int -> Int
scanTagName !bs !off !len
  | off >= len = off
  | isTagNameByte (BSU.unsafeIndex bs off) = scanTagName bs (off + 1) len
  | otherwise = off


{-# INLINE scanTagNameFast #-}
scanTagNameFast :: Addr# -> Int -> Int -> Int
scanTagNameFast addr# !off !end =
  let !(CPtrdiff r) = c_scan_tagname addr# (fromIntegral off) (fromIntegral end)
  in fromIntegral r


{-# INLINE isTagNameByte #-}
isTagNameByte :: Word8 -> Bool
isTagNameByte b =
  (b >= 0x61 && b <= 0x7A)
    || (b >= 0x41 && b <= 0x5A)
    || (b >= 0x30 && b <= 0x39)
    || b == 0x2D
    || b == 0x5F
    || b == 0x3A
    || b == 0x2E
    || b == 0x3C


tokenizeEndTagBS :: ByteString -> Int -> Int -> Int -> Bool -> [Token]
tokenizeEndTagBS !bs !off !len !svgD !svgH
  | off >= len = [TChar '<', TChar '/']
  | isAlphaByte (BSU.unsafeIndex bs off) =
      let !nameEnd = scanTagName bs off len
          !nameBS = BSU.unsafeTake (nameEnd - off) (BSU.unsafeDrop off bs)
          (!lcName, !tid) = internTagBS nameBS
          !afterGt = skipToGtBS bs nameEnd len
          !newSvgD = if lcName == "svg" && svgD > 0 then svgD - 1 else svgD
          !newSvgH = if lcName == "svg" && svgD > 0 then False else svgH
      in if nameEnd >= len
          then []
          else TEndTag lcName tid : tokenizeBS bs afterGt len newSvgD newSvgH
  | BSU.unsafeIndex bs off == 0x3E = TComment "" : tokenizeBS bs (off + 1) len svgD svgH
  | otherwise =
      let (comment, remaining) = readUntilStr ">" (toStringFrom bs off len)
      in TComment (T.pack comment) : tokenizeCtx svgD svgH remaining


skipToGtBS :: ByteString -> Int -> Int -> Int
skipToGtBS !bs !off !len
  | off >= len = len
  | BSU.unsafeIndex bs off == 0x3E = off + 1
  | otherwise = skipToGtBS bs (off + 1) len


{-# INLINE skipAttrsBS #-}
skipAttrsBS :: ByteString -> Int -> Int -> Int
skipAttrsBS !bs !off !len =
  let !(BS (ForeignPtr addr# _) _) = bs
      !(CPtrdiff packed) = c_skip_attrs addr# (fromIntegral off) (fromIntegral len)
  in fromIntegral (packed `quot` 2)


{-# INLINE skipTagBS #-}
skipTagBS :: Addr# -> Int -> Int -> (# Bool, Int #)
skipTagBS addr# = go
  where
    go !i !len
      | i >= len = (# False, len + 1 #)
      | otherwise = case readByteOff addr# i of
          0x3E -> (# False, i + 1 #)
          0x2F | i + 1 < len, readByteOff addr# (i + 1) == 0x3E -> (# True, i + 2 #)
          0x22 -> go (scanPast 0x22 (i + 1) len) len
          0x27 -> go (scanPast 0x27 (i + 1) len) len
          _ -> go (i + 1) len

    scanPast :: Word8 -> Int -> Int -> Int
    scanPast !q !i !len
      | i >= len = len
      | readByteOff addr# i == q = i + 1
      | otherwise = scanPast q (i + 1) len


-- | Scan tag attrs, extracting only the class value (as ByteString slice).
-- Returns (# classOff, classLen, selfClose, afterTag #) where classOff = -1
-- means no class attribute found.  Zero allocation.
{-# INLINE scanClassAndSkip #-}
scanClassAndSkip :: Addr# -> Int -> Int -> (# Int, Int, Bool, Int #)
scanClassAndSkip addr# !off0 !len = go off0 (-1) 0
  where
    rd :: Int -> Word8
    rd = readByteOff addr#
    {-# INLINE rd #-}

    go !i !cOff !cLen
      | i >= len = (# cOff, cLen, False, len + 1 #)
      | otherwise = case rd i of
          0x3E -> (# cOff, cLen, False, i + 1 #)
          0x2F | i + 1 < len, rd (i + 1) == 0x3E -> (# cOff, cLen, True, i + 2 #)
          b | isWSByte b -> go (i + 1) cOff cLen
          _ ->
            let !nameStart = i
                !nameEnd = scanAttrNameEnd i
                !nameLen = nameEnd - nameStart
                !isClass = nameLen == 5
                         && (rd nameStart .|. 0x20) == 0x63       -- c
                         && (rd (nameStart+1) .|. 0x20) == 0x6C   -- l
                         && (rd (nameStart+2) .|. 0x20) == 0x61   -- a
                         && (rd (nameStart+3) .|. 0x20) == 0x73   -- s
                         && (rd (nameStart+4) .|. 0x20) == 0x73   -- s
            in if nameLen == 0
                then go (max (i + 1) nameEnd) cOff cLen
                else
                  let !i2 = skipWSAddr addr# nameEnd len
                  in if i2 >= len || rd i2 /= 0x3D
                      then go i2 cOff cLen
                      else
                        let !i3 = skipWSAddr addr# (i2 + 1) len
                        in if i3 >= len
                            then go i3 cOff cLen
                            else case rd i3 of
                              0x22 -> let !vStart = i3 + 1
                                          !vEnd = scanPastQ 0x22 vStart
                                          !vLen = max 0 (vEnd - 1 - vStart)
                                      in if isClass
                                          then go vEnd vStart vLen
                                          else go vEnd cOff cLen
                              0x27 -> let !vStart = i3 + 1
                                          !vEnd = scanPastQ 0x27 vStart
                                          !vLen = max 0 (vEnd - 1 - vStart)
                                      in if isClass
                                          then go vEnd vStart vLen
                                          else go vEnd cOff cLen
                              _ -> let !vEnd = scanUnquotedEnd i3
                                   in if isClass
                                       then go vEnd i3 (vEnd - i3)
                                       else go vEnd cOff cLen

    scanAttrNameEnd !j
      | j >= len = j
      | otherwise = case rd j of
          0x3D -> j
          0x3E -> j
          0x2F -> j
          b | isWSByte b -> j
          _ -> scanAttrNameEnd (j + 1)

    scanPastQ :: Word8 -> Int -> Int
    scanPastQ !q !j
      | j >= len = len
      | rd j == q = j + 1
      | otherwise = scanPastQ q (j + 1)

    scanUnquotedEnd :: Int -> Int
    scanUnquotedEnd !j
      | j >= len = len
      | otherwise = case rd j of
          0x3E -> j
          b | isWSByte b -> j
          _ -> scanUnquotedEnd (j + 1)


{-# INLINE readTagAttrsBS #-}
readTagAttrsBS :: ByteArray -> ByteString -> Int -> Int -> (SmallArray HTMLAttribute, Bool, Int)
readTagAttrsBS !ba !bs !off !len =
  let !(acc, !n, sc, endOff) = go off [] 0
  in case n of
      0 -> (emptySmallArray, sc, endOff)
      1 | (a : _) <- acc -> (createSmallArray 1 a (\_ -> pure ()), sc, endOff)
      _ -> (smallArrayFromListN_Rev n acc, sc, endOff)
  where
    !(BS (ForeignPtr addr# _) _) = bs
    rd :: Int -> Word8
    rd = readByteOff addr#
    {-# INLINE rd #-}

    go !i !acc !n
      | i >= len = (acc, n, False, len + 1)
      | otherwise = case rd i of
          0x3E -> (acc, n, False, i + 1)
          0x2F ->
            if i + 1 < len && rd (i + 1) == 0x3E
              then (acc, n, True, i + 2)
              else go (i + 1) acc n
          b | isWSByte b -> go (i + 1) acc n
          _ ->
            let (!name, !i2) = scanAttrName i
                (!val, !i3) = scanAttrVal i2
            in if T.null name
                then go (max (i + 1) i3) acc n
                else
                  let !attr = HTMLAttribute name val
                  in if n > 0 && any (\(HTMLAttribute na _) -> na == name) acc
                    then go i3 acc n
                    else go i3 (attr : acc) (n + 1)

    scanAttrName !i = sn i
      where
        sn !j
          | j >= len = (internAttrNameRange bs i j, j)
          | otherwise = case rd j of
              0x3D -> (internAttrNameRange bs i j, j)
              0x3E -> (internAttrNameRange bs i j, j)
              0x2F -> (internAttrNameRange bs i j, j)
              b | isWSByte b -> (internAttrNameRange bs i j, j)
              _ -> sn (j + 1)

    scanAttrVal !i
      | i >= len = (T.empty, i)
      | otherwise =
          let !i1 = skipWSAddr addr# i len
          in if i1 >= len || rd i1 /= 0x3D
              then (T.empty, i1)
              else
                let !i2 = skipWSAddr addr# (i1 + 1) len
                in if i2 >= len
                    then (T.empty, i2)
                    else case rd i2 of
                      0x22 -> scanDQuoted (i2 + 1)
                      0x27 -> scanSQuoted (i2 + 1)
                      _ -> scanUnquoted i2

    scanDQuoted !i =
      let !(CPtrdiff packed) = c_scan_dquote_ascii addr# (fromIntegral i) (fromIntegral len)
          !h = fromIntegral (unsafeShiftR packed 1)
          !ascii = packed .&. 1 /= 0
      in if h >= len
          then (decodeTextSliceKnown ba i (h - i) bs ascii, h)
          else
            if rd h == 0x22
              then (decodeTextSliceKnown ba i (h - i) bs ascii, h + 1)
              else
                dqEntityLoop
                  h
                  (if h > i then [decodeTextSliceKnown ba i (h - i) bs ascii] else [])

    dqEntityLoop !ampPos !chunks =
      let !windowEnd = min len (ampPos + 65)
          !input = toStringFrom bs (ampPos + 1) windowEnd
          (ent, rest) = parseEntityRefInAttr input
          !consumed = length input - length rest
          !nextPos = ampPos + 1 + consumed
          !(CPtrdiff hit) = c_scan_dquote addr# (fromIntegral nextPos) (fromIntegral len)
          !h = fromIntegral hit
          !entT = T.pack ent
          !chunks' =
            if h > nextPos
              then decodeTextSlice ba addr# nextPos (h - nextPos) bs : entT : chunks
              else entT : chunks
      in if h >= len
          then (T.concat (reverse chunks'), h)
          else
            if BSU.unsafeIndex bs h == 0x22
              then (T.concat (reverse chunks'), h + 1)
              else dqEntityLoop h chunks'

    scanSQuoted !i =
      let !(CPtrdiff packed) = c_scan_squote_ascii addr# (fromIntegral i) (fromIntegral len)
          !h = fromIntegral (unsafeShiftR packed 1)
          !ascii = packed .&. 1 /= 0
      in if h >= len
          then (decodeTextSliceKnown ba i (h - i) bs ascii, h)
          else
            if rd h == 0x27
              then (decodeTextSliceKnown ba i (h - i) bs ascii, h + 1)
              else
                sqEntityLoop
                  h
                  (if h > i then [decodeTextSliceKnown ba i (h - i) bs ascii] else [])

    sqEntityLoop !ampPos !chunks =
      let !windowEnd = min len (ampPos + 65)
          !input = toStringFrom bs (ampPos + 1) windowEnd
          (ent, rest) = parseEntityRefInAttr input
          !consumed = length input - length rest
          !nextPos = ampPos + 1 + consumed
          !(CPtrdiff hit) = c_scan_squote addr# (fromIntegral nextPos) (fromIntegral len)
          !h = fromIntegral hit
          !entT = T.pack ent
          !chunks' =
            if h > nextPos
              then decodeTextSlice ba addr# nextPos (h - nextPos) bs : entT : chunks
              else entT : chunks
      in if h >= len
          then (T.concat (reverse chunks'), h)
          else
            if BSU.unsafeIndex bs h == 0x27
              then (T.concat (reverse chunks'), h + 1)
              else sqEntityLoop h chunks'

    scanUnquoted !i =
      let !(CPtrdiff hit) = c_scan_unquoted addr# (fromIntegral i) (fromIntegral len)
          !h = fromIntegral hit
      in if h >= len
          then (decodeTextSlice ba addr# i (h - i) bs, h)
          else
            if BSU.unsafeIndex bs h /= 0x26
              then (decodeTextSlice ba addr# i (h - i) bs, h)
              else
                uqEntityLoop
                  h
                  (if h > i then [decodeTextSlice ba addr# i (h - i) bs] else [])

    uqEntityLoop !ampPos !chunks =
      let !windowEnd = min len (ampPos + 65)
          !input = toStringFrom bs (ampPos + 1) windowEnd
          (ent, rest) = parseEntityRefInAttr input
          !consumed = length input - length rest
          !nextPos = ampPos + 1 + consumed
          !(CPtrdiff hit) = c_scan_unquoted addr# (fromIntegral nextPos) (fromIntegral len)
          !h = fromIntegral hit
          !entT = T.pack ent
          !chunks' =
            if h > nextPos
              then decodeTextSlice ba addr# nextPos (h - nextPos) bs : entT : chunks
              else entT : chunks
      in if h >= len
          then (T.concat (reverse chunks'), h)
          else
            if BSU.unsafeIndex bs h /= 0x26
              then (T.concat (reverse chunks'), h)
              else uqEntityLoop h chunks'


{-# INLINE isWSByte #-}
isWSByte :: Word8 -> Bool
isWSByte b = b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D || b == 0x0C


{-# INLINE bsSlice #-}
bsSlice :: ByteString -> Int -> Int -> ByteString
bsSlice !b !start !end = BSU.unsafeTake (end - start) (BSU.unsafeDrop start b)


{-# INLINE readWordAt #-}
readWordAt :: Addr# -> Int -> Word
readWordAt addr# (I# off#) = W# (indexWordOffAddr# (addr# `plusAddr#` off#) 0#)


{-# INLINE readByteOff #-}
readByteOff :: Addr# -> Int -> Word8
readByteOff addr# (I# i#) = W8# (indexWord8OffAddr# addr# i#)


{-# INLINE isAllASCII #-}
isAllASCII :: Addr# -> Int -> Int -> Bool
isAllASCII addr# !off !end =
  c_is_all_ascii addr# (fromIntegral off) (fromIntegral end) /= 0


{-# INLINE isAllASCIIFast #-}
isAllASCIIFast :: Addr# -> Int -> Int -> Bool
isAllASCIIFast addr# !off !sliceLen
  | sliceLen <= 0 = True
  | sliceLen <= 8 =
      let !(W# w) = W# (indexWordOffAddr# (addr# `plusAddr#` o#) 0#)
          !(I# o#) = off
          !mask = unsafeShiftL 1 (unsafeShiftL sliceLen 3) - 1
      in (W# w) .&. mask .&. 0x8080808080808080 == 0
  | sliceLen <= 16 =
      let !(I# o#) = off
          !w1 = W# (indexWordOffAddr# (addr# `plusAddr#` o#) 0#)
          !(I# o2#) = off + 8
          !w2 = W# (indexWordOffAddr# (addr# `plusAddr#` o2#) 0#)
          !mask2 = unsafeShiftL 1 (unsafeShiftL (sliceLen - 8) 3) - 1
      in (w1 .|. (w2 .&. mask2)) .&. 0x8080808080808080 == 0
  | otherwise = isAllASCII addr# off (off + sliceLen)


{-# INLINE decodeTextSlice #-}
decodeTextSlice :: ByteArray -> Addr# -> Int -> Int -> ByteString -> Text
decodeTextSlice !ba addr# !off !sliceLen !origBS
  | sliceLen == 0 = T.empty
  | isAllASCIIFast addr# off sliceLen = Text ba off sliceLen
  | otherwise = decodeTextSliceSlow origBS off sliceLen


{-# INLINE decodeTextSliceKnown #-}
decodeTextSliceKnown :: ByteArray -> Int -> Int -> ByteString -> Bool -> Text
decodeTextSliceKnown !ba !off !sliceLen !origBS !isAscii
  | sliceLen == 0 = T.empty
  | isAscii = Text ba off sliceLen
  | otherwise = decodeTextSliceSlow origBS off sliceLen


{-# NOINLINE decodeTextSliceSlow #-}
decodeTextSliceSlow :: ByteString -> Int -> Int -> Text
decodeTextSliceSlow !origBS !off !sliceLen =
  TE.decodeUtf8Lenient (bsSlice origBS off (off + sliceLen))


{-# NOINLINE bsToByteArray #-}
bsToByteArray :: ByteString -> ByteArray
bsToByteArray (BS (ForeignPtr addr# _) len) =
  case runRW#
    ( \s0 ->
        case newByteArray# len# s0 of
          (# s1, mba# #) ->
            case copyAddrToByteArray# addr# mba# 0# len# s1 of
              s2 ->
                case unsafeFreezeByteArray# mba# s2 of
                  (# s3, ba# #) ->
                    (# s3, ba# #)
    ) of
    (# _, ba# #) -> ByteArray ba#
  where
    !(I# len#) = len


{-# INLINE byteStringPinnedByteArray #-}
byteStringPinnedByteArray :: ByteString -> ByteArray
byteStringPinnedByteArray = bsToByteArray


skipWS :: ByteString -> Int -> Int -> Int
skipWS !bs !off !len
  | off >= len = len
  | isWSByte (BSU.unsafeIndex bs off) = skipWS bs (off + 1) len
  | otherwise = off


{-# INLINE skipWSAddr #-}
skipWSAddr :: Addr# -> Int -> Int -> Int
skipWSAddr addr# !off !len
  | off >= len = len
  | isWSByte (readByteOff addr# off) = skipWSAddr addr# (off + 1) len
  | otherwise = off


toStringFrom :: ByteString -> Int -> Int -> String
toStringFrom bsS offS lenS =
  T.unpack (TE.decodeUtf8Lenient (BSU.unsafeTake (lenS - offS) (BSU.unsafeDrop offS bsS)))


-- String-based tokenizer (bridge path for comments, doctype, raw text, etc.)

tokenizeCtx :: Int -> Bool -> String -> [Token]
tokenizeCtx !svgDepth !svgHIP [] = []
tokenizeCtx svgDepth svgHIP cs@(c : _)
  | c /= '<' && c /= '&' && c /= '\0' && c /= '\r' =
      let (text, rest) = span (\x -> x /= '<' && x /= '&' && x /= '\0' && x /= '\r') cs
      in TString (T.pack text) : tokenizeCtx svgDepth svgHIP rest
  | c == '<' = tokenizeAfterLTCtx svgDepth svgHIP (tail cs)
  | c == '&' =
      let (entity, remaining) = parseEntityRef (tail cs)
      in map TChar entity ++ tokenizeCtx svgDepth svgHIP remaining
  | c == '\0' = TChar '\0' : tokenizeCtx svgDepth svgHIP (tail cs)
  | c == '\r' = case tail cs of
      ('\n' : rest) -> TChar '\n' : tokenizeCtx svgDepth svgHIP rest
      rest -> TChar '\n' : tokenizeCtx svgDepth svgHIP rest
  | otherwise = TChar c : tokenizeCtx svgDepth svgHIP (tail cs)


tokenizeNormal :: String -> [Token]
tokenizeNormal = tokenizeCtx 0 False


tokenizeAfterLT :: String -> [Token]
tokenizeAfterLT = tokenizeAfterLTCtx 0 False


tokenizeAfterLTCtx :: Int -> Bool -> String -> [Token]
tokenizeAfterLTCtx _ _ [] = [TChar '<']
tokenizeAfterLTCtx svgDepth svgHIP ('!' : rest) = tokenizeMarkupDeclCtx svgDepth svgHIP rest
tokenizeAfterLTCtx svgDepth svgHIP ('/' : rest) = tokenizeEndTagCtx svgDepth svgHIP rest
tokenizeAfterLTCtx svgDepth svgHIP ('?' : rest) =
  let (comment, remaining) = readUntilStr ">" rest
  in TComment (T.pack ('?' : comment)) : tokenizeCtx svgDepth svgHIP remaining
tokenizeAfterLTCtx svgDepth svgHIP (c : rest)
  | isAlpha c =
      let (name, rest1) = span isTagNameChar (c : rest)
          lcName = map toLower name
          (attrs, selfClose, rest2) = readTagAttrs rest1
          !lcText = T.pack lcName
          !tid = tagIdFromText lcText
          !tok = TStartTag lcText attrs selfClose tid
          newSvgDepth = if lcName == "svg" then svgDepth + 1 else svgDepth
          inSvg = newSvgDepth > 0
          isSvgHIPTag = lcName `elem` ["foreignobject", "desc", "title"]
          newSvgHIP =
            if inSvg && isSvgHIPTag
              then True
              else
                if lcName == "svg"
                  then False
                  else svgHIP
          eofInTag = case rest2 of
            ('\x00' : _) -> False
            _ -> True
          rest2' = case rest2 of ('\x00' : r) -> r; _ -> rest2
      in if eofInTag
          then []
          else case lcName of
            n
              | n `elem` ["script", "style", "xmp", "iframe", "noembed", "noframes", "noscript"] ->
                  if selfClose
                    then tok : tokenizeCtx newSvgDepth newSvgHIP rest2'
                    else tok : tokenizeRawText rest2' (T.pack lcName)
            "textarea" ->
              if selfClose || inSvg
                then tok : tokenizeCtx newSvgDepth newSvgHIP rest2'
                else tok : tokenizeRCData rest2' (T.pack lcName)
            "title" ->
              if selfClose || inSvg
                then tok : tokenizeCtx newSvgDepth newSvgHIP rest2'
                else tok : tokenizeRCData rest2' (T.pack lcName)
            "plaintext" ->
              if inSvg && not newSvgHIP
                then tok : tokenizeCtx newSvgDepth newSvgHIP rest2'
                else tok : tokenizePlaintext rest2'
            _ -> tok : tokenizeCtx newSvgDepth newSvgHIP rest2'
  | otherwise = TChar '<' : tokenizeCtx svgDepth svgHIP (c : rest)


anyGt :: String -> Bool
anyGt [] = False
anyGt ('>' : _) = True
anyGt (_ : rest) = anyGt rest


tokenizeEndTag :: String -> [Token]
tokenizeEndTag = tokenizeEndTagCtx 0 False


tokenizeEndTagCtx :: Int -> Bool -> String -> [Token]
tokenizeEndTagCtx _ _ [] = [TChar '<', TChar '/']
tokenizeEndTagCtx svgDepth svgHIP (c : rest)
  | isAlpha c =
      let (name, rest1) = span isTagNameChar (c : rest)
          lcName = map toLower name
          rest2 = skipToGtStr rest1
          newSvgDepth = if lcName == "svg" && svgDepth > 0 then svgDepth - 1 else svgDepth
          newSvgHIP = if lcName == "svg" && svgDepth > 0 then False else svgHIP
      in if null rest1 && null rest2
          then []
          else
            let !lcText = T.pack lcName; !tid = tagIdFromText lcText
            in TEndTag lcText tid : tokenizeCtx newSvgDepth newSvgHIP rest2
  | c == '>' = TComment "" : tokenizeCtx svgDepth svgHIP rest
  | otherwise =
      let (comment, remaining) = readUntilStr ">" (c : rest)
      in TComment (T.pack comment) : tokenizeCtx svgDepth svgHIP remaining


tokenizeMarkupDecl :: String -> [Token]
tokenizeMarkupDecl = tokenizeMarkupDeclCtx 0 False


tokenizeMarkupDeclCtx :: Int -> Bool -> String -> [Token]
tokenizeMarkupDeclCtx svgDepth svgHIP ('-' : '-' : rest) =
  let (comment, remaining) = readComment rest
  in TComment (T.pack comment) : tokenizeCtx svgDepth svgHIP remaining
tokenizeMarkupDeclCtx svgDepth svgHIP rest
  | matchCaseI rest "doctype" =
      let rest1 = drop 7 rest
      in tokenizeDoctypeCtx svgDepth svgHIP rest1
  | matchCaseI rest "[cdata[" =
      let rest1 = drop 7 rest
          (content, remaining) = readUntilStr "]]>" rest1
      in TComment (cdataMarker <> T.pack (normalizeCR content)) : tokenizeCtx svgDepth svgHIP remaining
  | otherwise =
      let (comment, remaining) = readBogusComment rest
      in TComment (T.pack comment) : tokenizeCtx svgDepth svgHIP remaining


readBogusComment :: String -> (String, String)
readBogusComment [] = ("", [])
readBogusComment ('>' : rest) = ("", rest)
readBogusComment ('\0' : rest) =
  let (more, remaining) = readBogusComment rest
  in ('\xFFFD' : more, remaining)
readBogusComment (c : rest) =
  let (more, remaining) = readBogusComment rest
  in (c : more, remaining)


readComment :: String -> (String, String)
readComment ('>' : rest) = ("", rest)
readComment ('-' : '>' : rest) = ("", rest)
readComment cs = go [] cs
  where
    go acc [] = (reverse acc, [])
    go acc ('-' : '-' : '>' : rest) = (reverse acc, rest)
    go acc ('-' : '-' : '!' : '>' : rest) = (reverse acc, rest)
    go acc ('-' : '-' : []) = (reverse acc, [])
    go acc ('\0' : rest) = go ('\xFFFD' : acc) rest
    go acc (c : rest) = go (c : acc) rest


tokenizeDoctype :: String -> [Token]
tokenizeDoctype = tokenizeDoctypeCtx 0 False


tokenizeDoctypeCtx :: Int -> Bool -> String -> [Token]
tokenizeDoctypeCtx svgDepth svgHIP cs =
  let cs1 = dropWhile isSp cs
      (name, cs2) = readDoctypeName cs1
      cs3 = dropWhile isSp cs2
      (pub, sys, fq, cs4) = readDoctypeIds cs3
  in TDoctype (T.pack name) pub sys fq : tokenizeCtx svgDepth svgHIP cs4
  where
    isSp c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'


readDoctypeName :: String -> (String, String)
readDoctypeName = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('>' : rest) = (reverse acc, '>' : rest)
    go acc (c : rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' = (reverse acc, c : rest)
      | otherwise = go (toLower c : acc) rest


readDoctypeIds :: String -> (Maybe Text, Maybe Text, Bool, String)
readDoctypeIds [] = (Nothing, Nothing, False, [])
readDoctypeIds ('>' : rest) = (Nothing, Nothing, False, rest)
readDoctypeIds cs
  | matchCaseI cs "public" =
      let cs1 = dropWhile isWSChar (drop 6 cs)
      in case cs1 of
          (q : rest)
            | q == '"' || q == '\'' ->
                let (pub, rest1) = readQuotedDoc rest q
                    rest2 = dropWhile isWSChar rest1
                in case rest2 of
                    (q2 : rest3)
                      | q2 == '"' || q2 == '\'' ->
                          let (sys, rest4) = readQuotedDoc rest3 q2
                          in (Just (T.pack pub), Just (T.pack sys), False, skipToGtStr rest4)
                    ('>' : rest3) -> (Just (T.pack pub), Just (T.pack ""), False, rest3)
                    _ -> (Just (T.pack pub), Just (T.pack ""), False, skipToGtStr rest2)
          _ -> (Nothing, Nothing, True, skipToGtStr cs1)
  | matchCaseI cs "system" =
      let cs1 = dropWhile isWSChar (drop 6 cs)
      in case cs1 of
          (q : rest)
            | q == '"' || q == '\'' ->
                let (sys, rest1) = readQuotedDoc rest q
                in (Just (T.pack ""), Just (T.pack sys), False, skipToGtStr rest1)
          _ -> (Nothing, Nothing, True, skipToGtStr cs1)
  | otherwise = (Nothing, Nothing, True, skipToGtStr cs)
  where
    isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'


readQuotedDoc :: String -> Char -> (String, String)
readQuotedDoc cs q = go [] cs
  where
    go acc [] = (reverse acc, [])
    go acc (c : rest)
      | c == q = (reverse acc, rest)
      | otherwise = go (c : acc) rest


readTagAttrs :: String -> (SmallArray HTMLAttribute, Bool, String)
readTagAttrs = (\(acc, sc, rest) -> (attrsFromList (reverse acc), sc, rest)) . go []
  where
    go acc [] = (acc, False, [])
    go acc ('>' : rest) = (acc, False, '\x00' : rest)
    go acc ('/' : '>' : rest) = (acc, True, '\x00' : rest)
    go acc ('/' : rest) = go acc rest
    go acc (c : rest)
      | isWSChar c = go acc rest
      | otherwise =
          let (name, rest1) = readAttrName (c : rest)
              rest2 = dropWhile isWSChar rest1
          in if null name
              then go acc rest2
              else case rest2 of
                ('=' : rest3) ->
                  let rest4 = dropWhile isWSChar rest3
                      (val, rest5) = readAttrValue rest4
                      lcName = T.toLower (T.pack name)
                  in if any (\(n, _) -> n == lcName) acc
                      then go acc rest5
                      else go ((lcName, val) : acc) rest5
                _ ->
                  let lcName = T.toLower (T.pack name)
                  in if any (\(n, _) -> n == lcName) acc
                      then go acc rest2
                      else go ((lcName, T.empty) : acc) rest2
    isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'


readAttrName :: String -> (String, String)
readAttrName = go []
  where
    go acc [] = (reverse acc, [])
    go acc (c : rest)
      | c == '=' || c == '>' || c == '/' || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' =
          (reverse acc, c : rest)
      | otherwise = go (c : acc) rest


readAttrValue :: String -> (Text, String)
readAttrValue [] = (T.empty, [])
readAttrValue ('"' : rest) = readQuotedAttr rest '"'
readAttrValue ('\'' : rest) = readQuotedAttr rest '\''
readAttrValue cs = readUnquotedAttrVal cs


readQuotedAttr :: String -> Char -> (Text, String)
readQuotedAttr cs q = go [] cs
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c : rest)
      | c == q = (T.pack (reverse acc), rest)
      | c == '&' =
          let (entity, remaining) = parseEntityRefInAttr rest
          in go (reverse entity ++ acc) remaining
      | otherwise = go (c : acc) rest


readUnquotedAttr :: String -> (Text, String)
readUnquotedAttr = go []
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c : rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '>' =
          (T.pack (reverse acc), c : rest)
      | otherwise = go (c : acc) rest


readUnquotedAttrVal :: String -> (Text, String)
readUnquotedAttrVal = go []
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c : rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '>' =
          (T.pack (reverse acc), c : rest)
      | c == '&' =
          let (entity, remaining) = parseEntityRefInAttr rest
          in go (reverse entity ++ acc) remaining
      | otherwise = go (c : acc) rest


isTagNameChar :: Char -> Bool
isTagNameChar c = isAlphaNum c || c == '-' || c == '_' || c == ':' || c == '.' || c == '<'


skipToGtStr :: String -> String
skipToGtStr [] = []
skipToGtStr ('>' : rest) = rest
skipToGtStr (_ : rest) = skipToGtStr rest


skipToGtWithAttrs :: String -> String
skipToGtWithAttrs [] = []
skipToGtWithAttrs ('>' : rest) = rest
skipToGtWithAttrs ('"' : rest) = skipToGtWithAttrs (dropWhile (/= '"') rest |> safeDrop1)
skipToGtWithAttrs ('\'' : rest) = skipToGtWithAttrs (dropWhile (/= '\'') rest |> safeDrop1)
skipToGtWithAttrs (_ : rest) = skipToGtWithAttrs rest


(|>) :: a -> (a -> a) -> a
x |> f = f x


safeDrop1 :: [a] -> [a]
safeDrop1 [] = []
safeDrop1 (_ : xs) = xs


readUntilStr :: String -> String -> (String, String)
readUntilStr _ [] = ("", [])
readUntilStr needle cs@(c : rest)
  | matchPrefix needle cs = ("", drop (length needle) cs)
  | otherwise =
      let (more, remaining) = readUntilStr needle rest
      in (c : more, remaining)


matchPrefix :: String -> String -> Bool
matchPrefix [] _ = True
matchPrefix _ [] = False
matchPrefix (n : ns) (c : cs) = n == c && matchPrefix ns cs


matchCaseI :: String -> String -> Bool
matchCaseI _ [] = True
matchCaseI [] _ = False
matchCaseI (c : cs) (p : ps) = toLower c == p && matchCaseI cs ps


flushAcc :: [Char] -> [Token]
flushAcc [] = []
flushAcc acc = [TString (T.pack (reverse acc))]


tokenizeRawText :: String -> Text -> [Token]
tokenizeRawText cs tag
  | tag == "script" = tokenizeScriptData cs
  | otherwise = goRaw [] cs
  where
    tagStr = T.unpack tag
    goRaw acc [] = flushAcc acc
    goRaw acc ('<' : '/' : rest)
      | matchCloseTag rest tagStr =
          let rest1 = drop (length tagStr) rest
              rest2 = skipToGtWithAttrs rest1
          in flushAcc acc ++ [TEndTag tag (tagIdFromText tag)] ++ tokenizeNormal rest2
    goRaw acc ('\0' : rest) = goRaw ('\xFFFD' : acc) rest
    goRaw acc (c : rest) = goRaw (c : acc) rest


tokenizeScriptData :: String -> [Token]
tokenizeScriptData cs = scriptNormal [] cs
  where
    scriptNormal acc [] = flushAcc acc
    scriptNormal acc ('<' : '/' : rest)
      | matchCloseTag rest "script" =
          let rest1 = drop 6 rest
              rest2 = skipToGtWithAttrs rest1
          in flushAcc acc ++ [TEndTag "script" TagScript] ++ tokenizeNormal rest2
    scriptNormal acc ('<' : '!' : '-' : '-' : rest) =
      scriptEscaped ('-' : '-' : '!' : '<' : acc) rest
    scriptNormal acc ('\0' : rest) = scriptNormal ('\xFFFD' : acc) rest
    scriptNormal acc (c : rest) = scriptNormal (c : acc) rest

    scriptEscaped acc [] = flushAcc acc
    scriptEscaped acc ('-' : '-' : '>' : rest) =
      scriptNormal ('>' : '-' : '-' : acc) rest
    scriptEscaped acc ('<' : '/' : rest)
      | matchCloseTag rest "script" =
          let rest1 = drop 6 rest
              rest2 = skipToGtWithAttrs rest1
          in flushAcc acc ++ [TEndTag "script" TagScript] ++ tokenizeNormal rest2
    scriptEscaped acc ('<' : rest) =
      let (tag, rest') = tryMatchScriptStart rest
      in case tag of
          Just suffix ->
            scriptDoubleEscaped (reverse suffix ++ '<' : acc) rest'
          Nothing ->
            scriptEscaped ('<' : acc) rest
    scriptEscaped acc ('\0' : rest) = scriptEscaped ('\xFFFD' : acc) rest
    scriptEscaped acc (c : rest) = scriptEscaped (c : acc) rest

    scriptDoubleEscaped acc [] = flushAcc acc
    scriptDoubleEscaped acc ('-' : '-' : '>' : rest) =
      scriptEscaped ('>' : '-' : '-' : acc) rest
    scriptDoubleEscaped acc ('<' : '/' : rest) =
      let (isScript, consumed, rest') = tryMatchScriptEnd rest
      in if isScript
          then scriptEscaped (reverse consumed ++ '/' : '<' : acc) rest'
          else scriptDoubleEscaped (reverse consumed ++ '/' : '<' : acc) rest'
    scriptDoubleEscaped acc ('\0' : rest) = scriptDoubleEscaped ('\xFFFD' : acc) rest
    scriptDoubleEscaped acc (c : rest) = scriptDoubleEscaped (c : acc) rest

    tryMatchScriptStart cs =
      case cs of
        (c1 : c2 : c3 : c4 : c5 : c6 : rest)
          | map toLower [c1, c2, c3, c4, c5, c6] == "script"
          , case rest of
              [] -> True
              (r : _) -> r `elem` (" \t\n\r\x0C/>" :: [Char]) ->
              (Just [c1, c2, c3, c4, c5, c6], rest)
        _ -> (Nothing, cs)

    tryMatchScriptEnd cs =
      case cs of
        (c1 : c2 : c3 : c4 : c5 : c6 : rest)
          | map toLower [c1, c2, c3, c4, c5, c6] == "script"
          , case rest of
              [] -> True
              (r : _) -> r `elem` (" \t\n\r\x0C/>" :: [Char]) ->
              (True, [c1, c2, c3, c4, c5, c6], rest)
        _ -> (False, [], cs)


matchCloseTagEOF :: String -> String -> Bool
matchCloseTagEOF cs tag =
  let (name, rest) = span (\c -> isAlpha c || isDigit c || c == '-') cs
  in map toLower name == map toLower tag && null rest


normalizeCR :: String -> String
normalizeCR [] = []
normalizeCR ('\r' : '\n' : rest) = '\n' : normalizeCR rest
normalizeCR ('\r' : rest) = '\n' : normalizeCR rest
normalizeCR (c : rest) = c : normalizeCR rest


tokenizePlaintext :: String -> [Token]
tokenizePlaintext [] = []
tokenizePlaintext cs =
  let (text, rest) = span (/= '\0') cs
  in (if null text then [] else [TString (T.pack text)])
      ++ case rest of
        ('\0' : rs) -> TChar '\xFFFD' : tokenizePlaintext rs
        _ -> []


tokenizeRCData :: String -> Text -> [Token]
tokenizeRCData cs tag = go [] cs
  where
    tagStr = T.unpack tag
    go acc [] = flushAcc acc
    go acc ('<' : '/' : rest)
      | matchCloseTag rest tagStr =
          let rest1 = drop (length tagStr) rest
              rest2 = skipToGtWithAttrs rest1
          in flushAcc acc ++ [TEndTag tag (tagIdFromText tag)] ++ tokenizeNormal rest2
    go acc ('&' : rest) =
      let (entity, remaining) = parseEntityRef rest
      in go (reverse entity ++ acc) remaining
    go acc ('\0' : rest) = go ('\xFFFD' : acc) rest
    go acc ('\r' : '\n' : rest) = go ('\n' : acc) rest
    go acc ('\r' : rest) = go ('\n' : acc) rest
    go acc (c : rest) = go (c : acc) rest


matchCloseTag :: String -> String -> Bool
matchCloseTag cs tag =
  let (name, rest) = span (\c -> isAlpha c || isDigit c || c == '-') cs
  in map toLower name == map toLower tag
      && case rest of
        [] -> False
        (c : _) -> c == '>' || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '/'


-- | Remaining input after the first markup declaration when the input is the
-- substring that follows @\<!\@ (same convention as 'tokenizeMarkupDeclCtx').
{-# NOINLINE markupDeclRemaining #-}
markupDeclRemaining :: Int -> Bool -> String -> String
markupDeclRemaining _ _ = \case
  ('-':'-':rest) -> snd (readComment rest)
  rest
    | matchCaseI rest "doctype" ->
        let rest1 = drop 7 rest
            cs1 = dropWhile isSpDoctype rest1
            (_name, cs2) = readDoctypeName cs1
            cs3 = dropWhile isSpDoctype cs2
            (_pub, _sys, _fq, cs4) = readDoctypeIds cs3
         in cs4
    | matchCaseI rest "[cdata[" ->
        let rest1 = drop 7 rest
         in snd (readUntilStr "]]>" rest1)
    | otherwise ->
        snd (readBogusComment rest)
  where
    isSpDoctype c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'


-- | Advance a UTF-8 'ByteString' by @n@ Unicode scalar values starting at @off@.
{-# INLINE utf8CharWidth #-}
utf8CharWidth :: Word8 -> Int
utf8CharWidth !w
  | w .&. 0x80 == 0 = 1
  | w .&. 0xE0 == 0xC0 = 2
  | w .&. 0xF0 == 0xE0 = 3
  | w .&. 0xF8 == 0xF0 = 4
  | otherwise = 1


{-# INLINE utf8AdvanceNChars #-}
utf8AdvanceNChars :: ByteString -> Int -> Int -> Int
utf8AdvanceNChars !bs !o !n
  | n <= 0 = o
  | o >= BS.length bs = o
  | otherwise =
      let !w = utf8CharWidth (BSU.unsafeIndex bs o)
       in utf8AdvanceNChars bs (o + w) (n - 1)


-- | String suffix after a raw-text element's closing tag (after @>@), or the
-- original string if no closing tag is found.
{-# NOINLINE rawTextRemainingString #-}
rawTextRemainingString :: String -> Text -> String
rawTextRemainingString cs tag
  | tag == "plaintext" = []
  | tag == "script" =
      case scriptDataRemaining cs of
        Nothing -> []
        Just r -> r
  | otherwise = goRaw (T.unpack tag) cs
  where
    goRaw !_ [] = []
    goRaw !tagStr ('<' : '/' : rest)
      | matchCloseTag rest tagStr =
          skipToGtWithAttrs (drop (length tagStr) rest)
    goRaw !tagStr (_ : rest) = goRaw tagStr rest


{-# NOINLINE scriptDataRemaining #-}
scriptDataRemaining :: String -> Maybe String
scriptDataRemaining cs = scriptNormal cs
  where
    scriptNormal [] = Nothing
    scriptNormal ('<' : '/' : rest)
      | matchCloseTag rest "script" =
          Just (skipToGtWithAttrs (drop 6 rest))
    scriptNormal ('<' : '!' : '-' : '-' : rest) = scriptEscaped rest
    scriptNormal ('\0' : rest) = scriptNormal rest
    scriptNormal (_ : rest) = scriptNormal rest

    scriptEscaped [] = Nothing
    scriptEscaped ('-' : '-' : '>' : rest) = scriptNormal rest
    scriptEscaped ('<' : '/' : rest)
      | matchCloseTag rest "script" =
          Just (skipToGtWithAttrs (drop 6 rest))
    scriptEscaped ('<' : rest) =
      case tryMatchScriptStart rest of
        Just rest' -> scriptDoubleEscaped rest'
        Nothing -> scriptEscaped rest
    scriptEscaped ('\0' : rest) = scriptEscaped rest
    scriptEscaped (_ : rest) = scriptEscaped rest

    scriptDoubleEscaped [] = Nothing
    scriptDoubleEscaped ('-' : '-' : '>' : rest) = scriptEscaped rest
    scriptDoubleEscaped ('<' : '/' : rest) =
      let (isScript, _consumed, rest') = tryMatchScriptEnd rest
       in if isScript then scriptEscaped rest' else scriptDoubleEscaped rest'
    scriptDoubleEscaped ('\0' : rest) = scriptDoubleEscaped rest
    scriptDoubleEscaped (_ : rest) = scriptDoubleEscaped rest

    tryMatchScriptStart ds =
      case ds of
        (c1 : c2 : c3 : c4 : c5 : c6 : rest)
          | map toLower [c1, c2, c3, c4, c5, c6] == "script"
          , case rest of
              [] -> True
              (r : _) -> r `elem` (" \t\n\r\x0C/>" :: [Char]) ->
              Just rest
        _ -> Nothing

    tryMatchScriptEnd ds =
      case ds of
        (c1 : c2 : c3 : c4 : c5 : c6 : rest)
          | map toLower [c1, c2, c3, c4, c5, c6] == "script"
          , case rest of
              [] -> True
              (r : _) -> r `elem` (" \t\n\r\x0C/>" :: [Char]) ->
              (True, [c1, c2, c3, c4, c5, c6], rest)
        _ -> (False, [], ds)


------------------------------------------------------------------------
-- Entity resolution
------------------------------------------------------------------------

resolveEntitiesT :: Text -> Text
resolveEntitiesT t
  | T.any (== '&') t = T.pack (resolveChars (T.unpack t))
  | otherwise = t


parseEntityRef :: String -> (String, String)
parseEntityRef ('#' : 'x' : rest) = parseHexEntity "#x" rest
parseEntityRef ('#' : 'X' : rest) = parseHexEntity "#X" rest
parseEntityRef ('#' : rest) = parseDecEntity rest
parseEntityRef rest = case matchNamedEntity rest of
  Just (name, replacement, remaining) ->
    if hasSemicolon name rest
      then (replacement, remaining)
      else (replacement, remaining)
  Nothing -> ("&", rest)
  where
    hasSemicolon _ _ = True


parseEntityRefInAttr :: String -> (String, String)
parseEntityRefInAttr ('#' : 'x' : rest) = parseHexEntity "#x" rest
parseEntityRefInAttr ('#' : 'X' : rest) = parseHexEntity "#X" rest
parseEntityRefInAttr ('#' : rest) = parseDecEntity rest
parseEntityRefInAttr rest = case matchNamedEntityAttr rest of
  Just (_, replacement, remaining) -> (replacement, remaining)
  Nothing -> ("&", rest)


parseHexEntity :: String -> String -> (String, String)
parseHexEntity prefix rest =
  let (hex, after) = span isHexDigit rest
  in if null hex
      then ("&" ++ prefix, rest)
      else
        let val = foldl' (\a d -> a * 16 + digitToInt d) 0 hex
            after1 = case after of (';' : r) -> r; _ -> after
        in ([safeChar val], after1)


parseDecEntity :: String -> (String, String)
parseDecEntity rest =
  let (dec, after) = span isDigit rest
  in if null dec
      then ("&#", rest)
      else
        let val = foldl' (\a d -> a * 10 + digitToInt d) 0 dec
            after1 = case after of (';' : r) -> r; _ -> after
        in ([safeChar val], after1)


safeChar :: Int -> Char
safeChar 0 = '\xFFFD'
safeChar n
  | n > 0x10FFFF = '\xFFFD'
  | n >= 0xD800 && n <= 0xDFFF = '\xFFFD'
  | n >= 0x80 && n <= 0x9F = case lookup n windows1252Table of
      Just c -> c
      Nothing -> chr n
  | otherwise = chr n


windows1252Table :: [(Int, Char)]
windows1252Table =
  [ (0x80, '\x20AC')
  , (0x82, '\x201A')
  , (0x83, '\x0192')
  , (0x84, '\x201E')
  , (0x85, '\x2026')
  , (0x86, '\x2020')
  , (0x87, '\x2021')
  , (0x88, '\x02C6')
  , (0x89, '\x2030')
  , (0x8A, '\x0160')
  , (0x8B, '\x2039')
  , (0x8C, '\x0152')
  , (0x8E, '\x017D')
  , (0x91, '\x2018')
  , (0x92, '\x2019')
  , (0x93, '\x201C')
  , (0x94, '\x201D')
  , (0x95, '\x2022')
  , (0x96, '\x2013')
  , (0x97, '\x2014')
  , (0x98, '\x02DC')
  , (0x99, '\x2122')
  , (0x9A, '\x0161')
  , (0x9B, '\x203A')
  , (0x9C, '\x0153')
  , (0x9E, '\x017E')
  , (0x9F, '\x0178')
  ]


matchNamedEntity :: String -> Maybe (String, String, String)
matchNamedEntity cs =
  let (allAlpha, rest) = span isAlphaNum cs
  in if null allAlpha
      then Nothing
      else case rest of
        (';' : after) -> case lookup allAlpha namedEntities of
          Just rep -> Just (allAlpha, rep, after)
          Nothing -> tryPrefixesWithSemi allAlpha (';' : after)
        _ -> tryPrefixesNoSemi allAlpha rest


tryPrefixesWithSemi :: String -> String -> Maybe (String, String, String)
tryPrefixesWithSemi name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
          Just rep -> Just (prefix, rep, suffix)
          Nothing -> go (n - 1)


tryPrefixesNoSemi :: String -> String -> Maybe (String, String, String)
tryPrefixesNoSemi name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
          Just rep ->
            if prefix `elem` legacyEntities
              then Just (prefix, rep, suffix)
              else go (n - 1)
          Nothing -> go (n - 1)


legacyEntities :: [String]
legacyEntities =
  [ "amp"
  , "lt"
  , "gt"
  , "quot"
  , "apos"
  , "AMP"
  , "LT"
  , "GT"
  , "QUOT"
  , "Aacute"
  , "aacute"
  , "Acirc"
  , "acirc"
  , "acute"
  , "AElig"
  , "aelig"
  , "Agrave"
  , "agrave"
  , "Aring"
  , "aring"
  , "Atilde"
  , "atilde"
  , "Auml"
  , "auml"
  , "brvbar"
  , "Ccedil"
  , "ccedil"
  , "cedil"
  , "cent"
  , "copy"
  , "COPY"
  , "curren"
  , "deg"
  , "divide"
  , "Eacute"
  , "eacute"
  , "Ecirc"
  , "ecirc"
  , "Egrave"
  , "egrave"
  , "ETH"
  , "eth"
  , "Euml"
  , "euml"
  , "frac12"
  , "frac14"
  , "frac34"
  , "Iacute"
  , "iacute"
  , "Icirc"
  , "icirc"
  , "iexcl"
  , "Igrave"
  , "igrave"
  , "iquest"
  , "Iuml"
  , "iuml"
  , "laquo"
  , "macr"
  , "micro"
  , "middot"
  , "nbsp"
  , "not"
  , "Ntilde"
  , "ntilde"
  , "Oacute"
  , "oacute"
  , "Ocirc"
  , "ocirc"
  , "Ograve"
  , "ograve"
  , "ordf"
  , "ordm"
  , "Oslash"
  , "oslash"
  , "Otilde"
  , "otilde"
  , "Ouml"
  , "ouml"
  , "para"
  , "plusmn"
  , "pound"
  , "raquo"
  , "REG"
  , "reg"
  , "sect"
  , "shy"
  , "sup1"
  , "sup2"
  , "sup3"
  , "szlig"
  , "THORN"
  , "thorn"
  , "times"
  , "Uacute"
  , "uacute"
  , "Ucirc"
  , "ucirc"
  , "Ugrave"
  , "ugrave"
  , "uml"
  , "Uuml"
  , "uuml"
  , "Yacute"
  , "yacute"
  , "yen"
  , "yuml"
  ]


matchNamedEntityAttr :: String -> Maybe (String, String, String)
matchNamedEntityAttr cs =
  let (allAlpha, rest) = span isAlphaNum cs
  in if null allAlpha
      then Nothing
      else case rest of
        (';' : after) -> case lookup allAlpha namedEntities of
          Just rep -> Just (allAlpha, rep, after)
          Nothing -> Nothing
        _ -> tryAttrPrefixesLegacy allAlpha rest


tryAttrPrefixesLegacy :: String -> String -> Maybe (String, String, String)
tryAttrPrefixesLegacy name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
          Just rep ->
            if prefix `elem` legacyEntities
              then
                let nextChar = case suffix of (c : _) -> Just c; [] -> Nothing
                    nextIsAlnumOrEq = nextChar == Just '=' || maybe False isAlphaNum nextChar
                in if nextIsAlnumOrEq
                    then go (n - 1)
                    else Just (prefix, rep, suffix)
              else go (n - 1)
          Nothing -> go (n - 1)


resolveChars :: String -> String
resolveChars [] = []
resolveChars ('&' : rest) =
  let (resolved, remaining) = parseEntityRef rest
  in resolved ++ resolveChars remaining
resolveChars (c : rest) = c : resolveChars rest


namedEntities :: [(String, String)]
namedEntities =
  [ ("amp", "&")
  , ("lt", "<")
  , ("gt", ">")
  , ("quot", "\"")
  , ("apos", "'")
  , ("nbsp", "\x00A0")
  , ("iexcl", "\x00A1")
  , ("cent", "\x00A2")
  , ("pound", "\x00A3")
  , ("curren", "\x00A4")
  , ("yen", "\x00A5")
  , ("brvbar", "\x00A6")
  , ("sect", "\x00A7")
  , ("uml", "\x00A8")
  , ("copy", "\x00A9")
  , ("ordf", "\x00AA")
  , ("laquo", "\x00AB")
  , ("not", "\x00AC")
  , ("shy", "\x00AD")
  , ("reg", "\x00AE")
  , ("macr", "\x00AF")
  , ("deg", "\x00B0")
  , ("plusmn", "\x00B1")
  , ("sup2", "\x00B2")
  , ("sup3", "\x00B3")
  , ("acute", "\x00B4")
  , ("micro", "\x00B5")
  , ("para", "\x00B6")
  , ("middot", "\x00B7")
  , ("cedil", "\x00B8")
  , ("sup1", "\x00B9")
  , ("ordm", "\x00BA")
  , ("raquo", "\x00BB")
  , ("frac14", "\x00BC")
  , ("frac12", "\x00BD")
  , ("frac34", "\x00BE")
  , ("iquest", "\x00BF")
  , ("Agrave", "\x00C0")
  , ("Aacute", "\x00C1")
  , ("Acirc", "\x00C2")
  , ("Atilde", "\x00C3")
  , ("Auml", "\x00C4")
  , ("Aring", "\x00C5")
  , ("AElig", "\x00C6")
  , ("Ccedil", "\x00C7")
  , ("Egrave", "\x00C8")
  , ("Eacute", "\x00C9")
  , ("Ecirc", "\x00CA")
  , ("Euml", "\x00CB")
  , ("Igrave", "\x00CC")
  , ("Iacute", "\x00CD")
  , ("Icirc", "\x00CE")
  , ("Iuml", "\x00CF")
  , ("ETH", "\x00D0")
  , ("Ntilde", "\x00D1")
  , ("Ograve", "\x00D2")
  , ("Oacute", "\x00D3")
  , ("Ocirc", "\x00D4")
  , ("Otilde", "\x00D5")
  , ("Ouml", "\x00D6")
  , ("times", "\x00D7")
  , ("Oslash", "\x00D8")
  , ("Ugrave", "\x00D9")
  , ("Uacute", "\x00DA")
  , ("Ucirc", "\x00DB")
  , ("Uuml", "\x00DC")
  , ("Yacute", "\x00DD")
  , ("THORN", "\x00DE")
  , ("szlig", "\x00DF")
  , ("agrave", "\x00E0")
  , ("aacute", "\x00E1")
  , ("acirc", "\x00E2")
  , ("atilde", "\x00E3")
  , ("auml", "\x00E4")
  , ("aring", "\x00E5")
  , ("aelig", "\x00E6")
  , ("ccedil", "\x00E7")
  , ("egrave", "\x00E8")
  , ("eacute", "\x00E9")
  , ("ecirc", "\x00EA")
  , ("euml", "\x00EB")
  , ("igrave", "\x00EC")
  , ("iacute", "\x00ED")
  , ("icirc", "\x00EE")
  , ("iuml", "\x00EF")
  , ("eth", "\x00F0")
  , ("ntilde", "\x00F1")
  , ("ograve", "\x00F2")
  , ("oacute", "\x00F3")
  , ("ocirc", "\x00F4")
  , ("otilde", "\x00F5")
  , ("ouml", "\x00F6")
  , ("divide", "\x00F7")
  , ("oslash", "\x00F8")
  , ("ugrave", "\x00F9")
  , ("uacute", "\x00FA")
  , ("ucirc", "\x00FB")
  , ("uuml", "\x00FC")
  , ("yacute", "\x00FD")
  , ("thorn", "\x00FE")
  , ("yuml", "\x00FF")
  , ("ndash", "\x2013")
  , ("mdash", "\x2014")
  , ("lsquo", "\x2018")
  , ("rsquo", "\x2019")
  , ("sbquo", "\x201A")
  , ("ldquo", "\x201C")
  , ("rdquo", "\x201D")
  , ("bdquo", "\x201E")
  , ("dagger", "\x2020")
  , ("Dagger", "\x2021")
  , ("bull", "\x2022")
  , ("hellip", "\x2026")
  , ("prime", "\x2032")
  , ("Prime", "\x2033")
  , ("lsaquo", "\x2039")
  , ("rsaquo", "\x203A")
  , ("oline", "\x203E")
  , ("frasl", "\x2044")
  , ("euro", "\x20AC")
  , ("image", "\x2111")
  , ("weierp", "\x2118")
  , ("real", "\x211C")
  , ("trade", "\x2122")
  , ("alefsym", "\x2135")
  , ("larr", "\x2190")
  , ("uarr", "\x2191")
  , ("rarr", "\x2192")
  , ("darr", "\x2193")
  , ("harr", "\x2194")
  , ("crarr", "\x21B5")
  , ("lArr", "\x21D0")
  , ("uArr", "\x21D1")
  , ("rArr", "\x21D2")
  , ("dArr", "\x21D3")
  , ("hArr", "\x21D4")
  , ("nabla", "\x2207")
  , ("isin", "\x2208")
  , ("notin", "\x2209")
  , ("ni", "\x220B")
  , ("prod", "\x220F")
  , ("sum", "\x2211")
  , ("minus", "\x2212")
  , ("lowast", "\x2217")
  , ("radic", "\x221A")
  , ("prop", "\x221D")
  , ("infin", "\x221E")
  , ("ang", "\x2220")
  , ("and", "\x2227")
  , ("or", "\x2228")
  , ("cap", "\x2229")
  , ("cup", "\x222A")
  , ("int", "\x222B")
  , ("there4", "\x2234")
  , ("sim", "\x223C")
  , ("cong", "\x2245")
  , ("asymp", "\x2248")
  , ("ne", "\x2260")
  , ("equiv", "\x2261")
  , ("le", "\x2264")
  , ("ge", "\x2265")
  , ("sub", "\x2282")
  , ("sup", "\x2283")
  , ("nsub", "\x2284")
  , ("sube", "\x2286")
  , ("supe", "\x2287")
  , ("oplus", "\x2295")
  , ("otimes", "\x2297")
  , ("perp", "\x22A5")
  , ("sdot", "\x22C5")
  , ("lceil", "\x2308")
  , ("rceil", "\x2309")
  , ("lfloor", "\x230A")
  , ("rfloor", "\x230B")
  , ("loz", "\x25CA")
  , ("spades", "\x2660")
  , ("clubs", "\x2663")
  , ("hearts", "\x2665")
  , ("diams", "\x2666")
  , ("OElig", "\x0152")
  , ("oelig", "\x0153")
  , ("Scaron", "\x0160")
  , ("scaron", "\x0161")
  , ("Yuml", "\x0178")
  , ("fnof", "\x0192")
  , ("circ", "\x02C6")
  , ("tilde", "\x02DC")
  , ("ensp", "\x2002")
  , ("emsp", "\x2003")
  , ("thinsp", "\x2009")
  , ("zwnj", "\x200C")
  , ("zwj", "\x200D")
  , ("lrm", "\x200E")
  , ("rlm", "\x200F")
  , ("permil", "\x2030")
  , ("lang", "\x27E8")
  , ("rang", "\x27E9")
  , ("ImaginaryI", "\x2148")
  , ("Kopf", "\x1D542")
  , ("notinva", "\x2209")
  , ("NotEqualTilde", "\x2242\x0338")
  , ("ThickSpace", "\x205F\x200A")
  , ("NotSubset", "\x2282\x20D2")
  , ("Gopf", "\x1D53E")
  , ("AMP", "&")
  , ("COPY", "\xA9")
  , ("GT", ">")
  , ("LT", "<")
  , ("QUOT", "\"")
  , ("REG", "\xAE")
  , ("Tab", "\x0009")
  , ("NewLine", "\x000A")
  , ("Alpha", "\x0391")
  , ("Beta", "\x0392")
  , ("Gamma", "\x0393")
  , ("Delta", "\x0394")
  , ("Epsilon", "\x0395")
  , ("Zeta", "\x0396")
  , ("Eta", "\x0397")
  , ("Theta", "\x0398")
  , ("Iota", "\x0399")
  , ("Kappa", "\x039A")
  , ("Lambda", "\x039B")
  , ("Mu", "\x039C")
  , ("Nu", "\x039D")
  , ("Xi", "\x039E")
  , ("Omicron", "\x039F")
  , ("Pi", "\x03A0")
  , ("Rho", "\x03A1")
  , ("Sigma", "\x03A3")
  , ("Tau", "\x03A4")
  , ("Upsilon", "\x03A5")
  , ("Phi", "\x03A6")
  , ("Chi", "\x03A7")
  , ("Psi", "\x03A8")
  , ("Omega", "\x03A9")
  , ("alpha", "\x03B1")
  , ("beta", "\x03B2")
  , ("gamma", "\x03B3")
  , ("delta", "\x03B4")
  , ("epsilon", "\x03B5")
  , ("zeta", "\x03B6")
  , ("eta", "\x03B7")
  , ("theta", "\x03B8")
  , ("iota", "\x03B9")
  , ("kappa", "\x03BA")
  , ("lambda", "\x03BB")
  , ("mu", "\x03BC")
  , ("nu", "\x03BD")
  , ("xi", "\x03BE")
  , ("omicron", "\x03BF")
  , ("pi", "\x03C0")
  , ("rho", "\x03C1")
  , ("sigmaf", "\x03C2")
  , ("sigma", "\x03C3")
  , ("tau", "\x03C4")
  , ("upsilon", "\x03C5")
  , ("phi", "\x03C6")
  , ("chi", "\x03C7")
  , ("psi", "\x03C8")
  , ("omega", "\x03C9")
  , ("thetasym", "\x03D1")
  , ("upsih", "\x03D2")
  , ("piv", "\x03D6")
  ]
