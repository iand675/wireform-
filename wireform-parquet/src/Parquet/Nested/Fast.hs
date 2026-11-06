{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Two-pass shredding into exact-size level and PLAIN buffers. The first
pass validates all rows and sizes before any payload buffer is allocated.
FILE and Variant keep their specialized validation/normalization path.
-}
module Parquet.Nested.Fast (FastLeaf (..), shredFast) where

import Control.Monad.Primitive (PrimMonad (..), touch)
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (setBit, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short.Internal qualified as SBS
import Data.Int (Int32)
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Text.Foreign qualified as TF
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as MVP
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int#, State#, isTrue#, (+#), (==#))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.ST (ST (..))
import Parquet.Nested.Types
import Parquet.Types (LtTimeUnit (..))


-- Only the outer boundary boxes a validation result. Successful events carry
-- an unboxed sum through the primitive state thread.
newtype Check s a = Check (State# s -> (# State# s, (# a | String #) #))


instance Functor (Check s) where
  fmap f (Check action) = Check $ \state -> case action state of
    (# next, (# value | #) #) -> (# next, (# f value | #) #)
    (# next, (# | message #) #) -> (# next, (# | message #) #)
  {-# INLINE fmap #-}


instance Applicative (Check s) where
  pure value = Check $ \state -> (# state, (# value | #) #)
  function <*> argument = function >>= \f -> fmap f argument
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


instance Monad (Check s) where
  Check action >>= continuation = Check $ \state -> case action state of
    (# next, (# value | #) #) -> case continuation value of Check after -> after next
    (# next, (# | message #) #) -> (# next, (# | message #) #)
  {-# INLINE (>>=) #-}


instance PrimMonad (Check s) where
  type PrimState (Check s) = s
  primitive action = Check $ \state -> case action state of
    (# next, value #) -> (# next, (# value | #) #)
  {-# INLINE primitive #-}


runCheck :: Check s a -> ST s (Either String a)
runCheck (Check action) = ST $ \state -> case action state of
  (# next, (# value | #) #) -> (# next, Right value #)
  (# next, (# | message #) #) -> (# next, Left message #)
{-# INLINE runCheck #-}


failCheck :: String -> Check s a
failCheck message = Check $ \state -> (# state, (# | message #) #)
{-# INLINE failCheck #-}


data FastLeaf = FastLeaf
  { fastDefinitions :: !(VP.Vector Int32)
  , fastRepetitions :: !(VP.Vector Int32)
  , fastValueCount :: {-# UNPACK #-} !Int
  , fastPayloadSize :: {-# UNPACK #-} !Int
  , fastPayload :: ~ByteString
  }


data ValueKind = KInt32 | KInt64 | KFloat16 | KFloat | KDouble | KBool | KText | KBinary | KFixed {-# UNPACK #-} !Int


data Program
  = Scalar {-# UNPACK #-} !Int !LeafType !ValueKind
  | Optional {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Program
  | List {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Program
  | Mapping {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Program !Program
  | Struct !(V.Vector Program)


data Compiled = Compiled !Program {-# UNPACK #-} !Int


data CompiledFields = CompiledFields ![Program] {-# UNPACK #-} !Int


data LeafBuffer s = LeafBuffer
  { bufferDefinitions :: !(MVP.MVector s Int32)
  , bufferRepetitions :: !(MVP.MVector s Int32)
  , bufferValueCount :: {-# UNPACK #-} !Int
  , bufferPayloadSize :: {-# UNPACK #-} !Int
  , bufferPayload :: !(Maybe (MutableByteArray s))
  }


compile :: Int -> Int -> NestedSchema -> Maybe Compiled
compile cursor depth schema = case schema of
  NSPrimitive ty -> Just (Compiled (Scalar cursor ty (kind ty)) (cursor + 1))
  NSRequired child -> compile cursor depth child
  NSOptional child -> do
    Compiled node end <- compile cursor depth child
    pure (Compiled (Optional cursor end node) end)
  NSList child -> do
    Compiled node end <- compile cursor (depth + 1) child
    pure (Compiled (List cursor end (depth + 1) node) end)
  NSMap key value -> do
    Compiled kn middle <- compile cursor (depth + 1) key
    Compiled vn end <- compile middle (depth + 1) value
    pure (Compiled (Mapping cursor end (depth + 1) kn vn) end)
  NSStruct fields -> do
    CompiledFields nodes end <-
      V.foldM
        ( \(CompiledFields nodes next) (_, child) -> do
            Compiled node after <- compile next depth child
            pure (CompiledFields (node : nodes) after)
        )
        (CompiledFields [] cursor)
        fields
    pure (Compiled (Struct (V.fromList (reverse nodes))) end)
  _ -> Nothing
  where
    kind ty = case ty of
      LtInt32 -> KInt32
      LtInt64 -> KInt64
      LtFloat16 -> KFloat16
      LtFloat -> KFloat
      LtDouble -> KDouble
      LtBool -> KBool
      LtString -> KText
      LtJson -> KText
      LtBinary -> KBinary
      LtInteger 64 _ -> KInt64
      LtInteger _ _ -> KInt32
      LtDate -> KInt32
      LtTime _ LtMillis -> KInt32
      LtTime _ _ -> KInt64
      LtTimestamp {} -> KInt64
      LtTimestamp96 {} -> KFixed 12
      LtInt96 -> KFixed 12
      LtDecimal precision _ | precision <= 9 -> KInt32
      LtDecimal precision _ | precision <= 18 -> KInt64
      LtDecimal precision _ -> KFixed (if precision <= 38 then 16 else 32)
      LtFixedBinary width -> KFixed (fromIntegral width)
      LtUuid -> KFixed 16


shredFast
  :: NestedSchema
  -> V.Vector NestedRow
  -> Maybe (Either String (V.Vector FastLeaf))
shredFast schema rows = do
  Compiled program leaves <- compile 0 0 schema
  pure $
    if leaves > maxBound `quot` 3
      then Left "Parquet.Nested: leaf-state size exceeds addressable limit"
      else case simpleList program of
        Just simple -> shredSimpleList simple program rows
        Nothing -> runST $ do
          counters <- newPrimArray (leaves * 3)
          setPrimArray counters 0 (leaves * 3) 0
          problem <- runCheck (countRows counters program rows)
          case problem of
            Left message -> pure (Left message)
            Right () | Scalar {} <- program -> do
              -- A required primitive has one event per row and no levels on
              -- disk. Share one zero vector for the public shred result and
              -- write values directly when its independently lazy payload is used.
              events <- readPrimArray counters 0
              present <- readPrimArray counters 1
              bytes <- readPrimArray counters 2
              let levels = VP.replicate events 0
                  payload = materializePayload program rows 0 bytes
              pure (Right (V.singleton (FastLeaf levels levels present bytes payload)))
            Right () -> do
              cacheValues <- smallPayloads counters leaves 0 0
              buffers <- V.generateM leaves $ \index -> do
                events <- readPrimArray counters (index * 3)
                present <- readPrimArray counters (index * 3 + 1)
                bytes <- readPrimArray counters (index * 3 + 2)
                definitions <- MVP.unsafeNew events
                repetitions <- MVP.unsafeNew events
                payload <- if cacheValues then Just <$> newPinnedByteArray bytes else pure Nothing
                pure (LeafBuffer definitions repetitions present bytes payload)
              setPrimArray counters 0 (leaves * 3) 0
              fillRows counters buffers program rows
              Right <$> V.imapM (freeze program) buffers
  where
    freeze :: Program -> Int -> LeafBuffer s -> ST s FastLeaf
    freeze program index buffer = do
      definitions <- VP.unsafeFreeze (bufferDefinitions buffer)
      repetitions <- VP.unsafeFreeze (bufferRepetitions buffer)
      let size = bufferPayloadSize buffer
      bytes <- case bufferPayload buffer of
        Nothing -> pure (materializePayload program rows index size)
        Just payload -> do
          frozen <- unsafeFreezeByteArray payload
          pure (ownedPayload frozen)
      pure (FastLeaf definitions repetitions (bufferValueCount buffer) size bytes)


-- The standard three-level list has a single physical leaf. Carry its
-- counters through both row and element loops instead of loading/storing
-- the general leaf-state array at every list boundary.
data SimpleList = SimpleList !Bool !Bool !LeafType !ValueKind


data SimpleCounts = SimpleCounts
  { simpleEvents :: {-# UNPACK #-} !Int
  , simplePresent :: {-# UNPACK #-} !Int
  , simpleBytes :: {-# UNPACK #-} !Int
  }


simpleList :: Program -> Maybe SimpleList
simpleList program = case program of
  Optional _ _ child -> list True child
  _ -> list False program
  where
    list rootOptional (List _ _ 1 child) = case child of
      Scalar _ ty kind -> Just (SimpleList rootOptional False ty kind)
      Optional _ _ (Scalar _ ty kind) -> Just (SimpleList rootOptional True ty kind)
      _ -> Nothing
    list _ _ = Nothing


shredSimpleList :: SimpleList -> Program -> V.Vector NestedRow -> Either String (V.Vector FastLeaf)
shredSimpleList schema program rows = do
  counts <- countSimpleList schema rows
  runST $ do
    definitions <- MVP.unsafeNew (simpleEvents counts)
    repetitions <- MVP.unsafeNew (simpleEvents counts)
    storage <- if simpleBytes counts <= 1048576 then Just <$> newPinnedByteArray (simpleBytes counts) else pure Nothing
    filled <- fillSimpleList schema counts rows definitions repetitions storage
    if not filled
      then pure (Left "Parquet.Nested.Fast: list fill disagrees with preflight counts")
      else do
        defs <- VP.unsafeFreeze definitions
        reps <- VP.unsafeFreeze repetitions
        bytes <- case storage of
          Nothing -> pure (materializePayload program rows 0 (simpleBytes counts))
          Just payload -> ownedPayload <$> unsafeFreezeByteArray payload
        pure (Right (V.singleton (FastLeaf defs reps (simplePresent counts) (simpleBytes counts) bytes)))


countSimpleList :: SimpleList -> V.Vector NestedRow -> Either String SimpleCounts
countSimpleList schema@(SimpleList rootOptional elementOptional ty kind) rows = case fixedWidth kind of
  Just width -> countFixedList schema width rows
  Nothing -> row 0# 0# 0# 0#
  where
    !(I# rowCount#) = V.length rows
    !(I# limit#) = limit
    countError = Left "Parquet.Nested: value count exceeds signed 32-bit page limit"
    shapeError = Left "Parquet.Nested: row shape does not match schema"
    row i# events# present# bytes#
      | isTrue# (i# ==# rowCount#) = Right (SimpleCounts (I# events#) (I# present#) (I# bytes#))
      | otherwise = case V.unsafeIndex rows (I# i#) of
          NRNull
            | rootOptional ->
                if isTrue# (events# ==# limit#) then countError else row (i# +# 1#) (events# +# 1#) present# bytes#
          NRList values
            | V.null values ->
                if isTrue# (events# ==# limit#) then countError else row (i# +# 1#) (events# +# 1#) present# bytes#
            | V.length values > limit -> Left "Parquet.Nested: list length exceeds signed 32-bit page limit"
            | otherwise -> element i# values 0# events# present# bytes#
          _ -> shapeError
    element i# values j# events# present# bytes#
      | I# j# == V.length values = row (i# +# 1#) events# present# bytes#
      | otherwise = case V.unsafeIndex values (I# j#) of
          NRNull
            | not elementOptional -> Left "Parquet.Nested: NRNull at required NSPrimitive"
            | isTrue# (events# ==# limit#) -> countError
            | otherwise -> element i# values (j# +# 1#) (events# +# 1#) present# bytes#
          NRLeaf value
            | not (leafValueMatches ty value) -> Left "Parquet.Nested: leaf value violates its physical or logical type"
            | isTrue# (events# ==# limit#) -> countError
            | otherwise ->
                let !size = plainValueSize kind value (I# present#)
                in if size < 0 || size > limit || I# bytes# > limit - size
                     then Left "Parquet.Nested: value bytes exceed signed 32-bit page limit"
                     else case size of
                       I# size# -> element i# values (j# +# 1#) (events# +# 1#) (present# +# 1#) (bytes# +# size#)
          _ -> shapeError


-- Fixed-width leaves only need a present count during validation. Account
-- for a list's events once at its boundary and check the final multiplication
-- before allocating. Zero denotes the packed Boolean representation.
fixedWidth :: ValueKind -> Maybe Int
fixedWidth kind = case kind of
  KBool -> Just 0
  KInt32 -> Just 4
  KInt64 -> Just 8
  KFloat16 -> Just 2
  KFloat -> Just 4
  KDouble -> Just 8
  KFixed width | width > 0 -> Just width
  _ -> Nothing


countFixedList :: SimpleList -> Int -> V.Vector NestedRow -> Either String SimpleCounts
countFixedList (SimpleList rootOptional elementOptional ty _) width rows = row 0# 0# 0#
  where
    !(I# rowCount#) = V.length rows
    !(I# limit#) = limit
    countError = Left "Parquet.Nested: value count exceeds signed 32-bit page limit"
    shapeError = Left "Parquet.Nested: row shape does not match schema"
    row i# events# present#
      | isTrue# (i# ==# rowCount#) =
          let present = I# present#
          in if width /= 0 && present > limit `quot` width
               then Left "Parquet.Nested: value bytes exceed signed 32-bit page limit"
               else
                 let bytes = if width == 0 then present `quot` 8 + if present `rem` 8 == 0 then 0 else 1 else present * width
                 in Right (SimpleCounts (I# events#) present bytes)
      | otherwise = case V.unsafeIndex rows (I# i#) of
          NRNull
            | rootOptional ->
                if isTrue# (events# ==# limit#) then countError else row (i# +# 1#) (events# +# 1#) present#
          NRList values
            | V.null values ->
                if isTrue# (events# ==# limit#) then countError else row (i# +# 1#) (events# +# 1#) present#
            | V.length values > limit - I# events# -> countError
            | otherwise -> case V.length values of
                I# size# -> element i# values 0# (events# +# size#) present#
          _ -> shapeError
    element i# values j# events# present#
      | I# j# == V.length values = row (i# +# 1#) events# present#
      | otherwise = case V.unsafeIndex values (I# j#) of
          NRNull
            | elementOptional -> element i# values (j# +# 1#) events# present#
            | otherwise -> Left "Parquet.Nested: NRNull at required NSPrimitive"
          NRLeaf value
            | leafValueMatches ty value -> element i# values (j# +# 1#) events# (present# +# 1#)
            | otherwise -> Left "Parquet.Nested: leaf value violates its physical or logical type"
          _ -> shapeError


fillSimpleList :: forall s. SimpleList -> SimpleCounts -> V.Vector NestedRow -> MVP.MVector s Int32 -> MVP.MVector s Int32 -> Maybe (MutableByteArray s) -> ST s Bool
fillSimpleList (SimpleList rootOptional elementOptional _ kind) counts rows definitions repetitions storage = row 0# 0# 0# 0#
  where
    !(I# rowCount#) = V.length rows
    !emptyDefinition = if rootOptional then 1 else 0 :: Int
    !elementDefinition = emptyDefinition + 1
    !valueDefinition = elementDefinition + if elementOptional then 1 else 0
    putLevel :: Int# -> Int -> Int -> ST s ()
    putLevel event# definition repetition = do
      MVP.unsafeWrite definitions (I# event#) (fromIntegral definition)
      MVP.unsafeWrite repetitions (I# event#) (fromIntegral repetition)
    {-# INLINE putLevel #-}
    row i# event# present# cursor#
      | isTrue# (i# ==# rowCount#) =
          pure
            ( I# event# == simpleEvents counts
                && I# present# == simplePresent counts
                && case storage of Nothing -> True; Just _ -> I# cursor# == simpleBytes counts
            )
      | otherwise = case V.unsafeIndex rows (I# i#) of
          NRNull -> do
            putLevel event# 0 0
            row (i# +# 1#) (event# +# 1#) present# cursor#
          NRList values
            | V.null values -> do
                putLevel event# emptyDefinition 0
                row (i# +# 1#) (event# +# 1#) present# cursor#
            | otherwise -> element i# values 0# event# present# cursor#
          _ -> error "Parquet.Nested.Fast: preflighted list changed shape"
    element i# values j# event# present# cursor#
      | I# j# == V.length values = row (i# +# 1#) event# present# cursor#
      | otherwise = do
          let !repetition = if isTrue# (j# ==# 0#) then 0 else 1
          case V.unsafeIndex values (I# j#) of
            NRNull -> do
              putLevel event# elementDefinition repetition
              element i# values (j# +# 1#) (event# +# 1#) present# cursor#
            NRLeaf value -> do
              putLevel event# valueDefinition repetition
              case storage of
                Nothing -> element i# values (j# +# 1#) (event# +# 1#) (present# +# 1#) cursor#
                Just payload -> do
                  size <- writeValueAt payload kind value (I# present#) (I# cursor#)
                  case size of
                    I# size# -> element i# values (j# +# 1#) (event# +# 1#) (present# +# 1#) (cursor# +# size#)
            _ -> error "Parquet.Nested.Fast: preflighted element changed shape"


plainValueSize :: ValueKind -> LeafValue -> Int -> Int
plainValueSize kind value present = case kind of
  KBool -> if present `rem` 8 == 0 then 1 else 0
  KInt32 -> 4
  KInt64 -> 8
  KFloat16 -> 2
  KFloat -> 4
  KDouble -> 8
  KFixed width -> width
  KText -> case value of LvString text -> TF.lengthWord8 text + 4; _ -> error "validated text"
  KBinary -> case value of LvBinary binary -> BS.length binary + 4; _ -> error "validated binary"
{-# INLINE plainValueSize #-}


-- Cache values during level filling only when the entire column needs at
-- most one MiB. Large accepted plans retain independently lazy payloads,
-- including the near-limit preflight cases that must not allocate output.
smallPayloads :: MutablePrimArray s Int -> Int -> Int -> Int -> ST s Bool
smallPayloads counters leaves !index !total
  | index == leaves = pure True
  | otherwise = do
      size <- readPrimArray counters (index * 3 + 2)
      if size > 1048576 - total then pure False else smallPayloads counters leaves (index + 1) (total + size)


-- bytestring retains the frozen pinned array as its ForeignPtr owner.
-- fromShort has a safe copying fallback for an unpinned input too.
ownedPayload :: ByteArray -> ByteString
ownedPayload (ByteArray bytes#) = SBS.fromShort (SBS.SBS bytes#)


-- Payloads stay independently lazy for callers inspecting a successful plan.
-- Each leaf keeps only its own path through the immutable input tree.
data PayloadProgram
  = PayloadScalar !ValueKind
  | PayloadOptional !PayloadProgram
  | PayloadList !PayloadProgram
  | PayloadMapKey !PayloadProgram
  | PayloadMapValue !PayloadProgram
  | PayloadStruct {-# UNPACK #-} !Int !PayloadProgram


payloadProgram :: Int -> Program -> Maybe PayloadProgram
payloadProgram target program = case program of
  Scalar index _ kind
    | target == index -> Just (PayloadScalar kind)
    | otherwise -> Nothing
  Optional _ _ child -> PayloadOptional <$> payloadProgram target child
  List _ _ _ child -> PayloadList <$> payloadProgram target child
  Mapping _ _ _ key value -> case payloadProgram target key of
    Just child -> Just (PayloadMapKey child)
    Nothing -> PayloadMapValue <$> payloadProgram target value
  Struct children -> go children 0
  where
    go children !i
      | i == V.length children = Nothing
      | otherwise = case payloadProgram target (V.unsafeIndex children i) of
          Nothing -> go children (i + 1)
          Just child -> Just (PayloadStruct i child)


materializePayload :: Program -> V.Vector NestedRow -> Int -> Int -> ByteString
materializePayload program rows index size
  | size == 0 = BS.empty
  | otherwise = case payloadProgram index program of
      Nothing -> error "Parquet.Nested.Fast: missing preflighted leaf"
      Just selected -> runST $ do
        payload <- newPinnedByteArray size
        counters <- newPrimArray 3
        setPrimArray counters 0 3 0
        fillPayloadRows counters payload selected rows
        frozen <- unsafeFreezeByteArray payload
        pure (ownedPayload frozen)


fillPayload :: MutablePrimArray s Int -> MutableByteArray s -> PayloadProgram -> NestedRow -> ST s ()
fillPayload !counters !payload program row = case (program, row) of
  (PayloadScalar kind, NRLeaf value) -> writeValue counters payload 0 kind value
  (PayloadOptional _, NRNull) -> pure ()
  (PayloadOptional child, value) -> fillPayload counters payload child value
  (PayloadList child, NRList values) -> fillPayloadRows counters payload child values
  (PayloadMapKey child, NRMapEntries pairs) -> goPair True child pairs 0
  (PayloadMapValue child, NRMapEntries pairs) -> goPair False child pairs 0
  (PayloadStruct field child, NRStruct values) -> fillPayload counters payload child (V.unsafeIndex values field)
  _ -> error "Parquet.Nested.Fast: preflighted payload changed shape"
  where
    goPair key child pairs !i
      | i == V.length pairs = pure ()
      | otherwise = case V.unsafeIndex pairs i of
          (keyRow, valueRow) -> fillPayload counters payload child (if key then keyRow else valueRow) >> goPair key child pairs (i + 1)


eachRow :: V.Vector NestedRow -> (NestedRow -> Check s ()) -> Check s ()
eachRow rows action = go 0
  where
    go !i
      | i == V.length rows = pure ()
      | otherwise = action (V.unsafeIndex rows i) >> go (i + 1)
{-# INLINE eachRow #-}


-- Select the scalar loop once per sequence rather than interpret the same
-- schema nodes for every primitive in a list or top-level column.
countRows :: MutablePrimArray s Int -> Program -> V.Vector NestedRow -> Check s ()
countRows counters program rows = case program of
  Scalar index ty kind -> scalar False index ty kind
  Optional _ _ (Scalar index ty kind) -> scalar True index ty kind
  _ -> eachRow rows (count counters program)
  where
    scalar nullable index ty kind = case kind of
      KText -> variable nullable index ty kind
      KBinary -> variable nullable index ty kind
      KBool -> countFixedRows counters index ty nullable 0 rows
      KFloat16 -> countFixedRows counters index ty nullable 2 rows
      KInt64 -> countFixedRows counters index ty nullable 8 rows
      KDouble -> countFixedRows counters index ty nullable 8 rows
      KFixed width -> countFixedRows counters index ty nullable width rows
      _ -> countFixedRows counters index ty nullable 4 rows
    variable nullable index ty kind = eachRow rows $ \row -> case row of
      NRNull -> if nullable then addNull counters index else failCheck "Parquet.Nested: NRNull at required NSPrimitive"
      NRLeaf value
        | leafValueMatches ty value -> addPresent counters index kind value
        | otherwise -> failCheck "Parquet.Nested: leaf value violates its physical or logical type"
      _ -> failCheck "Parquet.Nested: row shape does not match schema"


-- Fixed-width sequences need only a local present count while validating.
-- Update the shared leaf counters once per sequence, with multiplication
-- checked before forming the byte size. Width zero denotes packed booleans.
countFixedRows :: MutablePrimArray s Int -> Int -> LeafType -> Bool -> Int -> V.Vector NestedRow -> Check s ()
countFixedRows counters leaf ty nullable width rows = do
  let slot = leaf * 3
      length_ = V.length rows
  events <- readPrimArray counters slot
  present <- readPrimArray counters (slot + 1)
  if length_ > limit - events
    then failCheck "Parquet.Nested: value count exceeds signed 32-bit page limit"
    else
      let go !i !found
            | i == length_ =
                let total = present + found
                in if width /= 0 && total > limit `quot` width
                     then failCheck "Parquet.Nested: value bytes exceed signed 32-bit page limit"
                     else do
                       let bytes = if width == 0 then total `quot` 8 + if total `rem` 8 == 0 then 0 else 1 else total * width
                       writePrimArray counters slot (events + length_)
                       writePrimArray counters (slot + 1) total
                       writePrimArray counters (slot + 2) bytes
            | otherwise = case V.unsafeIndex rows i of
                NRNull -> if nullable then go (i + 1) found else failCheck "Parquet.Nested: NRNull at required NSPrimitive"
                NRLeaf value
                  | leafValueMatches ty value -> go (i + 1) (found + 1)
                  | otherwise -> failCheck "Parquet.Nested: leaf value violates its physical or logical type"
                _ -> failCheck "Parquet.Nested: row shape does not match schema"
      in go 0 0


fillPayloadRows :: MutablePrimArray s Int -> MutableByteArray s -> PayloadProgram -> V.Vector NestedRow -> ST s ()
fillPayloadRows !counters !payload program rows = case program of
  PayloadScalar kind -> scalar False kind
  PayloadOptional (PayloadScalar kind) -> scalar True kind
  _ -> generic 0
  where
    scalar nullable kind = do
      firstPresent <- readPrimArray counters 1
      firstCursor <- readPrimArray counters 2
      let go !i !present !cursor
            | i == V.length rows = do
                writePrimArray counters 1 present
                writePrimArray counters 2 cursor
            | otherwise = case V.unsafeIndex rows i of
                NRNull | nullable -> go (i + 1) present cursor
                NRLeaf value -> do
                  size <- writeValueAt payload kind value present cursor
                  go (i + 1) (present + 1) (cursor + size)
                _ -> error "Parquet.Nested.Fast: preflighted scalar changed shape"
      go 0 firstPresent firstCursor
    generic !i
      | i == V.length rows = pure ()
      | otherwise = fillPayload counters payload program (V.unsafeIndex rows i) >> generic (i + 1)


count :: MutablePrimArray s Int -> Program -> NestedRow -> Check s ()
count counters program row = case (program, row) of
  (Scalar index ty kind, NRLeaf value)
    | leafValueMatches ty value -> addPresent counters index kind value
    | otherwise -> failCheck "Parquet.Nested: leaf value violates its physical or logical type"
  (Scalar {}, NRNull) -> failCheck "Parquet.Nested: NRNull at required NSPrimitive"
  (Optional begin end _, NRNull) -> countNulls counters begin end
  (Optional _ _ child, value) -> count counters child value
  (List begin end _ _, NRList values) | V.null values -> countNulls counters begin end
  (List _ _ _ child, NRList values)
    | V.length values > limit -> failCheck "Parquet.Nested: list length exceeds signed 32-bit page limit"
    | otherwise -> countRows counters child values
  (Mapping begin end _ _ _, NRMapEntries values) | V.null values -> countNulls counters begin end
  (Mapping _ _ _ key value, NRMapEntries pairs)
    | V.length pairs > limit -> failCheck "Parquet.Nested: map length exceeds signed 32-bit page limit"
    | otherwise -> goPair key value pairs 0
  (Struct children, NRStruct values)
    | V.length children /= V.length values -> failCheck "Parquet.Nested: NSStruct / NRStruct field count mismatch"
    | otherwise -> goField children values 0
  _ -> failCheck "Parquet.Nested: row shape does not match schema"
  where
    goPair key value pairs !i
      | i == V.length pairs = pure ()
      | otherwise = case V.unsafeIndex pairs i of
          (keyRow, valueRow) -> count counters key keyRow >> count counters value valueRow >> goPair key value pairs (i + 1)
    goField children values !i
      | i == V.length children = pure ()
      | otherwise = count counters (V.unsafeIndex children i) (V.unsafeIndex values i) >> goField children values (i + 1)


limit :: Int
limit = 2147483647


countNulls :: MutablePrimArray s Int -> Int -> Int -> Check s ()
countNulls counters begin end = go begin
  where
    go !i
      | i == end = pure ()
      | otherwise = addNull counters i >> go (i + 1)


addNull :: MutablePrimArray s Int -> Int -> Check s ()
addNull counters leaf = do
  let slot = leaf * 3
  events <- readPrimArray counters slot
  if events == limit
    then failCheck "Parquet.Nested: value count exceeds signed 32-bit page limit"
    else writePrimArray counters slot (events + 1)
{-# INLINE addNull #-}


addPresent :: MutablePrimArray s Int -> Int -> ValueKind -> LeafValue -> Check s ()
addPresent counters leaf kind value = do
  let slot = leaf * 3
  events <- readPrimArray counters slot
  if events == limit
    then failCheck "Parquet.Nested: value count exceeds signed 32-bit page limit"
    else do
      count <- readPrimArray counters (slot + 1)
      bytes <- readPrimArray counters (slot + 2)
      let size = plainValueSize kind value count
      if size < 0 || size > limit || bytes > limit - size
        then failCheck "Parquet.Nested: value bytes exceed signed 32-bit page limit"
        else do
          writePrimArray counters slot (events + 1)
          writePrimArray counters (slot + 1) (count + 1)
          writePrimArray counters (slot + 2) (bytes + size)
{-# INLINE addPresent #-}


fillRows :: MutablePrimArray s Int -> V.Vector (LeafBuffer s) -> Program -> V.Vector NestedRow -> ST s ()
fillRows counters buffers program rows = fillSequence counters buffers program rows 0 0 0


fillSequence :: MutablePrimArray s Int -> V.Vector (LeafBuffer s) -> Program -> V.Vector NestedRow -> Int -> Int -> Int -> ST s ()
fillSequence counters buffers program rows !definition !firstRep !laterRep = case program of
  Scalar leaf _ kind -> scalar False leaf kind
  Optional _ _ (Scalar leaf _ kind) -> scalar True leaf kind
  _ -> generic 0
  where
    scalar nullable leaf kind = do
      let slot = leaf * 3
          buffer = V.unsafeIndex buffers leaf
      start <- readPrimArray counters slot
      let go !i
            | i == V.length rows = writePrimArray counters slot (start + i)
            | otherwise = do
                let def = if nullable then case V.unsafeIndex rows i of NRNull -> definition; _ -> definition + 1 else definition
                    rep = if i == 0 then firstRep else laterRep
                MVP.unsafeWrite (bufferDefinitions buffer) (start + i) (fromIntegral def)
                MVP.unsafeWrite (bufferRepetitions buffer) (start + i) (fromIntegral rep)
                go (i + 1)
          cached payload !i !present !cursor
            | i == V.length rows = do
                writePrimArray counters slot (start + i)
                writePrimArray counters (slot + 1) present
                writePrimArray counters (slot + 2) cursor
            | otherwise = do
                let row = V.unsafeIndex rows i
                    def = if nullable then case row of NRNull -> definition; _ -> definition + 1 else definition
                    rep = if i == 0 then firstRep else laterRep
                MVP.unsafeWrite (bufferDefinitions buffer) (start + i) (fromIntegral def)
                MVP.unsafeWrite (bufferRepetitions buffer) (start + i) (fromIntegral rep)
                case row of
                  NRLeaf value -> do
                    size <- writeValueAt payload kind value present cursor
                    cached payload (i + 1) (present + 1) (cursor + size)
                  _ -> cached payload (i + 1) present cursor
      case bufferPayload buffer of
        Nothing -> go 0
        Just payload -> do
          present <- readPrimArray counters (slot + 1)
          cursor <- readPrimArray counters (slot + 2)
          cached payload 0 present cursor
    generic !i
      | i == V.length rows = pure ()
      | otherwise = fill counters buffers program (V.unsafeIndex rows i) definition (if i == 0 then firstRep else laterRep) >> generic (i + 1)


fill :: MutablePrimArray s Int -> V.Vector (LeafBuffer s) -> Program -> NestedRow -> Int -> Int -> ST s ()
fill counters buffers program row !definition !repetition = case (program, row) of
  (Scalar index _ kind, NRLeaf value) -> do
    emitLevel counters buffers index definition repetition
    case bufferPayload (V.unsafeIndex buffers index) of
      Nothing -> pure ()
      Just payload -> writeValue counters payload index kind value
  (Optional begin end _, NRNull) -> fillNulls counters buffers begin end definition repetition
  (Optional _ _ child, value) -> fill counters buffers child value (definition + 1) repetition
  (List begin end _ _, NRList values) | V.null values -> fillNulls counters buffers begin end definition repetition
  (List _ _ depth child, NRList values) -> fillSequence counters buffers child values (definition + 1) repetition depth
  (Mapping begin end _ _ _, NRMapEntries values) | V.null values -> fillNulls counters buffers begin end definition repetition
  (Mapping _ _ depth key value, NRMapEntries pairs) -> goPair depth key value pairs 0
  (Struct children, NRStruct values) -> goField children values 0
  _ -> error "Parquet.Nested.Fast: validated row changed shape"
  where
    goPair depth key value pairs !i
      | i == V.length pairs = pure ()
      | otherwise = case V.unsafeIndex pairs i of
          (keyRow, valueRow) -> do
            let rep = if i == 0 then repetition else depth
            fill counters buffers key keyRow (definition + 1) rep
            fill counters buffers value valueRow (definition + 1) rep
            goPair depth key value pairs (i + 1)
    goField children values !i
      | i == V.length children = pure ()
      | otherwise = fill counters buffers (V.unsafeIndex children i) (V.unsafeIndex values i) definition repetition >> goField children values (i + 1)


emitLevel :: MutablePrimArray s Int -> V.Vector (LeafBuffer s) -> Int -> Int -> Int -> ST s ()
emitLevel counters buffers leaf definition repetition = do
  let slot = leaf * 3; buffer = V.unsafeIndex buffers leaf
  event <- readPrimArray counters slot
  MVP.unsafeWrite (bufferDefinitions buffer) event (fromIntegral definition)
  MVP.unsafeWrite (bufferRepetitions buffer) event (fromIntegral repetition)
  writePrimArray counters slot (event + 1)
{-# INLINE emitLevel #-}


fillNulls :: MutablePrimArray s Int -> V.Vector (LeafBuffer s) -> Int -> Int -> Int -> Int -> ST s ()
fillNulls counters buffers begin end definition repetition = go begin
  where
    go !i
      | i == end = pure ()
      | otherwise = emitLevel counters buffers i definition repetition >> go (i + 1)


writeValue :: MutablePrimArray s Int -> MutableByteArray s -> Int -> ValueKind -> LeafValue -> ST s ()
writeValue counters payload leaf kind value = do
  let slot = leaf * 3
  present <- readPrimArray counters (slot + 1)
  cursor <- readPrimArray counters (slot + 2)
  size <- writeValueAt payload kind value present cursor
  writePrimArray counters (slot + 1) (present + 1)
  writePrimArray counters (slot + 2) (cursor + size)
{-# INLINE writeValue #-}


writeValueAt :: MutableByteArray s -> ValueKind -> LeafValue -> Int -> Int -> ST s Int
writeValueAt payload kind value present cursor =
  case (kind, value) of
    (KInt32, LvInt32 x) -> writeByteArray payload present (le32 (fromIntegral x)) >> pure 4
    (KInt64, LvInt64 x) -> writeByteArray payload present (le64 (fromIntegral x)) >> pure 8
    (KFloat16, LvFloat16 x) -> writeByteArray payload present (le16 x) >> pure 2
    (KFloat, LvFloat x) -> writeByteArray payload present (le32 (castFloatToWord32 x)) >> pure 4
    (KDouble, LvDouble x) -> writeByteArray payload present (le64 (castDoubleToWord64 x)) >> pure 8
    (KBool, LvBool x) -> do
      let (byte, bit) = present `quotRem` 8
      old <- if bit == 0 then pure 0 else readByteArray payload byte
      writeByteArray payload byte (if x then setBit old bit else old :: Word8)
      pure (if bit == 0 then 1 else 0)
    (KText, LvString text) -> do
      let size = TF.lengthWord8 text
      prefix payload cursor size
      if size == 0 then pure () else unsafeIOToST (TF.unsafeCopyToPtr text (mutableByteArrayContents payload `plusPtr` (cursor + 4)))
      touch payload
      pure (size + 4)
    (KBinary, LvBinary bytes) -> do
      let size = BS.length bytes
      prefix payload cursor size
      copy payload (cursor + 4) bytes
      pure (size + 4)
    (KFixed width, LvFixedBinary bytes) -> copy payload cursor bytes >> pure width
    _ -> error "Parquet.Nested.Fast: validated scalar changed type"
{-# INLINE writeValueAt #-}


prefix :: MutableByteArray s -> Int -> Int -> ST s ()
prefix payload cursor size = do
  writeByteArray payload cursor (fromIntegral size :: Word8)
  writeByteArray payload (cursor + 1) (fromIntegral (size `shiftR` 8) :: Word8)
  writeByteArray payload (cursor + 2) (fromIntegral (size `shiftR` 16) :: Word8)
  writeByteArray payload (cursor + 3) (fromIntegral (size `shiftR` 24) :: Word8)
{-# INLINE prefix #-}


copy :: MutableByteArray s -> Int -> ByteString -> ST s ()
copy payload cursor bytes = do
  let (fp, offset, size) = BSI.toForeignPtr bytes
  if size == 0 then pure () else unsafeIOToST $ withForeignPtr fp $ \ptr -> copyBytes (mutableByteArrayContents payload `plusPtr` cursor) (ptr `plusPtr` offset) size
  touch payload
{-# INLINE copy #-}


le16 :: Word16 -> Word16
le16 x = case targetByteOrder of LittleEndian -> x; BigEndian -> byteSwap16 x


le32 :: Word32 -> Word32
le32 x = case targetByteOrder of LittleEndian -> x; BigEndian -> byteSwap32 x


le64 :: Word64 -> Word64
le64 x = case targetByteOrder of LittleEndian -> x; BigEndian -> byteSwap64 x


{-# INLINE le16 #-}


{-# INLINE le32 #-}


{-# INLINE le64 #-}
