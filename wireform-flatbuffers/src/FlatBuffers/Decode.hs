{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Schema-directed decoding of dynamic FlatBuffers values. Unknown future
table slots are ignored, and absent scalars receive the schema's defaults.
-}
module FlatBuffers.Decode (decode, decodeWith) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import FlatBuffers.Internal.Schema
import FlatBuffers.Reader qualified as R
import FlatBuffers.Schema
import FlatBuffers.Value qualified as F
import GHC.Exts (Int (I#), Int#, isTrue#, (-#), (>#))


-- | Compile the schema, then decode a root table.
decode :: FlatBuffersSchema -> ByteString -> Either String F.Value
decode schema bytes = compileSchema schema >>= (`decodeWith` bytes)


{- | Reuse a compiled schema. Depth is limited to 64 and materialization to one
million values, including repeated references, so hostile DAGs cannot expand
into an unbounded AST. This is an allocating API; use Reader/View for cursors.
-}
decodeWith :: CompiledSchema -> ByteString -> Either String F.Value
decodeWith schema bytes = case runDecode# root 1000000# of
  (# (# value, _ #) | #) -> Right value
  (# | err #) -> Left err
  where
    root = do
      case csIdentifier schema of
        Just identifier -> require (BS.take 4 (BS.drop 4 bytes) == identifier) "file identifier mismatch"
        Nothing -> pure ()
      position <- follow bytes 0
      require (position >= maybe 4 (const 8) (csIdentifier schema)) "root overlaps header"
      readTable schema bytes 0 (csRoot schema) position


-- A bounded decoder with an unboxed budget and success/error result. In
-- particular, recursive reads do not allocate StateT (value, budget) pairs.
newtype Decode a = Decode {runDecode# :: Int# -> (# (# a, Int# #) | String #)}


instance Functor Decode where
  fmap f (Decode readValue) = Decode $ \budget -> case readValue budget of
    (# (# value, rest #) | #) -> (# (# f value, rest #) | #)
    (# | err #) -> (# | err #)
  {-# INLINE fmap #-}


instance Applicative Decode where
  pure value = Decode (\budget -> (# (# value, budget #) | #))
  {-# INLINE pure #-}
  functions <*> values = do
    f <- functions
    value <- values
    pure (f value)
  {-# INLINE (<*>) #-}


instance Monad Decode where
  Decode readValue >>= next = Decode $ \budget -> case readValue budget of
    (# (# value, rest #) | #) -> runDecode# (next value) rest
    (# | err #) -> (# | err #)
  {-# INLINE (>>=) #-}


lift :: Either String a -> Decode a
lift result = Decode $ \budget -> case result of
  Right value -> (# (# value, budget #) | #)
  Left err -> (# | err #)
{-# INLINE lift #-}


require :: Bool -> String -> Decode ()
require yes message = unless yes (lift (Left ("FlatBuffers: " <> message)))
{-# INLINE require #-}


charge :: Int -> Decode ()
charge depth = do
  require (depth < 64) "nesting exceeds limit"
  Decode $ \budget ->
    if isTrue# (budget ># 0#)
      then (# (# (), budget -# 1# #) | #)
      else (# | "FlatBuffers: materialized value limit exceeded" #)
{-# INLINE charge #-}


requireBudget :: Int -> Decode ()
requireBudget (I# count) = Decode $ \budget ->
  if isTrue# (count ># budget)
    then (# | "FlatBuffers: vector exceeds materialization limit" #)
    else (# (# (), budget #) | #)
{-# INLINE requireBudget #-}


bounds :: ByteString -> Int -> Int -> Int -> Decode ()
bounds bytes position size alignment =
  require (position >= 0 && size >= 0 && position <= BS.length bytes - size && position `rem` alignment == 0) "misaligned or out-of-bounds object"
{-# INLINE bounds #-}


follow :: ByteString -> Int -> Decode Int
follow bytes position = do
  bounds bytes position 4 4
  offset <- lift (R.peekU32 bytes position)
  require (offset >= 4 && toInteger offset <= toInteger (BS.length bytes - position)) "invalid forward offset"
  pure (position + fromIntegral offset)


readTable :: CompiledSchema -> ByteString -> Int -> TableLayout -> Int -> Decode F.Value
readTable schema bytes depth layout position = do
  charge depth
  bounds bytes position 4 4
  displacement <- lift (R.peekI32 bytes position)
  let vtable = position - fromIntegral displacement
  bounds bytes vtable 4 2
  size <- fromIntegral <$> lift (R.peekU16 bytes vtable)
  objectSize <- fromIntegral <$> lift (R.peekU16 bytes (vtable + 2))
  require (size >= 4 && even size && objectSize >= 4) "invalid table/vtable size"
  bounds bytes vtable size 2
  bounds bytes position objectSize 4
  let slot index width alignment
        | 4 + 2 * index >= size = pure Nothing
        | otherwise = do
            offset <- fromIntegral <$> lift (R.peekU16 bytes (vtable + 4 + 2 * index))
            if offset == 0
              then pure Nothing
              else do
                require (offset >= 4 && offset <= objectSize - width) "field overruns its table"
                bounds bytes (position + offset) width alignment
                pure (Just (position + offset))
      field definition
        | flDeprecated definition = pure Nothing
        | isUnion schema (flType definition) = do
            tagPosition <- slot (flSlot definition - 1) 1 1
            tag <- maybe (pure 0) (lift . R.peekU8 bytes) tagPosition
            targetPosition <- slot (flSlot definition) 4 4
            if tag == 0
              then do
                require (targetPosition == Nothing) "union value without discriminator"
                require (not (flRequired definition)) "missing required union"
                pure Nothing
              else do
                target <- maybe (lift (Left "FlatBuffers: union discriminator without value")) (follow bytes) targetPosition
                let members = case flType definition of
                      FTNamed name -> fudMembers (csUnions schema Map.! name)
                      _ -> V.empty
                require (fromIntegral tag <= V.length members) "unknown union discriminator"
                Just . F.VUnion tag <$> readType schema bytes (depth + 1) (FTNamed (members V.! (fromIntegral tag - 1))) target
        | otherwise = do
            let ty = flType definition
                inline = typeLayout schema ty
                width = if referenceType schema ty then 4 else slSize inline
                alignment = if referenceType schema ty then 4 else slAlign inline
            location <- slot (flSlot definition) width alignment
            case location of
              Nothing -> do
                require (not (flRequired definition)) "missing required field"
                pure (flDefault definition)
              Just at -> do
                target <- if referenceType schema ty then follow bytes at else pure at
                Just <$> readType schema bytes (depth + 1) ty target
  F.VTable <$> V.mapM field (tlFields layout)


readType :: CompiledSchema -> ByteString -> Int -> FBType -> Int -> Decode F.Value
readType schema bytes depth ty position = do
  charge depth
  case ty of
    FTString -> do
      bounds bytes position 4 4
      n <- fromIntegral <$> lift (R.peekU32 bytes position)
      require (n < BS.length bytes - position - 4) "string overruns buffer or lacks terminator"
      require (BS.index bytes (position + 4 + n) == 0) "string lacks NUL terminator"
      F.VString <$> lift (R.readString bytes position)
    FTVector element -> do
      bounds bytes position 4 4
      n <- fromIntegral <$> lift (R.peekU32 bytes position)
      let layout = typeLayout schema element
          reference = referenceType schema element
          width = if reference then 4 else slSize layout
          alignment = if reference then 4 else slAlign layout
      require (n <= (BS.length bytes - position - 4) `quot` width) "vector overruns buffer"
      bounds bytes (position + 4) (n * width) alignment
      requireBudget n
      F.VVector
        <$> V.generateM
          n
          ( \i -> do
              let at = position + 4 + i * width
              target <- if reference then follow bytes at else pure at
              readType schema bytes (depth + 1) element target
          )
    FTNamed name
      | Just table <- Map.lookup name (csTables schema) -> readTable schema bytes depth table position
      | Just layout <- Map.lookup name (csStructs schema) -> do
          bounds bytes position (slSize layout) (slAlign layout)
          F.VStruct <$> V.mapM (\field -> readType schema bytes (depth + 1) (sfType field) (position + sfOffset field)) (slFields layout)
      | Just enumeration <- Map.lookup name (csEnums schema) -> readScalar bytes (fedUnderlyingType enumeration) position
      | otherwise -> lift (Left "FlatBuffers: union outside table field")
    _ -> readScalar bytes ty position


readScalar :: ByteString -> FBType -> Int -> Decode F.Value
readScalar bytes ty position = do
  let layout = scalarLayout ty
  bounds bytes position (slSize layout) (slAlign layout)
  lift $ case ty of
    FTBool -> do
      value <- R.peekU8 bytes position
      if value <= 1 then Right (F.VBool (value /= 0)) else Left "FlatBuffers: invalid bool"
    FTByte -> F.VInt8 <$> R.peekI8 bytes position
    FTUByte -> F.VWord8 <$> R.peekU8 bytes position
    FTShort -> F.VInt16 <$> R.peekI16 bytes position
    FTUShort -> F.VWord16 <$> R.peekU16 bytes position
    FTInt -> F.VInt32 <$> R.peekI32 bytes position
    FTUInt -> F.VWord32 <$> R.peekU32 bytes position
    FTLong -> F.VInt64 <$> R.peekI64 bytes position
    FTULong -> F.VWord64 <$> R.peekU64 bytes position
    FTFloat -> F.VFloat <$> R.peekFloat bytes position
    FTDouble -> F.VDouble <$> R.peekDouble bytes position
    _ -> Left "FlatBuffers: expected scalar type"
