{-# LANGUAGE BangPatterns #-}

{- | Schema-directed dynamic encoding. Values use declaration order; field IDs
and union discriminator slots are an implementation detail of the schema.
-}
module FlatBuffers.Encode (encode, encodeWith) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Word (Word8)
import FlatBuffers.Builder qualified as B
import FlatBuffers.Internal.Schema
import FlatBuffers.Schema
import FlatBuffers.Value qualified as F
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)


{- | Compile the schema and encode a root table. Use 'encodeWith' when reusing
a schema. Missing scalars take their schema defaults; reference fields stay
absent unless required. Unsupported schemas and ill-typed values return Left.
-}
encode :: FlatBuffersSchema -> F.Value -> Either String ByteString
encode schema value = compileSchema schema >>= (`encodeWith` value)


encodeWith :: CompiledSchema -> F.Value -> Either String ByteString
encodeWith schema value = do
  normalized <- normalizeTable schema 0 (csRoot schema) value
  Right (emitRoot schema normalized)


emitRoot :: CompiledSchema -> F.Value -> ByteString
emitRoot schema value = unsafePerformIO $ do
  builder <- B.newBuilder
  root <- writeTable builder schema (csRoot schema) value
  case csIdentifier schema of
    Nothing -> B.finish builder root
    Just identifier -> B.finishWithIdentifier builder root identifier
{-# NOINLINE emitRoot #-}


normalizeTable :: CompiledSchema -> Int -> TableLayout -> F.Value -> Either String F.Value
normalizeTable schema depth layout (F.VTable fields) = do
  when (depth >= 64) (Left "FlatBuffers: value nesting exceeds limit")
  when (V.length fields > V.length (tlFields layout)) (Left "FlatBuffers: too many table values")
  F.VTable <$> V.imapM field (tlFields layout)
  where
    field i definition = case fields V.!? i >>= id of
      _ | flDeprecated definition -> Right Nothing
      Nothing
        | flRequired definition -> Left "FlatBuffers: missing required field"
        | otherwise -> Right (flDefault definition)
      Just value -> Just <$> normalize schema (depth + 1) (flType definition) value
normalizeTable _ _ _ _ = Left "FlatBuffers: root/table value must be VTable"


normalize :: CompiledSchema -> Int -> FBType -> F.Value -> Either String F.Value
normalize schema depth ty value
  | depth >= 64 = Left "FlatBuffers: value nesting exceeds limit"
  | otherwise = case (ty, value) of
      (FTString, F.VString _) -> Right value
      (FTVector element, F.VVector values) -> F.VVector <$> V.mapM (normalize schema (depth + 1) element) values
      (FTNamed name, _)
        | Just table <- Map.lookup name (csTables schema) -> normalizeTable schema depth table value
        | Just layout <- Map.lookup name (csStructs schema) -> case value of
            F.VStruct fields -> do
              unless (V.length fields == V.length (slFields layout)) (Left "FlatBuffers: wrong struct arity")
              F.VStruct <$> V.imapM (\i definition -> normalize schema (depth + 1) (sfType definition) (fields V.! i)) (slFields layout)
            _ -> Left "FlatBuffers: expected VStruct"
        | Just enumeration <- Map.lookup name (csEnums schema) -> scalarValue (fedUnderlyingType enumeration) value
        | Just union <- Map.lookup name (csUnions schema) -> case value of
            F.VUnion tag child
              | tag /= 0 && fromIntegral tag <= V.length (fudMembers union) ->
                  F.VUnion tag <$> normalize schema (depth + 1) (FTNamed (fudMembers union V.! (fromIntegral tag - 1))) child
            _ -> Left "FlatBuffers: expected VUnion with a declared, nonzero tag"
      (FTString, _) -> Left "FlatBuffers: expected VString"
      (FTVector _, _) -> Left "FlatBuffers: expected VVector"
      _ -> scalarValue ty value


writeTable :: B.Builder -> CompiledSchema -> TableLayout -> F.Value -> IO Int
writeTable builder schema layout (F.VTable fields) = do
  -- Indexed by physical slot, independently of declaration order.
  pairs <- V.imapM field (tlFields layout)
  let slots = V.accum (\_ new -> new) (V.replicate (tlSlots layout) Nothing) (concat (V.toList pairs))
  B.writeTable builder (V.toList slots)
  where
    field i definition = case fields V.! i of
      Nothing -> pure []
      Just (F.VUnion tag child) -> do
        target <- writeReference builder schema (unionType schema (flType definition) tag) child
        pure [(flSlot definition - 1, Just (B.scalarU8 tag)), (flSlot definition, Just (B.voff target))]
      Just value -> do
        slot <-
          if referenceType schema (flType definition)
            then B.voff <$> writeReference builder schema (flType definition) value
            else pure (inlineField schema (flType definition) value)
        pure [(flSlot definition, Just slot)]
writeTable _ _ _ _ = error "FlatBuffers: internal table invariant"


unionType :: CompiledSchema -> FBType -> Word8 -> FBType
unionType schema (FTNamed name) tag = FTNamed (fudMembers (csUnions schema Map.! name) V.! (fromIntegral tag - 1))
unionType _ _ _ = error "FlatBuffers: internal union invariant"


writeReference :: B.Builder -> CompiledSchema -> FBType -> F.Value -> IO Int
writeReference builder schema ty value = case (ty, value) of
  (FTString, F.VString text) -> B.writeString builder text
  (FTVector element, F.VVector values)
    | referenceType schema element -> do
        offsets <- V.mapM (writeReference builder schema element) values
        B.writeVectorOfOffsets builder (V.toList offsets)
    | otherwise -> do
        let layout = typeLayout schema element
        B.writeVectorOfStructs
          builder
          (slSize layout)
          (slAlign layout)
          (V.toList (V.map (writeInline schema element) values))
  (FTNamed name, _) -> writeTable builder schema (csTables schema Map.! name) value
  _ -> error "FlatBuffers: internal reference invariant"


inlineField :: CompiledSchema -> FBType -> F.Value -> B.Field'
inlineField schema ty value = case value of
  F.VStruct _ -> let layout = typeLayout schema ty in B.struct (slSize layout) (slAlign layout) (\builder -> writeInline schema ty value builder)
  F.VBool x -> B.scalarU8 (if x then 1 else 0)
  F.VInt8 x -> B.scalarU8 (fromIntegral x)
  F.VInt16 x -> B.scalarI16 x
  F.VInt32 x -> B.scalarI32 x
  F.VInt64 x -> B.scalarI64 x
  F.VWord8 x -> B.scalarU8 x
  F.VWord16 x -> B.scalarU16 x
  F.VWord32 x -> B.scalarU32 x
  F.VWord64 x -> B.scalarU64 x
  F.VFloat x -> B.scalarU32 (castFloatToWord32 x)
  F.VDouble x -> B.scalarU64 (castDoubleToWord64 x)
  _ -> error "FlatBuffers: internal scalar invariant"


writeInline :: CompiledSchema -> FBType -> F.Value -> B.Builder -> IO ()
writeInline schema ty (F.VStruct fields) builder = do
  let layout = typeLayout schema ty
      go i end
        | i < 0 = zeros end
        | otherwise = do
            let definition = slFields layout V.! i
                childLayout = typeLayout schema (sfType definition)
            zeros (end - sfOffset definition - slSize childLayout)
            writeInline schema (sfType definition) (fields V.! i) builder
            go (i - 1) (sfOffset definition)
      zeros n
        | n <= 0 = pure ()
        | otherwise = B.prependU8 builder 0 >> zeros (n - 1)
  go (V.length fields - 1) (slSize layout)
writeInline _ _ value builder = case value of
  F.VBool x -> B.prependU8 builder (if x then 1 else 0)
  F.VInt8 x -> B.prependU8 builder (fromIntegral x)
  F.VInt16 x -> B.prependI16 builder x
  F.VInt32 x -> B.prependI32 builder x
  F.VInt64 x -> B.prependI64 builder x
  F.VWord8 x -> B.prependU8 builder x
  F.VWord16 x -> B.prependU16 builder x
  F.VWord32 x -> B.prependU32 builder x
  F.VWord64 x -> B.prependU64 builder x
  F.VFloat x -> B.prependU32 builder (castFloatToWord32 x)
  F.VDouble x -> B.prependU64 builder (castDoubleToWord64 x)
  _ -> error "FlatBuffers: internal inline invariant"
