{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-flatbuffers' encode + decode hot paths,
plus the low-level "FlatBuffers.Builder" on the shapes Arrow IPC
emits (schema, record batch header) and generic table / vector /
string workloads.

The encode/decode groups operate at the dynamic 'FlatBuffers.Value'
level since the typed `View` interface is per-record-type and
doesn't have a single benchable function for an arbitrary record.
-}
module Main (main) where

import Control.Monad (foldM)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import FlatBuffers.Builder qualified as B
import FlatBuffers.Decode qualified as FBD
import FlatBuffers.Encode qualified as FBE
import FlatBuffers.Value qualified as FB


person :: FB.Value
person =
  FB.VTable $
    V.fromList
      [ Just (FB.VString "Alice")
      , Just (FB.VInt32 30)
      , Just (FB.VString "alice@example.com")
      ]


-- 100 Person tables wrapped in a root container with a vector slot.
people :: FB.Value
people = FB.VTable $
  V.singleton $
    Just $
      FB.VVector $
        V.generate 100 $ \i ->
          FB.VTable $
            V.fromList
              [ Just (FB.VString (T.pack ("user-" <> show i)))
              , Just (FB.VInt32 (fromIntegral (20 + i `mod` 50)))
              , Just (FB.VString (T.pack ("user" <> show i <> "@example.com")))
              ]


-- ============================================================
-- Builder workloads. The Arrow-shaped ones mirror the call
-- sequence in "Arrow.FlatBufferIPC" (writeField / writeSchema /
-- writeRecordBatch / buildSchemaMessage) so the numbers track what
-- the Arrow encoder pays per message.
-- ============================================================

{- | How scalar table fields are described. 'ScalarClosures' is what
"Arrow.FlatBufferIPC" writes (@scalar n (\\bb -> prependX bb v)@),
which the builder's rewrite rules turn into closure-free fields.
'OpaqueWriters' builds the same fields through the 'B.Field''
constructor, which no rule matches, so 'B.writeTable' has to call each
writer. The workloads are written once over this class and
specialised.
-}
class FieldStyle s where
  u8 :: Proxy s -> Word8 -> B.Field'
  i16 :: Proxy s -> Int16 -> B.Field'
  i32 :: Proxy s -> Int32 -> B.Field'
  i64 :: Proxy s -> Int64 -> B.Field'


data ScalarClosures


data OpaqueWriters


instance FieldStyle ScalarClosures where
  u8 _ v = B.scalar 1 (\bb -> B.prependU8 bb v)
  i16 _ v = B.scalar 2 (\bb -> B.prependI16 bb v)
  i32 _ v = B.scalar 4 (\bb -> B.prependI32 bb v)
  i64 _ v = B.scalar 8 (\bb -> B.prependI64 bb v)
  {-# INLINE u8 #-}
  {-# INLINE i16 #-}
  {-# INLINE i32 #-}
  {-# INLINE i64 #-}


instance FieldStyle OpaqueWriters where
  u8 _ v = B.Field' 1 (\bb _ -> B.prependU8 bb v)
  i16 _ v = B.Field' 2 (\bb _ -> B.prependI16 bb v)
  i32 _ v = B.Field' 4 (\bb _ -> B.prependI32 bb v)
  i64 _ v = B.Field' 8 (\bb _ -> B.prependI64 bb v)
  {-# INLINE u8 #-}
  {-# INLINE i16 #-}
  {-# INLINE i32 #-}
  {-# INLINE i64 #-}


-- | Arrow field type (the subset the workloads use).
data AType = AInt Int32 Bool | AFloat Int16 | AEmpty Word8


-- | Arrow field description: name, nullable, type, children.
data AField = AField T.Text Bool AType [AField]


-- | Returns the type union tag and the type table's UOffset.
writeArrowType :: (FieldStyle s) => Proxy s -> B.Builder -> AType -> IO (Word8, Int)
writeArrowType p b = \case
  AInt bits signed -> do
    u <- B.writeTable b [Just (i32 p bits), Just (u8 p (if signed then 1 else 0))]
    pure (2, u)
  AFloat prec -> do
    u <- B.writeTable b [Just (i16 p prec)]
    pure (3, u)
  AEmpty tag -> do
    u <- B.writeTable b []
    pure (tag, u)


writeArrowField :: (FieldStyle s) => Proxy s -> B.Builder -> AField -> IO Int
writeArrowField p b (AField name nullable ty kids) = do
  kidsVec <-
    if null kids
      then pure Nothing
      else do
        us <- mapM (writeArrowField p b) kids
        Just <$> B.writeVectorOfOffsets b us
  (tag, tyU) <- writeArrowType p b ty
  nameU <- B.writeString b name
  B.writeTable
    b
    [ Just (B.voff nameU)
    , if nullable then Just (u8 p 1) else Nothing
    , Just (u8 p tag)
    , Just (B.voff tyU)
    , Nothing
    , B.voff <$> kidsVec
    , Nothing
    ]


schemaMessage :: (FieldStyle s) => Proxy s -> [AField] -> IO ByteString
schemaMessage p fields = do
  b <- B.newBuilder
  us <- mapM (writeArrowField p b) fields
  fv <- B.writeVectorOfOffsets b us
  sch <- B.writeTable b [Nothing, Just (B.voff fv), Nothing, Nothing]
  msg <-
    B.writeTable
      b
      [ Just (i16 p 4)
      , Just (u8 p 1)
      , Just (B.voff sch)
      , Just (i64 p 0)
      , Nothing
      ]
  B.finish b msg


schema1 :: [AField]
schema1 = [AField "a" False (AInt 64 True) []]


schema6 :: [AField]
schema6 =
  [ AField "id" False (AInt 64 True) []
  , AField "name" True (AEmpty 5) []
  , AField "score" True (AFloat 2) []
  , AField "flag" False (AEmpty 6) []
  , AField "tags" True (AEmpty 12) [AField "item" True (AInt 32 True) []]
  , AField "count" False (AInt 32 True) []
  ]


-- | Record batch header with @nNodes@ field nodes and @nBufs@ buffers.
recordBatchMessage :: Int -> Int -> IO ByteString
recordBatchMessage nNodes nBufs = do
  b <- B.newBuilder
  bufs <-
    B.writeVectorOfStructs
      b
      16
      8
      [ \bb -> B.prependI64 bb (fromIntegral (8 * i)) >> B.prependI64 bb (fromIntegral (800 * i))
      | i <- [0 .. nBufs - 1]
      ]
  nodes <-
    B.writeVectorOfStructs
      b
      16
      8
      [ \bb -> B.prependI64 bb 0 >> B.prependI64 bb 100
      | _ <- [1 .. nNodes]
      ]
  rb <-
    B.writeTable
      b
      [ Just (B.scalar 8 (\bb -> B.prependI64 bb 100))
      , Just (B.voff nodes)
      , Just (B.voff bufs)
      , Nothing
      , Nothing
      ]
  msg <-
    B.writeTable
      b
      [ Just (B.scalar 2 (\bb -> B.prependI16 bb 4))
      , Just (B.scalar 1 (\bb -> B.prependU8 bb 3))
      , Just (B.voff rb)
      , Just (B.scalar 8 (\bb -> B.prependI64 bb 4096))
      , Nothing
      ]
  B.finish b msg


-- | One table with @n@ int64 scalar fields.
scalarTable :: (FieldStyle s) => Proxy s -> Int -> IO ByteString
scalarTable p n = do
  b <- B.newBuilder
  t <- B.writeTable b [Just (i64 p (fromIntegral i)) | i <- [1 .. n]]
  B.finish b t


-- | @n@ strings, a vector of offsets to them, and a root table.
stringVector :: [T.Text] -> IO ByteString
stringVector ss = do
  b <- B.newBuilder
  us <- mapM (B.writeString b) ss
  v <- B.writeVectorOfOffsets b us
  t <- B.writeTable b [Just (B.voff v)]
  B.finish b t


-- | @n@ small tables (same shape, so their vtables dedup), a vector of
-- offsets to them, and a root table.
tableVector :: Int -> IO ByteString
tableVector n = do
  b <- B.newBuilder
  us <-
    mapM
      ( \i ->
          B.writeTable
            b
            [ Just (B.scalar 4 (\bb -> B.prependI32 bb (fromIntegral i)))
            , Nothing
            , Just (B.scalar 8 (\bb -> B.prependI64 bb (fromIntegral i)))
            ]
      )
      [1 .. n]
  v <- B.writeVectorOfOffsets b us
  t <- B.writeTable b [Just (B.voff v)]
  B.finish b t


-- | A chain of @n@ nested tables.
nestedTables :: Int -> IO ByteString
nestedTables n = do
  b <- B.newBuilder
  leaf <- B.writeTable b [Just (B.scalar 4 (\bb -> B.prependI32 bb 7))]
  top <-
    foldM
      (\u i -> B.writeTable b [Just (B.voff u), Just (B.scalar 2 (\bb -> B.prependI16 bb (fromIntegral i)))])
      leaf
      [1 .. n]
  B.finish b top


-- | A vector of @n@ int64 scalars under a root table.
int64Vector :: [Int64] -> IO ByteString
int64Vector xs = do
  b <- B.newBuilder
  v <- B.writeVectorInt64 b xs
  t <- B.writeTable b [Just (B.voff v)]
  B.finish b t


-- | One table holding a long byte payload via 'B.prependBS'.
blobTable :: ByteString -> IO ByteString
blobTable payload = do
  b <- B.newBuilder
  B.prepForObject b (4 + BS.length payload) 4
  B.prependBS b payload
  B.prependU32 b (fromIntegral (BS.length payload))
  v <- B.currentUOff b
  t <- B.writeTable b [Just (B.voff v)]
  B.finish b t


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "Person table" $ nf FBE.encode person
        , bench "Person[100] vector" $ nf FBE.encode people
        ]
    , bgroup
        "decode"
        [ env (pure (FBE.encode person)) $ \bs ->
            bench "Person table" $ nf (FBD.decode :: ByteString -> Either String FB.Value) bs
        , env (pure (FBE.encode people)) $ \bs ->
            bench "Person[100] vector" $ nf (FBD.decode :: ByteString -> Either String FB.Value) bs
        ]
    , bgroup
        "builder"
        [ bench "arrow schema 1 field" $ whnfIO (schemaMessage (Proxy @ScalarClosures) schema1)
        , bench "arrow schema 6 fields" $ whnfIO (schemaMessage (Proxy @ScalarClosures) schema6)
        , bench "arrow schema 1 field, opaque writers" $ whnfIO (schemaMessage (Proxy @OpaqueWriters) schema1)
        , bench "arrow schema 6 fields, opaque writers" $ whnfIO (schemaMessage (Proxy @OpaqueWriters) schema6)
        , bench "arrow record batch 6 nodes 14 buffers" $ whnfIO (recordBatchMessage 6 14)
        , bench "table 16 scalars" $ whnfIO (scalarTable (Proxy @ScalarClosures) 16)
        , bench "table 16 scalars, opaque writers" $ whnfIO (scalarTable (Proxy @OpaqueWriters) 16)
        , bench "nested tables 100" $ whnfIO (nestedTables 100)
        , bench "tables[1000] vector" $ whnfIO (tableVector 1000)
        , env (pure strings1000) $ \ss ->
            bench "strings[1000] vector" $ whnfIO (stringVector ss)
        , env (pure ints1000) $ \xs ->
            bench "int64[1000] vector" $ whnfIO (int64Vector xs)
        , bench "structs[1000] vector" $ whnfIO (recordBatchMessage 1000 0)
        , env (pure (BS.replicate 65536 0x5a)) $ \p ->
            bench "64 KiB blob" $ whnfIO (blobTable p)
        ]
    ]
  where
    strings1000 = [T.pack ("string number " <> show i) | i <- [1 :: Int .. 1000]]
    ints1000 = [fromIntegral i * 7919 | i <- [1 :: Int .. 1000]] :: [Int64]

