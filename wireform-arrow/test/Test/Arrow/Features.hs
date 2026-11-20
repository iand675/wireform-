{-# LANGUAGE OverloadedStrings #-}

module Test.Arrow.Features (tests) where

import Arrow.Column qualified as C
import Arrow.FlatBufferIPC qualified as IPC
import Arrow.Stream qualified as S
import Arrow.Types
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import FlatBuffers.Builder qualified as B
import FlatBuffers.Reader qualified as R


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check name condition = unless condition (fail ("Arrow feature regression: " ++ name))


tests :: IO ()
tests = do
  let field = defaultLeafField "n" False (AInt 32 True)
      column = C.primColumn C.PInt32 (VS.fromList [-123, 0, 456])
      allFlags = [FeatureUnused, FeatureDictionaryReplacement, FeatureCompressedBody]
      combinations = map V.fromList [[], [FeatureUnused], [FeatureDictionaryReplacement], [FeatureCompressedBody], allFlags, reverse allFlags, [FeatureCompressedBody, FeatureDictionaryReplacement, FeatureCompressedBody]]
      options = S.defaultWriteOptions : map (\codec -> S.defaultWriteOptions {S.writeBodyCompression = Just codec}) (filter S.bodyCompressionAvailable [BodyZstd, LZ4Frame])
  forM_ combinations $ \flags -> do
    let schema = (defaultSchema (V.singleton field)) {arrowFeatures = flags}
        metadata = IPC.buildSchemaMessage schema
    decoded <- load (IPC.decodeSchemaMessage metadata)
    check "metadata retains flags, order and duplicates" (decoded == schema)
    codes <- featureCodes metadata
    check "wire values are signed64 enum constants in Schema slot3" (codes == V.map code flags)
    forM_ options $ \opts -> do
      let batch = [V.singleton column]
          encoders =
            [ S.encodeArrowStream opts schema batch
            , BL.toStrict <$> S.encodeArrowStreamLazy opts schema batch
            , S.encodeArrowFile opts schema batch
            , BL.toStrict <$> S.encodeArrowFileLazy opts schema batch
            ]
      forM_ (zip [False, False, True, True] encoders) $ \(file, encoded) -> do
        bytes <- load encoded
        (actualSchema, actualBatches) <- load ((if file then S.decodeArrowFile else S.decodeArrowStream) S.defaultReadOptions bytes)
        check "flags and values survive strict/lazy stream/file and compression" (actualSchema == schema && actualBatches == batch)
        if file
          then do
            index <- load (IPC.readFileIndex bytes)
            check "footer schema retains feature flags" (IPC.fiSchema index == schema)
            reader <- load (S.openFileReader S.defaultReadOptions bytes)
            check "file iterator schema retains feature flags" (S.fileReaderSchema reader == schema)
          else do
            reader <- load (S.openStreamReader S.defaultReadOptions bytes)
            check "stream iterator schema retains feature flags" (S.streamReaderSchema reader == schema)
  forM_ [([], V.empty), ([0, 1, 2], V.fromList allFlags), ([2, 1, 2], V.fromList [FeatureCompressedBody, FeatureDictionaryReplacement, FeatureCompressedBody])] $ \(codes, flags) -> do
    raw <- rawFeatureMessage codes
    schema <- load (IPC.decodeSchemaMessage raw)
    check "independent raw int64 vector is interpreted using feature wire values" (arrowFeatures schema == flags)
  forM_ [-1, 3, 4, 256, minBound, maxBound] $ \unknown -> do
    raw <- rawFeatureMessage [1, unknown, 2]
    check "unknown feature cannot silently lose semantics" (isLeft (IPC.decodeSchemaMessage raw))
  raw <- rawFeatureMessage [0, 1, 2]
  vec <- featureVector raw
  forM_ [0x7fffffff, 0xffffffff] $ \count -> do
    let malformed = BS.take vec raw <> BL.toStrict (BB.toLazyByteString (BB.word32LE count)) <> BS.drop (vec + 4) raw
    check "feature count is bounded before allocation" (isLeft (IPC.decodeSchemaMessage malformed))
  check "truncated feature payload is rejected" (isLeft (IPC.decodeSchemaMessage (BS.take (vec + 4 + 23) raw)))
  putStrLn "Arrow schema feature wire/round-trip/unknown/bounds regressions passed"
  where
    code :: Feature -> Int64
    code FeatureUnused = 0
    code FeatureDictionaryReplacement = 1
    code FeatureCompressedBody = 2


-- Independent table construction keeps the test's expected wire values out
-- of the production feature encoder, including values it cannot represent.
rawFeatureMessage :: [Int64] -> IO BS.ByteString
rawFeatureMessage values = do
  b <- B.newBuilder
  features <- B.writeVectorInt64 b values
  schema <- B.writeTable b [Nothing, Nothing, Nothing, Just (B.voff features)]
  message <- B.writeTable b [Just (B.scalarI16 4), Just (B.scalarU8 1), Just (B.voff schema)]
  B.finish b message


featureVector :: BS.ByteString -> IO Int
featureVector bytes = load $ do
  root <- fromIntegral <$> R.peekU32 bytes 0
  message <- R.resolveTable bytes root
  header <- maybe (Left "missing schema header") (R.followUOffset bytes) (message 2)
  schema <- R.resolveTable bytes header
  maybe (Left "missing feature vector") (R.followUOffset bytes) (schema 3)


featureCodes :: BS.ByteString -> IO (V.Vector Int64)
featureCodes bytes = do
  -- Absence is the default empty vector; non-empty vectors must use slot3.
  root <- load (fromIntegral <$> R.peekU32 bytes 0)
  message <- load (R.resolveTable bytes root)
  header <- load (maybe (Left "missing schema header") (R.followUOffset bytes) (message 2))
  schema <- load (R.resolveTable bytes header)
  case schema 3 of
    Nothing -> pure V.empty
    Just p -> load $ do
      vec <- R.followUOffset bytes p
      (_, elems) <- R.readVectorOfStructs bytes vec 8
      V.mapM (R.peekI64 bytes) elems
