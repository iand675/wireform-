{-# LANGUAGE BangPatterns #-}

module WriterBounds (run) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (unless)
import Data.Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as P
import Data.Word (Word32, Word64)
import GHC.Conc (getAllocationCounter)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.LevelsEncode qualified as Levels
import Parquet.Encryption qualified as Encryption
import Parquet.Page qualified as Page
import Parquet.Read qualified as Read
import Parquet.Types (Compression (GZip, Uncompressed), ParquetType (PTByteArray), Repetition (Optional, Required), SchemaElement (..))
import Parquet.Write qualified as Write


check :: String -> Bool -> IO ()
check message ok = unless ok (fail ("WriterBounds: " ++ message))


rejects :: String -> a -> IO ()
rejects message value = do
  result <- try (evaluate value >> pure ()) :: IO (Either ErrorCall ())
  check message (case result of Left err -> message `isInfixOf` show err; Right () -> False)


run :: IO ()
run = do
  dictionaryBounds
  payloadBounds
  encryptionFramingBounds
  hybridBounds
  mixedHybridBounds
  hybridAllocation


encryptionFramingBounds :: IO ()
encryptionFramingBounds = do
  let key = BS.replicate 16 0x42
      nonce = BS.replicate 12 0x11
      -- The backing buffer is one byte: rejection must inspect only its
      -- declared length, before attempting encryption or copying payload.
      oversized overhead =
        let (fp, offset, _) = BSI.toForeignPtr (BS.singleton 0)
            limit = min (toInteger (maxBound :: Word32) - overhead) (toInteger (maxBound :: Int) - overhead - 4)
        in BSI.fromForeignPtr fp offset (fromInteger (limit + 1))
  check "explicit GCM rejects an unrepresentable frame before payload access" (isLeft (Encryption.encryptGcmModuleFramedWithNonce key nonce BS.empty (oversized 28)))
  check "explicit CTR rejects an unrepresentable frame before payload access" (isLeft (Encryption.encryptCtrModuleFramedWithNonce key nonce (oversized 12)))
  gcm <- Encryption.encryptGcmModuleFramed key BS.empty (oversized 28)
  ctr <- Encryption.encryptCtrModuleFramed key (oversized 12)
  check "random-nonce GCM rejects an unrepresentable frame" (isLeft gcm)
  check "random-nonce CTR rejects an unrepresentable frame" (isLeft ctr)


dictionaryBounds :: IO ()
dictionaryBounds = do
  let page entries indices = Write.encodeDictDataPage (Write.Dictionary (Write.ColInt32 (P.fromList entries)) (P.fromList indices))
  mapM_
    (\indices -> rejects "exceeds maximum" (BS.length (page [10, 20, 30] indices)))
    [[-1], [3], [7], [0, 1, 3], [0, maxBound]]
  rejects "nonempty indices require a nonempty dictionary" (BS.length (page [] [0]))
  mapM_
    ( \(entries, indices) -> do
        let encoded = page entries indices
        (header, offset) <- either fail pure (Page.readPageHeaderAt encoded 0)
        check "valid dictionary indices survive page encoding" (Read.decodeDictionaryIndices (length indices) (BS.drop offset encoded) == Right (P.fromList indices))
        check "dictionary header has the checked row count" (case Page.phType header of Page.PtDataPage h -> Page.dphNumValues h == fromIntegral (length indices); _ -> False)
    )
    [([], []), ([10], []), ([10], [0, 0]), ([10, 20, 30], [2, 0, 2])]


payloadBounds :: IO ()
payloadBounds = do
  -- A shared 64 KiB source and 256 KiB of pointers request exactly 2 GiB
  -- of PLAIN data. Failure must precede copying any of that output.
  let bytes = BS.replicate 65532 120
      entries = V.replicate 32768 bytes
      column = Write.ColByteArray entries
      optional = Write.OptByteArray (V.map Just entries)
      dictionary = Write.Dictionary column P.empty
  _ <- evaluate (BS.length bytes + V.length entries)
  before <- getAllocationCounter
  rejects "PLAIN page payload exceeds signed 32-bit limit" (BS.length (Write.encodeColumnDataPage column))
  rejects "PLAIN page payload exceeds signed 32-bit limit" (BS.length (Write.encodeDictPage dictionary))
  rejects "PLAIN page payload exceeds signed 32-bit limit" (BS.length (Write.encodeOptionalColumnPage optional))
  check "required V2 returns its payload size failure" (case Write.encodeColumnDataPageV2 Uncompressed column of Left err -> "signed 32-bit limit" `isInfixOf` err; _ -> False)
  check "optional V2 returns its payload size failure" (case Write.encodeOptionalColumnPageV2 Uncompressed optional of Left err -> "signed 32-bit limit" `isInfixOf` err; _ -> False)
  after <- getAllocationCounter
  check "oversized writer payloads fail without a large allocation" (before - after < 4 * 1024 * 1024)
  let poisoned = V.snoc entries (error "inspected beyond overflowing payload")
  rejects "PLAIN page payload exceeds signed 32-bit limit" (BS.length (Write.encodeColumnDataPage (Write.ColByteArray poisoned)))
  -- The values alone fit. Their definition levels push both V1 and V2
  -- over the page limit, so neither may materialize the near-2-GiB values.
  let almost = Write.OptByteArray (V.map Just (V.snoc (V.take 32767 entries) (BS.replicate 65529 120)))
  rejects "page payload exceeds signed 32-bit limit" (BS.length (Write.encodeOptionalColumnPage almost))
  check "V2 includes level bytes in its size preflight" (case Write.encodeOptionalColumnPageV2 Uncompressed almost of Left err -> "page payload exceeds signed 32-bit limit" `isInfixOf` err; _ -> False)
  mapM_
    ( \version ->
        mapM_
          ( \codec ->
              mapM_
                ( \nullable -> do
                    let schema =
                          V.fromList
                            [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                            , SchemaElement "x" (Just (if nullable then Optional else Required)) (Just PTByteArray) Nothing Nothing Nothing Nothing Nothing
                            ]
                        values = if nullable then Write.PCOptional optional else Write.PCRequired column
                        options = Write.emptyColumnAux {Write.caCodec = codec, Write.caPageVersion = version}
                        result = Write.buildParquetFileMixedWithAux schema (V.singleton (V.singleton values)) (V.singleton (V.singleton options))
                    check "checked mixed writers return size errors rather than throwing" (case result of Left err -> "signed 32-bit limit" `isInfixOf` err; _ -> False)
                )
                [False, True]
          )
          [Uncompressed, GZip]
    )
    [Write.PageV1, Write.PageV2]


hybridBounds :: IO ()
hybridBounds = do
  passed <- H.check $ H.withTests 300 $ H.property $ do
    width <- H.forAll (Gen.int (Range.linear 0 32))
    count <- H.forAll (Gen.int (Range.linear 0 257))
    prefix <- H.forAll (Gen.int (Range.linear 1 17))
    words32 <- H.forAll (Gen.list (Range.singleton count) (Gen.word32 Range.constantBounded))
    let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Word32
        values = P.fromList (map (fromIntegral . (.&. mask)) words32)
        sliced = P.slice prefix count (P.replicate prefix (-1) P.++ values P.++ P.singleton (-1))
        encoded = Levels.encodeRLEHybrid width sliced
        prefixed = Levels.encodeLengthPrefixedHybrid width sliced
    plan <- H.evalEither (Levels.planRLEHybrid width mask sliced)
    encoded H.=== referenceHybrid width values
    Levels.hybridEncodedSize plan H.=== BS.length encoded
    Levels.encodeHybridPlan plan H.=== encoded
    Read.decodeDictionaryIndices count (BS.cons (fromIntegral width) encoded) H.=== Right values
    prefixed H.=== if width == 0 then BS.empty else littleEndian32 (fromIntegral (BS.length encoded)) <> encoded
  check "hybrid streams match independent bit-position packing on sliced inputs" passed
  -- Include all widths deterministically, and exercise high unsigned bits
  -- stored in Int32, constant streams, padding boundaries and empty input.
  mapM_
    ( \width -> do
        let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Word32
        mapM_
          ( \count ->
              mapM_
                (\values -> check "all widths and padding counts match independent bytes" (Levels.encodeRLEHybrid width values == referenceHybrid width values))
                [P.replicate count (fromIntegral mask), P.generate count (\i -> fromIntegral (fromIntegral i .&. mask))]
          )
          [0, 1, 2, 7, 8, 9, 15, 16, 17, 127, 128, 129]
    )
    [0 .. 32]
  mapM_
    ( \width -> do
        check "empty input still validates width" (case Levels.planRLEHybrid width 0 P.empty of Left _ -> True; _ -> False)
        rejects "bit width out of range" (BS.length (Levels.encodeRLEHybrid width P.empty))
    )
    [-1, 33, maxBound]
  rejects "negative maximum value" (Levels.bitWidthFor (-1))
  rejects "exceeds maximum" (BS.length (Levels.encodeRLEHybrid 0 (P.singleton 1)))
  rejects "exceeds maximum" (BS.length (Levels.encodeRLEHybrid 31 (P.singleton (-1))))
  check "semantic maximum is checked independently of width" (case Levels.planRLEHybrid 2 2 (P.singleton 3) of Left _ -> True; _ -> False)
  check "maximum must fit declared width" (case Levels.planRLEHybrid 1 2 P.empty of Left _ -> True; _ -> False)
  check "a semantic bound is not treated as a contiguous bit mask" (case Levels.planRLEHybrid 4 10 (P.fromList [10, 7, 10, 7]) of Right _ -> True; _ -> False)
  mapM_
    ( \width ->
        mapM_
          ( \prefix ->
              mapM_
                ( \count ->
                    mapM_
                      ( \row -> do
                          let bad = fromIntegral ((1 :: Word32) `shiftL` width)
                              values = P.replicate count 0 P.// [(row, bad)]
                              sliced = P.slice prefix count (P.replicate prefix (-1) P.++ values P.++ P.singleton (-1))
                          check "paired range checks inspect every logical lane and report its row" (case Levels.planRLEHybrid width (fromIntegral (((1 :: Word64) `shiftL` width) - 1)) sliced of Left err -> ("row " ++ show row ++ " exceeds") `isInfixOf` err; _ -> False)
                      )
                      [0 .. count - 1]
                )
                [1 .. 33]
          )
          [0 .. 3]
    )
    [1, 7, 8, 9, 16, 31]
  check "repeated plans reject a negative count" (case Levels.planRepeatedRLEHybrid 1 (-1) 0 of Left _ -> True; _ -> False)
  check "repeated plans reject an oversized value" (case Levels.planRepeatedRLEHybrid 1 1 2 of Left _ -> True; _ -> False)
  mapM_
    ( \count ->
        mapM_
          ( \width -> do
              let value = if width == 0 then 0 else (1 :: Word32) `shiftL` (width - 1)
              plan <- either fail pure (Levels.planRepeatedRLEHybrid width count value)
              let encoded = Levels.encodeHybridPlan plan
              check "large repeated plans split into legal runs" (encoded == referenceRepeated width count value)
              check "large repeated plans size their complete output" (BS.length encoded == Levels.hybridEncodedSize plan)
          )
          [0, 1, 7, 8, 9, 31, 32]
    )
    [0, 1, 63, 64, 127, 128, 2147483646, 2147483647, 2147483648, 4294967295]


-- The independent oracle addresses every output bit by its absolute
-- position, rather than sharing the encoder's accumulator/flush loop.
referenceHybrid :: Int -> P.Vector Int32 -> ByteString
referenceHybrid width values
  | P.null values = BS.empty
  | P.all (== P.head values) values = referenceRepeated width (P.length values) (fromIntegral (P.head values))
  | otherwise = split (P.toList values) []
  where
    minimumCount = if width == 1 then 16 else 8
    flush = referencePacked width . P.fromList . reverse
    split [] pending = flush pending
    split remaining@(first : _) pending =
      let (same, rest) = span (== first) remaining
      in if length same >= minimumCount || (null rest && (width /= 1 || length same >= 9))
           then flush pending <> referenceRepeated width (length same) (fromIntegral first) <> split rest []
           else let (group, tailValues) = splitAt 8 remaining in split tailValues (reverse group ++ pending)


referencePacked :: Int -> P.Vector Int32 -> ByteString
referencePacked width values
  | P.null values = BS.empty
  | otherwise =
      let groups = (P.length values + 7) `quot` 8
          byteAt byte = foldl bitAt 0 [0 .. 7]
            where
              bitAt acc bit =
                let (row, valueBit) = (byte * 8 + bit) `quotRem` width
                in if row < P.length values && testBit (values P.! row) valueBit then setBit acc bit else acc
      in unsignedVarint (fromIntegral (groups * 2 + 1)) <> BS.pack (map byteAt [0 .. groups * width - 1])


referenceRepeated :: Int -> Int -> Word32 -> ByteString
referenceRepeated width count value
  | count == 0 = BS.empty
  | otherwise =
      let here = min count 2147483647
          bytes = (width + 7) `quot` 8
      in unsignedVarint (fromIntegral here * 2)
           <> BS.pack (map (\byte -> fromIntegral (value `shiftR` (byte * 8))) [0 .. bytes - 1])
           <> referenceRepeated width (count - here) value


unsignedVarint :: Word64 -> ByteString
unsignedVarint value
  | value < 128 = BS.singleton (fromIntegral value)
  | otherwise = BS.cons (fromIntegral (value .&. 127) .|. 128) (unsignedVarint (value `shiftR` 7))


littleEndian32 :: Word32 -> ByteString
littleEndian32 value = BS.pack (map (\shift -> fromIntegral (value `shiftR` shift)) [0, 8, 16, 24])


hybridAllocation :: IO ()
hybridAllocation = do
  let values = P.generate 65536 (\i -> fromIntegral (i * 1664525 + 1013904223))
  _ <- evaluate (P.foldl' (+) 0 values)
  before <- getAllocationCounter
  size <- evaluate (BS.length (Levels.encodeRLEHybrid 32 values))
  after <- getAllocationCounter
  check "packed hybrid allocation stays near its final payload" (before - after < fromIntegral size + 8192)
  mapM_
    ( \width ->
        mapM_
          ( \raw -> do
              let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Int32
                  mixed = P.map (.&. mask) raw
              _ <- evaluate (P.foldl' (+) 0 mixed)
              beforeMixed <- getAllocationCounter
              mixedSize <- evaluate (BS.length (Levels.encodeRLEHybrid width mixed))
              afterMixed <- getAllocationCounter
              check "mixed hybrid runs do not allocate segment lists or per-group closures" (beforeMixed - afterMixed < fromIntegral mixedSize + 8192)
          )
          ( map (\block -> P.generate 65536 (\i -> fromIntegral (i `quot` block))) [8, 64]
              ++ [P.generate 65536 (\i -> fromIntegral (if i `rem` 16 < 8 then i else i `quot` 16))]
          )
    )
    [1, 2, 9, 31, 32]


mixedHybridBounds :: IO ()
mixedHybridBounds = do
  mapM_
    ( \width -> do
        let minimumCount = if width == 1 then 16 else 8
            middleMismatch = P.generate minimumCount (\i -> if i == minimumCount `quot` 2 then 1 else 0)
            lastMismatch = P.generate minimumCount (\i -> if i == minimumCount - 1 then 1 else 0)
        mapM_
          ( \prefix -> do
              let values = prefix P.++ P.replicate 64 1 P.++ P.fromList [0, 1, 0]
                  encoded = Levels.encodeRLEHybrid width values
              check "run lookahead checks matching endpoints against the full interior" (encoded == referenceHybrid width values)
              check "run lookahead preserves values and short final tails" (Read.decodeDictionaryIndices (P.length values) (BS.cons (fromIntegral width) encoded) == Right values)
          )
          [middleMismatch, lastMismatch]
    )
    [1 .. 32]
  passed <- H.check $ H.withTests 300 $ H.property $ do
    width <- H.forAll (Gen.int (Range.linear 1 32))
    pieces <- H.forAll (Gen.list (Range.linear 0 32) (Gen.either (Gen.word32 Range.constantBounded) (Gen.word32 Range.constantBounded)))
    runLengths <- H.forAll (Gen.list (Range.singleton (length pieces)) (Gen.int (Range.linear 1 96)))
    prefix <- H.forAll (Gen.int (Range.linear 0 23))
    let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Word32
        chunk piece count = case piece of
          Left value -> replicate count (fromIntegral (value .&. mask))
          Right value -> map (\i -> fromIntegral ((value + fromIntegral i * 1664525) .&. mask)) [0 .. count - 1]
        values = P.fromList (concat (zipWith chunk pieces runLengths))
        sliced = P.slice prefix (P.length values) (P.replicate prefix (-1) P.++ values P.++ P.singleton (-1))
        encoded = Levels.encodeRLEHybrid width sliced
    plan <- H.evalEither (Levels.planRLEHybrid width mask sliced)
    encoded H.=== referenceHybrid width values
    Levels.hybridEncodedSize plan H.=== BS.length encoded
    Read.decodeDictionaryIndices (P.length values) (BS.cons (fromIntegral width) encoded) H.=== Right values
  check "mixed streams preserve arbitrary run lengths, slices and transitions" passed
  mapM_
    ( \width ->
        mapM_
          ( \prefix ->
              mapM_
                ( \count -> do
                    let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Word32
                        values =
                          P.fromList
                            ( map (\i -> fromIntegral (fromIntegral i .&. mask)) [0 .. prefix - 1]
                                ++ replicate count (fromIntegral mask)
                                ++ [0, fromIntegral mask, 0]
                                ++ replicate (count + 1) (fromIntegral mask)
                            )
                        encoded = Levels.encodeRLEHybrid width values
                    check "all mixed-run boundaries match independent bytes" (encoded == referenceHybrid width values)
                    check "interior packed padding never inserts extra values" (Read.decodeDictionaryIndices (P.length values) (BS.cons (fromIntegral width) encoded) == Right values)
                )
                [7, 8, 9, 15, 16, 17, 23, 24, 31, 32, 33, 63, 64, 65]
          )
          [0 .. 15]
    )
    [1 .. 32]
  mapM_
    ( \width -> do
        let mask = fromIntegral (((1 :: Word64) `shiftL` width) - 1) :: Int32
            values = P.generate 65536 (\i -> fromIntegral (i `quot` 64) .&. mask)
            encoded = Levels.encodeRLEHybrid width values
            valueBytes = (width + 7) `quot` 8
        check "sixty-four-value blocks use one compact repeated run each" (BS.length encoded == 1024 * (2 + valueBytes))
    )
    [1, 8, 16, 24, 32]
