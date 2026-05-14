module Test.StreamCodec (streamCodecTests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto (MessageDecode (..))
import Proto.Decode.Stream (
  IDecode (..),
  decodeMessageIncremental,
  decodeMessageLazy,
  decodeMessageStream,
 )
import Proto (MessageEncode (..), encodeMessage, encodeMessageLazy, encodeMessageStream, encodeMessageStreamSized)
import Proto.Internal.Encode (fieldBool, fieldString, fieldVarint)
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode (DecodeError (..), Decoder, getTagOr, getText, getVarint, skipField)
import Proto.Internal.Wire.Encode (putVarint)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Wireform.Builder qualified as B


streamCodecTests :: TestTree
streamCodecTests =
  testGroup
    "Streaming & Lazy Codecs"
    [ lazyEncodeTests
    , lazyDecodeTests
    , streamRoundtripTests
    , incrementalDecodeTests
    ]


-- -----------------------------------------------------------------------
-- Lazy single-message encoding
-- -----------------------------------------------------------------------

lazyEncodeTests :: TestTree
lazyEncodeTests =
  testGroup
    "Lazy encoding"
    [ testProperty "encodeMessageLazy matches strict" $ property $ do
        msg <- genSMsg
        BL.toStrict (encodeMessageLazy msg) === encodeMessage msg
    , testProperty "encodeMessageLazy matches encodeMessageLazy" $ property $ do
        msg <- genSMsg
        encodeMessageLazy msg === encodeMessageLazy msg
    ]


-- -----------------------------------------------------------------------
-- Lazy single-message decoding
-- -----------------------------------------------------------------------

lazyDecodeTests :: TestTree
lazyDecodeTests =
  testGroup
    "Lazy decoding"
    [ testProperty "decodeMessageLazy roundtrip" $ property $ do
        msg <- genSMsg
        let lbs = encodeMessageLazy msg
        decodeMessageLazy lbs === Right msg
    , testCase "decodeMessageLazy empty" $ do
        let lbs = BL.empty
        decodeMessageLazy lbs @?= Right (SMsg 0 "" False)
    , testCase "decodeMessageLazy multi-chunk" $ do
        let strict = encodeMessage (SMsg 42 "hello" True)
            (a, b) = BS.splitAt (BS.length strict `div` 2) strict
            lbs = BL.fromChunks [a, b]
        decodeMessageLazy lbs @?= Right (SMsg 42 "hello" True)
    ]


-- -----------------------------------------------------------------------
-- Stream framing roundtrip
-- -----------------------------------------------------------------------

streamRoundtripTests :: TestTree
streamRoundtripTests =
  testGroup
    "Stream framing roundtrip"
    [ testProperty "stream roundtrip (no size)" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        let encoded = encodeMessageStream msgs
            decoded = decodeMessageStream encoded
        fmap fromRight' decoded === msgs
    , testProperty "stream roundtrip (sized)" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        let encoded = encodeMessageStreamSized msgs
            decoded = decodeMessageStream encoded
        fmap fromRight' decoded === msgs
    , testProperty "sized stream matches non-sized stream" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        encodeMessageStreamSized msgs === encodeMessageStream msgs
    , testCase "empty stream" $ do
        let encoded = encodeMessageStream ([] :: [SMsg])
        BL.null encoded @?= True
        decodeMessageStream @SMsg encoded @?= []
    , testCase "single-message stream" $ do
        let msg = SMsg 99 "solo" True
            encoded = encodeMessageStream [msg]
            decoded = decodeMessageStream encoded
        decoded @?= [Right msg]
    , testCase "stream decode truncated length" $ do
        let lbs = BL.pack [0x80]
        case decodeMessageStream @SMsg lbs of
          [Left UnexpectedEnd] -> pure ()
          other -> assertFailure ("Expected [Left UnexpectedEnd], got: " <> show other)
    , testCase "stream decode truncated payload" $ do
        let lbs = BL.pack [0x0A, 0x01]
        case decodeMessageStream @SMsg lbs of
          [Left UnexpectedEnd] -> pure ()
          other -> assertFailure ("Expected [Left UnexpectedEnd], got: " <> show other)
    , testProperty "stream framing matches manual framing" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 1 10) genSMsg'
        let autoFramed = encodeMessageStream msgs
            manualFramed = BL.fromChunks $ do
              msg <- msgs
              let payload = encodeMessage msg
              pure $ buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)
        autoFramed === BL.fromStrict (BL.toStrict manualFramed)
    ]


-- -----------------------------------------------------------------------
-- Incremental decoder
-- -----------------------------------------------------------------------

incrementalDecodeTests :: TestTree
incrementalDecodeTests =
  testGroup
    "Incremental decoder"
    [ testProperty "single message fed all at once" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded === msg
            BS.null leftover === True
          other -> do
            annotate (show other)
            failure
    , testProperty "single message fed byte-by-byte" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
            chunks = fmap BS.singleton (BS.unpack framed)
        case feedAll decodeMessageIncremental chunks of
          IDone decoded leftover -> do
            decoded === msg
            BS.null leftover === True
          other -> do
            annotate (show other)
            failure
    , testProperty "preserves leftover bytes" $ property $ do
        msg <- genSMsg
        extra <- forAll $ Gen.bytes (Range.linear 1 50)
        let framed = frameMessage msg <> extra
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded === msg
            leftover === extra
          other -> do
            annotate (show other)
            failure
    , testProperty "two messages fed together" $ property $ do
        msg1 <- genSMsg
        msg2 <- genSMsg
        let framed = frameMessage msg1 <> frameMessage msg2
        case feed decodeMessageIncremental framed of
          IDone decoded1 leftover1 -> do
            decoded1 === msg1
            case feed decodeMessageIncremental leftover1 of
              IDone decoded2 leftover2 -> do
                decoded2 === msg2
                BS.null leftover2 === True
              other -> do
                annotate ("second: " <> show other)
                failure
          other -> do
            annotate ("first: " <> show other)
            failure
    , testProperty "split at arbitrary byte boundary" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
        splitPos <- forAll $ Gen.int (Range.linear 0 (BS.length framed))
        let (chunk1, chunk2) = BS.splitAt splitPos framed
            dec0 = feed decodeMessageIncremental chunk1
        case dec0 of
          IDone decoded _ -> decoded === msg
          IPartial _ ->
            case feed dec0 chunk2 of
              IDone decoded leftover -> do
                decoded === msg
                BS.null leftover === True
              other -> do
                annotate (show other)
                failure
          other -> do
            annotate (show other)
            failure
    , testCase "empty message" $ do
        let framed = BS.singleton 0
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded @?= SMsg 0 "" False
            BS.null leftover @?= True
          other -> assertFailure (show other)
    , testCase "Nothing at start yields IFail" $ do
        case decodeMessageIncremental @SMsg of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> assertFailure ("Expected IFail UnexpectedEnd, got: " <> show other)
          other -> assertFailure ("Expected IPartial, got: " <> show other)
    , testCase "truncated varint" $ do
        let chunk = BS.pack [0x80, 0x80]
        case feed decodeMessageIncremental chunk of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> assertFailure ("Expected IFail UnexpectedEnd, got: " <> show (other :: IDecode SMsg))
          other -> assertFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))
    , testCase "truncated payload" $ do
        let chunk = BS.pack [0x0A, 0x01]
        case feed decodeMessageIncremental chunk of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> assertFailure ("Expected IFail, got: " <> show (other :: IDecode SMsg))
          other -> assertFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))
    ]


-- -----------------------------------------------------------------------
-- Incremental encoder
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- Test message type
-- -----------------------------------------------------------------------

data SMsg = SMsg
  { smValue :: {-# UNPACK #-} !Word64
  , smName :: !Text
  , smActive :: !Bool
  }
  deriving stock (Show, Eq)


instance MessageEncode SMsg where
  buildSized msg =
    (if smValue msg /= 0 then fieldVarint 1 (smValue msg) else mempty)
      <> (if smName msg /= "" then fieldString 2 (smName msg) else mempty)
      <> (if smActive msg then fieldBool 3 (smActive msg) else mempty)


instance MessageDecode SMsg where
  messageDecoder = loop 0 "" False
    where
      loop :: Word64 -> Text -> Bool -> Decoder SMsg
      loop !val !name !active = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (SMsg val name active)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v name active
            2 -> getText >>= \v -> loop val v active
            3 -> getVarint >>= \v -> loop val name (v /= 0)
            _ -> skipField wt >> loop val name active


genSMsg :: PropertyT IO SMsg
genSMsg = do
  v <- forAll $ Gen.word64 (Range.linear 0 1000000)
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
  b <- forAll Gen.bool
  pure (SMsg v t b)


genSMsg' :: Gen SMsg
genSMsg' =
  SMsg
    <$> Gen.word64 (Range.linear 0 1000000)
    <*> Gen.text (Range.linear 0 100) Gen.alphaNum
    <*> Gen.bool


fromRight' :: Either DecodeError a -> a
fromRight' (Right a) = a
fromRight' (Left e) = error ("unexpected decode error: " <> show e)


buildToBS :: B.Builder -> BS.ByteString
buildToBS = BL.toStrict . B.toLazyByteString


frameMessage :: (MessageEncode a) => a -> BS.ByteString
frameMessage msg =
  let payload = encodeMessage msg
  in buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)


feed :: IDecode a -> BS.ByteString -> IDecode a
feed (IPartial k) bs = k (Just bs)
feed done _ = done


feedAll :: IDecode a -> [BS.ByteString] -> IDecode a
feedAll dec [] = case dec of
  IPartial k -> k Nothing
  other -> other
feedAll dec (c : cs) = case dec of
  IPartial k -> feedAll (k (Just c)) cs
  other -> other
