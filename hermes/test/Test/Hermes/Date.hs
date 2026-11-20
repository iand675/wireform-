module Test.Hermes.Date (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers
import Network.HTTP.Headers.AcceptDatetime (AcceptDatetime (..))
import Network.HTTP.Headers.Age (Age (..))
import Network.HTTP.Headers.CacheControl (CacheControl (..), CacheControlDirective (..))
import Network.HTTP.Headers.Date (Date (..))
import Network.HTTP.Headers.Expires (Expires (..))
import Network.HTTP.Headers.HeaderFieldName (hDate, hRetryAfter)
import Network.HTTP.Headers.IfModifiedSince (IfModifiedSince (..))
import Network.HTTP.Headers.IfRange (IfRange (..))
import Network.HTTP.Headers.IfUnmodifiedSince (IfUnmodifiedSince (..))
import Network.HTTP.Headers.LastModified (LastModified (..))
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.MementoDatetime (MementoDatetime (..))
import Network.HTTP.Headers.RepeatabilityFirstSent (RepeatabilityFirstSent (..))
import Network.HTTP.Headers.RetryAfter (RetryAfter (..), largeDelayDigits, parseRetryAfterDelay, parseRetryAfterDelayWithin)
import Network.HTTP.Headers.SetCookie (SetCookie (..), SetCookies (..), parseSetCookieField)
import Network.HTTP.Headers.Sunset (Sunset (..))
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests = describe "HTTP dates and delays" $ do
  it "reads all three HTTP-date forms through typed header lookup" $
    mapM_
      (\value -> lookupDate value `shouldBe` Right (Just exampleDate))
      [ "Sun, 06 Nov 1994 08:49:37 GMT"
      , "Sunday, 06-Nov-94 08:49:37 GMT"
      , "Sun Nov  6 08:49:37 1994"
      ]
  it "folds only the ASCII date names and GMT when requested" $ do
    let settings =
          withHeaderDateReference (utc 2020 1 1 5) $
            withHeaderMessage HeaderResponse (withCaseInsensitiveDates defaultHeaderSettings)
        lookupAt value = lookupHeaderWith @Date settings (headerMapFromList [("Date", value)])
        folded = "sUN, 06 nOv 1994 08:49:37 gMt"
    forM_ [folded, "suNDaY, 06-nOV-94 08:49:37 GmT", "SUN nov  6 08:49:37 1994"] $ \value -> do
      lookupAt value `shouldBe` Right (Just exampleDate)
      lookupDate value `shouldSatisfy` isLeft
    lookupAt "wEDNESDAY, 01-jAN-70 00:00:05 gmT" `shouldBe` Right (Just (Date (utc 2070 1 1 5)))
    parseFromHeaders settings (folded :| []) `shouldBe` Right (Expires (date exampleDate))
    parseFromHeaders settings (folded :| []) `shouldBe` Right (LastModified (date exampleDate))
    renderToHeaders settings exampleDate `shouldBe` exampleWireDate
    forM_
      [ "mON, 06 nOv 1994 08:49:37 gMt"
      , "sUN, 06 nOv 1994 08:49:37 est"
      , "sUN, 06 nOv 1994 08:49:37 gmt extra"
      , "sUN, 06 nOv 1994 08:49:61 gmt"
      ]
      (\value -> lookupAt value `shouldSatisfy` isLeft)
  it "renders a zero-padded UTC day in canonical HTTP-date format" $
    lookupRawHeader hDate (setHeader exampleDate (headerMapFromList []))
      `shouldBe` Just ("Sun, 06 Nov 1994 08:49:37 GMT" :| [])
  it "truncates fractional seconds instead of carrying them into the minute" $ do
    let almostNoon = Date (addUTCTime 43_199.9 (utc 1994 11 6 0))
    renderToHeaders defaultHeaderSettings almostNoon `shouldBe` "Sun, 06 Nov 1994 11:59:59 GMT"
  it "round trips the leap-second wire value" $ do
    let leapSecond = Date (utc 2016 12 31 86_400)
    lookupDate "Sat, 31 Dec 2016 23:59:60 GMT" `shouldBe` Right (Just leapSecond)
    renderToHeaders defaultHeaderSettings leapSecond `shouldBe` "Sat, 31 Dec 2016 23:59:60 GMT"
  it "rejects invalid dates, clocks, and extra input" $
    mapM_
      (\value -> lookupDate value `shouldSatisfy` isLeft)
      [ "Tue, 31 Feb 2024 12:00:00 GMT"
      , "Thu, 29 Feb 1900 12:00:00 GMT"
      , "Sun, 06 Nov 1898 08:49:37 GMT"
      , "Mon, 06 Nov 1994 08:49:37 GMT"
      , "Sun, 06 Nov 1994 25:00:00 GMT"
      , "Sun, 06 Nov 1994 08:60:00 GMT"
      , "Sun, 06 Nov 1994 08:49:60 GMT"
      , "Sun, 06 Nov 1994 23:59:61 GMT"
      , "Sun, 6 Nov 1994 08:49:37 GMT"
      , "Sun Nov 6 08:49:37 1994"
      , "Sun, 06 Nov 1994 08:49:37 GMT extra"
      ]
  it "accepts field OWS and rejects multiple singleton date fields" $ do
    lookupDate "\t Sun, 06 Nov 1994 08:49:37 GMT \t" `shouldBe` Right (Just exampleDate)
    lookupHeader @IfModifiedSince
      (headerMapFromList [("If-Modified-Since", exampleWireDate), ("If-Modified-Since", exampleWireDate)])
      `shouldSatisfy` isLeft
  it "uses the full RFC 850 cutoff with an explicit reference clock" $ do
    let settings = withHeaderMessage HeaderResponse (withHeaderDateReference (utc 2020 1 1 5) defaultHeaderSettings)
        lookupAt value = lookupHeaderWith @Date settings (headerMapFromList [("Date", value)])
    lookupAt "Wednesday, 01-Jan-70 00:00:05 GMT" `shouldBe` Right (Just (Date (utc 2070 1 1 5)))
    lookupAt "Thursday, 01-Jan-70 00:00:06 GMT" `shouldBe` Right (Just (Date (utc 1970 1 1 6)))
    lookupHeaderWith @IfModifiedSince
      settings
      (headerMapFromList [("If-Modified-Since", "Wednesday, 01-Jan-70 00:00:05 GMT")])
      `shouldBe` Right (Just (IfModifiedSince (utc 2070 1 1 5)))
    lookupHeaderWith @IfRange
      settings
      (headerMapFromList [("If-Range", "Wednesday, 01-Jan-70 00:00:05 GMT")])
      `shouldBe` Right (Just (IfRangeDate (utc 2070 1 1 5)))
    lookupHeaderWith @RetryAfter
      settings
      (headerMapFromList [("Retry-After", "Wednesday, 01-Jan-70 00:00:05 GMT")])
      `shouldBe` Right (Just (SpecificTime (utc 2070 1 1 5)))
  it "applies the shared parser through each singleton HTTP-date codec" $ do
    let stamp = date exampleDate
        fields = " Sunday, 06-Nov-94 08:49:37 GMT\t" :| []
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (AcceptDatetime stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (Expires stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (IfUnmodifiedSince stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (LastModified stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (MementoDatetime stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (RepeatabilityFirstSent stamp)
    parseFromHeaders defaultHeaderSettings fields `shouldBe` Right (Sunset stamp)
  it "retains historical cookie expiry dates and ignores their weekday" $ do
    let fields = "session=abc; Expires=Mon, 06 Nov 1898 08:49:37 GMT" :| []
    case parseFromHeaders defaultHeaderSettings fields of
      Right (SetCookies [cookie]) -> setCookieExpires cookie `shouldBe` Just (utc 1898 11 6 31_777)
      other -> expectationFailure (show other)
  it "accepts cookie date tokens in any order and keeps HTTP date parsing strict" $ do
    let inputs =
          [ "08:49:37 | 6 nOVeMBer 1994"
          , "ignored [1994] / 06_NOV : 08:49:37PST"
          , "Sun, 06 Nov 1994 08:49:37 GMT extra"
          , "6th-1994year-November-08:49:37utc"
          , BS.pack [0xff] <> "ignored 6 Nov 1994 08:49:37"
          ]
    forM_ inputs $ \input -> do
      cookieExpiry input `shouldBe` Right (Just (date exampleDate))
      lookupDate input `shouldSatisfy` isLeft
  it "implements the cookie-specific year window and clock limits" $ do
    cookieExpiry "6 Nov 69 8:4:7" `shouldBe` Right (Just (utc 2069 11 6 29_047))
    cookieExpiry "6 Nov 70 8:4:7" `shouldBe` Right (Just (utc 1970 11 6 29_047))
    cookieExpiry "Sun Nov 01 00:00:00 0069" `shouldBe` Right (Just (utc 2069 11 1 0))
    cookieExpiry "06 Nov 1601 08:49:37" `shouldBe` Right (Just (utc 1601 11 6 31_777))
    forM_
      [ "06 Nov 1600 08:49:37"
      , "06 Nov 01994 08:49:37"
      , "30 Feb 2000 08:49:37"
      , "06 Nov 1994 08:49:037"
      , "Sat, 31 Dec 2016 23:59:60 GMT"
      , "06 Nov 1994"
      ]
      (\input -> cookieExpiry input `shouldBe` Right Nothing)
  it "commits to the first syntactic match even if a later token could repair the date" $
    forM_ ["99 06 Nov 1994 08:49:37", "06 Nov 1600 1994 08:49:37", "06 Nov 1994 25:00:00 08:49:37"] $
      \input -> cookieExpiry input `shouldBe` Right Nothing
  it "keeps expiry parsing inside its attribute and preserves the last valid duplicate" $ do
    let field = "session=abc; Expires=08:49:37 6 Nov 1994; Expires=bad" <> BS.pack [0xff] <> "; Path=/account; Secure"
    case parseSetCookieField field of
      Right cookie -> do
        setCookieExpires cookie `shouldBe` Just (date exampleDate)
        setCookiePath cookie `shouldBe` Just "/account"
        setCookieSecure cookie `shouldBe` True
      failure -> expectationFailure (show failure)
    cookieExpiry "08:49:37 6; trailing=Nov 1994" `shouldBe` Right Nothing
    cookieExpiry ("06 Nov 1994 08:49:37" <> BS.pack [0x7f]) `shouldSatisfy` isLeft
  it "scans long runs of ignored cookie date tokens" $
    cookieExpiry (BS.concat (replicate 20_000 "word ") <> "08:49:37 6 Nov 1994")
      `shouldBe` Right (Just (date exampleDate))
  it "round trips IMF-fixdate across century and day boundaries" $ property $ do
    year <- forAll (Gen.integral (Range.linear 1900 9999))
    dayOffset <- forAll (Gen.integral (Range.linear 0 364))
    seconds <- forAll (Gen.integral (Range.linear 0 86_399))
    let stamp = Date (addUTCTime (fromIntegral (dayOffset * 86_400 + seconds :: Int)) (utc year 1 1 0))
    lookupDate (renderToHeaders defaultHeaderSettings stamp) === Right (Just stamp)
  it "saturates Age for both Word32 and machine-word overflow" $ do
    lookupAge "4294967294" `shouldBe` Right (Just (Age 4_294_967_294))
    lookupAge "4294967296" `shouldBe` Right (Just (Age maxBound))
    lookupAge (BS.replicate 4096 57) `shouldBe` Right (Just (Age maxBound))
    lookupAge (BS.replicate 128 48 <> "12") `shouldBe` Right (Just (Age 12))
  it "uses the first Age member across comma lists and repeated field lines" $ do
    lookupAge " 12\t, 999" `shouldBe` Right (Just (Age 12))
    lookupAge "000000000" `shouldBe` Right (Just (Age 0))
    lookupAge "999999999" `shouldBe` Right (Just (Age 999_999_999))
    lookupAge "1000000000" `shouldBe` Right (Just (Age 1_000_000_000))
    lookupAge "invalid, 12" `shouldSatisfy` isLeft
    lookupHeader @Age (headerMapFromList [("Age", "12"), ("Age", "999")])
      `shouldBe` Right (Just (Age 12))
    lookupHeader @Age (headerMapFromList [("Age", "invalid"), ("Age", "12")])
      `shouldSatisfy` isLeft
  it "accepts bounded Retry-After delays and date forms" $ do
    let maxDelay = M.toStrictByteString (M.wordDec maxBound)
    lookupRetryAfter maxDelay `shouldBe` Right (Just (DelaySeconds maxBound))
    lookupRetryAfter "\t 00012 " `shouldBe` Right (Just (DelaySeconds 12))
    lookupRetryAfter "-1" `shouldSatisfy` isLeft
    lookupRetryAfter "12, 30" `shouldSatisfy` isLeft
    lookupRetryAfter exampleWireDate `shouldBe` Right (Just (SpecificTime (date exampleDate)))
    lookupHeader @RetryAfter (headerMapFromList [("Retry-After", "12"), ("Retry-After", "30")])
      `shouldSatisfy` isLeft
  it "round-trips arbitrary-length Retry-After delays without leading zeros" $ do
    let wide = M.toStrictByteString (M.wordDec maxBound) <> "0"
        long = BS.replicate 16_384 57
        renderLookup = fmap (fmap (renderToHeaders defaultHeaderSettings)) . lookupRetryAfter
    renderLookup wide `shouldBe` Right (Just wide)
    renderLookup ("\t 000" <> wide <> " ") `shouldBe` Right (Just wide)
    renderLookup (BS.replicate 16_384 48 <> long) `shouldBe` Right (Just long)
    renderLookup (BS.replicate 16_384 48) `shouldBe` Right (Just "0")
    lookupRetryAfter (long <> "junk") `shouldSatisfy` isLeft
    lookupRetryAfter (wide <> " 0") `shouldSatisfy` isLeft
    case parseRetryAfterDelay (" \t000" <> wide <> " ") of
      Right delay@(LargeDelaySeconds value) -> do
        largeDelayDigits value `shouldBe` SBS.toShort wide
        lookupRawHeader hRetryAfter (setHeader delay (headerMapFromList [])) `shouldBe` Just (wide :| [])
      other -> expectationFailure ("Expected a large delay, got " <> show other)
  it "checks Retry-After delays against an inclusive limit" $ do
    let largest = M.toStrictByteString (M.wordDec maxBound)
        large = BS.replicate 4096 57
        zeros = BS.replicate 128 48
    parseRetryAfterDelayWithin 0 "0" `shouldBe` Right (Just 0)
    parseRetryAfterDelayWithin 0 "1" `shouldBe` Right Nothing
    parseRetryAfterDelayWithin 12 "12" `shouldBe` Right (Just 12)
    parseRetryAfterDelayWithin 12 ("\t" <> zeros <> "12 ") `shouldBe` Right (Just 12)
    parseRetryAfterDelayWithin 12 "13" `shouldBe` Right Nothing
    parseRetryAfterDelayWithin maxBound largest `shouldBe` Right (Just maxBound)
    parseRetryAfterDelayWithin maxBound (largest <> "0") `shouldBe` Right Nothing
    parseRetryAfterDelayWithin 12 (large <> "\t ") `shouldBe` Right Nothing
    parseRetryAfterDelayWithin 12 (large <> "junk") `shouldSatisfy` isLeft
    parseRetryAfterDelayWithin 12 "13 0" `shouldSatisfy` isLeft
    parseRetryAfterDelayWithin 12 "-13" `shouldSatisfy` isLeft
  it "accepts quoted Cache-Control delays and saturates overflow" $ do
    let lookupCache value = lookupHeader @CacheControl (headerMapFromList [("Cache-Control", value)])
    lookupCache "max-age=\"12\", stale-if-error=18446744073709551616"
      `shouldBe` Right (Just (CacheControl (MaxAge 12 :| [StaleIfError maxBound])))
    lookupCache "max-age=\"12x\"" `shouldSatisfy` isLeft


utc :: Integer -> Int -> Int -> Integer -> UTCTime
utc year month day seconds = UTCTime (fromGregorian year month day) (secondsToDiffTime seconds)


exampleDate :: Date
exampleDate = Date (utc 1994 11 6 31_777)


exampleWireDate :: BS.ByteString
exampleWireDate = "Sun, 06 Nov 1994 08:49:37 GMT"


cookieExpiry :: ByteString -> Either String (Maybe UTCTime)
cookieExpiry value = setCookieExpires <$> parseSetCookieField ("session=abc; Expires=" <> value)


lookupDate :: BS.ByteString -> Either String (Maybe Date)
lookupDate value = lookupHeader (headerMapFromList [("Date", value)])


lookupAge :: BS.ByteString -> Either String (Maybe Age)
lookupAge value = lookupHeader (headerMapFromList [("Age", value)])


lookupRetryAfter :: BS.ByteString -> Either String (Maybe RetryAfter)
lookupRetryAfter value = lookupHeader (headerMapFromList [("Retry-After", value)])
