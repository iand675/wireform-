-- | Shared interpretation of Retry-After for client follow-up requests.
module Network.HTTP.Internal.RetryAfter (
  RetryDelay (..),
  retryAfterWithin,
) where

import Data.ByteString qualified as BS
import Data.Fixed (Fixed (MkFixed))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (UTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.RetryAfter qualified as RetryAfter
import Network.HTTP.Types.Header qualified as H


data RetryDelay
  = NoRetryDelay
  | RetryDelay {-# UNPACK #-} !Int
  | RetryDelayAboveLimit


-- | Interpret the singleton field against a nonnegative limit in microseconds.
-- Numeric values and missing fields require no wall-clock read.
retryAfterWithin :: IO UTCTime -> Int -> H.Headers -> IO RetryDelay
retryAfterWithin getTime limit fields =
  case H.lookupSingletonHeader H.hRetryAfter fields of
    Right (Just wire)
      | endToEnd -> case BS.uncons (BS.dropWhile isOWS wire) of
          Just (first, _)
            | first >= 0x30 && first <= 0x39 -> pure (decodeDelay wire)
            -- Each HTTP-date spelling begins with a weekday.
            | (first >= 0x41 && first <= 0x5A) || (first >= 0x61 && first <= 0x7A) ->
                decodeDate wire <$> getTime
          _ -> pure NoRetryDelay
    _ -> pure NoRetryDelay
  where
    isOWS w = w == 0x20 || w == 0x09
    endToEnd =
      not (H.hasHeader H.hConnection fields)
        || H.hasHeader H.hRetryAfter (H.stripHopByHop fields)
    decodeDelay wire =
      case RetryAfter.parseRetryAfterDelayWithin (fromIntegral (quot limit 1_000_000)) wire of
        Right (Just delay) -> RetryDelay (fromIntegral delay * 1_000_000)
        Right Nothing -> RetryDelayAboveLimit
        Left _ -> NoRetryDelay
    decodeDate wire now =
      let settings = Typed.withHeaderDateReference now Typed.defaultHeaderSettings
      in case Typed.parseFromHeaders settings (wire :| []) of
           Right (RetryAfter.SpecificTime target) -> dateDelay target now
           _ -> NoRetryDelay
    dateDelay target now =
      let MkFixed remaining = nominalDiffTimeToSeconds (diffUTCTime target now)
          micros = quot (max 0 remaining + 999_999) 1_000_000
      in if micros > toInteger limit
           then RetryDelayAboveLimit
           else RetryDelay (fromInteger micros)
