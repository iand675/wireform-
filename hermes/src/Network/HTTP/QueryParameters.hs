module Network.HTTP.QueryParameters where

import Data.Bit
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64, Word8)
import qualified Network.HTTP.Headers.Mason as M


newtype Word8Set = Word8Set (VU.Vector Bit) deriving (Eq, Show)


{- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
silently truncates to 8 bits Chars > '\255'.
-}
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}


-- Creates an empty Word8Set.
empty :: Word8Set
empty = Word8Set $ VU.replicate 256 0


-- Adds a Word8 value to the set.
insert :: Word8 -> Word8Set -> Word8Set
insert val (Word8Set w) = Word8Set (w VU.// [(fromIntegral val, 1)])


insertMany :: [Word8] -> Word8Set -> Word8Set
insertMany vals (Word8Set w) = Word8Set (w VU.// [(fromIntegral val, 1) | val <- vals])


-- Removes a Word8 value from the set.
delete :: Word8 -> Word8Set -> Word8Set
delete val (Word8Set w) = Word8Set (w VU.// [(fromIntegral val, 0)])


deleteMany :: [Word8] -> Word8Set -> Word8Set
deleteMany vals (Word8Set w) = Word8Set (w VU.// [(fromIntegral val, 0) | val <- vals])


-- Checks if a Word8 value is in the set.
member :: Word8 -> Word8Set -> Bool
member val (Word8Set w) = testBit w $ fromIntegral val


-- sqlcommenter spec wants them escaped with a slash, but this should
-- probably solve the same issue
unreservedQS :: Word8Set
unreservedQS = insertMany (map c2w "-_.~'") Network.HTTP.QueryParameters.empty


intersperse :: Foldable f => a -> f a -> [a]
intersperse sep a = case toList a of
  [] -> []
  (x : xs) -> x : prependToAll sep xs
    where
      prependToAll sep = \case
        [] -> []
        (x : xs) -> sep : x : prependToAll sep xs
{-# INLINE intersperse #-}


intercalate :: (Monoid a, Foldable f) => a -> f a -> a
intercalate delim l = mconcat (intersperse delim l)
{-# INLINE intercalate #-}


{- | Percent-encoding for URLs.

This will substitute every byte with its percent-encoded equivalent unless:

* The byte is alphanumeric. (i.e. one of @/[A-Za-z0-9]/@)

* The byte is one of the 'Word8' listed in the first argument.
-}
urlEncodeBuilder' :: Word8Set -> Text -> M.Builder
urlEncodeBuilder' extraUnreserved =
  BS.foldl' (\acc c -> acc <> encodeChar c) mempty . T.encodeUtf8
  where
    encodeChar ch
      | unreserved ch = M.word8 ch
      | otherwise = h2 ch

    unreserved ch
      | ch >= 65 && ch <= 90 = True -- A-Z
      | ch >= 97 && ch <= 122 = True -- a-z
      | ch >= 48 && ch <= 57 = True -- 0-9
    unreserved c = c `member` extraUnreserved

    -- must be upper-case
    h2 v = M.word8 37 `mappend` M.word8 (h a) `mappend` M.word8 (h b) -- 37 = %
      where
        (a, b) = v `divMod` 16
    h i
      | i < 10 = 48 + i -- zero (0)
      | otherwise = 65 + i - 10 -- 65: A


urlEncodeBuilder :: Text -> M.Builder
urlEncodeBuilder = urlEncodeBuilder' unreservedQS
