module Network.HTTP.Path (
  Path,
) where

import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as F
import GHC.IsList


newtype Path = Path
  { pathPieces :: F.Bundle V.Vector Text
  }


instance IsList Path where
  type Item Path = Text
  toList (Path ps) = F.toList ps
  fromList ps = Path $ F.fromList ps


-- length :: Path -> Int
-- length = F.length . pathPieces

-- null :: Path -> Bool
-- null = F.null . pathPieces

-- empty :: Path
-- empty = Path F.empty

-- singleton :: Text -> Path
-- singleton = Path . F.singleton

-- cons :: Text -> Path -> Path
-- cons piece (Path pieces) = Path $ F.cons piece pieces

-- snoc :: Path -> Text -> Path
-- snoc (Path pieces) piece = Path $ F.snoc pieces piec

-- replicate :: Int -> Text -> Path
-- replicate n piece = Path $ F.replicate n piece

-- generate :: Int -> (Int -> Text) -> Path
-- generate n f = Path $ F.generate n f

-- (++) :: Path -> Path -> Path
-- (++) (Path l) (Path r) = Path $ (F.++) l r

-- infixr 5 ++

-- head :: Path -> Text
-- head (Path pieces) = F.head pieces

-- last :: Path -> Text
-- last (Path pieces) = F.last pieces

-- init :: Path -> Path
-- init (Path pieces) = F.init pieces

-- tail :: Path -> Path
-- tail (Path pieces) = F.tail pieces

-- take :: Int -> Path -> Path
-- take n (Path pieces) = F.take n pieces

-- drop
-- map
-- concatMap
-- filter
-- takeWhile
-- dropWhile
-- elem
-- find
-- findIndex
-- foldl
-- foldl1
-- foldl'
-- foldr
-- foldr1
-- unfoldr
-- unfoldrN
-- unfoldrExactn
-- iterateN
-- eq

--   Percent-encoding for URLs.
--
-- This will substitute every byte with its percent-encoded equivalent unless:
--

-- * The byte is alphanumeric. (i.e. one of @/[A-Za-z0-9]/@)


--

-- * The byte is one of the 'Word8' listed in the first argument.

-- urlEncodeBuilder' :: [Word8] -> B.ByteString -> B.Builder
-- urlEncodeBuilder' extraUnreserved =
--     mconcat . map encodeChar . B.unpack
--   where
--     encodeChar ch
--         | unreserved ch = B.word8 ch
--         | otherwise = h2 ch

--     unreserved ch
--         | ch >= 65 && ch <= 90 = True -- A-Z
--         | ch >= 97 && ch <= 122 = True -- a-z
--         | ch >= 48 && ch <= 57 = True -- 0-9
--     unreserved c = c `elem` extraUnreserved

--     -- must be upper-case
--     h2 v = B.word8 37 `mappend` B.word8 (h a) `mappend` B.word8 (h b) -- 37 = %
--       where
--         (a, b) = v `divMod` 16
--     h i
--         | i < 10 = 48 + i -- zero (0)
--         | otherwise = 65 + i - 10 -- 65: A
