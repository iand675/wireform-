{-# LANGUAGE BangPatterns #-}

{- | ASCII EDI delimiters over UTF-8 payloads. Slices retain the input
array. Release pairs remain intact until the final component is
materialized: consuming them while splitting segments would turn
@??+@ into @?+@ and hide a real element boundary.
-}
module EDI.Internal.Scan (splitRaw, hasDelimiter, unescape, escape) where

import Control.Monad.ST (runST)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)


splitRaw :: Char -> Char -> Text -> Either String (Vector Text)
splitRaw delimiter release (T.Text array offset size)
  | ord delimiter > 127 || ord release > 127 =
      Left "EDI: service characters must be ASCII"
  | otherwise = do
      count <- countParts 0 1
      pure $ runST $ do
        output <- MV.unsafeNew count
        let emit !start !position !index
              | position >= size = MV.unsafeWrite output index (slice start size)
              | byte position == rel = emit start (position + 2) index
              | byte position == sep = do
                  MV.unsafeWrite output index (slice start position)
                  emit (position + 1) (position + 1) (index + 1)
              | otherwise = emit start (position + 1) index
        emit 0 0 0
        V.unsafeFreeze output
  where
    !sep = fromIntegral (ord delimiter) :: Word8
    !rel = fromIntegral (ord release) :: Word8
    byte position = A.unsafeIndex array (offset + position)
    slice start end = T.text array (offset + start) (end - start)
    countParts !position !count
      | position >= size = Right count
      | byte position == rel =
          if position + 1 >= size
            then Left "EDI: dangling release character"
            else countParts (position + 2) count
      | byte position == sep = countParts (position + 1) (count + 1)
      | otherwise = countParts (position + 1) count
{-# INLINE splitRaw #-}


{- | The caller has already checked release-pair completeness with
'splitRaw'. Only ASCII bytes are examined, so UTF-8 continuation
bytes cannot be mistaken for delimiters.
-}
hasDelimiter :: Char -> Char -> Text -> Bool
hasDelimiter delimiter release (T.Text array offset size) = go 0
  where
    !sep = fromIntegral (ord delimiter) :: Word8
    !rel = fromIntegral (ord release) :: Word8
    go !position
      | position >= size = False
      | w == rel = go (position + 2)
      | w == sep = True
      | otherwise = go (position + 1)
      where
        w = A.unsafeIndex array (offset + position)
{-# INLINE hasDelimiter #-}


{- | Remove release prefixes once, after all structural splits. The
common unescaped case aliases its input. Escaped fields allocate
one compact text array, with linear work even for long fields.
-}
unescape :: Char -> Text -> Text
unescape release source@(T.Text array offset size)
  | first < 0 = source
  | otherwise =
      let (result, written) = A.run2 $ do
            output <- A.new (size - 1)
            A.copyI first output 0 array offset
            let copy !position !next
                  | position >= size = do
                      A.shrinkM output next
                      pure (output, next)
                  | w == rel = do
                      A.unsafeWrite output next (A.unsafeIndex array (offset + position + 1))
                      copy (position + 2) (next + 1)
                  | otherwise = do
                      A.unsafeWrite output next w
                      copy (position + 1) (next + 1)
                  where
                    w = A.unsafeIndex array (offset + position)
            copy first first
      in T.text result 0 written
  where
    !rel = fromIntegral (ord release) :: Word8
    !first = findRelease 0
    findRelease !position
      | position >= size = -1
      | A.unsafeIndex array (offset + position) == rel = position
      | otherwise = findRelease (position + 1)
{-# INLINE unescape #-}


{- | Escape ASCII service characters without a Text/Builder fragment per
character. Unchanged fields alias the source; changed fields get one
exact-size UTF-8 allocation. The caller validates the service advice.
-}
escape :: Char -> (Word8 -> Bool) -> Text -> Text
escape release reserved source@(T.Text array offset size)
  | extra == 0 = source
  | otherwise =
      let result = A.run $ do
            output <- A.new (size + extra)
            let copy !position !next
                  | position >= size = pure output
                  | reserved w = do
                      A.unsafeWrite output next rel
                      A.unsafeWrite output (next + 1) w
                      copy (position + 1) (next + 2)
                  | otherwise = do
                      A.unsafeWrite output next w
                      copy (position + 1) (next + 1)
                  where
                    w = A.unsafeIndex array (offset + position)
            copy 0 0
      in T.text result 0 (size + extra)
  where
    !rel = fromIntegral (ord release) :: Word8
    !extra = count 0 0
    count !position !n
      | position >= size = n
      | reserved (A.unsafeIndex array (offset + position)) = count (position + 1) (n + 1)
      | otherwise = count (position + 1) n
{-# INLINE escape #-}
