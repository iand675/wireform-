{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- | Internal Punycode encoding/decoding implementation per RFC 3492.
--
-- This module contains the low-level implementation details. For public API,
-- see 'Data.Text.Punycode'.
module Data.Text.IDN.Internal.Punycode
  ( encodePunycode
  , decodePunycode
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..))
import Data.Char (ord, chr, isAscii)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Text.IDN.Types (PunycodeError(..), PunycodeState(..), initialState)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Intro as VA
import GHC.Exts (State#, TYPE, RuntimeRep)
import GHC.ST (ST(ST))

-- RFC 3492 parameters
base :: Int
base = 36

tmin :: Int
tmin = 1

tmax :: Int
tmax = 26

skew :: Int
skew = 38

damp :: Int
damp = 700

initialBias :: Int
initialBias = 72

initialN :: Int
initialN = 0x80

delimiter :: Char
delimiter = '-'

-- | Extract unique non-basic code points and return them sorted in an unboxed vector
-- Uses in-place sort/dedupe for cache-friendly operations
extractSortedNonBasicCPs :: U.Vector Int -> U.Vector Int
extractSortedNonBasicCPs arr = runST $ do
  let len = U.length arr
  -- First pass: collect non-basic code points into mutable vector
  mvec <- UM.new len  -- worst case: all non-basic
  count <- collectNonBasic 0 0 mvec
  if count == 0
    then pure U.empty
    else do
      -- Sort in-place using introsort (cache-friendly)
      VA.sort (UM.take count mvec)
      -- Deduplicate in-place: compact adjacent duplicates
      uniqueCount <- dedupInPlace mvec count
      -- Freeze and return the compacted vector
      U.unsafeFreeze (UM.take uniqueCount mvec)
  where
    collectNonBasic !i !outIdx !mvec
      | i >= U.length arr = pure outIdx
      | otherwise = do
          let cp = U.unsafeIndex arr i
          if isBasicCP cp
            then collectNonBasic (i + 1) outIdx mvec
            else do
              UM.unsafeWrite mvec outIdx cp
              collectNonBasic (i + 1) (outIdx + 1) mvec
    
    -- Compact duplicates in sorted vector, return new length
    dedupInPlace !mvec !count = do
      first <- UM.unsafeRead mvec 0
      compactFrom 1 1 first mvec count
    
    compactFrom !readIdx !writeIdx !prev !mvec !len
      | readIdx >= len = pure writeIdx
      | otherwise = do
          curr <- UM.unsafeRead mvec readIdx
          if curr == prev
            then compactFrom (readIdx + 1) writeIdx prev mvec len
            else do
              UM.unsafeWrite mvec writeIdx curr
              compactFrom (readIdx + 1) (writeIdx + 1) curr mvec len

-- | Encode Unicode text to Punycode using ST monad and mutable arrays for performance
encodePunycode :: Text -> Either PunycodeError Text
encodePunycode input
  | T.null input = Right input
  | otherwise = runST $ do
      let inputCPs = codePointsVector input
          len = U.length inputCPs
          initialCap = max (len * 4) 64
          sortedNonBasic = extractSortedNonBasicCPs inputCPs
      marr0 <- A.new initialCap
      (marr1, idxBasics, basicCount) <- writeBasicsVec inputCPs marr0 0
      let state0 = initialState { h = basicCount }
      (marr2, idxDelim) <-
        if basicCount > 0
          then do
            marr' <- ensureCapacity marr1 idxBasics 1
            writeChar marr' idxBasics delimiter
            pure (marr', idxBasics + 1)
          else pure (marr1, idxBasics)
      result <- encodeLoopSorted marr2 idxDelim inputCPs len basicCount state0 sortedNonBasic
      case result of
        Right (marrFinal, finalLen) -> do
          arr <- A.unsafeFreeze marrFinal
          pure (Right (Text arr 0 finalLen))
        Left err -> pure (Left err)

-- Internal loop using State# and CPS, iterating through sorted vector
encodeLoopSorted# :: forall (rep :: RuntimeRep) (r :: TYPE rep) s.
                      A.MArray s
                  -> Int
                  -> U.Vector Int
                  -> Int
                  -> Int
                  -> PunycodeState
                  -> U.Vector Int
                  -> Int
                  -> State# s
                  -> (State# s -> PunycodeError -> r)                    -- error continuation
                  -> (State# s -> A.MArray s -> Int -> r)                -- success continuation
                  -> r
encodeLoopSorted# marr idx inputCPs inputLen basicCount state sortedCPs sortedIdx s errK succK
  | h state >= inputLen = succK s marr idx
  | sortedIdx >= U.length sortedCPs = succK s marr idx
  | otherwise =
      let m = U.unsafeIndex sortedCPs sortedIdx
          !diff   = m - n state
          !prod   = diff * (h state + 1)
          !delta' = delta state + prod
      in if delta' < 0
        then errK s Overflow
        else encodeCurrentPointVec# marr idx inputCPs inputLen m delta' (bias state) (h state) basicCount s
               (\s' err -> errK s' err)
               (\s' marr' idx' deltaAfter bias' handled' ->
                 let !deltaNext = deltaAfter + 1
                 in if deltaNext < 0
                   then errK s' Overflow
                   else encodeLoopSorted# marr' idx' inputCPs inputLen basicCount PunycodeState
                         { n = m + 1
                         , delta = deltaNext
                         , bias = bias'
                         , h = handled'
                         } sortedCPs (sortedIdx + 1) s' errK succK)

-- Wrapper for ST monad - converts CPS back to Either
encodeLoopSorted :: A.MArray s
                 -> Int
                 -> U.Vector Int
                 -> Int
                 -> Int
                 -> PunycodeState
                 -> U.Vector Int
                 -> ST s (Either PunycodeError (A.MArray s, Int))
encodeLoopSorted marr idx inputCPs inputLen basicCount state sortedCPs = ST $ \s ->
  encodeLoopSorted#
    marr idx inputCPs inputLen basicCount state sortedCPs 0 s
    (\s' err -> (# s', Left err #))
    (\s' marr' idx' -> (# s', Right (marr', idx') #))

encodeCurrentPointVec# :: forall (rep :: RuntimeRep) (r :: TYPE rep) s.
                            A.MArray s
                        -> Int
                        -> U.Vector Int
                        -> Int
                        -> Int
                        -> Int
                        -> Int
                        -> Int
                        -> Int
                        -> State# s
                        -> (State# s -> PunycodeError -> r)                           -- error continuation
                        -> (State# s -> A.MArray s -> Int -> Int -> Int -> Int -> r) -- success continuation
                        -> r
encodeCurrentPointVec# marr idx cps len m delta0 bias0 handled0 basicCount s0 errK succK =
  go 0 marr idx delta0 bias0 handled0 s0
  where
    go !i !arr !outIdx !deltaAcc !biasAcc !handledAcc s
      | i >= len = succK s arr outIdx deltaAcc biasAcc handledAcc
      | otherwise =
          let cp = U.unsafeIndex cps i
          in if cp < m
               then
                 if deltaAcc == maxBound
                   then errK s Overflow
                   else go (i + 1) arr outIdx (deltaAcc + 1) biasAcc handledAcc s
             else if cp == m
               then
                 let firstTime = handledAcc == basicCount
                 in encodeVarIntST# arr outIdx deltaAcc biasAcc (handledAcc + 1) firstTime s
                      (\s' err -> errK s' err)
                      (\s' marr' outIdx' newBias ->
                        go (i + 1) marr' outIdx' 0 newBias (handledAcc + 1) s')
               else go (i + 1) arr outIdx deltaAcc biasAcc handledAcc s

-- writeBasicsVec still returns lifted tuple for ST boundary
writeBasicsVec :: U.Vector Int -> A.MArray s -> Int -> ST s (A.MArray s, Int, Int)
writeBasicsVec cps marr0 idx0 = go 0 marr0 idx0 0
  where
    len = U.length cps
    go !i !marr !idx !count
      | i >= len = pure (marr, idx, count)
      | otherwise =
          let cp = U.unsafeIndex cps i
          in if isBasicCP cp
               then do
                 marr' <- ensureCapacity marr idx 1
                 writeChar marr' idx (chr cp)
                 go (i + 1) marr' (idx + 1) (count + 1)
               else go (i + 1) marr idx count

codePointsVector :: Text -> U.Vector Int
codePointsVector (Text arr off len) = runST $ do
  let endIdx = off + len
      count = countLoop off endIdx 0
  marr <- UM.new count
  fillLoop marr off endIdx 0
  U.unsafeFreeze marr
  where
    countLoop :: Int -> Int -> Int -> Int
    countLoop idx endIdx acc
      | idx >= endIdx = acc
      | otherwise =
          case nextCodePoint arr idx of
            (# _, stepBytes #) -> countLoop (idx + stepBytes) endIdx (acc + 1)
    fillLoop :: UM.MVector s Int -> Int -> Int -> Int -> ST s ()
    fillLoop marr idx endIdx i
      | idx >= endIdx = pure ()
      | otherwise =
          case nextCodePoint arr idx of
            (# cp, deltaBytes #) -> do
              UM.unsafeWrite marr i cp
              fillLoop marr (idx + deltaBytes) endIdx (i + 1)

nextCodePoint :: A.Array -> Int -> (# Int, Int #)
nextCodePoint arr idx =
  let b1 = fromIntegral (A.unsafeIndex arr idx) :: Int
  in if b1 < 0x80
        then (# b1, 1 #)
     else if b1 < 0xE0
        then
          let b2 = cont (idx + 1)
              cp = ((b1 .&. 0x1F) `shiftL` 6) .|. (b2 .&. 0x3F)
          in (# cp, 2 #)
     else if b1 < 0xF0
        then
          let b2 = cont (idx + 1)
              b3 = cont (idx + 2)
              cp = ((b1 .&. 0x0F) `shiftL` 12)
                     .|. ((b2 .&. 0x3F) `shiftL` 6)
                     .|. (b3 .&. 0x3F)
          in (# cp, 3 #)
        else
          let b2 = cont (idx + 1)
              b3 = cont (idx + 2)
              b4 = cont (idx + 3)
              cp = ((b1 .&. 0x07) `shiftL` 18)
                     .|. ((b2 .&. 0x3F) `shiftL` 12)
                     .|. ((b3 .&. 0x3F) `shiftL` 6)
                     .|. (b4 .&. 0x3F)
          in (# cp, 4 #)
  where
    cont i = fromIntegral (A.unsafeIndex arr i) :: Int

-- | Encode variable-length integer using State# and CPS
encodeVarIntST# :: forall (rep :: RuntimeRep) (r :: TYPE rep) s.
                   A.MArray s -> Int -> Int -> Int -> Int -> Bool -> State# s
                -> (State# s -> PunycodeError -> r)                    -- error continuation
                -> (State# s -> A.MArray s -> Int -> Int -> r)         -- success continuation
                -> r
encodeVarIntST# marr idx q biasValue numPoints firstTime s errK succK =
  let newBias = adapt q numPoints firstTime
  in writeVarIntLoop# marr idx q base biasValue s
       (\s' err -> errK s' err)
       (\s' marr' idx' -> succK s' marr' idx' newBias)

-- | Helper to write var int loop using State# and CPS (this is always successful, no errors)
writeVarIntLoop# :: forall (rep :: RuntimeRep) (r :: TYPE rep) s.
                    A.MArray s -> Int -> Int -> Int -> Int -> State# s
                 -> (State# s -> PunycodeError -> r)         -- error continuation (unused)
                 -> (State# s -> A.MArray s -> Int -> r)     -- success continuation
                 -> r
writeVarIntLoop# marr idx q k biasValue s _errK succK =
  let t = if k <= biasValue then tmin
          else if k >= biasValue + tmax then tmax
          else k - biasValue
  in if q < t
    then case ensureCapacityST# marr idx 1 s of
           (# s', marr' #) ->
             case writeCharST# marr' idx (encodeDigit q) s' of
               s'' -> succK s'' marr' (idx + 1)
    else
      let digit = t + ((q - t) `mod` (base - t))
          q' = (q - t) `div` (base - t)
      in case ensureCapacityST# marr idx 1 s of
           (# s', marr' #) ->
             case writeCharST# marr' idx (encodeDigit digit) s' of
               s'' -> writeVarIntLoop# marr' (idx + 1) q' (k + base) biasValue s'' _errK succK

-- | Encode a single digit
encodeDigit :: Int -> Char
encodeDigit d
  | d < 26 = chr (ord 'a' + d)
  | otherwise = chr (ord '0' + d - 26)

-- | Write a char (must be ASCII) to mutable array
writeChar :: A.MArray s -> Int -> Char -> ST s ()
writeChar marr idx c = A.unsafeWrite marr idx (fromIntegral $ ord c)

writeCharST# :: A.MArray s -> Int -> Char -> State# s -> State# s
writeCharST# marr idx c s =
  case A.unsafeWrite marr idx (fromIntegral $ ord c) of
    ST f -> case f s of (# s', _ #) -> s'

-- | Ensure array has capacity, resize if needed. 
ensureCapacity :: A.MArray s -> Int -> Int -> ST s (A.MArray s)
ensureCapacity marr idx needed = do
  size <- A.getSizeofMArray marr
  if idx + needed <= size
    then return marr
    else do
      let newSize = max (size * 2) (idx + needed)
      A.resizeM marr newSize

ensureCapacityST# :: A.MArray s -> Int -> Int -> State# s -> (# State# s, A.MArray s #)
ensureCapacityST# marr idx needed s =
  case A.getSizeofMArray marr of
    ST f -> case f s of
      (# s', size #) ->
        if idx + needed <= size
          then (# s', marr #)
          else
            let newSize = max (size * 2) (idx + needed)
            in case A.resizeM marr newSize of
                 ST g -> g s'

-- | Decode Punycode to Unicode text  
decodePunycode :: Text -> Either PunycodeError Text
decodePunycode input
  | T.null input = Right input
  | otherwise =
      case T.findIndex (== delimiter) (T.reverse input) of
        Nothing ->
          -- No delimiter; attempt to treat the whole label as encoded. If decoding
          -- fails, fall back to the original ASCII label.
          if T.all isBasic input
            then case decodeWithParts T.empty input of
                   Right txt -> Right txt
                   Left _    -> Right input
            else Left (InvalidPunycode "Non-basic chars without delimiter")
        Just revPos ->
          let delimPos   = T.length input - revPos - 1
              basicPart  = T.take delimPos input
              encodedPart = T.drop (delimPos + 1) input
          in if not (T.all isBasic basicPart)
               then Left (InvalidPunycode "Non-basic in basic part")
               else if T.null encodedPart
                      then Right basicPart
                      else decodeWithParts basicPart encodedPart

decodeWithParts :: Text -> Text -> Either PunycodeError Text
decodeWithParts basicPart encodedPart = runST $ do
  let basicCodePoints   = codePointsVector basicPart
      basicLen          = U.length basicCodePoints
      encodedCodePoints = codePointsVector encodedPart
      encodedLen        = U.length encodedCodePoints
      initialCapacity   = max 32 (basicLen + encodedLen)
  vec0 <- U.thaw $ U.take initialCapacity basicCodePoints
  result <- decodeLoopVector vec0 basicLen encodedCodePoints encodedLen 0 initialN 0 initialBias
  case result of
    Right (vecFinal, finalLen) -> do
      text <- buildTextFromVector vecFinal finalLen
      pure (Right text)
    Left err -> pure (Left err)

decodeLoopVector# :: forall (rep :: RuntimeRep) (r :: TYPE rep) s.
                      UM.MVector s Int
                  -> Int
                  -> U.Vector Int
                  -> Int
                  -> Int
                  -> Int
                  -> Int
                  -> Int
                  -> State# s
                  -> (State# s -> PunycodeError -> r)                    -- error continuation
                  -> (State# s -> UM.MVector s Int -> Int -> r)          -- success continuation
                  -> r
decodeLoopVector# vec outLen encoded encodedLen encIdx stateN i stateBias s errK succK
  | encIdx >= encodedLen = succK s vec outLen
  | otherwise =
      case decodeVarIntVec encoded encodedLen encIdx stateBias of
        Left err -> errK s err
        Right (deltaVal, nextIdx) ->
          let !i'        = i + deltaVal
              !numPoints = outLen + 1
              (!wraps, !insertPos) = i' `quotRem` numPoints
              !n' = stateN + wraps
          in if n' > 0x10FFFF
                      then errK s Overflow
                      else case insertCodePointST# vec outLen insertPos n' s of
                             (# s', vec', outLen' #) ->
                               let !bias' = adapt deltaVal numPoints (i == 0)
                                   !i''   = insertPos + 1
                               in decodeLoopVector# vec' outLen' encoded encodedLen nextIdx n' i'' bias' s' errK succK

decodeLoopVector :: UM.MVector s Int
                 -> Int
                 -> U.Vector Int
                 -> Int
                 -> Int
                 -> Int
                 -> Int
                 -> Int
                 -> ST s (Either PunycodeError (UM.MVector s Int, Int))
decodeLoopVector vec outLen encoded encodedLen encIdx stateN i stateBias = ST $ \s ->
  decodeLoopVector# vec outLen encoded encodedLen encIdx stateN i stateBias s
    (\s' err -> (# s', Left err #))
    (\s' vec' len' -> (# s', Right (vec', len') #))

insertCodePointST# :: UM.MVector s Int -> Int -> Int -> Int -> State# s -> (# State# s, UM.MVector s Int, Int #)
insertCodePointST# vec len pos cp s =
  case ensureCapacityVectorST# vec len 1 s of
    (# s', vec' #) ->
      let s'' = if pos < len
                  then case UM.move (UM.slice (pos + 1) (len - pos) vec')
                                   (UM.slice pos (len - pos) vec') of
                         ST f -> case f s' of (# s1, _ #) -> s1
                  else s'
      in case UM.write vec' pos cp of
           ST g -> case g s'' of
                     (# s''', _ #) -> (# s''', vec', len + 1 #)

ensureCapacityVectorST# :: UM.MVector s Int -> Int -> Int -> State# s -> (# State# s, UM.MVector s Int #)
ensureCapacityVectorST# vec len needed s =
  let cap = UM.length vec
  in if len + needed <= cap
       then (# s, vec #)
       else
         let extra = max 16 (len + needed - cap)
         in case UM.grow vec extra of
              ST f -> f s

buildTextFromVector :: UM.MVector s Int -> Int -> ST s Text
buildTextFromVector vec len = do
  totalBytes <- total 0 0
  marr0 <- A.new (max 32 totalBytes)
  (marrFinal, finalIdx) <- fill marr0 0 0
  arr <- A.unsafeFreeze marrFinal
  pure (Text arr 0 finalIdx)
  where
    total i acc
      | i >= len = pure acc
      | otherwise = do
          cp <- UM.read vec i
          total (i + 1) (acc + utf8Length cp)
    fill marr idx i
      | i >= len = pure (marr, idx)
      | otherwise = do
          cp <- UM.read vec i
          let needed = utf8Length cp
          marr' <- ensureCapacity marr idx needed
          idx' <- utf8Write marr' idx cp
          fill marr' idx' (i + 1)

utf8Length :: Int -> Int
utf8Length cp
  | cp <= 0x7F = 1
  | cp <= 0x7FF = 2
  | cp <= 0xFFFF = 3
  | otherwise = 4

utf8Write :: A.MArray s -> Int -> Int -> ST s Int
utf8Write marr idx cp
  | cp <= 0x7F = do
      A.unsafeWrite marr idx (fromIntegral cp)
      pure (idx + 1)
  | cp <= 0x7FF = do
      let b1 = 0xC0 .|. (cp `shiftR` 6)
          b2 = 0x80 .|. (cp .&. 0x3F)
      A.unsafeWrite marr idx (fromIntegral b1)
      A.unsafeWrite marr (idx + 1) (fromIntegral b2)
      pure (idx + 2)
  | cp <= 0xFFFF = do
      let b1 = 0xE0 .|. (cp `shiftR` 12)
          b2 = 0x80 .|. ((cp `shiftR` 6) .&. 0x3F)
          b3 = 0x80 .|. (cp .&. 0x3F)
      A.unsafeWrite marr idx (fromIntegral b1)
      A.unsafeWrite marr (idx + 1) (fromIntegral b2)
      A.unsafeWrite marr (idx + 2) (fromIntegral b3)
      pure (idx + 3)
  | otherwise = do
      let b1 = 0xF0 .|. (cp `shiftR` 18)
          b2 = 0x80 .|. ((cp `shiftR` 12) .&. 0x3F)
          b3 = 0x80 .|. ((cp `shiftR` 6) .&. 0x3F)
          b4 = 0x80 .|. (cp .&. 0x3F)
      A.unsafeWrite marr idx (fromIntegral b1)
      A.unsafeWrite marr (idx + 1) (fromIntegral b2)
      A.unsafeWrite marr (idx + 2) (fromIntegral b3)
      A.unsafeWrite marr (idx + 3) (fromIntegral b4)
      pure (idx + 4)


-- | Decode variable-length integer from a code-point vector.
decodeVarIntVec :: U.Vector Int -> Int -> Int -> Int -> Either PunycodeError (Int, Int)
decodeVarIntVec encoded len idx biasValue = go idx 0 1 base
  where
    go !pos !value !weight !k
      | pos >= len = Left (InvalidPunycode "Incomplete variable-length integer")
      | otherwise =
          let cp = U.unsafeIndex encoded pos
              d = decodeDigitInt cp
          in if d < 0
               then Left (InvalidPunycode ("Invalid digit: " <> T.singleton (chr cp)))
               else
                 let !t = if k <= biasValue then tmin
                          else if k >= biasValue + tmax then tmax
                          else k - biasValue
                     !value' = value + d * weight
                     !pos'   = pos + 1
                 in if d < t
                      then Right (value', pos')
                      else go pos' value' (weight * (base - t)) (k + base)

-- Returns -1 for invalid digit
decodeDigitInt :: Int -> Int
decodeDigitInt cp
  | cp >= 97 && cp <= 122 = cp - 97      -- 'a'..'z'
  | cp >= 65 && cp <= 90  = cp - 65      -- 'A'..'Z'
  | cp >= 48 && cp <= 57  = cp - 48 + 26 -- '0'..'9'
  | otherwise = -1

-- | Adapt bias after each delta encoding
adapt :: Int -> Int -> Bool -> Int
adapt deltaValue numPoints firstTime =
  let delta' = if firstTime then deltaValue `div` damp else deltaValue `div` 2
      delta'' = delta' + (delta' `div` numPoints)
  in go delta'' 0
  where
    go d k
      | d > ((base - tmin) * tmax) `div` 2 =
          go (d `div` (base - tmin)) (k + base)
      | otherwise = k + (base - tmin + 1) * d `div` (d + skew)

-- | Check if character is basic (ASCII)
isBasic :: Char -> Bool
isBasic = isAscii

isBasicCP :: Int -> Bool
isBasicCP c = c < 0x80