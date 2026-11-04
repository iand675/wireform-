{-# LANGUAGE BangPatterns #-}

{- | Scan segment boundaries once, then materialize each segment in one
field pass. Segments, fields and components go directly into their final
vector buffers, without intermediate vectors of raw segments or fields.
-}
module EDI.Internal.Decode (parseSegments) where

import Control.Monad.ST (runST)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Array qualified as A
import Data.Text.Internal qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import EDI.Internal.Scan qualified as Scan
import EDI.Value


-- Scan each segment boundary once and write materialized segments directly
-- to the output vector. Release pairs stay intact for the field parser.
parseSegments :: Syntax -> Char -> Text -> Either String (V.Vector Segment)
parseSegments syntax releaseChar (T.Text array off size) = runST $ do
  output <- MV.unsafeNew 8
  let finish buffer !count = Right <$> V.unsafeFreeze (MV.unsafeSlice 0 count buffer)
      emit buffer !count !start !end !lastSegment =
        let raw = Text.dropWhile outerWhitespace (T.text array (off + start) (end - start))
            continue next nextCount
              | lastSegment = finish next nextCount
              | otherwise = scan next nextCount (end + 1) (end + 1)
        in if Text.null raw
             then continue buffer count
             else case parseSegment (elementSeparator syntax) (componentSeparator syntax) releaseChar raw of
               Left err -> pure (Left err)
               Right !value -> do
                 next <- if count < MV.length buffer then pure buffer else MV.unsafeGrow buffer (MV.length buffer)
                 MV.unsafeWrite next count value
                 continue next (count + 1)
      scan buffer !count !start !position
        | position >= size = emit buffer count start size True
        | byte position == rel =
            if position + 1 >= size
              then pure (Left "EDI: dangling release character")
              else scan buffer count start (position + 2)
        | byte position == term = emit buffer count start position False
        | otherwise = scan buffer count start (position + 1)
  scan output 0 0 0
  where
    !term = fromIntegral (ord (segmentTerminator syntax)) :: Word8
    !rel = fromIntegral (ord releaseChar) :: Word8
    byte i = A.unsafeIndex array (off + i)
    outerWhitespace c = c == '\r' || c == '\n' || c == ' ' || c == '\t'


parseSegment :: Char -> Char -> Char -> Text -> Either String Segment
parseSegment elemChar componentChar releaseChar (T.Text array off size)
  | tagEnd < 0 = Left "EDI: dangling release character"
  | tagEnd == 0 = Left "EDI.EDIFACT: empty segment tag"
  | tagEnd == size = Right (Segment tag V.empty)
  | otherwise =
      case runST
        ( do
            fields <- MV.unsafeNew 8
            emptyParts <- MV.unsafeNew 0
            let grow output !count !value = do
                  target <-
                    if count < MV.length output
                      then pure output
                      else MV.unsafeGrow output (max 4 (MV.length output))
                  MV.unsafeWrite target count value
                  pure target
                finish fields' fieldCount parts partCount fieldStart componentStart position escaped = do
                  value <-
                    if partCount == 0
                      then pure (Simple (materialize fieldStart position escaped))
                      else do
                        finalParts <- grow parts partCount (materialize componentStart position escaped)
                        Composite <$> V.unsafeFreeze (MV.unsafeSlice 0 (partCount + 1) finalParts)
                  grow fields' fieldCount value
                fieldLoop fields' !fieldCount parts !partCount !fieldStart !componentStart !position !escaped
                  | position >= size = do
                      finalFields <- finish fields' fieldCount parts partCount fieldStart componentStart size escaped
                      Right <$> V.unsafeFreeze (MV.unsafeSlice 0 (fieldCount + 1) finalFields)
                  | byte position == rel =
                      if position + 1 >= size
                        then pure (Left "EDI: dangling release character")
                        else fieldLoop fields' fieldCount parts partCount fieldStart componentStart (position + 2) True
                  | byte position == comp = do
                      nextParts <- grow parts partCount (materialize componentStart position escaped)
                      fieldLoop fields' fieldCount nextParts (partCount + 1) fieldStart (position + 1) (position + 1) False
                  | byte position == elemSep = do
                      nextFields <- finish fields' fieldCount parts partCount fieldStart componentStart position escaped
                      fieldLoop nextFields (fieldCount + 1) emptyParts 0 (position + 1) (position + 1) (position + 1) False
                  | otherwise = fieldLoop fields' fieldCount parts partCount fieldStart componentStart (position + 1) escaped
            fieldLoop fields 0 emptyParts 0 (tagEnd + 1) (tagEnd + 1) (tagEnd + 1) False
        ) of
        Left err -> Left err
        Right fields -> let !result = Segment tag fields in Right result
  where
    !elemSep = fromIntegral (ord elemChar) :: Word8
    !comp = fromIntegral (ord componentChar) :: Word8
    !rel = fromIntegral (ord releaseChar) :: Word8
    byte i = A.unsafeIndex array (off + i)
    materialize start end escaped =
      let text = T.text array (off + start) (end - start)
      in if escaped then Scan.unescape releaseChar text else text
    !tagEnd = findTag 0
    tag = Scan.unescape releaseChar (T.text array off tagEnd)
    findTag !i
      | i >= size = size
      | byte i == rel = if i + 1 >= size then -1 else findTag (i + 2)
      | byte i == elemSep = i
      | otherwise = findTag (i + 1)
