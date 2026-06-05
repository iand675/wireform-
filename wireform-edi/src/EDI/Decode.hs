-- | EDI text decoding.
module EDI.Decode
  ( decode
  , decodeWithSyntax
  , decodeBS
  , decodeBSWithSyntax
  , inferSyntax
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import EDI.Value

-- | Decode an interchange, inferring X12 delimiters from an @ISA@ segment
-- when present and otherwise using 'defaultSyntax'.
decode :: Text -> Either String Interchange
decode input = do
  syn <- inferSyntax input
  decodeWithSyntax syn input

decodeBS :: ByteString -> Either String Interchange
decodeBS = decode . TE.decodeUtf8

-- | Decode with a caller-supplied syntax.
decodeWithSyntax :: Syntax -> Text -> Either String Interchange
decodeWithSyntax syn input = do
  validateSyntax syn
  segments <- traverse (parseSegment syn) (segmentTexts syn input)
  Right (Interchange syn (V.fromList segments))

decodeBSWithSyntax :: Syntax -> ByteString -> Either String Interchange
decodeBSWithSyntax syn = decodeWithSyntax syn . TE.decodeUtf8

-- | Infer delimiters from an X12 @ISA@ header. If the input does not start
-- with @ISA@ after leading CR/LF whitespace, fall back to 'defaultSyntax'.
inferSyntax :: Text -> Either String Syntax
inferSyntax input =
  let stripped = T.dropWhile isLineBreak input
  in if T.isPrefixOf "ISA" stripped
       then inferIsaSyntax stripped
       else Right defaultSyntax

inferIsaSyntax :: Text -> Either String Syntax
inferIsaSyntax input
  | T.length input <= 105 =
      Left "EDI.Decode: ISA segment is too short to infer delimiters"
  | otherwise = do
      let elemSep = T.index input 3
          term = T.index input 105
          isaBody = T.takeWhile (/= term) input
          fields = T.split (== elemSep) isaBody
      component <- case fieldAt 16 fields of
        Just t | T.length t == 1 -> Right (T.head t)
        Just _ -> Left "EDI.Decode: ISA16 must be exactly one component separator"
        Nothing -> Left "EDI.Decode: ISA segment is missing ISA16"
      let repetition = case fieldAt 11 fields of
            Just t | T.length t == 1 -> Just (T.head t)
            _ -> Nothing
          syn = Syntax elemSep component repetition term
      validateSyntax syn
      Right syn

fieldAt :: Int -> [a] -> Maybe a
fieldAt target = go 0
  where
    go _ [] = Nothing
    go ix (x : xs)
      | ix == target = Just x
      | otherwise = go (ix + 1) xs

segmentTexts :: Syntax -> Text -> [Text]
segmentTexts syn =
  filter (not . T.null)
    . map trimSegment
    . T.split (== segmentTerminator syn)

trimSegment :: Text -> Text
trimSegment = T.dropAround isLineBreak

isLineBreak :: Char -> Bool
isLineBreak c = c == '\r' || c == '\n'

parseSegment :: Syntax -> Text -> Either String Segment
parseSegment syn raw =
  case T.break (== elementSeparator syn) raw of
    (tag, rest)
      | T.null tag -> Left "EDI.Decode: empty segment tag"
      | T.null rest -> Right (Segment tag V.empty)
      | otherwise ->
          let fields = T.split (== elementSeparator syn) (T.drop 1 rest)
              elems = zipWith (parseSegmentElement syn tag) [0 :: Int ..] fields
          in Right (Segment tag (V.fromList elems))

parseElement :: Syntax -> Text -> Element
parseElement syn t
  | T.any (== componentSeparator syn) t =
      Composite (V.fromList (T.split (== componentSeparator syn) t))
  | otherwise = Simple t

parseSegmentElement :: Syntax -> Text -> Int -> Text -> Element
parseSegmentElement _ "ISA" 15 t = Simple t
parseSegmentElement syn _ _ t = parseElement syn t
