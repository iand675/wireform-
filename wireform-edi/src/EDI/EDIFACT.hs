-- | UN/EDIFACT service-string and envelope helpers.
module EDI.EDIFACT
  ( ServiceStringAdvice(..)
  , defaultServiceStringAdvice
  , parseServiceStringAdvice
  , inferServiceStringAdvice
  , serviceStringSyntax
  , decodeEDIFACT
  , EdifactEnvelope(..)
  , EdifactMessage(..)
  , EdifactError(..)
  , parseEdifactEnvelope
  , validateEdifact
  , edifactErrors
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V

import EDI.Query
import EDI.Value

data ServiceStringAdvice = ServiceStringAdvice
  { serviceComponentSeparator :: !Char
  , serviceElementSeparator :: !Char
  , serviceDecimalMark :: !Char
  , serviceReleaseCharacter :: !Char
  , serviceRepetitionSeparator :: !(Maybe Char)
  , serviceSegmentTerminator :: !Char
  }
  deriving stock (Show, Eq, Ord)

defaultServiceStringAdvice :: ServiceStringAdvice
defaultServiceStringAdvice = ServiceStringAdvice
  { serviceComponentSeparator = ':'
  , serviceElementSeparator = '+'
  , serviceDecimalMark = '.'
  , serviceReleaseCharacter = '?'
  , serviceRepetitionSeparator = Nothing
  , serviceSegmentTerminator = '\''
  }

parseServiceStringAdvice :: Text -> Either String ServiceStringAdvice
parseServiceStringAdvice input
  | T.length input < 9 =
      Left "EDI.EDIFACT: UNA service string advice must be 9 characters"
  | not (T.isPrefixOf "UNA" input) =
      Left "EDI.EDIFACT: service string advice must start with UNA"
  | otherwise =
      Right ServiceStringAdvice
        { serviceComponentSeparator = T.index input 3
        , serviceElementSeparator = T.index input 4
        , serviceDecimalMark = T.index input 5
        , serviceReleaseCharacter = T.index input 6
        , serviceRepetitionSeparator =
            let r = T.index input 7
            in if r == ' ' then Nothing else Just r
        , serviceSegmentTerminator = T.index input 8
        }

inferServiceStringAdvice :: Text -> Either String ServiceStringAdvice
inferServiceStringAdvice input
  | T.isPrefixOf "UNA" stripped = parseServiceStringAdvice stripped
  | otherwise = Right defaultServiceStringAdvice
  where
    stripped = T.dropWhile isOuterWhitespace input

serviceStringSyntax :: ServiceStringAdvice -> Syntax
serviceStringSyntax advice = Syntax
  { elementSeparator = serviceElementSeparator advice
  , componentSeparator = serviceComponentSeparator advice
  , repetitionSeparator = serviceRepetitionSeparator advice
  , segmentTerminator = serviceSegmentTerminator advice
  }

decodeEDIFACT :: Text -> Either String Interchange
decodeEDIFACT input = do
  advice <- inferServiceStringAdvice input
  let body =
        if T.isPrefixOf "UNA" (T.dropWhile isOuterWhitespace input)
          then T.drop 9 (T.dropWhile isOuterWhitespace input)
          else input
      syntax = serviceStringSyntax advice
  validateSyntax syntax
  segments <- traverse (parseSegment advice) (segmentTexts advice body)
  Right (Interchange syntax (V.fromList segments))

data EdifactEnvelope = EdifactEnvelope
  { edifactUNB :: !Segment
  , edifactMessages :: !(Vector EdifactMessage)
  , edifactUNZ :: !Segment
  }
  deriving stock (Show, Eq)

data EdifactMessage = EdifactMessage
  { messageUNH :: !Segment
  , messageBody :: !(Vector Segment)
  , messageUNT :: !Segment
  }
  deriving stock (Show, Eq)

data EdifactError
  = EdifactEmptyInterchange
  | EdifactExpectedSegment !Int !Text !(Maybe Text)
  | EdifactTrailingSegment !Int !Text
  | EdifactMissingSegment !Text
  | EdifactMissingElement !Text !Int
  | EdifactInvalidInteger !Text !Text
  | EdifactControlReferenceMismatch !Text !Text !Text
  | EdifactCountMismatch !Text !Int !Int
  deriving stock (Show, Eq, Ord)

parseEdifactEnvelope :: Interchange -> Either (Vector EdifactError) EdifactEnvelope
parseEdifactEnvelope doc =
  case parseSkeleton doc of
    Left errs -> Left errs
    Right env ->
      let errs = edifactEnvelopeErrors env
      in if V.null errs
           then Right env
           else Left errs

validateEdifact :: Interchange -> Either (Vector EdifactError) ()
validateEdifact doc =
  case parseEdifactEnvelope doc of
    Right _ -> Right ()
    Left errs -> Left errs

edifactErrors :: Interchange -> Vector EdifactError
edifactErrors doc =
  case parseSkeleton doc of
    Left errs -> errs
    Right env -> edifactEnvelopeErrors env

parseSkeleton :: Interchange -> Either (Vector EdifactError) EdifactEnvelope
parseSkeleton doc
  | V.null segments = Left (V.singleton EdifactEmptyInterchange)
  | otherwise = do
      unb <- expectAt 0 "UNB" segments
      unz <- expectAt lastIx "UNZ" segments
      (messages, nextIx) <- parseMessages segments 1 []
      if nextIx == lastIx
        then Right (EdifactEnvelope unb messages unz)
        else case segments V.!? nextIx of
          Nothing -> Left (V.singleton (EdifactMissingSegment "UNZ"))
          Just seg -> Left (V.singleton (EdifactTrailingSegment nextIx (segmentTag seg)))
  where
    segments = interchangeSegments doc
    lastIx = V.length segments - 1

parseMessages :: Vector Segment -> Int -> [EdifactMessage] -> Either (Vector EdifactError) (Vector EdifactMessage, Int)
parseMessages segments ix acc =
  case segments V.!? ix of
    Nothing -> Left (V.singleton (EdifactMissingSegment "UNZ"))
    Just seg
      | segmentTag seg == "UNZ" -> Right (V.fromList (reverse acc), ix)
      | segmentTag seg == "UNH" -> do
          (msg, nextIx) <- parseMessage segments ix
          parseMessages segments nextIx (msg : acc)
      | otherwise ->
          Left (V.singleton (EdifactExpectedSegment ix "UNH" (Just (segmentTag seg))))

parseMessage :: Vector Segment -> Int -> Either (Vector EdifactError) (EdifactMessage, Int)
parseMessage segments unhIx = do
  unh <- expectAt unhIx "UNH" segments
  untIx <- findSegmentIndex "UNT" segments (unhIx + 1)
  unt <- expectAt untIx "UNT" segments
  let bodyLen = untIx - unhIx - 1
      body = V.slice (unhIx + 1) bodyLen segments
  Right (EdifactMessage unh body unt, untIx + 1)

findSegmentIndex :: Text -> Vector Segment -> Int -> Either (Vector EdifactError) Int
findSegmentIndex tag segments = go
  where
    len = V.length segments
    go ix
      | ix >= len = Left (V.singleton (EdifactMissingSegment tag))
      | maybe False (segmentHasTag tag) (segments V.!? ix) = Right ix
      | otherwise = go (ix + 1)

expectAt :: Int -> Text -> Vector Segment -> Either (Vector EdifactError) Segment
expectAt ix tag segments =
  case segments V.!? ix of
    Nothing -> Left (V.singleton (EdifactMissingSegment tag))
    Just seg
      | segmentTag seg == tag -> Right seg
      | otherwise -> Left (V.singleton (EdifactExpectedSegment ix tag (Just (segmentTag seg))))

edifactEnvelopeErrors :: EdifactEnvelope -> Vector EdifactError
edifactEnvelopeErrors env =
  V.fromList
    ( compareText "UNB/UNZ control reference" (field "UNB" 4 (edifactUNB env)) (field "UNZ" 1 (edifactUNZ env))
        <> compareCount "UNZ message count" (field "UNZ" 0 (edifactUNZ env)) (V.length (edifactMessages env))
        <> concat (V.ifoldr (\ix msg acc -> messageErrors ix msg : acc) [] (edifactMessages env))
    )

messageErrors :: Int -> EdifactMessage -> [EdifactError]
messageErrors ix msg =
  compareText label (field "UNH" 0 (messageUNH msg)) (field "UNT" 1 (messageUNT msg))
    <> compareCount countLabel (field "UNT" 0 (messageUNT msg)) (V.length (messageBody msg) + 2)
  where
    label = "UNH/UNT message reference " <> intText (ix + 1)
    countLabel = "UNT segment count " <> intText (ix + 1)

field :: Text -> Int -> Segment -> Either EdifactError Text
field tag ix seg =
  case elementTextAt ix seg of
    Just t -> Right t
    Nothing -> Left (EdifactMissingElement tag (ix + 1))

compareText :: Text -> Either EdifactError Text -> Either EdifactError Text -> [EdifactError]
compareText label left right =
  case (left, right) of
    (Right a, Right b)
      | a == b -> []
      | otherwise -> [EdifactControlReferenceMismatch label a b]
    (Left err, _) -> [err]
    (_, Left err) -> [err]

compareCount :: Text -> Either EdifactError Text -> Int -> [EdifactError]
compareCount label actualText expected =
  case actualText of
    Left err -> [err]
    Right t ->
      case parseInt label t of
        Right actual
          | actual == expected -> []
          | otherwise -> [EdifactCountMismatch label expected actual]
        Left err -> [err]

parseInt :: Text -> Text -> Either EdifactError Int
parseInt label t =
  case TR.decimal t of
    Right (n, rest) | T.null rest -> Right n
    _ -> Left (EdifactInvalidInteger label t)

parseSegment :: ServiceStringAdvice -> Text -> Either String Segment
parseSegment advice raw =
  case splitReleased (serviceReleaseCharacter advice) (serviceElementSeparator advice) raw of
    [] -> Left "EDI.EDIFACT: empty segment"
    tag : fields
      | T.null tag -> Left "EDI.EDIFACT: empty segment tag"
      | otherwise ->
          Right
            ( Segment
                tag
                ( V.fromList
                    (map (parseElement advice) fields)
                )
            )

parseElement :: ServiceStringAdvice -> Text -> Element
parseElement advice t =
  case splitReleased (serviceReleaseCharacter advice) (serviceComponentSeparator advice) t of
    [part] -> Simple part
    parts -> Composite (V.fromList parts)

segmentTexts :: ServiceStringAdvice -> Text -> [Text]
segmentTexts advice =
  filter (not . T.null)
    . map trimSegment
    . splitReleased (serviceReleaseCharacter advice) (serviceSegmentTerminator advice)

splitReleased :: Char -> Char -> Text -> [Text]
splitReleased release delimiter = finish . T.foldl' step initial
  where
    initial = SplitState False T.empty []
    step (SplitState escaping current chunks) ch
      | escaping && (ch == delimiter || ch == release) =
          SplitState False (T.snoc current ch) chunks
      | escaping =
          SplitState False (T.snoc (T.snoc current release) ch) chunks
      | ch == release = SplitState True current chunks
      | ch == delimiter = SplitState False T.empty (current : chunks)
      | otherwise = SplitState False (T.snoc current ch) chunks
    finish (SplitState _ current chunks) = reverse (current : chunks)

data SplitState = SplitState !Bool !Text ![Text]

trimSegment :: Text -> Text
trimSegment = T.dropAround isOuterWhitespace

isOuterWhitespace :: Char -> Bool
isOuterWhitespace c = c == '\r' || c == '\n' || c == ' ' || c == '\t'

intText :: Int -> Text
intText =
  toStrict . toLazyText . TBI.decimal
