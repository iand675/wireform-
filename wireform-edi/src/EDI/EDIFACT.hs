-- | UN/EDIFACT service-string and envelope helpers.
module EDI.EDIFACT (
  ServiceStringAdvice (..),
  defaultServiceStringAdvice,
  parseServiceStringAdvice,
  inferServiceStringAdvice,
  serviceStringSyntax,
  decodeEDIFACT,
  decodeEDIFACTBS,
  encodeEDIFACT,
  encodeEDIFACTBS,
  EdifactEnvelope (..),
  EdifactMessage (..),
  EdifactError (..),
  parseEdifactEnvelope,
  validateEdifact,
  edifactErrors,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Text.Read qualified as TR
import Data.Vector (Vector)
import Data.Vector qualified as V
import EDI.Internal.Decode qualified as FastDecode
import EDI.Internal.Encode qualified as Native
import EDI.Internal.Scan qualified as Scan
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
defaultServiceStringAdvice =
  ServiceStringAdvice
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
  | otherwise = do
      let advice =
            ServiceStringAdvice
              { serviceComponentSeparator = T.index input 3
              , serviceElementSeparator = T.index input 4
              , serviceDecimalMark = T.index input 5
              , serviceReleaseCharacter = T.index input 6
              , serviceRepetitionSeparator =
                  let r = T.index input 7
                  in if r == ' ' then Nothing else Just r
              , serviceSegmentTerminator = T.index input 8
              }
      validateServiceStringAdvice advice
      Right advice


-- Service characters are single ASCII bytes; payload text remains UTF-8.
-- Distinct roles are essential to preserve release pairs across splits.
validateServiceStringAdvice :: ServiceStringAdvice -> Either String ()
validateServiceStringAdvice advice = do
  validateSyntax (serviceStringSyntax advice)
  let chars =
        [ serviceComponentSeparator advice
        , serviceElementSeparator advice
        , serviceDecimalMark advice
        , serviceReleaseCharacter advice
        , serviceSegmentTerminator advice
        ]
          <> maybe [] pure (serviceRepetitionSeparator advice)
  if any ((> 127) . ord) chars
    then Left "EDI.EDIFACT: service characters must be ASCII"
    else
      if length (nub chars) /= length chars
        then Left "EDI.EDIFACT: service characters must be distinct"
        else Right ()


inferServiceStringAdvice :: Text -> Either String ServiceStringAdvice
inferServiceStringAdvice input
  | T.isPrefixOf "UNA" stripped = parseServiceStringAdvice stripped
  | otherwise = Right defaultServiceStringAdvice
  where
    stripped = T.dropWhile isOuterWhitespace input


serviceStringSyntax :: ServiceStringAdvice -> Syntax
serviceStringSyntax advice =
  Syntax
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
  segments <- FastDecode.parseSegments syntax (serviceReleaseCharacter advice) body
  Right (Interchange syntax segments)


-- | Strict UTF-8 byte entry point, including EDIFACT fragments without UNB.
decodeEDIFACTBS :: ByteString -> Either String Interchange
decodeEDIFACTBS input = case TE.decodeUtf8' input of
  Left _ -> Left "EDI.EDIFACT: invalid UTF-8"
  Right text -> decodeEDIFACT text


{- | Render EDIFACT with explicit release-character handling. Syntax and
advice must agree. Custom advice is included as UNA so the result can
be decoded without out-of-band delimiter information. The generic
EDI.Encode encoder deliberately does not guess EDIFACT escaping for X12.
-}
encodeEDIFACT :: ServiceStringAdvice -> Interchange -> Either String Text
encodeEDIFACT advice doc = do
  validateServiceStringAdvice advice
  if interchangeSyntax doc /= serviceStringSyntax advice
    then Left "EDI.EDIFACT: interchange syntax does not match service advice"
    else
      if V.any (T.null . segmentTag) (interchangeSegments doc)
        then Left "EDI.EDIFACT: empty segment tag"
        else Right (toStrict (toLazyText (header <> V.foldMap renderSegment (interchangeSegments doc))))
  where
    header
      | advice == defaultServiceStringAdvice = mempty
      | otherwise =
          fromText "UNA"
            <> foldMap
              singleton
              [ serviceComponentSeparator advice
              , serviceElementSeparator advice
              , serviceDecimalMark advice
              , serviceReleaseCharacter advice
              , maybe ' ' id (serviceRepetitionSeparator advice)
              , serviceSegmentTerminator advice
              ]
    renderSegment seg =
      escaped (segmentTag seg)
        <> V.foldMap (\element -> singleton (serviceElementSeparator advice) <> renderElement element) (segmentElements seg)
        <> singleton (serviceSegmentTerminator advice)
    renderElement (Simple text) = escaped text
    renderElement (Composite parts) =
      V.ifoldl'
        ( \acc i part ->
            acc
              <> (if i == 0 then mempty else singleton (serviceComponentSeparator advice))
              <> escaped part
        )
        mempty
        parts
    escaped :: Text -> Builder
    escaped = fromText . Scan.escape (serviceReleaseCharacter advice) reserved
    reserved byte =
      byte == ascii (serviceReleaseCharacter advice)
        || byte == ascii (serviceComponentSeparator advice)
        || byte == ascii (serviceElementSeparator advice)
        || byte == ascii (serviceSegmentTerminator advice)
        || maybe False ((byte ==) . ascii) (serviceRepetitionSeparator advice)
    ascii = fromIntegral . ord


-- | UTF-8 form of 'encodeEDIFACT'.
encodeEDIFACTBS :: ServiceStringAdvice -> Interchange -> Either String ByteString
encodeEDIFACTBS advice doc
  | advice == defaultServiceStringAdvice =
      if interchangeSyntax doc == serviceStringSyntax advice
        then Native.encode defaultServices BS.empty (interchangeSegments doc)
        else Left "EDI.EDIFACT: interchange syntax does not match service advice"
  | otherwise = do
      validateServiceStringAdvice advice
      if interchangeSyntax doc /= serviceStringSyntax advice
        then Left "EDI.EDIFACT: interchange syntax does not match service advice"
        else
          Native.encode
            (Native.services (serviceStringSyntax advice) (serviceReleaseCharacter advice))
            ( BS.pack
                ( map
                    (fromIntegral . ord)
                    [ 'U'
                    , 'N'
                    , 'A'
                    , serviceComponentSeparator advice
                    , serviceElementSeparator advice
                    , serviceDecimalMark advice
                    , serviceReleaseCharacter advice
                    , maybe ' ' id (serviceRepetitionSeparator advice)
                    , serviceSegmentTerminator advice
                    ]
                )
            )
            (interchangeSegments doc)


defaultServices :: Native.Services
defaultServices = Native.services (serviceStringSyntax defaultServiceStringAdvice) '?'
{-# NOINLINE defaultServices #-}


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
parseInt label t
  | T.null t = Left (EdifactInvalidInteger label t)
  | T.null significant = Right 0
  | T.length significant > 19 = Left (EdifactInvalidInteger label t)
  | otherwise = case TR.decimal significant of
      Right (n, rest) | T.null rest && n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
      _ -> Left (EdifactInvalidInteger label t)
  where
    significant = T.dropWhile (== '0') t


isOuterWhitespace :: Char -> Bool
isOuterWhitespace c = c == '\r' || c == '\n' || c == ' ' || c == '\t'


intText :: Int -> Text
intText =
  toStrict . toLazyText . TBI.decimal
