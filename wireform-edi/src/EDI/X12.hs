-- | X12 envelope helpers and functional acknowledgement generation.
module EDI.X12
  ( X12Envelope(..)
  , FunctionalGroup(..)
  , TransactionSet(..)
  , X12Error(..)
  , AckSettings(..)
  , defaultAckSettings
  , parseX12Envelope
  , validateX12
  , x12Errors
  , functionalAcknowledgment997
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

data X12Envelope = X12Envelope
  { x12ISA :: !Segment
  , x12Groups :: !(Vector FunctionalGroup)
  , x12IEA :: !Segment
  }
  deriving stock (Show, Eq)

data FunctionalGroup = FunctionalGroup
  { groupGS :: !Segment
  , groupTransactions :: !(Vector TransactionSet)
  , groupGE :: !Segment
  }
  deriving stock (Show, Eq)

data TransactionSet = TransactionSet
  { transactionST :: !Segment
  , transactionBody :: !(Vector Segment)
  , transactionSE :: !Segment
  }
  deriving stock (Show, Eq)

data X12Error
  = X12EmptyInterchange
  | X12ExpectedSegment !Int !Text !(Maybe Text)
  | X12TrailingSegment !Int !Text
  | X12MissingSegment !Text
  | X12MissingElement !Text !Int
  | X12InvalidInteger !Text !Text
  | X12ControlNumberMismatch !Text !Text !Text
  | X12CountMismatch !Text !Int !Int
  | X12InvalidSegmentArity !Text !Int !Int
  deriving stock (Show, Eq, Ord)

data AckSettings = AckSettings
  { ackSyntax :: !Syntax
  , ackSenderQualifier :: !Text
  , ackSenderId :: !Text
  , ackReceiverQualifier :: !Text
  , ackReceiverId :: !Text
  , ackDateYYMMDD :: !Text
  , ackDateCCYYMMDD :: !Text
  , ackTimeHHMM :: !Text
  , ackInterchangeControlNumber :: !Text
  , ackGroupControlNumber :: !Text
  , ackTransactionControlNumber :: !Text
  , ackVersion :: !Text
  }
  deriving stock (Show, Eq)

defaultAckSettings :: AckSettings
defaultAckSettings = AckSettings
  { ackSyntax = defaultSyntax
  , ackSenderQualifier = "ZZ"
  , ackSenderId = "SENDER"
  , ackReceiverQualifier = "ZZ"
  , ackReceiverId = "RECEIVER"
  , ackDateYYMMDD = "260101"
  , ackDateCCYYMMDD = "20260101"
  , ackTimeHHMM = "0000"
  , ackInterchangeControlNumber = "000000001"
  , ackGroupControlNumber = "1"
  , ackTransactionControlNumber = "0001"
  , ackVersion = "004010"
  }

parseX12Envelope :: Interchange -> Either (Vector X12Error) X12Envelope
parseX12Envelope doc =
  case parseSkeleton doc of
    Left errs -> Left errs
    Right env ->
      let errs = x12EnvelopeErrors env
      in if V.null errs
           then Right env
           else Left errs

validateX12 :: Interchange -> Either (Vector X12Error) ()
validateX12 doc =
  case parseX12Envelope doc of
    Right _ -> Right ()
    Left errs -> Left errs

x12Errors :: Interchange -> Vector X12Error
x12Errors doc =
  case parseSkeleton doc of
    Left errs -> errs
    Right env -> x12EnvelopeErrors env

parseSkeleton :: Interchange -> Either (Vector X12Error) X12Envelope
parseSkeleton doc
  | V.null segments = Left (V.singleton X12EmptyInterchange)
  | otherwise = do
      isa <- expectAt 0 "ISA" segments
      iea <- expectAt lastIx "IEA" segments
      (groups, nextIx) <- parseGroups segments 1 []
      if nextIx == lastIx
        then Right (X12Envelope isa groups iea)
        else case segments V.!? nextIx of
          Nothing -> Left (V.singleton (X12MissingSegment "IEA"))
          Just seg -> Left (V.singleton (X12TrailingSegment nextIx (segmentTag seg)))
  where
    segments = interchangeSegments doc
    lastIx = V.length segments - 1

parseGroups :: Vector Segment -> Int -> [FunctionalGroup] -> Either (Vector X12Error) (Vector FunctionalGroup, Int)
parseGroups segments ix acc =
  case segments V.!? ix of
    Nothing -> Left (V.singleton (X12MissingSegment "IEA"))
    Just seg
      | segmentTag seg == "IEA" -> Right (V.fromList (reverse acc), ix)
      | segmentTag seg == "GS" -> do
          (group, nextIx) <- parseGroup segments ix
          parseGroups segments nextIx (group : acc)
      | otherwise ->
          Left (V.singleton (X12ExpectedSegment ix "GS" (Just (segmentTag seg))))

parseGroup :: Vector Segment -> Int -> Either (Vector X12Error) (FunctionalGroup, Int)
parseGroup segments ix = do
  gs <- expectAt ix "GS" segments
  (transactions, ge, nextIx) <- parseTransactions segments (ix + 1) []
  Right (FunctionalGroup gs transactions ge, nextIx)

parseTransactions :: Vector Segment -> Int -> [TransactionSet] -> Either (Vector X12Error) (Vector TransactionSet, Segment, Int)
parseTransactions segments ix acc =
  case segments V.!? ix of
    Nothing -> Left (V.singleton (X12MissingSegment "GE"))
    Just seg
      | segmentTag seg == "GE" -> Right (V.fromList (reverse acc), seg, ix + 1)
      | segmentTag seg == "ST" -> do
          (tx, nextIx) <- parseTransaction segments ix
          parseTransactions segments nextIx (tx : acc)
      | otherwise ->
          Left (V.singleton (X12ExpectedSegment ix "ST" (Just (segmentTag seg))))

parseTransaction :: Vector Segment -> Int -> Either (Vector X12Error) (TransactionSet, Int)
parseTransaction segments stIx = do
  st <- expectAt stIx "ST" segments
  seIx <- findSegmentIndex "SE" segments (stIx + 1)
  se <- expectAt seIx "SE" segments
  let bodyLen = seIx - stIx - 1
      body = V.slice (stIx + 1) bodyLen segments
  Right (TransactionSet st body se, seIx + 1)

findSegmentIndex :: Text -> Vector Segment -> Int -> Either (Vector X12Error) Int
findSegmentIndex tag segments = go
  where
    len = V.length segments
    go ix
      | ix >= len = Left (V.singleton (X12MissingSegment tag))
      | maybe False (segmentHasTag tag) (segments V.!? ix) = Right ix
      | otherwise = go (ix + 1)

expectAt :: Int -> Text -> Vector Segment -> Either (Vector X12Error) Segment
expectAt ix tag segments =
  case segments V.!? ix of
    Nothing -> Left (V.singleton (X12MissingSegment tag))
    Just seg
      | segmentTag seg == tag -> Right seg
      | otherwise -> Left (V.singleton (X12ExpectedSegment ix tag (Just (segmentTag seg))))

x12EnvelopeErrors :: X12Envelope -> Vector X12Error
x12EnvelopeErrors env =
  V.fromList
    ( isaArityErrors
        <> ieaArityErrors
        <> controlErrors
        <> concat (V.ifoldr (\ix group acc -> groupErrors ix group : acc) [] (x12Groups env))
    )
  where
    isa = x12ISA env
    iea = x12IEA env
    isaArityErrors = arityErrors "ISA" 16 isa
    ieaArityErrors = arityErrors "IEA" 2 iea
    controlErrors =
      compareText "ISA/IEA control number" (field "ISA" 12 isa) (field "IEA" 1 iea)
        <> compareCount "IEA functional group count" (field "IEA" 0 iea) (V.length (x12Groups env))

groupErrors :: Int -> FunctionalGroup -> [X12Error]
groupErrors ix group =
  arityErrors "GS" 8 (groupGS group)
    <> arityErrors "GE" 2 (groupGE group)
    <> compareText label (field "GS" 5 (groupGS group)) (field "GE" 1 (groupGE group))
    <> compareCount countLabel (field "GE" 0 (groupGE group)) (V.length (groupTransactions group))
    <> concat (V.ifoldr (\txIx tx acc -> transactionErrors ix txIx tx : acc) [] (groupTransactions group))
  where
    label = "GS/GE control number group " <> intText (ix + 1)
    countLabel = "GE transaction count group " <> intText (ix + 1)

transactionErrors :: Int -> Int -> TransactionSet -> [X12Error]
transactionErrors groupIx txIx tx =
  arityErrors "ST" 2 (transactionST tx)
    <> arityErrors "SE" 2 (transactionSE tx)
    <> compareText label (field "ST" 1 (transactionST tx)) (field "SE" 1 (transactionSE tx))
    <> compareCount countLabel (field "SE" 0 (transactionSE tx)) (V.length (transactionBody tx) + 2)
  where
    suffix = " group " <> intText (groupIx + 1) <> " transaction " <> intText (txIx + 1)
    label = "ST/SE control number" <> suffix
    countLabel = "SE segment count" <> suffix

arityErrors :: Text -> Int -> Segment -> [X12Error]
arityErrors tag expected seg =
  let actual = V.length (segmentElements seg)
  in if actual == expected
       then []
       else [X12InvalidSegmentArity tag expected actual]

field :: Text -> Int -> Segment -> Either X12Error Text
field tag ix seg =
  case elementTextAt ix seg of
    Just t -> Right t
    Nothing -> Left (X12MissingElement tag (ix + 1))

compareText :: Text -> Either X12Error Text -> Either X12Error Text -> [X12Error]
compareText label left right =
  case (left, right) of
    (Right a, Right b)
      | a == b -> []
      | otherwise -> [X12ControlNumberMismatch label a b]
    (Left err, _) -> [err]
    (_, Left err) -> [err]

compareCount :: Text -> Either X12Error Text -> Int -> [X12Error]
compareCount label actualText expected =
  case actualText of
    Left err -> [err]
    Right t ->
      case parseInt label t of
        Right actual
          | actual == expected -> []
          | otherwise -> [X12CountMismatch label expected actual]
        Left err -> [err]

parseInt :: Text -> Text -> Either X12Error Int
parseInt label t =
  case TR.decimal t of
    Right (n, rest) | T.null rest -> Right n
    _ -> Left (X12InvalidInteger label t)

functionalAcknowledgment997 :: AckSettings -> X12Envelope -> Interchange
functionalAcknowledgment997 settings env =
  Interchange (ackSyntax settings) segments
  where
    groups = x12Groups env
    ackSets = V.imap (ackTransactionSet settings) groups
    segments =
      V.concat
        [ V.singleton (ackISA settings)
        , V.singleton (ackGS settings)
        , V.concatMap id ackSets
        , V.singleton (ackGE settings (V.length groups))
        , V.singleton (ackIEA settings)
        ]

ackTransactionSet :: AckSettings -> Int -> FunctionalGroup -> Vector Segment
ackTransactionSet settings ix group =
  V.concat
    [ V.singleton st
    , V.singleton ak1
    , V.concatMap ackForTransaction (groupTransactions group)
    , V.singleton ak9
    , V.singleton se
    ]
  where
    control = indexedControl (ackTransactionControlNumber settings) ix
    txCount = V.length (groupTransactions group)
    segmentCount = 4 + (2 * txCount)
    st = seg "ST" [Simple "997", Simple control]
    ak1 =
      seg
        "AK1"
        [ Simple (fieldOrBlank 0 (groupGS group))
        , Simple (fieldOrBlank 5 (groupGS group))
        ]
    ak9 =
      seg
        "AK9"
        [ Simple "A"
        , Simple (intText txCount)
        , Simple (intText txCount)
        , Simple (intText txCount)
        ]
    se = seg "SE" [Simple (intText segmentCount), Simple control]

ackForTransaction :: TransactionSet -> Vector Segment
ackForTransaction tx =
  V.fromList
    [ seg
        "AK2"
        [ Simple (fieldOrBlank 0 (transactionST tx))
        , Simple (fieldOrBlank 1 (transactionST tx))
        ]
    , seg "AK5" [Simple "A"]
    ]

ackISA :: AckSettings -> Segment
ackISA settings =
  seg
    "ISA"
    [ Simple "00"
    , Simple (fixed 10 "")
    , Simple "00"
    , Simple (fixed 10 "")
    , Simple (ackSenderQualifier settings)
    , Simple (fixed 15 (ackSenderId settings))
    , Simple (ackReceiverQualifier settings)
    , Simple (fixed 15 (ackReceiverId settings))
    , Simple (ackDateYYMMDD settings)
    , Simple (ackTimeHHMM settings)
    , Simple "U"
    , Simple (ackVersion settings)
    , Simple (fixedLeftZero 9 (ackInterchangeControlNumber settings))
    , Simple "0"
    , Simple "T"
    , Simple (T.singleton (componentSeparator (ackSyntax settings)))
    ]

ackGS :: AckSettings -> Segment
ackGS settings =
  seg
    "GS"
    [ Simple "FA"
    , Simple (ackSenderId settings)
    , Simple (ackReceiverId settings)
    , Simple (ackDateCCYYMMDD settings)
    , Simple (ackTimeHHMM settings)
    , Simple (ackGroupControlNumber settings)
    , Simple "X"
    , Simple (ackVersion settings)
    ]

ackGE :: AckSettings -> Int -> Segment
ackGE settings txSetCount =
  seg "GE" [Simple (intText txSetCount), Simple (ackGroupControlNumber settings)]

ackIEA :: AckSettings -> Segment
ackIEA settings =
  seg "IEA" [Simple "1", Simple (fixedLeftZero 9 (ackInterchangeControlNumber settings))]

seg :: Text -> [Element] -> Segment
seg tag elems = Segment tag (V.fromList elems)

fieldOrBlank :: Int -> Segment -> Text
fieldOrBlank ix segValue =
  case elementTextAt ix segValue of
    Just t -> t
    Nothing -> ""

intText :: Int -> Text
intText =
  toStrict . toLazyText . TBI.decimal

indexedControl :: Text -> Int -> Text
indexedControl base ix
  | ix == 0 = base
  | otherwise = intText (readIntDefault 0 base + ix)

readIntDefault :: Int -> Text -> Int
readIntDefault fallback t =
  case TR.decimal t of
    Right (n, rest) | T.null rest -> n
    _ -> fallback

fixed :: Int -> Text -> Text
fixed width t =
  T.take width (t <> T.replicate width " ")

fixedLeftZero :: Int -> Text -> Text
fixedLeftZero width t =
  T.takeEnd width (T.replicate width "0" <> t)
