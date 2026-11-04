{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Ion 1.0 symbol IDs. Decoder slots preserve gaps and duplicates;
encoder interning chooses the lowest ID, including system symbols.
-}
module Ion.SymbolTable (
  SymbolTable (..),
  systemTable,
  lookupSymbol,
  systemSymbols,
  EncoderSymbols,
  emptyEncoder,
  intern,
  internID,
  symbolID,
  encoderSymbols,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV


data SymbolTable = SymbolTable
  { localMinID :: {-# UNPACK #-} !Int
  , localSymbols :: !(Vector (Maybe Text))
  }
  deriving stock (Show, Eq)


systemSymbols :: Vector Text
systemSymbols =
  V.fromList
    [ "$ion"
    , "$ion_1_0"
    , "$ion_symbol_table"
    , "name"
    , "version"
    , "imports"
    , "symbols"
    , "max_id"
    , "$ion_shared_symbol_table"
    ]


systemTable :: SymbolTable
systemTable = SymbolTable 10 V.empty


lookupSymbol :: SymbolTable -> Int -> Maybe Text
lookupSymbol (SymbolTable base slots) sid
  | sid > 0 && sid <= 9 = Just (V.unsafeIndex systemSymbols (sid - 1))
  | sid >= base && sid - base < V.length slots = V.unsafeIndex slots (sid - base)
  | otherwise = Nothing
{-# INLINE lookupSymbol #-}


newtype EncoderSymbols = EncoderSymbols (HashMap Text Int)


emptyEncoder :: EncoderSymbols
emptyEncoder = EncoderSymbols systemIDs


systemIDs :: HashMap Text Int
systemIDs = HM.fromList (V.toList (V.imap (\i t -> (t, i + 1)) systemSymbols))


intern :: Text -> EncoderSymbols -> EncoderSymbols
intern text st@(EncoderSymbols symbols)
  | HM.member text symbols = st
  | otherwise = EncoderSymbols (HM.insert text (1 + HM.size symbols) symbols)
{-# INLINE intern #-}


{- | Assign or find a SID while threading the registry, with no pair on
the heap for the sizing pass. The Int is the same object stored in the
map, rather than another boxed result per field.
-}
internID :: Text -> EncoderSymbols -> (# Int, EncoderSymbols #)
internID text st@(EncoderSymbols symbols) = case HM.lookup text symbols of
  Just sid -> (# sid, st #)
  Nothing ->
    let !sid = 1 + HM.size symbols
    in (# sid, EncoderSymbols (HM.insert text sid symbols) #)
{-# INLINE internID #-}


symbolID :: EncoderSymbols -> Text -> Int
symbolID (EncoderSymbols symbols) text = case HM.lookup text symbols of
  Just sid -> sid
  Nothing -> error "Ion.Encode: symbol was not interned"
{-# INLINE symbolID #-}


{- | One exact-size vector in SID order, without a cons cell per symbol
or quadratic vector snoc while collecting a large document.
-}
encoderSymbols :: EncoderSymbols -> Vector Text
encoderSymbols (EncoderSymbols symbols) = V.create $ do
  output <- MV.unsafeNew (HM.size symbols - 9)
  HM.foldrWithKey
    (\text sid rest -> if sid >= 10 then MV.unsafeWrite output (sid - 10) text >> rest else rest)
    (pure ())
    symbols
  pure output
