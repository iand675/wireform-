-- | Shared storage and field codecs for nonempty lists that retain wire bytes.
module Network.HTTP.Headers.RetainedList (
  RetainedList (..),
  fieldLines,
  legacyText,
  decodeList,
  listEntries,
  listFromEntries,
  listParser,
  renderList,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (opaqueShortText, sepByCommas1, shortText)
import qualified Network.HTTP.Headers.Mason as M


data RetainedList a
  = RawList !ST.ShortText
  | DecodedList !(NonEmpty ByteString) !(NonEmpty a)


instance Eq (RetainedList a) where
  left == right = fieldLines left == fieldLines right


fieldLines :: RetainedList a -> NonEmpty ByteString
fieldLines (RawList text) = ST.toByteString text :| []
fieldLines (DecodedList wire _) = wire


legacyText :: RetainedList a -> ST.ShortText
legacyText (RawList text) = text
legacyText (DecodedList wire _) = opaqueShortText (SBS.toShort (combineFieldLines "," wire))


decodeList :: String -> ParserT st String [a] -> NonEmpty ByteString -> Either String (RetainedList a)
decodeList name item wire@(bytes :| []) =
  DecodedList wire <$> parseFieldLine name (nonEmptyList name item) bytes
decodeList name item wire =
  DecodedList wire <$> (parseListFieldLines name item wire >>= requireNonEmptyList name)
{-# INLINE decodeList #-}


listEntries :: String -> ParserT st String [a] -> RetainedList a -> Either String (NonEmpty a)
listEntries name item (RawList text) = reparseEntries name item text
listEntries _ _ (DecodedList _ entries) = Right entries
{-# INLINE listEntries #-}


reparseEntries :: String -> ParserT st String [a] -> ST.ShortText -> Either String (NonEmpty a)
reparseEntries name item text = parseFieldLine name (nonEmptyList name item) (ST.toByteString text)
{-# NOINLINE reparseEntries #-}


listFromEntries
  :: Eq a
  => String
  -> ParserT st String [a]
  -> (a -> M.Builder)
  -> NonEmpty a
  -> Either String (RetainedList a)
listFromEntries name item render entries = do
  let bytes = M.toStrictByteString (sepByCommas1 (fmap render entries))
  parsed <- parseFieldLine name (nonEmptyList name item) bytes
  if parsed == entries
    then Right (DecodedList (bytes :| []) parsed)
    else Left (name <> " entries do not match their wire representation")


listParser :: String -> ParserT st String [a] -> ParserT st String (RetainedList a)
listParser name item = withByteString (nonEmptyList name item) $ \entries wire ->
  pure (DecodedList (wire :| []) entries)


renderList :: RetainedList a -> M.Builder
renderList (RawList text) = shortText text
renderList (DecodedList (wire :| []) _) = M.byteString wire
renderList (DecodedList (first :| rest) _) =
  M.byteString first <> foldMap (("," <>) . M.byteString) rest


nonEmptyList :: String -> ParserT st String [a] -> ParserT st String (NonEmpty a)
nonEmptyList name item = item >>= either err pure . requireNonEmptyList name
{-# INLINE nonEmptyList #-}
