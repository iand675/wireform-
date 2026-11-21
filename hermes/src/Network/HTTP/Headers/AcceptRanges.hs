{-# LANGUAGE MagicHash #-}

{- |
@Accept-Ranges@ is a response header by which a server advertises whether
it supports range requests on the target resource, naming the range units
it accepts (most commonly @bytes@) or the literal @none@ to signal that
range requests are not supported. Clients consult it to decide whether they
can fetch partial content, e.g. to resume an interrupted download.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-14.3>

== Grammar

@
Accept-Ranges = 1#range-unit
range-unit    = token
@

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.ContentRange", "Network.HTTP.Headers.IfRange".
-}
module Network.HTTP.Headers.AcceptRanges (
  AcceptRanges (..),
  acceptRangesParser,
  renderAcceptRanges,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (indexWord8OffAddrAsWord32#)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Ptr (Ptr (..))
import GHC.Word (Word32 (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptRanges)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


{- | @AcceptRangesNone@ corresponds to the literal @\"none\"@ which
explicitly disables range requests (RFC 9110 §14.3). All other
values surface as a non-empty list of range-unit tokens.
-}
data AcceptRanges
  = AcceptRangesNone
  | AcceptRangesUnits !(NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader AcceptRanges where
  type ParseFailure AcceptRanges = String
  type Cardinality AcceptRanges = 'ZeroOrOne
  type Direction AcceptRanges = 'Response


  parseFromHeaders _ (wire :| [])
    | wire == "bytes" = Right (AcceptRangesUnits ("bytes" :| []))
    | isBareNone wire = Right AcceptRangesNone
  parseFromHeaders _ fields = parseRangeFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ AcceptRangesNone = "none"
  renderToHeaders _ (AcceptRangesUnits units) = R.encodeCommaSeparatedText units


  renderToHeadersChecked _ AcceptRangesNone = Just "none"
  renderToHeadersChecked _ (AcceptRangesUnits (unit :| []))
    | unit == "bytes" = Just "bytes"
    | isHttpTokenText unit = Just (ST.toByteString unit)
    | otherwise = Nothing
  renderToHeadersChecked _ (AcceptRangesUnits units)
    | all (\unit -> isHttpTokenText unit && not (isNone unit)) units = Just (R.encodeCommaSeparatedText units)
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hAcceptRanges


parseRangeFields :: NonEmpty ByteString -> Either String AcceptRanges
parseRangeFields fields =
  parseListFieldLines "Accept-Ranges" (httpList rfc9110Token) fields >>= classifyRanges
{-# NOINLINE parseRangeFields #-}


-- Read the exact four-letter token with ASCII case folding.
isBareNone :: ByteString -> Bool
isBareNone (BSI.BS input 4) = BSI.accursedUnutterablePerformIO $ unsafeWithForeignPtr input $ \(Ptr ptr) ->
  pure $! W32# (indexWord8OffAddrAsWord32# ptr 0#) .|. 0x20202020 == none
  where
    none = case targetByteOrder of
      BigEndian -> 0x6e6f6e65
      LittleEndian -> 0x656e6f6e
isBareNone _ = False
{-# INLINE isBareNone #-}


acceptRangesParser :: ParserT st String AcceptRanges
acceptRangesParser = do
  tokens <- httpList rfc9110Token
  either err pure (classifyRanges tokens)


classifyRanges :: [ST.ShortText] -> Either String AcceptRanges
classifyRanges [] = Left "Accept-Ranges requires at least one range unit"
classifyRanges [unit] | isNone unit = Right AcceptRangesNone
classifyRanges (unit : rest)
  | isNone unit || any isNone rest = Left "Accept-Ranges none cannot be combined with a range unit"
  | otherwise = Right (AcceptRangesUnits (unit :| rest))


isNone :: ST.ShortText -> Bool
isNone token =
  SBS.length bytes == 4
    && lowerAt 0 == 110
    && lowerAt 1 == 111
    && lowerAt 2 == 110
    && lowerAt 3 == 101
  where
    bytes = ST.toShortByteString token
    lowerAt i = SBS.index bytes i .|. 0x20


renderAcceptRanges :: AcceptRanges -> M.Builder
renderAcceptRanges = \case
  AcceptRangesNone -> "none"
  AcceptRangesUnits us ->
    M.intersperse ", " (map R.shortText (NE.toList us))
