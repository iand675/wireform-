{-# LANGUAGE TemplateHaskell #-}

{- |
@Content-Range@ is a response header sent with a @206 Partial Content@
(or @416 Range Not Satisfiable@) reply to indicate which portion of the
selected representation the message body carries, together with the total
length of the complete representation. It is the server's answer to a
client's @Range@ request.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-14.4>

== Grammar

@
Content-Range       = range-unit SP range-resp
range-resp          = incl-range \"/\" ( complete-length / \"*\" )
                    / unsatisfied-range
incl-range          = first-pos \"-\" last-pos
unsatisfied-range   = \"*/\" complete-length
complete-length     = 1*DIGIT
@

The unit is retained as received. RFC 9110 also permits this field
on partial PUT requests by private agreement.

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.AcceptRanges", "Network.HTTP.Headers.IfRange".
-}
module Network.HTTP.Headers.ContentRange (
  ContentRange (..),
  RangeResp (..),
  contentRangeParser,
  renderContentRange,
  renderContentRangeChecked,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.Text.Short as ST
import Data.Word (Word64, Word8)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (poke)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentRange)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | The @range-resp@ payload: either a satisfied range with
known\\/unknown total length, or the unsatisfied form
@*\\/complete-length@ used on @416 Range Not Satisfiable@.
-}
data RangeResp
  = RangeRespSatisfied
      { rangeFirst :: !Word64
      , rangeLast :: !Word64
      , rangeTotal :: !(Maybe Word64)
      }
  | {- | For 416 responses, with the complete length in 'Just'.
    'Nothing' remains available for callers constructing legacy @*\\/*@
    values with the structural builder. Parsing and checked emission require
    a complete length.
    -}
    RangeRespUnsatisfied {rangeTotal :: !(Maybe Word64)}
  deriving stock (Eq, Show)


-- | @range-unit SP range-resp@.
data ContentRange = ContentRange
  { contentRangeUnit :: !ST.ShortText
  , contentRangeResp :: !RangeResp
  }
  deriving stock (Eq, Show)


instance KnownHeader ContentRange where
  type ParseFailure ContentRange = String
  type Cardinality ContentRange = 'ZeroOrOne
  type Direction ContentRange = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "Content-Range" contentRangeParser


  renderToHeaders _ = M.toStrictByteString . renderContentRange


  renderToHeadersChecked _ value = case invalidContentRange value of
    Nothing -> Just (encodeContentRange value)
    Just _ -> Nothing


  headerName _ = hContentRange


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

contentRangeParser :: ParserT st String ContentRange
contentRangeParser = do
  ows
  unit <- branch $(string "bytes ") (pure "bytes") (rfc9110Token <* $(char ' '))
  resp <- branch $(string "*/") unsatisfied satisfied
  pure ContentRange {contentRangeUnit = unit, contentRangeResp = resp}
  where
    position = boundedDecimalWord64 (err "Content-Range position or length exceeds Word64")
    unsatisfied = RangeRespUnsatisfied . Just <$> position

    satisfied = do
      a <- position
      $(char '-')
      b <- position
      if b < a then err "Content-Range ends before it starts" else pure ()
      $(char '/')
      total <- branch $(char '*') (pure Nothing) (Just <$> position)
      case total of
        Just len | len <= b -> err "Content-Range ends outside the complete length"
        _ -> pure (RangeRespSatisfied a b total)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render the structural value. For checked emission use 'renderContentRangeChecked'.
renderContentRange :: ContentRange -> M.Builder
renderContentRange (ContentRange unit resp) =
  R.shortText unit <> M.char7 ' ' <> renderResp resp
  where
    renderResp = \case
      RangeRespSatisfied a b mTot ->
        M.word64Dec a
          <> M.char7 '-'
          <> M.word64Dec b
          <> M.char7 '/'
          <> mTotal mTot
      RangeRespUnsatisfied mTot ->
        "*/" <> mTotal mTot
    mTotal Nothing = "*"
    mTotal (Just n) = M.word64Dec n


-- | Validate the unit, interval, and complete length before emission.
renderContentRangeChecked :: ContentRange -> Either String M.Builder
renderContentRangeChecked value = case invalidContentRange value of
  Nothing -> Right (renderContentRange value)
  Just message -> Left message


invalidContentRange :: ContentRange -> Maybe String
invalidContentRange (ContentRange unit response)
  | unit /= "bytes" && not (isHttpTokenText unit) = Just "Invalid Content-Range unit"
  | otherwise = case response of
      RangeRespUnsatisfied Nothing -> Just "Unsatisfied Content-Range requires a complete length"
      RangeRespSatisfied first lastByte total
        | lastByte < first -> Just "Content-Range ends before it starts"
        | Just len <- total, len <= lastByte -> Just "Content-Range ends outside the complete length"
      _ -> Nothing
{-# INLINE invalidContentRange #-}


encodeContentRange :: ContentRange -> ByteString
encodeContentRange (ContentRange unit response) = BSI.unsafeCreateUptoN capacity $ \start -> do
  SBSI.copyToPtr unitBytes 0 start unitSize
  poke (start `plusPtr` unitSize) (32 :: Word8)
  let rangeStart = start `plusPtr` (unitSize + 1)
  end <- case response of
    RangeRespSatisfied first lastByte total -> do
      dash <- Prim.runB Prim.word64Dec first rangeStart
      poke dash (45 :: Word8)
      slash <- Prim.runB Prim.word64Dec lastByte (dash `plusPtr` 1)
      poke slash (47 :: Word8)
      writeTotal (slash `plusPtr` 1) total
    RangeRespUnsatisfied total -> do
      poke rangeStart (42 :: Word8)
      poke (rangeStart `plusPtr` 1) (47 :: Word8)
      writeTotal (rangeStart `plusPtr` 2) total
  pure (end `minusPtr` start)
  where
    unitBytes = ST.toShortByteString unit
    unitSize = SBS.length unitBytes
    -- A Word64 has at most 20 digits. Include space and both delimiters.
    capacity = BSI.checkedAdd "Content-Range" unitSize $ case response of
      RangeRespSatisfied _ _ _ -> 63
      RangeRespUnsatisfied _ -> 23
    writeTotal ptr = \case
      Just value -> Prim.runB Prim.word64Dec value ptr
      Nothing -> do
        poke ptr (42 :: Word8)
        pure (ptr `plusPtr` 1)
