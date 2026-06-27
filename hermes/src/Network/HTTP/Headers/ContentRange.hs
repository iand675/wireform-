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

The @bytes@ form is by far the most common; we surface it
typed. Other units are exposed as raw bytes so callers can route
them through their own logic.

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.AcceptRanges", "Network.HTTP.Headers.IfRange".
-}
module Network.HTTP.Headers.ContentRange (
  ContentRange (..),
  RangeResp (..),
  contentRangeParser,
  renderContentRange,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word64)
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
  | -- | For 416 responses. @rangeTotal@ is 'Just' iff the server
    --     actually knew the complete length and sent it (the
    --     @*\\/N@ form); @Nothing@ if the server emitted the
    --     @*\\/*@ form (rare but seen in the wild).
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
  type Direction ContentRange = 'Response


  parseFromHeaders _ headers = case runParser contentRangeParser (NE.head headers) of
    OK cr leftover
      | B.null (dropOws leftover) -> Right cr
      | otherwise ->
          Left ("Unconsumed input after parsing Content-Range: " <> show leftover)
    Fail -> Left "Failed to parse Content-Range header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderContentRange


  headerName _ = hContentRange


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

contentRangeParser :: ParserT st String ContentRange
contentRangeParser = do
  ows
  unit <- rfc9110Token
  skipSome $(char ' ')
  resp <- unsatisfied <|> satisfied
  pure ContentRange {contentRangeUnit = unit, contentRangeResp = resp}
  where
    unsatisfied = do
      $(char '*')
      $(char '/')
      total <- (Nothing <$ $(char '*')) <|> (Just . fromIntegral <$> anyAsciiDecimalWord)
      pure (RangeRespUnsatisfied total)

    satisfied = do
      a <- anyAsciiDecimalWord
      $(char '-')
      b <- anyAsciiDecimalWord
      $(char '/')
      total <- (Nothing <$ $(char '*')) <|> (Just . fromIntegral <$> anyAsciiDecimalWord)
      pure
        RangeRespSatisfied
          { rangeFirst = fromIntegral a
          , rangeLast = fromIntegral b
          , rangeTotal = total
          }


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderContentRange :: ContentRange -> M.Builder
renderContentRange (ContentRange unit resp) =
  R.shortText unit <> M.char7 ' ' <> renderResp resp
  where
    renderResp = \case
      RangeRespSatisfied a b mTot ->
        M.wordDec (fromIntegral a)
          <> M.char7 '-'
          <> M.wordDec (fromIntegral b)
          <> M.char7 '/'
          <> mTotal mTot
      RangeRespUnsatisfied mTot ->
        "*/" <> mTotal mTot
    mTotal Nothing = "*"
    mTotal (Just n) = M.wordDec (fromIntegral n)
