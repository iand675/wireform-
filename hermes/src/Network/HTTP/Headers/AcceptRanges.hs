{-# LANGUAGE TemplateHaskell #-}

{- |
@Accept-Ranges@ is a response header by which a server advertises whether
it supports range requests on the target resource, naming the range units
it accepts (most commonly @bytes@) or the literal @none@ to signal that
range requests are not supported. Clients consult it to decide whether they
can fetch partial content, e.g. to resume an interrupted download.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-14.3>

== Grammar

@
Accept-Ranges     = acceptable-ranges
acceptable-ranges = 1#range-unit / \"none\"
range-unit        = bytes-unit / other-range-unit
@

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.ContentRange", "Network.HTTP.Headers.IfRange".
-}
module Network.HTTP.Headers.AcceptRanges (
  AcceptRanges (..),
  acceptRangesParser,
  renderAcceptRanges,
) where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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


  parseFromHeaders _ headers = case runParser acceptRangesParser (NE.head headers) of
    OK ar leftover
      | B.null (dropOws leftover) -> Right ar
      | otherwise ->
          Left ("Unconsumed input after parsing Accept-Ranges: " <> show leftover)
    Fail -> Left "Failed to parse Accept-Ranges header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderAcceptRanges


  headerName _ = hAcceptRanges


acceptRangesParser :: ParserT st String AcceptRanges
acceptRangesParser = do
  ows
  first <- rfc9110Token
  if ST.toString first == "none"
    then do
      ows
      pure AcceptRangesNone
    else do
      rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
      ows
      pure (AcceptRangesUnits (first :| rest))


renderAcceptRanges :: AcceptRanges -> M.Builder
renderAcceptRanges = \case
  AcceptRangesNone -> "none"
  AcceptRangesUnits us ->
    M.intersperse ", " (map R.shortText (NE.toList us))
