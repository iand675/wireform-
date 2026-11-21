{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2324 §2.2.2.1 @Accept-Additions@ — HTCPCP request header listing the
additions (milk, syrup, sweetener, spice, alcohol, …) acceptable in the
brewed response.

== Grammar

@
Accept-Additions = #( addition-range [ accept-params ] )
addition-range   = token *( \";\" parameter )
accept-params    = \";q=\" qvalue *( \";\" parameter )
parameter       = token [ \"=\" ( token | quoted-string ) ]
@

The first @q@ parameter is a quality weight. The codec keeps the spelling
of names and weights, supports HTCPCP-TEA additions and extension tokens,
and preserves quoted octets using a one-to-one Latin-1 mapping.

Spec: <https://www.rfc-editor.org/rfc/rfc2324#section-2.2.2.1>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptFeatures", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.TCN".
-}
module Network.HTTP.Headers.AcceptAdditions (
  AcceptAdditions (..),
  Addition (..),
  AdditionParam (..),
  additionQuality,
  acceptAdditionsParser,
  renderAcceptAdditions,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import Data.List (find)
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (isQualityValueText, qualityValueParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptAdditions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Wireform.Encode.Direct (dWord8)


-- | A @;@-separated parameter attached to an addition.
data AdditionParam = AdditionParam
  { paramName :: !ST.ShortText
  , paramValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | An addition-type token plus its parameters.
data Addition = Addition
  { additionType :: !ST.ShortText
  , additionParams :: ![AdditionParam]
  }
  deriving stock (Eq, Show)


-- | The comma-separated list of additions carried by an @Accept-Additions@ header.
newtype AcceptAdditions = AcceptAdditions {acceptAdditions :: [Addition]}
  deriving stock (Eq, Show)


instance KnownHeader AcceptAdditions where
  type ParseFailure AcceptAdditions = String
  type Cardinality AcceptAdditions = 'ZeroOrOne
  type Direction AcceptAdditions = 'Request


  parseFromHeaders _ = parseFieldLine "Accept-Additions" acceptAdditionsParser . combineFieldLines ", "


  renderToHeaders _ = M.toStrictByteString . renderAcceptAdditions


  renderToHeadersChecked _ (AcceptAdditions entries) = R.encodeCommaSeparatedChecked bound write entries
    where
      bound (Addition ty ps)
        | isHttpTokenText ty = measure (width ty) False ps
        | otherwise = -1
      measure !size _ [] = size
      measure !size weighted (AdditionParam key value : rest)
        | not (isHttpTokenText key) = -1
        | not weighted && isQuality key = case value of
            Just wire | isQualityValueText wire -> measure (add (add nameEnd 1) (width wire)) True rest
            _ -> -1
        | otherwise = case value of
            Nothing -> measure nameEnd weighted rest
            Just text | isHttpQuotedText text ->
              measure (add (add nameEnd 3) (add (width text) (width text))) weighted rest
            _ -> -1
        where
          nameEnd = add (add size 1) (width key)
      add = BSI.checkedAdd "Accept-Additions"
      width = SBS.length . ST.toShortByteString
      write ptr start (Addition ty ps) = R.copyText ptr start ty >>= writeParams ptr ps
      writeParams _ [] !offset = pure offset
      writeParams ptr (AdditionParam key value : rest) !offset = do
        start <- dWord8 ptr offset 59
        nameEnd <- R.copyText ptr start key
        end <- case value of
          Nothing -> pure nameEnd
          Just text -> dWord8 ptr nameEnd 61 >>= \begin -> R.writeParameterValue ptr begin text
        writeParams ptr rest end


  headerName _ = hAcceptAdditions


acceptAdditionsParser :: ParserT st String AcceptAdditions
acceptAdditionsParser = AcceptAdditions <$> httpList addition
  where
    addition = Addition <$> rfc9110Token <*> parameters False
    parameters weighted =
      withOption (ows *> $(char ';') *> ows) (const (parameter weighted)) (pure [])
    parameter weighted = do
      name <- rfc9110Token
      if not weighted && isQuality name
        then do
          wire <- $(char '=') *> shortASCIIFromParser_ (qualityValueParser <* notFollowedBy tokenBytes)
          (AdditionParam name (Just wire) :) <$> parameters True
        else do
          value <- optional ($(char '=') *> (rfc9110Token <|> (shortTextFromLatin1 <$> httpQuotedBytes)))
          (AdditionParam name value :) <$> parameters weighted


-- | The first explicit quality weight. A missing weight defaults to one
-- when interpreting the addition's preference.
additionQuality :: Addition -> Maybe Double
additionQuality addition = do
  parameter <- find (isQuality . paramName) (additionParams addition)
  wire <- paramValue parameter
  either (const Nothing) Just (parseFieldLine "Accept-Additions quality" qualityValueParser (ST.toByteString wire))


isQuality :: ST.ShortText -> Bool
isQuality name = name == "q" || name == "Q"


renderAcceptAdditions :: AcceptAdditions -> M.Builder
renderAcceptAdditions (AcceptAdditions as) = R.renderSeparated ", " renderAddition as
  where
    renderAddition (Addition ty ps) = R.shortText ty <> foldMap renderParam ps
    renderParam (AdditionParam name Nothing) = M.char7 ';' <> R.shortText name
    renderParam (AdditionParam name (Just v)) =
      M.char7 ';' <> R.shortText name <> M.char7 '=' <> renderTokenOrQuoted v


{- | Render a value as a bare token when it is token-safe, otherwise as a
quoted-string with the mandatory escaping of @\"@ and @\\@.
-}
renderTokenOrQuoted :: ST.ShortText -> M.Builder
renderTokenOrQuoted t
  | not (ST.null t) && ST.all isTokenChar t = R.shortText t
  | SBS.all plainQuotedByte bytes = M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
  | otherwise = R.renderHttpQuotedString (ST.toText t)
  where
    bytes = ST.toShortByteString t
    plainQuotedByte byte = byte == 9 || (byte >= 32 && byte <= 126 && byte /= 34 && byte /= 92)
