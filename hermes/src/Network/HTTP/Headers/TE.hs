{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
RFC 9112 §7.4 @TE@ — request header announcing the transfer codings
(beyond the implicit @chunked@) that the client is willing to accept in
the response, plus the @trailers@ keyword indicating willingness to
accept trailer fields.

== Grammar

@
TE              = #t-codings
t-codings       = "trailers" / ( transfer-coding [ weight ] )
transfer-coding = token *( OWS ";" OWS transfer-parameter )
weight          = OWS ";" OWS "q=" qvalue
@

Each entry includes its transfer parameters and an optional quality
weight. The @trailers@ keyword has neither parameters nor a weight.

Spec: <https://www.rfc-editor.org/rfc/rfc9112#section-7.4>

See also: "Network.HTTP.Headers.TransferEncoding", "Network.HTTP.Headers.Trailer",
"Network.HTTP.Headers.Connection", "Network.HTTP.Headers.AcceptEncoding".
-}
module Network.HTTP.Headers.TE (
  TE (..),
  TECoding (..),
  teCodingParams,
  teParser,
  renderTE,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (qualityParameter, qualityValueParser, renderQualityValue)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTE)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)
import Network.HTTP.Headers.TransferEncoding (
  TransferParameter (..),
  encodeTransferCodingsChecked,
  renderTransferParameter,
  transferCodingIs,
  transferCodingNameParser,
  transferCodingSupportsParameters,
  transferParameterParser,
 )


-- | A single @t-codings@ entry: a transfer-coding name with an optional q weight.
data TECoding
  = TECoding
      { teCodingName :: !ST.ShortText
      , teCodingWeight :: !(Maybe Double)
      }
  | TEParameterizedCoding
      { teCodingName :: !ST.ShortText
      , teCodingWeight :: !(Maybe Double)
      , teCodingParameters :: !(NonEmpty TransferParameter)
      }
  deriving stock (Eq, Show)


-- | A @TE@ list. 'TEEmpty' represents an explicitly empty field.
data TE = TE {teCodings :: NonEmpty TECoding} | TEEmpty
  deriving stock (Eq, Show)


instance Semigroup TE where
  TEEmpty <> other = other
  other <> TEEmpty = other
  TE xs <> TE ys = TE (xs <> ys)


teCodingParams :: TECoding -> [TransferParameter]
teCodingParams (TECoding _ _) = []
teCodingParams (TEParameterizedCoding _ _ params) = NE.toList params


instance KnownHeader TE where
  type ParseFailure TE = String
  type Cardinality TE = 'ZeroOrMore
  type Direction TE = 'Request


  parseFromHeaders _ = fmap fromCodings . parseListFieldLines "TE" (httpList teCodingParser)
  renderToHeaders _ = pure . M.toStrictByteString . renderTE
  renderToHeadersChecked _ TEEmpty = Just [""]
  renderToHeadersChecked _ (TE codings)
    | all validTECoding codings =
        pure <$> encodeTransferCodingsChecked teCodingName teCodingParams teCodingWeight (NE.toList codings)
    | otherwise = Nothing
  headerName _ = hTE


validTECoding :: TECoding -> Bool
validTECoding (TECoding name weight)
  | transferCodingIs "chunked" name = False
  | transferCodingIs "trailers" name = case weight of
      Nothing -> True
      Just _ -> False
  | otherwise = True
validTECoding (TEParameterizedCoding name _ parameters) =
  not (transferCodingIs "trailers" name)
    && transferCodingSupportsParameters name
    && all (not . transferCodingIs "q" . transferParameterName) parameters
{-# INLINE validTECoding #-}


teParser :: ParserT st String TE
teParser = fromCodings <$> httpList teCodingParser


fromCodings :: [TECoding] -> TE
fromCodings = maybe TEEmpty TE . NE.nonEmpty


teCodingParser :: ParserT st String TECoding
teCodingParser = do
  name <- transferCodingNameParser
  if transferCodingIs "chunked" name
    then err "TE cannot advertise chunked"
    else
      if transferCodingIs "trailers" name
        then pure (TECoding name Nothing)
        else parameters name []
  where
    parameters name reversed =
      withOption
        (ows *> $(char ';') *> ows)
        (\() -> withOption qualityParameter readWeight readParameter)
        (pure (coding Nothing))
      where
        coding weight = case NE.nonEmpty (reverse reversed) of
          Nothing -> TECoding name weight
          Just params -> TEParameterizedCoding name weight params
        readWeight () = coding . Just <$> qualityValueParser
        readParameter = do
          param <- transferParameterParser
          if transferCodingIs "q" (transferParameterName param)
            then err "TE weight must use q= followed by a qvalue"
            else
              if transferCodingSupportsParameters name
                then parameters name (param : reversed)
                else err "TE coding does not allow parameters"


renderTE :: TE -> M.Builder
renderTE TEEmpty = mempty
renderTE (TE codings) = sepByCommas1 (fmap renderTECoding codings)


renderTECoding :: TECoding -> M.Builder
renderTECoding coding =
  shortText (teCodingName coding)
    <> foldMap renderTransferParameter (teCodingParams coding)
    <> maybe mempty (\weight -> ";q=" <> renderQualityValue weight) (teCodingWeight coding)
