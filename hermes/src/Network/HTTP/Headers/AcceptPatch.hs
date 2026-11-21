{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
RFC 5789 §3.1 @Accept-Patch@ — a response header by which a server
advertises the set of media types it accepts in a @PATCH@ request body.
Typically returned in response to @OPTIONS@.

== Grammar

@
Accept-Patch = \"Accept-Patch\" \":\" 1#media-type
@

Each entry preserves its media type and parameters. The value is
comma-combinable across multiple header lines.

Spec: <https://datatracker.ietf.org/doc/html/rfc5789#section-3.1>

See also: "Network.HTTP.Headers.AcceptPost", "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.AcceptPatch (
  AcceptPatch (AcceptPatch, AcceptPatchRanges, acceptPatchMediaTypes, acceptPatchMediaRanges),
  acceptPatchParser,
  renderAcceptPatch,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType, encodeMediaTypesChecked, mediaTypeWithParametersParser, renderMediaRange)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptPatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)


-- | A non-empty list of acceptable media types for @PATCH@ bodies.
newtype AcceptPatch = AcceptPatchRanges {acceptPatchMediaRanges :: NE.NonEmpty MediaRange}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


-- | Construct a list of bare media types or project the media type of each entry.
pattern AcceptPatch :: NE.NonEmpty MediaType -> AcceptPatch
pattern AcceptPatch {acceptPatchMediaTypes} <- AcceptPatchRanges (fmap mediaType -> acceptPatchMediaTypes)
  where
    AcceptPatch types = AcceptPatchRanges (fmap (\mt -> MediaRange mt []) types)


{-# COMPLETE AcceptPatch #-}


instance KnownHeader AcceptPatch where
  type ParseFailure AcceptPatch = String
  type Cardinality AcceptPatch = 'ZeroOrMore
  type Direction AcceptPatch = 'Response


  parseFromHeaders _ headers = do
    entries <- parseListFieldLines "Accept-Patch" mediaList headers
    maybe (Left "Accept-Patch requires at least one media type") (Right . AcceptPatchRanges) (NE.nonEmpty entries)


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptPatch


  renderToHeadersChecked _ (AcceptPatchRanges ranges) = pure <$> encodeMediaTypesChecked (NE.toList ranges)


  headerName _ = hAcceptPatch


acceptPatchParser :: ParserT st String AcceptPatch
acceptPatchParser = mediaList >>= maybe failed (pure . AcceptPatchRanges) . NE.nonEmpty


mediaList :: ParserT st String [MediaRange]
mediaList = httpList mediaTypeWithParametersParser


renderAcceptPatch :: AcceptPatch -> M.Builder
renderAcceptPatch = sepByCommas1 . fmap renderMediaRange . acceptPatchMediaRanges
