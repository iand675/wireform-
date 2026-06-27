{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 5789 §3.1 @Accept-Patch@ — a response header by which a server
advertises the set of media types it accepts in a @PATCH@ request body.
Typically returned in response to @OPTIONS@.

== Grammar

@
Accept-Patch = \"Accept-Patch\" \":\" 1#media-type
@

Each entry is reduced to its @type/subtype@ core via 'MediaType'
(see "Network.HTTP.ContentNegotiation"); media-type parameters are not
surfaced. The value is comma-combinable across multiple header lines.

Spec: <https://datatracker.ietf.org/doc/html/rfc5789#section-3.1>

See also: "Network.HTTP.Headers.AcceptPost", "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.AcceptPatch (
  AcceptPatch (..),
  acceptPatchParser,
  renderAcceptPatch,
) where

import Control.Monad.Combinators.NonEmpty
import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentNegotiation (MediaType, mediaTypeParser, renderMediaType)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptPatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)


-- | A non-empty list of acceptable media types for @PATCH@ bodies.
newtype AcceptPatch = AcceptPatch {acceptPatchMediaTypes :: NE.NonEmpty MediaType}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader AcceptPatch where
  type ParseFailure AcceptPatch = String
  type Cardinality AcceptPatch = 'ZeroOrMore
  type Direction AcceptPatch = 'Response


  parseFromHeaders _ headers = do
    res <- traverse runAcceptPatchParser headers
    pure (fold1 res)


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptPatch


  headerName _ = hAcceptPatch


runAcceptPatchParser :: B.ByteString -> Either String AcceptPatch
runAcceptPatchParser bs = case runParser acceptPatchParser bs of
  OK v leftover
    | B.null (dropOws leftover) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Accept-Patch header: " <> show leftover)
  Fail -> Left "Failed to parse Accept-Patch header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


acceptPatchParser :: ParserT st String AcceptPatch
acceptPatchParser =
  AcceptPatch <$> (ows *> mediaTypeParser `sepBy1` (ows *> $(char ',') *> ows))


renderAcceptPatch :: AcceptPatch -> M.Builder
renderAcceptPatch = sepByCommas1 . fmap renderMediaType . acceptPatchMediaTypes
