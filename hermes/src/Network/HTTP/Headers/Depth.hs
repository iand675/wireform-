{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 4918 §10.2 @Depth@ request header — controls how far a WebDAV
operation (e.g. @PROPFIND@, @COPY@, @MOVE@, @LOCK@) applies through a
collection hierarchy.

== Grammar

@
Depth = "0" | "1" | "infinity"
@

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.2>

See also: "Network.HTTP.Headers.DAV", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.Overwrite", "Network.HTTP.Headers.If", "Network.HTTP.Headers.LockToken".
-}
module Network.HTTP.Headers.Depth (
  Depth (..),
  depthParser,
  renderDepth,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDepth)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The three permitted @Depth@ values.
data Depth = Depth0 | Depth1 | DepthInfinity
  deriving stock (Eq, Show)


instance KnownHeader Depth where
  type ParseFailure Depth = String
  type Cardinality Depth = 'ZeroOrOne
  type Direction Depth = 'Request


  parseFromHeaders _ headers = case runParser depthParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Depth header: " <> show rest
    Fail -> Left "Failed to parse Depth header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDepth


  headerName _ = hDepth


depthParser :: ParserT st String Depth
depthParser =
  $( switch
      [|
        case _ of
          "0" -> pure Depth0
          "1" -> pure Depth1
          "infinity" -> pure DepthInfinity
        |]
   )


renderDepth :: Depth -> M.Builder
renderDepth = \case
  Depth0 -> "0"
  Depth1 -> "1"
  DepthInfinity -> "infinity"
