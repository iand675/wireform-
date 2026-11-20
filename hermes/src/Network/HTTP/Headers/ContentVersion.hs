{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Content-Version@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.2) carrying
the version tag of an evolving entity, intended to support collaborative
authoring together with @Derived-From@.

== Grammar (RFC 2068 §19.6.2.2)

@
Content-Version = \"Content-Version\" \":\" quoted-string
@

The RFC 9110 quoted-string codec retains the tag's decoded octets. Rendering
quotes DQUOTE and backslash octets as needed.

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.2>

See also: "Network.HTTP.Headers.DerivedFrom", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.URI".
-}
module Network.HTTP.Headers.ContentVersion (
  ContentVersion (ContentVersion, ContentVersionBytes, contentVersionTag, contentVersionTagBytes),
  contentVersionParser,
  renderContentVersion,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes, opaqueShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The decoded tag octets. Construct with valid HTTP quoted-string octets.
newtype ContentVersion = ContentVersionBytes {contentVersionTagBytes :: SBS.ShortByteString}
  deriving stock (Eq, Show)


{- | The text interface constructs UTF-8 tag octets. The view uses Latin-1
for invalid UTF-8. Use 'contentVersionTagBytes' to copy a received tag.
-}
pattern ContentVersion :: ST.ShortText -> ContentVersion
pattern ContentVersion {contentVersionTag} <- ContentVersionBytes (opaqueShortText -> contentVersionTag)
  where
    ContentVersion tag = ContentVersionBytes (ST.toShortByteString tag)


{-# COMPLETE ContentVersion #-}


instance KnownHeader ContentVersion where
  type ParseFailure ContentVersion = String
  type Cardinality ContentVersion = 'ZeroOrOne
  type Direction ContentVersion = 'RequestAndResponse


  parseFromHeaders _ = parseQuotedFieldLine "Content-Version" (ContentVersionBytes . SBS.toShort)


  renderToHeaders _ = encodeHttpQuotedBytes . contentVersionTagBytes


  headerName _ = hContentVersion


contentVersionParser :: ParserT st String ContentVersion
contentVersionParser = ows *> (ContentVersionBytes . SBS.toShort <$> httpQuotedBytes)


renderContentVersion :: ContentVersion -> M.Builder
renderContentVersion = M.byteString . encodeHttpQuotedBytes . contentVersionTagBytes
