{-# LANGUAGE PatternSynonyms #-}

{- |
@Derived-From@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.3) indicating
the version tag of the resource an enclosed entity was derived from before the
sender's modifications; required on PUT/PATCH when the prior retrieval carried
a @Content-Version@.

== Grammar (RFC 2068 §19.6.2.3)

@
Derived-From = \"Derived-From\" \":\" quoted-string
@

As with @Content-Version@, the RFC 9110 quoted-string codec preserves the
decoded tag octets.

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.3>

See also: "Network.HTTP.Headers.ContentVersion", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfMatch".
-}
module Network.HTTP.Headers.DerivedFrom (
  DerivedFrom (DerivedFrom, DerivedFromBytes, derivedFromTag, derivedFromTagBytes),
  derivedFromParser,
  renderDerivedFrom,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentVersion (ContentVersion (ContentVersion, ContentVersionBytes), contentVersionParser, renderContentVersion)
import Network.HTTP.Headers.HeaderFieldName (hDerivedFrom)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Derived-From@ value carrying the same opaque tag as @Content-Version@.
newtype DerivedFrom = DerivedFromVersion ContentVersion
  deriving stock (Eq, Show)


-- | The original text interface. The constructor encodes its tag as UTF-8.
pattern DerivedFrom :: ST.ShortText -> DerivedFrom
pattern DerivedFrom {derivedFromTag} = DerivedFromVersion (ContentVersion derivedFromTag)


-- | The decoded tag octets. Use this when copying a received tag verbatim.
pattern DerivedFromBytes :: SBS.ShortByteString -> DerivedFrom
pattern DerivedFromBytes {derivedFromTagBytes} = DerivedFromVersion (ContentVersionBytes derivedFromTagBytes)


{-# COMPLETE DerivedFrom #-}


{-# COMPLETE DerivedFromBytes #-}


instance KnownHeader DerivedFrom where
  type ParseFailure DerivedFrom = String
  type Cardinality DerivedFrom = 'ZeroOrOne
  type Direction DerivedFrom = 'Request


  parseFromHeaders _ = parseQuotedFieldLine "Derived-From" (DerivedFromBytes . SBS.toShort)


  renderToHeaders _ = encodeHttpQuotedBytes . derivedFromTagBytes


  headerName _ = hDerivedFrom


derivedFromParser :: ParserT st String DerivedFrom
derivedFromParser = DerivedFromVersion <$> contentVersionParser


renderDerivedFrom :: DerivedFrom -> M.Builder
renderDerivedFrom (DerivedFromVersion version) = renderContentVersion version
