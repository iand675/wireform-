{- |
Module      : Network.HTTP.Headers.SetCookie2
Description : The obsolete @Set-Cookie2@ response header (RFC 2965)

The @Set-Cookie2@ response header was defined by RFC 2965 (HTTP State
Management Mechanism, version 1):

> set-cookie    = "Set-Cookie2:" cookies
> cookies       = 1#cookie
> cookie        = NAME "=" VALUE *(";" set-cookie-av)
> set-cookie-av = "Comment" "=" value
>               | "CommentURL" "=" <"> http_URL <">
>               | "Discard"
>               | "Domain" "=" value
>               | "Max-Age" "=" value
>               | "Path" "=" value
>               | "Port" [ "=" <"> portlist <"> ]
>               | "Secure"
>               | "Version" "=" 1*DIGIT

For example: @Set-Cookie2: Customer="WILE_E_COYOTE"; Version="1"; Path="/acme"@.

An origin server may emit multiple @Set-Cookie2@ header lines per response, so
the header is modelled with cardinality 'ZeroOrMore' via the 'SetCookie2s'
wrapper (mirroring "Network.HTTP.Headers.SetCookie").

RFC 2965 has been moved to Historic and obsoleted by RFC 6265, which
explicitly deprecates the @Cookie2@ and @Set-Cookie2@ header fields. Because
this header is obsolete and never saw wide deployment, this module follows the
project's depth guidance for historic headers and preserves each header line
faithfully as a raw 'ST.ShortText' rather than fabricating a dead structured
grammar.

Spec: <https://www.rfc-editor.org/rfc/rfc2965#section-3.2.2>

See also: "Network.HTTP.Headers.SetCookie", "Network.HTTP.Headers.Cookie2", "Network.HTTP.Headers.Cookie".
-}
module Network.HTTP.Headers.SetCookie2 (
  SetCookie2 (..),
  SetCookie2s (..),
  setCookie2Parser,
  renderSetCookie2,
) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSetCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A single obsolete @Set-Cookie2@ header line. The wrapped value is the raw
cookie definition (e.g. @Customer="WILE_E_COYOTE"; Version="1"; Path="/acme"@),
preserved verbatim.
-}
newtype SetCookie2 = SetCookie2 {setCookie2Value :: ST.ShortText}
  deriving stock (Eq, Show)


-- | Wrapper for the (possibly repeated) @Set-Cookie2@ response header lines.
newtype SetCookie2s = SetCookie2s {getSetCookie2s :: [SetCookie2]}
  deriving stock (Eq, Show)


instance KnownHeader SetCookie2s where
  type ParseFailure SetCookie2s = String
  type Cardinality SetCookie2s = 'ZeroOrMore
  type Direction SetCookie2s = 'Response


  parseFromHeaders _ headers = do
    cookies <- traverse parseSingleSetCookie2 (NE.toList headers)
    pure $ SetCookie2s cookies


  renderToHeaders _ (SetCookie2s cookies) = map (M.toStrictByteString . renderSetCookie2) cookies


  headerName _ = hSetCookie2


parseSingleSetCookie2 :: B.ByteString -> Either String SetCookie2
parseSingleSetCookie2 header = case runParser setCookie2Parser header of
  OK cookie "" -> Right cookie
  OK _ rest -> Left $ "Unconsumed input after parsing Set-Cookie2 header: " <> show rest
  Fail -> Left "Failed to parse Set-Cookie2 header"
  Err e -> Left e


setCookie2Parser :: ParserT st String SetCookie2
setCookie2Parser = SetCookie2 <$> takeRestShortText


renderSetCookie2 :: SetCookie2 -> M.Builder
renderSetCookie2 (SetCookie2 value) = shortText value
