{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@X-Frame-Options@ — a legacy response header controlling whether a
browser may render the page inside a @\<frame>@, @\<iframe>@,
@\<embed>@ or @\<object>@, used to defend against clickjacking.

== Grammar

@
X-Frame-Options = "DENY"
                / "SAMEORIGIN"
                / ( "ALLOW-FROM" RWS serialized-origin )
@

@DENY@ and @SAMEORIGIN@ are the only forms honoured by current
browsers. The codec accepts the RFC 7034 @ALLOW-FROM@ form, including
the slash-led URI suffix in its example. 'xFrameOptionsOrigin' gives the
origin used by this legacy form. Directive keywords are case-insensitive.

Spec: <https://www.rfc-editor.org/rfc/rfc7034>

See also: "Network.HTTP.Headers.ContentSecurityPolicy", "Network.HTTP.Headers.XContentTypeOptions", "Network.HTTP.Headers.CrossOriginOpenerPolicy", "Network.HTTP.Headers.XPermittedCrossDomainPolicies".
-}
module Network.HTTP.Headers.XFrameOptions (
  XFrameOptions (..),
  xFrameOptionsOrigin,
  xFrameOptionsParser,
  renderXFrameOptions,
) where

import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (SerializedOrigin (..), serializedOriginParser)
import Network.HTTP.Grammar.URIReference (uriPathSuffixBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXFrameOptions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @X-Frame-Options@ value.
data XFrameOptions
  = -- | @DENY@ — never permit framing.
    Deny
  | -- | @SAMEORIGIN@ — permit framing only by same-origin documents.
    SameOrigin
  | -- | Obsolete @ALLOW-FROM@. The address retains its original spelling.
    AllowFrom !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader XFrameOptions where
  type ParseFailure XFrameOptions = String
  type Cardinality XFrameOptions = 'ZeroOrOne
  type Direction XFrameOptions = 'Response


  parseFromHeaders _ ("DENY" :| []) = Right Deny
  parseFromHeaders _ ("SAMEORIGIN" :| []) = Right SameOrigin
  parseFromHeaders _ fields = parseSingleFieldLine "X-Frame-Options" xFrameOptionsParser fields


  renderToHeaders _ Deny = "DENY"
  renderToHeaders _ SameOrigin = "SAMEORIGIN"
  renderToHeaders _ field = M.toStrictByteString (renderXFrameOptions field)


  headerName _ = hXFrameOptions


xFrameOptionsParser :: ParserT st String XFrameOptions
xFrameOptionsParser =
  ows
    *> $( switchAsciiCI
            [|
              case _ of
                "deny" -> pure Deny
                "sameorigin" -> pure SameOrigin
                "allow-from" -> rws *> (AllowFrom <$> shortASCIIFromParser_ allowFromAddress)
              |]
        )
    <* notFollowedBy (skipSatisfyAscii isTokenChar)
    <* ows


allowFromAddress :: ParserT st String SerializedOrigin
allowFromAddress = do
  origin <- serializedOriginParser
  let host = ST.toByteString (serializedHost origin)
  if B.elem 44 host || B.elem 42 host
    then failed
    else origin <$ optional_ uriPathSuffixBytes
{-# NOINLINE allowFromAddress #-}


-- | The validated origin of a legacy @ALLOW-FROM@ address.
xFrameOptionsOrigin :: XFrameOptions -> Maybe SerializedOrigin
xFrameOptionsOrigin (AllowFrom address) = case runParser (allowFromAddress <* eof) (ST.toByteString address) of
  OK origin _ -> Just origin
  _ -> Nothing
xFrameOptionsOrigin _ = Nothing


renderXFrameOptions :: XFrameOptions -> M.Builder
renderXFrameOptions = \case
  Deny -> "DENY"
  SameOrigin -> "SAMEORIGIN"
  AllowFrom origin -> "ALLOW-FROM " <> shortText origin
