{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Module      : Network.HTTP.Headers.CDNLoop
Description : The @CDN-Loop@ request header

The @CDN-Loop@ request header field is appended to by content delivery
networks (CDNs) so that they can detect and break infinite request loops when
a request is (mis)configured to pass through the same CDN more than once.

Its value is a comma-separated list of CDN identifiers and optional
parameters. An identifier is a URI host with an optional port or a token
pseudonym.

> CDN-Loop  = #cdn-info
> cdn-info  = cdn-id *( OWS ";" OWS parameter )
> cdn-id    = ( uri-host [ ":" port ] ) / token
> parameter = token "=" ( token / quoted-string )

See <https://datatracker.ietf.org/doc/html/rfc8586> for the official
specification (RFC 8586: Loop Detection in Content Delivery Networks (CDNs)).

See also: "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.SurrogateCapability".
-}
module Network.HTTP.Headers.CDNLoop (
  CDNLoop (..),
  CDNInfo (..),
  cdnLoopEntries,
  cdnLoopParser,
  renderCDNLoop,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (cdnIdentifier, isCdnIdentifier)
import Network.HTTP.Grammar.Rendering (encodeHttpParameterizedList, encodeHttpParameterizedListChecked, renderHttpParameterizedList)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCDNLoop)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | A single @cdn-info@ entry: a CDN identifier plus any trailing
@;@-delimited parameters. Parameter values are stored decoded (quoting, if
any, is removed on parse). Latin-1 code points retain the corresponding
wire octets.
-}
data CDNInfo = CDNInfo
  { cdnInfoId :: !ST.ShortText
  , cdnInfoParameters :: ![(ST.ShortText, ST.ShortText)]
  }
  deriving stock (Eq, Show)


-- | A @CDN-Loop@ value, including an explicitly empty field.
data CDNLoop
  = CDNLoop {cdnLoopInfos :: !(NonEmpty CDNInfo)}
  | CDNLoopEmpty
  deriving stock (Eq, Show)


-- | The identifiers and parameters in field-line order.
cdnLoopEntries :: CDNLoop -> [CDNInfo]
cdnLoopEntries CDNLoopEmpty = []
cdnLoopEntries (CDNLoop infos) = NE.toList infos


instance KnownHeader CDNLoop where
  type ParseFailure CDNLoop = String
  type Cardinality CDNLoop = 'ZeroOrMore
  type Direction CDNLoop = 'Request


  parseFromHeaders _ = parseFieldLine "CDN-Loop" cdnLoopParser . combineFieldLines ", "


  renderToHeaders _ CDNLoopEmpty = []
  renderToHeaders _ value =
    [encodeHttpParameterizedList cdnInfoId cdnInfoParameters fst snd (cdnLoopEntries value)]


  renderToHeadersChecked _ CDNLoopEmpty = Just []
  renderToHeadersChecked _ value =
    pure <$> encodeHttpParameterizedListChecked isCdnIdentifier cdnInfoId cdnInfoParameters fst snd (cdnLoopEntries value)


  omitHeaderValue CDNLoopEmpty = True
  omitHeaderValue _ = False


  headerName _ = hCDNLoop


cdnLoopParser :: ParserT st e CDNLoop
cdnLoopParser = maybe CDNLoopEmpty CDNLoop . NE.nonEmpty <$> httpList cdnInfoParser


cdnInfoParser :: ParserT st e CDNInfo
cdnInfoParser = do
  cid <- embedError cdnIdentifier (const failed)
  params <- many (ows *> $(char ';') *> ows *> parameterParser)
  pure (CDNInfo cid params)


parameterParser :: ParserT st e (ST.ShortText, ST.ShortText)
parameterParser = do
  k <- rfc9110Token
  $(char '=')
  v <- rfc9110Token <|> (shortTextFromLatin1 <$> httpQuotedBytes)
  pure (k, v)


renderCDNLoop :: CDNLoop -> M.Builder
renderCDNLoop = renderHttpParameterizedList cdnInfoId cdnInfoParameters fst snd . cdnLoopEntries
