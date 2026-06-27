{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Network.HTTP.Headers.CDNLoop
Description : The @CDN-Loop@ request header

The @CDN-Loop@ request header field is appended to by content delivery
networks (CDNs) so that they can detect and break infinite request loops when
a request is (mis)configured to pass through the same CDN more than once.

Its value is a comma-separated list of @cdn-info@ entries, where each entry is
a token (the @cdn-id@, usually a hostname or registered pseudonym) optionally
followed by @;@-separated parameters.

> CDN-Loop  = #cdn-info
> cdn-info  = cdn-id *( OWS ";" OWS parameter )
> cdn-id    = token
> parameter = token "=" ( token / quoted-string )

See <https://datatracker.ietf.org/doc/html/rfc8586> for the official
specification (RFC 8586: Loop Detection in Content Delivery Networks (CDNs)).

See also: "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.SurrogateCapability".
-}
module Network.HTTP.Headers.CDNLoop (
  CDNLoop (..),
  CDNInfo (..),
  cdnLoopParser,
  renderCDNLoop,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCDNLoop)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A single @cdn-info@ entry: a CDN identifier (token) plus any trailing
@;@-delimited parameters. Parameter values are stored decoded (quoting, if
any, is removed on parse).
-}
data CDNInfo = CDNInfo
  { cdnInfoId :: ST.ShortText
  , cdnInfoParameters :: [(ST.ShortText, ST.ShortText)]
  }
  deriving stock (Eq, Show)


-- | A @CDN-Loop@ value: a non-empty list of 'CDNInfo' entries.
newtype CDNLoop = CDNLoop {cdnLoopInfos :: NonEmpty CDNInfo}
  deriving stock (Eq, Show)


instance KnownHeader CDNLoop where
  type ParseFailure CDNLoop = String
  type Cardinality CDNLoop = 'ZeroOrMore
  type Direction CDNLoop = 'Request


  parseFromHeaders _ neHeaders =
    CDNLoop . sconcat <$> traverse parseLine neHeaders
    where
      parseLine bs = case runParser cdnLoopParser bs of
        OK v "" -> Right (cdnLoopInfos v)
        OK _ rest -> Left $ "Unconsumed input after parsing CDN-Loop header: " <> show rest
        Fail -> Left "Failed to parse CDN-Loop header"
        Err e -> Left e
  renderToHeaders _ = pure . M.toStrictByteString . renderCDNLoop
  headerName _ = hCDNLoop


cdnLoopParser :: ParserT st e CDNLoop
cdnLoopParser = CDNLoop <$> (cdnInfoParser `sepBy1` (ows *> $(char ',') *> ows))


cdnInfoParser :: ParserT st e CDNInfo
cdnInfoParser = do
  cid <- rfc9110Token
  params <- many (ows *> $(char ';') *> ows *> parameterParser)
  pure (CDNInfo cid params)


parameterParser :: ParserT st e (ST.ShortText, ST.ShortText)
parameterParser = do
  k <- rfc9110Token
  $(char '=')
  v <- rfc9110Token <|> quotedString
  pure (k, v)


renderCDNLoop :: CDNLoop -> M.Builder
renderCDNLoop (CDNLoop infos) = M.intersperse ", " (fmap renderCDNInfo infos)


renderCDNInfo :: CDNInfo -> M.Builder
renderCDNInfo (CDNInfo cid params) = shortText cid <> foldMap renderParameter params


renderParameter :: (ST.ShortText, ST.ShortText) -> M.Builder
renderParameter (k, v) = M.char7 ';' <> shortText k <> M.char7 '=' <> shortText v
