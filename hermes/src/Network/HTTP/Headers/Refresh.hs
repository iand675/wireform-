{-# LANGUAGE TemplateHaskell #-}

{- |
The @Refresh@ response header instructs the user agent to reload the current
document (or navigate to a different URL) after a given number of seconds. It
is the HTTP-header counterpart of the @\<meta http-equiv="refresh"\>@ pragma
introduced by Netscape Navigator: never standardised by the IETF but widely
implemented, so it is a de-facto field; the WHATWG HTML standard documents the
@meta@ form it mirrors.

The value is a non-negative @delta-seconds@ count, optionally followed by
@; url=\<uri\>@ giving the target to navigate to (when absent, the current
document is reloaded):

> Refresh = delta-seconds [ OWS ";" OWS "url=" URI ]

Spec (de-facto; WHATWG HTML @meta@ refresh): <https://html.spec.whatwg.org/multipage/semantics.html#attr-meta-http-equiv-refresh>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.RetryAfter", "Network.HTTP.Headers.Link".
-}
module Network.HTTP.Headers.Refresh (
  Refresh (..),
  refreshParser,
  renderRefresh,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRefresh)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A @Refresh@ directive: reload after @refreshSeconds@, optionally
navigating to @refreshUrl@ instead of reloading the current document.
-}
data Refresh = Refresh
  { refreshSeconds :: !Word
  , refreshUrl :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader Refresh where
  type ParseFailure Refresh = String
  type Cardinality Refresh = 'ZeroOrOne
  type Direction Refresh = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser refreshParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing Refresh header: " <> show rest
      Fail -> Left "Failed to parse Refresh header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderRefresh


  headerName _ = hRefresh


refreshParser :: ParserT st String Refresh
refreshParser = do
  seconds <- anyAsciiDecimalWord
  url <- optional urlParser
  pure $ Refresh seconds url
  where
    urlParser = do
      ows
      $(char ';')
      ows
      $(string "url=")
      takeRestShortText


renderRefresh :: Refresh -> M.Builder
renderRefresh (Refresh seconds url) =
  M.wordDec seconds <> maybe mempty (\u -> "; url=" <> shortText u) url
