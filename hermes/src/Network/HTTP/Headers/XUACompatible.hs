{-# LANGUAGE TemplateHaskell #-}

{- |
@X-UA-Compatible@ is a de-facto response (and @\<meta http-equiv\>@) header,
originally introduced by Microsoft Internet Explorer, used to select the
rendering / document mode of a user agent, e.g. @IE=edge@, @IE=EmulateIE7@, or
@chrome=1@. Its value is a comma-separated list of @engine=mode@ directives,
each side being an RFC 9110 token, surfaced here as a non-empty list of
'UACompatDirective' pairs.

This header is __not__ IANA-registered.

Spec: <https://html.spec.whatwg.org/multipage/semantics.html#attr-meta-http-equiv-x-ua-compatible>
(WHATWG HTML Living Standard note on the @x-ua-compatible@ pragma).

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.Server".
-}
module Network.HTTP.Headers.XUACompatible (
  XUACompatible (..),
  UACompatDirective (..),
  xUACompatibleParser,
  renderXUACompatible,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXUACompatible)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single @engine=mode@ directive, e.g. @IE=edge@.
data UACompatDirective = UACompatDirective
  { uaCompatEngine :: !ST.ShortText
  , uaCompatMode :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | An @X-UA-Compatible@ value: a non-empty list of @engine=mode@ directives.
newtype XUACompatible = XUACompatible {uaCompatDirectives :: NonEmpty UACompatDirective}
  deriving stock (Eq, Show)


instance KnownHeader XUACompatible where
  type ParseFailure XUACompatible = String
  type Cardinality XUACompatible = 'ZeroOrOne
  type Direction XUACompatible = 'Response


  parseFromHeaders _ headers = case runParser xUACompatibleParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing X-UA-Compatible header: " <> show leftover)
    Fail -> Left "Failed to parse X-UA-Compatible header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderXUACompatible


  headerName _ = hXUACompatible


xUACompatibleParser :: ParserT st String XUACompatible
xUACompatibleParser = XUACompatible <$> (ows *> directive `sepBy1` (ows *> $(char ',') *> ows))
  where
    directive = do
      engine <- rfc9110Token
      mode <- $(char '=') *> rfc9110Token
      pure (UACompatDirective engine mode)


renderXUACompatible :: XUACompatible -> M.Builder
renderXUACompatible (XUACompatible ds) = sepByCommas1 (fmap renderDirective ds)
  where
    renderDirective (UACompatDirective e m) = shortText e <> M.char7 '=' <> shortText m
