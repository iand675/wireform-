{-# LANGUAGE TemplateHaskell #-}

{- |
The Vary HTTP response header describes the parts of the request message aside from the method
and URL that influenced the content of the response it occurs in. Most often, this is used to create
a cache key when content negotiation is in use.

The same Vary header value should be used on all responses for a given URL,
including 304 Not Modified responses and the "default" response.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.5>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.Vary where

import Control.Monad.Combinators.NonEmpty
import qualified Data.ByteString as B
import Data.Foldable1
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util
import Prelude


newtype Vary = Vary {directives :: NE.NonEmpty VaryDirective}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


data VaryDirective = VaryAll | VaryHeader !HeaderFieldName
  deriving stock (Eq, Show)


instance KnownHeader Vary where
  type ParseFailure Vary = Maybe String
  type Cardinality Vary = 'ZeroOrMore
  type Direction Vary = 'Response


  parseFromHeaders _ headers = do
    res <- traverse runVaryParser headers
    pure $ fold1 res


  renderToHeaders _ = pure . M.toStrictByteString . renderVary


  headerName _ = hVary


runVaryParser :: B.ByteString -> Either (ParseFailure Vary) Vary
runVaryParser bs = case runParser parseVary bs of
  OK res "" -> Right res
  OK _ rest -> Left $ Just ("Unconsumed input after parsing Vary header: " <> show rest)
  Fail -> Left Nothing
  Err e -> Left (Just e)


parseVary :: ParserT st String Vary
parseVary = Vary <$> ((star <|> hdr) `sepBy1` (ows *> $(char ',') *> ows))
  where
    star = $(char '*') $> VaryAll
    hdr = VaryHeader . headerFieldName . ST.toText <$> fieldName


renderVary :: Vary -> M.Builder
renderVary = sepByCommas1 . fmap renderVaryDirective . directives
  where
    renderVaryDirective VaryAll = "*"
    renderVaryDirective (VaryHeader hdr) = M.shortByteString $ headerNameToShortByteString hdr
