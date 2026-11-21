{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
The Vary HTTP response header describes the parts of the request message aside from the method
and URL that influenced the content of the response it occurs in. Most often, this is used to create
a cache key when content negotiation is in use.

The same Vary header value should be used on all responses for a given URL,
including 304 Not Modified responses and the "default" response.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.5>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.CacheControl".
-}
module Network.HTTP.Headers.Vary (
  Vary (..),
  VaryDirective (..),
  runVaryParser,
  parseVary,
  renderVary,
) where

import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as SBS
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util
import Prelude


-- | An empty field has no selecting request header fields.
data Vary
  = Vary {directives :: NE.NonEmpty VaryDirective}
  | VaryEmpty
  deriving stock (Eq, Show)


instance Semigroup Vary where
  VaryEmpty <> other = other
  other <> VaryEmpty = other
  Vary xs <> Vary ys = Vary (xs <> ys)


data VaryDirective = VaryAll | VaryHeader !HeaderFieldName
  deriving stock (Eq, Show)


instance KnownHeader Vary where
  type ParseFailure Vary = Maybe String
  type Cardinality Vary = 'ZeroOrMore
  type Direction Vary = 'Response


  parseFromHeaders _ =
    first Just . fmap fromDirectives . parseListFieldLines "Vary" (httpList varyDirectiveParser)


  renderToHeaders _ = pure . M.toStrictByteString . renderVary


  renderToHeadersChecked _ VaryEmpty = Just [""]
  renderToHeadersChecked _ (Vary names)
    | all validDirective names = Just [encodeCommaSeparatedBytes varyDirectiveBytes names]
    | otherwise = Nothing
    where
      validDirective VaryAll = True
      validDirective (VaryHeader name) = isHttpTokenBytes (headerNameToShortByteString name)


  headerName _ = hVary


runVaryParser :: B.ByteString -> Either (ParseFailure Vary) Vary
runVaryParser = first Just . parseFieldLine "Vary" parseVary


parseVary :: ParserT st String Vary
parseVary = fromDirectives <$> httpList varyDirectiveParser


fromDirectives :: [VaryDirective] -> Vary
fromDirectives = maybe VaryEmpty Vary . NE.nonEmpty


varyDirectiveParser :: ParserT st e VaryDirective
varyDirectiveParser = commonDirective <|> (VaryHeader . unsafeUnknownHeaderFromBytestring . foldFieldName <$> rfc9110TokenBS)
  where
    -- Common lowercase names already have a canonical symbol.
    commonDirective =
      $( switch
           [|
             case _ of
               "*" -> whole VaryAll
               "accept" -> whole (VaryHeader hAccept)
               "accept-encoding" -> whole (VaryHeader hAcceptEncoding)
               "accept-language" -> whole (VaryHeader hAcceptLanguage)
               "cookie" -> whole (VaryHeader hCookie)
               "origin" -> whole (VaryHeader hOrigin)
               "user-agent" -> whole (VaryHeader hUserAgent)
             |]
       )
    whole value = value <$ notFollowedBy (skipSatisfyAscii (`CharSet.member` tokenCharSet))

    -- The token scanner has already checked the field-name bytes.
    foldFieldName field
      | BC.any isAsciiUpper field = BC.map toLower field
      | otherwise = field


renderVary :: Vary -> M.Builder
renderVary VaryEmpty = mempty
renderVary (Vary names) = sepByCommas1 (fmap (M.shortByteString . varyDirectiveBytes) names)


varyDirectiveBytes :: VaryDirective -> SBS.ShortByteString
varyDirectiveBytes VaryAll = "*"
varyDirectiveBytes (VaryHeader hdr) = headerNameToShortByteString hdr
