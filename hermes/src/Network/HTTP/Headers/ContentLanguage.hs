{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9110 §8.5 @Content-Language@ — representation metadata describing the
natural language(s) of the intended audience for the enclosed representation.

== Grammar

@
Content-Language = #language-tag
language-tag     = \<Language-Tag, see [RFC5646], Section 2.1\>
@

A @language-tag@ (RFC 5646) is a sequence of hyphen-separated subtags, each of
which is an alphanumeric token — so every well-formed tag is also an RFC 9110
@token@. We surface the list of tags, preserving their on-the-wire casing
(language tags are case-insensitive but conventionally cased, e.g. @en-US@).

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.5>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.ContentLanguage (
  ContentLanguage (..),
  LanguageTag (..),
  contentLanguageParser,
  renderContentLanguage,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentLanguage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single RFC 5646 language tag in its on-the-wire (token) form.
newtype LanguageTag = LanguageTag {unLanguageTag :: ST.ShortText}
  deriving stock (Eq, Show)


-- | A non-empty, comma-separated list of language tags.
newtype ContentLanguage = ContentLanguage {contentLanguageTags :: NE.NonEmpty LanguageTag}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader ContentLanguage where
  type ParseFailure ContentLanguage = String
  type Cardinality ContentLanguage = 'ZeroOrMore
  type Direction ContentLanguage = 'RequestAndResponse


  parseFromHeaders _ headers = do
    res <- traverse runContentLanguageParser headers
    pure (fold1 res)


  renderToHeaders _ = pure . M.toStrictByteString . renderContentLanguage


  headerName _ = hContentLanguage


runContentLanguageParser :: B.ByteString -> Either String ContentLanguage
runContentLanguageParser bs = case runParser contentLanguageParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("Unconsumed input after parsing Content-Language header: " <> show rest)
  Fail -> Left "Failed to parse Content-Language header"
  Err e -> Left e


contentLanguageParser :: ParserT st String ContentLanguage
contentLanguageParser =
  ContentLanguage <$> (languageTag `sepBy1` (ows *> $(char ',') *> ows))
  where
    languageTag = LanguageTag <$> rfc9110Token


renderContentLanguage :: ContentLanguage -> M.Builder
renderContentLanguage =
  sepByCommas1 . fmap (shortText . unLanguageTag) . contentLanguageTags
