{- |
RFC 9110 §8.5 @Content-Language@ — representation metadata describing the
natural language(s) of the intended audience for the enclosed representation.

== Grammar

@
Content-Language = #language-tag
language-tag     = \<Language-Tag, see [RFC5646], Section 2.1\>
@

The codec checks well-formed RFC 5646 tags, including their subtag order,
and preserves their wire spelling. Registry membership is application policy.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.5>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.ContentLanguage (
  ContentLanguage (..),
  LanguageTag (..),
  contentLanguageTagList,
  contentLanguageParser,
  renderContentLanguage,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Grammar.LanguageTag (languageTagBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentLanguage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single RFC 5646 language tag in its on-the-wire (token) form.
newtype LanguageTag = LanguageTag {unLanguageTag :: ST.ShortText}
  deriving stock (Eq, Show)


-- | A comma-separated list of language tags.
data ContentLanguage
  = ContentLanguage {contentLanguageTags :: NE.NonEmpty LanguageTag}
  | ContentLanguageEmpty
  deriving stock (Eq, Show)


instance Semigroup ContentLanguage where
  ContentLanguageEmpty <> tags = tags
  tags <> ContentLanguageEmpty = tags
  ContentLanguage left <> ContentLanguage right = ContentLanguage (left <> right)


instance KnownHeader ContentLanguage where
  type ParseFailure ContentLanguage = String
  type Cardinality ContentLanguage = 'ZeroOrMore
  type Direction ContentLanguage = 'RequestAndResponse


  parseFromHeaders _ = fmap fromTags . parseListFieldLines "Content-Language" tagList


  renderToHeaders _ = pure . M.toStrictByteString . renderContentLanguage


  headerName _ = hContentLanguage


contentLanguageParser :: ParserT st String ContentLanguage
contentLanguageParser = fromTags <$> tagList


tagList :: ParserT st String [LanguageTag]
tagList = httpList (LanguageTag . STU.fromByteStringUnsafe <$> languageTagBytes)


fromTags :: [LanguageTag] -> ContentLanguage
fromTags = maybe ContentLanguageEmpty ContentLanguage . NE.nonEmpty


-- | Return the tags of an empty or populated field.
contentLanguageTagList :: ContentLanguage -> [LanguageTag]
contentLanguageTagList ContentLanguageEmpty = []
contentLanguageTagList (ContentLanguage tags) = NE.toList tags


renderContentLanguage :: ContentLanguage -> M.Builder
renderContentLanguage ContentLanguageEmpty = mempty
renderContentLanguage (ContentLanguage tags) = sepByCommas1 (fmap (shortText . unLanguageTag) tags)
