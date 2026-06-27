{-# LANGUAGE TemplateHaskell #-}

{- | The @ETag@ response header field provides the entity-tag of the selected
representation: an opaque validator, either strong or @W/@-prefixed weak, that
the origin server changes whenever the representation changes. Clients echo it
in conditional requests so caches can be revalidated and concurrent updates
detected without transferring the whole representation.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3>

See also: "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.IfRange", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.ETag (
  ETag (..),
  EntityTag (..),
  eTagParser,
  entityTagParser,
  renderEntityTag,
  validTagChar,
  renderETag,
  parseETag,
) where

import Control.Monad.Combinators (between)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ETag = ETag {etag :: EntityTag}
  deriving stock (Eq, Show)


data EntityTag
  = StrongETag {-# UNPACK #-} !ST.ShortText
  | WeakETag {-# UNPACK #-} !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader ETag where
  type ParseFailure ETag = String
  type Cardinality ETag = 'ZeroOrOne
  type Direction ETag = 'Response


  parseFromHeaders _ headers = case parseETag $ NE.head headers of
    Left e -> Left e
    Right t -> Right t


  renderToHeaders _ = M.toStrictByteString . renderETag


  headerName _ = hETag


entityTagParser :: ParserT st e EntityTag
entityTagParser = do
  ($(string "W/") *> (WeakETag <$> parseTag)) <|> (StrongETag <$> parseTag)
  where
    parseTag = between $(char '"') $(char '"') (shortASCIIFromParser_ (many validTagChar))


eTagParser :: ParserT st e ETag
eTagParser = ETag <$> entityTagParser


etagCharSet :: CharSet
etagCharSet = "\x21" <> CharSet.fromList ['\x23' .. '\x7E'] <> obsTextCharSet


validTagChar :: ParserT st e Char
validTagChar = satisfyAscii (`CharSet.member` etagCharSet)


parseETag :: B.ByteString -> Either String ETag
parseETag bs = case runParser eTagParser bs of
  OK tag "" -> Right tag
  OK _ rest -> Left $ "Unconsumed input after parsing ETag header: " <> show rest
  Fail -> Left "Failed to parse ETag header"
  Err e -> Left e


renderEntityTag :: EntityTag -> M.Builder
renderEntityTag (StrongETag tag) = M.char8 '"' <> M.shortByteString (ST.toShortByteString tag) <> M.char8 '"'
renderEntityTag (WeakETag tag) = "W/\"" <> M.shortByteString (ST.toShortByteString tag) <> M.char8 '"'


renderETag :: ETag -> M.Builder
renderETag = renderEntityTag . etag
