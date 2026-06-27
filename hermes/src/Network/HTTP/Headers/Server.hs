{-# LANGUAGE TemplateHaskell #-}

{- | The @Server@ response header (RFC 9110 §10.2.4).

@Server@ advertises the software the origin server used to handle the
request, as a whitespace-separated sequence of @product@ identifiers (each
a @token@ with an optional @\/version@) interspersed with @comment@s:

> Server  = product *( RWS ( product / comment ) )
> product = token [ "/" product-version ]

This mirrors the @User-Agent@ request header, which shares the grammar. A
'Server' is modelled as a leading 'Product' plus the remaining
whitespace-separated elements, each either a 'Comment' or a 'Product'.

Comment bodies are parsed as RFC 9110 §5.6.5 @ctext@ (the 'commentCharSet')
— unescaped comment text, without nested comments or quoted-pairs. This is
sufficient for the values seen in practice and round-trips byte-for-byte.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.4>

See also: "Network.HTTP.Headers.UserAgent", "Network.HTTP.Headers.Via", "Network.HTTP.Headers.XPoweredBy".
-}
module Network.HTTP.Headers.Server (
  Server (..),
  Product (..),
  serverParser,
  renderServer,
) where

import Control.Monad.Combinators (between)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


data Server = Server
  { firstProduct :: Product
  , remainingServerDefinition :: [Either Comment Product]
  }
  deriving stock (Eq, Show)


data Product = Product
  { productName :: !ShortText
  , productVersion :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader Server where
  type ParseFailure Server = String
  type Cardinality Server = 'ZeroOrOne
  type Direction Server = 'Response


  parseFromHeaders _ headers = case runParser serverParser $ NE.head headers of
    OK server "" -> Right server
    OK _ rest -> Left $ "Unconsumed input after parsing Server header: " <> show rest
    Fail -> Left "Failed to parse Server header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderServer


  headerName _ = hServer


serverParser :: ParserT st String Server
serverParser =
  Server
    <$> productParser
    <*> many (rws *> ((Right <$> productParser) <|> (Left <$> commentParser)))
  where
    productParser = Product <$> rfc9110Token <*> optional ($(char '/') *> rfc9110Token)
    commentParser = Comment <$> commentBody
    commentBody =
      between
        $(char '(')
        $(char ')')
        (Text.pack <$> many (satisfyAscii (`CharSet.member` commentCharSet)))


renderServer :: Server -> M.Builder
renderServer (Server fp rest) =
  renderProduct fp
    <> foldMap (\e -> " " <> either renderComment renderProduct e) rest
  where
    renderProduct (Product n mv) = case mv of
      Nothing -> shortText n
      Just v -> shortText n <> "/" <> shortText v
    renderComment (Comment c) = "(" <> M.textUtf8 c <> ")"
