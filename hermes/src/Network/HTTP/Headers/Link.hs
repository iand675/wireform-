{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 8288 @Link@ — typed Web Linking. The value is a comma-separated
list of @link-value@s, each a target URI reference enclosed in angle
brackets followed by ordered @link-param@s.

== Grammar (RFC 8288 §3)

@
Link       = #link-value
link-value = \"<\" URI-Reference \">\" *( OWS \";\" OWS link-param )
link-param = token BWS [ \"=\" BWS ( token \/ quoted-string ) ]
@

where @#rule@ is the RFC 9110 §5.6.1 comma-separated list operator.

Each 'LinkValue' carries its target (the bytes between @<@ and @>@)
plus an ordered list of 'LinkParam's; a param may be valueless
(a bare token) or carry a 'LinkParamToken' or 'LinkParamQuoted'
value. Param names are case-insensitive per RFC 8288, but the
on-the-wire spelling is preserved so values round-trip byte-for-byte.

Spec: <https://www.rfc-editor.org/rfc/rfc8288>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.Sunset".
-}
module Network.HTTP.Headers.Link (
  -- * Types
  Link (..),
  LinkValue (..),
  LinkParam (..),
  LinkParamValue (..),

  -- * Parsing
  linkParser,
  linkValuesParser,

  -- * Rendering
  renderLink,
  renderLinkValue,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLink)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | A @link-param@ value: a bare token or a quoted-string.
data LinkParamValue
  = -- | @token@ value, e.g. @rel=next@.
    LinkParamToken !ST.ShortText
  | -- | @quoted-string@ value, e.g. @title=\"Next chapter\"@. The
    -- contained text is the unescaped string body.
    LinkParamQuoted !ST.ShortText
  deriving stock (Eq, Show)


-- | A single @link-param@: a name with an optional value.
data LinkParam = LinkParam
  { linkParamName :: !ST.ShortText
  , linkParamValue :: !(Maybe LinkParamValue)
  }
  deriving stock (Eq, Show)


{- | A single @link-value@: the target URI reference (the bytes
between @<@ and @>@) plus its params, in document order.
-}
data LinkValue = LinkValue
  { linkTarget :: !ST.ShortText
  , linkParams :: ![LinkParam]
  }
  deriving stock (Eq, Show)


-- | @Link@ header value: a comma-separated list of link-values.
newtype Link = Link {linkValues :: [LinkValue]}
  deriving stock (Eq, Show)


instance KnownHeader Link where
  type ParseFailure Link = String
  type Cardinality Link = 'ZeroOrMore
  type Direction Link = 'RequestAndResponse


  parseFromHeaders _ headers = do
    vss <- traverse parseOne (NE.toList headers)
    pure (Link (concat vss))
    where
      parseOne hdr = case runParser linkValuesParser hdr of
        OK vs leftover
          | B.null (dropOws leftover) -> Right vs
          | otherwise ->
              Left ("Unconsumed input after parsing Link: " <> show leftover)
        Fail -> Left "Failed to parse Link header"
        Err err -> Left err
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = pure . M.toStrictByteString . renderLink


  headerName _ = hLink


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse a whole @Link@ value.
linkParser :: ParserT st String Link
linkParser = Link <$> linkValuesParser


-- | Parse the bare comma-separated @#link-value@ list.
linkValuesParser :: ParserT st String [LinkValue]
linkValuesParser = ows *> (linkValue `sepBy` (ows *> $(char ',') *> ows))


-- | @\"<\" URI-Reference \">\" *( OWS \";\" OWS link-param )@.
linkValue :: ParserT st String LinkValue
linkValue = do
  $(char '<')
  target <- shortASCIIFromParser_ (skipMany (skipSatisfyAscii (/= '>')))
  $(char '>')
  params <- many linkParam
  pure LinkValue {linkTarget = target, linkParams = params}
  where
    linkParam = do
      ows
      $(char ';')
      ows
      name <- rfc9110Token
      val <- optional $ do
        bws
        $(char '=')
        bws
        (LinkParamQuoted <$> quotedString) <|> (LinkParamToken <$> rfc9110Token)
      pure LinkParam {linkParamName = name, linkParamValue = val}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

-- | Render a whole @Link@ value.
renderLink :: Link -> M.Builder
renderLink (Link vs) = M.intersperse ", " (map renderLinkValue vs)


-- | Render one @link-value@.
renderLinkValue :: LinkValue -> M.Builder
renderLinkValue (LinkValue target params) =
  M.char7 '<' <> R.shortText target <> M.char7 '>' <> foldMap renderParam params
  where
    renderParam (LinkParam name val) =
      "; " <> R.shortText name <> maybe mempty renderVal val
    renderVal = \case
      LinkParamToken t -> M.char7 '=' <> R.shortText t
      LinkParamQuoted s -> M.char7 '=' <> R.rfc8941String (RFC8941String s)
