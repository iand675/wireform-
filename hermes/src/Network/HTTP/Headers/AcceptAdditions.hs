{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2324 §2.2.2.1 @Accept-Additions@ — HTCPCP request header listing the
additions (milk, syrup, sweetener, spice, alcohol, …) acceptable in the
brewed response.

== Grammar

@
Accept-Additions = \"Accept-Additions\" \":\" #( addition-type *( \";\" parameter ) )
addition-type    = ( \"*\" | token ) *( \";\" parameter )
parameter        = token [ \"=\" ( token | quoted-string ) ]
@

Each comma-separated element is an addition-type token (the @\"*\"@
wildcard and the named additions are all tokens) followed by zero or more
@;@-separated parameters; the quality factor @q@ is just one such
parameter. Parameter values render as a bare token when token-safe and as
a quoted-string otherwise.

Spec: <https://www.rfc-editor.org/rfc/rfc2324#section-2.2.2.1>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptFeatures", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.TCN".
-}
module Network.HTTP.Headers.AcceptAdditions (
  AcceptAdditions (..),
  Addition (..),
  AdditionParam (..),
  acceptAdditionsParser,
  renderAcceptAdditions,
) where

import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptAdditions)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A @;@-separated parameter attached to an addition.
data AdditionParam = AdditionParam
  { paramName :: !ST.ShortText
  , paramValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | An addition-type token plus its parameters.
data Addition = Addition
  { additionType :: !ST.ShortText
  , additionParams :: ![AdditionParam]
  }
  deriving stock (Eq, Show)


-- | The comma-separated list of additions carried by an @Accept-Additions@ header.
newtype AcceptAdditions = AcceptAdditions {acceptAdditions :: [Addition]}
  deriving stock (Eq, Show)


instance KnownHeader AcceptAdditions where
  type ParseFailure AcceptAdditions = String
  type Cardinality AcceptAdditions = 'ZeroOrOne
  type Direction AcceptAdditions = 'Request


  parseFromHeaders _ headers = case runParser acceptAdditionsParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Accept-Additions: " <> show leftover)
    Fail -> Left "Failed to parse Accept-Additions header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderAcceptAdditions


  headerName _ = hAcceptAdditions


acceptAdditionsParser :: ParserT st String AcceptAdditions
acceptAdditionsParser = AcceptAdditions <$> (ows *> additions)
  where
    additions = nonEmpty <|> pure []
    nonEmpty = do
      a <- addition
      as <- many (ows *> $(char ',') *> ows *> addition)
      pure (a : as)
    addition = do
      ty <- rfc9110Token
      ps <- many (ows *> $(char ';') *> ows *> param)
      pure (Addition ty ps)
    param = do
      name <- rfc9110Token
      val <- optional ($(char '=') *> (rfc9110Token <|> quotedString))
      pure (AdditionParam name val)


renderAcceptAdditions :: AcceptAdditions -> M.Builder
renderAcceptAdditions (AcceptAdditions as) = M.intersperse ", " (map renderAddition as)
  where
    renderAddition (Addition ty ps) = R.shortText ty <> mconcat (map renderParam ps)
    renderParam (AdditionParam name Nothing) = M.char7 ';' <> R.shortText name
    renderParam (AdditionParam name (Just v)) =
      M.char7 ';' <> R.shortText name <> M.char7 '=' <> renderTokenOrQuoted v


{- | Render a value as a bare token when it is token-safe, otherwise as a
quoted-string with the mandatory escaping of @\"@ and @\\@.
-}
renderTokenOrQuoted :: ST.ShortText -> M.Builder
renderTokenOrQuoted t
  | isToken s = R.shortText t
  | otherwise = M.char8 '"' <> foldr esc mempty s <> M.char8 '"'
  where
    s = ST.toString t
    isToken cs = not (null cs) && all (`CharSet.member` tokenCharSet) cs
    esc c acc
      | c == '"' || c == '\\' = M.char8 '\\' <> M.char8 c <> acc
      | otherwise = M.char8 c <> acc
