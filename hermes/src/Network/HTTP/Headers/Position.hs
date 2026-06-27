{-# LANGUAGE TemplateHaskell #-}

{- |
@Position@ (RFC 3648 §6.1) — request header used with methods that add
an internal member to an ordered WebDAV collection (@PUT@, @COPY@,
@MOVE@, @MKCOL@, ...), specifying where the new member is placed in the
collection's ordering.

@
Position = \"Position\" \":\" ( \"first\" | \"last\"
                           | ( ( \"before\" | \"after\" ) segment ) )
@

@segment@ is an RFC 2396 §3.3 path segment, interpreted relative to the
collection the member is added to.

Spec: <https://www.rfc-editor.org/rfc/rfc3648.html#section-6.1>

See also: "Network.HTTP.Headers.OrderingType", "Network.HTTP.Headers.Destination", "Network.HTTP.Headers.Depth".
-}
module Network.HTTP.Headers.Position (
  Position (..),
  positionParser,
  renderPosition,
) where

import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPosition)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Where to place the new member in the collection's ordering.
data Position
  = -- | @first@: at the beginning of the ordering.
    PositionFirst
  | -- | @last@: at the end of the ordering.
    PositionLast
  | -- | @before \<segment\>@: immediately before the named member.
    PositionBefore !ST.ShortText
  | -- | @after \<segment\>@: immediately after the named member.
    PositionAfter !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader Position where
  type ParseFailure Position = String
  type Cardinality Position = 'ZeroOrOne
  type Direction Position = 'Request


  parseFromHeaders _ headers = case runParser positionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Position header: " <> show rest
    Fail -> Left "Failed to parse Position header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPosition


  headerName _ = hPosition


-- | RFC 2396 §3.3 @segment@ characters (pchar plus @;@ for params).
segmentCharSet :: CharSet
segmentCharSet = alnum <> "-_.!~*'():@&=+$,;%"


segment :: ParserT st e ST.ShortText
segment = shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` segmentCharSet)))


positionParser :: ParserT st String Position
positionParser = do
  ows
  pos <-
    (PositionFirst <$ $(string "first"))
      <|> (PositionLast <$ $(string "last"))
      <|> ($(string "before") *> rws *> (PositionBefore <$> segment))
      <|> ($(string "after") *> rws *> (PositionAfter <$> segment))
  ows
  pure pos


renderPosition :: Position -> M.Builder
renderPosition = \case
  PositionFirst -> "first"
  PositionLast -> "last"
  PositionBefore seg -> "before " <> shortText seg
  PositionAfter seg -> "after " <> shortText seg
