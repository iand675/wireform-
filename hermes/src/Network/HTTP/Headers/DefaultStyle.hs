{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@Default-Style@ — HTML 4.01 entity-header that names the preferred style sheet
set for the served document (matching the @title@ of a @LINK@/@STYLE@ style
sheet), the HTTP-header form of @META http-equiv=\"Default-Style\"@.

== Value

A quoted style sheet title such as @"compact"@. The codec also reads bare
titles. The last field line takes precedence. Rendering uses an HTTP quoted
string to preserve title bytes, including whitespace inside the quotes.

Spec: <https://www.w3.org/TR/html401/present/styles.html#h-14.3.2>

See also: "Network.HTTP.Headers.ContentStyleType", "Network.HTTP.Headers.ContentScriptType".
-}
module Network.HTTP.Headers.DefaultStyle (
  DefaultStyle (DefaultStyle, DefaultStyleBytes, defaultStyleTitle, defaultStyleTitleBytes),
  defaultStyleParser,
  renderDefaultStyle,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeHttpQuotedBytes, opaqueShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDefaultStyle)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Default-Style@ value: the decoded preferred style sheet title.
newtype DefaultStyle = DefaultStyleBytes {defaultStyleTitleBytes :: SBS.ShortByteString}
  deriving stock (Eq, Show)


pattern DefaultStyle :: ST.ShortText -> DefaultStyle
pattern DefaultStyle {defaultStyleTitle} <- DefaultStyleBytes (opaqueShortText -> defaultStyleTitle)
  where
    DefaultStyle title = DefaultStyleBytes (ST.toShortByteString title)


{-# COMPLETE DefaultStyle #-}


instance KnownHeader DefaultStyle where
  type ParseFailure DefaultStyle = String
  type Cardinality DefaultStyle = 'ZeroOrOne
  type Direction DefaultStyle = 'Response


  parseFromHeaders _ headers
    | BS.isPrefixOf "\"" wire = parseQuotedFieldLine "Default-Style" (DefaultStyleBytes . SBS.toShort) (wire :| [])
    | otherwise = parseFieldLine "Default-Style" (defaultStyleParser <* ows) wire
    where
      wire = NE.last headers


  renderToHeaders _ = encodeHttpQuotedBytes . defaultStyleTitleBytes


  headerName _ = hDefaultStyle


defaultStyleParser :: ParserT st String DefaultStyle
defaultStyleParser = ows *> branch (lookahead $(char '"')) (DefaultStyleBytes . SBS.toShort <$> httpQuotedBytes) bare
  where
    bare = withByteString (skipMany titleByte) $ \() bytes ->
      pure (DefaultStyleBytes (SBS.toShort (BS.dropWhileEnd isOwsByte bytes)))
    titleByte = do
      byte <- anyWord8
      if isOpaqueByte (fromIntegral byte) then pure () else failed
    isOwsByte byte = byte == 9 || byte == 32


renderDefaultStyle :: DefaultStyle -> M.Builder
renderDefaultStyle = M.byteString . encodeHttpQuotedBytes . defaultStyleTitleBytes
