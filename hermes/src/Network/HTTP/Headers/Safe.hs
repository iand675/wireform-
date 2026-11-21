{-# LANGUAGE TemplateHaskell #-}

{- |
@Safe@ — obsolete HTTP response header (RFC 2310) indicating whether the
corresponding request may be automatically repeated. RFC 2324 adds
conditional values such as @if-user-awake@.

@
Safe        = "Safe" ":" safe-nature
safe-nature = "yes" | "no" | "if-" token
@

Specs: <https://www.rfc-editor.org/rfc/rfc2310#section-4 RFC 2310 §4>
and <https://www.rfc-editor.org/rfc/rfc2324#section-2.2.1.1 RFC 2324 §2.2.1.1>.

See also: "Network.HTTP.Headers.PublicHeader", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.Safe (
  Safe (..),
  safeParser,
  renderSafe,
) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (copyText, shortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSafe)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Encode.Direct (dBytes, directEncode)


-- | Whether repeating the corresponding request is considered safe.
data Safe
  = SafeYes
  | SafeNo
  | SafeIf !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader Safe where
  type ParseFailure Safe = String
  type Cardinality Safe = 'ZeroOrOne
  type Direction Safe = 'Response


  parseFromHeaders _ fields@(wire :| []) = case BS.length wire of
    3 | lowerAt 0 == 121 && lowerAt 1 == 101 && lowerAt 2 == 115 -> Right SafeYes
    2 | lowerAt 0 == 110 && lowerAt 1 == 111 -> Right SafeNo
    _ -> parseSingleFieldLine "Safe" safeNature fields
    where
      -- The outer length check bounds each read. Setting bit five folds
      -- both ASCII spellings of a letter without accepting other octets.
      lowerAt offset = BSU.unsafeIndex wire offset .|. 32
  parseFromHeaders _ fields = parseSingleFieldLine "Safe" safeNature fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ SafeYes = "yes"
  renderToHeaders _ SafeNo = "no"
  renderToHeaders _ result = M.toStrictByteString (renderSafe result)


  renderToHeadersChecked _ SafeYes = Just "yes"
  renderToHeadersChecked _ SafeNo = Just "no"
  renderToHeadersChecked _ (SafeIf condition)
    | isHttpTokenText condition = Just $ directEncode size $ \ptr start ->
        dBytes ptr start "if-" >>= \offset -> copyText ptr offset condition
    | otherwise = Nothing
    where
      size = BSI.checkedAdd "Safe" 3 (SBS.length (ST.toShortByteString condition))


  headerName _ = hSafe


safeParser :: ParserT st String Safe
safeParser = ows *> safeNature <* ows


safeNature :: ParserT st String Safe
safeNature =
  (SafeYes <$ (letter 'y' 'Y' *> letter 'e' 'E' *> letter 's' 'S' *> notFollowedBy tokenBytes))
    <|> (SafeNo <$ (letter 'n' 'N' *> letter 'o' 'O' *> notFollowedBy tokenBytes))
    <|> (SafeIf <$> (letter 'i' 'I' *> letter 'f' 'F' *> $(char '-') *> rfc9110Token))
  where
    letter lower upper = skipSatisfyAscii (\c -> c == lower || c == upper)


renderSafe :: Safe -> M.Builder
renderSafe = \case
  SafeYes -> "yes"
  SafeNo -> "no"
  SafeIf condition -> "if-" <> shortText condition
