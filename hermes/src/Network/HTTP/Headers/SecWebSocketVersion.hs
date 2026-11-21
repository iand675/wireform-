{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 6455 §11.3.5 @Sec-WebSocket-Version@ — carries the WebSocket
protocol version(s). A client request names the single version it
intends to use (currently @13@); a server's @426 Upgrade Required@
response lists the versions it supports.

== Grammar

@
Sec-WebSocket-Version = version
version               = DIGIT | (NZDIGIT DIGIT) | ...   ; 0-255
@

On the response side the value is a comma-separated list of versions,
so the type holds a non-empty list of numbers.

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.5>

See also: "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.SecWebSocketVersion (
  SecWebSocketVersion (..),
  secWebSocketVersionParser,
  renderSecWebSocketVersion,
) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | One or more WebSocket version numbers.
newtype SecWebSocketVersion = SecWebSocketVersion {secWebSocketVersions :: NE.NonEmpty Word}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketVersion where
  type ParseFailure SecWebSocketVersion = String
  type Cardinality SecWebSocketVersion = 'ZeroOrOne
  type Direction SecWebSocketVersion = 'RequestAndResponse


  parseFromHeaders settings headers
    | value NE.:| [] <- headers
    , let size = BS.length value
    , size > 0 && size <= 3 && (size == 1 || BS.head value /= 48) =
        withShortDecimal
          value
          (\n -> if n <= 255 then Right (singleVersion (fromIntegral n)) else Left "WebSocket version exceeds 255")
          (parseVersions settings headers)
    | otherwise = parseVersions settings headers


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketVersion


  renderToHeadersChecked _ (SecWebSocketVersion (13 NE.:| [])) = Just "13"
  renderToHeadersChecked settings value@(SecWebSocketVersion (first NE.:| rest))
    | not (null rest) && headerMessage settings == Just HeaderRequest = Nothing
    | first <= 255 && all (<= 255) rest = Just (renderToHeaders settings value)
    | otherwise = Nothing


  headerName _ = hSecWebSocketVersion


parseVersions :: HeaderSettings -> NE.NonEmpty BS.ByteString -> Either String SecWebSocketVersion
parseVersions settings headers
  | Just HeaderRequest <- headerMessage settings =
      parseSingleFieldLine "Sec-WebSocket-Version" (singleVersion <$> versionNumber) headers
  | value NE.:| [] <- headers =
      parseFieldLine "Sec-WebSocket-Version" secWebSocketVersionParser value
  | otherwise =
      parseListFieldLines "Sec-WebSocket-Version" versionList headers >>= versionsFromList


singleVersion :: Word -> SecWebSocketVersion
singleVersion = SecWebSocketVersion . NE.singleton


secWebSocketVersionParser :: ParserT st String SecWebSocketVersion
secWebSocketVersionParser = SecWebSocketVersion <$> httpList1 "Sec-WebSocket-Version" versionNumber


versionList :: ParserT st String [Word]
versionList = httpList versionNumber


versionsFromList :: [Word] -> Either String SecWebSocketVersion
versionsFromList = fmap SecWebSocketVersion . requireNonEmptyList "Sec-WebSocket-Version"


versionNumber :: ParserT st String Word
versionNumber =
  branch
    $(char '0')
    (0 <$ notFollowedBy (skipSatisfyAscii isDigit))
    (boundedDecimalWord 255 (err "WebSocket version exceeds 255"))


renderSecWebSocketVersion :: SecWebSocketVersion -> M.Builder
renderSecWebSocketVersion (SecWebSocketVersion vs) =
  M.intersperse ", " (map M.wordDec (NE.toList vs))
