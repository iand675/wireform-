{-# LANGUAGE MagicHash #-}

{- |
RFC 6455 §11.3.4 @Sec-WebSocket-Protocol@ — names the application
subprotocol(s) used over the WebSocket connection. A client request
offers a comma-separated, preference-ordered list of subprotocol
tokens; a server response echoes the single token it selected.

== Grammar

@
Sec-WebSocket-Protocol = 1#token
@

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4>

See also: "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.SecWebSocketVersion", "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.ALPN", "Network.HTTP.Headers.Upgrade".
-}
module Network.HTTP.Headers.SecWebSocketProtocol (
  SecWebSocketProtocol (..),
  secWebSocketProtocolParser,
  renderSecWebSocketProtocol,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts (Char (C#), Int (I#), eqAddr#, indexCharOffAddr#, isTrue#, minusAddr#, plusAddr#, (<=#))
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Ptr (Ptr (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketProtocol)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- | A non-empty, preference-ordered list of subprotocol tokens.
newtype SecWebSocketProtocol = SecWebSocketProtocol {secWebSocketProtocols :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketProtocol where
  type ParseFailure SecWebSocketProtocol = String
  type Cardinality SecWebSocketProtocol = 'ZeroOrOne
  type Direction SecWebSocketProtocol = 'RequestAndResponse


  parseFromHeaders settings headers
    | Just HeaderResponse <- headerMessage settings =
        parseTokenFieldLine "Sec-WebSocket-Protocol" (SecWebSocketProtocol . NE.singleton) headers
    | value NE.:| [] <- headers =
        parseShortOffers value (parseFieldLine "Sec-WebSocket-Protocol" secWebSocketProtocolParser value)
    | otherwise =
        parseListFieldLines "Sec-WebSocket-Protocol" protocolList headers
          >>= requireNonEmptyList "Sec-WebSocket-Protocol"
          >>= distinctProtocols


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketProtocol


  headerName _ = hSecWebSocketProtocol


secWebSocketProtocolParser :: ParserT st String SecWebSocketProtocol
secWebSocketProtocolParser =
  httpList1 "Sec-WebSocket-Protocol" rfc9110Token >>= either err pure . distinctProtocols


protocolList :: ParserT st String [ST.ShortText]
protocolList = httpList rfc9110Token


-- Common single-line offers have one token or two separated by comma and OWS.
-- Cap speculative scanning before falling back to the general field parser.
parseShortOffers :: BSI.ByteString -> Either String SecWebSocketProtocol -> Either String SecWebSocketProtocol
parseShortOffers (BSI.BS input@(ForeignPtr start _) (I# size)) fallback
  | isTrue# (size <=# 64#) = unsafeDupablePerformIO $ withForeignPtr input $ \_ -> scanOffers
  | otherwise = fallback
  where
    end = plusAddr# start size

    scanOffers = case tokenAsciiSet of
      AsciiSet low high ->
        let tokenEnd cur
              | isTrue# (eqAddr# cur end) = cur
              | asciiMember low high (C# (indexCharOffAddr# cur 0#)) = tokenEnd (plusAddr# cur 1#)
              | otherwise = cur
        in case tokenEnd start of
             firstEnd
               | isTrue# (eqAddr# firstEnd start) -> pure fallback
               | isTrue# (eqAddr# firstEnd end) -> do
                   first <- copyToken start firstEnd
                   pure (Right (SecWebSocketProtocol (first NE.:| [])))
               | C# (indexCharOffAddr# firstEnd 0#) == ','
               , let second = skipWhitespace (plusAddr# firstEnd 1#)
               , not (isTrue# (eqAddr# second end))
               , isTrue# (eqAddr# (tokenEnd second) end) -> do
                   first <- copyToken start firstEnd
                   lastToken <- copyToken second end
                   pure $! distinctProtocols (first NE.:| [lastToken])
               | otherwise -> pure fallback

    skipWhitespace cur
      | isTrue# (eqAddr# cur end) = cur
      | let byte = C# (indexCharOffAddr# cur 0#)
      , byte == ' ' || byte == '\t' =
          skipWhitespace (plusAddr# cur 1#)
      | otherwise = cur

    -- Both endpoints were checked by the scanner. withForeignPtr keeps the
    -- source alive until its bytes have been copied into compact storage.
    copyToken begin finish =
      STU.fromShortByteStringUnsafe <$> SBS.createFromPtr (Ptr begin) (I# (minusAddr# finish begin))


distinctProtocols :: NE.NonEmpty ST.ShortText -> Either String SecWebSocketProtocol
distinctProtocols protocols
  | distinct protocols = Right (SecWebSocketProtocol protocols)
  | otherwise = Left "Duplicate Sec-WebSocket-Protocol token"
  where
    distinct (_ NE.:| []) = True
    distinct (first NE.:| [second]) = first /= second
    distinct (first NE.:| rest) = Set.size (Set.fromList (first : rest)) == 1 + length rest
{-# INLINE distinctProtocols #-}


renderSecWebSocketProtocol :: SecWebSocketProtocol -> M.Builder
renderSecWebSocketProtocol (SecWebSocketProtocol ps) =
  M.intersperse ", " (map shortText (NE.toList ps))
