{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9297 §3.4 @Capsule-Protocol@ — an Item Structured Field whose value
is a Boolean indicating that the message content is composed of capsules
(the Capsule Protocol). A value of @?1@ (true) signals capsule framing;
both endpoints include it during the handshake to confirm bidirectional
support.

@
Capsule-Protocol = sf-boolean   ; \"?1\" / \"?0\"
@

RFC 9297 §3.4 defines no parameters for this field and instructs receivers
to ignore unknown parameters; any non-Boolean value is to be handled as if
the field were absent. We surface the Boolean value directly.

Spec: RFC 9297, <https://www.rfc-editor.org/rfc/rfc9297#section-3.4>.

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection",
"Network.HTTP.Headers.TransferEncoding".
-}
module Network.HTTP.Headers.CapsuleProtocol (
  CapsuleProtocol (..),
  capsuleProtocolParser,
  renderCapsuleProtocol,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCapsuleProtocol)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | A parsed @Capsule-Protocol@ value. 'True' (@?1@) indicates the message
content is composed of capsules; 'False' (@?0@) indicates it is not.
-}
newtype CapsuleProtocol = CapsuleProtocol {capsuleProtocolEnabled :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader CapsuleProtocol where
  type ParseFailure CapsuleProtocol = String
  type Cardinality CapsuleProtocol = 'ZeroOrOne
  type Direction CapsuleProtocol = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser capsuleProtocolParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Capsule-Protocol header: " <> show rest
    Fail -> Left "Failed to parse Capsule-Protocol header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCapsuleProtocol


  headerName _ = hCapsuleProtocol


capsuleProtocolParser :: ParserT st String CapsuleProtocol
capsuleProtocolParser =
  $( switch
      [|
        case _ of
          "?1" -> pure (CapsuleProtocol True)
          "?0" -> pure (CapsuleProtocol False)
        |]
   )


renderCapsuleProtocol :: CapsuleProtocol -> M.Builder
renderCapsuleProtocol (CapsuleProtocol True) = "?1"
renderCapsuleProtocol (CapsuleProtocol False) = "?0"
