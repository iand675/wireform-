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
the field were absent. 'capsuleProtocolInUse' implements that receiver
rule and the typed codec reports malformed field values.

Spec: RFC 9297, <https://www.rfc-editor.org/rfc/rfc9297#section-3.4>.

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection",
"Network.HTTP.Headers.TransferEncoding".
-}
module Network.HTTP.Headers.CapsuleProtocol (
  CapsuleProtocol (..),
  capsuleProtocolInUse,
  capsuleProtocolParser,
  renderCapsuleProtocol,
) where

import Data.List.NonEmpty (NonEmpty (..))
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


  parseFromHeaders _ ("?1" :| []) = Right (CapsuleProtocol True)
  parseFromHeaders _ ("?0" :| []) = Right (CapsuleProtocol False)
  parseFromHeaders _ fields = parseSingleFieldLine "Capsule-Protocol" capsuleProtocolParser fields


  renderToHeaders _ (CapsuleProtocol True) = "?1"
  renderToHeaders _ (CapsuleProtocol False) = "?0"


  headerName _ = hCapsuleProtocol


capsuleProtocolParser :: ParserT st String CapsuleProtocol
capsuleProtocolParser = CapsuleProtocol <$> structuredFieldBooleanItem


-- | Check whether a valid received field enables capsule framing.
capsuleProtocolInUse :: HeaderMap -> Bool
capsuleProtocolInUse headers = case lookupHeader @CapsuleProtocol headers of
  Right (Just (CapsuleProtocol True)) -> True
  _ -> False


renderCapsuleProtocol :: CapsuleProtocol -> M.Builder
renderCapsuleProtocol (CapsuleProtocol True) = "?1"
renderCapsuleProtocol (CapsuleProtocol False) = "?0"
