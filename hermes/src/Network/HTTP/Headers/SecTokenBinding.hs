{- | The @Sec-Token-Binding@ request header field.

Defined by RFC 8473 (Token Binding over HTTP), §2. Its value is the
base64url encoding (without padding) of a @TokenBindingMessage@ structure
(RFC 8471). The inner binary structure is opaque at the HTTP layer, so this
module preserves the raw base64url text faithfully in a newtype rather than
fabricating a structured decoding of the binding message.

Spec: <https://www.rfc-editor.org/rfc/rfc8473#section-2>

See also: "Network.HTTP.Headers.IncludeReferredTokenBindingID", "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.SecTokenBinding (
  SecTokenBinding (..),
  secTokenBindingParser,
  renderSecTokenBinding,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecTokenBinding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Sec-Token-Binding@ value: the base64url-encoded @TokenBindingMessage@, preserved verbatim.
newtype SecTokenBinding = SecTokenBinding {secTokenBindingMessage :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader SecTokenBinding where
  type ParseFailure SecTokenBinding = String
  type Cardinality SecTokenBinding = 'ZeroOrOne
  type Direction SecTokenBinding = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser secTokenBindingParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing Sec-Token-Binding header: " <> show rest
      Fail -> Left "Failed to parse Sec-Token-Binding header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSecTokenBinding


  headerName _ = hSecTokenBinding


secTokenBindingParser :: ParserT st String SecTokenBinding
secTokenBindingParser = SecTokenBinding <$> takeRestShortText


renderSecTokenBinding :: SecTokenBinding -> M.Builder
renderSecTokenBinding (SecTokenBinding msg) = shortText msg
