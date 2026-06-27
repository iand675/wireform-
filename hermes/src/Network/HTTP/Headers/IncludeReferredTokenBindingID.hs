{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | The @Include-Referred-Token-Binding-ID@ request header field.

Defined by RFC 8473 (Token Binding over HTTP), §5.3. A client sets this
field to signal that the Token Binding message in 'Sec-Token-Binding' also
conveys a /referred/ Token Binding ID for a different (referred) server. The
value is the boolean-ish literal @true@; this module models it as a structured
'Bool' so the (otherwise trivial) grammar is captured rather than treated as
opaque text.

Spec: <https://www.rfc-editor.org/rfc/rfc8473#section-5.3>

See also: "Network.HTTP.Headers.SecTokenBinding", "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.IncludeReferredTokenBindingID (
  IncludeReferredTokenBindingID (..),
  includeReferredTokenBindingIDParser,
  renderIncludeReferredTokenBindingID,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIncludeReferredTokenBindingID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Include-Referred-Token-Binding-ID@ value: the boolean-ish flag @true@/@false@.
newtype IncludeReferredTokenBindingID = IncludeReferredTokenBindingID
  {includeReferredTokenBindingID :: Bool}
  deriving stock (Eq, Show)


instance KnownHeader IncludeReferredTokenBindingID where
  type ParseFailure IncludeReferredTokenBindingID = String
  type Cardinality IncludeReferredTokenBindingID = 'ZeroOrOne
  type Direction IncludeReferredTokenBindingID = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser includeReferredTokenBindingIDParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing Include-Referred-Token-Binding-ID header: " <> show rest
      Fail -> Left "Failed to parse Include-Referred-Token-Binding-ID header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIncludeReferredTokenBindingID


  headerName _ = hIncludeReferredTokenBindingID


includeReferredTokenBindingIDParser :: ParserT st String IncludeReferredTokenBindingID
includeReferredTokenBindingIDParser =
  IncludeReferredTokenBindingID
    <$> $( switch
            [|
              case _ of
                "true" -> pure True
                "false" -> pure False
              |]
         )


renderIncludeReferredTokenBindingID :: IncludeReferredTokenBindingID -> M.Builder
renderIncludeReferredTokenBindingID (IncludeReferredTokenBindingID b) =
  if b then "true" else "false"
