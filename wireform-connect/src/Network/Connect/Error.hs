{-# LANGUAGE OverloadedStrings #-}

-- | Connect error model: codes, HTTP-status mapping, and the JSON envelope.
--
-- Connect reuses gRPC's status-code taxonomy exactly, so 'ConnectCode' is
-- just 'Network.GRPC.Spec.GrpcError'. This module adds the Connect
-- string-code names, the code↔HTTP-status tables, the @Error@ / @ErrorDetail@
-- JSON envelope, and a throwable 'ConnectException'.
module Network.Connect.Error (
  -- * Codes
  ConnectCode,
  connectCodeName,
  connectCodeFromName,
  allConnectCodes,

  -- * HTTP-status mapping
  connectCodeToHttpStatus,
  httpStatusToConnectCode,

  -- * Error envelope
  ErrorDetail (..),
  errorDetailFromAny,
  ConnectError (..),
  encodeConnectError,
  decodeConnectError,

  -- * Exceptions
  ConnectException (..),
  throwConnect,
  toConnectError,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.URL qualified as B64U
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Status
  ( Status
  , pattern Status
  , status400
  , status401
  , status403
  , status404
  , status409
  , status429
  , status500
  , status501
  , status502
  , status503
  , status504
  )
import Network.GRPC.Spec (GrpcError (..))
import Proto.Google.Protobuf.Any (Any (..))
import Proto.Google.Protobuf.Any.Util (typeNameFromUrl)

-- | A Connect error code. Identical to gRPC's taxonomy.
type ConnectCode = GrpcError

-- | The Connect string code for a code (e.g. @unimplemented@).
connectCodeName :: ConnectCode -> Text
connectCodeName = \case
  GrpcCancelled -> "canceled"
  GrpcUnknown -> "unknown"
  GrpcInvalidArgument -> "invalid_argument"
  GrpcDeadlineExceeded -> "deadline_exceeded"
  GrpcNotFound -> "not_found"
  GrpcAlreadyExists -> "already_exists"
  GrpcPermissionDenied -> "permission_denied"
  GrpcResourceExhausted -> "resource_exhausted"
  GrpcFailedPrecondition -> "failed_precondition"
  GrpcAborted -> "aborted"
  GrpcOutOfRange -> "out_of_range"
  GrpcUnimplemented -> "unimplemented"
  GrpcInternal -> "internal"
  GrpcUnavailable -> "unavailable"
  GrpcDataLoss -> "data_loss"
  GrpcUnauthenticated -> "unauthenticated"

-- | Inverse of 'connectCodeName'.
connectCodeFromName :: Text -> Maybe ConnectCode
connectCodeFromName = \case
  "canceled" -> Just GrpcCancelled
  "unknown" -> Just GrpcUnknown
  "invalid_argument" -> Just GrpcInvalidArgument
  "deadline_exceeded" -> Just GrpcDeadlineExceeded
  "not_found" -> Just GrpcNotFound
  "already_exists" -> Just GrpcAlreadyExists
  "permission_denied" -> Just GrpcPermissionDenied
  "resource_exhausted" -> Just GrpcResourceExhausted
  "failed_precondition" -> Just GrpcFailedPrecondition
  "aborted" -> Just GrpcAborted
  "out_of_range" -> Just GrpcOutOfRange
  "unimplemented" -> Just GrpcUnimplemented
  "internal" -> Just GrpcInternal
  "unavailable" -> Just GrpcUnavailable
  "data_loss" -> Just GrpcDataLoss
  "unauthenticated" -> Just GrpcUnauthenticated
  _ -> Nothing

-- | All sixteen codes, in canonical order.
allConnectCodes :: [ConnectCode]
allConnectCodes =
  [ GrpcCancelled
  , GrpcUnknown
  , GrpcInvalidArgument
  , GrpcDeadlineExceeded
  , GrpcNotFound
  , GrpcAlreadyExists
  , GrpcPermissionDenied
  , GrpcResourceExhausted
  , GrpcFailedPrecondition
  , GrpcAborted
  , GrpcOutOfRange
  , GrpcUnimplemented
  , GrpcInternal
  , GrpcUnavailable
  , GrpcDataLoss
  , GrpcUnauthenticated
  ]

-- | The HTTP status Connect maps a code to (spec table). @canceled@ is 499,
-- which has no named constant, so it is built with the 'Status' pattern.
connectCodeToHttpStatus :: ConnectCode -> Status
connectCodeToHttpStatus = \case
  GrpcCancelled -> Status 499
  GrpcUnknown -> status500
  GrpcInvalidArgument -> status400
  GrpcDeadlineExceeded -> status504
  GrpcNotFound -> status404
  GrpcAlreadyExists -> status409
  GrpcPermissionDenied -> status403
  GrpcResourceExhausted -> status429
  GrpcFailedPrecondition -> status400
  GrpcAborted -> status409
  GrpcOutOfRange -> status400
  GrpcUnimplemented -> status501
  GrpcInternal -> status500
  GrpcUnavailable -> status503
  GrpcDataLoss -> status500
  GrpcUnauthenticated -> status401

-- | Infer a Connect code from a non-200 HTTP status (spec inference table),
-- for clients reading a response with no or garbled JSON body.
httpStatusToConnectCode :: Status -> ConnectCode
httpStatusToConnectCode (Status w) = case w of
  400 -> GrpcInternal
  401 -> GrpcUnauthenticated
  403 -> GrpcPermissionDenied
  404 -> GrpcUnimplemented
  429 -> GrpcUnavailable
  502 -> GrpcUnavailable
  503 -> GrpcUnavailable
  504 -> GrpcUnavailable
  _ -> GrpcUnknown

------------------------------------------------------------------------
-- Error envelope
------------------------------------------------------------------------

-- | A strongly-typed Connect error detail. On the wire this is
-- @{"type": …, "value": <base64>, "debug"?: …}@: @type@ is the
-- fully-qualified Protobuf message name, @value@ is the raw binary Protobuf
-- of the detail message, base64-encoded (standard, unpadded on encode;
-- padded or unpadded accepted on decode), and @debug@ is an optional
-- human-readable JSON rendering.
data ErrorDetail = ErrorDetail
  { edType :: !Text
  , edValue :: !ByteString
  -- ^ Raw binary Protobuf bytes of the detail message.
  , edDebug :: !(Maybe Aeson.Value)
  }
  deriving stock (Eq, Show)

-- | Build an 'ErrorDetail' from a @google.protobuf.Any@: the type is the
-- message name (everything after the last @\/@ of the type-URL), the value
-- is the Any's raw payload bytes.
errorDetailFromAny :: Any -> ErrorDetail
errorDetailFromAny a =
  ErrorDetail
    { edType = typeNameFromUrl (anyTypeUrl a)
    , edValue = anyValue a
    , edDebug = Nothing
    }

-- | A Connect error: code, optional message, optional details.
data ConnectError = ConnectError
  { ceCode :: !ConnectCode
  , ceMessage :: !(Maybe Text)
  , ceDetails :: ![ErrorDetail]
  }
  deriving stock (Eq, Show)

-- | Render a 'ConnectError' as the Connect JSON @Error@ object.
--
-- @{"code": <name>, "message"?: …, "details"?: [...]}@
-- @message@ is omitted when absent; @details@ is omitted when empty.
encodeConnectError :: ConnectError -> Aeson.Value
encodeConnectError err =
  Aeson.object (codePair : messagePair <> detailsPair)
  where
    codePair = "code" Aeson..= connectCodeName (ceCode err)
    messagePair =
      case ceMessage err of
        Just m | not (T.null m) -> ["message" Aeson..= m]
        _ -> []
    detailsPair =
      case ceDetails err of
        [] -> []
        ds -> ["details" Aeson..= map encodeDetail ds]

encodeDetail :: ErrorDetail -> Aeson.Value
encodeDetail d =
  Aeson.object
    ( [ "type" Aeson..= edType d
      , "value" Aeson..= stripPad (decodeUtf8 (B64.encode (edValue d)))
      ]
        <> debugPair
    )
  where
    debugPair =
      case edDebug d of
        Just v -> ["debug" Aeson..= v]
        Nothing -> []

-- | Parse a Connect JSON @Error@ object. Rejects @{}@ and @{"code": null}@
-- (invalid per spec). A missing @message@ yields 'Nothing'; a missing
-- @details@ yields @[]@.
decodeConnectError :: Aeson.Value -> Either String ConnectError
decodeConnectError (Aeson.Object o) = do
  rawCode <- maybe (Left "Connect Error: missing \"code\"") Right (KeyMap.lookup "code" o)
  codeName <- case rawCode of
    Aeson.Null -> Left "Connect Error: \"code\" must not be null"
    Aeson.String t -> Right t
    _ -> Left "Connect Error: \"code\" must be a string"
  code <-
    maybe
      (Left ("Connect Error: unknown code " <> T.unpack codeName))
      Right
      (connectCodeFromName codeName)
  let message = case KeyMap.lookup "message" o of
        Just (Aeson.String t) -> if T.null t then Nothing else Just t
        _ -> Nothing
  details <- case KeyMap.lookup "details" o of
    Nothing -> pure []
    Just (Aeson.Array xs) -> traverse decodeDetail (toList xs)
    Just _ -> Left "Connect Error: \"details\" must be an array"
  pure (ConnectError code message details)
decodeConnectError _ = Left "Connect Error: expected a JSON object"

decodeDetail :: Aeson.Value -> Either String ErrorDetail
decodeDetail (Aeson.Object o) = do
  typ <- reqText "type"
  valRaw <-
    maybe
      (Left "Connect Error detail: missing \"value\"")
      Right
      (KeyMap.lookup "value" o)
  val <- case valRaw of
    Aeson.String t -> first ("Connect Error detail value: " <>) (decodeB64 t)
    _ -> Left "Connect Error detail: \"value\" must be a string"
  let dbg = KeyMap.lookup "debug" o
  pure (ErrorDetail typ val dbg)
  where
    reqText name = case KeyMap.lookup name o of
      Just (Aeson.String t) -> Right t
      _ -> Left ("Connect Error detail: missing or non-string \"" <> T.unpack (AKey.toText name) <> "\"")
decodeDetail _ = Left "Connect Error detail: expected an object"

-- Accept standard (padded or unpadded) and URL-safe base64 for the value.
decodeB64 :: Text -> Either String ByteString
decodeB64 t =
  case B64.decode (pad (encodeUtf8 t)) of
    Right bs -> Right bs
    Left _ -> case B64U.decode (pad (encodeUtf8 t)) of
      Right bs -> Right bs
      Left e -> Left e
  where
    pad bs =
      let n = BS.length bs `mod` 4
       in if n == 0 then bs else bs <> BS.replicate (4 - n) 0x3D

stripPad :: Text -> Text
stripPad = T.dropWhileEnd (== '=')

------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------

-- | A throwable Connect error. Caught by the server runtime and rendered to
-- the wire; thrown by the client runtime when a non-200 response carries (or
-- implies) an error.
newtype ConnectException = ConnectException ConnectError
  deriving stock (Eq, Show)

instance Exception ConnectException

-- | Throw a 'ConnectException' with the given code and message.
throwConnect :: ConnectCode -> Text -> IO a
throwConnect code msg = throwIO (ConnectException (ConnectError code (justIf msg) []))
  where
    justIf m = if T.null m then Nothing else Just m

-- | Construct a 'ConnectError' from a code and an optional message.
toConnectError :: ConnectCode -> Maybe Text -> ConnectError
toConnectError code msg = ConnectError code msg []
