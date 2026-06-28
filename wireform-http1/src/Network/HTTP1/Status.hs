{-# LANGUAGE PatternSynonyms #-}

{- | HTTP status codes for the HTTP\/1.x layer.

A thin view over the canonical 'Network.HTTP.Status.StatusCode' from
@hermes@. We expose a @Status@ alias plus a @Status@ pattern synonym
(so existing @Status 200@ construction / matching keeps working) and
the common named pattern synonyms; reason phrases come from hermes'
shared table.
-}
module Network.HTTP1.Status (
  Status,
  pattern Status,
  statusCode,
  statusReason,
  statusCategory,
  StatusCategory (..),

  -- * Common codes
  pattern Continue,
  pattern SwitchingProtocols,
  pattern OK,
  pattern Created,
  pattern Accepted,
  pattern NoContent,
  pattern PartialContent,
  pattern MovedPermanently,
  pattern Found,
  pattern SeeOther,
  pattern NotModified,
  pattern TemporaryRedirect,
  pattern PermanentRedirect,
  pattern BadRequest,
  pattern Unauthorized,
  pattern Forbidden,
  pattern NotFound,
  pattern MethodNotAllowed,
  pattern NotAcceptable,
  pattern RequestTimeout,
  pattern Conflict,
  pattern Gone,
  pattern LengthRequired,
  pattern PayloadTooLarge,
  pattern UriTooLong,
  pattern UnsupportedMediaType,
  pattern RangeNotSatisfiable,
  pattern ExpectationFailed,
  pattern UpgradeRequired,
  pattern PreconditionFailed,
  pattern PreconditionRequired,
  pattern TooManyRequests,
  pattern RequestHeaderFieldsTooLarge,
  pattern InternalServerError,
  pattern NotImplemented,
  pattern BadGateway,
  pattern ServiceUnavailable,
  pattern GatewayTimeout,
  pattern HttpVersionNotSupported,
) where

import Data.ByteString (ByteString)
import Data.Word (Word16)
import Network.HTTP.Status (StatusCategory (..), StatusCode (..), statusCategory, statusCode)
import qualified Network.HTTP.Status as HStatus


-- | The shared HTTP status type (see "Network.HTTP.Status").
type Status = StatusCode


-- | Construct / match a status by numeric code. Bidirectional, so
-- @Status 200@ builds and @case s of Status w -> ...@ matches.
pattern Status :: Word16 -> Status
pattern Status w = StatusCode w


{-# COMPLETE Status #-}


{- | The IANA-registered reason phrase for a status, from hermes' shared
table. Codes inside @100–599@ get the canonical phrase (or a generic
category phrase); codes outside that range keep the historical
@"Unknown"@.
-}
statusReason :: Status -> ByteString
statusReason (Status w)
  | w >= 100 && w < 600 = HStatus.statusReason (StatusCode w)
  | otherwise = "Unknown"


-- * Common pattern synonyms


pattern Continue, SwitchingProtocols :: Status
pattern Continue = StatusCode 100
pattern SwitchingProtocols = StatusCode 101


pattern OK, Created, Accepted, NoContent, PartialContent :: Status
pattern OK = StatusCode 200
pattern Created = StatusCode 201
pattern Accepted = StatusCode 202
pattern NoContent = StatusCode 204
pattern PartialContent = StatusCode 206


pattern
  MovedPermanently
  , Found
  , SeeOther
  , NotModified
  , TemporaryRedirect
  , PermanentRedirect
    :: Status
pattern MovedPermanently = StatusCode 301
pattern Found = StatusCode 302
pattern SeeOther = StatusCode 303
pattern NotModified = StatusCode 304
pattern TemporaryRedirect = StatusCode 307
pattern PermanentRedirect = StatusCode 308


pattern
  BadRequest
  , Unauthorized
  , Forbidden
  , NotFound
  , MethodNotAllowed
  , NotAcceptable
  , RequestTimeout
  , Conflict
  , Gone
  , LengthRequired
  , PayloadTooLarge
  , UriTooLong
  , UnsupportedMediaType
  , RangeNotSatisfiable
  , ExpectationFailed
  , UpgradeRequired
  , PreconditionFailed
  , PreconditionRequired
  , TooManyRequests
    :: Status
pattern BadRequest = StatusCode 400
pattern Unauthorized = StatusCode 401
pattern Forbidden = StatusCode 403
pattern NotFound = StatusCode 404
pattern MethodNotAllowed = StatusCode 405
pattern NotAcceptable = StatusCode 406
pattern RequestTimeout = StatusCode 408
pattern Conflict = StatusCode 409
pattern Gone = StatusCode 410
pattern LengthRequired = StatusCode 411
pattern PreconditionFailed = StatusCode 412
pattern PayloadTooLarge = StatusCode 413
pattern UriTooLong = StatusCode 414
pattern UnsupportedMediaType = StatusCode 415
pattern RangeNotSatisfiable = StatusCode 416
pattern ExpectationFailed = StatusCode 417
pattern UpgradeRequired = StatusCode 426
pattern PreconditionRequired = StatusCode 428
pattern TooManyRequests = StatusCode 429


pattern RequestHeaderFieldsTooLarge :: Status
pattern RequestHeaderFieldsTooLarge = StatusCode 431


pattern
  InternalServerError
  , NotImplemented
  , BadGateway
  , ServiceUnavailable
  , GatewayTimeout
  , HttpVersionNotSupported
    :: Status
pattern InternalServerError = StatusCode 500
pattern NotImplemented = StatusCode 501
pattern BadGateway = StatusCode 502
pattern ServiceUnavailable = StatusCode 503
pattern GatewayTimeout = StatusCode 504
pattern HttpVersionNotSupported = StatusCode 505
