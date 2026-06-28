module Network.HTTP.Status (
  StatusCode (..),
  StatusCategory (..),
  statusCategory,
  statusReason,
  statusIsInformational,
  statusIsSuccessful,
  statusIsRedirection,
  statusIsClientError,
  statusIsServerError,
  status100,
  status101,
  status102,
  status103,
  status200,
  status201,
  status202,
  status203,
  status204,
  status205,
  status206,
  status207,
  status208,
  status226,
  status300,
  status301,
  status302,
  status303,
  status304,
  status305,
  status307,
  status308,
  status400,
  status401,
  status402,
  status403,
  status404,
  status405,
  status406,
  status407,
  status408,
  status409,
  status410,
  status411,
  status412,
  status413,
  status414,
  status415,
  status416,
  status417,
  status418,
  status421,
  status422,
  status423,
  status424,
  status425,
  status426,
  status428,
  status429,
  status431,
  status451,
  status500,
  status501,
  status502,
  status503,
  status504,
  status505,
  status506,
  status507,
  status508,
  status510,
  status511,
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Word (Word16)
import Data.ByteString (ByteString)


newtype StatusCode = StatusCode {statusCode :: Word16}
  deriving stock (Eq, Ord)
  deriving newtype (Hashable, NFData, Show)


-- | The broad category of an HTTP status code (RFC 9110 §15).
data StatusCategory
  = Informational
  | Successful
  | Redirection
  | ClientError
  | ServerError
  | Other
  deriving stock (Eq, Show)


-- | Classify a status code by its hundreds digit. Codes outside
-- @100–599@ are 'Other'.
statusCategory :: StatusCode -> StatusCategory
statusCategory (StatusCode n)
  | n >= 100 && n < 200 = Informational
  | n >= 200 && n < 300 = Successful
  | n >= 300 && n < 400 = Redirection
  | n >= 400 && n < 500 = ClientError
  | n >= 500 && n < 600 = ServerError
  | otherwise = Other


-- | True for 1xx informational codes.
statusIsInformational :: StatusCode -> Bool
statusIsInformational s = let n = statusCode s in n >= 100 && n < 200


-- | True for 2xx successful codes.
statusIsSuccessful :: StatusCode -> Bool
statusIsSuccessful s = let n = statusCode s in n >= 200 && n < 300


-- | True for 3xx redirection codes.
statusIsRedirection :: StatusCode -> Bool
statusIsRedirection s = let n = statusCode s in n >= 300 && n < 400


-- | True for 4xx client-error codes.
statusIsClientError :: StatusCode -> Bool
statusIsClientError s = let n = statusCode s in n >= 400 && n < 500


-- | True for 5xx server-error codes.
statusIsServerError :: StatusCode -> Bool
statusIsServerError s = let n = statusCode s in n >= 500 && n < 600


{- | The canonical IANA reason phrase for a known status code (RFC 9110
§15, plus the registered extensions in RFC 2518 \/ 4918 \/ 5842 \/ 3229
\/ 6585 \/ 7725 \/ 8297 \/ 8470). Falls back to a generic category
phrase for unregistered codes inside a known range (e.g. 599 →
@"Server Error"@), and an empty 'ByteString' for codes outside
@100–599@.
-}
statusReason :: StatusCode -> ByteString
statusReason s =
  let n = statusCode s
      fallback = case statusCategory s of
        Informational -> "Informational"
        Successful -> "OK"
        Redirection -> "Redirection"
        ClientError -> "Client Error"
        ServerError -> "Server Error"
        Other -> ""
  in case n of
       100 -> "Continue"
       101 -> "Switching Protocols"
       102 -> "Processing"
       103 -> "Early Hints"
       200 -> "OK"
       201 -> "Created"
       202 -> "Accepted"
       203 -> "Non-Authoritative Information"
       204 -> "No Content"
       205 -> "Reset Content"
       206 -> "Partial Content"
       207 -> "Multi-Status"
       208 -> "Already Reported"
       226 -> "IM Used"
       300 -> "Multiple Choices"
       301 -> "Moved Permanently"
       302 -> "Found"
       303 -> "See Other"
       304 -> "Not Modified"
       305 -> "Use Proxy"
       307 -> "Temporary Redirect"
       308 -> "Permanent Redirect"
       400 -> "Bad Request"
       401 -> "Unauthorized"
       402 -> "Payment Required"
       403 -> "Forbidden"
       404 -> "Not Found"
       405 -> "Method Not Allowed"
       406 -> "Not Acceptable"
       407 -> "Proxy Authentication Required"
       408 -> "Request Timeout"
       409 -> "Conflict"
       410 -> "Gone"
       411 -> "Length Required"
       412 -> "Precondition Failed"
       413 -> "Content Too Large"
       414 -> "URI Too Long"
       415 -> "Unsupported Media Type"
       416 -> "Range Not Satisfiable"
       417 -> "Expectation Failed"
       418 -> "I'm a teapot"
       421 -> "Misdirected Request"
       422 -> "Unprocessable Content"
       423 -> "Locked"
       424 -> "Failed Dependency"
       425 -> "Too Early"
       426 -> "Upgrade Required"
       428 -> "Precondition Required"
       429 -> "Too Many Requests"
       431 -> "Request Header Fields Too Large"
       451 -> "Unavailable For Legal Reasons"
       500 -> "Internal Server Error"
       501 -> "Not Implemented"
       502 -> "Bad Gateway"
       503 -> "Service Unavailable"
       504 -> "Gateway Timeout"
       505 -> "HTTP Version Not Supported"
       506 -> "Variant Also Negotiates"
       507 -> "Insufficient Storage"
       508 -> "Loop Detected"
       510 -> "Not Extended"
       511 -> "Network Authentication Required"
       _ -> fallback


-- 100	Continue	[RFC9110, Section 15.2.1]
status100 :: StatusCode
status100 = StatusCode 100


-- 101	Switching Protocols	[RFC9110, Section 15.2.2]
status101 :: StatusCode
status101 = StatusCode 101


-- 102	Processing	[RFC2518]
status102 :: StatusCode
status102 = StatusCode 102


-- 103	Early Hints	[RFC8297]
status103 :: StatusCode
status103 = StatusCode 103


-- 200	OK	[RFC9110, Section 15.3.1]
status200 :: StatusCode
status200 = StatusCode 200


-- 201	Created	[RFC9110, Section 15.3.2]
status201 :: StatusCode
status201 = StatusCode 201


-- 202	Accepted	[RFC9110, Section 15.3.3]
status202 :: StatusCode
status202 = StatusCode 202


-- 203	Non-Authoritative Information	[RFC9110, Section 15.3.4]
status203 :: StatusCode
status203 = StatusCode 203


-- 204	No Content	[RFC9110, Section 15.3.5]
status204 :: StatusCode
status204 = StatusCode 204


-- 205	Reset Content	[RFC9110, Section 15.3.6]
status205 :: StatusCode
status205 = StatusCode 205


-- 206	Partial Content	[RFC9110, Section 15.3.7]
status206 :: StatusCode
status206 = StatusCode 206


-- 207	Multi-Status	[RFC4918]
status207 :: StatusCode
status207 = StatusCode 207


-- 208	Already Reported	[RFC5842]
status208 :: StatusCode
status208 = StatusCode 208


-- 226	IM Used	[RFC3229]
status226 :: StatusCode
status226 = StatusCode 226


-- 300	Multiple Choices	[RFC9110, Section 15.4.1]
status300 :: StatusCode
status300 = StatusCode 300


-- 301	Moved Permanently	[RFC9110, Section 15.4.2]
status301 :: StatusCode
status301 = StatusCode 301


-- 302	Found	[RFC9110, Section 15.4.3]
status302 :: StatusCode
status302 = StatusCode 302


-- 303	See Other	[RFC9110, Section 15.4.4]
status303 :: StatusCode
status303 = StatusCode 303


-- 304	Not Modified	[RFC9110, Section 15.4.5]
status304 :: StatusCode
status304 = StatusCode 304


-- 305	Use Proxy	[RFC9110, Section 15.4.6]
status305 :: StatusCode
status305 = StatusCode 305


-- 307	Temporary Redirect	[RFC9110, Section 15.4.8]
status307 :: StatusCode
status307 = StatusCode 307


-- 308	Permanent Redirect	[RFC9110, Section 15.4.9]
status308 :: StatusCode
status308 = StatusCode 308


-- 400	Bad Request	[RFC9110, Section 15.5.1]
status400 :: StatusCode
status400 = StatusCode 400


-- 401	Unauthorized	[RFC9110, Section 15.5.2]
status401 :: StatusCode
status401 = StatusCode 401


-- 402	Payment Required	[RFC9110, Section 15.5.3]
status402 :: StatusCode
status402 = StatusCode 402


-- 403	Forbidden	[RFC9110, Section 15.5.4]
status403 :: StatusCode
status403 = StatusCode 403


-- 404	Not Found	[RFC9110, Section 15.5.5]
status404 :: StatusCode
status404 = StatusCode 404


-- 405	Method Not Allowed	[RFC9110, Section 15.5.6]
status405 :: StatusCode
status405 = StatusCode 405


-- 406	Not Acceptable	[RFC9110, Section 15.5.7]
status406 :: StatusCode
status406 = StatusCode 406


-- 407	Proxy Authentication Required	[RFC9110, Section 15.5.8]
status407 :: StatusCode
status407 = StatusCode 407


-- 408	Request Timeout	[RFC9110, Section 15.5.9]
status408 :: StatusCode
status408 = StatusCode 408


-- 409	Conflict	[RFC9110, Section 15.5.10]
status409 :: StatusCode
status409 = StatusCode 409


-- 410	Gone	[RFC9110, Section 15.5.11]
status410 :: StatusCode
status410 = StatusCode 410


-- 411	Length Required	[RFC9110, Section 15.5.12]
status411 :: StatusCode
status411 = StatusCode 411


-- 412	Precondition Failed	[RFC9110, Section 15.5.13]
status412 :: StatusCode
status412 = StatusCode 412


-- 413	Content Too Large	[RFC9110, Section 15.5.14]
status413 :: StatusCode
status413 = StatusCode 413


-- 414	URI Too Long	[RFC9110, Section 15.5.15]
status414 :: StatusCode
status414 = StatusCode 414


-- 415	Unsupported Media Type	[RFC9110, Section 15.5.16]
status415 :: StatusCode
status415 = StatusCode 415


-- 416	Range Not Satisfiable	[RFC9110, Section 15.5.17]
status416 :: StatusCode
status416 = StatusCode 416


-- 417	Expectation Failed	[RFC9110, Section 15.5.18]
status417 :: StatusCode
status417 = StatusCode 417


-- 418	(Unused)	[RFC9110, Section 15.5.19]
status418 :: StatusCode
status418 = StatusCode 418


-- 421	Misdirected Request	[RFC9110, Section 15.5.20]
status421 :: StatusCode
status421 = StatusCode 421


-- 422	Unprocessable Content	[RFC9110, Section 15.5.21]
status422 :: StatusCode
status422 = StatusCode 422


-- 423	Locked	[RFC4918]
status423 :: StatusCode
status423 = StatusCode 423


-- 424	Failed Dependency	[RFC4918]
status424 :: StatusCode
status424 = StatusCode 424


-- 425	Too Early	[RFC8470]
status425 :: StatusCode
status425 = StatusCode 425


-- 426	Upgrade Required	[RFC9110, Section 15.5.22]
status426 :: StatusCode
status426 = StatusCode 426


-- 428	Precondition Required	[RFC6585]
status428 :: StatusCode
status428 = StatusCode 428


-- 429	Too Many Requests	[RFC6585]
status429 :: StatusCode
status429 = StatusCode 429


-- 431	Request Header Fields Too Large	[RFC6585]
status431 :: StatusCode
status431 = StatusCode 431


-- 451	Unavailable For Legal Reasons	[RFC7725]
status451 :: StatusCode
status451 = StatusCode 451


-- 500	Internal Server Error	[RFC9110, Section 15.6.1]
status500 :: StatusCode
status500 = StatusCode 500


-- 501	Not Implemented	[RFC9110, Section 15.6.2]
status501 :: StatusCode
status501 = StatusCode 501


-- 502	Bad Gateway	[RFC9110, Section 15.6.3]
status502 :: StatusCode
status502 = StatusCode 502


-- 503	Service Unavailable	[RFC9110, Section 15.6.4]
status503 :: StatusCode
status503 = StatusCode 503


-- 504	Gateway Timeout	[RFC9110, Section 15.6.5]
status504 :: StatusCode
status504 = StatusCode 504


-- 505	HTTP Version Not Supported	[RFC9110, Section 15.6.6]
status505 :: StatusCode
status505 = StatusCode 505


-- 506	Variant Also Negotiates	[RFC2295]
status506 :: StatusCode
status506 = StatusCode 506


-- 507	Insufficient Storage	[RFC4918]
status507 :: StatusCode
status507 = StatusCode 507


-- 508	Loop Detected	[RFC5842]
status508 :: StatusCode
status508 = StatusCode 508


-- 510	Not Extended (OBSOLETED)	[RFC2774][status-change-http-experiments-to-historic]
status510 :: StatusCode
status510 = StatusCode 510


-- 511	Network Authentication Required	[RFC6585]
status511 :: StatusCode
status511 = StatusCode 511
