{- | Visibility context: claims payload and proof (spec §8.2) — and query
admission (spec §14.3).

The payload rides in the URL as @vc={base64url(canonicalJson(claims))}@ and
is part of the cache key. The proof rides in the @X-Vc-Auth@ header as
@{exp}.{sig}@ — outside the cache key, so token rotation never disturbs
cached entries. Origins verify the proof on every request; caches never
verify anything.

The proof scheme is deployment-pluggable ('ProofVerifier'); the bundled
implementation is HMAC-SHA256 by a shared-secret auth service:
@sig = base64url(HMAC-SHA256(secret, payloadB64 <> "." <> exp))@ with
@exp@ a unix timestamp in seconds rendered as decimal.

== Query admission (spec §14.3)

'QueryAdmission' governs who may spend compile resources on the cold path.
In 'AdmitSigned' mode the request carries @X-Lattice-Query-Sig@: the
__unpadded__ base64url of a detached Ed25519 signature over the UTF-8
bytes of the query's /canonical/ text. The origin re-canonicalizes the
presented query and verifies against its own canonical text, so any
whitespace-equivalent spelling of an admitted query is admitted. Any one
configured key verifying admits the query. Admission governs resources,
never confidentiality (the visibility partition bounds what a query sees),
so hash-form GETs of already-memoized queries are never re-verified.
-}
module Lattice.Server.Auth (
  ClaimsPayload (..),
  encodeClaims,
  decodeClaims,
  ProofVerifier (..),
  hmacVerifier,
  hmacProof,
  ProofError (..),

  -- * Query admission (spec §14.3)
  QueryAdmission (..),
  admitQuery,
  signQuery,

  -- ** Ed25519 key material (re-exported from crypton for consumers)
  Ed25519.PublicKey,
  Ed25519.SecretKey,
  Ed25519.generateSecretKey,
  Ed25519.toPublic,
) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime)
import Lattice.Types (ClaimName (..), Claims)
import Lattice.Value (canonicalJson)


-- | A decoded @vc@ parameter: the claims and the exact payload bytes presented.
data ClaimsPayload = ClaimsPayload
  { cpClaims :: Claims
  , cpRaw :: ByteString
  -- ^ The base64url text as presented (the cache-key component and the
  -- proof's subject).
  }
  deriving stock (Eq, Show)


-- | Canonical payload: base64url of the canonical JSON object of claims.
encodeClaims :: Claims -> Text
encodeClaims claims =
  TE.decodeUtf8 . B64U.encodeUnpadded . canonicalJson . A.Object $
    KM.fromList
      [ (AK.fromText c, v)
      | (ClaimName c, v) <- Map.toList claims
      ]


decodeClaims :: Text -> Either Text ClaimsPayload
decodeClaims t = do
  bs <-
    either (Left . T.pack) Right $
      B64U.decodeUnpadded (TE.encodeUtf8 t)
  v <- maybe (Left "vc payload is not JSON") Right (A.decodeStrict bs)
  case v of
    A.Object o ->
      Right
        ClaimsPayload
          { cpClaims = Map.fromList [(ClaimName (AK.toText k), val) | (k, val) <- KM.toList o]
          , cpRaw = TE.encodeUtf8 t
          }
    _ -> Left "vc payload must be a JSON object"


data ProofError
  = ProofMissing
  | ProofMalformed
  | ProofExpired
  | ProofInvalid
  deriving stock (Eq, Show)


{- | Deployment-supplied proof verification: given the presented payload
(base64url bytes) and the @X-Vc-Auth@ header value, accept or reject.

'proofExpiry' is the live-query seam (spec §12): the instant the presented
proof stops being valid, when the scheme carries one. A verifier reporting
'Nothing' opts out of the reauth cycle — the subscription lives until the
transport drops. The read is pure (it parses the proof, it does not verify
it); callers only consult it after 'verifyProof' accepted.
-}
data ProofVerifier = ProofVerifier
  { verifyProof :: ByteString -> Maybe ByteString -> IO (Either ProofError ())
  , proofExpiry :: ByteString -> Maybe ByteString -> Maybe POSIXTime
  }


-- | @{exp}.{sig}@ with @sig = base64url(HMAC-SHA256(secret, payload <> "." <> exp))@.
hmacVerifier ::
  -- | Shared secret.
  ByteString ->
  -- | Current time supplier.
  IO POSIXTime ->
  ProofVerifier
hmacVerifier secret now =
  ProofVerifier
    { verifyProof = \payload mProof -> verify payload mProof
    , proofExpiry = \_payload mProof -> do
        proof <- mProof
        let (expBytes, rest) = BS8.break (== '.') proof
        _ <- BS8.stripPrefix "." rest
        (expSecs, "") <- BS8.readInteger expBytes
        pure (fromInteger expSecs)
    }
  where
    verify payload mProof = case mProof of
      Nothing -> pure (Left ProofMissing)
      Just proof -> case BS8.break (== '.') proof of
        (expBytes, rest)
          | Just (sig) <- BS8.stripPrefix "." rest
          , Just (expSecs, "") <- BS8.readInteger expBytes -> do
              t <- now
              if fromInteger expSecs < t
                then pure (Left ProofExpired)
                else
                  pure $
                    if BA.constEq (TE.encodeUtf8 (hmacSig secret payload expSecs)) sig
                      then Right ()
                      else Left ProofInvalid
        _ -> pure (Left ProofMalformed)


-- | Mint a proof (test and demo helper; production mints live in the auth service).
hmacProof :: ByteString -> Text -> Integer -> Text
hmacProof secret payloadB64 expSecs =
  T.pack (show expSecs) <> "." <> hmacSig secret (TE.encodeUtf8 payloadB64) expSecs


hmacSig :: ByteString -> ByteString -> Integer -> Text
hmacSig secret payload expSecs =
  TE.decodeUtf8 . B64U.encodeUnpadded . BA.convert $
    hmacGetDigest (HMAC.hmac @_ @_ @SHA256 secret (payload <> "." <> BS8.pack (show expSecs)))
  where
    hmacGetDigest = HMAC.hmacGetDigest


-- ---------------------------------------------------------------------------
-- Query admission (spec §14.3)
-- ---------------------------------------------------------------------------

{- | Cold-path admission mode (spec §14.3): who may spend compile resources.

'AdmitSigned' restores build-time allowlisting for closed deployments: the
release pipeline signs each query's canonical text and the signature ships
in the client artifact. Trust rides with the client binary; no server-side
registry exists in either mode.
-}
data QueryAdmission
  = -- | Any well-typed, within-budget query compiles.
    AdmitOpen
  | -- | The request must carry @X-Lattice-Query-Sig@ verifying under one
    -- of these keys.
    AdmitSigned [Ed25519.PublicKey]
  deriving stock (Eq, Show)


{- | Enforce an admission mode: the presented @X-Lattice-Query-Sig@ value
(if any) against the origin's canonical text for the query. @Left@ carries
the human detail for the @403 lattice:admission-denied@ problem. Unpadded
base64url is the wire form; padded input is tolerated on decode.
-}
admitQuery :: QueryAdmission -> Maybe ByteString -> Text -> Either Text ()
admitQuery AdmitOpen _ _ = Right ()
admitQuery (AdmitSigned keys) mSig canonical = case mSig of
  Nothing -> Left "signed admission: the X-Lattice-Query-Sig header is required"
  Just presented -> do
    sig <- decodeSig presented
    if any (\k -> Ed25519.verify k msg sig) keys
      then Right ()
      else Left "signed admission: the signature does not verify against the canonical text under any configured key"
  where
    msg = TE.encodeUtf8 canonical
    decodeSig bs = case B64U.decodeUnpadded bs of
      Right raw -> asSignature raw
      Left _ -> case B64U.decode bs of
        Right raw -> asSignature raw
        Left _ -> Left "signed admission: X-Lattice-Query-Sig is not valid base64url"
    asSignature raw = case Ed25519.signature raw of
      CryptoPassed sig -> Right sig
      CryptoFailed _ -> Left "signed admission: X-Lattice-Query-Sig is not an Ed25519 signature"


{- | Mint an @X-Lattice-Query-Sig@ value over a query's __canonical__ text
(test and demo helper; production signing lives in the release pipeline).
-}
signQuery :: Ed25519.SecretKey -> Text -> Text
signQuery sk canonical =
  TE.decodeUtf8 . B64U.encodeUnpadded . BA.convert $
    Ed25519.sign sk (Ed25519.toPublic sk) (TE.encodeUtf8 canonical)
