{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | HTTP request methods (RFC 9110 § 9).

This is the canonical method type for the whole wireform HTTP stack
(@wireform-http1@, @wireform-http2@, and the @wireform-http@ wrapper
all re-export it). The nine RFC 9110 core methods plus the newer
@QUERY@ method are represented as nullary constructors so the common
case pattern-matches with no allocation; every other token — WebDAV
extensions, custom verbs — is carried by 'NonStandard' as an interned
'Symbol' (O(1) equality and hashing, automatically GC'd).

The @m*@ constants cover the IANA-registered standard methods plus the
common WebDAV / CalDAV extensions; arbitrary tokens are constructible
via 'methodFromBytes' or the 'IsString' instance.
-}
module Network.HTTP.Methods (
  Method (..),
  methodToBytes,
  methodFromBytes,
  mkMethod,
  MethodError (..),

  -- * Semantics (RFC 9110 § 9.2)
  methodIsSafe,
  methodIsIdempotent,
  methodBodyAllowedInRequest,

  -- * Standard methods (RFC 9110 / 5789 / HTTP QUERY)
  mGet,
  mHead,
  mPost,
  mPut,
  mDelete,
  mConnect,
  mOptions,
  mTrace,
  mPatch,
  mQuery,

  -- * WebDAV (RFC 4918) and other registered extensions
  mACL,
  mBaselineControl,
  mBind,
  mCheckin,
  mCheckout,
  mCopy,
  mLabel,
  mLink,
  mLock,
  mMerge,
  mMkActivity,
  mMkCalendar,
  mMkCol,
  mMkRedirectRef,
  mMkWorkspace,
  mMove,
  mOrderPatch,
  mPropFind,
  mPropPatch,
  mRebind,
  mReport,
  mSearch,
  mUnbind,
  mUnlink,
  mUnlock,
  mUncheckout,
  mUpdate,
  mUpdateRedirectRef,
  mVersionControl,
  mPri,
) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Symbolize (Symbol, intern, unintern)


{- | An HTTP request method. The RFC 9110 core verbs (plus @QUERY@) are
nullary constructors; everything else is an interned 'NonStandard'
token holding the exact, case-sensitive bytes that appeared on the
wire.
-}
data Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  | QUERY
  | NonStandard !Symbol
  deriving stock (Eq, Ord, Generic)


-- 'Symbol' is itself 'NFData' / 'Hashable', so the generic defaults are
-- correct and cheap (a nullary constructor reduces to @()@; 'NonStandard'
-- forces / hashes the interned symbol, both O(1)).
instance NFData Method
instance Hashable Method


instance Show Method where
  showsPrec p m = showsPrec p (methodToBytes m)


-- | @fromString@ preserves the token case verbatim (RFC 9110 § 9.1
-- methods are case-sensitive); the standard verbs are recognised, and
-- anything else is interned as 'NonStandard'.
instance IsString Method where
  fromString = methodFromBytes . BS8.pack


-- | Wire-stable 'Binary' instance: a method serialises as its on-the-wire
-- token bytes, independent of the constructor layout.
instance Binary Method where
  put = put . methodToBytes
  get = methodFromBytes <$> get


-- | Render a 'Method' as its on-the-wire token bytes.
{-# INLINE methodToBytes #-}
methodToBytes :: Method -> ByteString
methodToBytes = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  CONNECT -> "CONNECT"
  OPTIONS -> "OPTIONS"
  TRACE -> "TRACE"
  PATCH -> "PATCH"
  QUERY -> "QUERY"
  NonStandard s -> unintern @ByteString s


{- | Parse a method token. The standard verbs are recognised by
length-discriminated 'ByteString' equality (one comparison per
branch); anything else is interned into 'NonStandard'. Interning
copies the bytes, so the parse buffer is never retained.
-}
{-# INLINE methodFromBytes #-}
methodFromBytes :: ByteString -> Method
methodFromBytes bs = case BS.length bs of
  3
    | bs == "GET" -> GET
    | bs == "PUT" -> PUT
    | otherwise -> NonStandard (intern bs)
  4
    | bs == "HEAD" -> HEAD
    | bs == "POST" -> POST
    | otherwise -> NonStandard (intern bs)
  5
    | bs == "PATCH" -> PATCH
    | bs == "TRACE" -> TRACE
    | bs == "QUERY" -> QUERY
    | otherwise -> NonStandard (intern bs)
  6
    | bs == "DELETE" -> DELETE
    | otherwise -> NonStandard (intern bs)
  7
    | bs == "OPTIONS" -> OPTIONS
    | bs == "CONNECT" -> CONNECT
    | otherwise -> NonStandard (intern bs)
  _ -> NonStandard (intern bs)


-- | Errors raised by 'mkMethod'.
data MethodError
  = MethodEmpty
  | MethodInvalidByte !Word8
  deriving stock (Eq, Show)


{- | Validating constructor: ensures the bytes form a non-empty @token@
(RFC 9110 § 5.6.2), i.e. only @tchar@ characters. The standard and
extension constants are already valid by construction.
-}
mkMethod :: ByteString -> Either MethodError Method
mkMethod bs
  | BS.null bs = Left MethodEmpty
  | otherwise = case BS.find (not . isTchar) bs of
      Nothing -> Right (methodFromBytes bs)
      Just w -> Left (MethodInvalidByte w)


-- | RFC 9110 § 5.6.2 @tchar@: token characters.
{-# INLINE isTchar #-}
isTchar :: Word8 -> Bool
isTchar w =
  (w >= 0x30 && w <= 0x39) -- 0-9
    || (w >= 0x41 && w <= 0x5A) -- A-Z
    || (w >= 0x61 && w <= 0x7A) -- a-z
    || w `BS.elem` "!#$%&'*+-.^_`|~"


{- | Safe methods (RFC 9110 § 9.2.1): no side effects beyond retrieval —
@GET@, @HEAD@, @OPTIONS@, @TRACE@.
-}
{-# INLINE methodIsSafe #-}
methodIsSafe :: Method -> Bool
methodIsSafe = \case
  GET -> True
  HEAD -> True
  OPTIONS -> True
  TRACE -> True
  _ -> False


{- | Idempotent methods (RFC 9110 § 9.2.2): the safe methods plus @PUT@
and @DELETE@.
-}
{-# INLINE methodIsIdempotent #-}
methodIsIdempotent :: Method -> Bool
methodIsIdempotent = \case
  PUT -> True
  DELETE -> True
  m -> methodIsSafe m


{- | Whether a request with this method may carry content. @CONNECT@
(RFC 9110 § 9.3.6 — the body belongs to the tunnel) and @TRACE@
(§ 9.3.8) have no request body.
-}
{-# INLINE methodBodyAllowedInRequest #-}
methodBodyAllowedInRequest :: Method -> Bool
methodBodyAllowedInRequest = \case
  CONNECT -> False
  TRACE -> False
  _ -> True


-- Standard methods --------------------------------------------------------

mGet, mHead, mPost, mPut, mDelete :: Method
mGet = GET
mHead = HEAD
mPost = POST
mPut = PUT
mDelete = DELETE


mConnect, mOptions, mTrace, mPatch, mQuery :: Method
mConnect = CONNECT
mOptions = OPTIONS
mTrace = TRACE
mPatch = PATCH
mQuery = QUERY


-- WebDAV and other registered extensions ----------------------------------

-- | Build a 'NonStandard' method from a literal token (interned once).
nonStandard :: ByteString -> Method
nonStandard = NonStandard . intern


mACL
  , mBaselineControl
  , mBind
  , mCheckin
  , mCheckout
  , mCopy
  , mLabel
  , mLink
  , mLock
  , mMerge
  , mMkActivity
  , mMkCalendar
  , mMkCol
  , mMkRedirectRef
  , mMkWorkspace
  , mMove
  , mOrderPatch
  , mPropFind
  , mPropPatch
  , mRebind
  , mReport
  , mSearch
  , mUnbind
  , mUnlink
  , mUnlock
  , mUncheckout
  , mUpdate
  , mUpdateRedirectRef
  , mVersionControl
  , mPri
    :: Method
mACL = nonStandard "ACL"
mBaselineControl = nonStandard "BASELINE-CONTROL"
mBind = nonStandard "BIND"
mCheckin = nonStandard "CHECKIN"
mCheckout = nonStandard "CHECKOUT"
mCopy = nonStandard "COPY"
mLabel = nonStandard "LABEL"
mLink = nonStandard "LINK"
mLock = nonStandard "LOCK"
mMerge = nonStandard "MERGE"
mMkActivity = nonStandard "MKACTIVITY"
mMkCalendar = nonStandard "MKCALENDAR"
mMkCol = nonStandard "MKCOL"
mMkRedirectRef = nonStandard "MKREDIRECTREF"
mMkWorkspace = nonStandard "MKWORKSPACE"
mMove = nonStandard "MOVE"
mOrderPatch = nonStandard "ORDERPATCH"
mPropFind = nonStandard "PROPFIND"
mPropPatch = nonStandard "PROPPATCH"
mRebind = nonStandard "REBIND"
mReport = nonStandard "REPORT"
mSearch = nonStandard "SEARCH"
mUnbind = nonStandard "UNBIND"
mUnlink = nonStandard "UNLINK"
mUnlock = nonStandard "UNLOCK"
mUncheckout = nonStandard "UNCHECKOUT"
mUpdate = nonStandard "UPDATE"
mUpdateRedirectRef = nonStandard "UPDATEREDIRECTREF"
mVersionControl = nonStandard "VERSION-CONTROL"
mPri = nonStandard "PRI"
