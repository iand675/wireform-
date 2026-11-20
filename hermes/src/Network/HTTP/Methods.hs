{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | HTTP request methods (RFC 9110 § 9).

This is the canonical method type for the whole wireform HTTP stack
(@wireform-http1@, @wireform-http2@, and the @wireform-http@ wrapper
all re-export it). The nine RFC 9110 core methods plus the newer
@QUERY@ method are represented as nullary constructors so the common
case pattern-matches with no allocation. Extension methods own their wire
bytes and expose an interned 'Symbol' through the 'NonStandard' pattern.

The @m*@ constants cover the IANA-registered standard methods plus the
common WebDAV / CalDAV extensions; arbitrary tokens are constructible
via 'methodFromBytes' or the 'IsString' instance.
-}
module Network.HTTP.Methods (
  Method (GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH, QUERY, NonStandard),
  methodToBytes,
  methodFromBytes,
  methodTokenParser,
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

import Control.DeepSeq (NFData (..))
import Data.Binary (Binary (..))
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BSI
import Data.Hashable (Hashable (..))
import Data.String (IsString (..))
import Data.Word (Word8)
import GHC.Exts (Int (I#), dataToTag#)
import GHC.Generics (Generic (..))
import qualified GHC.Generics as G
import Network.HTTP.Grammar.Parser (ParserT, isTokenChar, rfc9110TokenBS)
import Symbolize (Symbol, intern, internUnsafe, unintern)


{- | An HTTP request method with case-sensitive spelling. The RFC 9110
core verbs (plus @QUERY@) are nullary constructors.
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
  | ExtensionMethod {-# UNPACK #-} !MethodExtension
  deriving stock (Eq, Ord)


-- Keep the symbol as a shared lazy view for callers that use Symbolize.
data MethodExtension = MethodExtension !ByteString Symbol
  deriving stock (Generic)


instance Eq MethodExtension where
  MethodExtension a _ == MethodExtension b _ = a == b


instance Ord MethodExtension where
  compare (MethodExtension a _) (MethodExtension b _) = compare a b


-- | Access or construct an extension through its interned symbol.
pattern NonStandard :: Symbol -> Method
pattern NonStandard symbol <- ExtensionMethod (MethodExtension _ symbol)
  where
    NonStandard symbol = fromSymbol symbol


{-# COMPLETE GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH, QUERY, NonStandard #-}


-- Preserve the public Generic shape, including the historical Symbol field.
-- Use local derived metadata to obtain the module and package identifiers.
type family MethodRep shape where
  MethodRep (G.M1 G.D ('G.MetaData _name modName pkg _isNewtype) _record) =
    G.D1 ('G.MetaData "Method" modName pkg 'False)
      ( ((Verb "GET" G.:+: Verb "HEAD") G.:+: (Verb "POST" G.:+: (Verb "PUT" G.:+: Verb "DELETE")))
          G.:+: ((Verb "CONNECT" G.:+: (Verb "OPTIONS" G.:+: Verb "TRACE")) G.:+: (Verb "PATCH" G.:+: (Verb "QUERY" G.:+: SymbolVerb)))
      )


type Verb name = G.C1 ('G.MetaCons name 'G.PrefixI 'False) G.U1


type SymbolVerb =
  G.C1
    ('G.MetaCons "NonStandard" 'G.PrefixI 'False)
    (G.S1 ('G.MetaSel 'Nothing 'G.NoSourceUnpackedness 'G.SourceStrict 'G.DecidedUnpack) (G.Rec0 Symbol))


instance Generic Method where
  type Rep Method = MethodRep (Rep MethodExtension)


  from method = G.M1 $ case method of
    GET -> G.L1 (G.L1 (G.L1 (G.M1 G.U1)))
    HEAD -> G.L1 (G.L1 (G.R1 (G.M1 G.U1)))
    POST -> G.L1 (G.R1 (G.L1 (G.M1 G.U1)))
    PUT -> G.L1 (G.R1 (G.R1 (G.L1 (G.M1 G.U1))))
    DELETE -> G.L1 (G.R1 (G.R1 (G.R1 (G.M1 G.U1))))
    CONNECT -> G.R1 (G.L1 (G.L1 (G.M1 G.U1)))
    OPTIONS -> G.R1 (G.L1 (G.R1 (G.L1 (G.M1 G.U1))))
    TRACE -> G.R1 (G.L1 (G.R1 (G.R1 (G.M1 G.U1))))
    PATCH -> G.R1 (G.R1 (G.L1 (G.M1 G.U1)))
    QUERY -> G.R1 (G.R1 (G.R1 (G.L1 (G.M1 G.U1))))
    NonStandard symbol -> G.R1 (G.R1 (G.R1 (G.R1 (G.M1 (G.M1 (G.K1 symbol))))))


  to (G.M1 representation) = case representation of
    G.L1 (G.L1 (G.L1 (G.M1 G.U1))) -> GET
    G.L1 (G.L1 (G.R1 (G.M1 G.U1))) -> HEAD
    G.L1 (G.R1 (G.L1 (G.M1 G.U1))) -> POST
    G.L1 (G.R1 (G.R1 (G.L1 (G.M1 G.U1)))) -> PUT
    G.L1 (G.R1 (G.R1 (G.R1 (G.M1 G.U1)))) -> DELETE
    G.R1 (G.L1 (G.L1 (G.M1 G.U1))) -> CONNECT
    G.R1 (G.L1 (G.R1 (G.L1 (G.M1 G.U1)))) -> OPTIONS
    G.R1 (G.L1 (G.R1 (G.R1 (G.M1 G.U1)))) -> TRACE
    G.R1 (G.R1 (G.L1 (G.M1 G.U1))) -> PATCH
    G.R1 (G.R1 (G.R1 (G.L1 (G.M1 G.U1)))) -> QUERY
    G.R1 (G.R1 (G.R1 (G.R1 (G.M1 (G.M1 (G.K1 symbol)))))) -> NonStandard symbol


instance NFData Method where
  rnf method = method `seq` ()
  {-# INLINE rnf #-}


instance Hashable Method where
  hashWithSalt salt (ExtensionMethod (MethodExtension bytes _)) = hashWithSalt (hashWithSalt salt (10 :: Int)) bytes
  hashWithSalt salt method = hashWithSalt salt (I# (dataToTag# method))
  {-# INLINE hashWithSalt #-}


instance Show Method where
  showsPrec p m = showsPrec p (methodToBytes m)


{- | @fromString@ preserves the token case verbatim (RFC 9110 § 9.1
methods are case-sensitive). Standard verbs use their dedicated constructors.
-}
instance IsString Method where
  fromString = methodFromBytes . BS8.pack


{- | Wire-stable 'Binary' instance: a method serialises as its on-the-wire
token bytes, independent of the constructor layout.
-}
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
  ExtensionMethod (MethodExtension bytes _) -> bytes


{- | Parse a method token. The standard verbs are recognised by
length-discriminated 'ByteString' equality (one comparison per
branch). Extension methods own a copy of their bytes. The historical
replacement of invalid UTF-8 is retained for this unchecked constructor.
-}
{-# INLINE methodFromBytes #-}
methodFromBytes :: ByteString -> Method
methodFromBytes bytes =
  withStandardMethod bytes id $
    if BS.isValidUtf8 bytes
      then extensionMethod bytes
      else fromSymbol (intern bytes)


-- | Parse one case-sensitive HTTP method token from a larger grammar.
methodTokenParser :: ParserT st e Method
methodTokenParser = fromToken <$> rfc9110TokenBS
  where
    fromToken bytes = withStandardMethod bytes id (extensionMethod bytes)
{-# INLINE methodTokenParser #-}


-- The grammar only calls this with an already validated ASCII token.
extensionMethod :: ByteString -> Method
extensionMethod bytes =
  let owned = BS.copy bytes
  in ExtensionMethod (MethodExtension owned (internUnsafe owned))
{-# INLINE extensionMethod #-}


fromSymbol :: Symbol -> Method
fromSymbol symbol = ExtensionMethod (MethodExtension (unintern @ByteString symbol) symbol)


-- Recognize standardized spellings before looking at an extension token.
withStandardMethod :: ByteString -> (Method -> a) -> a -> a
withStandardMethod bytes found extension = case BS.length bytes of
  3
    | bytes == "GET" -> found GET
    | bytes == "PUT" -> found PUT
    | otherwise -> extension
  4
    | bytes == "HEAD" -> found HEAD
    | bytes == "POST" -> found POST
    | otherwise -> extension
  5
    | bytes == "PATCH" -> found PATCH
    | bytes == "TRACE" -> found TRACE
    | bytes == "QUERY" -> found QUERY
    | otherwise -> extension
  6
    | bytes == "DELETE" -> found DELETE
    | otherwise -> extension
  7
    | bytes == "OPTIONS" -> found OPTIONS
    | bytes == "CONNECT" -> found CONNECT
    | otherwise -> extension
  _ -> extension
{-# INLINE withStandardMethod #-}


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
mkMethod bytes = withStandardMethod bytes Right (checkExtensionMethod bytes)
{-# INLINE mkMethod #-}


checkExtensionMethod :: ByteString -> Either MethodError Method
checkExtensionMethod bytes
  | BS.null bytes = Left MethodEmpty
  | otherwise = case BS.find (not . isMethodTokenByte) bytes of
      Nothing -> Right (extensionMethod bytes)
      Just invalid -> Left (MethodInvalidByte invalid)
{-# NOINLINE checkExtensionMethod #-}


-- Check the ASCII letter and digit ranges before consulting the shared set.
isMethodTokenByte :: Word8 -> Bool
isMethodTokenByte byte =
  (byte .|. 0x20) - 0x61 <= 25
    || byte - 0x30 <= 9
    || isTokenChar (BSI.w2c byte)
{-# INLINE isMethodTokenByte #-}


-- | Safe methods from the HTTP Method Registry (RFC 9110 §9.2.1).
{-# INLINE methodIsSafe #-}
methodIsSafe :: Method -> Bool
methodIsSafe = \case
  GET -> True
  HEAD -> True
  OPTIONS -> True
  TRACE -> True
  QUERY -> True
  ExtensionMethod (MethodExtension wire _) -> case wire of
    "PRI" -> True
    "PROPFIND" -> True
    "REPORT" -> True
    "SEARCH" -> True
    _ -> False
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

-- | Build an extension method from a literal token.
nonStandard :: ByteString -> Method
nonStandard = extensionMethod


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
