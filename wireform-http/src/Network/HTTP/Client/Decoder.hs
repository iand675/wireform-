{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Response decoders.

A 'ResponseDecoder' pairs an @Accept@ header (the set of media types
the caller is willing to handle, with @q=@ weights) with a function
that decodes the response body once the server has picked one of
them.

'as' is the most common constructor: given a content-type tag,
build a decoder that asks for exactly that media type and decodes
via the tag's 'Decode' instance. Decoders compose via '<!>' from
@semigroupoids@; for callers that don't want to depend on
@semigroupoids@ we also expose a plain @orDecoder@.
-}
module Network.HTTP.Client.Decoder (
  ResponseDecoder (..),
  as,
  orDecoder,
  statusAware,
  mapDecoder,
  bytesDecoder,
  asEither,
  withErrorStatus,

  -- * Re-exports for the @\<!\>@ operator
  Alt (..),
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Functor.Alt (Alt (..))
import Network.HTTP.Client.Media
import Network.HTTP.Internal.Validation (equalShortBytes)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as S


-- | Content negotiation + body decoding bundled together.
data ResponseDecoder a = ResponseDecoder
  { acceptable :: ![(MediaType, Quality)]
  {- ^ Media types this decoder will accept, in preference order.
  Used to fill in the @Accept@ header.
  -}
  , decodeBody :: !(S.Status -> MediaType -> ByteString -> Either DecodeError a)
  {- ^ Decode the body. The status code is available for
  status-aware decoders (e.g. dual error\/success bodies); the
  media type is the parsed @Content-Type@ of the response.
  -}
  }


instance Functor ResponseDecoder where
  fmap f d = d {decodeBody = \s m b -> f <$> decodeBody d s m b}


{- | The @\<!\>@ operator the spec calls for: combines two decoders so
their accept lists union and dispatch goes to whichever side
claims the response 'Content-Type'. Implemented in terms of
'orDecoder', which is the type-class-free version.
-}
instance Alt ResponseDecoder where
  (<!>) = orDecoder


{- | The standard smart constructor: accept the tag's media type with
quality 1.0 and decode the body via the tag's 'Decode' instance.
-}
as :: forall tag a. Decode tag a => ResponseDecoder a
as =
  ResponseDecoder
    { acceptable = [(mediaType @tag, maxQuality)]
    , decodeBody = \_status _ct bs -> decode @tag bs
    }


{- | Decoder combinator. The first decoder's accept list is preferred;
use it when one of its media ranges matches the response's
@Content-Type@, including all parameters required by the range.
Charset values are case-insensitive. Allow an omitted charset when
the range expects UTF-8 for JSON or plain text, as used by the
shipped decoders. Otherwise delegate to the second decoder.
-}
orDecoder :: ResponseDecoder a -> ResponseDecoder a -> ResponseDecoder a
orDecoder a b =
  ResponseDecoder
    { acceptable = acceptable a <> acceptable b
    , decodeBody = choose (acceptable a)
    }
  where
    choose [(MediaType wantedType wantedSubtype [], _)]
      | wantedType /= "*"
      , wantedSubtype /= "*" =
          \status ct@(MediaType actualType actualSubtype _) bs ->
            if equalShortBytes actualType wantedType && equalShortBytes actualSubtype wantedSubtype
              then decodeBody a status ct bs
              else decodeBody b status ct bs
    choose ranges =
      \status ct bs ->
        if any (matchesResponseRange ct . fst) ranges
          then decodeBody a status ct bs
          else decodeBody b status ct bs


matchesResponseRange :: MediaType -> MediaType -> Bool
matchesResponseRange actual@(MediaType base subtype supplied) range =
  matches actual range && all matchesParam (mtParameters range)
  where
    matchesParam (name, expected) =
      case lookupParameter name supplied of
        (# value | #) | equalShortBytes expected value -> True
        result ->
          sameMediaToken name "charset" && case result of
            (# value | #) -> foldedMediaTokenEqual expected value
            (# | (# #) #) ->
              sameMediaToken expected "utf-8"
                && ((base == "application" && subtype == "json") || (base == "text" && subtype == "plain"))
{-# INLINE matchesResponseRange #-}


lookupParameter :: ByteString -> [(ByteString, ByteString)] -> (# ByteString | (# #) #)
lookupParameter name = go
  where
    go [] = (# | (# #) #)
    go ((key, value) : rest)
      | sameMediaToken name key = (# value | #)
      | otherwise = go rest
{-# INLINE lookupParameter #-}


sameMediaToken :: ByteString -> ByteString -> Bool
sameMediaToken a b = equalShortBytes a b || foldedMediaTokenEqual a b
{-# INLINE sameMediaToken #-}


foldedMediaTokenEqual :: ByteString -> ByteString -> Bool
foldedMediaTokenEqual a b = size == BS.length b && loop 0
  where
    size = BS.length a
    loop i
      | i == size = True
      | x == y || (folded == (y .|. 32) && 97 <= folded && folded <= 122) = loop (i + 1)
      | otherwise = False
      where
        x = BSU.unsafeIndex a i
        y = BSU.unsafeIndex b i
        folded = x .|. 32
{-# INLINE foldedMediaTokenEqual #-}


{- | Replace the body-decoder while keeping the same accept list. The
new decoder sees the parsed status.
-}
statusAware
  :: ResponseDecoder a
  -> (Status -> MediaType -> ByteString -> Either DecodeError b)
  -> ResponseDecoder b
statusAware d k = d {decodeBody = k}


mapDecoder :: (a -> b) -> ResponseDecoder a -> ResponseDecoder b
mapDecoder = fmap


-- | Decoder that accepts anything and returns the raw bytes.
bytesDecoder :: ResponseDecoder ByteString
bytesDecoder =
  ResponseDecoder
    { acceptable = [("*/*", maxQuality)]
    , decodeBody = \_ _ -> Right
    }


{- | Dispatch on status: dispatch to the success decoder for 2xx
responses, otherwise to the error decoder. The accept list is the
union of both. Useful for APIs that return one shape on success
and another (also-JSON, usually) on failure.
-}
asEither
  :: forall tag e tag' a
   . (Decode tag e, Decode tag' a)
  => ResponseDecoder (Either e a)
asEither =
  ResponseDecoder
    { acceptable =
        [ (mediaType @tag, maxQuality)
        , (mediaType @tag', maxQuality)
        ]
    , decodeBody = \status _ct bs ->
        let code = S.statusCode status
        in if code >= 200 && code < 300
             then Right <$> decode @tag' bs
             else Left <$> decode @tag bs
    }


{- | Treat a particular status code as an error: 'decodeBody' returns
@Left e@ when the response status matches the supplied predicate,
otherwise delegates to the wrapped decoder. The wrapped decoder
still controls 'acceptable'.
-}
withErrorStatus
  :: forall tagE e a
   . Decode tagE e
  => (S.Status -> Bool)
  -> ResponseDecoder a
  -> ResponseDecoder (Either e a)
withErrorStatus isErr inner =
  ResponseDecoder
    { acceptable = (mediaType @tagE, maxQuality) : acceptable inner
    , decodeBody = \status ct bs ->
        if isErr status
          then Left <$> decode @tagE bs
          else Right <$> decodeBody inner status ct bs
    }
