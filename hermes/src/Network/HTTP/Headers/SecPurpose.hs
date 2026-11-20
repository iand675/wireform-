{-# LANGUAGE PatternSynonyms #-}

{- |
The @Sec-Purpose@ Fetch Metadata request header states the purpose for which a
request was issued. Fetch defines the @prefetch@ token. The WICG prefetch
specification extends the field to a Structured Fields List, with
@prerender@ and @anonymous-client-ip@ parameters on @prefetch@ Items.
Members and parameters from extensions are retained for forwarding.

Spec: <https://wicg.github.io/nav-speculation/prefetch.html#sec-purpose-header>
(WICG Speculation Rules).

See also: "Network.HTTP.Headers.AcceptCH", "Network.HTTP.Headers.SaveData", "Network.HTTP.Headers.SecGPC", "Network.HTTP.Headers.EarlyData", "Network.HTTP.Headers.Priority".
-}
module Network.HTTP.Headers.SecPurpose (
  SecPurpose (SecPurpose, SecPurposeList, secPurposeValue, secPurposeMembers),
  PrefetchPurpose (..),
  secPurposePrefetches,
  secPurposeParser,
  renderSecPurpose,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecPurpose)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The ordered members of @Sec-Purpose@.
newtype SecPurpose = SecPurposeList {secPurposeMembers :: [StructuredFieldMember]}
  deriving stock (Eq, Show)


{- | The original interface for a single bare token. Use 'secPurposeMembers'
to read values containing parameters or multiple members.
-}
pattern SecPurpose :: RFC8941Token -> SecPurpose
pattern SecPurpose {secPurposeValue} = SecPurposeList [StructuredItem (StructuredFieldItem (Token secPurposeValue) [])]


data PrefetchPurpose = PrefetchPurpose
  { prefetchPrerender :: !Bool
  , prefetchAnonymousClientIP :: !Bool
  }
  deriving stock (Eq, Show)


-- | Project each @prefetch@ Item and its defined parameters in wire order.
secPurposePrefetches :: SecPurpose -> [PrefetchPurpose]
secPurposePrefetches = mapMaybe project . secPurposeMembers
  where
    project (StructuredItem (StructuredFieldItem (Token (RFC8941Token "prefetch")) parameters)) =
      Just (foldl' apply (PrefetchPurpose False False) parameters)
    project _ = Nothing

    apply flags (StructuredFieldParameter "prerender" value) = flags {prefetchPrerender = value /= Boolean False}
    apply flags (StructuredFieldParameter "anonymous-client-ip" value) = flags {prefetchAnonymousClientIP = value /= Boolean False}
    apply flags _ = flags


instance KnownHeader SecPurpose where
  type ParseFailure SecPurpose = String
  type Cardinality SecPurpose = 'ZeroOrOne
  type Direction SecPurpose = 'Request


  parseFromHeaders _ ("prefetch" :| []) = Right (SecPurpose (RFC8941Token "prefetch"))
  parseFromHeaders _ fields = parseStructuredFieldLines "Sec-Purpose" secPurposeParser fields


  renderToHeaders _ (SecPurpose (RFC8941Token "prefetch")) = "prefetch"
  renderToHeaders _ (SecPurpose (RFC8941Token token)) = ST.toByteString token
  renderToHeaders _ field = M.toStrictByteString (renderSecPurpose field)


  omitHeaderValue = null . secPurposeMembers
  {-# INLINE omitHeaderValue #-}


  headerName _ = hSecPurpose


secPurposeParser :: ParserT st String SecPurpose
secPurposeParser = SecPurposeList <$> (ows *> rfc8941List structuredFieldMemberParser <* ows)


renderSecPurpose :: SecPurpose -> M.Builder
renderSecPurpose = R.renderStructuredFieldList . secPurposeMembers
