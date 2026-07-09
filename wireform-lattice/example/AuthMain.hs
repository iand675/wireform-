{- | An auth-slices demo: one Lattice query, three visibility slices
(@pub@ / @ctx@ / @priv@) that compose into a single view.

Run it:

@
cabal run example-auth
# then open http://127.0.0.1:8916/ in a browser
@

Configuration:

* @LATTICE_PORT@ — listen port (default 8916).

What it shows (spec §6.6, §8, §13.2)
------------------------------------

The same query — a board of an org's posts, each with its author — is
fetched at all three data slices. Emission is /slice-exact/ (spec §9.1,
'Lattice.Server.Execute.emitsAt'): each slice carries only the fields
whose visibility level /is/ that slice, so the three responses are
__disjoint field sets__ the client merges by @(id, ver)@ (§3.5.1):

* __pub__ — public fields (@title@, author @name@). Shared-cacheable,
  long TTL, one cache entry for everyone.
* __ctx__ — claim-gated fields: author @email@ (@visible when
  caller.org = orgId@) and post @views@ (@visible when caller.role =
  Admin@). Requires the @vc@ claims payload + an @X-Vc-Auth@ proof;
  shared-cacheable keyed by the claims.
* __priv__ — the per-viewer field @bookmarked@ (@private@). Requires an
  @Authorization@ header; @private@ cache-control, @Vary: Authorization@.

Because the gates are predicates over claims, the ctx slice reveals
different fields to different callers with the /same/ cache key
partition: an Acme admin sees @email@ (org matches) and @views@ (admin);
an Acme member sees @email@ but not @views@; a Globex admin sees @views@
but not @email@ (org mismatch). Anonymous callers get pub only.

The proofs are real: the origin runs with an HMAC 'ProofVerifier'
('ocVerifier'), so a ctx request without a valid @X-Vc-Auth@ is rejected
@401@. The browser never holds the secret — it calls the demo
/auth service/ endpoint @GET \/auth\/login?persona=…@, which mints the
@vc@ payload, the @X-Vc-Auth@ proof, and a bearer token exactly as a
deployment's auth service would (spec §8.2; 'hmacProof' is the mint).
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (IOException, try)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lattice.Backend.Memory (MemoryDb, defaultHooks, memoryBackend, newMemoryDb, putRow)
import Lattice.IDL.Parser (parseSchema)
import Lattice.Schema (Schema, defaultBudgets)
import Lattice.Server (OriginConfig (..), defaultLiveConfig, latticeHandler, newOrigin)
import Lattice.Server.Auth (QueryAdmission (..), encodeClaims, hmacProof, hmacVerifier)
import Lattice.Telemetry (noTelemetry)
import Lattice.Types (ClaimName (..), FieldName, TypeName)
import Network.HTTP.Message (Request (..), Response (..))
import Network.HTTP.Server (ServerConfig (..), defaultServerConfig, runServer)
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Types.Method (Method (..))
import Network.HTTP.Types.Status (pattern Status)
import Network.HTTP.Types.Version (pattern HTTP1_1)
import Network.HTTP.VersionRange (preferHttp1)
import AuthPage (embeddedPage)
import Control.Concurrent.STM (atomically)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stdout)


-- | The shared secret the demo auth service signs proofs with and the
-- origin verifies them against (spec §8.2). In production this lives only
-- in the auth service and the origin, never the client.
secret :: ByteString
secret = "acme-demo-visibility-secret"


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port <- fromMaybe "8916" <$> lookupEnv "LATTICE_PORT"
  page <- loadPage
  schema <- loadSchema
  db <- newBlogDb
  origin <- newOrigin (mkConfig schema db)
  let handler = authMiddleware page (latticeHandler origin)
      site = "http://127.0.0.1:" <> port
  putStrLn ("example-auth: auth-slices origin listening on " <> site)
  putStrLn ("  open " <> site <> "/ in a browser to compose pub/ctx/priv slices")
  runServer
    defaultServerConfig
      { serverHost = "0.0.0.0"
      , serverPort = port
      , serverVersionRange = preferHttp1
      , serverHandler = handler
      }


mkConfig :: Schema -> MemoryDb -> OriginConfig
mkConfig schema db =
  OriginConfig
    { ocSchema = schema
    , ocBudgets = defaultBudgets
    , ocBackend = memoryBackend schema db defaultHooks
    , -- The point of the demo: ctx proofs are actually verified.
      ocVerifier = Just (hmacVerifier secret getPOSIXTime)
    , ocSnapshotDomain = "main"
    , ocPurge = \_ -> pure ()
    , ocCors = True
    , ocNow = getPOSIXTime
    , ocAdmission = AdmitOpen
    , ocCoalesce = Nothing
    , ocRegistry = Nothing
    , ocLive = defaultLiveConfig
    , ocTelemetry = noTelemetry
    }


-- ---------------------------------------------------------------------------
-- Middleware: the page and the demo auth service
-- ---------------------------------------------------------------------------

{- | Serve the browser page at @GET \/@ and mint credentials at
@GET \/auth\/login?persona=…@; everything else is the Lattice handler.
-}
authMiddleware :: ByteString -> (Request -> IO Response) -> Request -> IO Response
authMiddleware page inner req =
  case (requestMethod req, path) of
    (GET, "/") -> pure (htmlResponse page)
    (GET, "/auth/login") -> loginResponse (lookupParam "persona" query)
    _ -> inner req
  where
    (path, query) = breakQuery (requestTarget req)


{- | The demo auth service. A persona maps to a claims payload; the mint
returns the @vc@ payload, its @X-Vc-Auth@ proof, and a bearer token — the
three credentials the browser attaches to ctx and priv requests. An
unknown or absent persona is anonymous (pub only).
-}
loginResponse :: Maybe Text -> IO Response
loginResponse mp = do
  now <- getPOSIXTime
  let persona = fromMaybe "anon" mp
  case personaClaims persona of
    Nothing ->
      pure . jsonNoStore $
        A.object ["persona" A..= persona, "anonymous" A..= True]
    Just (org, role) -> do
      let expSecs = floor now + 3600 :: Integer
          claims = Map.fromList [(ClaimName "org", A.String org), (ClaimName "role", A.String role)]
          vc = encodeClaims claims
          proof = hmacProof secret vc expSecs
          token = org <> ":" <> role
      pure . jsonNoStore $
        A.object
          [ "persona" A..= persona
          , "org" A..= org
          , "role" A..= role
          , "vc" A..= vc
          , "proof" A..= proof
          , "token" A..= token
          , "exp" A..= expSecs
          ]


-- | @(org, role)@ claims per persona; 'Nothing' is anonymous.
personaClaims :: Text -> Maybe (Text, Text)
personaClaims = \case
  "member" -> Just ("acme", "Member")
  "admin" -> Just ("acme", "Admin")
  "outsider" -> Just ("globex", "Admin")
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

loadSchema :: IO Schema
loadSchema = case parseSchema schemaSource of
  Right s -> pure s
  Left errs -> fail ("auth-demo schema failed to parse: " <> show errs)


schemaSource :: Text
schemaSource =
  T.unlines
    [ "\"An auth-slices demo: one query, three visibility slices that compose.\""
    , "schema acme.example.com"
    , ""
    , "newtype OrgId  = Text"
    , "newtype UserId = Text"
    , "newtype PostId = Text"
    , ""
    , "enum Role closed = Member | Editor | Admin"
    , ""
    , "claims {"
    , "  org:  OrgId"
    , "  role: Role"
    , "}"
    , ""
    , "\"A person. Public except for their email, which only same-org callers see.\""
    , "entity User by id {"
    , "  visible to all by default"
    , ""
    , "  id:    UserId"
    , "  orgId: OrgId"
    , "  name:  Text"
    , "  email: Text   visible when caller.org = orgId"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "\"A blog post: title public, view count admin-only, bookmark flag private.\""
    , "entity Post by id {"
    , "  visible to all by default"
    , ""
    , "  id:         PostId"
    , "  orgId:      OrgId"
    , "  authorId:   UserId"
    , "  title:      Text"
    , "  createdAt:  Timestamp"
    , "  views:      W32    visible when caller.role = Admin"
    , "  bookmarked: Bool?  private"
    , ""
    , "  has one author: User by authorId"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "\"Every post in an org, newest first.\""
    , "list posts of Post by orgId"
    , "     ordered by createdAt desc"
    , "     page 20 max 50"
    , "     public"
    ]


-- ---------------------------------------------------------------------------
-- Seed data: the Acme org's board
-- ---------------------------------------------------------------------------

newBlogDb :: IO MemoryDb
newBlogDb = do
  db <- newMemoryDb
  atomically $ do
    mapM_ (\(k, f) -> putRow db userType k f) users
    mapM_ (\(k, f) -> putRow db postType k f) posts
  pure db


userType :: TypeName
userType = "User"


postType :: TypeName
postType = "Post"


users :: [(Text, Map FieldName A.Value)]
users =
  [ user "u-ann" "acme" "Ann Reyes" "ann@acme.example"
  , user "u-ben" "acme" "Ben Cho" "ben@acme.example"
  ]
  where
    user uid org name email =
      ( uid
      , Map.fromList
          [ ("id", A.String uid)
          , ("orgId", A.String org)
          , ("name", A.String name)
          , ("email", A.String email)
          ]
      )


posts :: [(Text, Map FieldName A.Value)]
posts =
  [ post "p-1" "u-ann" "Shipping the cache layer" "2026-07-01T09:00:00Z" 1287 True
  , post "p-2" "u-ben" "Postmortem: the great purge" "2026-07-03T14:30:00Z" 942 False
  , post "p-3" "u-ann" "Roadmap for Q3" "2026-07-05T08:15:00Z" 2011 True
  ]
  where
    post pid author title createdAt views bookmarked =
      ( pid
      , Map.fromList
          [ ("id", A.String pid)
          , ("orgId", A.String "acme")
          , ("authorId", A.String author)
          , ("title", A.String title)
          , ("createdAt", A.String createdAt)
          , ("views", A.toJSON (views :: Int))
          , ("bookmarked", A.Bool bookmarked)
          ]
      )


-- ---------------------------------------------------------------------------
-- HTTP helpers
-- ---------------------------------------------------------------------------

{- | Prefer a live @auth.html@ on disk (so edits show up without a
rebuild), falling back to the compiled-in copy.
-}
loadPage :: IO ByteString
loadPage = go ["example/auth.html", "wireform-lattice/example/auth.html"]
  where
    go [] = pure embeddedPage
    go (p : ps) =
      try (BS8.readFile p) >>= \case
        Right bs -> pure bs
        Left (_ :: IOException) -> go ps


breakQuery :: ByteString -> (ByteString, ByteString)
breakQuery t = case BS8.break (== '?') t of
  (p, q) -> (p, BS8.drop 1 q)


-- | Look up a query-string parameter (values here are ASCII; no decode).
lookupParam :: ByteString -> ByteString -> Maybe Text
lookupParam k q =
  fmap (TE.decodeUtf8 . BS8.drop (BS8.length k + 1))
    . lookupFirst
    $ BS8.split '&' q
  where
    prefix = k <> "="
    lookupFirst [] = Nothing
    lookupFirst (kv : rest)
      | prefix `BS8.isPrefixOf` kv = Just kv
      | otherwise = lookupFirst rest


htmlResponse :: ByteString -> Response
htmlResponse body =
  Response
    { responseStatus = Status 200
    , responseVersion = HTTP1_1
    , responseHeaders =
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Cache-Control", "no-store")
        ]
    , responseBody = BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


jsonNoStore :: A.Value -> Response
jsonNoStore v =
  Response
    { responseStatus = Status 200
    , responseVersion = HTTP1_1
    , responseHeaders =
        [ ("Content-Type", "application/json")
        , ("Cache-Control", "no-store")
        , ("Access-Control-Allow-Origin", "*")
        ]
    , responseBody = BodyBytes (BL.toStrict (A.encode v))
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }
