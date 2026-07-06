{- | Shared test fixtures: the two IDL fixture schemas
(@test/fixtures/*.lattice@) parsed once, plus compile helpers used across
the suite.

The fixture files are normative-adjacent (they pin IDL surface decisions);
loading them through 'unsafePerformIO' is safe because their content is
fixed for the lifetime of a test run.
-}
module Test.Lattice.Fixtures (
  starwarsText,
  blogText,
  cokeyText,
  starwarsSchema,
  blogSchema,
  cokeySchema,
  cardText,
  cardSchema,
  mustParseSchema,
  compileWith,
  mustCompileWith,
  requireRight,
  feedPageText,
) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lattice.Canonical (Compiled, compileText)
import Lattice.IDL.Parser (parseSchema)
import Lattice.Query.Validate (CompileError)
import Lattice.Schema (Schema, defaultBudgets)
import System.IO.Unsafe (unsafePerformIO)
import Test.Syd (expectationFailure)


loadFixture :: FilePath -> Text
loadFixture p = unsafePerformIO (TE.decodeUtf8 <$> BS.readFile p)


starwarsText :: Text
starwarsText = loadFixture "test/fixtures/starwars.lattice"
{-# NOINLINE starwarsText #-}


blogText :: Text
blogText = loadFixture "test/fixtures/blog.lattice"
{-# NOINLINE blogText #-}


-- | Parse an IDL document, erroring loudly on failure (fixtures must parse).
mustParseSchema :: Text -> Schema
mustParseSchema src =
  either
    (\es -> error ("fixture schema failed to elaborate: " <> show es))
    id
    (parseSchema src)


starwarsSchema :: Schema
starwarsSchema = mustParseSchema starwarsText
{-# NOINLINE starwarsSchema #-}


blogSchema :: Schema
blogSchema = mustParseSchema blogText
{-# NOINLINE blogSchema #-}


cokeyText :: Text
cokeyText = loadFixture "test/fixtures/cokey.lattice"
{-# NOINLINE cokeyText #-}


-- | The §3.8 co-keyed fixture: @UserProfile joins User@, @AdminUser
-- refines User@, identity edges in both directions.
cokeySchema :: Schema
cokeySchema = mustParseSchema cokeyText
{-# NOINLINE cokeySchema #-}

{- | The §3.4–§3.6 cardinality fixture, inline on purpose: it exercises
every new cardinality surface — a required to-one (bare @has one@ =
exactly one), a declared-optional to-one (@has one?@ over an optional
link column), a floored bounded collection (@min 1 max 200@), and the
nonempty list type @[Text]+@ in field, newtype, and mutation-argument
position. Shared by the IDL, Plan, Query, and E2E suites.
-}
cardText :: Text
cardText =
  T.unlines
    [ "schema card.example.com"
    , ""
    , "newtype OrderId = Text"
    , "newtype CustomerId = Text"
    , "newtype ItemId = Text"
    , "newtype Tags = [Text]+"
    , ""
    , "entity Customer by id {"
    , "  visible to all by default"
    , ""
    , "  id:   CustomerId"
    , "  name: Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Order by id {"
    , "  visible to all by default"
    , ""
    , "  id:         OrderId"
    , "  customerId: CustomerId"
    , "  reviewerId: CustomerId?"
    , "  note:       Text"
    , "  tags:       [Text]+"
    , "  memo:       [Text]+?"
    , ""
    , "  has one customer: Customer by customerId"
    , "  has one? reviewer: Customer by reviewerId"
    , ""
    , "  has many lineItems: LineItem by orderId min 1 max 200"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity LineItem by id {"
    , "  visible to all by default"
    , ""
    , "  id:      ItemId"
    , "  orderId: OrderId"
    , "  sku:     Text"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "get order(id: OrderId) of Order public"
    , ""
    , "get orderTagged(tags: Tags) of Order public"
    , ""
    , "mutation tagOrder(order: OrderId, tags: [Text]+) returns Order {"
    , "  allow       public"
    , "  writes      Order(order)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "}"
    ]


cardSchema :: Schema
cardSchema = mustParseSchema cardText
{-# NOINLINE cardSchema #-}


-- | Compile query text under 'defaultBudgets'.
compileWith :: Schema -> Text -> Either CompileError Compiled
compileWith schema = compileText schema defaultBudgets


-- | Compile query text, failing the test with the compile error on Left.
mustCompileWith :: Schema -> Text -> IO Compiled
mustCompileWith schema = requireRight . compileWith schema


-- | Unwrap a Right, failing the test (with the error shown) on Left.
requireRight :: (Show e) => Either e a -> IO a
requireRight = either (expectationFailure . show) pure


{- | The spec §4.2 @FeedPage@ query, adapted to the self-contained blog
fixture (@Int@ alias spelled @I32@; both spellings canonicalize alike,
which "Test.Lattice.Canonical" asserts). Shared by the canonicalization,
plan, and compression tests.
-}
feedPageText :: Text
feedPageText =
  T.unlines
    [ "query FeedPage($after: Cursor, $limit: I32 = 20) {"
    , "  feed(after: $after, limit: $limit) {"
    , "    title"
    , "    publishedAt"
    , "    author { ...UserByline }"
    , "    comments(first: 3) {"
    , "      body"
    , "      createdAt"
    , "      author { ...UserByline }"
    , "    }"
    , "  }"
    , "}"
    ]
