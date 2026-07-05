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
  starwarsSchema,
  blogSchema,
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
