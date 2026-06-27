{-# LANGUAGE ApplicativeDo #-}

{- | @regen-stats@ entry point.

Walks every @wireform-*/README.md@, finds AUTOGEN marker regions,
and rewrites the body of each from the data captured under
@dist-stats/@ (tests, coverage) and the per-package
@wireform-X/bench-results/summary/@ directory (benchmarks).

Subcommands:

* @regen-stats render@: read what's in tree, regenerate the
markdown. Fast (no cabal commands run).
* @regen-stats render-bench-charts@: re-render every @bench-results\/charts\/@
SVG from its summary JSON. Use after a palette / layout change.
* @regen-stats badges@: regenerate every shields.io endpoint
badge JSON under @badges/@. Fast.
* @regen-stats check@: run @render@ in dry-run mode and exit
non-zero if anything would change. The CI gate.
-}
module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (
  dropExtension,
  takeExtension,
  (</>),
 )
import Wireform.Stats.Bench qualified as Bench
import Wireform.Stats.Coverage qualified as Cov
import Wireform.Stats.Marker qualified as Mk
import Wireform.Stats.SVG qualified as SVG
import Wireform.Stats.Shields qualified as Shi
import Wireform.Stats.Table qualified as Tbl
import Wireform.Stats.Test qualified as Tst


-- ---------------------------------------------------------------------------
-- Command-line interface
-- ---------------------------------------------------------------------------

data Command
  = CmdRender !RenderOpts
  | CmdRenderBenchCharts !RenderOpts
  | CmdBadges !RenderOpts
  | CmdCheck !RenderOpts


data RenderOpts = RenderOpts
  { roRoot :: !FilePath
  -- ^ Repo root. Defaults to current working directory.
  , roStatsDir :: !FilePath
  -- ^ Where to look for @test-results/@ + @coverage/@.
  , roBadgesDir :: !FilePath
  -- ^ Where to write shields.io endpoint badge JSON.
  , roDocsDir :: !FilePath
  -- ^ Astro docs-site package-page directory (relative to the repo
  --   root). Each @wireform-\<name\>@ package maps to a
  --   @\<name\>.md@ page in here; the @bench:\<id\>@ AUTOGEN markers in
  --   those pages are rewritten with an inline, color-scheme-adaptive
  --   SVG chart so the docs site stays in lock-step with the READMEs.
  , roVerbose :: !Bool
  }


cli :: ParserInfo Command
cli =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Regenerate the AUTOGEN regions in every wireform-*/README.md."
        <> header "regen-stats - per-package README stats regenerator"
    )


commands :: Parser Command
commands =
  subparser
    ( command
        "render"
        ( info
            (CmdRender <$> renderOpts <**> helper)
            (progDesc "Rewrite README markers from in-tree data.")
        )
        <> command
          "render-bench-charts"
          ( info
              (CmdRenderBenchCharts <$> renderOpts <**> helper)
              (progDesc "Re-render every benchmark SVG from its summary JSON.")
          )
        <> command
          "badges"
          ( info
              (CmdBadges <$> renderOpts <**> helper)
              (progDesc "Regenerate the shields.io endpoint badge JSON files.")
          )
        <> command
          "check"
          ( info
              (CmdCheck <$> renderOpts <**> helper)
              (progDesc "Dry-run: exit non-zero if rendering would change anything.")
          )
    )


renderOpts :: Parser RenderOpts
renderOpts = do
  root <-
    strOption
      ( long "root"
          <> short 'C'
          <> metavar "DIR"
          <> value "."
          <> help "Repository root (default: current directory)."
      )
  stats <-
    strOption
      ( long "stats-dir"
          <> metavar "DIR"
          <> value "dist-stats"
          <> help "Where to read test-results/<pkg>.junit.xml and coverage/<pkg>.hpc.txt (default: dist-stats)."
      )
  badges <-
    strOption
      ( long "badges-dir"
          <> metavar "DIR"
          <> value "badges"
          <> help "Where to write shields.io endpoint badge JSON files (default: badges)."
      )
  docs <-
    strOption
      ( long "docs-dir"
          <> metavar "DIR"
          <> value "website/src/content/docs/packages"
          <> help "Astro docs-site package-page directory whose bench markers are rewritten (default: website/src/content/docs/packages)."
      )
  verbose <-
    switch
      ( long "verbose"
          <> short 'v'
          <> help "Print every marker key seen + its source."
      )
  pure (RenderOpts root stats badges docs verbose)


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser cli
  case cmd of
    CmdRender opts -> runRender opts
    CmdRenderBenchCharts opts -> runRenderBenchCharts opts
    CmdBadges opts -> runBadges opts
    CmdCheck opts -> runCheck opts


-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

runRender :: RenderOpts -> IO ()
runRender opts = do
  packages <- discoverPackages (roRoot opts)
  forM_ packages $ \pkg -> do
    let readme = roRoot opts </> pkg </> "README.md"
    haveReadme <- doesFileExist readme
    when haveReadme $ do
      reps <- buildReplacementsFor opts pkg
      changed <- Mk.rewriteFile readme reps
      when (roVerbose opts || changed) $
        putStrLn $
          pkg
            <> "/README.md: "
            <> if changed
              then "rewrote "
              else
                "no change ("
                  <> show (Map.size reps)
                  <> " markers)"
    -- Docs site: rewrite the bench markers on the matching package page.
    mDocs <- docsPageFor opts pkg
    forM_ mDocs $ \docsPath -> do
      dreps <- docsReplacementsFor opts pkg
      unless (Map.null dreps) $ do
        changed <- Mk.rewriteFile docsPath dreps
        when (roVerbose opts || changed) $
          putStrLn $
            docsPath
              <> ": "
              <> if changed
                then "rewrote"
                else "no change (" <> show (Map.size dreps) <> " bench markers)"


runCheck :: RenderOpts -> IO ()
runCheck opts = do
  packages <- discoverPackages (roRoot opts)
  readmeChanged <- or <$> mapM (checkOne opts) packages
  docsChanged <- or <$> mapM (checkDocs opts) packages
  if readmeChanged || docsChanged
    then do
      putStrLn "regen-stats check: README and/or docs markers are stale. Run `regen-stats render`."
      exitFailure
    else exitSuccess


checkOne :: RenderOpts -> FilePath -> IO Bool
checkOne opts pkg = do
  let readme = roRoot opts </> pkg </> "README.md"
  haveReadme <- doesFileExist readme
  if not haveReadme
    then pure False
    else do
      reps <- buildReplacementsFor opts pkg
      before <- TIO.readFile readme
      let after = Mk.rewriteMarkers reps before
      let changed = before /= after
      when (changed && roVerbose opts) $
        putStrLn $
          pkg <> "/README.md: stale (" <> show (Map.size reps) <> " markers)"
      pure changed


checkDocs :: RenderOpts -> FilePath -> IO Bool
checkDocs opts pkg = do
  mDocs <- docsPageFor opts pkg
  case mDocs of
    Nothing -> pure False
    Just docsPath -> do
      dreps <- docsReplacementsFor opts pkg
      if Map.null dreps
        then pure False
        else do
          before <- TIO.readFile docsPath
          let after = Mk.rewriteMarkers dreps before
          let changed = before /= after
          when (changed && roVerbose opts) $
            putStrLn $
              docsPath <> ": stale (" <> show (Map.size dreps) <> " bench markers)"
          pure changed


-- ---------------------------------------------------------------------------
-- render-bench-charts
-- ---------------------------------------------------------------------------

runRenderBenchCharts :: RenderOpts -> IO ()
runRenderBenchCharts opts = do
  packages <- discoverPackages (roRoot opts)
  forM_ packages $ \pkg -> do
    summaries <- listSummaries (roRoot opts) pkg
    forM_ summaries $ \summaryPath -> do
      r <- Bench.readSummary summaryPath
      case r of
        Left err -> putStrLn $ summaryPath <> ": " <> err
        Right summary -> do
          let chartsDir = roRoot opts </> pkg </> "bench-results" </> "charts"
              base = chartsDir </> T.unpack (Bench.bsId summary)
              chart = Bench.summaryToBarChart summary
          let (lightSvg, darkSvg) = SVG.renderBarChartBoth chart
          createDirectoryIfMissing True chartsDir
          BS.writeFile (base <> "-light.svg") lightSvg
          BS.writeFile (base <> "-dark.svg") darkSvg
          when (roVerbose opts) $
            putStrLn $
              pkg
                <> ": rendered "
                <> T.unpack (Bench.bsId summary)
                <> " ("
                <> show (BS.length lightSvg + BS.length darkSvg)
                <> " bytes)"


-- ---------------------------------------------------------------------------
-- badges
-- ---------------------------------------------------------------------------

runBadges :: RenderOpts -> IO ()
runBadges opts = do
  packages <- discoverPackages (roRoot opts)
  let badgesDir = roRoot opts </> roBadgesDir opts
  createDirectoryIfMissing True badgesDir
  forM_ packages $ \pkg -> do
    -- tests
    mTs <- loadTests opts pkg
    forM_ mTs $ \ts -> do
      let path = badgesDir </> (pkg <> "-tests.json")
      Shi.writeBadge path (Shi.testsBadge ts)
      when (roVerbose opts) $ putStrLn $ "wrote " <> path
    -- coverage
    mCv <- loadCoverage opts pkg
    forM_ mCv $ \cv -> do
      let path = badgesDir </> (pkg <> "-coverage.json")
      Shi.writeBadge path (Shi.coverageBadge cv)
      when (roVerbose opts) $ putStrLn $ "wrote " <> path


-- ---------------------------------------------------------------------------
-- Replacement assembly
-- ---------------------------------------------------------------------------

buildReplacementsFor :: RenderOpts -> FilePath -> IO (Map Mk.MarkerKey Mk.Replacement)
buildReplacementsFor opts pkg = do
  testsRep <- testsReplacement opts pkg
  covRep <- coverageReplacement opts pkg
  covTblRep <- coverageTableReplacement opts pkg
  benchReps <- benchReplacementsFor opts pkg
  pure $
    Map.fromList $
      concat
        [ maybeToList testsRep
        , maybeToList covRep
        , maybeToList covTblRep
        , benchReps
        ]


testsReplacement :: RenderOpts -> FilePath -> IO (Maybe (Mk.MarkerKey, Mk.Replacement))
testsReplacement opts pkg = do
  mTs <- loadTests opts pkg
  case mTs of
    Nothing -> pure (Just (key "tests", "_No data yet. Run `cabal test " <> T.pack pkg <> ":all --test-show-details=streaming --xml=dist-stats/test-results/" <> T.pack pkg <> ".junit.xml` to populate._"))
    Just ts -> pure (Just (key "tests", Tst.summaryToTestLine ts))


coverageReplacement :: RenderOpts -> FilePath -> IO (Maybe (Mk.MarkerKey, Mk.Replacement))
coverageReplacement opts pkg = do
  mCv <- loadCoverage opts pkg
  case mCv of
    Nothing -> pure (Just (key "coverage", "_No data yet. Run `cabal test " <> T.pack pkg <> ":all --enable-coverage` and capture `hpc report` output._"))
    Just cv -> pure (Just (key "coverage", Cov.summaryToCoverageLine cv))


coverageTableReplacement :: RenderOpts -> FilePath -> IO (Maybe (Mk.MarkerKey, Mk.Replacement))
coverageTableReplacement opts pkg = do
  mCv <- loadCoverage opts pkg
  case mCv of
    Nothing -> pure Nothing
    Just cv -> pure (Just (key "coverage:table", Tbl.renderTable (Cov.summaryToCoverageTable cv)))


benchReplacementsFor :: RenderOpts -> FilePath -> IO [(Mk.MarkerKey, Mk.Replacement)]
benchReplacementsFor opts pkg = do
  summaries <- listSummaries (roRoot opts) pkg
  concat <$> mapM (oneBench opts pkg) summaries


oneBench :: RenderOpts -> FilePath -> FilePath -> IO [(Mk.MarkerKey, Mk.Replacement)]
oneBench _opts pkg summaryPath = do
  r <- Bench.readSummary summaryPath
  case r of
    Left _ -> pure []
    Right summary ->
      let theId = Bench.bsId summary
          theKey = key ("bench:" <> theId)
          tableT = Tbl.renderTable (Bench.summaryToTable summary)
          chart =
            "<picture>\n"
              <> "  <source media=\"(prefers-color-scheme: dark)\" srcset=\""
              <> chartPath pkg theId "dark"
              <> "\">\n"
              <> "  <img src=\""
              <> chartPath pkg theId "light"
              <> "\" alt=\""
              <> Bench.bsTitle summary
              <> "\">\n"
              <> "</picture>"
          captionT =
            "<sub>Last run "
              <> T.pack (show (Bench.bsCapturedAt summary))
              <> ". "
              <> Bench.bsToolchain summary
              <> ".</sub>"
          replacement = chart <> "\n\n" <> tableT <> "\n" <> captionT
      in pure [(theKey, replacement)]


chartPath :: FilePath -> Text -> Text -> Text
chartPath _pkg theId variant =
  -- README-relative: charts live alongside the README under
  -- bench-results/charts/.
  "bench-results/charts/" <> theId <> "-" <> variant <> ".svg"


-- ---------------------------------------------------------------------------
-- Docs-site bench markers
-- ---------------------------------------------------------------------------

{- | The Astro docs-site page for a package, if it exists.

Packages live in @wireform-\<name\>/@ and map to a @\<name\>.md@
page under 'roDocsDir' (e.g. @wireform-cbor@ -> @cbor.md@). Returns
'Nothing' when there is no matching page so packages without a docs
stub (or with a non-default page name) are simply skipped.
-}
docsPageFor :: RenderOpts -> FilePath -> IO (Maybe FilePath)
docsPageFor opts pkg =
  case T.stripPrefix "wireform-" (T.pack pkg) of
    Nothing -> pure Nothing
    Just shortName -> do
      let path = roRoot opts </> roDocsDir opts </> (T.unpack shortName <> ".md")
      ok <- doesFileExist path
      pure (if ok then Just path else Nothing)


{- | Bench replacements for the docs page of a package, keyed by
@bench:\<id\>@. Unlike 'benchReplacementsFor' (which references the
two committed README SVG files via a @\<picture\>@), the docs body
inlines a single color-scheme-adaptive SVG so the chart renders in a
plain-markdown Starlight page with no asset path or @base@ wiring.
-}
docsReplacementsFor :: RenderOpts -> FilePath -> IO (Map Mk.MarkerKey Mk.Replacement)
docsReplacementsFor opts pkg = do
  summaries <- listSummaries (roRoot opts) pkg
  Map.fromList . concat <$> mapM oneDocsBench summaries


oneDocsBench :: FilePath -> IO [(Mk.MarkerKey, Mk.Replacement)]
oneDocsBench summaryPath = do
  r <- Bench.readSummary summaryPath
  case r of
    Left _ -> pure []
    Right summary ->
      let theId = Bench.bsId summary
          theKey = key ("bench:" <> theId)
          tableT = Tbl.renderTable (Bench.summaryToTable summary)
          svgT = TE.decodeUtf8 (SVG.renderBarChartAdaptive (Bench.summaryToBarChart summary))
          captionT =
            "<sub>Last run "
              <> T.pack (show (Bench.bsCapturedAt summary))
              <> ". "
              <> Bench.bsToolchain summary
              <> ".</sub>"
          replacement = svgT <> "\n\n" <> tableT <> "\n" <> captionT
      in pure [(theKey, replacement)]


-- ---------------------------------------------------------------------------
-- Loaders
-- ---------------------------------------------------------------------------

loadTests :: RenderOpts -> FilePath -> IO (Maybe Tst.TestSummary)
loadTests opts pkg = do
  let path = roRoot opts </> roStatsDir opts </> "test-results" </> (pkg <> ".junit.xml")
  ok <- doesFileExist path
  if not ok
    then pure Nothing
    else do
      r <- Tst.readJUnit path
      case r of
        Right ts -> pure (Just ts)
        Left _ -> pure Nothing


loadCoverage :: RenderOpts -> FilePath -> IO (Maybe Cov.CoverageSummary)
loadCoverage opts pkg = do
  let path = roRoot opts </> roStatsDir opts </> "coverage" </> (pkg <> ".hpc.txt")
  ok <- doesFileExist path
  if not ok
    then pure Nothing
    else Just <$> Cov.readHpcReport path


listSummaries :: FilePath -> FilePath -> IO [FilePath]
listSummaries root pkg = do
  let dir = root </> pkg </> "bench-results" </> "summary"
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      entries <- listDirectory dir
      pure
        [ dir </> e
        | e <- entries
        , takeExtension e == ".json"
        , not (null (dropExtension e))
        ]


discoverPackages :: FilePath -> IO [FilePath]
discoverPackages root = do
  entries <- listDirectory root
  let candidates = filter ("wireform-" `isPrefix`) entries
  filter (/= "") <$> mapM checkOne' candidates
  where
    checkOne' name = do
      isDir <- doesDirectoryExist (root </> name)
      pure (if isDir then name else "")
    isPrefix p s = take (length p) s == p


key :: Text -> Mk.MarkerKey
key t = case Mk.markerKey t of
  Right k -> k
  Left err -> error ("Wireform.Stats.Main.key: invalid hard-coded key " <> T.unpack t <> " (" <> err <> ")")
