{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- \$(switch [| case _ of
--     \"if\"   -> pure TIf
--     \"else\" -> pure TElse
--     _      -> identifier |])
-- @

{- | Efficient literal branching using Template Haskell.

Compiles string-literal case branches into a trie of primitive byte
comparisons with grouped bounds checks.

@
-}
module Wireform.Parser.Switch (
  switch,
  switchWithPost,

  -- * TH literal splices
  char,
  string,

  -- * Helpers used by generated code (not for direct use)
  switchFailed,
  switchBranch,
  switchAnyWord8Unsafe,
  switchPeekWord8Unsafe,
  switchSkip1,
) where

import Control.Monad (forM)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Word (Word8)
import GHC.Exts
import GHC.Word (Word8 (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import Wireform.Parser (byteString, word8)
import Wireform.Parser.Internal


------------------------------------------------------------------------
-- Helpers called by generated code
------------------------------------------------------------------------

-- | Fail without consuming input.
switchFailed :: Parser m e a
switchFailed = Parser \_ _ _ st -> (# st, Fail# #)
{-# INLINE switchFailed #-}


-- | Unsafe read — caller must have ensured at least 1 byte.
switchAnyWord8Unsafe :: Parser m e Word8
switchAnyWord8Unsafe = Parser \_ _ s st ->
  case indexWord8OffAddr# s 0# of
    w# -> (# st, OK# (W8# w#) (plusAddr# s 1#) #)
{-# INLINE switchAnyWord8Unsafe #-}


{- | Peek the next byte without consuming. Used by the trie code
generator so that a wildcard-branch falling back to the
current node's terminal action does not eat a byte that
belongs to whatever runs after the switch.

(Previously the generator used 'switchAnyWord8Unsafe' for the
byte dispatch, which consumed the byte and then ran the
terminal action /past/ that byte. For input like @\"1,…\"@ on a
switch with literals @\"1\", \"1.0\", \"1.00\"@, that meant the
@\"1\"@ terminal action ran from after the comma — silently
breaking every quality-weighted Accept-* parser whose first
entry used @q=1@.)
-}
switchPeekWord8Unsafe :: Parser m e Word8
switchPeekWord8Unsafe = Parser \_env _eob s st ->
  case indexWord8OffAddr# s 0# of
    w# -> (# st, OK# (W8# w#) s #)
{-# INLINE switchPeekWord8Unsafe #-}


{- | Skip one byte (used after 'switchPeekWord8Unsafe' has
decided a child branch matches).
-}
switchSkip1 :: Parser m e ()
switchSkip1 = Parser \_env _eob s st ->
  (# st, OK# () (plusAddr# s 1#) #)
{-# INLINE switchSkip1 #-}


-- | Branch on an ensure check: if enough bytes, run @t@; else @f@.
switchBranch :: Int -> Parser m e a -> Parser m e a -> Parser m e a
switchBranch (I# n#) (Parser t) (Parser f) = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> t env eob s st
    _ -> f env eob s st
{-# INLINE switchBranch #-}


------------------------------------------------------------------------
-- Trie
------------------------------------------------------------------------

type Rule = Maybe Int


data Trie a = Branch !a !(Map Word8 (Trie a))


nilTrie :: Trie Rule
nilTrie = Branch Nothing mempty


insertTrie :: Int -> [Word8] -> Trie Rule -> Trie Rule
insertTrie rule = go
  where
    go [] (Branch r ts) = Branch (Just $ maybe rule (min rule) r) ts
    go (c : cs) (Branch r ts) =
      Branch r (M.alter (Just . maybe (go cs nilTrie) (go cs)) c ts)


listToTrie :: [(Int, String)] -> Trie Rule
listToTrie = foldl' (\t (!r, !s) -> insertTrie r (charToBytes s) t) nilTrie


charToBytes :: String -> [Word8]
charToBytes = concatMap go
  where
    go c
      | n < 0x80 = [fromIntegral n]
      | n < 0x800 =
          [ fromIntegral (0xC0 + n `div` 64)
          , fromIntegral (0x80 + n `mod` 64)
          ]
      | otherwise = [fromIntegral n] -- simplified; ASCII-only for v1
      where
        n = ord c


mindepths :: Trie Rule -> Trie (Rule, Int)
mindepths (Branch rule ts)
  | M.null ts = Branch (rule, 0) mempty
  | otherwise =
      let ts' = M.map mindepths ts
          d =
            minimum $
              M.elems $
                M.map
                  ( \(Branch (r, depth) _) ->
                      maybe (depth + 1) (const 1) r
                  )
                  ts'
      in Branch (rule, d) ts'

------------------------------------------------------------------------
-- TH code generation
------------------------------------------------------------------------

mkDoE :: [Stmt] -> Exp
mkDoE = DoE Nothing


switch :: Q Exp -> Q Exp
switch = switchWithPost Nothing


switchWithPost :: Maybe (Q Exp) -> Q Exp -> Q Exp
switchWithPost postAction qexp = do
  post <- sequence postAction
  (cases, deflt) <- parseSwitchExp qexp
  genSwitch post cases deflt


parseSwitchExp :: Q Exp -> Q ([(String, Exp)], Maybe Exp)
parseSwitchExp qexp =
  qexp >>= \case
    CaseE (UnboundVarE _) [] -> fail "switch: empty case list"
    CaseE (UnboundVarE _) matches -> do
      let (ini, lst) = (init matches, last matches)
      cases <- forM ini \case
        Match (LitP (StringL s)) (NormalB rhs) [] -> pure (s, rhs)
        _ -> fail "switch: expected string literal pattern"
      (cases', deflt) <- case lst of
        Match (LitP (StringL s)) (NormalB rhs) [] -> pure (cases <> [(s, rhs)], Nothing)
        Match WildP (NormalB rhs) [] -> pure (cases, Just rhs)
        _ -> fail "switch: expected string literal or wildcard pattern"
      pure (cases', deflt)
    _ -> fail "switch: expected 'case _ of' expression"


genSwitch :: Maybe Exp -> [(String, Exp)] -> Maybe Exp -> Q Exp
genSwitch post cases deflt = do
  let indexed = zip [0 :: Int ..] cases
      trie = mindepths (listToTrie [(i, s) | (i, (s, _)) <- indexed])

  let ruleMap =
        M.fromList $
          (Nothing, VarE 'switchFailed)
            : [(Just i, applyPost post rhs) | (i, (_, rhs)) <- indexed]
      defltExp = maybe (VarE 'switchFailed) id deflt

  bindings <- forM (M.toList (M.insert Nothing defltExp ruleMap)) \(k, e) -> do
    n <- newName ("r" <> maybe "D" show k)
    pure (k, n, e)

  let nameMap = M.fromList [(k, n) | (k, n, _) <- bindings]

  body <- genTrieCode nameMap trie
  letE
    [valD (varP n) (normalB (pure e)) [] | (_, n, e) <- bindings]
    (pure body)


genTrieCode :: Map (Maybe Int) Name -> Trie (Rule, Int) -> Q Exp
genTrieCode names = go 0
  where
    ruleName r = case M.lookup r names of
      Just n -> n
      Nothing -> error "switch: missing rule"

    go :: Int -> Trie (Rule, Int) -> Q Exp
    go ensured (Branch (rule, depth) ts)
      | M.null ts = pure (VarE (ruleName rule))
      | otherwise = do
          let need = ensured < 1
          branches <- forM (M.toList ts) \(w, sub) -> do
            e <- go (if need then depth - 1 else ensured - 1) sub
            pure (w, InfixE (Just (VarE 'switchSkip1)) (VarE '(>>)) (Just e))

          let defE = VarE (ruleName rule)
              body =
                mkDoE
                  [ BindS (VarP (mkName "c")) (VarE 'switchPeekWord8Unsafe)
                  , NoBindS
                      ( CaseE
                          (VarE (mkName "c"))
                          ( [ Match (LitP (IntegerL (fromIntegral w))) (NormalB e) []
                            | (w, e) <- branches
                            ]
                              <> [Match WildP (NormalB defE) []]
                          )
                      )
                  ]

          if need
            then [|switchBranch depth $(pure body) $(pure defE)|]
            else pure body


applyPost :: Maybe Exp -> Exp -> Exp
applyPost Nothing rhs = rhs
applyPost (Just p) rhs = InfixE (Just p) (VarE '(>>)) (Just rhs)


------------------------------------------------------------------------
-- TH literal splices (flatparse-compatible)
------------------------------------------------------------------------

{- | @$(char \'x\')@ compiles to @word8 0xNN@ for ASCII characters,
or the appropriate multi-byte UTF-8 sequence otherwise.
Only ASCII is supported for now.
-}
char :: Char -> Q Exp
char c
  | n < 0x80 = [|word8 $(lift (fromIntegral n :: Word8))|]
  | otherwise = error ("Wireform.Parser.Switch.char: non-ASCII character: " <> show c)
  where
    n = ord c


-- | @$(string \"foo\")@ compiles to @byteString \"foo\"@.
string :: String -> Q Exp
string s = [|byteString $(lift (BSC.pack s))|]
