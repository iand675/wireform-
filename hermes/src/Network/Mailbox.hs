{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
RFC 5322 §3.4 email addresses — @addr-spec@, @mailbox@, @address@ and
the @mailbox-list@ \/ @address-list@ aggregates.

This is a faithful but /practical/ subset of the grammar restricted to
the common, non-obsolete forms. The @obs-@ productions are ignored, and
folding white space \/ comments (@CFWS@) are treated as simple ASCII
@OWS@ (space \/ tab) rather than the full RFC 5234 folding machinery.

== Grammar (the subset we accept)

@
addr-spec       = local-part \"\@\" domain
local-part      = dot-atom \/ quoted-string
domain          = dot-atom \/ domain-literal
dot-atom        = 1*atext *(\".\" 1*atext)
atext           = ALPHA \/ DIGIT \/ \"!#$%&'*+-/=?^_`{|}~\"
domain-literal  = \"[\" *dtext \"]\"
dtext           = %d33-90 \/ %d94-126
mailbox         = name-addr \/ addr-spec
name-addr       = [display-name] \"<\" addr-spec \">\"
display-name    = phrase
phrase          = 1*word                 ; words joined by single SP
word            = atom \/ quoted-string
group           = display-name \":\" [mailbox-list] \";\"
mailbox-list    = mailbox *(\",\" mailbox)
address         = mailbox \/ group
address-list    = address *(\",\" address)
@

Surrounding 'ows' is tolerated everywhere a @CFWS@ would be permitted.

<https://www.rfc-editor.org/rfc/rfc5322#section-3.4>
-}
module Network.Mailbox (
  -- * Types
  AddrSpec (..),
  Domain (..),
  Mailbox (..),
  Address (..),

  -- * Parsers
  addrSpecParser,
  mailboxParser,
  addressParser,
  mailboxListParser,
  addressListParser,

  -- * Renderers
  renderAddrSpec,
  renderMailbox,
  renderAddress,
  renderMailboxList,
  renderAddressList,

  -- * Convenience runners
  parseAddrSpec,
  parseMailbox,
  parseAddress,
  parseMailboxList,
  parseAddressList,
) where

import Control.Monad.Combinators (sepBy, sepBy1)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import Network.HTTP.Grammar (WireGrammar (..))


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | @addr-spec@: a bare @local-part \"\@\" domain@ email address.
data AddrSpec = AddrSpec
  { localPart :: !ST.ShortText
  -- ^ The decoded local part (quoting removed). Render quotes it back
  --   if it is not a valid @dot-atom@.
  , domain :: !Domain
  }
  deriving stock (Eq, Show)


-- | The domain half of an 'AddrSpec'.
data Domain
  = -- | A @dot-atom@ host name, e.g. @example.com@.
    DomainName !ST.ShortText
  | -- | A @domain-literal@: the text /inside/ the @[ ]@, e.g.
    --   @192.0.2.1@ for @[192.0.2.1]@.
    DomainLiteral !ST.ShortText
  deriving stock (Eq, Show)


-- | @mailbox@: an optional display name plus an 'AddrSpec'.
data Mailbox = Mailbox
  { mailboxName :: !(Maybe ST.ShortText)
  -- ^ The decoded display name, if any (quoting removed).
  , mailboxAddr :: !AddrSpec
  }
  deriving stock (Eq, Show)


-- | @address@: either a single 'Mailbox' or a named @group@.
data Address
  = AddressMailbox !Mailbox
  | AddressGroup
      { groupName :: !ST.ShortText
      , groupMembers :: ![Mailbox]
      }
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Character classes
-- ---------------------------------------------------------------------------

-- | @atext = ALPHA \/ DIGIT \/ \"!#$%&'*+-/=?^_`{|}~\"@.
atextCharSet :: CharSet
atextCharSet =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> "!#$%&'*+-/=?^_`{|}~"


-- | Domain @dot-atom@ label characters: let-dig and hyphen.
ldhCharSet :: CharSet
ldhCharSet =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> CharSet.singleton '-'


-- | @dtext = %d33-90 \/ %d94-126@ (printable ASCII minus @[@, @\\@, @]@).
dtextCharSet :: CharSet
dtextCharSet = CharSet.range '\x21' '\x5A' <> CharSet.range '\x5E' '\x7E'


atextChar :: ParserT st String Char
atextChar = satisfyAscii (`CharSet.member` atextCharSet)


ldhChar :: ParserT st String Char
ldhChar = satisfyAscii (`CharSet.member` ldhCharSet)


dtextChar :: ParserT st String Char
dtextChar = satisfyAscii (`CharSet.member` dtextCharSet)


isAtext :: Char -> Bool
isAtext c = c `CharSet.member` atextCharSet


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | @1*atext *(\".\" 1*atext)@, captured verbatim (dots included).
dotAtomText :: ParserT st String ST.ShortText
dotAtomText = shortASCIIFromParser_ $ do
  skipSome atextChar
  skipMany ($(char '.') *> skipSome atextChar)


-- | Domain @dot-atom@ over let-dig-hyphen labels.
domainDotAtom :: ParserT st String ST.ShortText
domainDotAtom = shortASCIIFromParser_ $ do
  skipSome ldhChar
  skipMany ($(char '.') *> skipSome ldhChar)


-- | @\"[\" *dtext \"]\"@; the result holds the text inside the brackets.
domainLiteral :: ParserT st String Domain
domainLiteral = do
  $(char '[')
  inner <- shortASCIIFromParser_ (skipMany dtextChar)
  $(char ']')
  pure (DomainLiteral inner)


-- | @local-part = dot-atom \/ quoted-string@ (decoded).
localPartParser :: ParserT st String ST.ShortText
localPartParser = dotAtomText <|> quotedString


-- | @domain = dot-atom \/ domain-literal@.
domainParser :: ParserT st String Domain
domainParser = (DomainName <$> domainDotAtom) <|> domainLiteral


-- | A single @word@: an @atom@ or a @quoted-string@ (decoded).
word :: ParserT st String ST.ShortText
word = atom <|> quotedString
  where
    atom = shortASCIIFromParser_ (skipSome atextChar)


-- | @phrase@: one or more 'word's, joined by single ASCII spaces.
displayName :: ParserT st String ST.ShortText
displayName = do
  ws <- word `sepBy1` rws
  pure (ST.fromString (unwords (map ST.toString ws)))


-- | The separator between list members: @[OWS] \",\" [OWS]@.
listSeparator :: ParserT st String ()
listSeparator = ows *> $(char ',') *> ows


-- | @addr-spec@ without leading 'ows' (composed under 'mailboxCore').
addrSpecCore :: ParserT st String AddrSpec
addrSpecCore = do
  lp <- localPartParser
  ows
  $(char '@')
  ows
  d <- domainParser
  pure AddrSpec {localPart = lp, domain = d}


-- | @mailbox@ without leading 'ows'.
mailboxCore :: ParserT st String Mailbox
mailboxCore = nameAddr <|> bareAddr
  where
    nameAddr = do
      mName <- optional displayName
      ows
      $(char '<')
      ows
      a <- addrSpecCore
      ows
      $(char '>')
      pure Mailbox {mailboxName = mName, mailboxAddr = a}
    bareAddr = Mailbox Nothing <$> addrSpecCore


-- | @group@ without leading 'ows'.
groupCore :: ParserT st String Address
groupCore = do
  name <- displayName
  ows
  $(char ':')
  ows
  members <- mailboxCore `sepBy` listSeparator
  ows
  $(char ';')
  pure AddressGroup {groupName = name, groupMembers = members}


-- | @address@ without leading 'ows'.
addressCore :: ParserT st String Address
addressCore = groupCore <|> (AddressMailbox <$> mailboxCore)


-- | Parse a bare @addr-spec@ (allowing surrounding 'ows').
addrSpecParser :: ParserT st String AddrSpec
addrSpecParser = ows *> addrSpecCore


-- | Parse a @mailbox@ (allowing surrounding 'ows').
mailboxParser :: ParserT st String Mailbox
mailboxParser = ows *> mailboxCore


-- | Parse an @address@ — a 'Mailbox' or a @group@ (allowing surrounding 'ows').
addressParser :: ParserT st String Address
addressParser = ows *> addressCore


-- | Parse a non-empty @mailbox-list@.
mailboxListParser :: ParserT st String [Mailbox]
mailboxListParser = ows *> (mailboxCore `sepBy1` listSeparator)


-- | Parse a non-empty @address-list@.
addressListParser :: ParserT st String [Address]
addressListParser = ows *> (addressCore `sepBy1` listSeparator)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAddrSpec :: AddrSpec -> M.Builder
renderAddrSpec (AddrSpec lp d) =
  renderLocalPart lp <> M.char7 '@' <> renderDomain d


renderDomain :: Domain -> M.Builder
renderDomain (DomainName t) = R.shortText t
renderDomain (DomainLiteral t) = M.char7 '[' <> R.shortText t <> M.char7 ']'


renderMailbox :: Mailbox -> M.Builder
renderMailbox (Mailbox Nothing a) = renderAddrSpec a
renderMailbox (Mailbox (Just name) a) =
  renderDisplayName name
    <> M.char7 ' '
    <> M.char7 '<'
    <> renderAddrSpec a
    <> M.char7 '>'


renderAddress :: Address -> M.Builder
renderAddress (AddressMailbox m) = renderMailbox m
renderAddress (AddressGroup name members) =
  renderDisplayName name
    <> M.char7 ':'
    <> (if null members then mempty else M.char7 ' ' <> renderMailboxList members)
    <> M.char7 ';'


renderMailboxList :: [Mailbox] -> M.Builder
renderMailboxList = M.intersperse ", " . map renderMailbox


renderAddressList :: [Address] -> M.Builder
renderAddressList = M.intersperse ", " . map renderAddress


{- | Render bare when the local part is a valid @dot-atom@, else as a
  @quoted-string@.
-}
renderLocalPart :: ST.ShortText -> M.Builder
renderLocalPart lp
  | isDotAtomText lp = R.shortText lp
  | otherwise = renderQuoted lp


{- | Render bare when the display name is a @phrase@ of @atom@s, else as a
  single @quoted-string@.
-}
renderDisplayName :: ST.ShortText -> M.Builder
renderDisplayName name
  | isPhraseOfAtoms name = R.shortText name
  | otherwise = renderQuoted name


-- | Emit a @quoted-string@, escaping @"@ and @\\@ with @quoted-pair@.
renderQuoted :: ST.ShortText -> M.Builder
renderQuoted t =
  M.char7 '"' <> foldMap escape (ST.toString t) <> M.char7 '"'
  where
    escape '"' = M.char7 '\\' <> M.char7 '"'
    escape '\\' = M.char7 '\\' <> M.char7 '\\'
    escape c = M.char8 c


{- | True iff @t@ is a valid @dot-atom-text@: dot-separated, non-empty
  @atext@ labels (no leading\/trailing\/doubled dots).
-}
isDotAtomText :: ST.ShortText -> Bool
isDotAtomText t =
  all (\l -> not (null l) && all isAtext l) (splitOn '.' (ST.toString t))


-- | True iff @t@ is a space-separated @phrase@ of @atom@s.
isPhraseOfAtoms :: ST.ShortText -> Bool
isPhraseOfAtoms t =
  all (\w -> not (null w) && all isAtext w) (splitOn ' ' (ST.toString t))


splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest


-- ---------------------------------------------------------------------------
-- WireGrammar instances
-- ---------------------------------------------------------------------------

-- | The 'WireGrammar' surface uses the strict 'Network.HTTP.Grammar.parseGrammar'
-- runner (no trailing-OWS tolerance). The OWS-tolerant 'parseAddrSpec' &
-- friends below remain the permissive entry points for header-style input.
instance WireGrammar AddrSpec where
  type GrammarErr AddrSpec = String
  grammarParser = addrSpecParser
  grammarRender = renderAddrSpec


instance WireGrammar Mailbox where
  type GrammarErr Mailbox = String
  grammarParser = mailboxParser
  grammarRender = renderMailbox


instance WireGrammar Address where
  type GrammarErr Address = String
  grammarParser = addressParser
  grammarRender = renderAddress


-- ---------------------------------------------------------------------------
-- Convenience runners (require full consumption modulo trailing OWS)
-- ---------------------------------------------------------------------------

parseAddrSpec :: B.ByteString -> Either String AddrSpec
parseAddrSpec = runFully addrSpecParser "addr-spec"


parseMailbox :: B.ByteString -> Either String Mailbox
parseMailbox = runFully mailboxParser "mailbox"


parseAddress :: B.ByteString -> Either String Address
parseAddress = runFully addressParser "address"


parseMailboxList :: B.ByteString -> Either String [Mailbox]
parseMailboxList = runFully mailboxListParser "mailbox-list"


parseAddressList :: B.ByteString -> Either String [Address]
parseAddressList = runFully addressListParser "address-list"


runFully :: ParserT st String a -> String -> B.ByteString -> Either String a
runFully p what bs = case runParser p bs of
  OK v rest
    | B.null (B.dropWhile isOwsByte rest) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing " <> what <> ": " <> show rest)
  Fail -> Left ("Failed to parse " <> what)
  Err e -> Left e
  where
    isOwsByte w = w == 0x20 || w == 0x09
