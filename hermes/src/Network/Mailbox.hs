{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
RFC 5322 §3.4 email addresses — @addr-spec@, @mailbox@, @address@ and
the @mailbox-list@ \/ @address-list@ aggregates.

This accepts common, non-obsolete forms of the grammar in a single field
value. Comments and space or tab are accepted at @CFWS@ positions. HTTP
field values carry the unfolded spelling.

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

Domain names use letter, digit, and hyphen labels. Comments are discarded
and omitted when rendering.

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
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
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


atextSet :: AsciiSet
atextSet = asciiSet atextCharSet
{-# NOINLINE atextSet #-}


ldhSet :: AsciiSet
ldhSet = asciiSet ldhCharSet
{-# NOINLINE ldhSet #-}


dtextChar :: ParserT st String ()
dtextChar = satisfyAscii_ (`CharSet.member` dtextCharSet)


isAtext :: Char -> Bool
isAtext c = c `CharSet.member` atextCharSet


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | @1*atext *(\".\" 1*atext)@, captured verbatim (dots included).
dotAtomText :: ParserT st String ST.ShortText
dotAtomText = dotAtomWith atextSet


-- | Domain @dot-atom@ over let-dig-hyphen labels.
domainDotAtom :: ParserT st String ST.ShortText
domainDotAtom = dotAtomWith ldhSet


dotAtomWith :: AsciiSet -> ParserT st String ST.ShortText
dotAtomWith chars = shortASCIIFromParser_ (atom *> skipMany ($(char '.') *> atom))
  where
    atom = asciiAlnumSetBytes chars
{-# INLINE dotAtomWith #-}


-- | @\"[\" *dtext \"]\"@; the result holds the text inside the brackets.
domainLiteral :: ParserT st String Domain
domainLiteral = $(char '[') *> (DomainLiteral <$> (bare <|> spaced))
  where
    bare = shortASCIIFromParser_ (skipMany dtextChar) <* $(char ']')
    spaced =
      (STU.fromByteStringUnsafe . B.filter (\w -> w /= 9 && w /= 32)
        <$> byteStringOf (ows *> skipMany (dtextChar *> ows))) <* $(char ']')


-- | @local-part = dot-atom \/ quoted-string@ (decoded).
localPartParser :: ParserT st String ST.ShortText
localPartParser = dotAtomText <|> mailboxQuotedString


-- | @domain = dot-atom \/ domain-literal@.
domainParser :: ParserT st String Domain
domainParser = (DomainName <$> domainDotAtom) <|> domainLiteral


-- | A single @word@: an @atom@ or a @quoted-string@ (decoded).
word :: ParserT st String ST.ShortText
word = atom <|> mailboxQuotedString
  where
    atom = shortASCIIFromParser_ (asciiAlnumSetBytes atextSet)


mailboxQuotedString :: ParserT st String ST.ShortText
mailboxQuotedString = shortTextFromLatin1 <$> httpQuotedBytes


-- | Comment and whitespace positions on an already unfolded field line.
cfws :: ParserT st String ()
cfws = ows *> skipMany (skipComment *> ows)
{-# INLINE cfws #-}


-- | @phrase@: one or more 'word's, joined by single ASCII spaces.
displayName :: ParserT st String ST.ShortText
displayName = do
  first <- word
  rest <- many (cfws *> word)
  case rest of
    [] -> pure first
    _ -> pure (ST.fromText (T.intercalate " " (map ST.toText (first : rest))))


-- | The separator between list members: @[OWS] \",\" [OWS]@.
listSeparator :: ParserT st String ()
listSeparator = cfws *> $(char ',') *> cfws


-- | @addr-spec@ without leading 'ows' (composed under 'mailboxCore').
addrSpecCore :: ParserT st String AddrSpec
addrSpecCore = do
  lp <- localPartParser
  cfws
  $(char '@')
  cfws
  d <- domainParser <* cfws
  pure AddrSpec {localPart = lp, domain = d}


-- | @mailbox@ without leading 'ows'.
mailboxCore :: ParserT st String Mailbox
mailboxCore = bareAddr <|> nameAddr
  where
    nameAddr = do
      mName <- optional displayName
      cfws
      $(char '<')
      cfws
      a <- addrSpecCore
      $(char '>')
      cfws
      pure Mailbox {mailboxName = mName, mailboxAddr = a}
    bareAddr = Mailbox Nothing <$> addrSpecCore


-- | @group@ without leading 'ows'.
groupCore :: ParserT st String Address
groupCore = do
  name <- displayName
  cfws
  $(char ':')
  cfws
  members <- mailboxCore `sepBy` listSeparator
  cfws
  $(char ';')
  cfws
  pure AddressGroup {groupName = name, groupMembers = members}


-- | @address@ without leading 'ows'.
addressCore :: ParserT st String Address
addressCore = groupCore <|> (AddressMailbox <$> mailboxCore)


-- | Parse a bare @addr-spec@ (allowing surrounding 'ows').
addrSpecParser :: ParserT st String AddrSpec
addrSpecParser = cfws *> addrSpecCore


-- | Parse a @mailbox@ (allowing surrounding 'ows').
mailboxParser :: ParserT st String Mailbox
mailboxParser = cfws *> mailboxCore


-- | Parse an @address@ — a 'Mailbox' or a @group@ (allowing surrounding 'ows').
addressParser :: ParserT st String Address
addressParser = cfws *> addressCore


-- | Parse a non-empty @mailbox-list@.
mailboxListParser :: ParserT st String [Mailbox]
mailboxListParser = cfws *> (mailboxCore `sepBy1` listSeparator)


-- | Parse a non-empty @address-list@.
addressListParser :: ParserT st String [Address]
addressListParser = cfws *> (addressCore `sepBy1` listSeparator)


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
  M.char7 '"' <> content <> M.char7 '"'
  where
    bytes = ST.toShortByteString t
    content
      | SBS.any (\w -> w == 34 || w == 92 || w >= 128) bytes =
          T.foldr (\c rest -> escape c <> rest) mempty (ST.toText t)
      | otherwise = M.shortByteString bytes
    escape '"' = M.char7 '\\' <> M.char7 '"'
    escape '\\' = M.char7 '\\' <> M.char7 '\\'
    escape c = M.char8 c


{- | True iff @t@ is a valid @dot-atom-text@: dot-separated, non-empty
  @atext@ labels (no leading\/trailing\/doubled dots).
-}
isDotAtomText :: ST.ShortText -> Bool
isDotAtomText = separatedAtoms 46


-- | True iff @t@ is a space-separated @phrase@ of @atom@s.
isPhraseOfAtoms :: ST.ShortText -> Bool
isPhraseOfAtoms = separatedAtoms 32


separatedAtoms :: Word8 -> ST.ShortText -> Bool
separatedAtoms separator text = scan 0 False
  where
    bytes = ST.toShortByteString text
    size = SBS.length bytes
    scan !offset !inAtom
      | offset == size = inAtom
      | byte == separator = inAtom && scan (offset + 1) False
      | otherwise = isAtext (toEnum (fromIntegral byte)) && scan (offset + 1) True
      where
        byte = SBSI.unsafeIndex bytes offset
{-# INLINE separatedAtoms #-}


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
