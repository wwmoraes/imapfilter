module Network.IMAP.Search.Internal.Parser (
  parseSearch
) where

import Control.Monad (void)
import Data.Functor  (($>))

import Text.Parsec

import Data.Time (TimeLocale, ZonedTime, defaultTimeLocale, parseTimeOrError)

import           Network.IMAP.Search.Internal.SearchExpr
import qualified Network.IMAP.Search.Internal.SearchExpr as Search (SearchExpr (..))

type Parser a = Parsec String () a

parseSearch :: SearchExpr q => String -> Either ParseError [q]
parseSearch = parse query ""

query :: SearchExpr q => Parser [q]
query = spaces *> sepEndBy operation (spaces <|> void endOfLine)

operation :: SearchExpr q => Parser q
operation = spaces *> choice operations <|> (text <$> literal)
  where
    operations :: SearchExpr q => [Parser q]
    operations =
      [ Search.all <$ static "in:all" -- for completeness sake :P
      , answered <$ static "is:answered"
      , bcc <$> prefixed "bcc:" address
      , before <$> prefixed "before:" calendarTime
      , body <$> prefixed "body:" literal
      , cc <$> prefixed "cc:" address
      , deleted <$ static "is:deleted"
      , draft <$ static "is:draft"
      , flagged <$ static "is:flagged"
      , from <$> prefixed "from:" address
      , header <$> prefixed "header:" fieldName <*> literal
      , keyword <$> prefixed "keyword:" literal
      , larger <$> prefixed "larger:" digits
      , new <$ static "is:new"
      , old <$ static "is:old"
      , on <$> prefixed "on:" calendarTime
      , braces (Search.or <$> operation <*> operation)
      -- TODO parenthesized or with keyword
      , recent <$ static "is:recent"
      , seen <$ static "is:seen"
      , sentBefore <$> prefixed "sent_before:" calendarTime
      , sentOn <$> prefixed "sent_on:" calendarTime
      , sentSince <$> prefixed "sent_since:" calendarTime
      , since <$> prefixed "since:" calendarTime
      , smaller <$> prefixed "smaller:" digits
      , subject <$> prefixed "subject:" literal
      , to <$> prefixed "to:" address
      -- TODO uid
      , unanswered <$ static "is:unanswered"
      , unanswered <$ static "-is:answered"
      , undeleted <$ static "is:undeleted"
      , undeleted <$ static "-is:deleted"
      , undraft <$ static "is:undraft"
      , undraft <$ static "-is:draft"
      , unflagged <$ static "is:unflagged"
      , unflagged <$ static "-is:flagged"
      , unkeyword <$> prefixed "unkeyword:" literal
      , unkeyword <$> prefixed "-keyword:" literal
      , unseen <$ static "is:unseen"
      , unseen <$ static "-is:seen"

      -- NOT must be last as there's static parsers prefixed with minus as well
      , Search.not <$> prefixed "-" operation
      , Search.not . text <$> prefixed "-" literal

      -- Gmail operators compatibility
      , Search.all <$ (static "in:anywhere" *> parserFail "IMAP search works only within a mailbox")
      , seen <$ static "is:read"
      , unseen <$ static "is:unread"
      , since <$> prefixed "after:" calendarTime
      , before <$> prefixed "before:" calendarTime
      , before <$> prefixed "older:" calendarTime
      , since <$> prefixed "newer:" calendarTime
      , larger <$> prefixed "size:" digits
      , text <$> prefixed "+" literal
      ]

braces :: Parser p -> Parser p
braces = between (spaces *> char '{') (spaces *> char '}')

prefixed :: String -> Parser p -> Parser p
prefixed a b = try (string a) *> b

-- it is not our objective to fully validate email addresses
address :: Parser String
address = many1 (alphaNum <|> oneOf ".@-+_")

static :: String -> Parser ()
static p = try (string p) $> ()

digits :: (Read a, Num a) => Parser a
digits = read <$> many1 digit

-- FIX parse returns UTC instead of TZ-aware time
calendarTime :: Parser ZonedTime
calendarTime = choice
  [ parsecZonedTime defaultTimeLocale "%Y/%m/%d"
  , parsecZonedTime defaultTimeLocale "%Y/%m/%d"
  ]

parsecZonedTime :: TimeLocale -> String -> Parser ZonedTime
parsecZonedTime t s = try (parseTimeOrError False t s <$> many1 (digit <|> char '/'))

-- oldCalendarTime :: Parser OST.CalendarTime
-- oldCalendarTime = choice
--   [ OSTP.parsecCalendarTime OSL.defaultTimeLocale "%Y/%m/%d"
--   , OSTP.parsecCalendarTime OSL.defaultTimeLocale "%m/%d/%Y" -- BEWARE: this is mostly to support Gmail
--   ]

literal :: Parser String
literal = doubleQuoted <|> singleQuoted <|> parenthesized <|> word
  where
    word = many1 (noneOf " (){}\r\n")
    doubleQuoted = between (char '"') (char '"') (many (noneOf "\"\r\n"))
    singleQuoted = between (char '\'') (char '\'') (many (noneOf "'\r\n"))
    parenthesized = between (char '(') (char ')') (many (noneOf ")\r\n"))

fieldName :: Parser String
fieldName = manyTill anyChar (try $ char '=')

-- RFC 3501 - IMAP 4rev1
-- [ ] <sequence set>
-- [x] all
-- [x] answered
-- [x] bcc
-- [x] before
-- [x] body
-- [x] cc
-- [x] deleted
-- [x] draft
-- [x] flagged
-- [ ] or
--   [x] {}
--   [ ] OR in-between
-- [x] from (plain)
-- [x] header
-- [x] keyword
-- [x] larger
-- [x] new
-- [x] not
-- [x] old
-- [x] on
-- [x] recent
-- [x] seen
-- [x] sent before
-- [x] sent on
-- [x] sent since
-- [x] since
-- [x] smaller
-- [x] subject
-- [x] text
-- [x] to
-- [ ] uid <sequence set>
-- [x] unanswered
-- [x] undeleted
-- [x] undraft
-- [x] unflagged
-- [x] unkeyword
-- [x] unseen

-- RFC 9051 - IMAP 4rev2
-- https://datatracker.ietf.org/doc/html/rfc9051
-- [ ] SAVE support

-- Gmail search operators https://support.google.com/mail/answer/7190?hl=en
-- they can be used also with the custom X-GM-RAW search attribute
-- [ ] larger/smaller/size
--   [x] plain bytes
--   [ ] with units
-- [ ] OR/{}/inline e.g. from:(foo@quux.com OR bar@quux.com)
-- [x] - (negate)
-- [ ] AROUND
-- [x] has (<custom flag>)
-- [ ] list
-- [ ] filename
-- [x] " " (exact search)
-- [x] is:unread
-- [x] is:read
-- [x] after/newer/before/older
-- [x] + (exact word)
-- [ ] older_than/newer_than
-- [ ] deliveredto
-- [ ] Rfc822msgid
-- [ ] category (maybe general if using openXchange categories?)
-- [x] size

-- Gmail IMAP extensions https://developers.google.com/gmail/imap/imap-extensions
-- TODO parse the capability X-GM-EXT-1
-- [ ] has:drive
-- [ ] has:document
-- [ ] has:spreadsheet
-- [ ] has:presentation
-- [ ] has:youtube
-- [ ] has:attachment
-- [ ] has:nouserlabels
-- [ ] has:userlabels
-- [ ] is:important
-- [ ] is:snoozed
-- [ ] is:starred
-- [ ] label
-- [x] in:anywhere (nope, IMAP works within a mailbox)
