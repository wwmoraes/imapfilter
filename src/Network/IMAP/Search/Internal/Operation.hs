module Network.IMAP.Search.Internal.Operation (
  parseOperations,
  Operation,
) where

import Data.Time       (ZonedTime)
import Numeric.Natural (Natural)
import Text.Parsec     (ParseError)

import Network.HaskellNet.IMAP.Types (UID)

import Data.List                               (intercalate)
import Network.IMAP.Search.Internal.Parser     (parseSearch)
import Network.IMAP.Search.Internal.SearchExpr (SearchExpr (..), flattenUIDSet)

data Operation
  = All
  | Answered
  | Bcc String
  | Before ZonedTime
  | Body String
  | Cc String
  | Deleted
  | Draft
  | Flagged
  | From String
  | Header String String
  | Keyword String
  | Larger Natural
  | New
  | Not Operation
  | Old
  | On ZonedTime
  | Or Operation Operation
  | Recent
  | Seen
  | SentBefore ZonedTime
  | SentOn ZonedTime
  | SentSince ZonedTime
  | Since ZonedTime
  | Smaller Natural
  | Subject String
  | Text String
  | To String
  | UID [UID]
  | Unanswered
  | Undeleted
  | Undraft
  | Unflagged
  | Unkeyword String
  | Unseen

instance Show Operation where
  show All            = "ALL"
  show Answered       = "ANSWERED"
  show (Bcc s)        = "BCC " ++ s
  show (Before d)     = "BEFORE " ++ show d
  show (Body s)       = "BODY " ++ s
  show (Cc s)         = "CC " ++ show s
  show Deleted        = "DELETED"
  show Draft          = "DRAFT"
  show Flagged        = "FLAGGED"
  show (From s)       = "FROM " ++ s
  show (Header k v)   = "HEADER " ++ show k ++ show v
  show (Keyword s)    = "KEYWORD " ++ show s
  show (Larger n)     = "LARGER " ++ show n
  show New            = "NEW"
  show (Not o)        = "NOT " ++ show o
  show Old            = "OLD"
  show (On d)         = "ON " ++ show d
  show (Or a b)       = "OR " ++ show a ++ " " ++ show b
  show Recent         = "RECENT"
  show Seen           = "SEEN"
  show (SentBefore d) = "SENTBEFORE " ++ show d
  show (SentOn d)     = "SENTON " ++ show d
  show (SentSince d)  = "SENTSINCE " ++ show d
  show (Since d)      = "SINCE " ++ show d
  show (Smaller n)    = "SMALLER " ++ show n
  show (Subject s)    = "SUBJECT " ++ s
  show (Text s)       = "TEXT " ++ s
  show (To s)         = "TO " ++ s
  show (UID ds)       = "UID " ++ intercalate "," (map show ds)
  show Unanswered     = "UNANSWERED"
  show Undeleted      = "UNDELETED"
  show Undraft        = "UNDRAFT"
  show Unflagged      = "UNFLAGGED"
  show (Unkeyword s)  = "UNKEYWORD " ++ s
  show Unseen         = "UNSEEN"

instance SearchExpr Operation where
  all = All
  answered = Answered
  bcc = Bcc
  before = Before
  body = Body
  cc = Cc
  deleted = Deleted
  draft = Draft
  flagged = Flagged
  from = From
  header = Header
  keyword = Keyword
  larger = Larger
  new = New
  not = Not
  old = Old
  on = On
  or = Or
  recent = Recent
  seen = Seen
  sentBefore = SentBefore
  sentOn = SentOn
  sentSince = SentSince
  since = Since
  smaller = Smaller
  subject = Subject
  text = Text
  to = To
  uid = UID . flattenUIDSet
  unanswered = Unanswered
  undeleted = Undeleted
  undraft = Undraft
  unflagged = Unflagged
  unkeyword = Unkeyword
  unseen = Unseen

parseOperations :: String -> Either ParseError [Operation]
parseOperations = parseSearch
