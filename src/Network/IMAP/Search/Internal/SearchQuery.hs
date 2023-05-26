module Network.IMAP.Search.Internal.SearchQuery where

import Data.Time (ZonedTime)
-- import System.Time (CalendarTime)

import Network.HaskellNet.IMAP.Types (Flag (..), UID)

import Network.IMAP.Search.Internal.SearchExpr (SearchExpr (..), flattenUIDSet)

-- TODO SearchQuery: change CalendarTime to ZonedTime from Data.Time.LocalTime
data SearchQuery
  = ALLs
  | AND [SearchQuery]
  | FLAG Flag
  | UNFLAG Flag
  | BCCs String
  | BEFOREs ZonedTime
  | BODYs String
  | CCs String
  | FROMs String
  | HEADERs String String
  | LARGERs Integer
  | NEWs
  | NOTs SearchQuery
  | OLDs
  | ONs ZonedTime
  | ORs SearchQuery SearchQuery
  | SENTBEFOREs ZonedTime
  | SENTONs ZonedTime
  | SENTSINCEs ZonedTime
  | SINCEs ZonedTime
  | SMALLERs Integer
  | SUBJECTs String
  | TEXTs String
  | TOs String
  | XGMRAW String
  | UIDs [UID]

instance Semigroup SearchQuery where
  (AND a) <> (AND b) = AND (a ++ b)
  a <> (AND b)       = AND (a:b)
  (AND a) <> b       = AND (a ++ [b])
  a <> b             = AND [a,b]

instance Monoid SearchQuery where
  mempty = AND mempty

instance SearchExpr SearchQuery where
  all = ALLs
  answered = FLAG Answered
  bcc = BCCs
  before = BEFOREs
  body = BODYs
  cc = CCs
  deleted = FLAG Deleted
  draft = FLAG Draft
  flagged = FLAG Flagged
  from = FROMs
  header = HEADERs
  keyword "\\Answered" = FLAG Answered
  keyword "\\Deleted"  = FLAG Deleted
  keyword "\\Draft"    = FLAG Draft
  keyword "\\Flagged"  = FLAG Flagged
  keyword "\\Recent"   = FLAG Recent
  keyword "\\Seen"     = FLAG Seen
  keyword s            = FLAG (Keyword s)
  larger = LARGERs . toInteger
  new = NEWs
  not = NOTs
  old = OLDs
  on = ONs
  or = ORs
  recent = FLAG Recent
  seen = FLAG Seen
  sentBefore = SENTBEFOREs
  sentOn = SENTONs
  sentSince = SENTSINCEs
  since = SINCEs
  smaller = SMALLERs . toInteger
  subject = SUBJECTs
  text = TEXTs
  to = TOs
  uid = UIDs . flattenUIDSet
  unanswered = UNFLAG Answered
  undeleted = UNFLAG Deleted
  undraft = UNFLAG Draft
  unflagged = UNFLAG Flagged
  unkeyword "\\Answered" = UNFLAG Answered
  unkeyword "\\Deleted"  = UNFLAG Deleted
  unkeyword "\\Draft"    = UNFLAG Draft
  unkeyword "\\Flagged"  = UNFLAG Flagged
  unkeyword "\\Recent"   = UNFLAG Recent
  unkeyword "\\Seen"     = UNFLAG Seen
  unkeyword s            = NOTs . FLAG $ Keyword s
  unseen = UNFLAG Seen
