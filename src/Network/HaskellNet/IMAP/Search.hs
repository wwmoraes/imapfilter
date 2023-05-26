{-# OPTIONS_GHC -Wno-orphans #-}

module Network.HaskellNet.IMAP.Search (
  parseSearchQuery
) where

import Network.IMAP.Search (SearchExpr (..), flattenUIDSet, parseSearch)
import Text.Parsec         (ParseError)

import Network.HaskellNet.IMAP (Flag (..), SearchQuery (..))
import Network.IMAP.Date       (toOldCalendarTime)

instance SearchExpr SearchQuery where
  all = ALLs
  answered = FLAG Answered
  bcc = BCCs
  before = BEFOREs . toOldCalendarTime
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
  on = ONs . toOldCalendarTime
  or = ORs
  recent = FLAG Recent
  seen = FLAG Seen
  sentBefore = SENTBEFOREs . toOldCalendarTime
  sentOn = SENTONs . toOldCalendarTime
  sentSince = SENTSINCEs . toOldCalendarTime
  since = SINCEs . toOldCalendarTime
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

parseSearchQuery :: String -> Either ParseError [SearchQuery]
parseSearchQuery = parseSearch
