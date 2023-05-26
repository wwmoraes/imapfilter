module Network.IMAP.Search.Internal.SearchExpr (
  SearchExpr(..),
  UIDSet(..),
  flattenUIDSet,
) where

import Data.Time       (ZonedTime)
import Numeric.Natural (Natural)

import Network.HaskellNet.IMAP.Types (UID)

data UIDSet = Single UID | Range (UID, UID)

flattenUIDSet :: [UIDSet] -> [UID]
flattenUIDSet []                  = []
flattenUIDSet ((Single a):as)     = a : flattenUIDSet as
flattenUIDSet ((Range (a, b)):as) = [a..b] ++ flattenUIDSet as

-- TODO SearchExpr: change CalendarTime to ZonedTime from Data.Time.LocalTime
-- TODO SearchExpr: AND operation + monoid instance
class SearchExpr a where
  all :: a
  answered :: a
  bcc :: String -> a
  before :: ZonedTime -> a
  body :: String -> a
  cc :: String -> a
  deleted :: a
  draft :: a
  flagged :: a
  from :: String -> a
  header :: String -> String -> a
  keyword :: String -> a
  larger :: Natural -> a
  new :: a
  not :: a -> a
  old :: a
  on :: ZonedTime -> a
  or :: a -> a -> a
  recent :: a
  seen :: a
  sentBefore :: ZonedTime -> a
  sentOn :: ZonedTime -> a
  sentSince :: ZonedTime -> a
  since :: ZonedTime -> a
  smaller :: Natural -> a
  subject :: String -> a
  text :: String -> a
  to :: String -> a
  uid :: [UIDSet] -> a
  unanswered :: a
  undeleted :: a
  undraft :: a
  unflagged :: a
  unkeyword :: String -> a
  unseen :: a
