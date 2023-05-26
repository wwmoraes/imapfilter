module Helpers where

import           Network.IMAP.Search (SearchExpr, UIDSet (..))
import qualified Network.IMAP.Search as Search (SearchExpr (..))

fromEither :: Show a => Either a b -> b
fromEither = either (error . show) id

showQuery :: (Show a, SearchExpr a) => [a] -> String
showQuery = unwords . map show

testQuery :: SearchExpr a => [a]
testQuery = [ Search.all
            , Search.undraft
            , Search.seen
            , Search.uid [Single 1]
            ]
