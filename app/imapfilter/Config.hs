{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
  Config,
  Filter(..),
  rsAddress,
  rsPassword,
  rsUsername,
  RuleSet(..),
) where

import Data.Text    (unpack)
import Data.Yaml    (FromJSON)
import GHC.Generics (Generic)

import qualified Data.Aeson.KeyMap              as KM
import qualified Data.Map                       as M
import qualified Data.Yaml                      as Y
import           Network.HaskellNet.IMAP        (SearchQuery)
import           Network.HaskellNet.IMAP.Search (parseSearchQuery)
import           Network.HaskellNet.IMAP.Types  (MailboxName)


data Server = Server
  { address  :: String
  , username :: String
  , password :: String
  }
  deriving (Show, Generic, FromJSON)

data Filter
  = Archive [SearchQuery]
  | Copy [SearchQuery] String
  | Delete [SearchQuery]
  | Move [SearchQuery] String
  | Trash [SearchQuery]
  deriving (Show, Generic)

instance FromJSON Filter where
  parseJSON = Y.withObject "Filter" $ filterAction . KM.toList
    where
      filterAction [("archive", Y.String q)]
        = return . Archive . unsafeParseSQ $ unpack q
      filterAction [("copy", Y.String q),("target", Y.String t)]
        = return $ Copy (unsafeParseSQ $ unpack q) (unpack t)
      filterAction [("delete", Y.String q)]
        = return . Delete . unsafeParseSQ $ unpack q
      filterAction [("move", Y.String q),("target", Y.String t)]
        = return $ Move (unsafeParseSQ $ unpack q) (unpack t)
      filterAction [("trash", Y.String q)]
        = return . Trash . unsafeParseSQ $ unpack q
      filterAction []
        = fail "a filter must have one action"
      filterAction (_:_:_)
        = fail "a filter must have only one action"
      filterAction _
        = fail "invalid filter syntax"

data RuleSet = RuleSet
  { server    :: Server
  , mailboxes :: M.Map MailboxName [Filter]
  }
  deriving (Show, Generic, FromJSON)

type Config = [RuleSet]

rsAddress :: RuleSet -> String
rsAddress = address . server

rsUsername :: RuleSet -> String
rsUsername = username . server

rsPassword :: RuleSet -> String
rsPassword = password . server

unsafeParseSQ :: String -> [SearchQuery]
unsafeParseSQ = either (error . show) id . parseSearchQuery
