module Main (main) where

import Control.Exception      (NonTermination (NonTermination))
import Control.Exception.Base (handle)
import Data.Foldable          (Foldable (foldl'))
import Data.Function.Apply    ((--$))
import System.Environment     (getArgs)
import System.Exit            (ExitCode (ExitFailure), exitWith)

import qualified Data.Map as M

import Network.HaskellNet.IMAP            (Flag (Deleted),
                                           FlagsQuery (PlusFlags), login,
                                           search, select, store)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.SSL        (connectIMAPSSL)

import Config                        (Config, Filter (..), RuleSet (..),
                                      rsAddress, rsPassword, rsUsername)
import Data.Yaml                     (decodeFileThrow)
import Network.HaskellNet.IMAP.Types (MailboxName)

main :: IO ()
main = handle (\NonTermination -> exitWith (ExitFailure 1)) $ do
  config <- foldl' (<>) [] <$> (mapM loadConfig =<< getArgs)
  mapM_ executeCollection config

loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow

executeCollection :: RuleSet -> IO ()
executeCollection r = do
  conn <- connectIMAPSSL $ rsAddress r
  login conn <$> rsUsername <*> rsPassword $ r
  M.foldMapWithKey (executeMailboxes conn) (mailboxes r)

executeMailboxes :: IMAPConnection -> MailboxName -> [Filter] -> IO ()
executeMailboxes c k fs = select c k *> mapM_ (executeFilter c) fs

executeFilter :: IMAPConnection -> Filter -> IO ()
executeFilter c (Delete q) = search c q >>= mapM_ (store --$ PlusFlags [Deleted] $ c)
executeFilter _ _ = fail "unknown type of action"
