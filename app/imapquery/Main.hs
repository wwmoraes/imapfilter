module Main (main) where

import Control.Exception      (NonTermination (NonTermination))
import Control.Exception.Base (handle)
import Control.Monad          (forM_)
import System.Environment     (getArgs, getEnv)
import System.Exit            (ExitCode (ExitFailure), exitWith)

import Network.HaskellNet.IMAP (fetchHeaderFields, list, login, search, select)

import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.SSL        (connectIMAPSSL)
import Network.HaskellNet.IMAP.Types      (UID)

import Network.HaskellNet.IMAP.Search (parseSearchQuery)

import qualified Data.ByteString as BS

main :: IO ()
main = handle (\NonTermination -> exitWith (ExitFailure 1)) $ do
  query <- either (error . show) id . parseSearchQuery . unwords <$> getArgs

  putStr "parsed query: "
  print query

  con <- connectWithEnv
  loginWithEnv con

  boxes <- list con
  putStrLn "mailboxes:"
  mapM_ (putStrLn . snd) boxes

  select con "INBOX"

  messages <- search con query
  mapM_ print (take 3 messages)
  putStrLn "first 3 messages:"
  forM_ (take 3 messages) (showMessage con)

connectWithEnv :: IO IMAPConnection
connectWithEnv = getEnv "IMAP_SERVER" >>= connectIMAPSSL

loginWithEnv :: IMAPConnection -> IO ()
loginWithEnv conn = do
  username <- getEnv "IMAP_USERNAME"
  password <- getEnv "IMAP_PASSWORD"
  login conn username password

showMessage :: IMAPConnection -> UID -> IO ()
showMessage c u = do
  headers <- fetchHeaderFields c u ["FROM", "TO", "SUBJECT"]
  BS.putStr headers
  putStrLn (replicate 80 '#')
