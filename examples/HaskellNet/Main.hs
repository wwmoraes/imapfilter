module Main (main) where

import Control.Monad      (forM_)
import System.Environment (getArgs, getEnv)

import Network.HaskellNet.IMAP (fetchHeaderFields, login, search, select)

import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.Search     (parseSearchQuery)
import Network.HaskellNet.IMAP.SSL        (connectIMAPSSL)

import qualified Data.ByteString as BS

main :: IO ()
main = do
  query <- either (error . show) id . parseSearchQuery . unwords <$> getArgs

  putStr "parsed query: "
  print query

  con <- connectWithEnv
  loginWithEnv con

  select con "INBOX"

  messages <- search con query
  putStrLn "results:"
  separatorLn
  forM_ messages $ \m -> do
    headers <- fetchHeaderFields con m ["FROM", "TO", "SUBJECT"]
    BS.putStr headers
    separatorLn

connectWithEnv :: IO IMAPConnection
connectWithEnv = getEnv "IMAP_SERVER" >>= connectIMAPSSL

loginWithEnv :: IMAPConnection -> IO ()
loginWithEnv conn = do
  username <- getEnv "IMAP_USERNAME"
  password <- getEnv "IMAP_PASSWORD"
  login conn username password

separatorLn :: IO ()
separatorLn = putStrLn $ replicate 80 '#'
