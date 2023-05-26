module Main (main) where

import qualified Data.Text           as T
import           Network.IMAP.Search (parseOperations)
import           System.Environment  (getArgs)

main :: IO ()
main = do
  ops <- either (error . show) id . parseOperations . unwords <$> getArgs
  let query = T.unwords $ map (T.pack . show) ops
  putStr "query: "
  putStrLn $ T.unpack query
