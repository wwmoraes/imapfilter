# imapfilter

> IMAP message filtering and actions for humans, in pure Haskell

## Table of Contents

- [About](#about)
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Contributing](../CONTRIBUTING.md)

## About

A library that filters messages found using human-readable search queries that
resemble Gmail operations. The command-line tool leverages that to act upon the
search results.

The current version implements the expression interface for the [HaskellNet]
`SearchQuery` type.

NOTE: This is a work-in-progress, use at your own risk! 😷

## Getting Started

The project uses Stack, so you can clone and `stack build` to get it up and
running locally.

## Usage

Expressions can contain either literal terms to search as plain text and/or
operators to configure specific criteria. The query language supports all
[RFC 9051 search keys][rfc-9051-search-command], plus some
[Gmail-specific operators][gmail-operators] for compatibility. Here's a
non-exhaustive list of criteria supported and how to use it:

[rfc-9051-search-command]: https://datatracker.ietf.org/doc/html/rfc9051#name-search-command
[gmail-operators]: https://support.google.com/mail/answer/7190?hl=en

| Criteria                | Query                                         |
|-------------------------|-----------------------------------------------|
| `FROM/TO/CC/BCC`        | `from:foo@bar.com`                            |
| `DELETED/DRAFT/FLAGGED` | `is:draft`, `-is:flagged` (or `is:unflagged`) |
| `ANSWERED/SEEN/NEW/OLD` | `is:answered`, `-is:seen` (or `is:unseen`)    |
| `KEYWORD`               | `keyword:foo`                                 |
| `SUBJECT/BODY`          | `subject:lorem`, `subject:(lorem ipsum)`      |
| `BEFORE/ON/SINCE`       | `before:2023/4/18`                            |
| `SENTBEFORE/-ON/-SINCE` | `sent_before:2023/4/18`                       |
| `HEADER`                | `header:foo=bar`                              |
| `LARGER/SMALLER`        | `larger:10`                                   |
| `NOT`                   | `-from:qux@bar.com`                           |
| `OR`                    | `{from:foo@bar.com from:qux@bar.com}`         |

Gmail-compatibility ones:

| Criteria      | Query                                |
|---------------|--------------------------------------|
| `SEEN/UNSEEN` | `is:read`, `is:unread`               |
| `LARGER`      | `size:10`                            |
| `SINCE`       | `after:2023/4/18`, `newer:2023/4/18` |
| `BEFORE`      | `older:2023/4/18`                    |

Check the `examples` folder in this repository for samples on how to use the
expressions directly in code or with common libraries such as `HaskellNet` and
`imap`.

### inline queries

You can build library-independent queries directly in code using the
`SearchExpr` class. For that, it's wise to import the module qualified, as some
functions conflict with Prelude, such as `or`, `all` and `not`. For example:

```haskell
import           System.IO.Unsafe    (unsafePerformIO)
import qualified System.Time         as ST
import qualified Network.IMAP.Search as Search (SearchExpr (..))

{-# NOINLINE now #-}
now :: ST.ClockTime
now = unsafePerformIO ST.getClockTime

1dago :: ST.ClockTime
1dago = ST.addToClockTime (ST.TimeDiff {ST.tdDay = -1}) now

read1dago :: SearchExpr a => [a]
read1dago = [ Search.seen
            , Search.before (ST.toCalendarTime 1dago)
            ]
```

You can then use `read1dago` with any library interface that takes a list of
query operations, such as `HaskellNet`'s `search`. You can also transform it to
the generic `Operation` type provided, which you can transform to string or text
for libraries that don't have proper typing for the IMAP search keys:

```haskell
```

### With HaskellNet

```haskell
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
```
