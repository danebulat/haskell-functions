{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use writeList2Chan" #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Applicative
import Control.Monad (forever, replicateM, mapM_, mapM)
import System.CPUTime
import qualified System.Random as R
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Network.Wreq (responseBody)

-- -----------------------------------------------------------------
-- Channel Demo 

worker :: Chan Int -> IO ()
worker chan = forever $ do
  -- delay thread between 1 and 2 seconds 
  r <- R.randomRIO (1, 2)
  threadDelay (10^6 * r)
  -- read the channel and print the value
  v <- readChan chan
  putStr $ show v

main :: IO ()
main = do
  chan <- newChan
  -- start 10 workers (blocking)
  ids <- replicateM 10 (forkIO (worker chan))
  -- write to channel, workers will consume values
  mapM_ (writeChan chan) [1..100]

-- -------------------------------------------------------------------
-- Simple MVar Demo
--
-- Downloading web pages asynchronously and displaying some
-- results in the main thread.

demo01 :: IO ()
demo01 = do
  -- empty MVars to hold results
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  -- download webpages and store results in MVars
  forkIO $ do
    r <- W.get "http://www.wikipedia.org/wiki/Shove"
    putMVar m1 r

  forkIO $ do
    r <- W.get "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  -- blocks until spawned threads put data in MVar 
  r1 <- takeMVar m1
  r2 <- takeMVar m2

  -- print 
  print ( B.length $ r1 ^. W.responseBody
        , B.length $ r2 ^. W.responseBody
        )

-- -------------------------------------------------------------------
-- Async Demo
--
-- Using the Async interface to clean up our webpage downloading
-- example.

demo02 :: IO ()
demo02 = do
  a1 <- async (W.get "http://www.wikipedia.org/wiki/Shove")
  a2 <- async (W.get "http://www.wikipedia.org/wiki/Spade")

  r1 <- wait a1
  r2 <- wait a2

  print ( B.length $ r1 ^. W.responseBody
        , B.length $ r2 ^. W.responseBody
        )

-- -------------------------------------------------------------------
-- Timing Demo

sites :: [String]
sites =
  [ "http://www.google.com"
  , "http://www.bing.com"
  , "http://www.yahoo.com"
  , "http://www.wikipedia.com/wiki/Spade"
  , "http://www.wikipedia.com/wiki/Shovel"
  ]

-- times an IO action and returns its result and time taken 
timeIt :: Fractional d => IO a -> IO (a, d)
timeIt action = do
  start <- getCPUTime
  res   <- action
  end   <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12)
  return (res, diff)

-- timing a web page download
timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeIt $ W.get url
  let len = B.length $ page ^. responseBody
  print $ "downloaded" ++ url
                       ++ " (" ++ show len ++ " bytes, "
                       ++ show time ++ "s)"

demo03 :: IO ()
demo03 = do
  as <- mapM (async . timeDownload) sites
  mapM_ wait as

-- -------------------------------------------------------------------
-- Logger Demo

newtype Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

-- creates a new logging service
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s () -- close logger thread (doesn't call loop)

-- lets a client log a message (put a message command in MVar)
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

-- stops the log thread
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)  -- send stop command to logger thread 
  takeMVar s          -- wait until logger threads stops (blocks)

-- test logger
demo04 :: IO ()
demo04 = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l

-- -------------------------------------------------------------------
-- Phonebook Demo 
--
-- The following example models a phone book as a piece of mutable
-- state that may be concurrently modified and inspected by multiple
-- threads. 

type Name        = String
type PhoneNumber = String
type PhoneBook   = M.Map Name PhoneNumber

newtype PhoneBookState =
  PhoneBookState (MVar PhoneBook)

-- making a new phonebook state
new :: IO PhoneBookState
new = do
  m <- newMVar M.empty
  return $ PhoneBookState m

-- insert operation can run in a spawned thread
insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (M.insert name number book)  

-- lookup operation 
lookUp :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookUp (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ M.lookup name book

-- test phonebook demo
demo05 :: IO ()
demo05 = do
  s <- new
  sequence_ [insert s ("name" ++ show n) (show n) | n <- [1..10000]]
  lookUp s "name999" >>= print
  lookUp s "unknown" >>= print
