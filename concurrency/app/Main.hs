{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use writeList2Chan" #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens 
import Control.Monad (forever, replicateM, mapM_)
import System.CPUTime
import qualified System.Random as R
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as B 

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

