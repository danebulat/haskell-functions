module TypesSafe where 

import Control.Concurrent hiding
  ( Async(..), async, wait, cancel
  , Chan(..), writeChan, readChan, newChan
  , readMVar, forkFinally
  )
import Control.Exception
  ( SomeException, try, mask, mask_, throwIO
  , AsyncException (ThreadKilled)
  )


-- -------------------------------------------------------------------
-- Async 

data Async a = Async ThreadId (MVar (Either SomeException a))

-- | async without forkFinally 
async' :: IO a -> IO (Async a)
async' action = do
  m <- newEmptyMVar
  t <- mask $ \restore ->
    forkIO (do r <- try (restore action); putMVar m r)
  return (Async t m)

-- | safely run an action followed by another action in a new thread 
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

-- | async with forkFinally
async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkFinally action (putMVar m)
  return (Async t m)

-- | wait for the thread to finish and return its Either value
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var

-- | wait for the thread to finish and return its value inside Either 
wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e  -- propogate error
    Right a -> return a  -- operation successful, return a

-- | make it safe by wrapping it in mask_
readMVar :: MVar a -> IO a
readMVar m =
  mask_ $ do
    a <- takeMVar m
    putMVar m a
    return a

-- | throw the ThreadKilled exception in a given thread (also see cancelWith)
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled


-- -------------------------------------------------------------------
-- Safe Channel 

type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data Chan a =
  Chan (MVar (Stream a))
       (MVar (Stream a))

-- | construct a new channel
newChan :: IO (Chan a)
newChan = do
  hole     <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

-- | write a value to the end of channel stream
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  mask_ $ do
    oldHole <- takeMVar writeVar
    putMVar oldHole (Item val newHole)
    putMVar writeVar newHole

-- | read value at front of channel stream
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \stream -> do
    Item val tail <- readMVar stream
    return (tail, val)
         -- set readVar to tail
         -- return val 
