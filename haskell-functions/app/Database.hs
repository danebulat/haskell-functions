module Database where 

import Data.Time

-- ===================================================================
-- Folding Exercises
-- ===================================================================

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

-- The database is a list of DatabaseItem
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9022
  ]

-- 1.
-- Write a function that filters for DbDate values and
-- returns a list of UTCTime values inside them.

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
  where f (DbDate time) acc = time : acc
        f _ acc             = acc

-- 2.
-- Write a function that filters for DbNumber values
-- and returns a lis of the Integer values inside them.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber n) acc = n : acc
        f _ acc            = acc

-- 3.
-- Write a function that gets the most recent data
-- NOTE: getCurrentTime

-- Solution: 
-- Retreive all the dates in the database and determine
-- the most recent date with the foldr1 function.

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr1 (\x acc-> if x < acc then x else acc) dates
  where dates = filterDbDate xs

-- 4.
-- Write a function that sums all of the DbNumber values

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 ns
  where ns = filterDbNumber xs
  
-- 5.
-- Write a function that gets the average of the DbNumber
-- values.
--
-- a) Get all Integers from database and convert  (Num a)
-- b) Calculate number of Integers in database    (Num a)
-- c) Calculate average Integer value in database (Double)

avgDb :: [DatabaseItem] -> Double
avgDb xs = foldr1 (+) ns / l                       -- c)
  where ns = map fromIntegral $ filterDbNumber xs  -- a)
        l  = fromIntegral $ length ns              -- b)

