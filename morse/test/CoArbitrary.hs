{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where

import GHC.Generics
import Test.QuickCheck

-- Deriving Generic 
data Bool' =
  True' | False' deriving (Generic)

-- Make type instance of CoArbitrary 
instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary 

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary 

-- Read:
-- https://carlo-hamalainen.net/2018/01/30/quickchecks-coarbitrary-generate-random-functions/
