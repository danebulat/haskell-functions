-- src/More.hs 

-- -------------------------------------------------------------------
-- A small Morse code library for converting between Morse code 
-- strings to characters.
-- -------------------------------------------------------------------

module Morse (
         charToMorse
       , morseToChar
       , stringToMorse
       , letterToMorse
       , morseToLetter
       ) where

import qualified Data.Map as M

type Morse = String

-- Map to associate characters with their
-- Morse code representations.
letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList 
  [ ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")

  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")

  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")

  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")

  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

-- Construct a "mirrored" map, with morse code as
-- the key, and character as the value
-- https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-IntMap-Internal.html#v:foldrWithKey
morseToLetter :: M.Map Morse Char
morseToLetter =
  M.foldrWithKey (flip M.insert) M.empty letterToMorse

-- Convert a character to Morse code
charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterToMorse 

-- Convert a string of characters to Morse
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:sequence
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = 
  sequence $ fmap charToMorse s -- [Maybe Morse] -> Maybe [Morse]
                                -- mapM better

-- Convert Morse code to character
morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseToLetter
