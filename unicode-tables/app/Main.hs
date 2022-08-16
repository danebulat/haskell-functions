module Main where

import Control.Monad      (mapM_)
import Data.Char          (toLower)
import Data.Default
import Data.List          (delete, isSuffixOf)
import Numeric
import System.Environment (getArgs)

-- -------------------------------------------------------------------
-- Data types

data CharacterSet =
  CharacterSet { start      :: Integer
               , end        :: Integer
               , name       :: String
               , slug       :: String
               } deriving (Eq, Show)

instance Default CharacterSet where
  def = CharacterSet
    { start=32, end=127, name="Basic Latin", slug="basic-latin" }


-- -------------------------------------------------------------------
-- Functions

mkCharacterSet :: String -> String -> Maybe CharacterSet
mkCharacterSet input content =
  let line = findSet input (lines content)
  in case line of
    Nothing -> Nothing
    Just x  -> let ws = words x
                   s  = getBound (ws !! 0)
                   e  = getBound (ws !! 1)
                   n  = input
                   sl = slugName input
               in Just $ CharacterSet { start=s, end=e, name=n, slug=sl }
  where findSet _ [] = Nothing
        findSet s (x:xs)
          | s `isSuffixOf` x = Just x
          | otherwise  = findSet s xs
        getBound = fst . head . readHex

-- for map values v in (k v)
getHexSequences :: CharacterSet -> [String]
getHexSequences = getEscapeSeqs . getHexStrings
  where getEscapeSeqs = map (\x -> "\"\\x" ++ x ++ "\"")

-- for map keys k in (k v)
getHexStrings :: CharacterSet -> [String]
getHexStrings cs = map ((flip showHex) "") [s..e]
  where s = start cs
        e = end cs

-- a map represents (simple hex code, `read`able hex code)
getMap :: CharacterSet -> [(String, String)]
getMap cs = zipWith combine strs escSeqs
  where
    escSeqs     = getHexSequences cs
    strs        = getHexStrings cs
    combine x y =
          let l  = length x
              x' = if l < 4 then (replicate (4 - l) '0') ++ x else x 
          in (x', y)   -- x' is padded with 0 to be 4 characters

-- render the generated map 
renderMap :: [(String, String)] -> IO ()
renderMap cs = do
  renderCols
  go 1 cs
  where go _ []     = return ()
        go n (x:xs) = do
          putStrLn $ "  "   ++ show n
                  ++ "\t"   ++ fst x
                  ++ "\t"   ++ (init (tail (snd x)))
                  ++ "\t  " ++ (read $ snd x)
          go (n + 1) xs
        renderCols = putStrLn $ "\n  No.\tHex\t\t  Char\n"

-- utilities
rmSequence :: String -> [String] -> [String]
rmSequence x = delete x

outputChars :: [String] -> IO ()
outputChars = mapM_ (putStr . read)

slugName :: String -> String 
slugName = fmap toLower . map (\x -> if x == ' ' then '-' else x)


-- ------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  args <- getArgs
  content <- readFile "data/ranges.txt"
 
  let mcs = mkCharacterSet (args !! 0) content
  case mcs of
    Nothing -> putStrLn $ "unable to load character set: " ++ (args !! 0)
    Just cs -> renderMap . getMap $ cs

