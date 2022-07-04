module ReaderExample2 where

import Reader 

-- -------------------------------------------------------------------
-- Another simple example

myReader1 :: Reader Int Int
myReader1 =
  ask >>= \x -> return $ x + 1

myReader2 :: Reader Int String
myReader2 =
  ask >>= \x -> return $ show x

myReader :: Reader Int String
myReader =
  myReader1 >>=
    \x -> myReader2 >>=
      \y -> return $ show x ++ y

runExample :: Int -> String
runExample n = (runReader myReader) n
