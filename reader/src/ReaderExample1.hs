module ReaderExample1 where 

import Reader 

-- -------------------------------------------------------------------
-- `Reader r a`
-- where r is some “environment” and a is some value you create
-- from that environment.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Because Reader is a Monad, we can do stuff like this:
--
--   > let r1 = return 5 :: Reader String Int
--
-- We have created a simple Reader using the Monad's
-- return function. Type checking:
--
--   > :t r1
--     r1 :: Reader String Int
--
-- The String is the "environment" of the Reader. So how
-- can we get the Int value out of the reader? By running
-- it:
--
--   > (runReader t1) "this is your environment"
--     5
--
--   > :t runReader
--     runReader :: Reader r a -> r -> a
--
-- So runReader takes in a Reader and an environment (r)
-- and returns a value (a).
--
-- Note that we didn't really do anything with the environment
-- supplied to us.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- What if we had a bunch of Readers and we wanted to bind across
-- them?
-- -------------------------------------------------------------------

-- tom Reader 
--
tom' :: Reader String String
tom' =
  ask >>=
    \env -> return (env ++ " This is Tom.")

tom :: Reader String String
tom = do
  env <- ask -- gives you the environment which in this case is a String
  return (env ++ " This is Tom.")

-- jerry Reader
--
jerry' :: Reader String String
jerry' =
  ask >>=
    \env -> return (env ++ " This is Jerry")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry")

-- tomAndJerry Reader 
-- 
tomAndJerry' :: Reader String String
tomAndJerry' =
  tom' >>=
    \t -> jerry' >>=
      \j -> return (t ++ "\n" ++ j)

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

-- run program
--
runProgram :: IO ()
runProgram = putStrLn str
  where str = (runReader tomAndJerry') "Who is this?"

