module ReplaceExperiment where 

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- -------------------------------------------------------------------
-- What happens if we lift it?
--
-- :t fmap replaceWithP :: Functor f => f a -> f Char 

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP 

-- But we can assert a more specific type fo liftedReplace
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace 

-- :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP ::
--   (Functor f1, Functor f2) => f2 (f1 a) -> f2 (f1 Char)

twiceLifted :: (Functor f1, Functor f2)
            => f2 (f1 a) -> f2 (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP 

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted 
-- f1 ~ []
-- f2 ~ Maybe 

-- -------------------------------------------------------------------
-- Thrice?
--
-- :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP ::
--   (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))

thriceLifted :: (Functor f2, Functor f1, Functor f)
             => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP 

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted 
-- f  ~ []
-- f1 ~ Maybe
-- f2 ~ []

-- -------------------------------------------------------------------
-- Now we can print the results from our expressions and compare
-- them:

runProg :: IO ()
runProg = do
  putStr "replaceWithP' lms:    "
  print (replaceWithP' lms)

  putStr "liftedReplace lms:    "
  print (liftedReplace lms)

  putStr "liftedReplace' lms:   "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:      "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:     "
  print (twiceLifted' lms)

  putStr "thriceLifted lms:     "
  print (thriceLifted lms)

  putStr "thriceLifted' lms:    "
  print (thriceLifted' lms)
