{-# LANGUAGE InstanceSigs #-}

module Reader where 

-- Reader Type
newtype Reader r a =
  Reader { runReader :: r -> a }

-- Reader Functor 
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

-- Reader Applicative 
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\_ -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  rf <*> rx =
    Reader (\r -> (runReader rf r) (runReader rx r))

-- Reader Monad 
instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= faRrb =
    Reader $ \r -> runReader (faRrb (ra r))  r

-- Ask 
ask :: Reader a a
ask = Reader id
