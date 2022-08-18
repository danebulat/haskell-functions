{-# LANGUAGE InstanceSigs #-}

module MonadTransformers where 

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(..))


-- -------------------------------------------------------------------
-- MaybeT 

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m
      => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f x = MaybeT $ (fmap . fmap) f (runMaybeT x)

instance Applicative m
      => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . Just

  (<*>) :: MaybeT m (a -> b)
        -> MaybeT m a
        -> MaybeT m b
  x <*> y = MaybeT $ liftA2 (<*>) (runMaybeT x) (runMaybeT y)

instance Monad m
      => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  x >>= k = MaybeT $ do
    ma <- runMaybeT x
    case ma of
      Just a  -> runMaybeT $ k a
      Nothing -> pure Nothing

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . fmap Just 

instance MonadIO m
      => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO
  

-- -------------------------------------------------------------------
-- EitherT 

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f x = EitherT $ (fmap . fmap) f (runEitherT x)

instance Applicative m
      => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . Right

  (<*>) :: EitherT e m (a -> b)
        -> EitherT e m a
        -> EitherT e m b
  x <*> y = EitherT $ liftA2 (<*>) (runEitherT x) (runEitherT y)

instance Monad m
      => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  x >>= k = EitherT $ do
    ea <- runEitherT x
    case ea of
      Right a -> runEitherT $ k a
      Left e  -> pure $ Left e

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadIO m
      => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO = lift . liftIO

-- swapEither function
swapEither :: Either e a -> Either a e
swapEither (Right x) = Left x
swapEither (Left x)  = Right x

-- in terms of Monad 
swapEitherT :: Monad m => EitherT e m a -> EitherT a m e
swapEitherT x = EitherT $ do
  y <- runEitherT x
  pure $ swapEither y

-- in terms of Functor 
swapEitherT' :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT' x = EitherT $  swapEither <$> runEitherT x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g x = do
  y <- runEitherT x
  case y of
    Left e -> f e
    Right a -> g a


-- -------------------------------------------------------------------
-- ReaderT 

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m
      => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f x = ReaderT $ (fmap . fmap) f (runReaderT x)

instance Applicative m
      => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure x = ReaderT $ \r -> pure x

  (<*>) :: ReaderT r m (a -> b)
        -> ReaderT r m a
        -> ReaderT r m b
  x <*> y = ReaderT $ \r ->
    let mf = runReaderT x r
        ma = runReaderT y r
    in runReaderT x r <*> runReaderT y r

instance Monad m
      => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  x >>= k = ReaderT $ \r -> do
    a <- runReaderT x r
    runReaderT (k a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

instance MonadIO m
      => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO


-- -------------------------------------------------------------------
-- StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m
      => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f x = StateT $ \s ->
    fmap (\(a, s') -> (f a, s')) (runStateT x s)

instance Monad m
      => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  x <*> y = StateT $ \s -> do
    (f, s')  <- runStateT x s
    (a, s'') <- runStateT y s
    pure (f a, s'')

instance Monad m
      => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  x >>= k = StateT $ \s -> do
    (a, s') <- runStateT x s
    runStateT (k a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    pure (a, s)

instance MonadIO m
      => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO


-- -------------------------------------------------------------------
-- IdentityT

newtype IdentityT m a =
  IdentityT { runIdentityT :: m a }
  deriving (Eq, Show)

instance Functor m
      => Functor (IdentityT m) where
  fmap f (IdentityT x) = IdentityT $ f <$> x

instance Applicative m
      => Applicative (IdentityT m) where
  pure = IdentityT . pure
  IdentityT f <*> IdentityT x =
    IdentityT $ f <*> x

instance Monad m
      => Monad (IdentityT m) where
  return = pure
  IdentityT x >>= k =
    IdentityT $ x >>= runIdentityT . k

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance MonadIO m
      => MonadIO (IdentityT m) where
  liftIO :: IO a -> IdentityT m a
  liftIO = IdentityT . liftIO

-- -------------------------------------------------------------------
-- Bonus: Compose

-- The f and g represent type constructors,
-- a is a concrete type 
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

-- Functor
instance (Functor g, Functor f)
       => Functor (Compose f g) where
  fmap f x = Compose $ (fmap . fmap) f (getCompose x)

-- Applicative
instance (Applicative f, Applicative g)
       => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure (pure x)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
-- (.)     :: (b -> c) -> (a -> b) -> a -> c
composeEx1 :: Compose [] Maybe Int
composeEx1 =
  let r1 = Compose [Just 1, Nothing]        -- Num a => Compose [] Maybe a
      xs = [Just (1 :: Int), Nothing]       -- [Maybe Int]
      r2 = Compose xs                       -- Compose [] Maybe Int
      r3 = Compose $ Just (Right (1::Int))  -- Compose Maybe (Either String) Int
  in r2

composeEx2 :: Num a => Compose [] Maybe a
composeEx2 =
  let xs = [Just 1, Nothing]
      r1 = Compose xs
  in fmap (+1) r1

-- (fmap . fmap) (+1) v
v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]
