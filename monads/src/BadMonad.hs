module BadMonad where 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes 

-- -------------------------------------------------------------------
-- Data structure

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)         -- fixed
--fmap f (CountMe i a) = CountMe (i + 1) (f a)   -- i+1 bad!

instance Applicative CountMe where
  pure = CountMe 0             -- Identity value for Monoid Sum (integer addition)
  (CountMe n f) <*> (CountMe n' a) =
    CountMe (n + n') (f a)
         -- (n + 1 )           -- Will break the applicative!

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =          -- Fixed!
    let CountMe n' b = f a
    in CountMe (n + n') b      -- Change monoid-of-structure to addition/identity zero for `n`

--  CountMe _ a >>= f = f a    -- Still fails right identity law (bad pure)
                               -- CountMe 2 "blah" >>= return
                               -- CountMe 0 "blah"
--  CountMe n a >>= f =
--    let CountMe _ b = f a    -- Ignore count in function output (bad)
--    in CountMe (n + 1) b     -- Increment original count by 1   (bad)

-------------------------------------------------------------------
-- QuickCheck 

instance Arbitrary a
      => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

-- -------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger 

