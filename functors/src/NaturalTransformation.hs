{-# LANGUAGE Rank2Types #-}

module NaturalTransformation where 

-- -------------------------------------------------------------------
-- Natural Transformation:
-- 
-- If we wanted to transform only the *structure* and leave the
-- type argument to that structure or type constructor alone.

-- We can attempt to put together a type to express what we
-- want:
--
--     nat :: (f -> g) -> f a -> g a
--
-- This is impossible; cannot have higher-kinded types as
-- argument types to the function type. (`f` and `g` are
-- higher-kinded types as shown in the signature.

-- -------------------------------------------------------------------
type Nat f g = forall a . f a -> g a

-- The quantification of `a` lets us avoid talking about `a`
-- in the type of `Nat`.
--
-- Quantifier will not work without the `RankNTypes` language
-- extension.
--
-- `Rank2Types` also works.

-- This will work:
maybeToList1 :: Nat Maybe []    -- hiding `a` from signature means we can't touch it
maybeToList1 Nothing  = []
maybeToList1 (Just a) = [a]     -- unable to change `a`

-- -------------------------------------------------------------------
-- What if we use a version of Nat that mentions `a` in the type:

type NatBad f g a = f a -> g a

-- This will work
maybeToList2 :: NatBad Maybe [] a
maybeToList2 Nothing  = []
maybeToList2 (Just a) = [a]

-- This will work too
maybeToList3 :: (Num a) => NatBad Maybe [] a
maybeToList3 Nothing  = []
maybeToList3 (Just a) = [a + 1] -- don't want to be able to touch `a`

-- Normal implementation (without RankNTypes)
mtl :: Num a => Maybe a -> [a]
mtl Nothing  = []
mtl (Just a) = [a + 1]          -- don't want to be able to touch `a`
