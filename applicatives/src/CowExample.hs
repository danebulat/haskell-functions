module CowExample where 

import Control.Applicative

data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing 

-- Validating to get rid of empty
-- strings, negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow 
cowFromString' name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name')         -- <$>
             (noNegative age')       -- <*>
             (noNegative weight')    -- <*>

-- Summary
-- -------------------------------------------------------------------

-- We fmap'd function over some
-- functorial ``f'' or it already
-- was in ``f'' somehow 

-- f ~ Maybe

cow1 :: Maybe (Int -> Int -> Cow)
cow1 = fmap Cow (noEmpty "Bess")

-- and we hit a situation where we want to map
--           f (a -> b)
-- not just    (a -> b)
--
-- (<*>) :: Applicative f
--       => f (a -> b) -> f a -> f b
--
-- over some   f a
-- to get an   f b

cow2 :: Maybe (Int -> Cow)
cow2 = cow1 <*> noNegative 1

-- As a result, you may be able to imagine yourself saying, “I
-- want to do something kinda like an fmap, but my function is
-- embedded in the functorial structure too, not only the value I
-- want to apply my function to.” This is a basic motivation for
-- Applicative.

