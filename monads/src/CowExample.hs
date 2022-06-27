module CowExample where 

-- Maybe Monad
-- -------------------------------------------------------------------

data Cow = Cow {
     name   :: String
   , age    :: Int
   , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing 
noEmpty str = Just str 

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing 

-- If Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
  
-- Writing it with (>>=)
-- Stack up the nested lambdas.
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
        weightCheck (Cow nammy agey weighty)
  
