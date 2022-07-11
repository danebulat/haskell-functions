{-# LANGUAGE DeriveGeneric #-}

module Example2 where 

import GHC.Generics ( Generic )
import Data.Text
import Data.Aeson

{-
Simple example showing how to let Haskell generate
the ToJSON and FromJSON instances for a given type.
-}

data Person =
  Person { firstName   :: Text
          , lastName   :: Text
          , age        :: Int
          , likesPizza :: Bool
            } deriving (Show, Generic)

{-
Have Haskell generate instances for us by deriving from the `Generic`
typeclass. Requires DeriveGeneric extension. Import GHC.Generics.
-}

instance FromJSON Person
instance ToJSON Person
