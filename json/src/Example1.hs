{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Example1 where 

{-
Example fetches JSON from a file or URL in the form of
a ByteString. This ByteString is parsed into a list of
Person objects via parsing functions developed with
the trifecta.
-}

import Data.Text            ( Text )
import Control.Applicative  ( (<|>) )
import Control.Monad        ( mzero, mapM )
import Network.HTTP.Conduit ( simpleHttp )
import Data.Aeson
import Text.RawString.QQ

import qualified Text.Trifecta as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string

data Person =
  Person { firstName  :: !(Maybe Text)
         , lastName   :: !(Maybe Text)
         , age        :: Int
         , likesPizza :: Bool
         } deriving (Eq, Show)

{-
if a field in your type is not present in the JSON data, an error
will arise. If you want to have optional records, use (.:?) instead
of (.:) in the JSON parser. 
-}

instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .:? "firstName"
           <*> v .:? "lastName"
           <*> v .:  "age"
           <*> v .:  "likesPizza"
  parseJSON _ = mzero

instance ToJSON Person where 
  toJSON (Person firstName lastName age likesPizza) =
    object [ "firstName"  .= firstName
           , "lastName"   .= lastName
           , "age"        .= age
           , "likesPizza" .= likesPizza
           ]

-- -------------------------------------------------------------------
-- JSON from a file 

jsonFile :: FilePath
jsonFile = "data/example1/survey.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile 

-- -------------------------------------------------------------------
-- JSON from a URL
-- Using `simpleHttp` from the `http-conduit` package

jsonURL :: String
jsonURL = "http://path/to/file.json"

getJSON' :: IO B.ByteString
getJSON' = simpleHttp jsonURL

-- -------------------------------------------------------------------
-- Tests

testEncode :: Bool
testEncode =
  let s = encode $ Person
            { firstName  = Just "Foo"
            , lastName   = Just "Bar"
            , age        = 30
            , likesPizza = True }
  in s == [r|{"age":30,"firstName":"Foo","lastName":"Bar","likesPizza":true}|]

testDecode :: Bool
testDecode =
  let Just p = decode [r|{"age":30,"firstName":"Foo","lastName":"Bar","likesPizza":true}|]
   in p == Person (Just "Foo") (Just "Bar") 30 True

-- -------------------------------------------------------------------
-- JSON parser written with trifecta

skipChar :: Char -> T.Parser ()
skipChar c = T.char c >> return ()

skipCommaOrBracket :: T.Parser ()
skipCommaOrBracket = do
  T.skipMany (T.char ',' >> return ()) <|>
           (T.char ']' >> return ())

skipEOL :: T.Parser ()
skipEOL = do
  T.skipMany (T.oneOf "\n")

skipSpaces :: T.Parser ()
skipSpaces = do
  T.skipMany (T.oneOf " ")

skipWS :: T.Parser ()
skipWS = skipSpaces >> skipEOL >> skipSpaces

parsePerson :: T.Parser String
parsePerson = do
  x <- T.char '{'
  y <- T.some (T.noneOf "}")
  z <- T.char '}'
  skipWS
  skipCommaOrBracket
  skipWS
  return ([x] ++ y ++ [z])

parseAllPersons :: T.Parser [String]
parseAllPersons = do
  skipChar '['
  skipWS 
  let ps = []
  ps <- T.many parsePerson
  skipChar ']'
  return ps

-- Splits the retrieved JSON string into a list of
-- strings. Each string can be decoded into a single
-- Person object.

splitPersonJSON :: B.ByteString -> IO (T.Result [String])
splitPersonJSON bs = do
  let str = BLU.toString bs
  return $ T.parseString parseAllPersons mempty str

-- Confirms that JSON objects were parsed into a list
-- with length greater than one.

confirmLength :: IO Int
confirmLength = do
  bs <- getJSON
  (T.Success xs) <- splitPersonJSON bs
  return $ length xs

-- -------------------------------------------------------------------
-- Person utilities

getPerson :: Maybe Person -> Person
getPerson Nothing  = error "expecting Just Person"
getPerson (Just p) = p

outputPersons :: [Maybe Person] -> IO ()
outputPersons = mapM_ (print . getPerson)

mkPersonList :: IO [Maybe Person]
mkPersonList = do
  bs <- getJSON  -- or getJSON' to fetch via URL
  (T.Success ps) <- splitPersonJSON bs
  return $ map (decode . BLU.fromString) ps

-- -------------------------------------------------------------------
-- Main 

main :: IO ()
main = do
  ps <- mkPersonList
  outputPersons ps
  return ()

