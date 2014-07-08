--Connor Cormier, 7/07/14

module DiningData where

import Data.List (intersperse)

--Each type includes a name and a list of the next highest type in the hierarchy
data DiningHallMenu = DiningHallMenu String [Meal] deriving (Show)
data Meal = Meal String [Station] deriving (Show)
data Station = Station String [Item] deriving (Show)
data Item = Item String [Nutrition] deriving (Show)

newtype Nutrition = Nutrition String deriving (Show)

data DiningHallOverview = DiningHallOverview { name :: String
                                             , description :: String
                                             , location :: String
                                             , hours :: String
                                             , acceptsMealswipes :: String
                                             , mealswipeHours :: String
                                             } deriving (Show)

jsonTemplate :: (Json a) => String -> String -> [a] -> String
jsonTemplate name listLabel list = "{\"name\": \"" ++
                                     name ++ "\", \"" ++
                                     listLabel ++ "\": " ++
                                     jsonList list ++ "}"


jsonList :: (Json a) => [a] -> String
jsonList xs = "[" ++ (convertList xs) ++ "]"
         where convertList = concat . intersperse ", " . map (json)

--To JSON
class Json a where
      json :: a -> String

instance Json (DiningHallMenu) where
         json (DiningHallMenu name meals) = jsonTemplate name "meals" meals

instance Json (Meal) where
         json (Meal name stations) = jsonTemplate name "stations" stations

instance Json (Station) where
         json (Station name items) = jsonTemplate name "items" items

instance Json (Item) where
         json (Item name nutrition) = jsonTemplate name "nutrition" nutrition

--For our String type: convert to json. Allows us to maintain consistency
instance Json (Nutrition) where
         json (Nutrition info) = info

--List instance
instance (Json a) => Json [a] where
         json lst = "[" ++ (toJson lst) ++ "]"
              where toJson = concat . intersperse ", " . map (json)

--The overview:
instance Json (DiningHallOverview) where
         json (DiningHallOverview name des loc hours aM mH) =
                "{\"accepts_mealswipes\": \"" ++ aM ++ "\", \"name\": \"" ++ name ++ "\", \"mealswipe_hours\": \"" ++ mH ++ "\", \"hours\": \"" ++ hours ++ "\", \"location\": \"" ++ loc ++ "\", \"description\": \"" ++ des ++ "\"}"
