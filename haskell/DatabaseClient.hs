--Connor Cormier, 7/07/14

module DatabaseClient where

import Database.HDBC
import Database.HDBC.ODBC
import DiningData
import Data.List (groupBy)
import Data.Function (on)
import Data.Char (isSpace)


databaseConnectionString :: String
databaseConnectionString = "DSN=HooEats;"


connectDb :: IO Connection
connectDb = connectODBC databaseConnectionString

--Wrapper for symmetry
closeDb :: Connection -> IO ()
closeDb = disconnect

--Query to access all data at once
retrieveAllQuery :: String
retrieveAllQuery = "select DiningHalls.name,Meals.name,Stalls.name,Items.name,Items.nutrition from DiningHalls left join Meals on DiningHalls.id = Meals.dining_hall left join Stalls on Meals.id = Stalls.meal left join Items on Stalls.id = Items.stall where DiningHalls.active = 1 and Meals.active = 1 and Stalls.active = 1 and Items.active = 1;"

--Access database with single query
retrieveAll :: Connection -> IO [DiningHallMenu]
retrieveAll db = do
            results <- stringQuery db retrieveAllQuery
            let halls = toDiningHallMenus results
            return halls


--Access database with multiple queries
retrieveAllMulti :: Connection -> IO [DiningHallMenu]
retrieveAllMulti db = do
                 halls <- stringQuery db "select id,name from DiningHalls where active=1"
                 meals <- stringQuery db "select dining_hall,id,name from Meals where active=1"
                 stats <- stringQuery db "select meal,id,name from Stalls where active=1"
                 items <- stringQuery db "select stall,name,nutrition from Items where active=1"
                 return $ process halls meals stats items

--Access overviews
retrieveOverviews :: Connection -> IO [DiningHallOverview]
retrieveOverviews db = do
                  lst <- stringQuery db "select * from DiningHallOverviews"
                  let views = processOverviews lst
                  return views

------------------------------------------
--Utilities for processing query results--
------------------------------------------

--Processing for single query
groupHead :: (Eq a) => [[a]] -> [(a, [[a]])]
groupHead = map split . grouped
          where grouped = groupBy ((==) `on` head)
                split xs = ((head . head $ xs),  map tail xs)

--Split at a string. This is a bit hard to read. Sorry.
splitAtS :: String -> String -> [String]
splitAtS del tot = reverse $ map reverse $ split tot [""]
        where split string@(h:str) list@(curWord:ls)
                    | take (length del) string /= del = split str ((h:curWord):ls)
                    | otherwise = split (drop (length del) string) ([]:list)
              split [] list = list

toItems :: [[String]] -> [Item]
toItems hI = map (\(name:nut:[]) -> Item name $ toList nut) hI
        where toList = map (\x -> Nutrition x) . listify
              listify = splitAtS ", " . init . tail

toStations :: [[String]] -> [Station]
toStations hS = map toStation (groupHead hS)
           where toStation (name, items) = Station name (toItems items)

toMeals :: [[String]] -> [Meal]
toMeals hM = map toMeal (groupHead hM)
        where toMeal (name, stations) = Meal name (toStations stations)

toDiningHallMenus :: [[String]] -> [DiningHallMenu]
toDiningHallMenus hM = map toDiningHallMenu (groupHead hM)
                  where toDiningHallMenu (name, meals) = DiningHallMenu name (toMeals meals)
--End processing for single query

--Processing for multi-query.
process :: [[String]] -> [[String]] -> [[String]] -> [[String]] -> [DiningHallMenu]
process hallLs mealLs statLs itemLs = map hallify hallLs
        where hallify [id, name] = DiningHallMenu name $ (map mealify . filter ((==id) . head)) mealLs
              mealify [_, id, name] = Meal name $ (map statify . filter ((==id) . head)) statLs
              statify [_, id, name] = Station name $ toItems $ map (tail) $ filter ((==id) . head) itemLs
--End processing for multi-query

convStrLst :: [[SqlValue]] -> [[String]]
convStrLst = map (map $ trimStr . (\x -> fromSql x :: String))
           where trimStr s = reverse . dropWhile isSpace . reverse . dropWhile isSpace $ s

stringQuery :: Connection -> String -> IO [[String]]
stringQuery db sQ = do
            res <- quickQuery' db sQ []
            let strRes = convStrLst res
            return strRes

processOverviews :: [[String]] -> [DiningHallOverview]
processOverviews = map toOverview
                 where toOverview [_, name, des, loc, hours, aM, mH] =
                         DiningHallOverview name des loc hours aM mH

------------------------------------------
--End utilities                         --
------------------------------------------
