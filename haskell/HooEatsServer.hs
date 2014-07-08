--Connor Cormier, 7/07/14

import Happstack.Server
import Happstack.Server.RqData
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import System.Time
import DatabaseClient
import Database.HDBC
import Database.HDBC.ODBC
import DiningData

main = do
     --Create mutable memory locations to share between threads
     mView <- (newMVar "I'm a hall overview list!")
     mMenu <- (newMVar "I'm a dining hall menu list!")
     --Open database connection
     db <- connectDb
     --Start cache refresh threads
     forkIO $ refreshOverviewList mView db
     forkIO $ refreshMenuList mMenu db
     --Start our server
     simpleHTTP nullConf $ server (mView, mMenu)

guardParam :: String -> String -> ServerPart String
guardParam name val = do
         nVal <- look name
         guard (val == nVal)
         return nVal

server :: (MVar String, MVar String) -> ServerPart String
server (mView, mMenu) = msum [ do guardParam "halls" "all"
                                  serve mMenu
                             , do guardParam "halls" "overview"
                                  serve mView
                             , badRequest "Missing or invalid \"halls\" parameter"
                             ]

serve :: (MVar String) -> ServerPart String
serve mVar = do
      val <- liftIO $ readMVar mVar
      ok val


--Minutes between overview information refreshes (from database)
oMinRef :: Int
oMinRef = 3600

--Fetch the dining hall overview information out of the database
refreshOverviewList :: MVar String -> Connection -> IO ()
refreshOverviewList mVar db = forever $ do
                putStrLn "Refreshing overview!"
                --Get from database and convert to json
                overList <- retrieveOverviews db
                let jsonOver = json overList
                swapMVar mVar jsonOver
                --Show completion
                time <- getClockTime
                let notify = "Finished refreshing overview at " ++ (show time)
                putStrLn notify
                threadDelay $ 1000 * 1000 * 60 * oMinRef

--Minutes between menu refreshes (runs scraper then refreshes cache from db
mMinRef :: Int
mMinRef = 37

--Fetch the hall menu information out of the database
refreshMenuList :: MVar String -> Connection -> IO ()
refreshMenuList mVar db = forever $ do
                putStrLn "Refreshing menu list (and running scraper)!"
                --runScraper db
                --Pull new list out of database and jsonify it
                menuList <- retrieveAll db
                let jsonMenu = json menuList
                swapMVar mVar jsonMenu
                --Show completion time
                time <- getClockTime
                let notify = "Finished refreshing menu list at " ++ (show time)
                putStrLn notify
                threadDelay $ 1000 * 1000 * 60 * mMinRef
