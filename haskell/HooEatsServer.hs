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
import qualified Data.ByteString.Char8 as B  --Bytestrings to fix performance issues

main = do
     --Create mutable memory locations to share between threads
     mView <- (newMVar $ B.pack "I'm a hall overview list!")
     mMenu <- (newMVar $ B.pack "I'm a dining hall menu list!")
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

server :: (MVar B.ByteString, MVar B.ByteString) -> ServerPart B.ByteString
server (mView, mMenu) = msum [ do guardParam "halls" "all"
                                  serve mMenu
                             , do guardParam "halls" "overview"
                                  serve mView
                             , badRequest $ B.pack "Missing or invalid \"halls\" parameter"
                             ]

serve :: (MVar B.ByteString) -> ServerPart B.ByteString
serve mVar = do
      val <- liftIO $ readMVar mVar
      ok val


--Minutes between overview information refreshes (from database)
oMinRef :: Int
oMinRef = 3600

--Fetch the dining hall overview information out of the database
refreshOverviewList :: MVar B.ByteString -> Connection -> IO ()
refreshOverviewList mVar db = forever $ do
                putStrLn "Refreshing overview!"
                --Get from database and convert to json
                overList <- retrieveOverviews db
                let jsonOver = B.pack $ json overList
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
refreshMenuList :: MVar B.ByteString -> Connection -> IO ()
refreshMenuList mVar db = forever $ do
                putStrLn "Refreshing menu list (and running scraper)!"
                --runScraper db
                --Pull new list out of database and jsonify it
                menuList <- retrieveAll db
                let jsonMenu = B.pack $ json menuList
                swapMVar mVar jsonMenu
                --Show completion time
                time <- getClockTime
                let notify = "Finished refreshing menu list at " ++ (show time)
                putStrLn notify
                threadDelay $ 1000 * 1000 * 60 * mMinRef
