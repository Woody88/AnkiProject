{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where


import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Utils.StaticFiles
import DB (DB(..), initializeDB, getDB)
import Accounts (UserServer, userServer)
import Anki (AnkiServer, ankiServer)
import System.IO
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

type API = UserServer
      :<|> AnkiServer
      :<|> Raw

webRoot :: FilePath
webRoot = "web/build/"

startApp :: IO ()
startApp = do
    -- accounts <- putStrLn "Initializing Accounts..." >> initializeAccountsDB -- Set Memory DB for Accounts, later on will create a config file.
    dbs <- initializeDB
    putStrLn "Server Running..." >> run 3000 (logStdoutDev $ app dbs)

app :: DB -> Application
app accDB = appCors $ serve api $ server accDB


-- need to allow cors communication with elm
appCors :: Middleware
appCors = cors $ const (Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = simpleMethods
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

api :: Proxy API
api = Proxy

server :: DB -> Server API
server db = userServer accounts
       :<|> ankiServer ankis
       :<|> serveDirectoryFileServer webRoot
    where accounts = accountDB db
          ankis    = ankiDB db

parser :: IO ()
parser = do
     fHandler <- openFile "anki_list.csv" ReadWriteMode
    --  contents <- hGetContents fHandler :: IO String
    --  let contentList = splitByLine contents
    --  let mapC        = foldl mapById Map.empty contentList
    --  let updated     = replaceCard mapC "1" ["1","Protect","安全","あんぜん","\"For you protection, please fasten your seat belt\"","安全のためシートベルトを着用してください。","Noun"]
     hClose fHandler

splitByLine :: String -> [String]
splitByLine = splitOn "\n"

mapById :: Map String [String] -> String -> Map String [String]
mapById storage str = Map.insert cardId splitted storage
    where splitted  = splitOn "," str
          cardId    = head splitted

replaceCard :: Map String [String] -> String -> [String] -> Map String [String]
replaceCard storage cardId card = Map.insert cardId card storage
    where cardToUpdate = Map.lookup cardId storage
