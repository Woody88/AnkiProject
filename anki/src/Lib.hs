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
import DB (DB(..), initializeDB, getDB)
import Accounts (UserServer, userServer)
import Anki (AnkiServer, ankiServer)

type API = UserServer
      :<|> AnkiServer

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
    where accounts = accountDB db
          ankis    = ankiDB db
