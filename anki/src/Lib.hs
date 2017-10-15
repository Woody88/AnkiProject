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
import Accounts (AccountDB, UserServer, initializeAccountsDB, userServer)

type API = UserServer

startApp :: IO ()
startApp = do
    accounts <- putStrLn "Initializing Accounts..." >> initializeAccountsDB
    putStrLn "Server Running..." >> (run 3000 $ logStdoutDev $ app accounts)
    -- withStdoutLogger $ \aplogger -> do
    --     let settings = setPort 3000 $ setLogger aplogger defaultSettings
    --     runSettings settings app

app :: AccountDB -> Application
app accDB = appCors $ (serve api $ server accDB)


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

server :: AccountDB -> Server API
server accDB = userServer accDB
