{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}

module Lib
    ( startApp
    , app
    ) where


import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Server
import Servant.API.Experimental.Auth    (AuthProtect)
import Accounts.User                     (UserLogin(..))
import Servant.Utils.StaticFiles
import System.Environment (lookupEnv)
import DB (DB(..), initializeDB, getDB)
import Auth (Auth, Token(..), initializeJwt, initializeTokens, authServerContext)
import Accounts (UserServer, LoginEndpoints, userServer)
import Accounts.User (UserLogin(..))
import Anki (AnkiServer, AnkiEndpoints, ankiServer)
import System.IO
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map.Lazy as Map



type API =  "anki" :> Auth :> Protected
        :<|> "login" :> Unprotected
        :<|> Raw


type Unprotected = LoginEndpoints
type Protected   = AnkiEndpoints

webRoot :: FilePath
webRoot = "web/build/"

startApp :: IO ()
startApp =
    withStdoutLogger $ \aplogger -> do
        -- accounts <- putStrLn "Initializing Accounts..." >> initializeAccountsDB -- Set Memory DB for Accounts, later on will create a config file.
        dbs          <- initializeDB
        portEnv      <- lookupEnv "PORT"
        _            <- initializeJwt
        let port     = read (fromMaybe "8080" portEnv) :: Int
            settings = setPort port $ setLogger aplogger defaultSettings
        putStrLn ("Server Running on port: " ++ (show port)) >> (runSettings settings $ logStdoutDev $ app dbs)

app :: DB -> Application
app dbs = appCors $ serveWithContext api authServerContext $ server dbs


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


protected :: DB -> Token -> AnkiServer
protected db t = ankiServer ankis t
    where ankis    = ankiDB db
          tokens   = tokenDB db


unprotected :: DB -> UserServer
unprotected db = userServer accounts tokens
  where accounts = accountDB db
        tokens   = tokenDB db


api :: Proxy API
api = Proxy

server :: DB -> Server API
server dbs = protected dbs
         :<|> unprotected dbs
         :<|> serveDirectoryFileServer webRoot

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
